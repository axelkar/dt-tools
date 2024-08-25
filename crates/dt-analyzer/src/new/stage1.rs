//! Per-file stage where a rough outline of the file is parsed
//!
//! Nothing is resolved or computed
use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

use dt_diagnostic::{Diagnostic, DiagnosticCollector, Severity};
use dt_parser::{
    ast::{self, AstNode, AstNodeOrToken, AstToken, HasLabel, HasName, SourceFile},
    cst2::lexer::TokenKind,
    TextRange,
};
use enum_as_inner::EnumAsInner;
use rayon::iter::{ParallelBridge, ParallelIterator};
use rustc_hash::FxHashMap;

use crate::macros::MacroDefinition;

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum AnalyzedToplevel {
    Node(AnalyzedToplevelNode),
    Include(AnalyzedInclude),
    MacroDefinition {
        text_range: TextRange,
        parsed: MacroDefinition,
    },
}

impl AnalyzedToplevel {
    /// Returns the range of the textual contents.
    pub fn text_range(&self) -> TextRange {
        match self {
            Self::Node(node) => node.text_range,
            Self::Include(inc) => inc.text_range,
            Self::MacroDefinition {
                text_range,
                parsed: _,
            } => *text_range,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum PPIncludeParseError {
    #[error("Nothing after `#include`")]
    NothingAfterInclude,
    #[error("Unexpected character after `#include`")]
    UnexpectedCharacter,
    #[error("Missing string terminator")]
    MissingStringTerminator,
}

pub(crate) fn subslice_offset(this: &str, inner: &str) -> Option<usize> {
    let self_beg = this.as_ptr() as usize;
    let inner = inner.as_ptr() as usize;
    if inner < self_beg || inner > self_beg.wrapping_add(this.len()) {
        None
    } else {
        Some(inner.wrapping_sub(self_beg))
    }
}

// TODO: direct reference to AST so you don't have to go digging around for it
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalyzedInclude {
    /// The range of this include in the including file.
    ///
    /// Usage of constants defined in the included file is only valid after this position.
    ///
    /// This can be used to sort values.
    pub text_range: TextRange,
    /// Whether the include is a preprocessor (`#include`) or a DTS (`/include/`) include.
    pub is_preprocessor: bool,
    /// The raw path pointed to.
    pub path: String,
    /// Whether the current directory should be searched too.
    ///
    /// `true` if it's a DTS include or double quotes are used.
    ///
    /// `false` if angle brackets are used.
    pub relative: bool,
}

impl AnalyzedInclude {
    /// Parses the preprocessor include directive token's text.
    ///
    /// Returns `Err` when there is invalid (or missing) text after `#include`.
    fn pp_parse(
        text_range: TextRange,
        input: &str,
        diag: &impl DiagnosticCollector,
    ) -> Result<Self, PPIncludeParseError> {
        debug_assert!(input.starts_with('#'));
        let s = input
            .get(1..)
            .expect("lexer safe")
            .trim_start_matches(|ch| ch == ' ' || ch == '\t');

        debug_assert!(s.starts_with("include"));
        let s = s
            .get("include".len()..)
            .expect("lexer safe")
            .trim_start_matches(|ch| ch == ' ' || ch == '\t');

        let (relative, (path, rest)) = match s
            .as_bytes()
            .first()
            .ok_or(PPIncludeParseError::NothingAfterInclude)?
        {
            b'<' => (
                false,
                s.get(1..)
                    .expect("safe")
                    .split_once('>')
                    .ok_or(PPIncludeParseError::MissingStringTerminator)?,
            ),
            b'"' => (
                true,
                s.get(1..)
                    .expect("safe")
                    .split_once('"')
                    .ok_or(PPIncludeParseError::MissingStringTerminator)?,
            ),
            _ => return Err(PPIncludeParseError::UnexpectedCharacter),
        };

        if !rest.is_empty() {
            if let Some(offset) = subslice_offset(input, rest) {
                diag.emit(Diagnostic::new(
                    TextRange {
                        start: text_range.start + offset,
                        end: text_range.end,
                    },
                    Cow::Borrowed("Unexpected characters after include string"),
                    Severity::Warn,
                ));
            }
        }

        Ok(Self {
            text_range,
            is_preprocessor: true,
            path: path.to_owned(),
            relative,
        })
    }

    /// Finds the file in `parent_path` or `include_dirs`, depending on [`relative`](Self::relative).
    pub fn find_file<P: AsRef<Path>>(
        &self,
        parent_path: &Path,
        include_dirs: &[P],
    ) -> Option<PathBuf> {
        // TODO: canonicalized SourceId
        // LSP client (not server) might not canonicalize the path, but this does
        //
        // Alternatively, don't canonicalize the path here, just make include_dirs and parent_path
        // absolute.

        self.relative
            .then_some(parent_path)
            .into_iter()
            .chain(include_dirs.iter().map(AsRef::as_ref))
            .find_map(|base_path| {
                let new_path = base_path.join(&self.path).canonicalize().ok()?;
                new_path.exists().then_some(new_path)
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalyzedToplevelNode {
    /// Whether this is an extension for a labeled node.
    pub is_extension: bool,
    /// If an extension, the name of the label, otherwise the name of the node.
    pub name: String,
    /// The range of this node.
    ///
    /// This can be used to sort values.
    pub text_range: TextRange,
    pub ast: ast::DtNode,
    // TODO: Label per file, not per toplevel node
    /// Labels defined in this node.
    pub labels: FxHashMap<String, LabelDef>,
}

/// Label definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LabelDef {
    // TODO: no node_ast
    pub node_ast: ast::DtNode,
    pub label_ast: ast::DtLabel,
}

// TODO: gather labels from broken nodes (e.g. missing name, unit addr or right curly)
pub(crate) fn gather_labels<'input>(
    ast: &ast::DtNode,
    src: &'input str,
    on_label: &mut impl FnMut(&'input str, LabelDef),
) {
    if let Some(label) = ast.label() {
        if let Some(name) = label.name() {
            on_label(
                name.syntax().text_from_source(src),
                LabelDef {
                    node_ast: ast.clone(),
                    label_ast: label,
                },
            );
        }
    }

    for ast in ast.subnodes() {
        if ast.is_concrete() {
            gather_labels(&ast, src, on_label);
        }
    }
}

pub fn analyze_file(
    file: &SourceFile,
    src: &str,
    diag: &(impl DiagnosticCollector + Sync),
) -> Vec<AnalyzedToplevel> {
    // Passthrough the span to the Rayon worker threads
    let span = tracing::Span::current();

    let mut analyzed: Vec<_> = file
        .syntax()
        .children()
        .par_bridge()
        .filter_map(ast::ToplevelItem::cast)
        .filter_map(|item| {
            let _span = span.clone().entered();
            match item {
                ast::ToplevelItem::Node(node) => {
                    let mut labels = FxHashMap::default();
                    gather_labels(&node, src, &mut |name, def| {
                        labels.insert(name.to_owned(), def);
                    });
                    Some(AnalyzedToplevel::Node(AnalyzedToplevelNode {
                        is_extension: node.is_extension(),
                        name: node
                            .text_name(src)
                            .map(|c| c.into_owned())
                            .unwrap_or_default(),
                        text_range: node.syntax().text_range(),
                        ast: node,
                        labels,
                    }))
                }
                ast::ToplevelItem::Directive(dir) => {
                    let mut iter = dir.syntax().child_tokens();

                    // Skip until DtIncludeDirective is found, return None if it isn't found
                    iter.find(|tok| tok.green.kind == TokenKind::DtIncludeDirective)?;

                    let string_tok = iter.find(|tok| tok.green.kind == TokenKind::String)?;
                    let path = match crate::string::interpret_escaped_string(string_tok.text()) {
                        Ok(path) => path,
                        Err(err) => {
                            diag.emit(Diagnostic::new(
                                string_tok.text_range(),
                                Cow::Owned(err.to_string()),
                                Severity::Error,
                            ));
                            return None;
                        }
                    };

                    Some(AnalyzedToplevel::Include(AnalyzedInclude {
                        text_range: dir.syntax().text_range(),
                        is_preprocessor: false,
                        path,
                        relative: true,
                    }))
                }
                ast::ToplevelItem::PreprocessorDirective(dir)
                    if dir.kind() == TokenKind::IncludeDirective =>
                {
                    Some(AnalyzedToplevel::Include(
                        match AnalyzedInclude::pp_parse(
                            dir.syntax().text_range(),
                            dir.syntax().text(),
                            diag,
                        ) {
                            Ok(inc) => inc,
                            Err(err) => {
                                diag.emit(Diagnostic::new(
                                    dir.syntax().text_range(),
                                    Cow::Owned(err.to_string()),
                                    Severity::Error,
                                ));
                                return None;
                            }
                        },
                    ))
                }
                ast::ToplevelItem::PreprocessorDirective(dir)
                    if dir.kind() == TokenKind::DefineDirective =>
                {
                    Some(AnalyzedToplevel::MacroDefinition {
                        text_range: dir.syntax().text_range(),
                        parsed: match MacroDefinition::parse(dir.syntax().text()) {
                            Ok(inc) => inc,
                            Err(err) => {
                                diag.emit(Diagnostic::new(
                                    if let Some(local_range) = err.text_range() {
                                        local_range.offset(dir.syntax().text_offset)
                                    } else {
                                        dir.syntax().text_range()
                                    },
                                    Cow::Owned(err.to_string()),
                                    Severity::Error,
                                ));
                                return None;
                            }
                        },
                    })
                }
                _ => None,
            }
        })
        .collect();

    // Parallelism makes it unpredictable
    analyzed.sort_unstable_by_key(|top| top.text_range());

    analyzed
}
