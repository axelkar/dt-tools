//! Diagnostic emission and source mapping for lowering.

use dt_tools_analyzer::macros::{MacroDefinition, TextRangeMap, TextRangeMapTo};
use dt_tools_diagnostic::{Diagnostic, DiagnosticMessage, MultiSpan, Severity, Span, SpanLabel};
use dt_tools_parser::TextRange;

use crate::file::File;

/// Maps a [`TextRange`] in the text being lowered to a [`MultiSpan<File>`].
///
/// The chain of parents is the chain of derived texts: each frame's parent maps the text one level
/// out, up to the file text.
///
/// For example, a range emitted deep inside a macro expansion is mapped up through the parents.
#[derive(Debug)]
pub enum SourceMap<'a> {
    /// Ranges are byte offsets into the file's source text.
    File(File),
    /// Ranges must be shifted by `offset` before mapping through `parent`.
    ///
    /// Used when a substring is reparsed on its own (e.g preprocessor conditionals) so its local
    /// ranges need translating back into the enclosing text.
    Offset {
        parent: &'a SourceMap<'a>,
        offset: usize,
    },
    /// Ranges are byte offsets into a macro substitution.
    Macro {
        /// `SourceMap` that maps ranges in the invocation.
        parent: &'a SourceMap<'a>,
        substitution: &'a MacroSubstitutionProvenance,
    },
}

impl SourceMap<'_> {
    /// Maps a [`TextRange`] in the text being lowered to a [`MultiSpan<File>`].
    pub(crate) fn resolve_full(&self, range: TextRange) -> MultiSpan<File> {
        match self {
            SourceMap::File(file) => MultiSpan::from(range.within_file(*file)),
            SourceMap::Offset { parent, offset } => parent.resolve_full(range.offset(*offset)),
            SourceMap::Macro {
                parent,
                substitution,
            } => substitution.resolve_full(range, parent),
        }
    }
}

/// Macro substitution metadata for diagnostic and MIR provenance.
///
/// Where each part of a macro substitution came from.
#[derive(Debug, Clone)]
pub struct MacroSubstitutionProvenance {
    /// The macro name.
    pub(crate) name: String,

    /// Maps offsets in the expanded text to the macro definition or arguments.
    pub(crate) source_mappings: Vec<TextRangeMap>,

    /// The length of the substituted text, to compute end offsets for source mappings.
    pub(crate) substituted_text_len: usize,

    /// The parsed macro definition.
    pub(crate) def: MacroDefinition,

    /// Span of the `#define` directive.
    ///
    /// ```text
    /// #define FOO(bar) bar
    /// ^^^^^^^^^^^^^^^^^^^^ this is the #define directive
    ///
    /// FOO(baz)
    /// ```
    pub(crate) def_span: Span<File>,

    /// Range of each argument, in the enclosing text of the macro invocation.
    ///
    /// Resolved by [`SourceMap::Macro::parent`].
    ///
    /// ```text
    /// #define FOO(bar) bar
    ///
    /// FOO(baz)
    ///     ^^^ this is an argument
    /// ```
    pub(crate) args: Vec<TextRange>,

    /// Range of the whole macro invocation, in the enclosing text.
    ///
    /// Resolved by [`SourceMap::Macro::parent`].
    ///
    /// ```text
    /// #define FOO(bar) bar
    ///
    /// FOO(baz)
    /// ^^^^^^^^ this is the invocation
    /// ```
    pub(crate) invocation: TextRange,
}

impl MacroSubstitutionProvenance {
    /// Returns an iterator of the source mappings with the end offset computed.
    fn source_mappings_with_end(&self) -> impl Iterator<Item = (TextRange, &TextRangeMapTo)> {
        let mut iter = self.source_mappings.iter().peekable();

        std::iter::from_fn(move || {
            let current = iter.next()?;

            let end_offset = iter
                .peek()
                .map_or(self.substituted_text_len, |next| next.from_offset);

            Some((TextRange::new(current.from_offset, end_offset), &current.to))
        })
    }

    /// Returns the argument's resolved text ranges and its name.
    ///
    /// # Example
    ///
    /// ```text
    /// #define FOO(bar) bar
    ///             ^^^ &str returned by get_arg_info
    ///
    /// FOO(baz)
    ///     ^^^ MultiSpan<File> returned by get_arg_info
    /// ```
    fn get_arg_info(&self, parent: &SourceMap, argument_idx: usize) -> (MultiSpan<File>, &str) {
        let arg_range = *self.args.get(argument_idx).unwrap_or(&self.invocation);
        let result = parent.resolve_full(arg_range);

        // TODO: test that this doesn't panic
        let arg_name = self
            .def
            .arg_name(argument_idx)
            .expect("argument index should be valid");

        (result, arg_name)
    }

    /// Push "in this expansion" and "defined here" labels for one macro level.
    fn push_expansion_labels(&self, invoc_parent: &SourceMap, result: &mut MultiSpan<File>) {
        let invoc_full = invoc_parent.resolve_full(self.invocation);
        for invoc_span in &invoc_full.primary_spans {
            result.span_labels.push(SpanLabel {
                span: *invoc_span,
                msg: format!("in this expansion of `{}`", self.name).into(),
            });
        }
        result.span_labels.push(SpanLabel {
            span: self.def_span.subspan_inside(self.def.name_range),
            msg: format!("`{}` defined here", self.name).into(),
        });
    }

    /// Push labels for this macro level, and for all ancestor macros if needed.
    fn push_macro_chain_labels(
        &self,
        parent: &SourceMap,
        result: &mut MultiSpan<File>,
        walk_ancestors: bool,
    ) {
        self.push_expansion_labels(parent, result);

        if walk_ancestors {
            // Walk up ancestor macros
            let mut ancestor = parent;
            while let SourceMap::Macro {
                parent: anc_parent,
                substitution: anc_exp,
            } = ancestor
            {
                anc_exp.push_expansion_labels(anc_parent, result);
                ancestor = anc_parent;
            }
        }
    }

    /// Maps `range` (in the expanded text) to all contributing source locations.
    ///
    /// `parent` resolves ranges in the enclosing text (invocation).
    fn resolve_full(&self, range: TextRange, parent: &SourceMap) -> MultiSpan<File> {
        /// Returns the union of the text ranges in the sorted iterator, or `None` if the
        /// iterator is empty.
        ///
        /// The iterator must be sorted.
        fn text_range_union_sorted(
            iter: &mut impl DoubleEndedIterator<Item = TextRange>,
        ) -> Option<TextRange> {
            let first = iter.next()?;
            let end = iter.next_back().unwrap_or(first).end;
            Some(TextRange {
                start: first.start,
                end,
            })
        }

        let mut multispan = MultiSpan::empty();

        let source_mappings: Vec<_> = self
            .source_mappings_with_end()
            .filter_map(|(map_range, to)| {
                // Clamp the range of the source mapping to the passed in `range`.
                let clamped_range = map_range.intersect(range)?;
                Some((clamped_range, map_range, to))
            })
            .collect();

        // Combine MacroTextOffsets so we don't get too many diagnostics
        if let Some(macro_text) =
            text_range_union_sorted(&mut source_mappings.iter().filter_map(|(_, _, to)| {
                if let TextRangeMapTo::MacroTextOffset(macro_text) = to {
                    Some(*macro_text)
                } else {
                    None
                }
            }))
        {
            multispan
                .primary_spans
                .push(self.def_span.subspan_inside(macro_text));
        }

        for (clamped_range, map_range, to) in &source_mappings {
            if let TextRangeMapTo::MacroTextOffset(_) = to {
                // Combined above
                continue;
            }

            self.resolve_piece(to, clamped_range, map_range, parent, &mut multispan);
        }

        if let Some(mapped_range) =
            text_range_union_sorted(&mut source_mappings.iter().map(|(clamped, _, _)| *clamped))
        {
            // Parts of `range` not covered by the mappings
            for unmapped in range.difference(mapped_range) {
                self.resolve_unmapped(parent, unmapped, &mut multispan);
            }
        } else {
            // Completely unmapped
            self.resolve_unmapped(parent, range, &mut multispan);
        }

        let needs_ancestor_walk = multispan.span_labels.is_empty();
        self.push_macro_chain_labels(parent, &mut multispan, needs_ancestor_walk);

        multispan
    }

    /// Resolves text not covered by the source mappings to the invocation site.
    ///
    /// `range` is only used for an internal compiler warning.
    fn resolve_unmapped(
        &self,
        parent: &SourceMap,
        range: TextRange,
        multispan: &mut MultiSpan<File>,
    ) {
        tracing::warn!("unmapped text of macro `{}` expansion: {range}", self.name);

        // Forward invocation but dedupe.
        let invoc_full = parent.resolve_full(self.invocation);
        for span in invoc_full.primary_spans {
            if !multispan.primary_spans.contains(&span) {
                multispan.primary_spans.push(span);
            }
        }
        for label in invoc_full.span_labels {
            if !multispan.span_labels.contains(&label) {
                multispan.span_labels.push(label);
            }
        }
    }

    /// Resolves one piece of a range through a single [`TextRangeMapTo`].
    fn resolve_piece(
        &self,
        to: &TextRangeMapTo,
        clamped_range: &TextRange,
        map_range: &TextRange,
        parent: &SourceMap,
        multispan: &mut MultiSpan<File>,
    ) {
        match to {
            TextRangeMapTo::MacroTextOffset(macro_text) => {
                let final_range_in_def = map_range.project_subrange(*clamped_range, *macro_text);

                multispan
                    .primary_spans
                    .push(self.def_span.subspan_inside(final_range_in_def));
            }
            // Arguments may be prescanned, so map to the whole argument.
            TextRangeMapTo::ArgumentIdx(argument_idx) => {
                self.resolve_argument_full(parent, multispan, *argument_idx);
            }
            // The separator isn't in the source; blame the vararg parameter.
            TextRangeMapTo::GeneratedVarargSeparator { macro_text } => {
                multispan
                    .primary_spans
                    .push(self.def_span.subspan_inside(*macro_text));
            }
            TextRangeMapTo::Concat { operator, sources } => {
                self.resolve_concat_full(parent, operator, sources, multispan);
            }
            TextRangeMapTo::Stringify {
                macro_text,
                argument_idx,
            } => {
                self.resolve_stringify_full(parent, *macro_text, *argument_idx, multispan);
            }
            TextRangeMapTo::GeneratedWhitespace => {
                // Generated and shouldn't have any significance
            }
        }
    }

    /// Resolves a [`TextRangeMapTo::ArgumentIdx`].
    fn resolve_argument_full(
        &self,
        parent: &SourceMap<'_>,
        multispan: &mut MultiSpan<File>,
        argument_idx: usize,
    ) {
        let (arg_full, arg_name) = self.get_arg_info(parent, argument_idx);

        multispan
            .primary_spans
            .extend_from_slice(&arg_full.primary_spans);
        multispan
            .span_labels
            .extend_from_slice(&arg_full.span_labels);

        for arg_span in &arg_full.primary_spans {
            multispan.span_labels.push(SpanLabel {
                span: *arg_span,
                msg: format!("argument `{arg_name}` of `{}`", self.name).into(),
            });
        }
    }

    /// Resolves a [`TextRangeMapTo::Concat`].
    fn resolve_concat_full(
        &self,
        parent: &SourceMap,
        operator: &TextRange,
        sources: &[TextRangeMapTo; 2],
        multispan: &mut MultiSpan<File>,
    ) {
        multispan.primary_spans.reserve(2);

        // Label the `##` paste site in the macro definition
        multispan.span_labels.push(SpanLabel {
            span: self.def_span.subspan_inside(*operator),
            msg: "pasted here with `##`".into(),
        });

        // Resolve each source to its spans
        for source in sources {
            match source {
                TextRangeMapTo::MacroTextOffset(macro_text) => {
                    multispan
                        .primary_spans
                        .push(self.def_span.subspan_inside(*macro_text));
                }
                // TODO: forward to argumentidx?
                TextRangeMapTo::ArgumentIdx(_)
                | TextRangeMapTo::GeneratedVarargSeparator { .. }
                | TextRangeMapTo::GeneratedWhitespace => {}
                TextRangeMapTo::Concat {
                    operator: inner_op,
                    sources: inner_src,
                } => {
                    self.resolve_concat_full(parent, inner_op, inner_src, multispan);
                }
                TextRangeMapTo::Stringify {
                    macro_text,
                    argument_idx,
                } => {
                    // Edge case: stringify as a concat source
                    // Show where the stringify occurred
                    multispan
                        .primary_spans
                        .push(self.def_span.subspan_inside(*macro_text));
                    self.resolve_stringify_full(parent, *macro_text, *argument_idx, multispan);
                }
            }
        }
    }

    /// Resolves a [`TextRangeMapTo::Stringify`].
    fn resolve_stringify_full(
        &self,
        parent: &SourceMap,
        macro_text: TextRange,
        argument_idx: usize,
        multispan: &mut MultiSpan<File>,
    ) {
        let primary = self.def_span.subspan_inside(macro_text);
        multispan.primary_spans.push(primary);

        let (arg_full, arg_name) = self.get_arg_info(parent, argument_idx);

        multispan
            .primary_spans
            .extend_from_slice(&arg_full.primary_spans);
        multispan
            .span_labels
            .extend_from_slice(&arg_full.span_labels);

        // Label the argument that was stringified
        for arg_span in &arg_full.primary_spans {
            multispan.span_labels.push(SpanLabel {
                span: *arg_span,
                msg: format!("argument `{arg_name}` of `{}` stringified here", self.name).into(),
            });
        }
    }
}

/// Diagnostic sink and source-mapping context.
///
/// The sink cannot live inside [`SourceMap`] because every `SourceMap` frame like
/// [`SourceMap::Macro`] would have to alias the single `&mut` sink.
pub struct Diag<'s, 'm> {
    pub(crate) sink: &'s mut Vec<Diagnostic<File>>,
    pub(crate) map: &'m SourceMap<'m>,
}

impl<'s, 'm> Diag<'s, 'm> {
    /// Creates a [`Diag`] from a sink and a source map.
    pub fn new(sink: &'s mut Vec<Diagnostic<File>>, map: &'m SourceMap<'m>) -> Self {
        Self { sink, map }
    }

    /// Emits a diagnostic for `range` in the current text.
    ///
    /// The diagnostic will include span labels from the source map chain (e.g. macro
    /// expansion sites).
    pub fn emit<Msg: Into<DiagnosticMessage>>(
        &mut self,
        range: TextRange,
        msg: Msg,
        severity: Severity,
    ) {
        /// Helper function that isn't monomorphized
        fn emit_inner(
            this: &mut Diag<'_, '_>,
            range: TextRange,
            msg: DiagnosticMessage,
            severity: Severity,
        ) {
            let multi_span = this.map.resolve_full(range);
            this.sink.push(Diagnostic {
                span: multi_span,
                msg,
                severity,
            });
        }

        emit_inner(self, range, msg.into(), severity);
    }

    /// Maps `range` to a [`MultiSpan<File>`] with primary spans and span labels.
    #[must_use]
    pub fn resolve_full(&self, range: TextRange) -> MultiSpan<File> {
        self.map.resolve_full(range)
    }

    /// Maps `range` to the first primary [`Span<File>`].
    ///
    /// # Panics
    ///
    /// Panics if [`Self::resolve_full`] doesn't return any primary spans.
    #[must_use]
    pub fn resolve(&self, range: TextRange) -> Span<File> {
        self.resolve_full(range)
            .primary_spans
            .into_iter()
            .next()
            .expect("resolve_full must return at least one primary span")
    }

    /// Pushes a pre-built diagnostic.
    pub fn push(&mut self, diag: Diagnostic<File>) {
        self.sink.push(diag);
    }
}
