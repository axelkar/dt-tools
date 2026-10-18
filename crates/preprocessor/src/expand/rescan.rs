use std::sync::Arc;

use rustc_hash::FxHashSet;

use crate::PpToken;

/// A set of macro names that must not expand again.
pub(crate) type HideSet = Arc<FxHashSet<String>>;

/// A token flowing through the expander.
#[derive(Clone)]
pub(crate) struct MacroToken {
    /// The preprocessing token.
    pub(crate) token: PpToken,
    /// The set of macro names that must not expand again.
    pub(crate) hide: Option<HideSet>,
    /// The number of macro expansions that produced this token.
    pub(crate) depth: usize,
}

impl MacroToken {
    /// Returns a token from the source text.
    pub(crate) fn source(token: PpToken) -> Self {
        Self {
            token,
            hide: None,
            depth: 0,
        }
    }

    /// Returns whether `name` is hidden on this token.
    pub(crate) fn is_hidden(&self, name: &str) -> bool {
        self.hide.as_ref().is_some_and(|hide| hide.contains(name))
    }
}

/// The token stream consumed during macro expansion.
///
/// Replacement tokens produced by expansion are returned before the remaining source tokens.
/// Directives are parsed from [`Self::remaining_source`] when no replacement tokens are pending.
///
/// Reference: [WG14 N3220 (C23 draft) § 6.10.5.4 Rescanning and further replacement](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf#subsubsection.6.10.5.4).
pub(crate) struct TokenStream<'a> {
    /// Tokens produced by macro expansion, waiting for rescan.
    pending: Vec<MacroToken>,
    /// The source tokens of the file, never produced by expansion.
    source: &'a [PpToken],
    /// The index of the next unread source token.
    index: usize,
}

impl<'a> TokenStream<'a> {
    /// Creates a stream over `source`.
    pub(crate) fn new(source: &'a [PpToken]) -> Self {
        Self {
            pending: Vec::new(),
            source,
            index: 0,
        }
    }

    /// Returns the next token, preferring replacement tokens.
    pub(crate) fn next(&mut self) -> Option<MacroToken> {
        if let Some(token) = self.pending.pop() {
            return Some(token);
        }

        let token = self.source.get(self.index).copied()?;
        self.index += 1;
        Some(MacroToken::source(token))
    }

    /// Returns the next token without consuming it.
    pub(crate) fn peek(&self) -> Option<PpToken> {
        self.pending
            .last()
            .map(|token| token.token)
            .or_else(|| self.source.get(self.index).copied())
    }

    /// Returns whether no replacement tokens are waiting for rescan.
    pub(crate) fn is_rescan_empty(&self) -> bool {
        self.pending.is_empty()
    }

    /// Skips `len` source tokens.
    pub(crate) fn skip_source(&mut self, len: usize) {
        self.index += len;
    }

    /// Returns the unread source tokens.
    pub(crate) fn remaining_source(&self) -> &'a [PpToken] {
        &self.source[self.index..]
    }

    /// Pushes replacement tokens for rescan.
    pub(crate) fn push_replacement(&mut self, tokens: Vec<MacroToken>) {
        self.pending.extend(tokens.into_iter().rev());
    }
}

/// Returns `hide` extended with `name`.
pub(crate) fn extend_hide(hide: Option<&HideSet>, name: &str) -> HideSet {
    if let Some(hide) = hide
        && hide.contains(name)
    {
        return Arc::clone(hide);
    }

    let mut hide = hide.map(|hide| hide.as_ref().clone()).unwrap_or_default();
    hide.insert(name.to_owned());
    Arc::new(hide)
}

/// Returns the union of `hide` and `extra`.
pub(crate) fn union_hide(hide: Option<&HideSet>, extra: &HideSet) -> HideSet {
    match hide {
        Some(hide) if hide.is_superset(extra) => Arc::clone(hide),
        Some(hide) => {
            let mut hide = hide.as_ref().clone();
            hide.extend(extra.iter().cloned());
            Arc::new(hide)
        }
        None => Arc::clone(extra),
    }
}
