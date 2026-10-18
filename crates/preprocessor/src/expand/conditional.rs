//! Conditional group tracking.

use dt_tools_diagnostic::text_range::TextRange;

/// One open conditional group.
#[expect(
    clippy::struct_excessive_bools,
    reason = "independent conditional state flags"
)]
struct Conditional {
    /// Range of the opening directive.
    opening: TextRange,
    /// Whether the enclosing group is active.
    parent_active: bool,
    /// Whether a branch has been taken.
    taken: bool,
    /// Whether the current branch is active.
    active: bool,
    /// Whether an `#else` branch was seen.
    else_seen: bool,
    /// Offset where the current branch starts.
    branch_start: usize,
}

/// The open conditional groups.
#[derive(Default)]
pub(crate) struct Conditionals {
    /// The stack of currently open conditional groups.
    stack: Vec<Conditional>,
    /// The skipped branch ranges.
    skipped: Vec<TextRange>,
}

impl Conditionals {
    /// Returns true if the current group is active.
    pub(crate) fn is_active(&self) -> bool {
        self.stack
            .last()
            .is_none_or(|conditional| conditional.active)
    }

    /// Returns true if no group is open.
    pub(crate) fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Returns true if the current group has already seen `#else`.
    pub(crate) fn else_seen(&self) -> bool {
        self.stack
            .last()
            .is_some_and(|conditional| conditional.else_seen)
    }

    /// Returns true if an `#elif` condition should be evaluated.
    pub(crate) fn should_evaluate_elif(&self) -> bool {
        self.stack
            .last()
            .is_some_and(|conditional| conditional.parent_active && !conditional.taken)
    }

    /// Opens a group whose condition evaluated to `condition`.
    pub(crate) fn open(&mut self, opening: TextRange, branch_start: usize, condition: bool) {
        let parent_active = self.is_active();
        let active = parent_active && condition;
        self.stack.push(Conditional {
            opening,
            parent_active,
            taken: active,
            active,
            else_seen: false,
            branch_start,
        });
    }

    /// Starts an `#elif` branch whose condition evaluated to `condition`.
    pub(crate) fn elif(&mut self, directive_start: usize, branch_start: usize, condition: bool) {
        self.record_inactive_branch(directive_start);
        let Some(conditional) = self.stack.last_mut() else {
            return;
        };
        conditional.active = conditional.parent_active && !conditional.taken && condition;
        conditional.taken |= conditional.active;
        conditional.branch_start = branch_start;
    }

    /// Starts an `#else` branch.
    pub(crate) fn else_branch(&mut self, directive_start: usize, branch_start: usize) {
        self.record_inactive_branch(directive_start);
        let Some(conditional) = self.stack.last_mut() else {
            return;
        };
        conditional.active = conditional.parent_active && !conditional.taken;
        conditional.taken = true;
        conditional.else_seen = true;
        conditional.branch_start = branch_start;
    }

    /// Closes the current group. Returns false if none is open.
    pub(crate) fn close(&mut self, directive_start: usize) -> bool {
        let Some(conditional) = self.stack.pop() else {
            return false;
        };
        self.record(&conditional, directive_start);
        true
    }

    /// Closes every open group, returning their opening ranges.
    pub(crate) fn finish(&mut self, input_end: usize) -> Vec<TextRange> {
        let mut openings = Vec::with_capacity(self.stack.len());
        while let Some(conditional) = self.stack.pop() {
            self.record(&conditional, input_end);
            openings.push(conditional.opening);
        }
        openings.reverse();
        openings
    }

    /// Returns the skipped branch ranges.
    pub(crate) fn skipped(&self) -> &[TextRange] {
        &self.skipped
    }

    /// Records the current branch if it is inactive and its parent is active.
    fn record_inactive_branch(&mut self, end: usize) {
        let Some(conditional) = self.stack.last() else {
            return;
        };
        if !conditional.active && conditional.parent_active {
            self.record_range(TextRange::new(conditional.branch_start, end));
        }
    }

    /// Records `conditional`'s current branch if it is inactive.
    fn record(&mut self, conditional: &Conditional, end: usize) {
        if conditional.parent_active && !conditional.active {
            self.record_range(TextRange::new(conditional.branch_start, end));
        }
    }

    /// Pushes a non-empty skipped range.
    fn record_range(&mut self, range: TextRange) {
        if range.length() > 0 {
            self.skipped.push(range);
        }
    }
}
