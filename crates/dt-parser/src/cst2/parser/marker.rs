use drop_bomb::DropBomb;

use crate::cst2::{parser::event::Event, NodeKind};

use super::Parser;

pub struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(super) fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb: DropBomb::new("Markers must be completed!"),
        }
    }

    /// Completes the marker.
    pub fn complete(mut self, p: &mut Parser, kind: NodeKind) -> CompletedMarker {
        self.bomb.defuse();

        let event_at_pos = &mut p.events[self.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };

        #[cfg(feature = "grammar-tracing")]
        tracing::debug!(pos = p.events.len(), "end node");

        p.events.push(Event::FinishNode);

        CompletedMarker { pos: self.pos }
    }
}

pub struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    /// Wraps the completed marker within a new marker.
    pub fn precede(self, p: &mut Parser) -> Marker {
        let new_m = p.start();

        if let Event::StartNode {
            ref mut forward_parent,
            ..
        } = p.events[self.pos]
        {
            *forward_parent = Some(new_m.pos - self.pos);
        } else {
            // CompletedMarker::pos is private and only created
            // by Marker::complete so it must be valid
            unreachable!();
        }

        new_m
    }
}
