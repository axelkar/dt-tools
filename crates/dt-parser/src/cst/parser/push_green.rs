use crate::cst::GreenItem;
use either::Either;

pub trait PushGreenItem: Sized {
    fn push_onto(self, vec: &mut Vec<GreenItem>);

    fn collect(self) -> Vec<GreenItem> {
        let mut vec = Vec::new();
        self.push_onto(&mut vec);
        vec
    }
}
impl<T: PushGreenItem> PushGreenItem for Option<T> {
    fn push_onto(self, vec: &mut Vec<GreenItem>) {
        if let Some(item) = self {
            item.push_onto(vec)
        }
    }
}
impl<T: PushGreenItem> PushGreenItem for Vec<T> {
    fn push_onto(self, vec: &mut Vec<GreenItem>) {
        vec.reserve(self.len());
        for item in self {
            item.push_onto(vec);
        }
    }
}
impl PushGreenItem for GreenItem {
    fn push_onto(self, vec: &mut Vec<GreenItem>) {
        vec.push(self)
    }
}
impl<A: PushGreenItem, B: PushGreenItem> PushGreenItem for (A, B) {
    fn push_onto(self, vec: &mut Vec<GreenItem>) {
        self.0.push_onto(vec);
        self.1.push_onto(vec);
    }
}
impl<A: PushGreenItem, B: PushGreenItem, C: PushGreenItem> PushGreenItem for (A, B, C) {
    fn push_onto(self, vec: &mut Vec<GreenItem>) {
        self.0.push_onto(vec);
        self.1.push_onto(vec);
        self.2.push_onto(vec);
    }
}
impl<A: PushGreenItem, B: PushGreenItem, C: PushGreenItem, D: PushGreenItem> PushGreenItem
    for (A, B, C, D)
{
    fn push_onto(self, vec: &mut Vec<GreenItem>) {
        self.0.push_onto(vec);
        self.1.push_onto(vec);
        self.2.push_onto(vec);
        self.3.push_onto(vec);
    }
}
impl<A: PushGreenItem, B: PushGreenItem, C: PushGreenItem, D: PushGreenItem, E: PushGreenItem>
    PushGreenItem for (A, B, C, D, E)
{
    fn push_onto(self, vec: &mut Vec<GreenItem>) {
        self.0.push_onto(vec);
        self.1.push_onto(vec);
        self.2.push_onto(vec);
        self.3.push_onto(vec);
        self.4.push_onto(vec);
    }
}
impl<A: PushGreenItem, B: PushGreenItem> PushGreenItem for Either<A, B> {
    fn push_onto(self, vec: &mut Vec<GreenItem>) {
        match self {
            Either::Left(a) => a.push_onto(vec),
            Either::Right(b) => b.push_onto(vec),
        }
    }
}
