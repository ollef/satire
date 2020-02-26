use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::Not;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Ternary {
    True,
    False,
    Unknown,
}

impl BitAnd<Ternary> for Ternary {
    type Output = Ternary;
    fn bitand(self, other: Ternary) -> Ternary {
        match (self, other) {
            (Ternary::True, Ternary::True) => Ternary::True,
            (Ternary::False, _) => Ternary::False,
            (_, Ternary::False) => Ternary::False,
            _ => Ternary::Unknown,
        }
    }
}

impl BitOr<Ternary> for Ternary {
    type Output = Ternary;
    fn bitor(self, other: Ternary) -> Ternary {
        match (self, other) {
            (Ternary::False, Ternary::False) => Ternary::False,
            (Ternary::True, _) => Ternary::True,
            (_, Ternary::True) => Ternary::True,
            _ => Ternary::Unknown,
        }
    }
}

impl Not for Ternary {
    type Output = Ternary;
    fn not(self) -> Ternary {
        match self {
            Ternary::False => Ternary::True,
            Ternary::True => Ternary::False,
            Ternary::Unknown => Ternary::Unknown,
        }
    }
}

impl From<bool> for Ternary {
    fn from(b: bool) -> Ternary {
        if b {
            Ternary::True
        } else {
            Ternary::False
        }
    }
}
impl From<&bool> for Ternary {
    fn from(b: &bool) -> Ternary {
        (*b).into()
    }
}
