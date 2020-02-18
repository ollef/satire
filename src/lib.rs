use std::collections::BTreeMap;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::Not;

#[derive(Ord, PartialOrd, Clone, Debug, Eq, PartialEq)]
pub struct Atom(pub String);

impl From<&str> for Atom {
    fn from(str: &str) -> Atom {
        Atom(str.into())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Literal {
    Atom(Atom),
    Neg(Atom),
}

impl From<&str> for Literal {
    fn from(str: &str) -> Literal {
        Literal::Atom(str.into())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Or {
    or: Vec<Literal>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct And {
    and: Vec<Or>,
}

impl From<&str> for Formula {
    fn from(str: &str) -> Formula {
        And {
            and: vec![Or {
                or: vec![str.into()],
            }],
        }
    }
}

type Formula = And;

type Interpretation = BTreeMap<Atom, bool>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Ternary {
    True,
    False,
    Unknown,
}

impl From<bool> for Ternary {
    fn from(b: bool) -> Ternary {
        (&b).into()
    }
}

impl From<&bool> for Ternary {
    fn from(b: &bool) -> Ternary {
        match *b {
            true => Ternary::True,
            false => Ternary::False,
        }
    }
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

impl Formula {
    fn true_() -> Formula {
        And { and: vec![] }
    }

    fn false_() -> Formula {
        And {
            and: vec![Or { or: vec![] }],
        }
    }
}

impl From<bool> for Formula {
    fn from(b: bool) -> Formula {
        match b {
            false => Formula::false_(),
            true => Formula::true_(),
        }
    }
}

impl From<&Atom> for Formula {
    fn from(a: &Atom) -> Formula {
        And {
            and: vec![Or {
                or: vec![Literal::Atom(a.clone())],
            }],
        }
    }
}

impl From<Or> for Formula {
    fn from(or: Or) -> Formula {
        And { and: vec![or] }
    }
}

impl Not for Or {
    type Output = Formula;
    fn not(self) -> Formula {
        let ret: Formula = self.into();
        !ret
    }
}

impl Not for Formula {
    type Output = Formula;
    fn not(self) -> Formula {
        self.and.iter().fold(Formula::false_(), |acc, x| {
            acc | {
                let y: Formula = x.clone().into();
                !y
            }
        })
    }
}

impl BitAnd<Formula> for Formula {
    type Output = Formula;
    fn bitand(mut self, mut other: Formula) -> Formula {
        self.and.append(&mut other.and);
        self
    }
}

impl BitOr<Formula> for Formula {
    type Output = Formula;
    fn bitor(self, other: Formula) -> Formula {
        match (self.and.as_slice(), other.and.as_slice()) {
            ([], _) => return other,
            (_, []) => return self,
            _ => {}
        }

        let (x1, y1): (Formula, Formula) = match self.and.as_slice() {
            [only] => (only.clone().into(), Formula::true_()),
            _ => {
                let head = self.and[0].clone();
                let tail = self.and[1..].to_vec();
                (head.into(), And { and: tail })
            }
        };

        let (x2, y2): (Formula, Formula) = match other.and.as_slice() {
            [only] => (only.clone().into(), Formula::true_()),
            _ => {
                let head = other.and[0].clone();
                let tail = other.and[1..].to_vec();
                (head.into(), And { and: tail })
            }
        };

        (x1.clone() | x2.clone()) & (y1.clone() | x2) & (x1 | y2.clone()) & (y1 | y2)
    }
}

impl Atom {
    fn is_satisfied(&self, interpretation: &Interpretation) -> Ternary {
        interpretation
            .get(self)
            .map(|x| x.into())
            .unwrap_or(Ternary::Unknown)
    }

    fn simplify(&self, interpretation: &Interpretation) -> Formula {
        match interpretation.get(self) {
            None => self.into(),
            Some(b) => (*b).into(),
        }
    }
}

impl Literal {
    fn is_satisfied(&self, interpretation: &Interpretation) -> Ternary {
        match self {
            Literal::Atom(atom) => atom.is_satisfied(interpretation),
            Literal::Neg(atom) => !atom.is_satisfied(interpretation),
        }
    }

    fn simplify(&self, interpretation: &Interpretation) -> Formula {
        match self {
            Literal::Atom(atom) => atom.simplify(interpretation),
            Literal::Neg(atom) => !atom.simplify(interpretation),
        }
    }
}

impl Or {
    fn is_satisfied(&self, interpretation: &Interpretation) -> Ternary {
        self.or.iter().fold(Ternary::False, |acc, literal| {
            acc | literal.is_satisfied(interpretation)
        })
    }

    fn simplify(&self, interpretation: &Interpretation) -> Formula {
        self.or.iter().fold(Formula::false_(), |acc, literal| {
            acc | literal.simplify(interpretation)
        })
    }
}

impl And {
    fn is_satisfied(&self, interpretation: &Interpretation) -> Ternary {
        self.and.iter().fold(Ternary::True, |acc, or| {
            acc & or.is_satisfied(interpretation)
        })
    }

    fn simplify(&self, interpretation: &Interpretation) -> Formula {
        self.and.iter().fold(Formula::true_(), |acc, or| {
            acc & or.simplify(interpretation)
        })
    }
}

pub fn find_necessary_valuations(formula: &Formula) -> Interpretation {
    formula
        .and
        .iter()
        .fold(Interpretation::new(), |mut acc, clause| {
            if clause.or.len() == 1 {
                match clause.or[0].clone() {
                    Literal::Atom(atom) => acc.insert(atom, true),
                    Literal::Neg(atom) => acc.insert(atom, false),
                };
            }
            acc
        })
}

pub fn solve(mut formula: Formula) -> Result<Interpretation, Interpretation> {
    let mut interpretation = Interpretation::new();
    loop {
        println!("solve loop, formula: {:?}", formula);
        formula = formula.simplify(&interpretation);
        if formula == Formula::true_() {
            return Ok(interpretation);
        }
        if formula == Formula::false_() {
            return Err(interpretation);
        }
        let mut new_values = find_necessary_valuations(&formula);
        interpretation.append(&mut new_values);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn single_var() {
    //     assert_eq!(
    //         solve("A".into()),
    //         Ok(vec![("A".into(), true)].into_iter().collect())
    //     );
    // }

    #[test]
    fn single_var_neg() {
        assert_eq!(
            solve({
                let x: Formula = "A".into();
                !x
            }),
            Ok(vec![("A".into(), false)].into_iter().collect())
        );
    }
}
