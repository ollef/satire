#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
use quickcheck::{Arbitrary, Gen};
use std::collections::BTreeMap;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::Not;

#[derive(Ord, PartialOrd, Clone, Copy, Debug, Eq, PartialEq)]
pub struct Atom(pub i32);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Literal {
    value: i32,
}

impl Literal {
    pub fn pos(atom: Atom) -> Literal {
        Literal::new(atom, true)
    }

    pub fn neg(atom: Atom) -> Literal {
        Literal::new(atom, false)
    }

    pub fn new(atom: Atom, polarity: bool) -> Literal {
        match atom {
            Atom(i) => Literal {
                value: i + i + if polarity { 1 } else { 0 },
            },
        }
    }

    fn atom(&self) -> Atom {
        Atom(self.value >> 1)
    }

    fn polarity(&self) -> bool {
        self.value & 1 == 1
    }
}

#[cfg(test)]
impl Arbitrary for Literal {
    fn arbitrary<G: Gen>(g: &mut G) -> Literal {
        Literal::new(Atom(i32::arbitrary(g).abs() % 8), bool::arbitrary(g))
    }
}

type Or = Vec<Literal>;

type And = Vec<Or>;

type Interpretation = BTreeMap<Atom, bool>;

pub fn dpll(formula: And) -> Option<Interpretation> {
    fn go(formula: And, mut interpretation: Interpretation) -> Option<Interpretation> {
        dbg!(&formula);
        if is_true(&formula) {
            Some(interpretation)
        } else if is_false(&formula) {
            None
        } else {
            let mut unit_clauses = unit_clauses(&formula);
            dbg!(&unit_clauses);
            let formula = assign_interpretation(formula, &unit_clauses);
            interpretation.append(&mut unit_clauses);

            let mut pure_literals = pure_literals(&formula);
            dbg!(&pure_literals);
            let formula = assign_interpretation(formula, &pure_literals);
            interpretation.append(&mut pure_literals);

            dbg!(&formula);
            match get_first_atom(&formula) {
                None => go(formula, interpretation),
                Some(atom) => {
                    dbg!(&atom);
                    let true_formula = assign_atom(&formula, atom, true);
                    let interpretation_before = interpretation.clone();
                    let mut true_interpretation = interpretation;
                    true_interpretation.insert(atom, true);
                    match go(true_formula, true_interpretation) {
                        Some(interpretation) => Some(interpretation),
                        None => {
                            dbg!(&atom);
                            let false_formula = assign_atom(&formula, atom, false);
                            let mut false_interpretation = interpretation_before;
                            false_interpretation.insert(atom, false);
                            go(false_formula, false_interpretation)
                        }
                    }
                }
            }
        }
    }
    go(formula, Interpretation::new())
}

fn is_true(formula: &And) -> bool {
    formula.is_empty()
}

fn is_false(formula: &And) -> bool {
    formula.iter().any(|clause| clause.is_empty())
}

fn unit_clauses(formula: &And) -> Interpretation {
    let mut result = Interpretation::new();
    for clause in formula {
        if clause.len() == 1 {
            result.insert(clause[0].atom(), clause[0].polarity());
        }
    }
    result
}

fn pure_literals(formula: &And) -> Interpretation {
    let mut result = BTreeMap::<Atom, Option<bool>>::new();
    for clause in formula {
        for literal in clause {
            let optional_polarity = result
                .entry(literal.atom())
                .or_insert_with(|| Some(literal.polarity()));
            match optional_polarity {
                None => {}
                Some(polarity) => {
                    if *polarity != literal.polarity() {
                        *optional_polarity = None;
                    }
                }
            }
        }
    }
    result
        .iter()
        .filter_map(|(atom, optional_polarity)| optional_polarity.map(|polarity| (*atom, polarity)))
        .collect()
}

fn get_first_atom(formula: &And) -> Option<Atom> {
    for clause in formula {
        for literal in clause {
            return Some(literal.atom());
        }
    }
    None
}

fn assign_atom(formula: &And, atom: Atom, polarity: bool) -> And {
    let mut result = And::new();
    for clause in formula {
        assign_clause_atom(clause, atom, polarity).map(|clause| result.push(clause));
    }
    result
}

fn assign_clause_atom(clause: &Or, atom: Atom, polarity: bool) -> Option<Or> {
    let mut result = Or::new();
    for literal in clause {
        if literal.atom() == atom {
            if literal.polarity() == polarity {
                return None;
            }
        } else {
            result.push(*literal);
        }
    }
    Some(result)
}

fn assign_interpretation(formula: And, interpretation: &Interpretation) -> And {
    if interpretation.is_empty() {
        return formula;
    }
    let mut result = And::new();
    for clause in formula {
        if let Some(clause) = assign_clause_interpretation(&clause, interpretation) {
            result.push(clause);
        }
    }
    result
}

fn assign_clause_interpretation(clause: &Or, interpretation: &Interpretation) -> Option<Or> {
    let mut result = Or::new();
    for literal in clause {
        match interpretation.get(&literal.atom()) {
            None => result.push(*literal),
            Some(polarity) => {
                if literal.polarity() == *polarity {
                    return None;
                }
            }
        }
    }
    Some(result)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Ternary {
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

fn interpret(formula: &And, interpretation: &Interpretation) -> Ternary {
    formula.iter().fold(Ternary::True, |acc, clause| {
        acc & clause.iter().fold(Ternary::False, |acc, literal| {
            acc | {
                let atom_value = interpretation
                    .get(&literal.atom())
                    .map_or(Ternary::Unknown, |b| b.into());
                if literal.polarity() {
                    atom_value
                } else {
                    !atom_value
                }
            }
        })
    })
}

fn exponential_sat(formula: &And) -> Option<Interpretation> {
    fn go(
        formula: &And,
        interpretation: Interpretation,
        mut atoms_left: Vec<Atom>,
    ) -> Option<Interpretation> {
        if interpret(&formula, &interpretation) == Ternary::True {
            return Some(interpretation);
        }
        match atoms_left.pop() {
            None => None,
            Some(atom) => {
                let interpretation_before = interpretation.clone();
                let mut true_interpretation = interpretation;
                true_interpretation.insert(atom, true);
                match go(formula, true_interpretation, atoms_left.clone()) {
                    Some(interpretation) => Some(interpretation),
                    None => {
                        let mut false_interpretation = interpretation_before;
                        false_interpretation.insert(atom, false);
                        go(formula, false_interpretation, atoms_left)
                    }
                }
            }
        }
    }
    let mut atoms_left: Vec<Atom> = formula
        .iter()
        .flat_map(|clause| clause.iter().map(|literal| literal.atom()))
        .collect();
    atoms_left.sort();
    atoms_left.dedup();
    go(formula, Interpretation::new(), atoms_left)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn false_() -> And {
        vec![vec![]]
    }

    #[test]
    fn false_is_false() {
        assert!(dpll(false_()).is_none())
    }

    fn true_() -> And {
        vec![]
    }

    #[test]
    fn true_is_true() {
        assert_eq!(dpll(true_()), Some(Interpretation::new()))
    }

    fn single_positive_var() -> And {
        vec![vec![Literal::pos(Atom(0))]]
    }

    #[test]
    fn single_positive_var_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), true);
        assert_eq!(dpll(single_positive_var()), Some(interpretation),)
    }

    fn single_negative_var() -> And {
        vec![vec![Literal::neg(Atom(0))]]
    }

    #[test]
    fn single_negative_var_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), false);
        assert_eq!(dpll(single_negative_var()), Some(interpretation),)
    }

    fn pure_positive_polarity() -> And {
        vec![
            vec![Literal::pos(Atom(0))],
            vec![Literal::pos(Atom(0)), Literal::pos(Atom(1))],
        ]
    }

    #[test]
    fn pure_positive_polarity_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), true);
        assert_eq!(dpll(pure_positive_polarity()), Some(interpretation),)
    }

    fn pure_negative_polarity() -> And {
        vec![
            vec![Literal::neg(Atom(0))],
            vec![Literal::neg(Atom(0)), Literal::pos(Atom(1))],
        ]
    }

    #[test]
    fn pure_negative_polarity_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), false);
        assert_eq!(dpll(pure_negative_polarity()), Some(interpretation),)
    }

    fn backtracking_example() -> And {
        vec![
            vec![Literal::pos(Atom(0)), Literal::pos(Atom(1))],
            vec![Literal::neg(Atom(0)), Literal::pos(Atom(2))],
            vec![Literal::neg(Atom(1)), Literal::neg(Atom(2))],
        ]
    }

    #[test]
    fn backtracking_example_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), true);
        interpretation.insert(Atom(1), false);
        interpretation.insert(Atom(2), true);
        assert_eq!(dpll(backtracking_example()), Some(interpretation),)
    }

    #[quickcheck]
    fn dpll_correct(formula: And) -> bool {
        match dpll(formula.clone()) {
            None => exponential_sat(&formula).is_none(),
            Some(interpretation) => interpret(&formula, &interpretation) == Ternary::True,
        }
    }
}
