#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

mod ternary;

#[cfg(test)]
use quickcheck::{Arbitrary, Gen};
use std::collections::BTreeMap;
use std::ops::BitAnd;
use ternary::Ternary;

#[derive(Ord, PartialOrd, Clone, Copy, Debug, Eq, PartialEq)]
pub struct Atom(pub u32);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Literal {
    value: u32,
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
        Literal::new(Atom(u32::arbitrary(g) % 8), bool::arbitrary(g))
    }
}

type Or = Vec<Literal>;

type And = Vec<Or>;

#[derive(Clone, Debug)]
pub struct Formula {
    and: And,
}

type Interpretation = BTreeMap<Atom, bool>;

pub fn dpll(formula: &Formula) -> Option<Interpretation> {
    fn go(formula: &Formula, mut interpretation: Interpretation) -> Option<Interpretation> {
        dbg!(&formula);
        if formula.is_true() {
            Some(interpretation)
        } else if formula.is_false() {
            None
        } else {
            let mut unit_clauses = formula.unit_clauses();
            dbg!(&unit_clauses);
            let formula = formula.assign(&unit_clauses);
            interpretation.append(&mut unit_clauses);

            let mut pure_literals = formula.pure_literals();
            dbg!(&pure_literals);
            let formula = formula.assign(&pure_literals);
            interpretation.append(&mut pure_literals);

            dbg!(&formula);
            match formula.first_atom() {
                None => go(&formula, interpretation),
                Some(atom) => {
                    dbg!(&atom);
                    let true_formula = formula.assign_atom(atom, true);
                    let interpretation_before = interpretation.clone();
                    let mut true_interpretation = interpretation;
                    true_interpretation.insert(atom, true);
                    match go(&true_formula, true_interpretation) {
                        Some(interpretation) => Some(interpretation),
                        None => {
                            dbg!(&atom);
                            let false_formula = formula.assign_atom(atom, false);
                            let mut false_interpretation = interpretation_before;
                            false_interpretation.insert(atom, false);
                            go(&false_formula, false_interpretation)
                        }
                    }
                }
            }
        }
    }
    go(formula, Interpretation::new())
}

impl Formula {
    pub fn false_() -> Formula {
        Formula { and: vec![vec![]] }
    }

    pub fn true_() -> Formula {
        Formula { and: vec![] }
    }

    pub fn is_true(&self) -> bool {
        self.and.is_empty()
    }

    pub fn is_false(&self) -> bool {
        self.and.iter().any(|clause| clause.is_empty())
    }

    fn unit_clauses(&self) -> Interpretation {
        let mut result = Interpretation::new();
        for clause in &self.and {
            if clause.len() == 1 {
                result.insert(clause[0].atom(), clause[0].polarity());
            }
        }
        result
    }

    fn pure_literals(&self) -> Interpretation {
        let mut result = BTreeMap::<Atom, Option<bool>>::new();
        for clause in &self.and {
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
            .filter_map(|(atom, optional_polarity)| {
                optional_polarity.map(|polarity| (*atom, polarity))
            })
            .collect()
    }

    fn first_atom(&self) -> Option<Atom> {
        for clause in &self.and {
            for literal in clause {
                return Some(literal.atom());
            }
        }
        None
    }

    fn assign_atom(&self, atom: Atom, polarity: bool) -> Formula {
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

        let mut result = Formula::true_();
        for clause in &self.and {
            if let Some(clause) = assign_clause_atom(&clause, atom, polarity) {
                result.and.push(clause);
            }
        }
        result
    }

    fn assign(&self, interpretation: &Interpretation) -> Formula {
        fn assign_clause(clause: &Or, interpretation: &Interpretation) -> Option<Or> {
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
        if interpretation.is_empty() {
            return self.clone();
        }
        let mut result = Formula::true_();
        for clause in &self.and {
            if let Some(clause) = assign_clause(&clause, interpretation) {
                result.and.push(clause);
            }
        }
        result
    }

    pub fn interpret(&self, interpretation: &Interpretation) -> Ternary {
        self.and.iter().fold(Ternary::True, |acc, clause| {
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
}

#[cfg(test)]
impl Arbitrary for Formula {
    fn arbitrary<G: Gen>(g: &mut G) -> Formula {
        Formula {
            and: And::arbitrary(g),
        }
    }
}

impl BitAnd<Formula> for Formula {
    type Output = Formula;
    fn bitand(mut self, other: Formula) -> Formula {
        self.and.extend(other.and);
        self
    }
}

pub fn exponential_sat(formula: &Formula) -> Option<Interpretation> {
    fn go(
        formula: &Formula,
        interpretation: Interpretation,
        mut atoms_left: Vec<Atom>,
    ) -> Option<Interpretation> {
        if formula.interpret(&interpretation) == Ternary::True {
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
        .and
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

    #[test]
    fn false_is_false() {
        assert!(dpll(&Formula::false_()).is_none())
    }

    #[test]
    fn true_is_true() {
        assert_eq!(dpll(&Formula::true_()), Some(Interpretation::new()))
    }

    fn single_positive_var() -> Formula {
        Formula {
            and: vec![vec![Literal::pos(Atom(0))]],
        }
    }

    #[test]
    fn single_positive_var_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), true);
        assert_eq!(dpll(&single_positive_var()), Some(interpretation),)
    }

    fn single_negative_var() -> Formula {
        Formula {
            and: vec![vec![Literal::neg(Atom(0))]],
        }
    }

    #[test]
    fn single_negative_var_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), false);
        assert_eq!(dpll(&single_negative_var()), Some(interpretation),)
    }

    fn pure_positive_polarity() -> Formula {
        Formula {
            and: vec![
                vec![Literal::pos(Atom(0))],
                vec![Literal::pos(Atom(0)), Literal::pos(Atom(1))],
            ],
        }
    }

    #[test]
    fn pure_positive_polarity_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), true);
        assert_eq!(dpll(&pure_positive_polarity()), Some(interpretation),)
    }

    fn pure_negative_polarity() -> Formula {
        Formula {
            and: vec![
                vec![Literal::neg(Atom(0))],
                vec![Literal::neg(Atom(0)), Literal::pos(Atom(1))],
            ],
        }
    }

    #[test]
    fn pure_negative_polarity_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), false);
        assert_eq!(dpll(&pure_negative_polarity()), Some(interpretation),)
    }

    fn backtracking_example() -> Formula {
        Formula {
            and: vec![
                vec![Literal::pos(Atom(0)), Literal::pos(Atom(1))],
                vec![Literal::neg(Atom(0)), Literal::pos(Atom(2))],
                vec![Literal::neg(Atom(1)), Literal::neg(Atom(2))],
            ],
        }
    }

    #[test]
    fn backtracking_example_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), true);
        interpretation.insert(Atom(1), false);
        interpretation.insert(Atom(2), true);
        assert_eq!(dpll(&backtracking_example()), Some(interpretation),)
    }

    #[quickcheck]
    fn dpll_correct(formula: Formula) -> bool {
        match dpll(&formula) {
            None => exponential_sat(&formula).is_none(),
            Some(interpretation) => formula.interpret(&interpretation) == Ternary::True,
        }
    }

    fn xor(a: Atom, b: Atom) -> Formula {
        Formula {
            and: vec![
                vec![Literal::pos(a), Literal::pos(b)],
                vec![Literal::neg(a), Literal::neg(b)],
            ],
        }
    }

    #[test]
    fn xor_correct() {
        let truth_table = vec![
            (false, false, false),
            (true, false, true),
            (false, true, true),
            (true, true, false),
        ];
        for (a, b, result) in truth_table {
            assert_eq!(
                xor(Atom(0), Atom(1))
                    .interpret(&vec![(Atom(0), a), (Atom(1), b)].into_iter().collect(),),
                result.into()
            );
        }
    }

    fn different_neighbors(atoms: &[Atom]) -> Formula {
        let mut result = Formula::true_();
        for (i, atom) in atoms.iter().enumerate() {
            let j = (i + 1) % atoms.len();
            result = result & xor(*atom, atoms[j]);
        }
        result
    }

    #[test]
    fn different_neighbors_correct() {
        for i in 0..100 {
            let atoms = (0..i).map(|x| Atom(x)).collect::<Vec<_>>();
            if i % 2 == 0 {
                assert!(dpll(&different_neighbors(&atoms)).is_some());
            } else {
                assert_eq!(dpll(&different_neighbors(&atoms)), None);
            }
        }
    }

    fn one_hot(atoms: &[Atom]) -> Formula {
        //let xor_reduce = atoms.iter().fold(Formula::false_(), |acc, x| xor(acc, x));
    }
}

mod prop {
    use super::Atom;

    #[derive(Clone)]
    enum Formula {
        Atom(Atom),
        Neg(Box<Formula>),
        And(Box<Formula>, Box<Formula>),
        Or(Box<Formula>, Box<Formula>),
    }

    fn xor(lhs: Formula, rhs: Formula) -> Formula {
        Formula::And(
            Box::new(Formula::Or(Box::new(lhs.clone()), Box::new(rhs.clone()))),
            Box::new(Formula::Neg(Box::new(Formula::And(
                Box::new(lhs),
                Box::new(rhs),
            )))),
        )
    }
}
