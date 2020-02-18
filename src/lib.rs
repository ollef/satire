use std::collections::BTreeMap;

#[derive(Ord, PartialOrd, Clone, Copy, Debug, Eq, PartialEq)]
pub struct Atom(pub i32);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Literal {
    Pos(Atom),
    Neg(Atom),
}

impl Literal {
    fn atom(&self) -> Atom {
        match self {
            Literal::Pos(atom) => *atom,
            Literal::Neg(atom) => *atom,
        }
    }

    fn polarity(&self) -> bool {
        match self {
            Literal::Pos(_) => true,
            Literal::Neg(_) => false,
        }
    }
}

type Or = Vec<Literal>;

type And = Vec<Or>;

type Interpretation = BTreeMap<Atom, bool>;

pub fn dpll(formula: And, mut interpretation: Interpretation) -> Option<Interpretation> {
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

        if is_true(&formula) {
            return Some(interpretation);
        }

        dbg!(&formula);
        match get_first_atom(&formula) {
            None => dpll(formula, interpretation),
            Some(atom) => {
                dbg!(&atom);
                let true_formula = assign_atom(&formula, atom, true);
                let interpretation_before = interpretation.clone();
                let mut true_interpretation = interpretation;
                true_interpretation.insert(atom, true);
                match dpll(true_formula, true_interpretation) {
                    Some(interpretation) => Some(interpretation),
                    None => {
                        dbg!(&atom);
                        let false_formula = assign_atom(&formula, atom, false);
                        let mut false_interpretation = interpretation_before;
                        false_interpretation.insert(atom, false);
                        dpll(false_formula, false_interpretation)
                    }
                }
            }
        }
    }
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
            match result.get_mut(&literal.atom()) {
                Some(optional_polarity) => match optional_polarity {
                    None => {}
                    Some(polarity) => {
                        if *polarity != literal.polarity() {
                            *optional_polarity = None;
                        }
                    }
                },
                None => {
                    result.insert(literal.atom(), Some(literal.polarity()));
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
        assign_clause_interpretation(&clause, interpretation).map(|clause| result.push(clause));
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

#[cfg(test)]
mod tests {
    use super::*;

    fn false_() -> And {
        vec![vec![]]
    }

    #[test]
    fn false_is_false() {
        assert!(dpll(false_(), Interpretation::new()).is_none())
    }

    fn true_() -> And {
        vec![]
    }

    #[test]
    fn true_is_true() {
        assert_eq!(
            dpll(true_(), Interpretation::new()),
            Some(Interpretation::new())
        )
    }

    fn single_positive_var() -> And {
        vec![vec![Literal::Pos(Atom(0))]]
    }

    #[test]
    fn single_positive_var_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), true);
        assert_eq!(
            dpll(single_positive_var(), Interpretation::new()),
            Some(interpretation),
        )
    }

    fn single_negative_var() -> And {
        vec![vec![Literal::Neg(Atom(0))]]
    }

    #[test]
    fn single_negative_var_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), false);
        assert_eq!(
            dpll(single_negative_var(), Interpretation::new()),
            Some(interpretation),
        )
    }

    fn pure_positive_polarity() -> And {
        vec![
            vec![Literal::Pos(Atom(0))],
            vec![Literal::Pos(Atom(0)), Literal::Pos(Atom(1))],
        ]
    }

    #[test]
    fn pure_positive_polarity_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), true);
        assert_eq!(
            dpll(pure_positive_polarity(), Interpretation::new()),
            Some(interpretation),
        )
    }

    fn pure_negative_polarity() -> And {
        vec![
            vec![Literal::Neg(Atom(0))],
            vec![Literal::Neg(Atom(0)), Literal::Pos(Atom(1))],
        ]
    }

    #[test]
    fn pure_negative_polarity_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), false);
        assert_eq!(
            dpll(pure_negative_polarity(), Interpretation::new()),
            Some(interpretation),
        )
    }

    fn backtracking_example() -> And {
        vec![
            vec![Literal::Pos(Atom(0)), Literal::Pos(Atom(1))],
            vec![Literal::Neg(Atom(0)), Literal::Pos(Atom(2))],
            vec![Literal::Neg(Atom(1)), Literal::Neg(Atom(2))],
        ]
    }

    #[test]
    fn backtracking_example_solvable() {
        let mut interpretation = Interpretation::new();
        interpretation.insert(Atom(0), true);
        interpretation.insert(Atom(1), false);
        interpretation.insert(Atom(2), true);
        assert_eq!(
            dpll(backtracking_example(), Interpretation::new()),
            Some(interpretation),
        )
    }
}
