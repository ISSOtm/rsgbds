use std::ops::Not;

#[derive(Debug)]
pub enum Reg8 {
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    HlInd = 6,
    A = 7,
}

#[derive(Debug)]
pub enum Reg16 {
    Bc = 0,
    De = 1,
    Hl = 2,
    Sp = 3,
}

#[derive(Debug)]
pub enum Reg16Ind {
    Bc = 0,
    De = 1,
    Hli = 2,
    Hld = 3,
}

#[derive(Debug)]
pub enum Reg16Stack {
    Bc = 0,
    De = 1,
    Hl = 2,
    Af = 3,
}

#[derive(Debug)]
pub enum Condition {
    Nz = 0,
    Z = 1,
    Nc = 2,
    C = 3,
}

impl Not for Condition {
    type Output = Condition;

    fn not(self) -> Self::Output {
        match self {
            Condition::Nz => Condition::Z,
            Condition::Z => Condition::Nz,
            Condition::Nc => Condition::C,
            Condition::C => Condition::Nc,
        }
    }
}
