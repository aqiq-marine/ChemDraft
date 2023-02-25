#[derive(Debug, Clone)]
pub enum BondType {
    Single(SingleBond),
    Double(DoubleBond),
    Triple,
    Hidden,
}

impl BondType {
    pub fn get_bond_order(&self) -> i32 {
        self.into()
    }
}

impl Into<i32> for &BondType {
    fn into(self) -> i32 {
        match self {
            BondType::Single(_) => 1,
            BondType::Double(_) => 2,
            BondType::Triple => 3,
            BondType::Hidden => 0,
        }
    }
}

impl Into<i32> for BondType {
    fn into(self) -> i32 {
        (&self).into()
    }
}

impl Default for BondType {
    fn default() -> Self {
        Self::Single(SingleBond::default())
    }
}

#[derive(Debug, Default, Clone)]
pub enum SingleBond {
    #[default]
    Normal,
    Forward,
    Backward,
}

#[derive(Debug, Default, Clone)]
pub enum DoubleBond {
    #[default]
    Left,
    Right,
}
