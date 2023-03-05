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
    pub fn from_id(id: i32) -> Self {
        match id {
            1 => Self::Single(SingleBond::Normal),
            2 => Self::Single(SingleBond::Forward),
            3 => Self::Single(SingleBond::Backward),
            4 => Self::Single(SingleBond::Wide),
            5 => Self::Double(DoubleBond::Left),
            6 => Self::Double(DoubleBond::Center),
            7 => Self::Double(DoubleBond::Right),
            8 => Self::Triple,
            9 => Self::Hidden,
            _ => Self::default(),
        }
    }
    pub fn into_id(&self) -> i32 {
        match self {
            Self::Single(SingleBond::Normal) => 1,
            Self::Single(SingleBond::Forward) => 2,
            Self::Single(SingleBond::Backward) => 3,
            Self::Single(SingleBond::Wide) => 4,
            Self::Double(DoubleBond::Left) => 5,
            Self::Double(DoubleBond::Center) => 6,
            Self::Double(DoubleBond::Right) => 7,
            Self::Triple => 8,
            Self::Hidden => 9,
        }
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

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum SingleBond {
    #[default]
    Normal,
    Forward,
    Backward,
    Wide,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum DoubleBond {
    #[default]
    Left,
    Right,
    Center,
}
