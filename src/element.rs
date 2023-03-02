#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Element {
    Text(String),
    H ,                                                                 He, 
    Li, Be,                                         B , C , N , O , F , Ne, 
    Na, Mg,                                         Al, Si, P , S , Cl, Ar, 
    K , Ca, Sc, Ti, V , Cr, Mn, Fe, Co, Ni, Cu, Zn, Ga, Ge, As, Se, Br, Kr, 
}

impl Default for Element {
    fn default() -> Self {
        Element::C
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Nuclide {
    pub elm: Element,
    neutron_num: NeutronNum,
}

impl Default for Nuclide {
    fn default() -> Self {
        let elm = Element::C;
        let neutron_num = 6;
        Self {elm, neutron_num}
    }
}

impl From<Element> for Nuclide {
    fn from(elm: Element) -> Self {
        let neutron_num = match elm {
            Element::Text(_) => 0,
            Element::H => 0,
            Element::He => 2,
            Element::Li => 4,
            Element::Be => 5,
            Element::B => 6,
            Element::C => 6,
            Element::N => 7,
            Element::O => 8,
            Element::F => 10,
            Element::Ne => 10,
            Element::Na => 12,
            Element::Mg => 12,
            Element::Al => 14,
            Element::Si => 14,
            Element::P => 16,
            Element::S => 16,
            Element::Cl => 18,
            Element::Ar => 22,
            Element::K => 20,
            Element::Ca => 20,
            Element::Sc => 24,
            Element::Ti => 26,
            Element::V => 28,
            Element::Cr => 28,
            Element::Mn => 30,
            Element::Fe => 30,
            Element::Co => 32,
            Element::Ni => 30,
            Element::Cu => 34,
            Element::Zn => 36,
            Element::Ga => 39,
            Element::Ge => 40,
            Element::As => 42,
            Element::Se => 46,
            Element::Br => 44,
            Element::Kr => 48,
        };
        Self { elm, neutron_num }
    }
}

impl Element {
    pub fn get_atomic_number(&self) -> i32 {
        match self {
            Element::Text(_) => 0,
            Element::H => 1,
            Element::He => 2,
            Element::Li => 3,
            Element::Be => 4,
            Element::B => 5,
            Element::C => 6,
            Element::N => 7,
            Element::O => 8,
            Element::F => 9,
            Element::Ne => 10,
            Element::Na => 11,
            Element::Mg => 12,
            Element::Al => 13,
            Element::Si => 14,
            Element::P => 15,
            Element::S => 16,
            Element::Cl => 17,
            Element::Ar => 18,
            Element::K => 19,
            Element::Ca => 20,
            Element::Sc => 21,
            Element::Ti => 22,
            Element::V => 23,
            Element::Cr => 24,
            Element::Mn => 25,
            Element::Fe => 26,
            Element::Co => 27,
            Element::Ni => 28,
            Element::Cu => 29,
            Element::Zn => 30,
            Element::Ga => 31,
            Element::Ge => 32,
            Element::As => 33,
            Element::Se => 34,
            Element::Br => 35,
            Element::Kr => 36,
        }
    }

    pub fn from_atomic_number(number: i32) -> Option<Element> {
        match number {
            1 => Some(Element::H),
            2 => Some(Element::He),
            3 => Some(Element::Li),
            4 => Some(Element::Be),
            5 => Some(Element::B),
            6 => Some(Element::C),
            7 => Some(Element::N),
            8 => Some(Element::O),
            9 => Some(Element::F),
            10 => Some(Element::Ne),
            11 => Some(Element::Na),
            12 => Some(Element::Mg),
            13 => Some(Element::Al),
            14 => Some(Element::Si),
            15 => Some(Element::P),
            16 => Some(Element::S),
            17 => Some(Element::Cl),
            18 => Some(Element::Ar),
            19 => Some(Element::K),
            20 => Some(Element::Ca),
            21 => Some(Element::Sc),
            22 => Some(Element::Ti),
            23 => Some(Element::V),
            24 => Some(Element::Cr),
            25 => Some(Element::Mn),
            26 => Some(Element::Fe),
            27 => Some(Element::Co),
            28 => Some(Element::Ni),
            29 => Some(Element::Cu),
            30 => Some(Element::Zn),
            31 => Some(Element::Ga),
            32 => Some(Element::Ge),
            33 => Some(Element::As),
            34 => Some(Element::Se),
            35 => Some(Element::Br),
            36 => Some(Element::Kr),
            _ => None,
        }
    }
    pub fn get_valences(&self) -> i32 {
        match self {
            Element::H | Element::Li | Element::Na | Element::K => 1,
            Element::B | Element::Al | Element::Ga => 3,
            Element::C | Element::Si | Element::Ge => 4,
            Element::N | Element::P | Element::As => 3,
            Element::O | Element::S | Element::Se => 2,
            Element::F | Element::Cl | Element::Br => 1,
            Element::He | Element::Ne | Element::Ar | Element::Kr => 0,
            _ => 1,
        }
    }
    pub fn get_lone_pair(&self) -> i32 {
        match self {
            Element::N | Element::P | Element::As => 1,
            Element::O | Element::S | Element::Se => 2,
            Element::F | Element::Cl | Element::Br => 3,
            _ => 0,
        }
    }
    pub fn symbol(&self) -> String {
        let symbol = match self {
            Element::Text(text) => text.as_str(),
            Element::H => "H",
            Element::He => "He",
            Element::Li => "Li",
            Element::Be => "Be",
            Element::B => "B",
            Element::C => "C",
            Element::N => "N",
            Element::O => "O",
            Element::F => "F",
            Element::Ne => "Ne",
            Element::Na => "Na",
            Element::Mg => "Mg",
            Element::Al => "Al",
            Element::Si => "Si",
            Element::P => "P",
            Element::S => "S",
            Element::Cl => "Cl",
            Element::Ar => "Ar",
            Element::K => "K",
            Element::Ca => "Ca",
            Element::Sc => "Sc",
            Element::Ti => "Ti",
            Element::V => "V",
            Element::Cr => "Cr",
            Element::Mn => "Mn",
            Element::Fe => "Fe",
            Element::Co => "Co",
            Element::Ni => "Ni",
            Element::Cu => "Cu",
            Element::Zn => "Zn",
            Element::Ga => "Ga",
            Element::Ge => "Ge",
            Element::As => "As",
            Element::Se => "Se",
            Element::Br => "Br",
            Element::Kr => "Kr",
        };
        String::from(symbol)
    }
    pub fn from_symbol(symbol: &str) -> Option<Element> {
        match symbol {
            "H" => Some(Element::H),
            "He" => Some(Element::He),
            "Li" => Some(Element::Li),
            "Be" => Some(Element::Be),
            "B" => Some(Element::B),
            "C" => Some(Element::C),
            "N" => Some(Element::N),
            "O" => Some(Element::O),
            "F" => Some(Element::F),
            "Ne" => Some(Element::Ne),
            "Na" => Some(Element::Na),
            "Mg" => Some(Element::Mg),
            "Al" => Some(Element::Al),
            "Si" => Some(Element::Si),
            "P" => Some(Element::P),
            "S" => Some(Element::S),
            "Cl" => Some(Element::Cl),
            "Ar" => Some(Element::Ar),
            "K" => Some(Element::K),
            "Ca" => Some(Element::Ca),
            "Sc" => Some(Element::Sc),
            "Ti" => Some(Element::Ti),
            "V" => Some(Element::V),
            "Cr" => Some(Element::Cr),
            "Mn" => Some(Element::Mn),
            "Fe" => Some(Element::Fe),
            "Co" => Some(Element::Co),
            "Ni" => Some(Element::Ni),
            "Cu" => Some(Element::Cu),
            "Zn" => Some(Element::Zn),
            "Ga" => Some(Element::Ga),
            "Ge" => Some(Element::Ge),
            "As" => Some(Element::As),
            "Se" => Some(Element::Se),
            "Br" => Some(Element::Br),
            "Kr" => Some(Element::Kr),
            _ => None,
        }
    }
}

// pub type Charge = i32;
pub type NeutronNum = i32;
