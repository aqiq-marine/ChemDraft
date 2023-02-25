use crate::bond_type::{BondType, DoubleBond, SingleBond};
use crate::element::Element;
use crate::vector::Vector;
use std::collections::HashMap;
use std::f64::consts::PI;

#[derive(Debug, Clone)]
pub struct Atom {
    elm: Element,
    charge: i32,
    p: Vector,
}

impl Default for Atom {
    fn default() -> Self {
        let p = Vector::new(512.0, 512.0);
        Atom {
            elm: Element::default(),
            charge: 0,
            p,
        }
    }
}

impl Atom {
    pub fn new(elm: Element, p: Vector) -> Atom {
        Atom { elm, charge: 0, p }
    }
    pub fn get_point(&self) -> Vector {
        self.p.clone()
    }
    pub fn get_element(&self) -> Element {
        self.elm.clone()
    }
    pub fn move_atom(&mut self, v: &Vector) {
        self.p = &self.p + v;
    }
    pub fn rotate(&mut self, theta: f64, origin: &Vector) {
        let v = (&self.p - origin).rotate(theta);
        self.p = origin + v;
    }
    pub fn increase_charge(&mut self, increase: i32) {
        self.charge += increase;
    }
}

struct UnionFind {
    dict: HashMap<AtomId, AtomId>,
}
impl UnionFind {
    pub fn new(atoms: Vec<AtomId>) -> Self {
        let dict = HashMap::from_iter(atoms.iter().map(|&id| (id, id)));
        Self { dict }
    }
    pub fn find(&mut self, id: AtomId) -> AtomId {
        let root = self.dict.get(&id).cloned();
        let root = root.map(|parent| {
            if parent == id {
                id
            } else {
                let root = self.find(parent);
                self.dict.get_mut(&id).map(|r| {
                    *r = root;
                });
                root
            }
        });
        root.unwrap_or(id)
    }
    pub fn unite(&mut self, atom1: AtomId, atom2: AtomId) {
        let root1 = self.find(atom1);
        let root2 = self.find(atom2);
        let exist_root1 = self.dict.contains_key(&root1);
        self.dict.get_mut(&root2).map(|r| {
            if exist_root1 {
                *r = root1;
            }
        });
    }
}

#[derive(Debug, Default, Clone)]
pub enum DisplayMode {
    AllElms,
    #[default]
    CarbonOmit,
}

#[derive(Debug)]
pub struct ChemStruct {
    atoms: HashMap<AtomId, Atom>,
    bonds: HashMap<(AtomId, AtomId), BondType>,
    next_id: AtomId,
    display_mode: DisplayMode,
    bond_len: f64,
}

impl Default for ChemStruct {
    fn default() -> Self {
        let bond_len = 50.0;
        let atoms = {
            // let mut atoms = HashMap::new();
            // let mut p = Vector::new(256.0, 256.0);
            // let v = bond_len * Vector::new(1.0, 0.0).rotate(-PI / 6.0);
            // for i in 0..8 {
            //     p = p + v.rotate((i % 2) as f64 * PI / 3.0);
            // }
            HashMap::new()
        };
        let bonds = HashMap::new();
        let next_id = 1;
        let display_mode = DisplayMode::default();
        ChemStruct {
            atoms,
            bonds,
            next_id,
            display_mode,
            bond_len,
        }
    }
}

impl ChemStruct {
    pub fn new_atom(&mut self, elm: Element) -> AtomId {
        let p = Vector::new(512.0, 256.0);
        let new_atom = Atom::new(elm, p);
        let new_atom_id = self.next_id;
        self.atoms.insert(new_atom_id, new_atom);
        self.next_id += 1;
        self.compensate_hydrogen(&new_atom_id, 0);
        return new_atom_id;
    }
    pub fn append(&mut self, atom_id: &AtomId, elm: Element) -> Option<AtomId> {
        if !self.atoms.contains_key(atom_id) {
            return None;
        }
        self.delete_hydrogen(atom_id);
        let new_point = self.calc_append_position(atom_id);
        let new_atom = Atom::new(elm, new_point);
        let new_atom_id = self.next_id;
        self.atoms.insert(new_atom_id, new_atom);
        self.bonds.insert(
            (*atom_id, new_atom_id),
            BondType::Single(SingleBond::Normal),
        );
        self.next_id += 1;
        self.compensate_hydrogen(&new_atom_id, 1);
        return Some(new_atom_id);
    }
    pub fn get_point(&self, atom_id: AtomId) -> Option<Vector> {
        self.atoms.get(&atom_id).map(|atom| atom.get_point())
    }
    pub fn delete(&mut self, atom_id: &AtomId) -> Option<AtomId> {
        println!("{}", self.atoms.len());
        if !self.atoms.contains_key(atom_id) {
            return None;
        }
        let is_h = if let Element::H(_) = self.atoms[atom_id].elm {
            true
        } else {
            false
        };
        self.atoms.remove(&atom_id);
        let connected_bonds = self.get_connected_bonds(atom_id);
        let mut connected_atom = None;
        for bond in connected_bonds {
            let other = Self::get_other(&bond, atom_id);
            self.bonds.remove(&bond);
            if let Element::H(_) = self.atoms[&other].elm {
                // 結合は先に削除してある
                // 孤立している水素を削除
                if !is_h && self.get_connected_bonds(&other).len() == 0 {
                    self.atoms.remove(&other);
                    continue;
                }
            }
            connected_atom = connected_atom.or(Some(other));
        }
        return connected_atom;
    }
    pub fn delete_bond(&mut self, bond_id: &(AtomId, AtomId)) {
        self.bonds.remove(bond_id);
    }
    pub fn get_atom_in_rect(&self, left_top: &Vector, right_bot: &Vector) -> Option<AtomId> {
        let mut hydrogen = None;
        let mut other_elm = None;
        for (id, atom) in self.atoms.iter() {
            if atom.get_point().is_in_rect(left_top, right_bot) {
                if let Element::H(_) = self.atoms[id].elm {
                    hydrogen = Some(*id);
                } else {
                    other_elm = Some(*id);
                }
            }
        }
        return other_elm.or(hydrogen);
    }
    pub fn move_atom(&mut self, atom_id: &AtomId, v: &Vector) {
        self.atoms.get_mut(atom_id).map(|atom| atom.move_atom(v));
    }
    pub fn rotate(&mut self, target: AtomId, origin: AtomId, theta: f64) {
        if !self.atoms.contains_key(&target) || !self.atoms.contains_key(&origin) {
            return;
        }
        let mut uf = UnionFind::new(self.atoms.clone().into_keys().collect());
        for ((atom1, atom2), _) in self.bonds.iter() {
            let atom1 = *atom1;
            let atom2 = *atom2;
            if atom1 == target && atom2 == origin || atom1 == origin && atom2 == target {
                continue;
            }
            uf.unite(atom1, atom2);
        }
        let origin = self.atoms[&origin].get_point();
        let target_root = uf.find(target);
        for (id, atom) in self.atoms.iter_mut() {
            let r = uf.find(*id);
            if r == target_root {
                atom.rotate(theta, &origin);
            }
        }
    }
    pub fn change_bond(&mut self, bond_id: &(AtomId, AtomId), bond_type: BondType) {
        self.bonds.get_mut(bond_id).map(|v| {
            *v = bond_type;
        });
        self.recompensate(&bond_id.0);
        // recompensateで削除されていないか
        let exist_atom = self.atoms.contains_key(&bond_id.1);
        if exist_atom {
            self.recompensate(&bond_id.1);
        }
    }
    pub fn reverse_bond(&mut self, bond_id: &(AtomId, AtomId)) -> Option<(AtomId, AtomId)> {
        self.bonds.remove(bond_id)
            .map(|bond| {
                let new_bond_id = (bond_id.1, bond_id.0);
                self.bonds.insert(new_bond_id, bond);
                new_bond_id
            })
    }
    pub fn calc_next_bond(&self, origin: &AtomId, bond_id: &(AtomId, AtomId)) -> Option<(AtomId, AtomId)> {
        self.calc_best_angle_bond(origin, bond_id, |cur, acc| cur > acc, 0.0)
    }
    pub fn calc_prev_bond(&self, origin: &AtomId, bond_id: &(AtomId, AtomId)) -> Option<(AtomId, AtomId)> {
        self.calc_best_angle_bond(origin, bond_id, |cur, acc| cur < acc, 2.0 * PI)
    }
    fn calc_best_angle_bond(
        &self,
        origin: &AtomId,
        bond_id: &(AtomId, AtomId),
        cmp_f: fn(f64, f64) -> bool,
        worst_theta: f64,
    ) -> Option<(AtomId, AtomId)> {
        if !self.atoms.contains_key(origin) || !self.bonds.contains_key(bond_id) {
            return None;
        }
        let atoms = self.get_connected_atoms(origin);
        let cur_atom = Self::get_other(bond_id, origin);
        let origin_p = &self.atoms[origin].get_point();
        let cur_vec = (self.atoms[&cur_atom].get_point() - origin_p).norm();
        let mut best_theta = worst_theta;
        let mut best_atom = cur_atom;
        for a in atoms {
            if a == cur_atom {
                continue;
            }
            let v = self.atoms[&a].get_point() - origin_p;
            let theta = cur_vec.calc_angle(&v);
            if cmp_f(theta, best_theta) {
                best_theta = theta;
                best_atom = a;
            }
        }
        let bond = if self.bonds.contains_key(&(*origin, best_atom)) {
            (*origin, best_atom)
        } else {
            (best_atom, *origin)
        };
        return Some(bond);
    }
    pub fn increase_charge(&mut self, atom_id: &AtomId, increase: i32) {
        self.atoms.get_mut(atom_id)
            .map(|atom| atom.increase_charge(increase));
    }
    pub fn connect_atoms(&mut self, atom1: &AtomId, atom2: &AtomId) -> Option<(AtomId, AtomId)> {
        if !self.atoms.contains_key(atom1) || !self.atoms.contains_key(atom2) || atom1 == atom2 {
            return None;
        }
        let bond = (*atom1, *atom2);
        self.bonds.insert(bond, BondType::default());
        self.recompensate(atom1);
        // recompensateで削除されたか
        // 削除されることある？
        let exist_atom = self.atoms.contains_key(atom2);
        if exist_atom {
            self.recompensate(atom2);
        }
        if self.bonds.contains_key(&bond) {
            return Some(bond);
        } else {
            return None;
        }
    }
    fn recompensate(&mut self, atom_id: &AtomId) {
        for _ in 0..3 {
            self.delete_hydrogen(atom_id);
        }
        self.compensate_hydrogen(atom_id, self.calc_connected_bond_num(atom_id));
    }
    fn calc_append_position(&self, atom_id: &AtomId) -> Vector {
        match self.display_mode {
            DisplayMode::AllElms => self.calc_append_pos_on_all_elms(atom_id),
            DisplayMode::CarbonOmit => self.calc_append_pos_on_carbon_omit(atom_id),
        }
    }
    fn calc_append_pos_on_all_elms(&self, atom_id: &AtomId) -> Vector {
        let atom = &self.atoms[atom_id];
        let atoms = self.get_connected_atoms(atom_id);
        let append_vec = match atoms.len() {
            0 => Vector::new(self.bond_len, 0.0),
            1 => -self.bond_len * (self.atoms[&atoms[0]].get_point() - atom.get_point()).norm(),
            2 => {
                self.bond_len
                    * (self.atoms[&atoms[0]].get_point() - atom.get_point())
                        .norm()
                        .rotate(PI / 2.0)
            }
            _ => {
                let mut v = Vector::new(0.0, 0.0);
                for a in atoms.iter() {
                    v = v + self.atoms[a].get_point() - atom.get_point();
                }
                if v.dist2() == 0.0 {
                    self.bond_len * Vector::new(0.0, 1.0).norm()
                } else {
                    -self.bond_len * v.norm()
                }
            }
        };
        let append_vec = &append_vec;
        let append_pos = atom.get_point() + append_vec;
        return append_pos;
    }
    fn calc_append_pos_on_carbon_omit(&self, atom_id: &AtomId) -> Vector {
        let atom = &self.atoms[atom_id];
        let atoms = self.get_connected_atoms(atom_id);
        let append_vec = self.bond_len
            * match atoms.len() {
                0 => Vector::new(1.0, 0.0).rotate(PI / 6.0),
                1 => (self.atoms[&atoms[0]].get_point() - atom.get_point())
                    .rotate(2.0 * PI / 3.0)
                    .norm(),
                3 => {
                    let x = &Vector::new(1.0, 0.0);
                    let calc_simi = |v: &Vector| {
                        let sharpness = 4;
                        atoms.iter().fold(0.0, |acc, cur| {
                            let simi = (self.atoms[cur].get_point() - atom.get_point())
                                .norm()
                                .dot(v)
                                .powi(2 * sharpness);
                            acc + simi
                        })
                    };
                    let basic_direc = vec![
                        x.rotate(PI / 6.0),
                        x.rotate(5.0 * PI / 6.0),
                        x.rotate(-PI / 2.0 + PI / 12.0),
                        x.rotate(-PI / 2.0 - PI / 12.0),
                    ];
                    let similarity = basic_direc.iter().map(calc_simi);
                    // 適当な大きい数
                    let mut least_simi = 100.0;
                    let mut least_simi_index = 0;
                    for (i, simi) in similarity.enumerate() {
                        if simi < least_simi {
                            least_simi = simi;
                            least_simi_index = i;
                        }
                    }
                    let v = basic_direc[least_simi_index].clone();
                    let v0 = (self.atoms[&atoms[0]].get_point() - atom.get_point()).norm();
                    let mp = basic_direc
                        .iter()
                        .fold(0.0, |acc: f64, cur| {
                            let simi = v0.dot(cur);
                            if simi.abs() > acc.abs() {
                                simi
                            } else {
                                acc
                            }
                        })
                        .signum();
                    mp * v
                }
                _ => {
                    let mut v = Vector::new(0.0, 0.0);
                    for a in atoms.iter() {
                        v = v + self.atoms[a].get_point() - atom.get_point();
                    }
                    if v.dist2() == 0.0 {
                        Vector::new(1.0, 0.0)
                    } else {
                        -v.norm()
                    }
                }
            };
        let append_pos = atom.get_point() + append_vec;
        return append_pos;
    }
    fn get_connected_atoms(&self, atom_id: &AtomId) -> Vec<AtomId> {
        let bonds = self.get_connected_bonds(atom_id);
        let mut result = vec![];
        for (atom1, atom2) in bonds {
            let other = if atom1 == *atom_id { atom2 } else { atom1 };
            result.push(other);
        }
        return result;
    }
    fn get_connected_atoms_except_h(&self, atom_id: &AtomId) -> Vec<AtomId> {
        self.get_connected_atoms(atom_id)
            .into_iter()
            .filter(|&atom| match self.atoms[&atom].elm {
                Element::H(_) => false,
                _ => true,
            })
            .collect()
    }
    fn compensate_hydrogen(&mut self, atom_id: &AtomId, bond_order: i32) {
        let v = self.atoms[atom_id].elm.get_valences() - bond_order;
        for _ in 0..v {
            self.append(atom_id, Element::H(0));
        }
    }
    fn delete_hydrogen(&mut self, atom_id: &AtomId) {
        let bond_num = self.calc_connected_bond_num(atom_id);
        let atom = &self.atoms[atom_id];
        let val = atom.get_element().get_valences() - atom.charge;
        if bond_num - val < 0 {
            return;
        }
        if let Some(hydrogen) = self.get_connected_hydrogen(atom_id) {
            let h_bond = self.calc_connected_bond_num(&hydrogen);
            if h_bond != 1 {
                return;
            }
            self.atoms.remove(&hydrogen);
            if self.bonds.contains_key(&(*atom_id, hydrogen)) {
                self.bonds.remove(&(*atom_id, hydrogen));
            } else {
                self.bonds.remove(&(hydrogen, *atom_id));
            }
        }
    }
    fn calc_connected_bond_num(&self, atom_id: &AtomId) -> i32 {
        self.get_connected_bonds(atom_id).iter()
            .fold(0, |acc, cur| {
                acc + self.bonds.get(cur)
                    .unwrap_or(&BondType::Hidden)
                    .get_bond_order()
            })
    }
    fn get_connected_hydrogen(&self, atom_id: &AtomId) -> Option<AtomId> {
        let bonds = self.get_connected_bonds(atom_id);
        let mut dot = -1.0;
        let axis = &Vector::new(self.bond_len, 0.0);
        let mut result = None;
        for (atom1, atom2) in bonds {
            let mut other = atom1;
            let v = self.atoms[&atom2].get_point() - self.atoms[&atom1].get_point();
            if &atom1 == atom_id {
                other = atom2;
            }
            if let Element::H(_) = self.atoms[&other].elm {
                let bond = self.get_connected_bonds(&other);
                let td = axis.dot(&v).abs();
                if bond.len() == 1 && td > dot {
                    result = Some(other);
                    dot = td;
                }
            }
        }
        return result;
    }
    pub fn get_connected_bonds(&self, atom_id: &AtomId) -> Vec<(AtomId, AtomId)> {
        let mut result = vec![];
        for (bond, _) in &self.bonds {
            if &bond.0 == atom_id || &bond.1 == atom_id {
                result.push(bond.clone());
            }
        }
        return result;
    }

    pub fn get_atoms(&self) -> Vec<Atom> {
        match self.display_mode {
            DisplayMode::AllElms => self.get_atoms_on_all_elms(),
            DisplayMode::CarbonOmit => self.get_atoms_on_carbon_omit(),
        }
    }
    fn get_atoms_on_all_elms(&self) -> Vec<Atom> {
        self.atoms.values().cloned().collect()
    }
    fn get_atoms_on_carbon_omit(&self) -> Vec<Atom> {
        let mut result = self.atoms.clone();
        let mut bond_count = 0;
        for ((atom1, atom2), bond_type) in &self.bonds {
            let bond_order: i32 = bond_type.into();
            bond_count += bond_order;
            if self.is_carbon_hydrogen_bond(atom1, atom2) {
                result.remove(atom1);
                result.remove(atom2);
                continue;
            }
            if let Element::C(_) = self.atoms[atom1].elm {
                result.remove(atom1);
            }
            if let Element::C(_) = self.atoms[atom2].elm {
                result.remove(atom2);
            }
        }
        if bond_count <= 7 {
            return self.get_atoms_on_all_elms();
        }
        let result = result.into_values().collect();
        return result;
    }

    pub fn get_bonds(&self) -> Vec<(BondType, Vector, Vector)> {
        match self.display_mode {
            DisplayMode::AllElms => self.get_bonds_on_all_elms(),
            DisplayMode::CarbonOmit => self.get_bonds_on_carbon_omit(),
        }
    }

    fn get_bonds_on_all_elms(&self) -> Vec<(BondType, Vector, Vector)> {
        let mut bonds = vec![];
        for ((atom1, atom2), bond_type) in self.bonds.iter() {
            let start = self.atoms[atom1].get_point();
            let end = self.atoms[atom2].get_point();
            bonds.push((bond_type.clone(), start, end));
        }
        return bonds;
    }

    fn get_bonds_on_carbon_omit(&self) -> Vec<(BondType, Vector, Vector)> {
        let mut bonds = vec![];
        let mut ch_bond = vec![];
        let mut bond_num = 0;
        for ((atom1, atom2), bond_type) in self.bonds.iter() {
            bond_num += bond_type.get_bond_order();
            let start = self.atoms[atom1].get_point();
            let end = self.atoms[atom2].get_point();
            let bond = (bond_type.clone(), start, end);
            if self.is_carbon_hydrogen_bond(atom1, atom2) {
                ch_bond.push(bond);
            } else {
                bonds.push(bond);
            }
        }
        if bond_num <= 7 {
            bonds.append(&mut ch_bond);
        }
        return bonds;
    }

    fn is_carbon_hydrogen_bond(&self, atom1: &AtomId, atom2: &AtomId) -> bool {
        let atom_num1 = self.atoms[atom1].elm.get_atomic_number();
        let atom_num2 = self.atoms[atom2].elm.get_atomic_number();
        let carbon = 6;
        let hydrogen = 1;
        return atom_num1 == carbon && atom_num2 == hydrogen
            || atom_num1 == hydrogen && atom_num2 == carbon;
    }
    pub fn get_other(bond: &(AtomId, AtomId), atom1: &AtomId) -> AtomId {
        if atom1 == &bond.0 {
            bond.1
        } else {
            bond.0
        }
    }
}

pub type AtomId = i32;
