use crate::bond_type::{BondType, DoubleBond, SingleBond};
use crate::element::{Element, Nuclide};
use crate::vector::Vector;
use sqlite::Connection;
use std::collections::{HashMap, VecDeque};
use std::f64::consts::PI;

#[derive(Debug, Clone)]
pub struct Atom {
    nuclide: Nuclide,
    charge: i32,
    p: Vector,
}

impl Default for Atom {
    fn default() -> Self {
        let p = Vector::new(512.0, 512.0);
        Atom {
            nuclide: Nuclide::default(),
            charge: 0,
            p,
        }
    }
}

impl Atom {
    pub fn new(elm: Nuclide, p: Vector) -> Atom {
        Atom {
            nuclide: elm,
            charge: 0,
            p,
        }
    }
    pub fn get_point(&self) -> Vector {
        self.p.clone()
    }
    pub fn get_element(&self) -> Element {
        self.nuclide.elm.clone()
    }
    pub fn get_nuclide(&self) -> Nuclide {
        self.nuclide.clone()
    }
    pub fn symbol(&self) -> String {
        let symbol = self.nuclide.elm.symbol();
        if self.charge == 0 {
            return symbol;
        }
        let mut charge = if self.charge.abs() > 1 {
            self.charge.abs().to_string()
        } else {
            String::new()
        };
        charge.push(if self.charge > 0 { '+' } else { '-' });
        return symbol + " " + charge.as_str();
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
    pub fn get_valences(&self) -> i32 {
        if self.charge < 0 {
            return self.nuclide.elm.get_valences();
        }
        if self.nuclide.elm.get_lone_pair() > 0 {
            self.nuclide.elm.get_valences() + self.charge
        } else {
            self.nuclide.elm.get_valences() - self.charge
        }
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
pub struct Compound {
    atoms: Vec<(AtomId, Atom)>,
    bonds: Vec<(AtomId, AtomId, BondType)>,
    focus: Option<AtomId>,
}

pub struct DBWrapper {
    compound_db: Connection,
    temp_version: i32,
    temp_name: String,
}
impl std::fmt::Debug for DBWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "DBWrapper")
    }
}
impl DBWrapper {
    pub fn new(path: &str) -> Self {
        let compound_db = Connection::open(path).unwrap();
        DBWrapper {
            compound_db,
            temp_version: 0,
            temp_name: String::from("temporary_data"),
        }
    }
    pub fn get_compound(&self, name: &str) -> sqlite::Result<Compound> {
        let query = format!("
        SELECT * FROM compounds
            WHERE
                name = '{}'",
            name
        );
        self.compound_db
            .prepare(query)?
            .into_iter()
            .next()
            .map(|row| {
                let row = row.unwrap();
                self.row_to_compound(row)
            })
            .ok_or(sqlite::Error {
                code: None,
                message: Some(format!("there is no compound {}", name)),
            })
    }
    pub fn undo(&mut self) -> sqlite::Result<Compound> {
        let query = format!("
        SELECT * FROM compounds
            WHERE
                name = '{}'
                AND id <= {}
            ORDER BY id DESC;
        ",
            self.temp_name,
            self.temp_version
        );
        let mut rows = self.compound_db
            .prepare(query)?
            .into_iter();
        if let Some(row) = rows.next() {
            let row = row.unwrap();
            self.temp_version = rows.next()
                .map(|row2| row2.unwrap().read::<i64, _>("id") as i32)
                .unwrap_or(self.temp_version);

            Ok(self.row_to_compound(row))
        } else {
            Err(sqlite::Error {
                code: None,
                message: Some(format!("there is no checkpoint")),
            })
        }
    }
    pub fn redo(&mut self) -> sqlite::Result<Compound> {
        let query = format!("
        SELECT * FROM compounds
            WHERE
                name = '{}'
                AND id > {}
            ORDER BY id;
        ",
            self.temp_name,
            self.temp_version
        );
        let mut rows = self.compound_db
            .prepare(query)?
            .into_iter();
        if let Some(row) = rows.next() {
            let row = row.unwrap();
            self.temp_version = row.read::<i64, _>("id") as i32;
            Ok(self.row_to_compound(row))
        } else {
            Err(sqlite::Error {
                code: None,
                message: Some(format!("there is no checkpoint")),
            })
        }
    }
    fn row_to_compound(&self, row: sqlite::Row) -> Compound {
        let id = row.read::<i64, _>("id");
        let atoms = self.get_atoms(id);
        let bonds = self.get_bonds(id);
        let focus = self.get_focus(id);

        Compound {
            atoms,
            bonds,
            focus,
        }
    }
    pub fn temp_save(&mut self, compound: Compound) -> sqlite::Result<i32> {
        let delete =  |table: &str| {
            format!("
            DELETE FROM {}
            WHERE
                compound_id IN (
                    SELECT id FROM compounds
                    WHERE
                        compounds.name = '{}'
                        AND compounds.id > {}
                );
            ", table, self.temp_name, self.temp_version)
        };
        let mut statement = "BEGIN TRANSACTION tran;\n".to_string();
        statement.push_str(delete("focuses").as_str());
        statement.push_str(delete("bonds").as_str());
        statement.push_str(delete("atoms").as_str());
        statement.push_str(format!("
        DELETE FROM compounds
        WHERE name = '{}'
            AND id > {};
        ", self.temp_name, self.temp_version).as_str());
        statement.push_str("COMMIT TRANSACTION tran;");

        self.compound_db.execute(statement)?;
        let ver = self.save_compound(self.temp_name.as_str(), compound)?;
        self.temp_version = ver;
        Ok(ver)
    }
    pub fn save_compound(
        &self,
        name: &str,
        compound: Compound,
    ) -> sqlite::Result<i32> {
        let get_max_id = "SELECT MAX(id) AS max_id FROM compounds";
        let comp_id = self
            .compound_db
            .prepare(get_max_id)?
            .into_iter()
            .next()
            .map(|row| {
                let row = row.unwrap();
                row.try_read::<i64, _>("max_id").unwrap_or(0)
            })
            .ok_or(sqlite::Error {
                code: None,
                message: Some("something went wrong".to_string()),
            })? as i32
            + 1;
        let mut add_compound = "BEGIN TRANSACTION tran;\n".to_string();
        add_compound.push_str(format!("INSERT INTO compounds VALUES ({}, '{}');\n", comp_id, name).as_str());
        for (id, atom) in compound.atoms {
            let insert_to_atoms = format!(
                "INSERT INTO atoms VALUES ({}, {}, {}, '{}', {}, {}, {}, {});\n",
                comp_id,
                id,
                atom.nuclide.elm.get_atomic_number(),
                atom.nuclide.elm.symbol(),
                atom.nuclide.neutron_num,
                atom.charge,
                atom.p.x,
                atom.p.y,
            );
            add_compound.push_str(insert_to_atoms.as_str());
        }
        for (from, to, bond_type) in compound.bonds {
            let insert_to_bonds = format!(
                "INSERT INTO bonds VALUES ({}, {}, {}, {});\n",
                comp_id,
                from,
                to,
                bond_type.into_id(),
            );
            add_compound.push_str(insert_to_bonds.as_str());
        }
        if let Some(focus) = compound.focus {
            let insert_to_focuses =
                format!("INSERT INTO focuses VALUES ({}, {});\n", comp_id, focus);
            add_compound.push_str(insert_to_focuses.as_str());
        }
        add_compound.push_str("COMMIT TRANSACTION tran;");
        self.compound_db.execute(add_compound)?;
        Ok(comp_id)
    }
    fn get_atoms(&self, compound_id: i64) -> Vec<(AtomId, Atom)> {
        let query = format!(
            "
        SELECT * FROM atoms
	        WHERE
    	        compound_id = {}",
            compound_id
        );
        self.compound_db
            .prepare(query)
            .unwrap()
            .into_iter()
            .map(|row| {
                let row = row.unwrap();
                let id = row.read::<i64, _>("id") as i32;
                let elm = Element::from_atomic_number(row.read::<i64, _>("elm_id") as i32)
                    .unwrap_or(Element::Text(row.read::<&str, _>("atom_text").to_string()));
                let x = row.read::<f64, _>("x");
                let y = row.read::<f64, _>("y");
                (id, Atom::new(elm.into(), Vector::new(x, y)))
            })
            .collect()
    }
    fn get_bonds(&self, compound_id: i64) -> Vec<(AtomId, AtomId, BondType)> {
        let query = format!(
            "
        SELECT * FROM bonds
	        WHERE
    	        compound_id = {}",
            compound_id
        );
        self.compound_db
            .prepare(query)
            .unwrap()
            .into_iter()
            .map(|row| {
                let row = row.unwrap();
                let from = row.read::<i64, _>("from_atom") as i32;
                let to = row.read::<i64, _>("to_atom") as i32;
                let type_number = row.read::<i64, _>("bond_type_id");
                (from, to, BondType::from_id(type_number as i32))
            })
            .collect()
    }
    fn get_focus(&self, compound_id: i64) -> Option<AtomId> {
        let query = format!(
            "
        SELECT * FROM focuses
            WHERE
                compound_id = {}",
            compound_id
        );
        self.compound_db
            .prepare(query)
            .unwrap()
            .into_iter()
            .map(|row| {
                let row = row.unwrap();
                row.read::<i64, _>("focus_atom") as i32
            })
            .next()
    }
}

#[derive(Debug)]
pub struct ChemStruct {
    atoms: HashMap<AtomId, Atom>,
    bonds: HashMap<(AtomId, AtomId), BondType>,
    next_id: AtomId,
    display_mode: DisplayMode,
    bond_len: f64,
    compound_db: DBWrapper,
}

impl Default for ChemStruct {
    fn default() -> Self {
        let bond_len = 50.0;
        let atoms = HashMap::new();
        let bonds = HashMap::new();
        let next_id = 1;
        let display_mode = DisplayMode::default();
        let db_path = "../data/basic_compounds.db";
        let db = DBWrapper::new(db_path);
        ChemStruct {
            atoms,
            bonds,
            next_id,
            display_mode,
            bond_len,
            compound_db: db,
        }
    }
}

impl ChemStruct {
    pub fn new_atom(&mut self, elm: Nuclide) -> AtomId {
        let p = Vector::new(512.0, 256.0);
        let new_atom = Atom::new(elm, p);
        let new_atom_id = self.next_id;
        self.atoms.insert(new_atom_id, new_atom);
        self.next_id += 1;
        self.compensate_hydrogen(&new_atom_id, 0);
        return new_atom_id;
    }
    pub fn append(&mut self, atom_id: &AtomId, elm: Nuclide) -> Option<AtomId> {
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
    pub fn new_compound(&mut self, name: &str) -> Option<AtomId> {
        if let Ok(comp) = self.compound_db.get_compound(name) {
            let now_atom_id = self.next_id;
            let mut max_id = 0;
            for (id, atom) in comp.atoms {
                self.atoms.insert(now_atom_id + id, atom);
                if id > max_id {
                    max_id = id;
                }
            }
            for (from, to, bond_type) in comp.bonds {
                self.bonds
                    .insert((now_atom_id + from, now_atom_id + to), bond_type);
            }
            self.next_id = now_atom_id + max_id + 1;
            return comp.focus;
        }
        return None;
    }
    pub fn temp_save(&mut self, focus: Option<AtomId>) -> sqlite::Result<i32> {
        let compound = self.make_compound(focus);
        self.compound_db.temp_save(compound)
    }
    pub fn undo(&mut self) -> Option<AtomId> {
        if let Ok(comp) = self.compound_db.undo() {
            let focus = comp.focus.clone();
            self.load_compound(comp);
            focus
        } else {
            None
        }
    }
    pub fn redo(&mut self) -> Option<AtomId> {
        if let Ok(comp) = self.compound_db.redo() {
            let focus = comp.focus.clone();
            self.load_compound(comp);
            focus
        } else {
            None
        }
    }
    fn load_compound(&mut self, comp: Compound) {
        self.atoms = HashMap::from_iter(comp.atoms);
        self.bonds = HashMap::from_iter(
            comp.bonds.into_iter()
            .map(|(f, t, bond)| ((f, t), bond))
        );
        self.next_id = self.atoms.keys().max().unwrap_or(&0) + 1;
    }
    pub fn save_compound(&self, name: &str, focus: Option<AtomId>) -> sqlite::Result<i32> {
        let compound = self.make_compound(focus);
        self.compound_db.save_compound(name, compound)
    }
    fn make_compound(&self, focus: Option<AtomId>) -> Compound {
        Compound {
            atoms: self.atoms.clone().into_iter().collect(),
            bonds: self
                .bonds
                .clone()
                .into_iter()
                .map(|((from, to), bond_type)| (from, to, bond_type))
                .collect(),
            focus,
        }
    }
    pub fn get_point(&self, atom_id: AtomId) -> Option<Vector> {
        self.atoms.get(&atom_id).map(|atom| atom.get_point())
    }
    pub fn delete(&mut self, atom_id: &AtomId) -> Option<AtomId> {
        if !self.atoms.contains_key(atom_id) {
            return None;
        }
        let is_h = Element::H == self.atoms[atom_id].nuclide.elm;
        self.atoms.remove(&atom_id);
        let connected_bonds = self.get_connected_bonds(atom_id);
        let mut connected_atom = None;
        for bond in connected_bonds {
            let other = Self::get_other(&bond, atom_id);
            self.bonds.remove(&bond);
            if Element::H == self.atoms[&other].nuclide.elm {
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
                if Element::H == self.atoms[id].nuclide.elm {
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
        let mut uf = self.separete_atoms_by_bonds(vec![(origin, target)]);
        let origin_p = self.atoms[&origin].get_point();
        let target_root = uf.find(target);
        for (id, atom) in self.atoms.iter_mut() {
            let r = uf.find(*id);
            if r == target_root {
                atom.rotate(theta, &origin_p);
            }
        }
        self.recompensate(&origin);
        if self.atoms.contains_key(&target) {
            self.recompensate(&target);
        }
    }
    pub fn opt_cycle(&mut self, origin: &AtomId, bond: &(AtomId, AtomId)) {
        if !self.bonds.contains_key(bond) {
            return;
        }
        let loops = self.find_loops(origin, bond);
        if loops.len() == 2 {
            let path1 = &loops[0];
            let mut path2 = loops[1].clone();
            path2.reverse();
            let path2 = &path2;
            let other = Self::get_other(bond, origin);
            // 小さい環を優先
            self.opt_path_except(&other, bond, path2, &vec![]);
            self.opt_path_except(origin, bond, path1, path2);
        } else if let Some(path) = loops.get(0) {
            self.opt_path_except(origin, bond, path, &vec![]);
        }

        for atom in loops.iter().flatten() {
            self.recompensate(&atom);
        }
    }
    fn opt_path_except(&mut self, origin: &AtomId, bond: &(AtomId, AtomId), path: &Vec<i32>, ex: &Vec<i32>) {
        let x_axis = Vector::new(1.0, 0.0);
        let other = Self::get_other(bond, origin);
        let base_bond_v = self.atoms[&other].get_point() - self.atoms[origin].get_point();
        let init_theta = x_axis.calc_angle(&base_bond_v);
        let mut uf = {
            let edges: Vec<(AtomId, AtomId)> = {
                let mut rot_path = path.clone();
                rot_path.rotate_right(1);
                rot_path
                    .into_iter()
                    .zip(path.clone())
                    .collect()
            };
            let uf = self.separete_atoms_by_bonds(edges.clone());
            uf
        };
        let roots: Vec<i32> = path.iter().map(|&node| uf.find(node)).collect();
        let ex_roots: Vec<i32> = ex.iter().map(|&node| uf.find(node)).collect();
        for (i, (b, f)) in path.iter().zip(&path[1..]).enumerate() {
            let bond_v = self.atoms[f].get_point() - self.atoms[b].get_point();
            let now_theta = x_axis.calc_angle(&bond_v);
            let wtb = init_theta - (i + 1) as f64 * 2.0 * PI / path.len() as f64;
            let theta = wtb - now_theta;
            // 結合長調節
            let para_v = (bond_v.dist() - base_bond_v.dist()) * x_axis.rotate(wtb);
            let para_v = &para_v;
            let tmp_origin = &self.atoms[b].get_point();
            for (id, atom) in self.atoms.iter_mut() {
                let atom_root = uf.find(*id);
                if !roots[..i].contains(&atom_root)
                    && !ex_roots.contains(&atom_root)
                    || id == f
                {
                    atom.rotate(theta, tmp_origin);

                    if roots[i] != atom_root || id == f {
                        atom.p = &atom.p - para_v;
                    }
                }
            }
        }
    }
    fn find_loops(&self, origin: &AtomId, bond: &(AtomId, AtomId)) -> Vec<Vec<AtomId>> {
        let make_ad_list = |bonds: Vec<&(i32, i32)>| {
            let mut ad_list = HashMap::new();
            for b in bonds {
                ad_list.entry(b.0).or_insert(Vec::new()).push(b.1);
                ad_list.entry(b.1).or_insert(Vec::new()).push(b.0);
            }
            ad_list
        };
        let calc_loop = |ad_list: HashMap<i32, Vec<i32>>| {
            // 辺を含む最小の環を探索
            let other = Self::get_other(bond, origin);
            let mut queue = VecDeque::new();
            queue.push_back(other);
            let mut path = HashMap::from([(other, *origin)]);
            while let Some(visited) = queue.pop_front() {
                let mut find_end = false;
                for seen in ad_list[&visited].iter() {
                    if path.contains_key(seen) {
                        continue;
                    }
                    if visited == other && seen == origin {
                        continue;
                    }
                    path.entry(*seen).or_insert(visited);
                    queue.push_back(*seen);
                    if seen == origin {
                        find_end = true;
                        break;
                    }
                }
                if find_end {
                    break;
                }
            }
            // 環のパス
            let loop_path = {
                let mut loop_path = vec![origin];
                while let Some(pre_node) = loop_path
                    .last()
                    .and_then(|&last| path.get(last))
                    .filter(|&node| node != origin)
                {
                    loop_path.push(pre_node);
                }

                if loop_path.len() <= 1 {
                    None
                } else {
                    // otherを先頭としてorigin->otherとなるように変形
                    loop_path.reverse();
                    Some(loop_path.into_iter().map(|&id| id).collect())
                }
            };
            loop_path
        };
        let cycle_edges = |path: Vec<&i32>| -> Vec<(i32, i32)> {
            let mut rot_path = path.clone();
            rot_path.rotate_right(1);
            rot_path
                .into_iter()
                .zip(path)
                .map(|(&f, &t)| (f, t))
                .collect()
        };
        let mut loops = calc_loop(make_ad_list(self.bonds.keys().collect()))
            .into_iter()
            .collect::<Vec<Vec<i32>>>();
        if let Some(loop_path1) = loops.get(0) {
            let mut edges = cycle_edges(loop_path1.iter().collect());
            let mut loop_path1 = loop_path1.clone();
            loop_path1.reverse();
            edges.append(&mut cycle_edges(loop_path1.iter().collect()));
            let bonds = self.bonds.keys().filter(|&b| !edges.contains(b)).collect();
            let loop_path2 = calc_loop(make_ad_list(bonds));
            loops.append(&mut loop_path2.into_iter().collect());
        }
        return loops;
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
        self.bonds.remove(bond_id).map(|bond| {
            let new_bond_id = (bond_id.1, bond_id.0);
            self.bonds.insert(new_bond_id, bond);
            new_bond_id
        })
    }
    pub fn calc_next_bond(
        &self,
        origin: &AtomId,
        bond_id: &(AtomId, AtomId),
    ) -> Option<(AtomId, AtomId)> {
        self.calc_best_angle_bond(origin, bond_id, |cur, acc| cur > acc, 0.0)
    }
    pub fn calc_prev_bond(
        &self,
        origin: &AtomId,
        bond_id: &(AtomId, AtomId),
    ) -> Option<(AtomId, AtomId)> {
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
        self.atoms.get_mut(atom_id).map(|atom| {
            atom.increase_charge(increase);
        });
        self.recompensate(atom_id);
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
    fn separete_atoms_by_bonds(&self, bonds: Vec<(AtomId, AtomId)>) -> UnionFind {
        let mut uf = UnionFind::new(self.atoms.clone().into_keys().collect());
        for ((atom1, atom2), _) in self.bonds.iter() {
            let atom1 = *atom1;
            let atom2 = *atom2;
            if bonds.contains(&(atom1, atom2)) || bonds.contains(&(atom2, atom1)) {
                continue;
            }
            uf.unite(atom1, atom2);
        }
        return uf;
    }
    fn recompensate(&mut self, atom_id: &AtomId) {
        while let Some(hydro_id) = self.get_connected_normal_h(atom_id) {
            self.delete(&hydro_id);
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
                    let make_v = |i: usize| {
                        let a = &self.atoms[&atoms[i]];
                        let k = if a.nuclide.elm == Element::H {
                            0.5
                        } else {
                            1.0
                        };
                        k * (a.get_point() - self.atoms[atom_id].get_point()).norm()
                    };
                    let v1 = make_v(0);
                    let v2 = make_v(1);
                    let v3 = make_v(2);
                    let connected_vs = vec![v1.clone(), v2.clone(), v3.clone()];
                    let basic_tetra: Vec<Vector> = {
                        let v4 = Vector::new(0.0, 0.0);
                        let angle = Self::calc_fit_tetra_angle(&v1, &v2, &v3, &v4);
                        Vector::basic_tetra()
                            .iter()
                            .map(|v| v.rotate(angle))
                            .collect()
                    };
                    let similarity = basic_tetra.iter().map(|v| {
                        let (simi, _) = v.most_similar(&connected_vs);
                        simi
                    });
                    // 適当な大きい数
                    let mut least_simi = 100.0;
                    let mut least_simi_index = 0;
                    for (i, simi) in similarity.enumerate() {
                        if simi < least_simi {
                            least_simi = simi;
                            least_simi_index = i;
                        }
                    }
                    basic_tetra[least_simi_index].clone()
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
    fn compensate_hydrogen(&mut self, atom_id: &AtomId, bond_order: i32) {
        let v = self.atoms[atom_id].get_valences() - bond_order;
        for _ in 0..v {
            self.append(atom_id, Element::H.into());
        }
    }
    fn delete_hydrogen(&mut self, atom_id: &AtomId) {
        let bond_num = self.calc_connected_bond_num(atom_id);
        let atom = &self.atoms[atom_id];
        let val = atom.get_valences();
        if bond_num - val < 0 {
            return;
        }
        if let Some(hydrogen) = self.get_connected_normal_h(atom_id) {
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
        self.get_connected_bonds(atom_id)
            .iter()
            .fold(0, |acc, cur| {
                acc + self
                    .bonds
                    .get(cur)
                    .unwrap_or(&BondType::Hidden)
                    .get_bond_order()
            })
    }
    // 一番いい感じの消したい水素を優先的に選択する
    fn get_connected_normal_h(&self, atom_id: &AtomId) -> Option<AtomId> {
        let bonds = self.get_connected_bonds(atom_id);
        // 軸に近いほうから選択
        let axis = {
            let angle = if bonds.len() == 4 {
                let origin = &self.atoms[atom_id].get_point();
                let v: Vec<Vector> = bonds
                    .iter()
                    .map(|b| {
                        let other = Self::get_other(b, atom_id);
                        let k = if self.atoms[&other].nuclide.elm == Element::H {
                            0.5
                        } else {
                            1.0
                        };
                        k * (self.atoms[&other].get_point() - origin).norm()
                    })
                    .collect();
                // v.sort_by(|a, b| x_axis.calc_angle(a).partial_cmp(&x_axis.calc_angle(b)).unwrap());
                // v.reverse();
                Self::calc_fit_tetra_angle(&v[0], &v[1], &v[2], &v[3])
            } else {
                0.0
            };
            &Vector::new(1.0, 0.0).rotate(angle)
        };
        let mut result = None;
        let mut dot = -1.0;
        let mut vert_h = None;
        for (atom1, atom2) in bonds {
            let v = (self.atoms[&atom2].get_point() - self.atoms[&atom1].get_point()).norm();
            let other = Self::get_other(&(atom1, atom2), atom_id);
            if self.atoms[&other].nuclide.elm == Element::H {
                let bond = self.get_connected_bonds(&other);
                let td = axis.dot(&v).abs();
                if bond.len() == 1 && td > dot {
                    result = Some(other);
                    dot = td;
                }
                if bond.len() == 1 && (1000.0 * td).floor() == 0.0 {
                    vert_h = Some(other);
                }
            }
        }
        // 垂直なやつしかなかったら、それを選択
        if dot.abs() < (PI / 4.0).cos() {
            return vert_h.or(result);
        } else {
            return result;
        }
    }
    fn calc_fit_tetra_angle(v1: &Vector, v2: &Vector, v3: &Vector, v4: &Vector) -> f64 {
        let basic_tetra = Vector::basic_tetra();
        let test_tetra = vec![v1, v2, v3, v4];
        let try_count = 6.0;
        let step = 2.0 * PI / try_count;
        let rotate_tetra = |tetra: &Vec<Vector>, theta: f64| -> Vec<Vector> {
            tetra.iter().map(|v| v.rotate(theta)).collect()
        };
        let calc_simi = |vs1: Vec<Vector>, vs2: &Vec<&Vector>| -> f64 {
            vs2.iter()
                .map(|v| {
                    let (simi, _) = v.most_similar(&vs1);
                    simi
                })
                .sum()
        };
        let mut max_similarity = -100.0;
        let mut theta = 0.0;
        for i in 0..try_count as i32 {
            let rot_theta = i as f64 * step;
            let rot_tetra = rotate_tetra(&basic_tetra, rot_theta);
            let simi = calc_simi(rot_tetra, &test_tetra);
            let simi = (1000.0 * simi).floor() / 1000.0;
            if simi > max_similarity {
                max_similarity = simi;
                theta = rot_theta;
            }
        }
        return theta;
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
        let ins_charged_carbon = |result: &mut HashMap<i32, Atom>, id: &AtomId| {
            if self.atoms[id].nuclide.elm == Element::C {
                result.remove(id);
                if self.atoms[id].charge != 0 {
                    let mut dummy = Atom::new(
                        Element::Text(String::new()).into(),
                        self.atoms[id].get_point(),
                    );
                    dummy.charge = self.atoms[id].charge;
                    result.insert(*id, dummy);
                    return true;
                }
            }
            return false;
        };

        for ((atom1, atom2), bond_type) in &self.bonds {
            let bond_order: i32 = bond_type.into();
            bond_count += bond_order;
            let atom1_charged = ins_charged_carbon(&mut result, atom1);
            let atom2_charged = ins_charged_carbon(&mut result, atom2);
            if self.is_carbon_hydrogen_bond(atom1, atom2) {
                if !atom1_charged {
                    result.remove(atom1);
                }
                if !atom2_charged {
                    result.remove(atom2);
                }
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
        let atom_num1 = self.atoms[atom1].nuclide.elm.get_atomic_number();
        let atom_num2 = self.atoms[atom2].nuclide.elm.get_atomic_number();
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
