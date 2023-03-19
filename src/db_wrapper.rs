use sqlite::Connection;
use crate::chem_struct::{
    Compound,
    Atom,
    AtomId,
};
use crate::bond_type::BondType;
use crate::vector::Vector;
use crate::element::Element;

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
            let p = atom.get_point();
            let insert_to_atoms = format!(
                "INSERT INTO atoms VALUES ({}, {}, {}, '{}', {}, {}, {}, {});\n",
                comp_id,
                id,
                atom.get_atomic_number(),
                atom.symbol(),
                atom.get_neutron_num(),
                atom.get_charge(),
                p.x,
                p.y,
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
