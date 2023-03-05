use sqlite::Connection;
use crate::element::Element;
use crate::bond_type::BondType;

pub fn init_database() {
    let db_path = "../data/basic_compounds.db";
    let con = &Connection::open(db_path).unwrap();
    create_database(con);
    insert_element(con);
    insert_bond_type(con);
}

fn insert_element(con: &Connection) {
    for i in 1..37 {
        if let Some(elm) = Element::from_atomic_number(i) {
            let statement = format!("INSERT INTO elements VALUES ({}, '{}')", i, elm.symbol());
            con.execute(statement).unwrap();
        }
    }
}

fn insert_bond_type(con: &Connection) {
    let ins = |id: i32, name: &str| {
        let query = format!("INSERT INTO bond_type VALUES ({}, '{}')", id, name);
        con.execute(query).unwrap();
    };
    for i in 1..10 {
        ins(i, format!("{:?}", BondType::from_id(i)).as_str());
    }
}

fn create_database(con: &Connection) {
    let query = "
        CREATE TABLE elements (id INTEGER, symbol TEXT, PRIMARY KEY (id));
        CREATE TABLE bond_type (id INTEGER, name TEXT, PRIMARY KEY (id));
        create table compounds (
            id INTEGER,
            name TEXT,
            PRIMARY KEY (id)
        );
        
        CREATE TABLE atoms (
            compound_id INTEGER,
            id INTEGER,
            elm_id INTEGER,
            neutron_num INTEGER,
            charge INTEGER,
            x FLOAT,
            y FLOAT,
            PRIMARY KEY (compound_id, id),
            FOREIGN KEY (compound_id) REFERENCES compounds(id),
            FOREIGN KEY (elm_id) REFERENCES elements(id)
        );
        CREATE TABLE bonds (
            compound_id INTEGER,
            from_atom INTEGER,
            to_atom INTEGER,
            bond_type_id INTEGER,
            PRIMARY KEY (compound_id, from_atom, to_atom),
            FOREIGN KEY (compound_id, from_atom) REFERENCES atoms(compound_id, id),
            FOREIGN KEY (compound_id, to_atom) REFERENCES atoms(compound_id, id)
        );
        CREATE TABLE focuses (
            compound_id INTEGER,
            focus_atom INTEGER,
            PRIMARY KEY (compound_id),
            FOREIGN KEY (compound_id, focus_atom) REFERENCES atoms(compound_id, id)
        );";
        con.execute(query).unwrap();
}
