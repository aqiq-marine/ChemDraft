use iced::keyboard::{KeyCode, Modifiers};
use iced::widget::canvas::{
    self, Program, Geometry, Path, path::Builder, Stroke,
    Cache, Cursor, Frame,
};
use iced::{
    Size,
    Theme, Rectangle,
    Color, Command,
};
use std::f64::consts::PI;
use crate::message::Message;

use crate::element::{Element, Nuclide};
use crate::chem_struct::{self, ChemStruct, Atom};
use crate::bond_type::{BondType, SingleBond, DoubleBond};
use crate::vector::Vector;
use crate::canvas_to_image;
use crate::save_image;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum StructDrawState {
    #[default]
    AtomFocus,
    BondFocus,
    Input(InputFor),
    Move,
    Capturing,
}
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum InputFor {
    #[default]
    AppendElm,
    AppendLabelElm,
    Save,
}

#[derive(Debug)]
pub struct StructDraw {
    canv: Cache,
    chem_struct: ChemStruct,
    mode: StructDrawState,
    input: String,
    focus_atom: Option<chem_struct::AtomId>,
    selected_atom: Option<chem_struct::AtomId>,
    focus_bond: Option<(chem_struct::AtomId, chem_struct::AtomId)>,
    select_area_size: f64,
    select_area_top_left: Vector,
    focus_color: Color,
}

impl Default for StructDraw {
    fn default() -> Self {
        let chem_struct = ChemStruct::default();
        Self {
            canv: Cache::default(),
            chem_struct,
            mode: StructDrawState::default(),
            input: String::new(),
            focus_atom: None,
            selected_atom: None,
            focus_bond: None,
            select_area_size: 50.0,
            select_area_top_left: Vector::new(0.0, 0.0),
            focus_color: Color::new(0.52, 0.81, 0.98, 0.5),
        }
    }
}

impl StructDraw {
    pub fn handle_key(
        &mut self,
        modifiers: Modifiers, key_code: KeyCode,
    ) -> iced::Command<Message> {
        match self.mode {
            StructDrawState::AtomFocus => {
                let cmd = self.hk_on_atom_focus(modifiers, key_code);
                self.canv.clear();
                return cmd;
            },
            StructDrawState::BondFocus => self.hk_on_bond_focus(modifiers, key_code),
            StructDrawState::Input(input_for) => self.hk_on_input_mode(modifiers, key_code, input_for),
            StructDrawState::Move => self.hk_on_move_mode(modifiers, key_code),
            StructDrawState::Capturing => {},
        }
        self.canv.clear();
        iced::Command::none()
    }
    fn hk_on_atom_focus(
        &mut self,
        modifiers: Modifiers, key_code: KeyCode,
    ) -> Command<Message> {
        if modifiers.control() && modifiers.shift() {
            let mut move_atoms = |direc: Vector| {
                let step = 5.0;
                let v = step * direc;
                self.focus_atom.map(|atom| {
                    self.chem_struct.move_atom_group(&atom, &v);
                });
            };
            match key_code {
                KeyCode::H | KeyCode::J | KeyCode::K | KeyCode::L => {
                    move_atoms(Self::key2direc(key_code));
                },
                _ => {}
            }

            return Command::none();
        }
                

        if modifiers.control() {
            let mut move_atom = |direc: Vector| {
                let step = 5.0;
                let v = step * direc;
                self.focus_atom.map(|atom| {
                    self.chem_struct.move_atom(&atom, &v);
                });
            };
            match key_code {
                KeyCode::H | KeyCode::J | KeyCode::K | KeyCode::L => {
                    move_atom(Self::key2direc(key_code));
                }
                KeyCode::M => {
                    if modifiers.control() {
                        self.mode = StructDrawState::Move;
                        let p = self.focus_atom
                            .and_then(|atom| self.chem_struct.get_point(atom))
                            .map(|p| p - 0.5 * self.select_area_size * Vector::new(1.0, 1.0))
                            .unwrap_or(self.select_area_top_left.clone());
                        self.select_area_top_left = p;
                    }
                },
                KeyCode::B => {
                    self.mode = StructDrawState::BondFocus;
                    let bonds = self.focus_atom
                        .map(|atom| self.chem_struct.get_connected_bonds(&atom))
                        .unwrap_or(Vec::new());
                    self.focus_bond = bonds.get(0).map(|bond| bond.clone());
                },
                KeyCode::P => {
                    self.selected_atom = self.focus_atom.clone();
                },
                KeyCode::C => {
                    // connect atoms
                    let bond = self.selected_atom.zip(self.focus_atom)
                        .and_then(|(from, to)| self.chem_struct.connect_atoms(&from, &to));
                    if let Some(bond) = bond {
                        self.mode = StructDrawState::BondFocus;
                        self.focus_bond = Some(bond);
                        self.selected_atom = None;
                    }
                },
                KeyCode::S => {
                    let result = self.chem_struct.temp_save(self.focus_atom);
                    if let Err(err) = result {
                        println!("{}", err);
                    }
                },
                KeyCode::Z => {
                    self.focus_atom = self.chem_struct.undo();
                },
                KeyCode::Y => {
                    self.focus_atom = self.chem_struct.redo();
                },
                KeyCode::N => {
                    self.mode = StructDrawState::Input(InputFor::Save);
                }
                KeyCode::I => {
                    let bounds = self.calc_bounds_for_mol();
                    let mode = self.mode;
                    self.mode = StructDrawState::Capturing;
                    let g = self.capture_canvas(bounds.clone());
                    self.mode = mode;
                    let future = canvas_to_image::geometry_to_image(g, bounds);
                    return Command::perform(future, |image| {
                        image.map_or(Message::SaveErrorOccur, |image| {
                            if let Ok(_) = save_image::save_image(image) {
                                Message::Saved
                            } else {
                                Message::SaveErrorOccur
                            }
                        })
                    });
                },
                _ => {},
            }
        }
        if modifiers.shift() {
            let increase = match key_code {
                KeyCode::Equals => 1,
                KeyCode::Minus => -1,
                _ => 0,
            };
            self.focus_atom.as_mut()
                .map(|atom_id| self.chem_struct.increase_charge(atom_id, increase));
        }
        if modifiers.is_empty() {
            if let Some(elm) = Self::key2elm(key_code) {
                self.append_nuclide(elm);
                return Command::none();
            }
            match key_code {
                // 本当はバックスラッシュ
                KeyCode::OEM102 => {
                    self.mode = StructDrawState::Input(InputFor::AppendElm);
                },
                KeyCode::T => {
                    self.mode = StructDrawState::Input(InputFor::AppendLabelElm);
                },
                KeyCode::X => {
                    if let Some(atom) = self.focus_atom {
                        self.focus_atom = self.chem_struct.delete(&atom);
                    }
                },
                _ => {}
            }
        }
        Command::none()
    }
    fn key2direc(key_code: KeyCode) -> Vector {
        match key_code {
            KeyCode::H => Vector::new(-1.0, 0.0),
            KeyCode::J => Vector::new(0.0, 1.0),
            KeyCode::K => Vector::new(0.0, -1.0),
            KeyCode::L => Vector::new(1.0, 0.0),
            _ => Vector::new(0.0, 0.0),
        }
    }
    fn key2elm(key_code: KeyCode) -> Option<Nuclide> {

        let symbol = match key_code {
            KeyCode::H => "H",
            KeyCode::B => "B",
            KeyCode::C => "C",
            KeyCode::N => "N",
            KeyCode::O => "O",
            KeyCode::F => "F",
            KeyCode::P => "P",
            KeyCode::S => "S",
            KeyCode::K => "K",
            _ => "",
        };
        return Element::from_symbol(symbol).map(|elm| elm.into());
    }
    fn hk_on_bond_focus(
        &mut self,
        modifiers: Modifiers, key_code: KeyCode,
    ) {
        if modifiers.control() {
            match key_code {
                KeyCode::E => {
                    self.mode = StructDrawState::AtomFocus;
                },
                _ => {},
            }
        }
        if modifiers.shift() {
            let atom_s = self.focus_atom;
            let bond = atom_s.zip(self.focus_bond).map(|(s, (a1, a2))| (s, if s == a1 {a2} else {a1}));
            let rot_theta = PI / 12.0 * match key_code {
                // キーボード配置が違う？
                // 実際には;/+のキー
                KeyCode::Equals => -1.0,
                KeyCode::Minus => 1.0,
                _ => 0.0,
            };
            bond.map(|(s, e)| self.chem_struct.rotate(e, s, rot_theta));
        }
        if modifiers.alt() {
            match key_code {
                KeyCode::O => {
                    self.focus_atom.zip(self.focus_bond)
                        .map(|(origin, bond)| self.chem_struct.opt_cycle(&origin, &bond));
                },
                _ => {},
            }
        }
        if modifiers.is_empty() {
            let mut change_bond = |bond: BondType| {
                self.focus_bond.map(|id| self.chem_struct.change_bond(&id, bond));
            };
            match key_code {
                KeyCode::S => change_bond(BondType::Single(SingleBond::Normal)),
                KeyCode::F => change_bond(BondType::Single(SingleBond::Forward)),
                KeyCode::B => change_bond(BondType::Single(SingleBond::Backward)),
                KeyCode::W => change_bond(BondType::Single(SingleBond::Wide)),
                KeyCode::D => change_bond(BondType::Double(DoubleBond::Left)),
                KeyCode::C => change_bond(BondType::Double(DoubleBond::Center)),
                KeyCode::T => change_bond(BondType::Triple),
                KeyCode::H => change_bond(BondType::Hidden),
                KeyCode::R => {
                    self.focus_bond = self.focus_bond.as_ref()
                        .and_then(|bond_id| {
                            self.chem_struct.reverse_bond(&bond_id)
                        });
                },
                KeyCode::N => {
                    if let Some(origin) = self.focus_atom {
                        self.focus_bond = self.focus_bond.as_ref()
                            .and_then(|b| {
                                self.chem_struct.calc_next_bond(&origin, b)
                            });
                    }
                },
                KeyCode::P => {
                    if let Some(origin) = self.focus_atom {
                        self.focus_bond = self.focus_bond.as_ref()
                            .and_then(|b| {
                                self.chem_struct.calc_prev_bond(&origin, b)
                            });
                    }
                },
                KeyCode::M => {
                    self.focus_atom = self.focus_atom
                        .zip(self.focus_bond)
                        .map(|(atom, bond)| ChemStruct::get_other(&bond, &atom));
                },
                KeyCode::X => {
                    self.focus_bond.take().map(|id| self.chem_struct.delete_bond(&id));
                    self.mode = StructDrawState::AtomFocus;
                },
                _ => {},
            }
        }
    }
    fn hk_on_input_mode(
        &mut self,
        modifiers: Modifiers, key_code: KeyCode,
        input_for: InputFor,
    ) {
        if modifiers.is_empty() {
            if key_code == KeyCode::Enter {
                self.mode = StructDrawState::AtomFocus;
                match input_for {
                    InputFor::AppendElm => {
                        Element::from_symbol(self.input.as_str())
                            .map(|elm| self.append_nuclide(elm.into()))
                            .or_else(|| {
                                let focus = self.chem_struct.new_compound(self.input.as_str());
                                self.focus_atom = focus.or(self.focus_atom);
                                focus.map(|_| ())
                            });
                    },
                    InputFor::AppendLabelElm => {
                        self.append_nuclide(Element::Text(self.input.clone()).into());
                    },
                    InputFor::Save => {
                        let result = self.chem_struct.save_compound(self.input.as_str(), self.focus_atom.clone());
                        if let Err(err) = result {
                            println!("{}", err);
                        }
                    }
                }
                self.input = String::new();
                return;
            }
            if key_code == KeyCode::Backspace {
                self.input.pop();
                return;
            }
        }
        if modifiers.control() && key_code == KeyCode::LBracket {
            self.mode = StructDrawState::AtomFocus;
            self.input = String::new();
            return;
        }
        if modifiers.is_empty() {
            let key = Self::key2string(key_code).to_lowercase();
            self.input += key.as_str();
        }
        if modifiers.shift() {
            if key_code == KeyCode::Key7 {
                self.input += "'";
                return;
            }
            let key = Self::key2string(key_code).to_uppercase();
            self.input += key.as_str();
        }
    }
    fn append_nuclide(&mut self, nuclide: Nuclide) {
        let new_atom_id = if let Some(id) = self.focus_atom {
            self.chem_struct.append(&id, nuclide)
        } else {
            Some(self.chem_struct.new_atom(nuclide))
        };
        self.focus_atom = new_atom_id;
    }
    fn key2string(key_code: KeyCode) -> String {
        if KeyCode::A <= key_code && key_code <= KeyCode::Z {
            return format!("{:?}", key_code);
        }
        if KeyCode::Key1 <= key_code && key_code <= KeyCode::Key9 {
            return format!("{:?}", key_code).chars().nth(3)
                .map_or(String::new(), |c| c.to_string());
        }
        match key_code {
            KeyCode::Equals => String::from("+"),
            KeyCode::Minus => String::from("-"),
            KeyCode::Space => String::from(" "),
            _ => String::new(),
        }
    }
    fn hk_on_move_mode(
        &mut self,
        modifiers: Modifiers, key_code: KeyCode,
    ) {
        if modifiers.control() {
            match key_code {
                KeyCode::LBracket => {
                    self.mode = StructDrawState::AtomFocus;
                }
                KeyCode::S => {
                    self.select_area_size /= 2.0;
                },
                KeyCode::M => {
                    self.select_area_size *= 2.0;
                },
                _ => {},
            }
        }
        if modifiers.is_empty() {
            if key_code == KeyCode::Enter {
                let right_bot = &self.select_area_top_left + self.select_area_size * Vector::new(1.0, 1.0);
                let focus_atom = self.chem_struct.get_atom_in_rect(&self.select_area_top_left, &right_bot);
                if focus_atom.is_some() {
                    self.focus_atom = focus_atom;
                    self.mode = StructDrawState::AtomFocus;
                }
                return;
            }
            let v = self.select_area_size * Self::key2direc(key_code);
            self.select_area_top_left.add(&v);
        }
    }
    fn draw_bond(frame: &mut Frame, bond_type: &BondType, from: &Vector, to: &Vector) {
        let stroke = Stroke::default();
        // let stroke = Stroke::with_color(stroke, Color::new(0.0, 0.0, 1.0, 1.0));
        match bond_type {
            BondType::Single(single_type) => Self::draw_single_bond(frame, single_type, from, to, stroke),
            BondType::Double(double_type) => Self::draw_double_bond(frame, double_type, from, to, stroke),
            BondType::Triple => Self::draw_triple_bond(frame, from, to, stroke),
            BondType::Hidden => {},
        }
    }
    fn draw_single_bond(frame: &mut Frame, single_type: &SingleBond, from: &Vector, to: &Vector, stroke: Stroke) {
        match single_type {
            SingleBond::Normal => Self::draw_norm_single_bond(frame, from, to, stroke),
            SingleBond::Forward => Self::draw_forward_single_bond(frame, from, to),
            SingleBond::Backward => Self::draw_backward_single_bond(frame, from, to),
            SingleBond::Wide => {
                let width = 4.0 * stroke.width;
                let stroke = stroke.with_width(width);
                let v = &(2.0 * (to - from).norm());
                let from = from - v;
                let to = to + v;
                Self::draw_norm_single_bond(frame, &from, &to, stroke)
            },
        }
    }
    fn draw_double_bond(frame: &mut Frame, double_type: &DoubleBond, from: &Vector, to: &Vector, stroke: Stroke) {
        let mut path = Builder::new();
        let vv = to - from;
        let vv = &vv;
        let width = 4.0;
        let hv = width * vv.rotate(PI / 2.0).norm();
        let hv = &hv;
        let draw_left_right_bond = |path: &mut Builder, d: f64| {
            let pad = 0.1;
            path.move_to(from.into());
            path.line_to(to.into());
            path.move_to((from + pad * vv + d * hv).into());
            path.line_to((to - pad * vv + d * hv).into());
        };
        match double_type {
            DoubleBond::Left => draw_left_right_bond(&mut path, 1.0),
            DoubleBond::Right => draw_left_right_bond(&mut path, -1.0),
            DoubleBond::Center => {
                let pad = width / 2.0 / f64::sqrt(3.0);
                let vv = &vv.norm();
                path.move_to((from - pad * vv - 0.5 * hv).into());
                path.line_to((to + pad * vv - 0.5 * hv).into());
                path.move_to((from - pad * vv + 0.5 * hv).into());
                path.line_to((to + pad * vv + 0.5 * hv).into());
            },
        }
        let path = path.build();
        frame.stroke(&path, stroke);
    }
    fn draw_triple_bond(frame: &mut Frame, from: &Vector, to: &Vector, stroke: Stroke) {
        let mut path = Builder::new();
        path.move_to(from.into());
        path.line_to(to.into());
        let vv = to - from;
        let vv = &vv;
        let hv = 4.0 * vv.rotate(PI / 2.0).norm();
        let hv = &hv;
        let short_from = &(from + 0.1 * vv);
        let short_to = &(to - 0.1 * vv);
        path.move_to((short_from + hv).into());
        path.line_to((short_to + hv).into());
        path.move_to((short_from - hv).into());
        path.line_to((short_to - hv).into());
        let path = path.build();
        frame.stroke(&path, stroke);
    }
    fn draw_norm_single_bond(frame: &mut Frame, from: &Vector, to: &Vector, stroke: Stroke) {
        let bond = Path::line(from.into(), to.into());
        frame.stroke(&bond, stroke);
    }
    fn draw_forward_single_bond(frame: &mut Frame, from: &Vector, to: &Vector) {
        let mut path = Builder::new();
        let hv = 3.0 * (to - from).rotate(PI / 2.0).norm();
        let hv = &hv;
        path.move_to((from - 0.3 * hv).into());
        path.line_to((to - hv).into());
        path.line_to((to + hv).into());
        path.move_to((from + 0.3 * hv).into());
        path.close();
        let path = path.build();
        frame.fill(&path, Color::BLACK);
    }
    fn draw_backward_single_bond(frame: &mut Frame, from: &Vector, to: &Vector) {
        let mut path = Builder::new();
        let hv = 3.0 * (to - from).rotate(PI / 2.0).norm();
        let hv = &hv;
        let length = (to - from).dist();
        let step_len = 3.0;
        let vv = to - from;
        let vv = &vv;
        let step = (length / step_len) as i32;
        for i in 0..step {
            let r = i as f64 / step as f64;
            path.move_to((from + r * vv - r * hv).into());
            path.line_to((from + r * vv + r * hv).into());
        }
        let path = path.build();
        let stroke = Stroke::default();
        frame.stroke(&path, stroke);
    }
    fn draw_element(frame: &mut Frame, atom: &Atom, bg_color: Color) {
        let size = 25.0;
        let p = &atom.get_point();
        if atom.get_element() != Element::Text(String::new()) {
            let bg = Path::circle(p.into(), size / 2.0);
            frame.fill(&bg, bg_color);
        }
        let setting = canvas::Text {
            content: String::new(),
            position: p.into(),
            color: Color::BLACK,
            size,
            font: iced::Font::Default,
            horizontal_alignment: iced::alignment::Horizontal::Center,
            vertical_alignment: iced::alignment::Vertical::Center,
        };
        let text = Self::string2text(atom.symbol(), setting);
        for t in text {
            frame.fill_text(t);
        }
    }
    fn write_text(frame: &mut Frame, input: String, center_hint_color: Color) {
        let size = 25.0;
        let v_pad = size / 3.0;
        let center = iced::Point::new(4.0 * size, frame.height() - v_pad - 0.5 * size);
        let set = canvas::Text {
            content: String::new(),
            position: center.into(),
            color: Color::BLACK,
            size,
            font: iced::Font::Default,
            horizontal_alignment: iced::alignment::Horizontal::Center,
            vertical_alignment: iced::alignment::Vertical::Center,
        };
        let text = Self::string2text(input, set);
        for t in text {
            frame.fill_text(t);
        }

        Self::mark_atom(frame, &Vector::from(center), center_hint_color);
    }
    fn string2text(
        text: String,
        default_setting: canvas::Text
    ) -> Vec<canvas::Text> {
        let mut result = vec![];
        let mut cur_norm_text = String::new();
        let mut cur_low_up_text = String::new();
        let height = default_setting.size;
        let aspect = 0.5;
        let mut cursor_p = iced::Vector::new(0.0, 0.0);
        let pad = iced::Vector::new(height * aspect * match default_setting.horizontal_alignment {
            iced::alignment::Horizontal::Left => 0.0,
            iced::alignment::Horizontal::Center => 0.5,
            iced::alignment::Horizontal::Right => 1.0,
        }, 0.0);
        let mut accent = pad.clone();
        let append = |t: &mut String, setting: &canvas::Text, v: &mut iced::Vector, result: &mut Vec<canvas::Text>| {
            result.push(canvas::Text {
                content: t.clone(),
                position: setting.position + *v,
                ..setting.clone()
            });
            *v = *v + iced::Vector::new(setting.size * aspect * t.len() as f32, 0.0);
            t.clear();
        };
        let up_low_size_ratio = 2.0 / 3.0;
        let upper_y = match default_setting.vertical_alignment {
            iced::alignment::Vertical::Top => up_low_size_ratio / 2.0,
            iced::alignment::Vertical::Center => 0.5,
            iced::alignment::Vertical::Bottom => 1.0 - up_low_size_ratio / 2.0,
        };
        let lower_y = upper_y - 1.0 + 0.2;
        let default_setting = canvas::Text {
            horizontal_alignment: iced::alignment::Horizontal::Left,
            ..default_setting
        };
        let upper = canvas::Text {
            position: iced::Point {
                x: default_setting.position.x,
                y: default_setting.position.y - upper_y * height,
            },
            size: default_setting.size * 2.0 / 3.0,
            ..default_setting.clone()
        };
        let lower = canvas::Text {
            position: iced::Point {
                x: default_setting.position.x,
                y: default_setting.position.y - lower_y * height,
            },
            size: default_setting.size * 2.0 / 3.0,
            ..default_setting.clone()
        };
        for c in text.chars() {
            match c {
                '0'..='9' => {
                    if !cur_norm_text.is_empty() {
                        append(&mut cur_norm_text, &default_setting, &mut cursor_p, &mut result);
                    }
                    cur_low_up_text.push(c);
                },
                'a'..='z' | 'A'..='Z' => {
                    if !cur_low_up_text.is_empty() {
                        append(&mut cur_low_up_text, &lower, &mut cursor_p, &mut result);
                    }
                    cur_norm_text.push(c);
                },
                '+' | '-' => {
                    if !cur_norm_text.is_empty() {
                        append(&mut cur_norm_text, &default_setting, &mut cursor_p, &mut result);
                    }
                    cur_low_up_text.push(c);
                    append(&mut cur_low_up_text, &upper, &mut cursor_p, &mut result);
                },
                ' ' => {
                    if !cur_low_up_text.is_empty() {
                        append(&mut cur_low_up_text, &lower, &mut cursor_p, &mut result);
                    }
                },
                '\'' => {
                    accent = pad + cursor_p + iced::Vector::new(
                        lower.size * aspect * cur_low_up_text.len() as f32
                        + default_setting.size * aspect * cur_norm_text.len() as f32,
                        0.0);
                },
                _ => {},
            }
        }
        append(&mut cur_norm_text, &default_setting, &mut cursor_p, &mut result);
        append(&mut cur_low_up_text, &lower, &mut cursor_p, &mut result);
        for t in result.iter_mut() {
            t.position = t.position - accent;
        }
        return result;
    }
    fn mark_atom(frame: &mut Frame, p: &Vector, color: Color) {
        let mark = Path::circle(p.into(), 25.0 / 2.0);
        // 1.0で不透明だけど文字は透過する？
        frame.fill(&mark, color);
    }
    fn mark_bond(frame: &mut Frame, from: &Vector, to: &Vector, color: Color) {
        let hv = 5.0 * (to - from).norm().rotate(PI / 2.0);
        let hv = &hv;
        let mut mark = Builder::new();
        mark.move_to((from - hv).into());
        mark.line_to((from + hv).into());
        mark.line_to((to + hv).into());
        mark.line_to((to - hv).into());
        mark.close();
        let mark = mark.build();
        frame.fill(&mark, color);
    }
    fn mark_select_area(frame: &mut Frame, left_top: &Vector, size: f64, color: Color) {
        let size = size as f32;
        let size = Size::new(size, size);
        let rect = Path::rectangle(left_top.into(), size);
        frame.fill(&rect, color);
    }

    fn calc_bounds_for_mol(&self) -> Rectangle {
        let margin = 50.0;
        let mut left_top = Vector::new(512.0, 256.0);
        let mut right_bot = Vector::new(512.0, 256.0);
        let atoms = self.chem_struct.get_atoms()
            .iter()
            .map(|atom| atom.get_point())
            .chain(
                self.chem_struct.get_bonds()
                    .into_iter()
                    .map(|(_, f, t)| (f, t))
                    .flat_map(|(f, t)| vec![f, t].into_iter())
            ).collect::<Vec<Vector>>();
        for p in atoms.iter() {
            if p.x < left_top.x {
                left_top.x = p.x;
            }
            if p.y < left_top.y {
                left_top.y = p.y;
            }
            if p.x > right_bot.x {
                right_bot.x = p.x;
            }
            if p.y > right_bot.y {
                right_bot.y = p.y;
            }
        }
        let bounds = Rectangle {
            x: left_top.x as f32 - margin,
            y: left_top.y as f32 - margin,
            width: (right_bot.x - left_top.x) as f32 + 2.0 * margin,
            height: (right_bot.y - left_top.y) as f32 + 2.0 * margin,
        };
        bounds
    }
    fn capture_canvas(&self, bounds: Rectangle) -> Vec<Geometry> {
        let margin = 50.0;
        let bounds = Rectangle {
            x: bounds.x.min(0.0), 
            y: bounds.y.min(0.0),
            width: bounds.x.max(0.0) + bounds.width + margin,
            height: bounds.y.max(0.0) + bounds.height + margin,
        };
        self.draw(
            &StructDrawState::default(),
            &Theme::default(),
            bounds,
            Cursor::Unavailable
        )
    }
}

impl Program<Message> for StructDraw {
    type State = StructDrawState;
    fn draw(
        &self,
        _state: &StructDrawState,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry> {
        let atoms = self.chem_struct.get_atoms();
        let bonds = self.chem_struct.get_bonds();
        let focus_atom = self.focus_atom.and_then(|id| self.chem_struct.get_point(id));
        let focus_bond = self.focus_bond
            .and_then(|(a1, a2)| {
                self.chem_struct.get_point(a1)
                    .zip(self.chem_struct.get_point(a2))
            });
        let hint_color = Color::new(1.0, 1.0, 0.0, 0.7);
        let selected_color = Color::new(1.0, 0.0, 0.0, 0.5);
        let select_area_left_top = &self.select_area_top_left;
        let chem_struct = self.canv.draw(bounds.size(), |frame| {
            for (bond, from, to) in &bonds {
                Self::draw_bond(frame, bond, from, to);
            }
            for atom in &atoms {
                Self::draw_element(frame, atom, Color::WHITE);
            }

            if let Some(atom_p) = self.selected_atom.and_then(|id| self.chem_struct.get_point(id)) {
                if self.mode != StructDrawState::Capturing {
                    Self::mark_atom(frame, &atom_p, selected_color);
                }
            }

            match self.mode {
                StructDrawState::AtomFocus => {
                    if let Some(p) = focus_atom.clone() {
                        Self::mark_atom(frame, &p, self.focus_color)
                    }
                },
                StructDrawState::BondFocus => {
                    if let Some(p) = focus_atom.clone() {
                        Self::mark_atom(frame, &p, hint_color);
                    }
                    if let Some((from, to)) = focus_bond.clone() {
                        Self::mark_bond(frame, &from, &to, self.focus_color);
                    }
                },
                StructDrawState::Move => {
                    Self::mark_select_area(frame, &select_area_left_top, self.select_area_size, self.focus_color);
                },
                StructDrawState::Input(_) => {
                    Self::write_text(frame, self.input.clone(), hint_color);
                },
                StructDrawState::Capturing => {},
            }
        });
        vec![chem_struct]
    }
}
