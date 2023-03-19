use iced::keyboard::{KeyCode, Modifiers};

#[derive(Debug, Clone)]
pub enum Message {
    KeyEvent(Modifiers, KeyCode),
    Saved,
}
