use iced::{
    self,
    executor, Application, Command, Element,
    Subscription, subscription, Event, keyboard,
    Theme, Settings,
    Length,
    widget::canvas::{self, Canvas},
};
mod struct_draw;
use struct_draw::StructDraw;
mod message;
use message::Message;
mod element;
mod bond_type;
mod chem_struct;
mod vector;

fn main() -> iced::Result {
    ChemDraft::run(Settings {
        antialiasing: true,
        window: iced::window::Settings {
            size: (1024, 512),
            ..iced::window::Settings::default()
        },
        ..Settings::default()
    })
}

struct ChemDraft {
    struct_draw: StructDraw,
}

impl Application for ChemDraft {
    type Message = Message;
    type Executor = executor::Default;
    type Flags = ();
    type Theme = Theme;

    fn new(_flags: ()) -> (Self, Command<Message>) {
        (
            Self{struct_draw: StructDraw::default()},
            Command::none(),
        )
    }
    fn title(&self) -> String {
        String::from("Chemical Structure Drawer")
    }
    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        match message {
            Message::KeyEvent(modifiers, key_code) => self.struct_draw.handle_key(modifiers, key_code),
        }
        Command::none()
    }
    fn view(&self) -> Element<Self::Message> {
        Canvas::new(&self.struct_draw)
            .width(Length::Fill)
            .height(Length::Fill)
            .into()
    }

    fn subscription(&self) -> Subscription<Self::Message> {
        subscription::events_with(|event, _status| match event {
            Event::Keyboard(keyboard::Event::KeyPressed {
                modifiers,
                key_code,
            }) => Some(Message::KeyEvent(modifiers, key_code)),
            _ => None,
        })
    }
}
