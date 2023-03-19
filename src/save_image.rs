use native_dialog::FileDialog;
use image::{ImageBuffer, Rgba};

pub fn save_image(image: ImageBuffer<Rgba<u8>, Vec<u8>>) {
    let path = FileDialog::new()
        .add_filter("PNG Image", &["png"])
        .add_filter("JPEG Image", &["jpg", "jpeg"])
        .show_open_single_file();
    println!("{:?}", path);
}
