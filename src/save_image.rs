use std::error::Error;

use native_dialog::FileDialog;
use image::{ImageBuffer, Rgba};

pub fn save_image(image: ImageBuffer<Rgba<u8>, Vec<u8>>) -> Result<(), Box<dyn Error>> {
    let path = FileDialog::new()
        .add_filter("PNG Image", &["png"])
        .show_save_single_file()?.ok_or(native_dialog::Error::NoImplementation)?;
    image.save(path)?;
    Ok(())
}
