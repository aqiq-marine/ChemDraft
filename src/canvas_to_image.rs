use std::num::NonZeroU32;

use iced::{widget::canvas::Geometry, Rectangle};
use iced_core;
use iced_graphics::Primitive;
use iced_wgpu::{
    self,
    wgpu::{
        self, util::StagingBelt, Backend, Buffer, BufferAsyncError, BufferDescriptor,
        CommandEncoder, CommandEncoderDescriptor, Device, DeviceDescriptor, Extent3d,
        Instance, Limits, Queue, Texture, TextureDescriptor, TextureFormat,
    },
};
use image::{self, GenericImageView, ImageBuffer, Rgba};

// mikan
// 画像内のフォントがMyrica（等幅フォント）でない
pub async fn geometry_to_image(
    geometries: Vec<Geometry>,
    bounds: Rectangle
) -> ImageBuffer<Rgba<u8>, Vec<u8>> {
    let width = bounds.width as u32;
    let width =
        width + wgpu::COPY_BYTES_PER_ROW_ALIGNMENT - width % wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;
    let height = bounds.height as u32;

    let primitives: Vec<_> = {
        let primitives: Vec<Primitive> = geometries
            .into_iter()
            .map(|g| g.into_primitive())
            .collect();
        let primitives = Primitive::Group { primitives };
        let primitives = Primitive::Translate {
            translation: iced::Vector {
                x: -bounds.x,
                y: -bounds.y,
            },
            content: Box::new(primitives),
        };
        vec![primitives]
    };

    let (device, queue) = create_device().await;
    let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
        label: Some("my encoder"),
    });
    let format = TextureFormat::Rgba8Unorm;
    let pixel_byte = 4;
    let mut staging_belt = StagingBelt::new(((pixel_byte + 1) * width * height) as u64);
    let texture = make_drawed_texture(
        primitives,
        &device,
        &mut encoder,
        format,
        &mut staging_belt,
        width,
        height,
    );

    let buffer = copy_texture_to_buffer(&device, &mut encoder, texture, width, height, pixel_byte);

    staging_belt.finish();
    queue.submit(Some(encoder.finish()));
    staging_belt.recall();

    let raw_data = buffer_to_raw_data(&device, buffer).await.unwrap();
    let image =
        image::ImageBuffer::<image::Rgba<u8>, _>::from_raw(width, height, raw_data).unwrap()
        .view(0, 0, bounds.width as u32, bounds.height as u32)
        .to_image();
    image.save("test_image1.png").unwrap();
    return image;
}

async fn create_device() -> (Device, Queue) {
    let instance = Instance::new(Backend::Vulkan.into());
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            force_fallback_adapter: false,
            compatible_surface: None,
        })
        .await
        .unwrap();

    let device_descriptor = DeviceDescriptor {
        label: Some("my device"),
        features: adapter.features() & wgpu::Features::default(),
        limits: Limits::default(),
    };

    adapter
        .request_device(&device_descriptor, None)
        .await
        .unwrap()
}

fn make_drawed_texture(
    primitives: Vec<Primitive>,
    device: &Device,
    encoder: &mut CommandEncoder,
    format: TextureFormat,
    staging_belt: &mut StagingBelt,
    width: u32,
    height: u32,
) -> Texture {
    let mut backend =
        iced_wgpu::Backend::new(device, iced_wgpu::settings::Settings::default(), format);
    let texture_size = Extent3d {
        width,
        height,
        depth_or_array_layers: 1,
    };
    let texture = device.create_texture(&TextureDescriptor {
        label: Some("my texture"),
        size: texture_size,
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
    });
    let texture_view = texture.create_view(&wgpu::TextureViewDescriptor {
        format: Some(format),
        ..wgpu::TextureViewDescriptor::default()
    });
    let viewport = iced_wgpu::Viewport::with_physical_size(iced_core::Size { width, height }, 1.0);

    backend.present::<String>(
        &device,
        staging_belt,
        encoder,
        &texture_view,
        &primitives,
        &viewport,
        &vec![],
    );
    return texture;
}

fn copy_texture_to_buffer(
    device: &Device,
    encoder: &mut CommandEncoder,
    texture: Texture,
    width: u32,
    height: u32,
    pixel_byte: u32,
) -> Buffer {
    let buffer = device.create_buffer(&BufferDescriptor {
        label: Some("my buffer"),
        size: (pixel_byte * width * height) as u64,
        usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
        mapped_at_creation: false,
    });

    encoder.copy_texture_to_buffer(
        wgpu::ImageCopyTexture {
            aspect: wgpu::TextureAspect::All,
            texture: &texture,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
        },
        wgpu::ImageCopyBuffer {
            buffer: &buffer,
            layout: wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: NonZeroU32::new(4 * width),
                rows_per_image: NonZeroU32::new(height),
            },
        },
        Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
    );
    return buffer;
}

async fn buffer_to_raw_data(device: &Device, buffer: Buffer) -> Result<Vec<u8>, BufferAsyncError> {
    let buffer_slice = buffer.slice(..);
    let (tx, rx) = tokio::sync::oneshot::channel();
    buffer_slice.map_async(wgpu::MapMode::Read, move |result| {
        if let Err(_) = tx.send(result) {
            println!("some error occur in map_async");
        }
    });
    device.poll(wgpu::Maintain::Wait);
    rx.await.unwrap().map(|_| {
        let buffer_view = buffer.slice(..).get_mapped_range();
        let data: Vec<u8> = buffer_view.iter().cloned().collect();
        drop(buffer_view);
        buffer.unmap();
        data
    })
}
