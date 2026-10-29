use std::fs::{self, File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::Path;

use asf_core::{path::image_path, Error, Result};
use asf_meta::{read_metadata, write_metadata, DataType, Metadata};

#[derive(Debug, Clone)]
pub struct Raster {
    pub metadata: Metadata,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResampleMethod {
    Nearest,
    Bilinear,
    Average,
    LogicalOr,
}

pub fn open_raster(base_name: impl AsRef<Path>) -> Result<Raster> {
    let base_name = base_name.as_ref();
    let metadata = read_metadata(base_name)?;
    let img_path = image_path(base_name);

    let actual_size = fs::metadata(&img_path)?.len();
    let expected_size = expected_image_size(&metadata)?;

    if actual_size != expected_size {
        return Err(Error::InvalidInput(format!(
            "Raster image size does not match metadata for {}. Expected {} bytes, found {} bytes",
            img_path.display(),
            expected_size,
            actual_size
        )));
    }

    Ok(Raster { metadata })
}

pub fn create_raster(base_name: impl AsRef<Path>, metadata: &Metadata) -> Result<Raster> {
    let base_name = base_name.as_ref();

    if metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "create_raster currently supports only REAL32 rasters, found {}",
            metadata.general.data_type.as_meta_str()
        )));
    }

    if metadata.bands.len() != metadata.general.band_count {
        return Err(Error::InvalidInput(format!(
            "Metadata band_count is {}, but bands contains {} names",
            metadata.general.band_count,
            metadata.bands.len()
        )));
    }

    write_metadata(base_name, metadata)?;

    let img_path = image_path(base_name);
    let file = File::create(&img_path)?;
    file.set_len(expected_image_size(metadata)?)?;

    Ok(Raster {
        metadata: metadata.clone(),
    })
}

pub fn read_f32_band(base_name: impl AsRef<Path>, band_index: usize) -> Result<Vec<f32>> {
    let base_name = base_name.as_ref();
    let raster = open_raster(base_name)?;
    let metadata = &raster.metadata;

    if metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "read_f32_band currently supports only REAL32 rasters, found {}",
            metadata.general.data_type.as_meta_str()
        )));
    }

    if band_index >= metadata.general.band_count {
        return Err(Error::InvalidArgument(format!(
            "Band index {} is out of range for raster with {} bands",
            band_index, metadata.general.band_count
        )));
    }

    let samples_per_band = metadata
        .general
        .line_count
        .checked_mul(metadata.general.sample_count)
        .ok_or_else(|| Error::InvalidInput("Raster dimensions overflow usize".to_string()))?;

    let bytes_per_band = samples_per_band
        .checked_mul(DataType::F32.byte_size())
        .ok_or_else(|| Error::InvalidInput("Raster band byte size overflows usize".to_string()))?;

    let byte_offset = band_index
        .checked_mul(bytes_per_band)
        .ok_or_else(|| Error::InvalidInput("Raster band offset overflows usize".to_string()))?;

    let img_path = image_path(base_name);
    let mut file = File::open(&img_path)?;
    file.seek(SeekFrom::Start(byte_offset as u64))?;

    let mut bytes = vec![0_u8; bytes_per_band];
    file.read_exact(&mut bytes)?;

    let samples = bytes
        .chunks_exact(4)
        .map(|chunk| f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
        .collect();

    Ok(samples)
}

pub fn write_f32_band(
    base_name: impl AsRef<Path>,
    band_index: usize,
    samples: &[f32],
) -> Result<()> {
    let base_name = base_name.as_ref();
    let raster = open_raster(base_name)?;
    let metadata = &raster.metadata;

    if metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "write_f32_band currently supports only REAL32 rasters, found {}",
            metadata.general.data_type.as_meta_str()
        )));
    }

    if band_index >= metadata.general.band_count {
        return Err(Error::InvalidArgument(format!(
            "Band index {} is out of range for raster with {} bands",
            band_index, metadata.general.band_count
        )));
    }

    let samples_per_band = metadata
        .general
        .line_count
        .checked_mul(metadata.general.sample_count)
        .ok_or_else(|| Error::InvalidInput("Raster dimensions overflow usize".to_string()))?;

    if samples.len() != samples_per_band {
        return Err(Error::InvalidArgument(format!(
            "Band {} write expected {} samples, received {}",
            band_index,
            samples_per_band,
            samples.len()
        )));
    }

    let bytes_per_band = samples_per_band
        .checked_mul(DataType::F32.byte_size())
        .ok_or_else(|| Error::InvalidInput("Raster band byte size overflows usize".to_string()))?;

    let byte_offset = band_index
        .checked_mul(bytes_per_band)
        .ok_or_else(|| Error::InvalidInput("Raster band offset overflows usize".to_string()))?;

    let img_path = image_path(base_name);
    let mut file = OpenOptions::new().write(true).open(&img_path)?;
    file.seek(SeekFrom::Start(byte_offset as u64))?;

    for sample in samples {
        file.write_all(&sample.to_le_bytes())?;
    }

    Ok(())
}

fn expected_image_size(metadata: &Metadata) -> Result<u64> {
    let pixel_count = metadata
        .general
        .line_count
        .checked_mul(metadata.general.sample_count)
        .and_then(|value| value.checked_mul(metadata.general.band_count))
        .ok_or_else(|| Error::InvalidInput("Raster dimensions overflow usize".to_string()))?;

    let byte_count = pixel_count
        .checked_mul(metadata.general.data_type.byte_size())
        .ok_or_else(|| Error::InvalidInput("Raster image byte size overflows usize".to_string()))?;

    Ok(byte_count as u64)
}
