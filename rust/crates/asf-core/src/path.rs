use std::path::{Path, PathBuf};

pub fn with_extension(base: impl AsRef<Path>, extension: &str) -> PathBuf {
    let mut path = base.as_ref().to_path_buf();
    path.set_extension(extension.trim_start_matches('.'));
    path
}

pub fn image_path(base: impl AsRef<Path>) -> PathBuf {
    with_extension(base, "img")
}

pub fn metadata_path(base: impl AsRef<Path>) -> PathBuf {
    with_extension(base, "meta")
}
