pub mod path;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unsupported format: {0}")]
    UnsupportedFormat(String),

    #[error("invalid argument: {0}")]
    InvalidArgument(String),

    #[error("invalid input: {0}")]
    InvalidInput(String),

    #[error("unsupported data type: {0}")]
    UnsupportedDataType(String),

    #[error("metadata error: {0}")]
    Metadata(String),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("{0}")]
    Message(String),
}

pub fn strip_img_extension(path: &str) -> &str {
    path.strip_suffix(".img").unwrap_or(path)
}
