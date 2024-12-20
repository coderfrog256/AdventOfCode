// Common error type that includes std::io::Error and ParseIntError's parent type
use std::io;
use std::num;
use std::fmt;

pub enum Error {
    Io(io::Error),
    Parse(num::ParseIntError),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<num::ParseIntError> for Error {
    fn from(err: num::ParseIntError) -> Error {
        Error::Parse(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Io(ref err) => write!(f, "IO error: {}", err),
            Error::Parse(ref err) => write!(f, "Parse error: {}", err),
        }
    }
}
