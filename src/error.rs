use std::fmt;
use std::error;
use std::io;

#[derive(Debug)]
pub enum Error {
	IO(io::Error),
	End,
	Malformed,
}

impl From<io::Error> for Error {
	fn from(value: io::Error) -> Self {
		Error::IO(value)
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
		f.write_str(error::Error::description(self))
	}
}

impl error::Error for Error {
	fn description(&self) -> &str {
		match self {
			&Error::IO(ref err) =>
				err.description(),

			&Error::End =>
				"End of file reached.",

			&Error::Malformed =>
				"Malformed filter.",
		}
	}
}
