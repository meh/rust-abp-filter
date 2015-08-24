#[macro_use]
extern crate nom;

use std::io::Read;
use std::fs::File;
use std::path::Path;

mod error;
pub use error::Error;

pub mod filter;
pub use filter::Filter;

mod reader;
pub use reader::Reader;

pub fn open<P: AsRef<Path>>(path: P) -> Result<Reader<File>, Error> {
	Ok(read(try!(File::open(path))))
}

pub fn read<R: Read>(stream: R) -> Reader<R> {
	Reader::from(stream)
}
