use std::io::{Read, BufRead, BufReader, Lines};
use nom::IResult;

use {Error, Filter, filter};

pub struct Reader<R: Read> {
	stream: Lines<BufReader<R>>,
}

impl<R: Read> From<R> for Reader<R> {
	fn from(stream: R) -> Reader<R> {
		Reader {
			stream: BufReader::new(stream).lines(),
		}
	}
}

impl<R: Read> Reader<R> {
	fn filter(&mut self) -> Result<filter::Exception<Filter>, Error> {
		loop {
			let line = try!(try!(self.stream.next().ok_or(Error::End)));

			if line.starts_with('!') || line.starts_with('[') {
				continue;
			}

			match filter::parse(&line) {
				IResult::Done(_, filter) =>
					return Ok(filter),

				IResult::Incomplete(..) =>
					return Err(Error::Malformed),

				IResult::Error(..) =>
					return Err(Error::Malformed),
			}
		}
	}
}

impl<R: Read> Iterator for Reader<R> {
	type Item = filter::Exception<Filter>;

	fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		match self.filter() {
			Ok(filter) =>
				Some(filter),

			Err(..) =>
				None,
		}
	}
}
