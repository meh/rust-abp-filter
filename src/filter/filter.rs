use std::ops::{Deref, DerefMut};
use std::option::Option as Opt;
use super::Matchable;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Filter {
	uri:      Vec<Exception<Vec<Matcher>>>,
	options:  Vec<Exception<Option>>,
	selector: Opt<Exception<String>>,
}

impl Filter {
	pub fn new(uri: Vec<Exception<Vec<Matcher>>>, options: Vec<Exception<Option>>, selector: Opt<Exception<String>>) -> Self {
		Filter {
			uri:      uri,
			options:  options,
			selector: selector,
		}
	}
	
	pub fn uri(&self) -> Vec<Exception<&[Matcher]>> {
		self.uri.iter().map(|m| m.as_ref().map(|v| v.as_ref())).collect()
	}

	pub fn options(&self) -> &[Exception<Option>] {
		&*self.options
	}

	pub fn selector<'a>(&'a self) -> Opt<Exception<&'a str>> {
		self.selector.as_ref().map(|x| x.as_ref().map(|s| s.as_ref()))
	}

	pub fn matches<M: Matchable + ?Sized>(&self, thing: &M) -> bool {
		thing.matches(self)
	}
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Exception<T> {
	No(T),
	Yes(T),
}

impl<T> Exception<T> {
	pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Exception<U> {
		match self {
			Exception::No(v)  => Exception::No(f(v)),
			Exception::Yes(v) => Exception::Yes(f(v)),
		}
	}

	pub fn as_ref(&self) -> Exception<&T> {
		match self {
			&Exception::No(ref v)  => Exception::No(v),
			&Exception::Yes(ref v) => Exception::Yes(v),
		}
	}

	pub fn yes(&self) -> bool {
		if let &Exception::Yes(..) = self {
			true
		}
		else {
			false
		}
	}

	pub fn no(&self) -> bool {
		if let &Exception::No(..) = self {
			true
		}
		else {
			false
		}
	}

	pub fn into_inner(self) -> T {
		match self {
			Exception::No(v)  => v,
			Exception::Yes(v) => v,
		}
	}
}

impl<T> Deref for Exception<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		match self {
			&Exception::No(ref v)  => v,
			&Exception::Yes(ref v) => v,
		}
	}
}

impl<T> DerefMut for Exception<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		match self {
			&mut Exception::No(ref mut v)  => v,
			&mut Exception::Yes(ref mut v) => v,
		}
	}
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Matcher {
	Anchor,
	Verbatim(String),
	Regex(String),
	Domain(Vec<String>),
	Wildcard,
	Separator,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Option {
	/// Include or exclude JavaScript files.
	Script,

	/// Include or exclude image files.
	Image,

	/// Include or exclude stylesheets (CSS files).
	StyleSheet,

	/// Include or exclude content handled by browser plugins like Flash or Java.
	Object,

	/// Include or exclude files loaded by browser plugins.
	ObjectSubRequest,

	/// Used to whitelist the page itself.
	Document,

	/// Include or exclude pages loaded within pages (frames).
	SubDocument,

	/// Specify whether a filter should be active on third-party or first
	/// domains.
	ThirdParty,

	///
	Domain(Vec<Exception<String>>),

	/// Makes the filter only apply to addresses with matching letter case.
	MatchCase,

	/// Include or exclude requests started through XHR.
	XmlHttpRequst,

	/// Used to prevent element rules from applying on a page.
	ElemHide,

	///
	SiteKey(Vec<String>),

	/// Always or never hide elements.
	Collapse,

	/// For any address matching a blocking rule with this option and not
	/// matching any exception rules with this option a Do-Not-Track header will
	/// be sent.
	DoNotTrack,

	/// Types of requests not covered in the list above.
	Other,
}

#[cfg(test)]
mod tests {
	use nom::IResult::Done;
	use super::Exception::No;
	use super::{Matcher, Option, Filter};
	use filter;

	#[test]
	fn parse() {
		assert_eq!(filter::parse(b"google.com"), Done(&b""[..], No(Filter {
			uri:      vec![No(vec![Matcher::Verbatim("google.com".to_owned())])],
			options:  vec![],
			selector: None })));

		assert_eq!(filter::parse(b"google.com$script"), Done(&b""[..], No(Filter {
			uri:      vec![No(vec![Matcher::Verbatim("google.com".to_owned())])],
			options:  vec![No(Option::Script)],
			selector: None })));

		assert_eq!(filter::parse(b"google.com##.hue"), Done(&b""[..], No(Filter {
			uri:      vec![No(vec![Matcher::Verbatim("google.com".to_owned())])],
			options:  vec![],
			selector: Some(No(".hue".to_owned())) })));

		assert_eq!(filter::parse(b"google.com$script##.hue"), Done(&b""[..], No(Filter {
			uri:      vec![No(vec![Matcher::Verbatim("google.com".to_owned())])],
			options:  vec![No(Option::Script)],
			selector: Some(No(".hue".to_owned())) })));
	}
}
