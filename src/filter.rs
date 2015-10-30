use nom::{IResult, alphanumeric, eof};
use std::str;

use std::option::Option as Opt;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Exception<T> {
	No(T),
	Yes(T),
}

impl<T> Exception<T> {
	fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Exception<U> {
		match self {
			Exception::No(v) => Exception::No(f(v)),
			Exception::Yes(v) => Exception::Yes(f(v)),
		}
	}
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Filter {
	uri:      Vec<Vec<Matcher>>,
	options:  Vec<Exception<Option>>,
	selector: Opt<Exception<String>>,
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
	Script,
	Image,
	StyleSheet,
	Object,
	ObjectSubRequest,
	Document,
	SubDocument,
	ThirdParty,
	Domain(Vec<Exception<String>>),
	MatchCase,
	XmlHttpRequst,
	ElemHide,
	SiteKey(Vec<String>),
	Collapse,
	DoNotTrack,
	Other,
}

impl Filter {
	pub fn parse<T: AsRef<[u8]> + ?Sized>(line: &T) -> IResult<&[u8], Exception<Filter>> {
		root(line.as_ref())
	}
}

fn rest(i: &[u8]) -> IResult<&[u8], &[u8]> {
	IResult::Done(b"", i)
}

named!(root(&[u8]) -> Exception<Filter>, chain!(
	exception: tag!("@@")? ~

	matchers: matchers ~
	options:  complete!(options)? ~
	selector: complete!(selector)? ~

	eof,

	|| {
		let result = Filter {
			uri:      matchers,
			options:  options.unwrap_or(Vec::new()),
			selector: selector.map(|e| e.map(|s| s.to_owned())),
		};

		if exception.is_none() {
			Exception::No(result)
		}
		else {
			Exception::Yes(result)
		}
	}));

named!(matchers(&[u8]) -> Vec<Vec<Matcher>>, chain!(
	first: many1!(matcher) ~
	rest:  many0!(chain!(tag!(",") ~ m: many1!(matcher), || m)),

	|| {
		let mut result = vec![first];
		result.extend(rest);
		result
	}));

named!(matcher(&[u8]) -> Matcher, alt!(
	chain!(tag!("||") ~ d: domain, || { Matcher::Domain(d.iter().map(|&s| s.to_owned()).collect()) })
	|
	tag!("|") => { |_| Matcher::Anchor }
	|
	tag!("^") => { |_| Matcher::Separator }
	|
	tag!("*") => { |_| Matcher::Wildcard }
	|
	verbatim => { |s| Matcher::Verbatim(String::from(s)) }));

named!(domain(&[u8]) -> Vec<&str>, chain!(
	first: map_res!(alphanumeric, |s| str::from_utf8(s)) ~
	rest:  many0!(chain!(tag!(".") ~
		part: map_res!(alphanumeric, |s| str::from_utf8(s)), || { part })),

	|| {
		let mut result = vec![first];
		result.extend(rest);
		result
	}));

named!(verbatim(&[u8]) -> &str, map_res!(
	alt!(take_until_either!("|^*$#") | rest),
	str::from_utf8));

named!(options(&[u8]) -> Vec<Exception<Option>>, chain!(tag!("$") ~
	first: option ~
	rest:  many0!(chain!(tag!(",") ~ o: option, || o)),

	|| {
		let mut result = vec![first];
		result.extend(rest);
		result
	}));

named!(option(&[u8]) -> Exception<Option>, alt!(
	complete!(tag!("script"))  => { |_| Exception::No(Option::Script) } |
	complete!(tag!("~script")) => { |_| Exception::Yes(Option::Script) } |

	complete!(tag!("image")) => { |_| Exception::No(Option::Image) } |
	complete!(tag!("~image")) => { |_| Exception::Yes(Option::Image) } |

	complete!(tag!("stylesheet")) => { |_| Exception::No(Option::StyleSheet) } |
	complete!(tag!("~stylesheet")) => { |_| Exception::Yes(Option::StyleSheet) } |

	complete!(tag!("object-subrequest")) => { |_| Exception::No(Option::ObjectSubRequest) } |
	complete!(tag!("~object-subrequest")) => { |_| Exception::Yes(Option::ObjectSubRequest) } |

	complete!(tag!("object")) => { |_| Exception::No(Option::Object) } |
	complete!(tag!("~object")) => { |_| Exception::Yes(Option::Object) } |

	complete!(tag!("document")) => { |_| Exception::No(Option::Document) } |
	complete!(tag!("~document")) => { |_| Exception::Yes(Option::Document) } |

	complete!(tag!("subdocument")) => { |_| Exception::No(Option::SubDocument) } |
	complete!(tag!("~subdocument")) => { |_| Exception::Yes(Option::SubDocument) } |

	complete!(tag!("third-party")) => { |_| Exception::No(Option::ThirdParty) } |
	complete!(tag!("~third-party")) => { |_| Exception::Yes(Option::ThirdParty) } |

	complete!(tag!("match-case")) => { |_| Exception::No(Option::MatchCase) } |
	complete!(tag!("~match-case")) => { |_| Exception::Yes(Option::MatchCase) } |

	complete!(tag!("xmlhttprequest")) => { |_| Exception::No(Option::XmlHttpRequst) } |
	complete!(tag!("~xmlhttprequest")) => { |_| Exception::Yes(Option::XmlHttpRequst) } |

	complete!(tag!("elemhide")) => { |_| Exception::No(Option::ElemHide) } |
	complete!(tag!("~elemhide")) => { |_| Exception::Yes(Option::ElemHide) } |

	complete!(tag!("collapse")) => { |_| Exception::No(Option::Collapse) } |
	complete!(tag!("~collapse")) => { |_| Exception::Yes(Option::Collapse) } |

	complete!(tag!("donottrack")) => { |_| Exception::No(Option::DoNotTrack) } |
	complete!(tag!("~donottrack")) => { |_| Exception::Yes(Option::DoNotTrack) } |

	complete!(tag!("other")) => { |_| Exception::No(Option::Other) } |
	complete!(tag!("~other")) => { |_| Exception::Yes(Option::Other) }));

named!(selector(&[u8]) -> Exception<&str>, map_res!(
	alt!(chain!(tag!("##") ~ s: rest, || { Exception::No(s) }) |
	     chain!(tag!("#@#") ~ s: rest, || { Exception::Yes(s) })),

	|s| match s {
		Exception::No(s) => str::from_utf8(s).map(|s| Exception::No(s)),
		Exception::Yes(s) => str::from_utf8(s).map(|s| Exception::Yes(s)),
	}));

#[cfg(test)]
mod tests {
	use nom::IResult::Done;
	use super::Exception::{No, Yes};
	use super::{Option, Matcher, Filter};

	#[test]
	fn full() {
		println!("{:?}", super::root(b"google.com"));

		assert_eq!(super::root(b"google.com$script"), Done(&b""[..], No(Filter {
			uri:      vec![vec![Matcher::Verbatim("google.com".to_owned())]],
			options:  vec![No(Option::Script)],
			selector: None })));

		assert_eq!(super::root(b"google.com##.hue"), Done(&b""[..], No(Filter {
			uri:      vec![vec![Matcher::Verbatim("google.com".to_owned())]],
			options:  vec![],
			selector: Some(No(".hue".to_owned())) })));

		assert_eq!(super::root(b"google.com$script##.hue"), Done(&b""[..], No(Filter {
			uri:      vec![vec![Matcher::Verbatim("google.com".to_owned())]],
			options:  vec![No(Option::Script)],
			selector: Some(No(".hue".to_owned())) })));
	}

	#[test]
	fn domain() {
		assert_eq!(super::domain(b"google"), Done(&b""[..], vec!["google"]));
		assert_eq!(super::domain(b"google.com"), Done(&b""[..], vec!["google", "com"]));
		assert_eq!(super::domain(b"crap.google.com"), Done(&b""[..], vec!["crap", "google", "com"]));
	}

	#[test]
	fn verbatim() {
		assert_eq!(super::verbatim(b"lol.wut"), Done(&b""[..], "lol.wut"));
		assert_eq!(super::verbatim(b"lol.wut$"), Done(&b"$"[..], "lol.wut"));
		assert_eq!(super::verbatim(b"lol.wut*"), Done(&b"*"[..], "lol.wut"));
		assert_eq!(super::verbatim(b"lol.wut^"), Done(&b"^"[..], "lol.wut"));
		assert_eq!(super::verbatim(b"lol.wut|"), Done(&b"|"[..], "lol.wut"));
	}

	#[test]
	fn options() {
		assert_eq!(super::options(b"$script,~object"), Done(&b""[..],
			vec![No(Option::Script), Yes(Option::Object)]));
	}

	#[test]
	fn option() {
		assert_eq!(super::option(b"script"), Done(&b""[..], No(Option::Script)));
		assert_eq!(super::option(b"~script"), Done(&b""[..], Yes(Option::Script)));

		assert_eq!(super::option(b"image"), Done(&b""[..], No(Option::Image)));
		assert_eq!(super::option(b"~image"), Done(&b""[..], Yes(Option::Image)));

		assert_eq!(super::option(b"stylesheet"), Done(&b""[..], No(Option::StyleSheet)));
		assert_eq!(super::option(b"~stylesheet"), Done(&b""[..], Yes(Option::StyleSheet)));

		assert_eq!(super::option(b"object-subrequest"), Done(&b""[..], No(Option::ObjectSubRequest)));
		assert_eq!(super::option(b"~object-subrequest"), Done(&b""[..], Yes(Option::ObjectSubRequest)));

		assert_eq!(super::option(b"object"), Done(&b""[..], No(Option::Object)));
		assert_eq!(super::option(b"~object"), Done(&b""[..], Yes(Option::Object)));

		assert_eq!(super::option(b"document"), Done(&b""[..], No(Option::Document)));
		assert_eq!(super::option(b"~document"), Done(&b""[..], Yes(Option::Document)));

		assert_eq!(super::option(b"subdocument"), Done(&b""[..], No(Option::SubDocument)));
		assert_eq!(super::option(b"~subdocument"), Done(&b""[..], Yes(Option::SubDocument)));

		assert_eq!(super::option(b"third-party"), Done(&b""[..], No(Option::ThirdParty)));
		assert_eq!(super::option(b"~third-party"), Done(&b""[..], Yes(Option::ThirdParty)));

		assert_eq!(super::option(b"match-case"), Done(&b""[..], No(Option::MatchCase)));
		assert_eq!(super::option(b"~match-case"), Done(&b""[..], Yes(Option::MatchCase)));

		assert_eq!(super::option(b"xmlhttprequest"), Done(&b""[..], No(Option::XmlHttpRequst)));
		assert_eq!(super::option(b"~xmlhttprequest"), Done(&b""[..], Yes(Option::XmlHttpRequst)));

		assert_eq!(super::option(b"elemhide"), Done(&b""[..], No(Option::ElemHide)));
		assert_eq!(super::option(b"~elemhide"), Done(&b""[..], Yes(Option::ElemHide)));

		assert_eq!(super::option(b"collapse"), Done(&b""[..], No(Option::Collapse)));
		assert_eq!(super::option(b"~collapse"), Done(&b""[..], Yes(Option::Collapse)));

		assert_eq!(super::option(b"donottrack"), Done(&b""[..], No(Option::DoNotTrack)));
		assert_eq!(super::option(b"~donottrack"), Done(&b""[..], Yes(Option::DoNotTrack)));

		assert_eq!(super::option(b"other"), Done(&b""[..], No(Option::Other)));
		assert_eq!(super::option(b"~other"), Done(&b""[..], Yes(Option::Other)));
	}

	#[test]
	fn selector() {
		assert_eq!(super::selector(b"##.hue"), Done(&b""[..], No(".hue")));
		assert_eq!(super::selector(b"#@#.hue"), Done(&b""[..], Yes(".hue")));
	}
}
