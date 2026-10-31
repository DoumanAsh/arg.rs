#![doc = include_str!("../README.md")]
#![no_std]
#![warn(missing_docs)]
#![allow(clippy::style)]
#![allow(clippy::needless_lifetimes)]

#[cfg(feature = "std")]
extern crate std;

pub use arg_derive::*;

mod split;
pub use split::Split;

use core::fmt;

#[derive(PartialEq, Eq, Debug)]
///Parse errors
pub enum ParseKind<'a> {
    ///Main command result
    Top(ParseError<'a>),
    ///Sub-command name and result
    Sub(&'static str, ParseError<'a>),
}

impl<'a> ParseKind<'a> {
    #[inline]
    ///Returns whether help is requested
    pub fn is_help(&self) -> bool {
        match self {
            Self::Top(err) => err.is_help(),
            Self::Sub(_, err) => err.is_help(),
        }
    }
}

impl<'a> PartialEq<ParseError<'a>> for ParseKind<'a> {
    #[inline(always)]
    fn eq(&self, right: &ParseError<'a>) -> bool {
        match self {
            Self::Top(left) => PartialEq::eq(left, right),
            Self::Sub(_, left) => PartialEq::eq(left, right),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
///Parse errors
pub enum ParseError<'a> {
    ///User requested help.
    ///
    ///Contains slice with `Args::HELP`
    HelpRequested(&'static str),
    ///Too many arguments are specified.
    TooManyArgs,
    ///Argument is required, but missing
    ///
    ///Contains name of argument
    RequiredArgMissing(&'a str),
    ///Flag is specified, but value is missing.
    ///
    ///Contains full flag name.
    MissingValue(&'a str),
    ///Flag is specified with invalid value
    ///
    ///Contains full flag name and provided value.
    InvalidFlagValue(&'a str, &'a str),
    ///Argument is supplied with invalid value
    ///
    ///Contains argument name and provided value.
    InvalidArgValue(&'a str, &'a str),
    ///Unknown flag is specified.
    UnknownFlag(&'a str)
}

impl<'a> ParseError<'a> {
    ///Returns whether help is requested
    pub fn is_help(&self) -> bool {
        match self {
            ParseError::HelpRequested(_) => true,
            _ => false,
        }
    }
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::HelpRequested(help) => f.write_str(help),
            ParseError::TooManyArgs => f.write_str("Too many arguments are provided"),
            ParseError::RequiredArgMissing(arg) => write!(f, "Argument '{}' is required, but not provided", arg),
            ParseError::MissingValue(arg) => write!(f, "Flag '{}' is provided without value", arg),
            ParseError::InvalidFlagValue(arg, value) => write!(f, "Flag '{}' is provided with '{}' which is invalid", arg, value),
            ParseError::InvalidArgValue(arg, value) => write!(f, "Argument '{}' is provided with '{}' which is invalid", arg, value),
            ParseError::UnknownFlag(flag) => write!(f, "Unknown flag '{}' is provided", flag),
        }
    }
}

impl<'a> fmt::Display for ParseKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseKind::Top(res) => fmt::Display::fmt(res, f),
            ParseKind::Sub(name, res) => write!(f, "{name}: {res}"),
        }
    }
}


///Describers command line argument parser
pub trait Args: Sized {
    ///Help message for parser.
    const HELP: &'static str;

    ///Parses arguments from iterator of strings
    fn from_args<'a, T: IntoIterator<Item = &'a str>>(args: T) -> Result<Self, ParseKind<'a>>;

    ///Parses arguments from string, which gets tokenized and passed to from.
    fn from_text<'a>(text: &'a str) -> Result<Self, ParseKind<'a>> {
        Self::from_args(Split::from_str(text))
    }
}

#[cfg(feature = "std")]
///Parses CLI arguments from `std::env::args()`
///
///Requires feature `std`
///
///In case of help it prints help to stdout and exits with return code 0.
///In case of error it prints error to stderr and exits with return code 1.
pub fn parse_args<T: Args>() -> T {
    let code = {
        let args: std::vec::Vec<_> = std::env::args().skip(1).collect();
        match T::from_args(args.iter().map(std::string::String::as_str)) {
            Ok(args) => return args,
            Err(ParseKind::Sub(name, ParseError::HelpRequested(help))) => {
                std::println!("{name}: {}", help);
                0
            },
            Err(ParseKind::Top(ParseError::HelpRequested(help))) => {
                std::println!("{}", help);
                0
            },
            Err(error) => {
                std::eprintln!("{}", error);
                1
            }
        }
    };
    std::process::exit(code);
}
