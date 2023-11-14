//! Arg is simple command line argument parser, without any dependencies
//!
//!# Features
//!
//!- `std` - Enables utilities that require `std` library.
//!
//! # Syntax
//!
//! ## Fields
//!
//! ### Arg
//!
//! - `short` - Specifies that it is flag with short switch. Optionally can be supplied with flag.
//! - `long` - Specifies that it is flag with long switch. Optionally can be supplied with flag.
//! - `default_value` - Specifies default value to use. Can be supplied with initialization expression as string. Otherwise uses Default trait.
//! - `required` - Specifies whether argument is required. By default all arguments are optional. But booleans cannot be marked as `required`
//! - `sub` - Specifies field to be sub-command. There can be only one sub-command and it is mutually exclusive with `Vec<_>` argument to collect rest of arguments. All other options are not applied to `sub` type of field.
//!
//! ### Types
//!
//! - Flag - is `bool` switch, automatically selected when `bool` is type of argument. Each time flag is supplied it results in `!previous_state`
//! - Option - switch that accepts value. Used for any non-Vec type. Automatically overrides.
//! - Multi Option - switch with `Vec<T>` type, which allows to accumulate multiple values of switch.
//! - Argument - Plain argument that takes value.
//! - Multi argument - Collection of arguments that accumulates into `Vec<T>`, there can be only one.
//! - Sub-command - Propagates rest of arguments to another parser, there ca be only one.
//!
//! ### Conversion
//!
//! By default all types, aside from `bool` flags use `FromStr::from_str` to parse value from string.
//!
//! ### Optional
//!
//! If type is `Option<T>` then argument is assumed to be optional, in which case it cannot be
//! marked with `required` or `default_value`
//!
//! As result, not providing argument shall not fail parser.
//!
//! ### Sub-command
//!
//! It relies on enum to represent sub-commands.
//!
//! Note that when sub-command is used, it is no longer possible to collect multiple arguments into array, resulting in compilation error.
//!
//! Sub-command consumes all remaining arguments, so top command flags/options must be passed prior sub-command invocation.
//!
//! ```rust
//! use arg::Args;
//!
//! #[derive(Args, Debug)]
//! ///First
//! struct First {
//!     #[arg(short, long)]
//!     ///About this flag
//!     flag: bool,
//!
//!     #[arg(short = "v", long = "velocity", default_value = "42")]
//!     ///This is felocity. Default value is 42.
//!     speed: u32,
//! }
//!
//! #[derive(Args, Debug)]
//! ///Second
//! struct Second {
//!     #[arg(short = "v", long = "velocity", default_value = "42")]
//!     ///This is velocity. Default value is 42.
//!     speed: u32,
//!     ///To store rest of paths
//!     paths: Vec<String>,
//! }
//!
//! #[derive(Args, Debug)]
//! ///My subcommand with implicit command 'help` to list commands
//! enum MySubCommand {
//!     ///my first command
//!     First(First),
//!     ///my second command
//!     Second(Second),
//! }
//!
//! #[derive(Args, Debug)]
//! struct MyArgs {
//!     #[arg(short, long)]
//!     ///About this flag
//!     verbose: bool,
//!     #[arg(sub)]
//!     ///My sub command. Use `help` to show list of commands.
//!     cmd: MySubCommand
//! }
//! ```
//!
//! # Usage
//!
//! Here is comprehensive example to illustrate all ways to handle flags and options
//!
//! ```rust
//! use arg::Args;
//!
//! #[derive(Args, Debug)]
//! ///my_exe 0.1.0
//! ///About my program
//! ///
//! ///About my program
//! struct MyArgs {
//!     #[arg(short, long)]
//!     ///About this flag
//!     flag: bool,
//!
//!     #[arg(long = "verbose")]
//!     ///Verbose mode
//!     verbose: Option<bool>,
//!
//!     #[arg(short = "v", long = "velocity", default_value = "42")]
//!     ///This is velocity. Default value is 42.
//!     speed: u32,
//!
//!     #[arg(short = "g", long = "gps")]
//!     ///GPS coordinates.
//!     gps: Vec<u32>,
//!
//!     #[arg(short, long, default_value = "\"./address.txt\".to_owned()")]
//!     ///Extra to show how to set default string value
//!     extra: String,
//!
//!     ///To store path
//!     path: String,
//!
//!     ///To store path 2
//!     path2: String,
//!
//!     ///To store rest of paths as multi argument collector
//!     remain_paths: Vec<String>,
//! }
//!
//! fn main() {
//!     match MyArgs::from_text("-v path1 path2") {
//!         Ok(args) => println!("args={:?}", args),
//!         Err(err) => println!("err={:?}", err),
//!     }
//! }
//! ```
//!

#![no_std]
#![warn(missing_docs)]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::style))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::needless_lifetimes))]

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
    ///Argument is supplied with invalid vlaue
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
