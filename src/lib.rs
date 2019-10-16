//! Arg is simple command line argument parser, without any dependencies
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
//!
//! ### Types
//!
//! - Flag - is `bool` switch, automatically selected when `bool` is type of argument. Each time flag is supplied it results in `!previous_state`
//! - Option - switch that accepts value. Used for any non-Vec type. Automatically overrides.
//! - Multi Option - switch with `Vec<T>` type, which allows to accumulate multiple values of switch.
//! - Argument - Plain argument that takes value.
//! - Multi argument - Collection of arguments that accumulates into `Vec<T>`, there can be only one.
//!
//! ### Conversion
//!
//! By default all types, aside from `bool` flags use `FromStr::from_str` to parse value from string.
//!
//! # Usage
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
//!     verbose: bool,
//!
//!     #[arg(short = "v", long = "velocity", default_value = "42")]
//!     ///This is felocity. Default value is 42.
//!     speed: u32,
//!
//!     #[arg(short = "g", long = "gps")]
//!     ///GPS coordinates.
//!     gps: Vec<u32>,
//!
//!     ///To store path
//!     path: String,
//!
//!     ///To store path 2
//!     path2: String,
//!
//!     ///To store rest of paths
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

pub use arg_derive::*;

mod split;
pub use split::Split;

#[derive(PartialEq, Eq, Debug)]
///Parse errors
pub enum ParseError<'a> {
    ///User requested help.
    HelpRequested,
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

///Describers command line argument parser
pub trait Args: Sized {
    ///Help message for parser.
    const HELP: &'static str;

    ///Parses arguments from iterator of strings
    fn from_args<'a, T: IntoIterator<Item = &'a str>>(args: T) -> Result<Self, ParseError<'a>>;

    ///Parses arguments from string, which gets tokenized and passed to from.
    fn from_text<'a>(text: &'a str) -> Result<Self, ParseError<'a>> {
        Self::from_args(Split::from_str(text))
    }
}
