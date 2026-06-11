# arg

[![Actions Status](https://github.com/DoumanAsh/arg.rs/workflows/Rust/badge.svg)](https://github.com/DoumanAsh/arg.rs/actions)
[![Crates.io](https://img.shields.io/crates/v/arg.svg)](https://crates.io/crates/arg)
[![Documentation](https://docs.rs/arg/badge.svg)](https://docs.rs/crate/arg/)

Arg is simple command line argument parser, with dependency on code generation only

# Features

 `std` - Enables utilities that require `std` library.

# Syntax

## Struct

### Arg

- `env_prefix` - Specifies prefix to be used for environment based initialization. Defaults to `ARG`
- `infer_name` - Specifies to insert binary name/version as combination of `env!("CARGO_PKG_NAME")` and  `env!("CARGO_PKG_VERSION")`. Applicable only to `struct`

## Fields

### Arg

- `short` - Specifies that it is flag with short switch. Optionally can be supplied with flag.
- `long` - Specifies that it is flag with long switch. Optionally can be supplied with flag.
- `default_value` - Specifies default value to use. Can be supplied with initialization expression as string. Otherwise uses Default trait.
- `required` - Specifies whether argument is required. By default all arguments are optional. But booleans cannot be marked as `required`
- `sub` - Specifies field to be sub-command. There can be only one sub-command and it is mutually exclusive with `Vec<_>` argument to collect rest of arguments. All other options are not applied to `sub` type of field.
- `env_value` - Specifies to fallback to environment variable if argument is not provided. Takes precedence over default value. Can be supplied with different name, otherwise defaults to the field name in upper case

### Types

- Flag - is `bool` switch, automatically selected when `bool` is type of argument. Each time flag is supplied it results in `!previous_state`
- Option - switch that accepts value. Used for any non-Vec type. Automatically overrides.
- Multi Option - switch with `Vec<T>` type, which allows to accumulate multiple values of switch.
- Argument - Plain argument that takes value.
- Multi argument - Collection of arguments that accumulates into `Vec<T>`, there can be only one.
- Sub-command - Propagates rest of arguments to another parser, there can be only one.

### Conversion

By default all types, aside from `bool` flags use `FromStr::from_str` to parse value from string.

### Optional

If type is `Option<T>` then argument is assumed to be optional, in which case it cannot be
marked with `required` or `default_value`

As result, not providing argument shall not fail parser.

### Sub-command

It relies on enum to represent sub-commands.

Note that when sub-command is used, it is no longer possible to collect multiple arguments into array, resulting in compilation error.

Sub-command consumes all remaining arguments, so top command flags/options must be passed prior sub-command invocation.

### Environment variable initialization

Option `env_value` can specified on argument to enable initialization from environment variable in case command line argument is not specified.

When specified without value, `env_value` enables initialization using uppercase name of the field.

If needed, override of the name is possible by declaring name `env_value = <YOUR NAME>`

#### Notes on initialization

Environment variable initialization is not possible on subcommands or multi argument collector (by specifying `Vec<T>` as last argument)

When `env_value` specified on required argument, it will be used as fallback instead of failing parsing.

In case of boolean, it always acts as initial value of the switch instead of the default `false`

# Usage

## Simple
Here is comprehensive example to illustrate all ways to handle flags and options

```rust
use arg::Args;

#[derive(Args, Debug)]
///my_exe 0.1.0
///About my program
///
///About my program
struct MyArgs {
    #[arg(short, long)]
    ///About this flag
    flag: bool,

    #[arg(long = "verbose", env_value)]
    ///Verbose mode
    verbose: Option<bool>,

    #[arg(short = "v", long = "velocity", default_value = "42", env_value)]
    ///This is velocity. Default value is 42.
    speed: u32,

    #[arg(short = "g", long = "gps", env_value)]
    ///GPS coordinates.
    gps: Vec<u32>,

    #[arg(short, long, default_value = "\"./address.txt\".to_owned()", env_value)]
    ///Extra to show how to set default string value
    extra: String,

    #[arg(required, env_value)]
    ///To store path
    path: String,

    #[arg(env_value)]
    ///To store path 2
    path2: String,

    ///To store rest of paths as multi argument collector
    remain_paths: Vec<String>,
}

fn main() {
    match MyArgs::from_text("-v path1 path2") {
        Ok(args) => println!("args={:?}", args),
        Err(err) => println!("err={:?}", err),
    }
}
```

## Sub-command

Illustration of sub-command introduction via enum

```rust
use arg::Args;

#[derive(Args, Debug)]
///First
struct First {
    #[arg(short, long, env_value)]
    ///About this flag
    flag: bool,

    #[arg(short = "v", long = "velocity", default_value = "42")]
    ///This is felocity. Default value is 42.
    speed: u32,
}

#[derive(Args, Debug)]
///Second
struct Second {
    #[arg(short = "v", long = "velocity", default_value = "42")]
    ///This is velocity. Default value is 42.
    speed: u32,
    ///To store rest of paths
    paths: Vec<String>,
}

#[derive(Args, Debug)]
///My subcommand with implicit command 'help` to list commands
enum MySubCommand {
    ///my first command
    First(First),
    ///my second command
    Second(Second),
}

#[derive(Args, Debug)]
struct MyArgs {
    #[arg(short, long)]
    ///About this flag
    verbose: bool,
    #[arg(sub)]
    ///My sub command. Use `help` to show list of commands.
    cmd: MySubCommand
}
```
