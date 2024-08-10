//! Command line argument parser derive

#![cfg_attr(feature = "cargo-clippy", allow(clippy::style))]

extern crate proc_macro;

mod utils;
use utils::*;

use proc_macro::TokenStream;
use quote::quote;

use core::fmt::Write;

struct Argument {
    field_name: String,
    name: String,
    desc: String,
    required: bool,
    is_optional: bool,
    default: Option<String>,
}

#[derive(PartialEq, Eq, Debug)]
enum OptValueType {
    Help,
    Bool,
    Value,
    MultiValue,
}

struct Opt {
    arg: Argument,
    long: String,
    short: Option<String>,
    typ: OptValueType,
}

struct Command {
    variant_name: String,
    command_name: String,
    desc: String,
}

const FROM_FN: &str = "core::str::FromStr::from_str";
const TAB: &str = "    ";
const PARSER_TRAIT: &str = "arg::Args";
const DEFAULT_INIT: &str = "Default::default()";
const INVALID_ARG_TYPE_STRING: &str = "Attribute accepts only str";
const INVALID_REQUIRED_BOOL: &str = "Attribute required cannot be applied to bool switch";
const UNKNOWN_ARG_ATTR: &str = "Unknown attribute is used";
const ARG_INVALID_CHARS: &[char] = &[' ', '\t'];
const ARG_NAME_SPACE_ERROR: &str = "Name contains space character";

fn parse_segment(segment: &syn::PathSegment) -> OptValueType {
    if segment.ident == "bool" {
        OptValueType::Bool
    } else if segment.ident == "Vec" {
        OptValueType::MultiValue
    } else {
        OptValueType::Value
    }
}

fn from_enum(ast: &syn::DeriveInput, payload: &syn::DataEnum) -> TokenStream {
    let mut about_prog = String::new();
    for attr in ast.attrs.iter() {
        match &attr.meta {
            syn::Meta::NameValue(value) => if value.path.is_ident("doc") {
                let literal = match &value.value {
                    syn::Expr::Lit(literal) => &literal.lit,
                    _ => return syn::Error::new_spanned(value.clone(), "Attribute should be liberal").to_compile_error().into()
                };
                if let syn::Lit::Str(ref text) = literal {
                    about_prog.push_str(&text.value());
                    about_prog.push_str("\n");
                }
            },
            _ => (),
        }
    }
    about_prog.pop();

    let mut commands = Vec::new();
    for variant in payload.variants.iter() {
        let mut desc = String::new();
        let variant_name = variant.ident.to_string();
        if variant_name.is_empty() {
            return syn::Error::new_spanned(&variant.ident, "Oi, mate, You cannot have enum variant without name").to_compile_error().into()
        }
        let command_name = to_hyphenated_lower_case(&variant_name);
        if command_name.eq_ignore_ascii_case("help") {
            return syn::Error::new_spanned(&variant.ident, "Oi, mate, You cannot use variant 'Help'").to_compile_error().into()
        }

        for attr in variant.attrs.iter() {
            match &attr.meta {
                syn::Meta::NameValue(value) => if value.path.is_ident("doc") {
                    let literal = match &value.value {
                        syn::Expr::Lit(literal) => &literal.lit,
                        _ => return syn::Error::new_spanned(value.clone(), "Attribute should be liberal").to_compile_error().into()
                    };

                    if let syn::Lit::Str(ref text) = literal {
                        desc.push_str(&text.value());
                        desc.push_str(" ");
                    }
                },
                _ => continue
            }
        }

        let field = match &variant.fields {
            syn::Fields::Unit => return syn::Error::new_spanned(&variant.fields, "Unit variant cannot be used").to_compile_error().into(),
            syn::Fields::Named(_) => return syn::Error::new_spanned(&variant.fields, "I'm too lazy to support named variant").to_compile_error().into(),
            syn::Fields::Unnamed(fields) => {
                if fields.unnamed.empty_or_trailing() {
                    return syn::Error::new_spanned(&fields, "MUST specify single field").to_compile_error().into();
                } else if fields.unnamed.len() > 1 {
                    return syn::Error::new_spanned(fields, "MUST not specify more than 1 field").to_compile_error().into();
                } else {
                    fields.unnamed.first().unwrap()
                }
            },
        };

        match &field.ty {
            syn::Type::Path(ref ty) => {
                let ty = ty.path.segments.last().expect("To have at least one segment");
                if ty.ident == "Option" {
                    return syn::Error::new_spanned(ty, "Command cannot be optional").to_compile_error().into()
                } else {
                    match parse_segment(ty) {
                        OptValueType::Bool => return syn::Error::new_spanned(ty, "Command value cannot be boolean").to_compile_error().into(),
                        OptValueType::MultiValue => return syn::Error::new_spanned(ty, "Command value Vec<_>").to_compile_error().into(),
                        _ => (),
                    }
                }
            },
            ty => {
                return syn::Error::new_spanned(ty, "Expected simple ident or path").to_compile_error().into()
            }
        }

        commands.push(Command {
            command_name,
            variant_name,
            desc
        })
    }

    if commands.is_empty() {
        return syn::Error::new_spanned(ast, "Enum must have at least one variant").to_compile_error().into()
    }

    let (impl_gen, type_gen, where_clause) = ast.generics.split_for_impl();

    let help_msg = {
        use std::io::Write;
        use tabwriter::TabWriter;

        let mut tw = TabWriter::new(vec![]);

        let _ = writeln!(tw, "COMMANDS:");
        for command in commands.iter() {
            let _ = writeln!(tw, "\t{}\t{}", command.command_name, command.desc);
        }

        let _ = tw.flush();

        String::from_utf8(tw.into_inner().unwrap()).unwrap()
    };

    let mut result = String::new();
    let _ = writeln!(result, "{} {} for {}{} {{", quote!(impl#impl_gen), PARSER_TRAIT, ast.ident, quote!(#type_gen #where_clause));

    let _ = writeln!(result, "{}const HELP: &'static str = \"{}\";", TAB, help_msg);

    //from_args START
    let _ = writeln!(result, "{}fn from_args<'a, T: IntoIterator<Item = &'a str>>(_args_: T) -> Result<Self, arg::ParseKind<'a>> {{", TAB);

    let _ = writeln!(result, "{0}{0}let mut _args_ = _args_.into_iter();\n", TAB);

    //args START
    let _ = writeln!(result, "{0}{0}while let Some(_arg_) = _args_.next() {{", TAB);

    //help
    let _ = writeln!(result, "{0}{0}{0}if _arg_.eq_ignore_ascii_case(\"help\") {{", TAB);
    let _ = writeln!(result, "{0}{0}{0}{0}return Err(arg::ParseKind::Top(arg::ParseError::HelpRequested(Self::HELP)));", TAB);
    let _ = write!(result, "{0}{0}{0}}}", TAB);

    for command in commands.iter() {
        //arg START
        let _ = writeln!(result, " else if _arg_.eq_ignore_ascii_case(\"{}\") {{", command.command_name);

        let _ = writeln!(result, "{0}{0}{0}{0}match {1}::from_args(_args_) {{", TAB, PARSER_TRAIT);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Ok(res) => return Ok(Self::{1}(res)),", TAB, command.variant_name);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Err(arg::ParseKind::Top(error)) => return Err(arg::ParseKind::Sub(\"{1}\", error)),", TAB, command.command_name);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Err(arg::ParseKind::Sub(name, error)) => return Err(arg::ParseKind::Sub(name, error)),", TAB);
        let _ = writeln!(result, "{0}{0}{0}{0}}}", TAB);

        //arg END
        let _ = write!(result, "{0}{0}{0}}}", TAB);
    }

    //args END
    let _ = writeln!(result, "\n{0}{0}}}", TAB);

    let _ = writeln!(result, "{0}{0}Err(arg::ParseKind::Top(arg::ParseError::RequiredArgMissing(\"command\")))", TAB);

    //from_args END
    let _ = writeln!(result, "{}}}", TAB);

    let _ = writeln!(result, "}}");

    if let Ok(val) = std::env::var("ARG_RS_PRINT_PARSER") {
        match val.trim() {
            "0" | "false" => (),
            _ => println!("{result}"),
        }
    }
    result.parse().expect("To parse generated code")
}

fn from_struct(ast: &syn::DeriveInput, payload: &syn::DataStruct) -> TokenStream {
    let mut about_prog = String::new();
    for attr in ast.attrs.iter() {
        match &attr.meta {
            syn::Meta::NameValue(value) => if value.path.is_ident("doc") {
                let literal = match &value.value {
                    syn::Expr::Lit(literal) => &literal.lit,
                    _ => return syn::Error::new_spanned(attr, "Attribute should be liberal").to_compile_error().into()
                };
                if let syn::Lit::Str(ref text) = literal {
                    about_prog.push_str(&text.value());
                    about_prog.push('\n');
                }
            },
            _ => (),
        }
    }

    about_prog.pop();

    let mut options = Vec::new();
    let mut arguments = Vec::new();

    options.push(Opt {
        arg: Argument {
            field_name: "_".to_owned(),
            name: "help".to_owned(),
            desc: "Prints this help information".to_owned(),
            required: false,
            is_optional: false,
            default: None,
        },
        short: Some("h".to_owned()),
        long: "help".to_owned(),
        typ: OptValueType::Help,
    });

    let mut sub_command = None;
    let mut multi_argument = None;

    for field in payload.fields.iter() {
        let field_name = field.ident.as_ref().unwrap().to_string();
        let name = field.ident.as_ref().unwrap().to_string().trim_matches(|ch| !char::is_alphanumeric(ch)).to_owned();
        let mut desc = String::new();
        let mut short = None;
        let mut long = None;
        let mut required = false;
        let mut is_sub = false;

        let (is_optional, typ) = match field.ty {
            syn::Type::Path(ref ty) => {
                let ty = ty.path.segments.last().expect("To have at least one segment");

                if ty.ident == "Option" {
                    let ty = match &ty.arguments {
                        syn::PathArguments::AngleBracketed(ref args) => match args.args.len() {
                            0 => return syn::Error::new_spanned(&ty.ident, "Oi, mate, Option is without type arguments. Fix it").to_compile_error().into(),
                            1 => match args.args.first().unwrap() {
                                syn::GenericArgument::Type(syn::Type::Path(ty)) => parse_segment(ty.path.segments.last().expect("To have at least one segment")),
                                _ => return syn::Error::new_spanned(&ty.ident, "Oi, mate, Option should have type argument, but got some other shite. Fix it").to_compile_error().into(),
                            },
                            _ => return syn::Error::new_spanned(&ty.ident, "Oi, mate, Option has too many type arguments. Fix it").to_compile_error().into()
                        },
                        syn::PathArguments::None => return syn::Error::new_spanned(&ty.ident, "Oi, mate, Option is without type arguments. Fix it").to_compile_error().into(),
                        syn::PathArguments::Parenthesized(_) => return syn::Error::new_spanned(&ty.ident, "Oi, mate, you got wrong brackets for your Option . Fix it").to_compile_error().into(),
                    };

                    (true, ty)
                } else {
                    (false, parse_segment(ty))
                }
            },
            _ => (false, OptValueType::Value),
        };

        if is_optional && typ == OptValueType::MultiValue {
            return syn::Error::new_spanned(field, "Option<Vec<_>> makes no sense. Just use plain Vec<_>").to_compile_error().into();
        }

        let mut default = None;

        for attr in field.attrs.iter() {
            match &attr.meta {
                syn::Meta::NameValue(value) => if value.path.is_ident("doc") {
                    let literal = match &value.value {
                        syn::Expr::Lit(literal) => &literal.lit,
                        _ => return syn::Error::new_spanned(attr, "Attribute should be liberal").to_compile_error().into()
                    };
                    if let syn::Lit::Str(ref text) = literal {
                        desc.push_str(&text.value());
                        desc.push(' ');
                    }
                },
                syn::Meta::List(value) => if value.path.is_ident("arg") {
                    let nested = match attr.parse_args_with(syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated) {
                        Ok(nested) => nested,
                        Err(error) => {
                            let error = format!("arg attribute should be list of attributes: {error}");
                            return syn::Error::new_spanned(value, error).to_compile_error().into();
                        }
                    };

                    for value_attr in nested {
                        match value_attr {
                            syn::Meta::Path(value_attr) => if value_attr.is_ident("short") {
                                short = Some(format!("{}", name.chars().next().unwrap()).to_lowercase());
                            } else if value_attr.is_ident("long") {
                                long = Some(name.to_lowercase());
                            } else if value_attr.is_ident("default_value") {
                                default = Some(DEFAULT_INIT.to_owned());
                            } else if value_attr.is_ident("required") {
                                if typ != OptValueType::Bool {
                                    required = true
                                } else {
                                    return syn::Error::new_spanned(value_attr, INVALID_REQUIRED_BOOL).to_compile_error().into();
                                }
                            } else if value_attr.is_ident("sub") {
                                if typ == OptValueType::Value {
                                    is_sub = true;
                                } else {
                                    return syn::Error::new_spanned(value_attr, "Sub-command must be simple value").to_compile_error().into();
                                }
                            }
                            syn::Meta::NameValue(value_attr) => if value_attr.path.is_ident("short") {
                                let literal = match &value_attr.value {
                                    syn::Expr::Lit(literal) => &literal.lit,
                                    _ => return syn::Error::new_spanned(attr, "Attribute should be liberal").to_compile_error().into()
                                };

                                if let syn::Lit::Str(ref text) = literal {
                                    let value_attr_text = text.value();

                                    if value_attr_text.contains(ARG_INVALID_CHARS) {
                                        return syn::Error::new_spanned(literal.clone(), ARG_NAME_SPACE_ERROR).to_compile_error().into();
                                    }

                                    short = Some(value_attr_text);
                                } else {
                                    return syn::Error::new_spanned(value_attr.path.clone(), INVALID_ARG_TYPE_STRING).to_compile_error().into();
                                }
                            } else if value_attr.path.is_ident("long") {
                                let literal = match &value_attr.value {
                                    syn::Expr::Lit(literal) => &literal.lit,
                                    _ => return syn::Error::new_spanned(attr, "Attribute should be liberal").to_compile_error().into()
                                };

                                if let syn::Lit::Str(ref text) = literal {
                                    let value_attr_text = text.value();

                                    if value_attr_text.contains(ARG_INVALID_CHARS) {
                                        return syn::Error::new_spanned(literal.clone(), ARG_NAME_SPACE_ERROR).to_compile_error().into();
                                    }

                                    long = Some(value_attr_text)
                                } else {
                                    return syn::Error::new_spanned(value_attr.path.clone(), INVALID_ARG_TYPE_STRING).to_compile_error().into();
                                }
                            } else if value_attr.path.is_ident("default_value") {
                                let literal = match &value_attr.value {
                                    syn::Expr::Lit(literal) => &literal.lit,
                                    _ => return syn::Error::new_spanned(attr, "Attribute should be liberal").to_compile_error().into()
                                };

                                if let syn::Lit::Str(ref text) = literal {
                                    default = Some(text.value());
                                } else {
                                    return syn::Error::new_spanned(value_attr.path.clone(), INVALID_ARG_TYPE_STRING).to_compile_error().into();
                                }
                            } else {
                                return syn::Error::new_spanned(value_attr.path.clone(), UNKNOWN_ARG_ATTR).to_compile_error().into();
                            }
                            _ => {
                            },
                        }
                    }
                },
                _ => (),
            }
        }

        desc.pop();

        if required && default.is_some() {
            return syn::Error::new_spanned(field.ident.clone(), "Marked as required, but default value is provided?").to_compile_error().into();
        } else if is_optional && default.is_some() {
            return syn::Error::new_spanned(field.ident.clone(), "Optional, but default value is provided?").to_compile_error().into();
        } else if is_sub && is_optional {
            return syn::Error::new_spanned(field.ident.clone(), "Sub-command cannot be optional").to_compile_error().into();
        } else if is_sub && default.is_some() {
            return syn::Error::new_spanned(field.ident.clone(), "Sub-command cannot have default value").to_compile_error().into();
        } else if !required && !is_optional && default.is_none() {
            default = Some(DEFAULT_INIT.to_owned());
        }

        if short.is_none() && long.is_none() {
            if typ == OptValueType::MultiValue {
                if multi_argument.is_some() {
                    return syn::Error::new_spanned(field.ident.clone(), "Second argument collection. There can be only one").to_compile_error().into();
                } else if sub_command.is_some() {
                    return syn::Error::new_spanned(field.ident.clone(), "Multi-argument collection and sub-command are mutually exclusive").to_compile_error().into();
                }

                multi_argument = Some(Argument {
                    field_name,
                    name,
                    desc,
                    required,
                    is_optional,
                    default,
                });

            } else if is_sub {
                if sub_command.is_some() {
                    return syn::Error::new_spanned(field.ident.clone(), "Second sub-command. There can be only one").to_compile_error().into();
                } else if multi_argument.is_some() {
                    return syn::Error::new_spanned(field.ident.clone(), "Sub-command and multi-argument collection are mutually exclusive").to_compile_error().into();
                }

                sub_command = Some(Argument {
                    field_name,
                    name,
                    desc,
                    required: true,
                    is_optional: false,
                    default: None,
                });
            } else {
                arguments.push(Argument {
                    field_name,
                    name,
                    desc,
                    required,
                    is_optional,
                    default,
                })
            }

        } else {
            let long = match long {
                Some(long) => long,
                None => name.clone()
            };

            options.push(Opt {
                arg: Argument {
                    field_name,
                    name,
                    desc,
                    required,
                    is_optional,
                    default,
                },
                short,
                long,
                typ
            })
        }
    }

    let (impl_gen, type_gen, where_clause) = ast.generics.split_for_impl();

    let help_msg = {
        use std::io::Write;
        use tabwriter::TabWriter;

        let mut tw = TabWriter::new(vec![]);

        let _ = write!(tw, "{}

USAGE:", about_prog);

        if !options.is_empty() {
            let _ = write!(tw, " [OPTIONS]");
        }

        for argument in arguments.iter() {
            let _ = if argument.required {
                write!(tw, " <{}>", argument.name)
            } else {
                write!(tw, " [{}]", argument.name)
            };
        }

        if let Some(argument) = multi_argument.as_ref() {
            let _ = if argument.required {
                write!(tw, " <{}>...", argument.name)
            } else {
                write!(tw, " [{}]...", argument.name)
            };
        } else if let Some(argument) = sub_command.as_ref() {
            let _ = write!(tw, " <{}>", argument.name);
        }

        if !options.is_empty() {
            let _ = write!(tw, "\n\nOPTIONS:\n");
        }

        for option in options.iter() {
            let _ = write!(tw, "\t");
            if let Some(short) = option.short.as_ref() {
                let _ = write!(tw, "-{},", short);
            }
            let _ = write!(tw, "\t");

            let _ = write!(tw, "--{}", option.long);

            let _ = match option.typ {
                OptValueType::MultiValue => write!(tw, " <{}>...", option.arg.name),
                OptValueType::Value => write!(tw, " <{}>", option.arg.name),
                _ => Ok(()),
            };

            let _ = writeln!(tw, "\t{}", option.arg.desc);
        }

        if !arguments.is_empty() || multi_argument.is_some() || sub_command.is_some() {
            let _ = write!(tw, "\nARGS:\n");
        }

        for argument in arguments.iter() {
            let _ = if argument.required {
                writeln!(tw, "\t<{}>\t{}", argument.name, argument.desc)
            } else {
                writeln!(tw, "\t[{}]\t{}", argument.name, argument.desc)
            };
        }

        if let Some(argument) = multi_argument.as_ref() {
            let _ = writeln!(tw, "\t<{}>...\t{}", argument.name, argument.desc);
        } else if let Some(command) = sub_command.as_ref() {
            let _ = writeln!(tw, "\t<{}>\t{}", command.name, command.desc);
        }

        let _ = tw.flush();

        String::from_utf8(tw.into_inner().unwrap()).unwrap()
    };

    let mut result = String::new();
    let _ = writeln!(result, "{} {} for {}{} {{", quote!(impl#impl_gen), PARSER_TRAIT, ast.ident, quote!(#type_gen #where_clause));
    let _ = writeln!(result, "{}const HELP: &'static str = \"{}\";", TAB, help_msg);

    let _ = writeln!(result, "{}fn from_args<'a, T: IntoIterator<Item = &'a str>>(_args_: T) -> Result<Self, arg::ParseKind<'a>> {{", TAB);

    for option in options.iter() {
        if option.arg.field_name == "_" {
            continue;
        }

        let _ = match option.typ {
            OptValueType::MultiValue => writeln!(result, "{0}{0}let mut {1} = Vec::new();", TAB, option.arg.field_name),
            OptValueType::Bool => writeln!(result, "{0}{0}let mut {1} = false;", TAB, option.arg.field_name),
            _ => writeln!(result, "{0}{0}let mut {1} = None;", TAB, option.arg.field_name),
        };
    }

    for argument in arguments.iter() {
        let _ = writeln!(result, "{0}{0}let mut {1} = None;", TAB, argument.field_name);
    }

    if let Some(argument) = multi_argument.as_ref() {
        let _ = writeln!(result, "{0}{0}let mut {1} = Vec::new();", TAB, argument.field_name);
    } else if let Some(command) = sub_command.as_ref() {
        let _ = writeln!(result, "{0}{0}let mut {1} = None;", TAB, command.field_name);
    }

    let _ = writeln!(result, "{0}{0}let mut _args_ = _args_.into_iter();\n", TAB);
    let _ = writeln!(result, "{0}{0}while let Some(_arg_) = _args_.next() {{", TAB);

    //options
    let _ = writeln!(result, "{0}{0}{0}if let Some(_arg_) = _arg_.strip_prefix('-') {{", TAB);
    let _ = writeln!(result, "{0}{0}{0}{0}match _arg_ {{", TAB);
    let _ = writeln!(result, "{0}{0}{0}{0}{0}\"h\" | \"-help\" => return Err(arg::ParseKind::Top(arg::ParseError::HelpRequested(Self::HELP))),", TAB);
    let _ = writeln!(result, "{0}{0}{0}{0}{0}\"\" => (),", TAB);

    for option in options.iter() {
        if option.arg.field_name == "_" {
            continue;
        }

        let _ = write!(result, "{0}{0}{0}{0}{0}", TAB);

        if let Some(short) = option.short.as_ref() {
            let _ = write!(result, "\"{}\" | ", short);
        }

        let _ = write!(result, "\"-{}\" => ", option.long);

        let _ = match option.typ {
            OptValueType::Help => panic!("Option Help is invalid here. Bug report it"),
            OptValueType::Bool => write!(result, "{{ {0} = !{0}; continue }},", option.arg.field_name),
            OptValueType::Value => write!(result, "match _args_.next() {{
{0}{0}{0}{0}{0}{0}Some(_next_arg_) => match {1}(_next_arg_) {{
{0}{0}{0}{0}{0}{0}{0}Ok(value) => {{ {2} = Some(value); continue }},
{0}{0}{0}{0}{0}{0}{0}Err(_) => return Err(arg::ParseKind::Top(arg::ParseError::InvalidFlagValue(\"{3}\", _next_arg_))),
{0}{0}{0}{0}{0}{0}}},
{0}{0}{0}{0}{0}{0}None => return Err(arg::ParseKind::Top(arg::ParseError::MissingValue(\"{3}\"))),
{0}{0}{0}{0}{0}}}", TAB, FROM_FN, option.arg.field_name, option.arg.name),
            OptValueType::MultiValue => write!(result, "match _args_.next() {{
{0}{0}{0}{0}{0}{0}Some(_next_arg_) => match {1}(_next_arg_) {{
{0}{0}{0}{0}{0}{0}{0}Ok(value) => {{ {2}.push(value); continue }},
{0}{0}{0}{0}{0}{0}{0}Err(_) => return Err(arg::ParseKind::Top(arg::ParseError::InvalidFlagValue(\"{3}\", _next_arg_))),
{0}{0}{0}{0}{0}{0}}},
{0}{0}{0}{0}{0}{0}None => return Err(arg::ParseKind::Top(arg::ParseError::MissingValue(\"{3}\"))),
{0}{0}{0}{0}{0}}}", TAB, FROM_FN, option.arg.field_name, option.arg.name),
        };
        result.push('\n');
    }
    let _ = writeln!(result, "{0}{0}{0}{0}{0}_ => return Err(arg::ParseKind::Top(arg::ParseError::UnknownFlag(_arg_))),", TAB);

    let _ = writeln!(result, "{0}{0}{0}{0}}}", TAB);
    let _ = writeln!(result, "{0}{0}{0}}}", TAB);
    //rest args
    for (idx, arg) in arguments.iter().enumerate() {
        if idx == 0 {
            let _ = writeln!(result, "{0}{0}{0}if {1}.is_none() {{", TAB, arg.field_name);
        } else {
            let _ = writeln!(result, "{0}{0}{0}}} else if {1}.is_none() {{", TAB, arg.field_name);
        }
        let _ = writeln!(result, "{0}{0}{0}{0}match {1}(_arg_) {{", TAB, FROM_FN);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Ok(_res_) => {1} = Some(_res_),", TAB, arg.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Err(_) => return Err(arg::ParseKind::Top(arg::ParseError::InvalidArgValue(\"{1}\", _arg_))),", TAB, arg.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}}}", TAB);
    }
    //too many args?
    if !arguments.is_empty() {
        let _ = writeln!(result, "{0}{0}{0}}} else {{", TAB);
    }

    if let Some(arg) = multi_argument.as_ref() {
        let _ = writeln!(result, "{0}{0}{0}{0}match {1}(_arg_) {{", TAB, FROM_FN);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Ok(_res_) => {1}.push(_res_),", TAB, arg.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Err(_) => return Err(arg::ParseKind::Top(arg::ParseError::InvalidArgValue(\"{1}\", _arg_))),", TAB, arg.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}}}", TAB);
    } else if let Some(command) = sub_command.as_ref() {
        let _ = writeln!(result, "{0}{0}{0}{0}match {1}::from_args(core::iter::once(_arg_).chain(_args_)) {{", TAB, PARSER_TRAIT);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Ok(_res_) => {{ {1} = Some(_res_); break; }},", TAB, command.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Err(arg::ParseKind::Top(_)) => return Err(arg::ParseKind::Top(arg::ParseError::RequiredArgMissing(\"{1}\"))),", TAB, command.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Err(arg::ParseKind::Sub(name, error)) => return Err(arg::ParseKind::Sub(name, error)),", TAB);
        let _ = writeln!(result, "{0}{0}{0}{0}}}", TAB);
    } else {
        let _ = writeln!(result, "{0}{0}{0}{0} return Err(arg::ParseKind::Top(arg::ParseError::TooManyArgs));", TAB);
    }
    //exit args
    if !arguments.is_empty() {
        let _ = writeln!(result, "{0}{0}{0}}}", TAB);
        let _ = writeln!(result, "{0}{0}}}", TAB);
    } else {
        let _ = writeln!(result, "{0}{0}}}", TAB);
    }

    //Set defaults
    for option in options.iter() {
        if option.arg.field_name == "_" {
            continue;
        }

        let _ = match option.typ {
            OptValueType::MultiValue => Ok(()),
            OptValueType::Bool => Ok(()),
            _ => match option.arg.default {
                Some(ref default) => writeln!(result, "{0}{0}let {1} = if let Some(value) = {1} {{ value }} else {{ {2} }};", TAB, option.arg.field_name, default),
                None => match option.arg.is_optional {
                    true => Ok(()),
                    false => writeln!(result, "{0}{0}let {1} = if let Some(value) = {1} {{ value }} else {{ return Err(arg::ParseKind::Top(arg::ParseError::RequiredArgMissing(\"{2}\"))) }};", TAB, option.arg.field_name, option.arg.name),
                },
            },
        };
    }

    for arg in arguments.iter() {
        let _ = match arg.default {
            Some(ref default) => writeln!(result, "{0}{0}let {1} = if let Some(value) = {1} {{ value }} else {{ {2} }};", TAB, arg.field_name, default),
            None => match arg.is_optional {
                true => Ok(()),
                false => writeln!(result, "{0}{0}let {1} = if let Some(value) = {1} {{ value }} else {{ return Err(arg::ParseKind::Top(arg::ParseError::RequiredArgMissing(\"{2}\"))) }};", TAB, arg.field_name, arg.name),
            }
        };
    }

    if let Some(command) = sub_command.as_ref() {
        let _ = writeln!(result, "{0}{0}let {1} = if let Some(value) = {1} {{ value }} else {{ return Err(arg::ParseKind::Top(arg::ParseError::RequiredArgMissing(\"{2}\"))) }};", TAB, command.field_name, command.name);
    }

    //Fill result
    let _ = writeln!(result, "{0}{0}Ok(Self {{", TAB);

    for option in options.iter() {
        if option.arg.field_name == "_" {
            continue;
        }

        let _ = if option.arg.is_optional && option.typ == OptValueType::Bool {
            writeln!(result, "{0}{0}{0}{1}: Some({1}),", TAB, option.arg.field_name)
        } else {
            writeln!(result, "{0}{0}{0}{1},", TAB, option.arg.field_name)
        };
    }

    for arg in arguments.iter() {
        let _ = writeln!(result, "{0}{0}{0}{1},", TAB, arg.field_name);
    }

    if let Some(arg) = multi_argument.as_ref() {
        let _ = writeln!(result, "{0}{0}{0}{1},", TAB, arg.field_name);
    } else if let Some(arg) = sub_command.as_ref() {
        let _ = writeln!(result, "{0}{0}{0}{1},", TAB, arg.field_name);
    }

    let _ = writeln!(result, "{0}{0}}})", TAB);

    //Exit fn
    let _ = writeln!(result, "{}}}", TAB);

    let _ = writeln!(result, "}}");

    if let Ok(val) = std::env::var("ARG_RS_PRINT_PARSER") {
        match val.trim() {
            "0" | "false" => (),
            _ => println!("{result}"),
        }
    }
    result.parse().expect("To parse generated code")
}

#[proc_macro_derive(Args, attributes(parser, arg))]
pub fn parser_derive(input: TokenStream) -> TokenStream {
    const INVALID_INPUT_TYPE: &str = "Unsupported parser input type. Expect: struct";
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    match ast.data {
        syn::Data::Struct(ref data) => from_struct(&ast, data),
        syn::Data::Enum(ref data) => from_enum(&ast, data),
        _ => syn::Error::new_spanned(ast.ident, INVALID_INPUT_TYPE).to_compile_error().into(),
    }
}
