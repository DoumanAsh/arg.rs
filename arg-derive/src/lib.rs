//! Command line argument parser derive

extern crate proc_macro;

use proc_macro::TokenStream;

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

fn from_struct(ast: &syn::DeriveInput, payload: &syn::DataStruct) -> TokenStream {
    let mut about_prog = String::new();
    for attr in ast.attrs.iter().filter_map(|attr| attr.parse_meta().ok()) {
        match attr {
            syn::Meta::NameValue(value) => if value.path.is_ident("doc") {
                if let syn::Lit::Str(ref text) = value.lit {
                    about_prog.push_str(&text.value());
                    about_prog.push_str("\n");
                }
            } else {
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

    let mut multi_argument = None;

    for field in payload.fields.iter() {
        let field_name = field.ident.as_ref().unwrap().to_string();
        let name = field.ident.as_ref().unwrap().to_string().trim_matches(|ch| !char::is_alphanumeric(ch)).to_owned();
        let mut desc = String::new();
        let mut short = None;
        let mut long = None;
        let mut required = false;

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

        for attr in field.attrs.iter().filter_map(|attr| attr.parse_meta().ok()) {
            match attr {
                syn::Meta::NameValue(value) => if value.path.is_ident("doc") {
                    if let syn::Lit::Str(ref text) = value.lit {
                        desc.push_str(&text.value());
                        desc.push_str(" ");
                    }
                },
                syn::Meta::List(value) => if value.path.is_ident("arg") {
                    for value_attr in value.nested.iter() {
                        match value_attr {
                            syn::NestedMeta::Meta(value_attr) => {
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
                                    },
                                    syn::Meta::NameValue(value_attr) => if value_attr.path.is_ident("short") {
                                        if let syn::Lit::Str(ref text) = value_attr.lit {
                                            let value_attr_text = text.value();

                                            if value_attr_text.contains(ARG_INVALID_CHARS) {
                                                return syn::Error::new_spanned(value_attr.lit.clone(), ARG_NAME_SPACE_ERROR).to_compile_error().into();
                                            }

                                            short = Some(value_attr_text);
                                        } else {
                                            return syn::Error::new_spanned(value_attr.path.clone(), INVALID_ARG_TYPE_STRING).to_compile_error().into();
                                        }
                                    } else if value_attr.path.is_ident("long") {
                                        if let syn::Lit::Str(ref text) = value_attr.lit {
                                            let value_attr_text = text.value();

                                            if value_attr_text.contains(ARG_INVALID_CHARS) {
                                                return syn::Error::new_spanned(value_attr.lit.clone(), ARG_NAME_SPACE_ERROR).to_compile_error().into();
                                            }

                                            long = Some(value_attr_text)
                                        } else {
                                            return syn::Error::new_spanned(value_attr.path.clone(), INVALID_ARG_TYPE_STRING).to_compile_error().into();
                                        }
                                    } else if value_attr.path.is_ident("default_value") {
                                        if let syn::Lit::Str(ref text) = value_attr.lit {
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
                            },
                            syn::NestedMeta::Lit(_) => (),
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
        } else if !required && !is_optional && default.is_none() {
            default = Some(DEFAULT_INIT.to_owned());
        }

        if short.is_none() && long.is_none() {
            if typ == OptValueType::MultiValue {
                if multi_argument.is_some() {
                    return syn::Error::new_spanned(field.ident.clone(), "Second argument collection. There can be only one").to_compile_error().into();
                }

                multi_argument = Some(Argument {
                    field_name,
                    name,
                    desc,
                    required,
                    is_optional,
                    default,
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

USAGE: [OPTIONS]", about_prog);

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
        }

        let _ = write!(tw, "\n\nOPTIONS:\n");

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

            let _ = write!(tw, "\t{}\n", option.arg.desc);
        }

        let _ = write!(tw, "\nARGS:\n");

        for argument in arguments.iter() {
            let _ = if argument.required {
                writeln!(tw, "\t<{}>\t{}", argument.name, argument.desc)
            } else {
                writeln!(tw, "\t[{}]\t{}", argument.name, argument.desc)
            };
        }

        if let Some(argument) = multi_argument.as_ref() {
            let _ = writeln!(tw, "\t<{}>...\t{}", argument.name, argument.desc);
        }

        let _ = tw.flush();

        String::from_utf8(tw.into_inner().unwrap()).unwrap()
    };

    use quote::quote;

    let mut result = String::new();
    let _ = writeln!(result, "{} {} for {}{} {{", quote!(impl#impl_gen), PARSER_TRAIT, ast.ident, quote!(#type_gen #where_clause));
    let _ = writeln!(result, "{}const HELP: &'static str = \"{}\";", TAB, help_msg);

    let _ = writeln!(result, "{}fn from_args<'a, T: IntoIterator<Item = &'a str>>(_args_: T) -> Result<Self, arg::ParseError<'a>> {{", TAB);

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
    }

    let _ = writeln!(result, "{0}{0}let mut _args_ = _args_.into_iter();\n", TAB);
    let _ = writeln!(result, "{0}{0}while let Some(_arg_) = _args_.next() {{", TAB);

    //options
    let _ = writeln!(result, "{0}{0}{0}if _arg_.starts_with('-') {{", TAB);
    let _ = writeln!(result, "{0}{0}{0}{0}match &_arg_[1..] {{", TAB);
    let _ = writeln!(result, "{0}{0}{0}{0}{0}\"h\" | \"-help\" => return Err(arg::ParseError::HelpRequested(Self::HELP)),", TAB);

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
            OptValueType::Bool => write!(result, "{0} = !{0},", option.arg.field_name),
            OptValueType::Value => write!(result, "match _args_.next() {{
{0}{0}{0}{0}{0}{0}Some(_next_arg_) => match {1}(_next_arg_) {{
{0}{0}{0}{0}{0}{0}{0}Ok(value) => {2} = Some(value),
{0}{0}{0}{0}{0}{0}{0}Err(_) => return Err(arg::ParseError::InvalidFlagValue(\"{3}\", _next_arg_)),
{0}{0}{0}{0}{0}{0}}},
{0}{0}{0}{0}{0}{0}None => return Err(arg::ParseError::MissingValue(\"{3}\")),
{0}{0}{0}{0}{0}}}", TAB, FROM_FN, option.arg.field_name, option.arg.name),
            OptValueType::MultiValue => write!(result, "match _args_.next() {{
{0}{0}{0}{0}{0}{0}Some(_next_arg_) => match {1}(_next_arg_) {{
{0}{0}{0}{0}{0}{0}{0}Ok(value) => {2}.push(value),
{0}{0}{0}{0}{0}{0}{0}Err(_) => return Err(arg::ParseError::InvalidFlagValue(\"{3}\", _next_arg_)),
{0}{0}{0}{0}{0}{0}}},
{0}{0}{0}{0}{0}{0}None => return Err(arg::ParseError::MissingValue(\"{3}\")),
{0}{0}{0}{0}{0}}}", TAB, FROM_FN, option.arg.field_name, option.arg.name),
        };
        let _ = writeln!(result, "");
    }
    let _ = writeln!(result, "{0}{0}{0}{0}{0}_ => return Err(arg::ParseError::UnknownFlag(_arg_)),", TAB);

    let _ = writeln!(result, "{0}{0}{0}{0}}}", TAB);
    //rest args
    for arg in arguments.iter() {
        let _ = writeln!(result, "{0}{0}{0}}} else if {1}.is_none() {{", TAB, arg.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}match {1}(_arg_) {{", TAB, FROM_FN);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Ok(_res_) => {1} = Some(_res_),", TAB, arg.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Err(_) => return Err(arg::ParseError::InvalidArgValue(\"{1}\", _arg_)),", TAB, arg.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}}}", TAB);
    }
    //too many args?
    let _ = writeln!(result, "{0}{0}{0}}} else {{", TAB);

    if let Some(arg) = multi_argument.as_ref() {
        let _ = writeln!(result, "{0}{0}{0}{0}match {1}(_arg_) {{", TAB, FROM_FN);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Ok(_res_) => {1}.push(_res_),", TAB, arg.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}{0}Err(_) => return Err(arg::ParseError::InvalidArgValue(\"{1}\", _arg_)),", TAB, arg.field_name);
        let _ = writeln!(result, "{0}{0}{0}{0}}}", TAB);
    } else {
        let _ = writeln!(result, "{0}{0}{0}{0} return Err(arg::ParseError::TooManyArgs);", TAB);
    }
    let _ = writeln!(result, "{0}{0}{0}}}", TAB);
    //exit args

    let _ = writeln!(result, "{0}{0}}}", TAB);

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
                    false => writeln!(result, "{0}{0}let {1} = if let Some(value) = {1} {{ value }} else {{ return Err(arg::ParseError::RequiredArgMissing(\"{2}\")) }};", TAB, option.arg.field_name, option.arg.name),
                },
            },
        };
    }

    for arg in arguments.iter() {
        let _ = match arg.default {
            Some(ref default) => writeln!(result, "{0}{0}let {1} = if let Some(value) = {1} {{ value }} else {{ {2} }};", TAB, arg.field_name, default),
            None => match arg.is_optional {
                true => Ok(()),
                false => writeln!(result, "{0}{0}let {1} = if let Some(value) = {1} {{ value }} else {{ return Err(arg::ParseError::RequiredArgMissing(\"{2}\")) }};", TAB, arg.field_name, arg.name),
            }
        };
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
    }

    let _ = writeln!(result, "{0}{0}}})", TAB);

    //Exit fn
    let _ = writeln!(result, "{}}}", TAB);

    let _ = writeln!(result, "}}");

    result.parse().expect("To parse generated code")
}

#[proc_macro_derive(Args, attributes(parser, arg))]
pub fn parser_derive(input: TokenStream) -> TokenStream {
    const INVALID_INPUT_TYPE: &str = "Unsupported parser input type. Expect: struct";
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    match ast.data {
        syn::Data::Struct(ref data) => from_struct(&ast, data),
        _ => syn::Error::new_spanned(ast.ident, INVALID_INPUT_TYPE).to_compile_error().into(),
    }
}
