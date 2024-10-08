use arg::Args;

#[allow(unused)]
#[derive(Args, Debug)]
struct Test4 {
}

#[allow(unused)]
#[derive(Args, Debug)]
struct Test3 {
  paths: Vec<String>,
}

#[allow(unused)]
#[derive(Args, Debug)]
struct Test2 {
  #[arg(short = "u")]
  u: bool,

  paths: Vec<String>,
}

#[allow(unused)]
#[derive(Args, Debug)]
struct BoolOptionArgs {
    #[arg(required)]
    option: bool,
}

#[derive(Debug, Args)]
///my_exe 0.1.0
///About my program
///
///About my program
struct MyArgs {
    #[arg(short, long, required)]
    ///Required argument
    required: u32,

    #[arg(short, long)]
    ///Optional argument
    optional: Option<u32>,

    #[arg(short, long)]
    ///About this flag
    flag: bool,

    #[arg(long = "verbose")]
    ///Verbose mode
    verbose: bool,

    #[arg(short = "v", long = "velocity", default_value = "42")]
    ///This is felocity. Default value is 42.
    speed: u32,

    #[arg(short = "g", long = "gps")]
    ///GPS coordinates.
    gps: Vec<u32>,

    #[arg(required)]
    ///To store path
    path: String,

    #[arg(required)]
    ///To store path 2
    path2: String,

    ///To store rest of paths
    remain_paths: Vec<String>,
}

#[derive(Debug, Args)]
///My subcommand
enum BigSubArgs {
    ///my arguments
    MyArgs(MyArgs),
    ///test args
    Test(Test2),
}

#[derive(Debug, Args)]
struct BigArgs {
    #[arg(long = "verbose")]
    ///Verbose mode
    verbose: bool,
    #[arg(sub)]
    cmd: BigSubArgs,
}

#[test]
fn should_error_on_missing_args() {
    let result = MyArgs::from_text("-f --verbose path1").unwrap_err();
    assert_eq!(result, arg::ParseError::RequiredArgMissing("required"));

    let result = MyArgs::from_text("-f -r 5 --verbose path1").unwrap_err();
    assert_eq!(result, arg::ParseError::RequiredArgMissing("path2"));

    let result = MyArgs::from_text("-f -r 5 --verbose").unwrap_err();
    assert_eq!(result, arg::ParseError::RequiredArgMissing("path"));
}

#[test]
fn should_error_on_missing_flag_value() {
    let result = MyArgs::from_text("-f -r").unwrap_err();
    assert_eq!(result, arg::ParseError::MissingValue("required"));
}

#[test]
fn should_error_on_missing_option_bool() {
    let result = BoolOptionArgs::from_text("").unwrap_err();
    assert_eq!(result, arg::ParseError::RequiredArgMissing("option"));
}

#[test]
fn should_error_on_invalid_flag_value() {
    let result = MyArgs::from_text("-f -r gg").unwrap_err();
    assert_eq!(result, arg::ParseError::InvalidFlagValue("required", "gg"));
}

#[test]
fn should_handle_all_flags() {
    let result = MyArgs::from_text("-f -r 5 --verbose -v 32 -g 1 --gps 55 path1 path2 rest1 rest2").unwrap();
    assert!(result.flag);
    assert!(result.verbose);
    assert_eq!(result.optional, None);
    assert_eq!(result.required, 5);
    assert_eq!(result.speed, 32);
    assert_eq!(result.gps, &[1, 55]);
    assert_eq!(result.path, "path1");
    assert_eq!(result.path2, "path2");
    assert_eq!(result.remain_paths, &["rest1", "rest2"]);

    let result = MyArgs::from_text("-f -r 5 --verbose -o 13 -v 32 -g 1 --gps 55 path1 path2 rest1 rest2").unwrap();
    assert!(result.flag);
    assert!(result.verbose);
    assert_eq!(result.optional, Some(13));
    assert_eq!(result.required, 5);
    assert_eq!(result.speed, 32);
    assert_eq!(result.gps, &[1, 55]);
    assert_eq!(result.path, "path1");
    assert_eq!(result.path2, "path2");
    assert_eq!(result.remain_paths, &["rest1", "rest2"]);
}

#[test]
fn should_fail_missing_sub_command() {
    let result = BigArgs::from_text("--verbose").unwrap_err();
    println!("result={result}");
    assert_eq!(result, arg::ParseKind::Top(arg::ParseError::HelpRequested("Missing command, possible values: [my-args, test]\nSee 'help' for details")));
}

#[test]
fn should_fail_invalid_sub_command() {
    let result = BigArgs::from_text("--verbose my-invalid-args -f -r 5 --verbose -v 32 -g 1 --gps 55 path1 path2 rest1 rest2").unwrap_err();
    assert_eq!(result, arg::ParseKind::Top(arg::ParseError::InvalidArgValue("cmd", "my-invalid-args")));
}

#[test]
fn should_fail_sub_command_with_wrong_args() {
    let result = BigArgs::from_text("--verbose my-args -f -r 5 --verbose -v 32 -g 1 --gps lolka path1 path2 rest1 rest2").unwrap_err();
    assert_eq!(result, arg::ParseKind::Sub("my-args", arg::ParseError::InvalidFlagValue("gps", "lolka")));
}

#[test]
fn should_handle_all_flags_as_sub_command() {
    let result = BigArgs::from_text("--verbose my-args -f -r 5 --verbose -v 32 -g 1 --gps 55 path1 path2 rest1 rest2").unwrap();
    assert!(result.verbose);
    let result = match result.cmd {
        BigSubArgs::MyArgs(args) => args,
        unexpected => panic!("invalid sub command result: {:?}", unexpected),
    };
    assert!(result.flag);
    assert!(result.verbose);
    assert_eq!(result.optional, None);
    assert_eq!(result.required, 5);
    assert_eq!(result.speed, 32);
    assert_eq!(result.gps, &[1, 55]);
    assert_eq!(result.path, "path1");
    assert_eq!(result.path2, "path2");
    assert_eq!(result.remain_paths, &["rest1", "rest2"]);

    let result = BigArgs::from_text("--verbose test -u path1 path2 rest1 rest2").unwrap();
    assert!(result.verbose);
    let result = match result.cmd {
        BigSubArgs::Test(args) => args,
        unexpected => panic!("invalid sub command result: {:?}", unexpected),
    };
    assert!(result.u);
    assert_eq!(result.paths, &["path1", "path2", "rest1", "rest2"]);
}

#[test]
fn should_supply_default_value() {
    let result = MyArgs::from_text("-f -r 5 --verbose -g 1 --gps 55 path1 path2 rest1 rest2").unwrap();
    assert_eq!(result.speed, 42);
}

#[test]
fn shoukd_handle_dash() {
    let result = MyArgs::from_text("-f -r 5 --verbose -g 1 --gps 55 path1 path2 rest1 -").unwrap();
    assert_eq!(result.remain_paths[1], "-");
}
