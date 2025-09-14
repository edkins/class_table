use std::fs;

use clap::Parser;

use crate::interpreter::{ast::ProgramFile, check::Value, env::Env, parse};

mod interpreter;

#[derive(Parser)]
#[command(version, about)]
struct Args {
    #[arg(short, long, default_value_t = false)]
    self_test: bool,
    program: Option<String>,
    input: Option<String>,
    #[arg(short, long)]
    output: Option<String>,
}

fn run_tests() {
    let mut paths: Vec<_> = fs::read_dir("ct_src")
        .expect("Failed to read test directory")
        .map(|entry| entry.unwrap().path())
        .collect();
    paths.sort();

    for path in paths {
        if path.extension().and_then(|s| s.to_str()) != Some("txt") {
            continue;
        }
        if !path.to_str().is_some_and(|s| s.starts_with("ct_src/test")) {
            continue;
        }
        let program_name = path.to_str().unwrap();
        println!("Running test {}", program_name);
        let _ = run_program(program_name, "input would go here");
    }
}

fn run_program(program_name: &str, input_text: &str) -> String {
    let (filename_base, program) = parse_program(program_name);
    let (std_base, std_program) = parse_program("ct_src/std.txt");
    let mut env = Env::new(
        &[(std_base, std_program), (filename_base.clone(), program)],
        &filename_base,
    );
    match env.run("main", &[Value::Str(input_text.to_string())]) {
        Value::Str(output) => output,
        result => panic!("Unexpected return type {:?}", result),
    }
}

fn parse_program(program_name: &str) -> (String, ProgramFile) {
    let text = fs::read_to_string(program_name).expect("Failed to read program file");
    let program = parse::parse(&text);
    let filename_base = program_name
        .rsplit('/')
        .next()
        .unwrap()
        .split('.')
        .next()
        .unwrap();
    (filename_base.to_owned(), program)
}

fn main() {
    let args = Args::parse();
    if args.self_test {
        run_tests();
        return;
    }
    let program_name = args.program.clone().unwrap();
    let input = fs::read_to_string(args.input.unwrap()).expect("Failed to read input file");
    let output = run_program(&program_name, &input);
    fs::write(args.output.unwrap(), output).expect("Failed to write output file");
}
