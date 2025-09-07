use std::fs;

use clap::Parser;

use crate::interpreter::{env::{Env,Value}, parse};

mod interpreter;

#[derive(Parser)]
#[command(version, about)]
struct Args {
    program: String,
    input: String,
    #[arg(short, long)]
    output: String,
}

fn main() {
    let args = Args::parse();
    let text = fs::read_to_string(args.program).expect("Failed to read program file");
    let program = parse::parse(&text);
    let mut env = Env::new(program);
    let input = fs::read_to_string(args.input).expect("Failed to read input file");
    match env.run("main", &[Value::Str(input)]) {
        Value::Str(output) => {
            fs::write(args.output, output).expect("Failed to write output file");
        }
        result => panic!("Unexpected return type {:?}", result),
    }
}
