use std::fs;

use clap::Parser;

mod interpreter;

#[derive(Parser)]
#[command(version, about)]
struct Args {
    input: String,
}

fn main() {
    let args = Args::parse();
    let text = fs::read_to_string(args.input).expect("Failed to read input file");
    let program = interpreter::parse::parse(&text);
    let mut env = interpreter::env::Env::new(program);
    let result = env.run("main", &[]);
    println!("{:?}  ", result);
}
