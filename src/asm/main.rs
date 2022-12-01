use std::{cell::RefCell, fs::File};

mod error;
use error::Reporter;
mod expr;
mod fstack;
use fstack::Fstack;
mod input;
use input::{SourceString, Storage};
mod instructions;
mod language;
use language::{Lexer, Parser, Tokenizer};
use symbols::Symbols;
mod symbols;

fn main() {
    let mut reporter = Reporter::new(codespan_reporting::term::termcolor::ColorChoice::Always);

    let root_path = "/tmp/test.asm";
    let root_file = File::open(root_path).expect("Failed to open root file");
    let root_file = Storage::from_file(root_path.to_string().into(), &root_file)
        .expect("Failed to read root file");
    let fstack = Fstack::new(root_file);
    let symbols = RefCell::new(Symbols::new(&fstack));
    let lexer = RefCell::new(Lexer::new());

    if let Err(error) = Parser::new().parse(
        &fstack,
        &lexer,
        &symbols,
        &mut reporter,
        Tokenizer::new(&fstack, &lexer, &symbols),
    ) {
        reporter.report_error(&fstack, error);
    };
}
