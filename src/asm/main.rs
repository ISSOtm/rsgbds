use std::{cell::RefCell, fs::File, rc::Rc};

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
mod macro_args;
mod sections;
use sections::Sections;
mod symbols;
use symbols::Symbols;

fn main() {
    // TODO: arg parsing

    // TODO: colour choice
    let mut reporter = RefCell::new(Reporter::new(
        codespan_reporting::term::termcolor::ColorChoice::Always,
    ));

    let root_path = "/tmp/test.asm"; // TODO
    let root_file = File::open(root_path).expect("Failed to open root file"); // TODO: also support stdin/stdout
    let root_file = Rc::new(
        Storage::from_file(root_path.to_string().into(), &root_file)
            .expect("Failed to read root file"),
    );
    let fstack = Fstack::new(root_file);
    let sections = RefCell::new(Sections::new());
    let symbols = RefCell::new(Symbols::new());
    let lexer = RefCell::new(Lexer::new());
    let macro_args = RefCell::new(Vec::new());

    if let Err(error) = Parser::new().parse(
        &fstack,
        &lexer,
        &macro_args,
        &sections,
        &symbols,
        &reporter,
        Tokenizer::new(&fstack, &lexer, &macro_args, &reporter, &symbols),
    ) {
        reporter.get_mut().report_fatal_error(&fstack, error);
    };
}
