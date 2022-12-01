use std::rc::Rc;

use crate::input::SourceString;

#[derive(Debug, Clone)]
pub struct MacroArgs {
    args: Vec<Rc<SourceString>>,
    shift: usize,
}

impl MacroArgs {
    pub fn new(args: Vec<Rc<SourceString>>) -> Self {
        Self { args, shift: 0 }
    }

    pub fn nb_args(&self) -> usize {
        self.args.len()
    }

    pub fn get(&self, idx: usize) -> Option<&Rc<SourceString>> {
        self.args.get(idx + self.shift - 1)
    }

    pub fn make_concat(&self) -> Rc<SourceString> {
        let mut args = self.args.iter();
        match args.next() {
            None => Rc::new(SourceString::new()),
            Some(first_arg) => {
                if self.args.len() == 1 {
                    Rc::clone(first_arg)
                } else {
                    let mut string = SourceString::clone(first_arg);
                    let result = SourceString::make_owned(&mut string);
                    for arg in args {
                        result.push(',');
                        result.push_str(arg);
                    }
                    Rc::new(string)
                }
            }
        }
    }
}
