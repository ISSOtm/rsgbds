use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct MacroArgs {
    args: Vec<Rc<String>>,
    shift: usize,
}

impl MacroArgs {
    pub fn new(args: Vec<Rc<String>>) -> Self {
        Self { args, shift: 0 }
    }

    pub fn nb_args(&self) -> usize {
        self.args.len()
    }

    pub fn get(&self, idx: usize) -> Option<&Rc<String>> {
        self.args.get(idx + self.shift - 1)
    }

    pub fn make_concat(&self) -> Rc<String> {
        let mut args = self.args.iter();
        match args.next() {
            None => Rc::new(String::new()),

            Some(first_arg) => {
                if self.args.len() == 1 {
                    Rc::clone(first_arg)
                } else {
                    let mut string = String::clone(first_arg);
                    for arg in args {
                        string.push(',');
                        string.push_str(arg);
                    }
                    Rc::new(string)
                }
            }
        }
    }
}
