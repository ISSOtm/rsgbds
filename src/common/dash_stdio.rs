use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

pub enum Input {
    File(File),
    Stdin,
}

pub enum Output {
    File(File),
    Stdout,
}

pub fn read<P: AsRef<Path>>(path: P) -> std::io::Result<Vec<u8>> {
    let mut buf = Vec::new();
    Input::new(path)?.read_to_end(&mut buf)?;
    Ok(buf)
}

impl Input {
    pub fn new<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        Ok(if path.as_ref() == Path::new("-") {
            Self::Stdin
        } else {
            Self::File(File::open(path)?)
        })
    }
}

impl Output {
    pub fn new<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        Ok(if path.as_ref() == Path::new("-") {
            Self::Stdout
        } else {
            Self::File(File::create(path)?)
        })
    }
}

macro_rules! forward {
    (const $Stdio:ident = $io_func:expr; $(fn $name:ident $params:tt -> $ret:ty;)*) => {$(
        forward_one!($Stdio, $io_func, $name, $params, $ret);
    )*};
}

macro_rules! forward_one {
    ($Stdio:ident, $io_func:expr, $name:ident, (&self $(
        , $param_name:ident: $param_ty:ty
    )*), $ret:ty) => {
        fn $name(&self $(, $param_name: $param_ty)*) -> $ret {
            match self {
                Self::$Stdio => $io_func.$name($($param_name),*),
                Self::File(file) => file.$name($($param_name),*),
            }
        }
    };
    ($Stdio:ident, $io_func:expr, $name:ident, (&mut self $(
        , $param_name:ident: $param_ty:ty
    )*), $ret:ty) => {
        fn $name(&mut self $(, $param_name: $param_ty)*) -> $ret {
            match self {
                Self::$Stdio => $io_func.$name($($param_name),*),
                Self::File(file) => file.$name($($param_name),*),
            }
        }
    };
}

impl Read for Input {
    forward! {
        const Stdin = std::io::stdin();

        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize>;

        fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize>;

        fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize>;

        fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize>;

        fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()>;
    }
}

impl Write for Output {
    forward! {
        const Stdout = std::io::stdout();

        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize>;

        fn flush(&mut self) -> std::io::Result<()>;

        fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()>;

        fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()>;
    }
}
