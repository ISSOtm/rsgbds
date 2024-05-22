use std::{
    fs::File,
    io::{BufRead, BufReader, BufWriter, Read, StdinLock, StdoutLock, Write},
    path::Path,
};

pub enum Input<'stdin> {
    File(BufReader<File>),
    Stdin(StdinLock<'stdin>),
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

impl Input<'_> {
    pub fn new<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        Ok(if path.as_ref() == Path::new("-") {
            Self::Stdin(std::io::stdin().lock())
        } else {
            Self::File(BufReader::new(File::open(path)?))
        })
    }
}

impl Output {
    pub fn new<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        Ok(if path.as_ref() == Path::new("-") {
            Self::Stdout(std::io::stdout().lock())
        } else {
            Self::File(BufWriter::new(File::create(path)?))
        })
    }
}

macro_rules! forward {
    ($Stdio:pat => $io:expr, $(fn $name:ident $params:tt $( -> $ret:ty )?;)*) => {$(
        forward_one!($Stdio, $io, $name, $params, $($ret)?);
    )*};
}

macro_rules! forward_one {
    ($Stdio:pat, $io:expr, $name:ident, (&self $(
        , $param_name:ident: $param_ty:ty
    )*), $($ret:ty)?) => {
        fn $name(&self $(, $param_name: $param_ty)*) $( -> $ret )? {
            match self {
                Self::$Stdio => $io.$name($($param_name),*),
                Self::File(file) => file.$name($($param_name),*),
            }
        }
    };
    ($Stdio:pat, $io:expr, $name:ident, (&mut self $(
        , $param_name:ident: $param_ty:ty
    )*), $($ret:ty)?) => {
        fn $name(&mut self $(, $param_name: $param_ty)*) $( -> $ret )? {
            match self {
                $Stdio => $io.$name($($param_name),*),
                Self::File(file) => file.$name($($param_name),*),
            }
        }
    };
}

impl Read for Input<'_> {
    forward! { Self::Stdin(lock) => lock,
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize>;
        fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize>;
        fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize>;
        fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize>;
        fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()>;
    }
}

impl BufRead for Input<'_> {
    forward! { Self::Stdin(lock) => lock,
        fn fill_buf(&mut self) -> std::io::Result<&[u8]>;
        fn consume(&mut self, amt: usize);
        fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> std::io::Result<usize>;
        fn read_line(&mut self, buf: &mut String) -> std::io::Result<usize>;
    }
}

impl Write for Output {
    forward! { Self::Stdout => std::io::stdout(),
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize>;
        fn flush(&mut self) -> std::io::Result<()>;
        fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()>;
        fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()>;
    }
}
