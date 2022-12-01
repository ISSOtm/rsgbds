use std::{
    mem::{ManuallyDrop, MaybeUninit},
    ops::Deref,
};

use rgbds::RelocKind;

use super::{Instruction, PrefixKind};
use crate::{expr::ByteOrExpr, language::ParseError};

#[derive(Debug)]
pub struct Encoder<'fstack>(EncoderStorage<ByteOrExpr<'fstack>>, usize);
type EncoderStorage<T> = [MaybeUninit<T>; 2];

impl<'fstack> Encoder<'fstack> {
    pub fn new(instr: Instruction<'fstack>) -> Result<Self, ParseError<'fstack>> {
        let mut storage: EncoderStorage<_> = unsafe { MaybeUninit::uninit().assume_init() };

        // Let's try to make the monster below a bit more digestible.
        macro_rules! encode {
            ($variant0:expr $( , $variant1:expr )?) => { {
                storage[0].write($variant0);
                $( storage[1].write($variant1); )?
                len!( {$variant0} $({$variant1})? )
            } };
        }
        macro_rules! len {
            () => { 0 };
            ($tok:tt $( $more:tt )*) => { len!($($more)*) + 1 };
        }
        use ByteOrExpr::Byte;
        let expr = ByteOrExpr::try_from_expr;

        use Instruction::*;
        let nb_elems = match instr {
            Nop => encode!(Byte(0x00)),
            LdAddr16Sp(addr) => encode!(Byte(0x08), expr(addr, RelocKind::Word)?),
            Stop(skipped) => encode!(Byte(0x10), expr(skipped, RelocKind::Byte)?),
            Jr(ofs) => encode!(Byte(0x18), expr(ofs, RelocKind::Ofs8)?),
            JrCond(cond, ofs) => encode!(Byte(0x20 | cond as u8), expr(ofs, RelocKind::Ofs8)?),
            LdImm16(dest, src) => encode!(Byte(0x01 | dest as u8), expr(src, RelocKind::Word)?),
            AddHl(rhs) => encode!(Byte(0x09 | rhs as u8)),
            LdReg16IndA(dest) => encode!(Byte(0x02 | dest as u8)),
            LdAReg16Ind(src) => encode!(Byte(0x0A | src as u8)),
            IncReg16(reg) => encode!(Byte(0x03 | reg as u8)),
            DecReg16(reg) => encode!(Byte(0x0B | reg as u8)),
            IncReg8(reg) => encode!(Byte(0x04 | (reg as u8) << 3)),
            DecReg8(reg) => encode!(Byte(0x05 | (reg as u8) << 3)),
            LdImm8(dest, src) => {
                encode!(Byte(0x06 | (dest as u8) << 3), expr(src, RelocKind::Byte)?)
            }
            Rlca => encode!(Byte(0x07)),
            Rrca => encode!(Byte(0x0F)),
            Rla => encode!(Byte(0x17)),
            Rra => encode!(Byte(0x1F)),
            Daa => encode!(Byte(0x27)),
            Cpl => encode!(Byte(0x2F)),
            Scf => encode!(Byte(0x37)),
            Ccf => encode!(Byte(0x3F)),

            LdReg8Reg8(dest, src) => encode!(Byte(0x40 | (dest as u8) << 3 | src as u8)),
            Halt => encode!(Byte(0x76)),

            Add(rhs) => encode!(Byte(0x80 | rhs as u8)),
            Adc(rhs) => encode!(Byte(0x88 | rhs as u8)),
            Sub(rhs) => encode!(Byte(0x90 | rhs as u8)),
            Sbc(rhs) => encode!(Byte(0x98 | rhs as u8)),
            And(rhs) => encode!(Byte(0xA0 | rhs as u8)),
            Xor(rhs) => encode!(Byte(0xA8 | rhs as u8)),
            Or(rhs) => encode!(Byte(0xB0 | rhs as u8)),
            Cp(rhs) => encode!(Byte(0xB8 | rhs as u8)),

            RetCond(cond) => encode!(Byte(0xC0 | cond as u8)),
            LdhAddr8A(dest) => encode!(Byte(0xE0), expr(dest, RelocKind::Byte)?),
            AddSpRel8(ofs) => encode!(Byte(0xE8), expr(ofs, RelocKind::Ofs8)?),
            LdhAAddr8(src) => encode!(Byte(0xF0), expr(src, RelocKind::Byte)?),
            LdHlSpRel8(ofs) => encode!(Byte(0xF8), expr(ofs, RelocKind::Ofs8)?),
            Pop(reg) => encode!(Byte(0xC1 | reg as u8)),
            Ret => encode!(Byte(0xC9)),
            Reti => encode!(Byte(0xD9)),
            JpHl => encode!(Byte(0xE9)),
            LdSpHl => encode!(Byte(0xF9)),
            JpCond(cond, dest) => encode!(Byte(0xC2 | cond as u8), expr(dest, RelocKind::Word)?),
            LdhCA => encode!(Byte(0xE2)),
            LdAddr16A(dest) => encode!(Byte(0xEA), expr(dest, RelocKind::Byte)?),
            LdhAC => encode!(Byte(0xF2)),
            LdAAddr16(src) => encode!(Byte(0xFA), expr(src, RelocKind::Byte)?),
            Jp(dest) => encode!(Byte(0xC3), expr(dest, RelocKind::Word)?),
            Di => encode!(Byte(0xF3)),
            Ei => encode!(Byte(0xFB)),
            CallCond(cond, dest) => encode!(Byte(0xC4 | cond as u8), expr(dest, RelocKind::Word)?),
            Push(reg) => encode!(Byte(0xC5 | reg as u8)),
            Call(dest) => encode!(Byte(0xCD), expr(dest, RelocKind::Word)?),

            AddImm8(rhs) => encode!(Byte(0xC6), expr(rhs, RelocKind::Byte)?),
            AdcImm8(rhs) => encode!(Byte(0xCE), expr(rhs, RelocKind::Byte)?),
            SubImm8(rhs) => encode!(Byte(0xD6), expr(rhs, RelocKind::Byte)?),
            SbcImm8(rhs) => encode!(Byte(0xDE), expr(rhs, RelocKind::Byte)?),
            AndImm8(rhs) => encode!(Byte(0xE6), expr(rhs, RelocKind::Byte)?),
            XorImm8(rhs) => encode!(Byte(0xEE), expr(rhs, RelocKind::Byte)?),
            OrImm8(rhs) => encode!(Byte(0xF6), expr(rhs, RelocKind::Byte)?),
            CpImm8(rhs) => encode!(Byte(0xFE), expr(rhs, RelocKind::Byte)?),
            Rst(dest) => encode!(expr(dest, RelocKind::Byte)?),

            Prefixed(PrefixKind::Rlc, reg) => encode!(Byte(0xCB), Byte(0x00 | reg as u8)),
            Prefixed(PrefixKind::Rrc, reg) => encode!(Byte(0xCB), Byte(0x08 | reg as u8)),
            Prefixed(PrefixKind::Rl, reg) => encode!(Byte(0xCB), Byte(0x10 | reg as u8)),
            Prefixed(PrefixKind::Rr, reg) => encode!(Byte(0xCB), Byte(0x18 | reg as u8)),
            Prefixed(PrefixKind::Sla, reg) => encode!(Byte(0xCB), Byte(0x20 | reg as u8)),
            Prefixed(PrefixKind::Sra, reg) => encode!(Byte(0xCB), Byte(0x28 | reg as u8)),
            Prefixed(PrefixKind::Swap, reg) => encode!(Byte(0xCB), Byte(0x30 | reg as u8)),
            Prefixed(PrefixKind::Srl, reg) => encode!(Byte(0xCB), Byte(0x38 | reg as u8)),
            Prefixed(PrefixKind::Bit(bit), reg) => {
                encode!(Byte(0xCB), Byte(0x40 | bit << 3 | reg as u8))
            }
            Prefixed(PrefixKind::Res(bit), reg) => {
                encode!(Byte(0xCB), Byte(0x80 | bit << 3 | reg as u8))
            }
            Prefixed(PrefixKind::Set(bit), reg) => {
                encode!(Byte(0xCB), Byte(0xC0 | bit << 3 | reg as u8))
            }
        };

        Ok(Self(storage, nb_elems))
    }
}

/// Since the `Encoder` contains a slice of `MaybeUninit`s, it's responsible for dropping any that
/// have been init'd during `new()`.
impl Drop for Encoder<'_> {
    fn drop(&mut self) {
        for item in &mut self.0[..self.1] {
            unsafe {
                item.assume_init_drop();
            }
        }
    }
}

impl<'fstack> Deref for Encoder<'fstack> {
    type Target = [ByteOrExpr<'fstack>];

    fn deref(&self) -> &Self::Target {
        // SAFETY:
        //    All elements in this slice have been initialised by `new()`; as for the lifetimes of
        //    the references we are transmutating, they are constrained by the function's signature.
        unsafe { std::mem::transmute(&self.0[..self.1]) }
    }
}

impl<'fstack> AsRef<<Self as Deref>::Target> for Encoder<'fstack> {
    fn as_ref(&self) -> &<Self as Deref>::Target {
        self.deref()
    }
}

impl<'fstack> IntoIterator for Encoder<'fstack> {
    type IntoIter = Iter<'fstack>;
    type Item = <Iter<'fstack> as Iterator>::Item;

    fn into_iter(self) -> Self::IntoIter {
        Iter::new(self)
    }
}

// Since this iterator consumes the elements, the init'd slice will shrink from the left edge, and
// the `Encoder`'s destructor does not know this. Thus, `ManuallyDrop`, and we `Drop` ourselves.
#[derive(Debug)]
pub struct Iter<'fstack>(ManuallyDrop<Encoder<'fstack>>, usize);

impl<'fstack> Iter<'fstack> {
    fn new(encoder: Encoder<'fstack>) -> Self {
        Self(ManuallyDrop::new(encoder), 0)
    }
}

impl Drop for Iter<'_> {
    fn drop(&mut self) {
        let end = self.0 .1;
        for item in &mut self.0 .0[self.1..end] {
            unsafe {
                item.assume_init_drop();
            }
        }
    }
}

impl<'fstack> Iterator for Iter<'fstack> {
    type Item = ByteOrExpr<'fstack>;

    fn next(&mut self) -> Option<Self::Item> {
        debug_assert!(self.1 <= self.0 .1);
        if self.1 == self.0 .1 {
            return None;
        }
        self.1 += 1;
        // SAFETY: `self.1` has been increased above, so this won't be read again.
        Some(unsafe { self.0 .0[self.1 - 1].assume_init_read() })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.0 .1 - self.1;
        (len, Some(len))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // This is mainly intended as a "no-UB" check for running under Miri.
    #[test]
    fn nop_encoding() {
        let encoded = Encoder::new(Instruction::Nop).expect("Failed to encode `nop`!?");
        let encoding: Vec<_> = encoded.into_iter().collect();

        assert_eq!(encoding.len(), 1);
        assert!(
            matches!(encoding[0], ByteOrExpr::Byte(0x00)),
            "{:?}",
            encoding[0]
        );
    }
}
