pub trait Bits {
    const BITS: u32;
}
macro_rules! impl_bits {
    ($($T:ty),+) => {
        $(impl Bits for $T {
            const BITS: u32 = <$T>::BITS;
        })+
    }
}
impl_bits!(u8, i8, u16, i16, u32, i32, u64, i64);

pub trait Signedness {
    const SIGNEDNESS: &str;
}
macro_rules! impl_signedness {
    ($signedness:literal, $($T:ty),+) => {
        $(impl Signedness for $T {
            const SIGNEDNESS: &str = $signedness;
        })+
    }
}
impl_signedness!("signed", i8, i16, i32, i64);
impl_signedness!("unsigned", u8, u16, u32, u64);
