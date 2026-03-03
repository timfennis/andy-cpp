use std::cmp::Ordering;

pub trait FallibleOrd {
    type Error;
    fn try_cmp(&self, other: &Self) -> Result<Ordering, Self::Error>;
}
