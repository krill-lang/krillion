use core::mem::{copy, transmute};

pub enum MaybeMutable<'a, A> {
    Mutable(&'a mut A),
    Immutable(&'a A),
}

impl<'a, A: Clone> MaybeMutable<'a, A> {
    #[allow(clippy::mut_from_ref)]
    pub fn unwrap_mut(&'a self) -> &'a mut A {
        match self {
            Self::Mutable(ptr) => unsafe { transmute(copy(transmute::<_, &&A>(ptr))) },
            _ => panic!("called `unwrap_mut()` on immutable reference"),
        }
    }

    /* pub fn unwrap_immut(&'a self) -> &'a A {
        match self {
            Self::Immutable(ptr) => ptr,
            _ => panic!("called `unwrap_mut()` on mutable reference"),
        }
    } */

    pub fn force_immut(&'a self) -> &'a A {
        match self {
            Self::Immutable(ptr) => ptr,
            Self::Mutable(ptr) => ptr,
        }
    }

    pub fn map_mut(&'a self, f: impl Fn(&'a mut A)) {
        if let Self::Mutable(_) = self {
            f(self.unwrap_mut())
        }
    }
}

use std::fmt::*;
impl<'a, A: Display> Display for MaybeMutable<'a, A> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Mutable(i) => write!(f, "mut {i}"),
            Self::Immutable(i) => write!(f, "immut {i}"),
        }
    }
}

#[allow(clippy::mut_from_ref)]
pub unsafe fn as_mut<A>(a: &A) -> &mut A {
    std::mem::transmute(a)
}
