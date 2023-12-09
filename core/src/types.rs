use std::{
    mem,
    cell::RefCell,
    rc::Rc,
};

use emacs::{defun, Env, FromLisp, IntoLisp, Result, Value, Vector};

pub type Shared<T> = Rc<RefCell<T>>;

pub unsafe fn erase_lifetime<'t, T>(x: &'t T) -> &'static T {
    mem::transmute(x)
}

macro_rules! impl_pred {
    ($name:ident, $type:ty) => {
        #[defun]
        fn $name(value: Value) -> Result<bool> {
            Ok(value.into_rust::<$type>().is_ok())
        }
    };
}

macro_rules! impl_newtype_traits {
    ($newtype:ty, $inner:ty) => {
        impl From<$inner> for $newtype {
            #[inline(always)]
            fn from(inner: $inner) -> Self {
                Self(inner)
            }
        }

        impl Into<$inner> for $newtype {
            #[inline(always)]
            fn into(self) -> $inner {
                self.0
            }
        }
    };
    ($name:ident) => {
        impl_newtype_traits!($name, tree_sitter::$name);
    };
}

// -------------------------------------------------------------------------------------------------
// Point

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Point(tree_sitter::Point);

impl_pred!(point_p, Point);

impl_newtype_traits!(Point);

impl IntoLisp<'_> for Point {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        env.cons(self.line_number(), self.byte_column())
    }
}

impl FromLisp<'_> for Point {
    fn from_lisp(value: Value) -> Result<Point> {
        let row = value.car::<usize>()? - 1;
        let column = value.cdr()?;
        Ok(tree_sitter::Point { row, column }.into())
    }
}

impl Point {
    #[inline(always)]
    pub(crate) fn line_number(&self) -> usize {
        self.0.row + 1
    }

    #[inline(always)]
    pub(crate) fn byte_column(&self) -> usize {
        self.0.column
    }
}

// -------------------------------------------------------------------------------------------------
// Emacs Byte Position (1-based, which is different from byte offset, which is 0-based).

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(usize);

impl From<usize> for BytePos {
    #[inline(always)]
    fn from(byte_offset: usize) -> Self {
        Self(byte_offset + 1)
    }
}

impl Into<usize> for BytePos {
    #[inline(always)]
    fn into(self) -> usize {
        self.0 - 1
    }
}

impl FromLisp<'_> for BytePos {
    #[inline(always)]
    fn from_lisp(value: Value) -> Result<BytePos> {
        value.into_rust().map(Self)
    }
}

impl IntoLisp<'_> for BytePos {
    #[inline(always)]
    fn into_lisp(self, env: &Env) -> Result<Value> {
        self.0.into_lisp(env)
    }
}

// -------------------------------------------------------------------------------------------------
// Range

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Range(pub(crate) tree_sitter::Range);

impl_pred!(range_p, Range);

impl_newtype_traits!(Range);

impl IntoLisp<'_> for Range {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        let inner = self.0;
        let start_byte_pos: BytePos = inner.start_byte.into();
        let end_byte_pos: BytePos = inner.end_byte.into();
        env.vector((
            start_byte_pos,
            end_byte_pos,
            Point(inner.start_point),
            Point(inner.end_point),
        ))
    }
}

impl FromLisp<'_> for Range {
    fn from_lisp(value: Value) -> Result<Range> {
        let vector: Vector = value.into_rust()?;
        let start_byte = vector.get::<BytePos>(0)?.into();
        let end_byte = vector.get::<BytePos>(1)?.into();
        let start_point = vector.get::<Point>(2)?.into();
        let end_point = vector.get::<Point>(3)?.into();
        Ok(tree_sitter::Range { start_byte, end_byte, start_point, end_point }.into())
    }
}
