use std::{
    cell::RefCell,
    rc::Rc,
    mem,
    ops::Deref,
    convert::TryInto,
    marker::PhantomData,
};

use emacs::{defun, Env, Value, Result, IntoLisp, FromLisp, Transfer, Vector};

use tree_sitter::{Tree, InputEdit, Node, TreeCursor, Parser};

pub fn shared<T>(t: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(t))
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

macro_rules! impl_wrapper {
    ($inner:path, $raw:path, $wrapper:path) => {
        impl $wrapper {
            #[inline]
            pub unsafe fn new(tree: SharedTree, inner: $inner) -> Self {
                let ptr = (&inner as *const $inner) as *const $raw;
                let raw = ptr.read();
                Self { tree, raw }
            }

            #[inline]
            pub unsafe fn wrap(&self, inner: $inner) -> Self {
                let tree = self.tree.clone();
                Self::new(tree, inner)
            }

            #[inline]
            pub fn inner(&self) -> &$inner {
                let ptr = (&self.raw as *const $raw) as *const $inner;
                unsafe { &*ptr }
            }

            #[inline]
            pub fn inner_mut(&mut self) -> &mut $inner {
                let ptr = (&mut self.raw as *mut $raw) as *mut $inner;
                unsafe { &mut *ptr }
            }
        }
    };
}

// -------------------------------------------------------------------------------------------------
// Point

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Point(tree_sitter::Point);

impl_newtype_traits!(Point);

impl IntoLisp<'_> for Point {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        let inner = self.0;
        env.call("vector", &[
            inner.row.into_lisp(env)?,
            inner.column.into_lisp(env)?,
        ])
    }
}

impl FromLisp<'_> for Point {
    fn from_lisp(value: Value) -> Result<Point> {
        let vector = Vector(value);
        let row = vector.get(0)?;
        let column = vector.get(1)?;
        Ok(tree_sitter::Point { row, column }.into())
    }
}

// -------------------------------------------------------------------------------------------------
// Range

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range(pub(crate) tree_sitter::Range);

impl_newtype_traits!(Range);

impl IntoLisp<'_> for Range {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        let inner = self.0;
        env.call("vector", &[
            inner.start_byte.into_lisp(env)?,
            inner.end_byte.into_lisp(env)?,
            Point(inner.start_point).into_lisp(env)?,
            Point(inner.end_point).into_lisp(env)?,
        ])
    }
}

impl FromLisp<'_> for Range {
    fn from_lisp(value: Value) -> Result<Range> {
        let vector = Vector(value);
        let start_byte = vector.get(0)?;
        let end_byte = vector.get(1)?;
        let start_point = vector.get::<Point>(2)?.0;
        let end_point = vector.get::<Point>(3)?.0;
        Ok(tree_sitter::Range { start_byte, end_byte, start_point, end_point }.into())
    }
}

// -------------------------------------------------------------------------------------------------
// Language

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Language(pub(crate) tree_sitter::Language);

impl_newtype_traits!(Language);

impl Transfer for Language {
    fn type_name() -> &'static str {
        "TreeSitterLanguage"
    }
}

impl IntoLisp<'_> for Language {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        Box::new(self).into_lisp(env)
    }
}

impl FromLisp<'_> for Language {
    fn from_lisp(value: Value) -> Result<Language> {
        Ok(*value.into_rust::<&Language>()?)
    }
}

// -------------------------------------------------------------------------------------------------
// Tree

// TODO: We probably don't need RefCell.
pub type SharedTree = Rc<RefCell<Tree>>;

// -------------------------------------------------------------------------------------------------
// Node

// XXX: Find a better way to make 2 types have the same alignment.
const NODE_LEN: usize = mem::size_of::<Node>() / 8;
pub type RawNode = [u64; NODE_LEN];

/// Wrapper around `tree_sitter::Node` that can have 'static lifetime, by keeping a ref-counted
/// reference to the underlying tree.
#[derive(Clone)]
#[repr(C)]
pub struct WrappedNode {
    pub(crate) tree: SharedTree,
    raw: RawNode,
}

impl_wrapper!(Node, RawNode, WrappedNode);

// -------------------------------------------------------------------------------------------------
// Cursor

// XXX: Find a better way to make 2 types have the same alignment.
const TREE_CURSOR_LEN: usize = mem::size_of::<TreeCursor>() / 8;
pub type RawCursor = [u64; TREE_CURSOR_LEN];

/// Wrapper around `tree_sitter::TreeCursor` that can have 'static lifetime, by keeping a
/// ref-counted reference to the underlying tree.
#[derive(Clone)]
#[repr(C)]
pub struct WrappedCursor {
    pub(crate) tree: SharedTree,
    raw: RawCursor,
}

impl_wrapper!(TreeCursor, RawCursor, WrappedCursor);

// -------------------------------------------------------------------------------------------------

pub enum Either<'e, L, R> where L: FromLisp<'e>, R: FromLisp<'e> {
    Left(L, PhantomData<&'e ()>),
    Right(R, PhantomData<&'e ()>),
}

impl<'e, L, R> FromLisp<'e> for Either<'e, L, R> where L: FromLisp<'e>, R: FromLisp<'e> {
    fn from_lisp(value: Value<'e>) -> Result<Self> {
        if let Ok(value) = value.into_rust::<L>() {
            return Ok(Either::Left(value, PhantomData));
        }
        let value = value.into_rust::<R>()?;
        Ok(Either::Right(value, PhantomData))
    }
}

macro_rules! impl_pred {
    ($name:ident, $type:ty) => {
        #[defun]
        fn $name(value: Value) -> Result<bool> {
            Ok(value.into_rust::<$type>().is_ok())
        }
    };
}

// TODO: Add docstring for these.
impl_pred!(language_p, Language);
impl_pred!(range_p, Range);
impl_pred!(point_p, Point);
impl_pred!(parser_p, &RefCell<Parser>);
impl_pred!(tree_p, &SharedTree);
impl_pred!(node_p, &RefCell<WrappedNode>);
impl_pred!(cursor_p, &RefCell<WrappedCursor>);
