use std::{
    cell::RefCell,
    rc::Rc,
    mem,
    ops::Deref,
    convert::TryInto,
    marker::PhantomData,
};

use emacs::{defun, Env, Value, Result, IntoLisp, FromLisp, Transfer, Vector};

use tree_sitter::{Tree, InputEdit, Node, TreeCursor};

pub fn shared<T>(t: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(t))
}

macro_rules! impl_wrapper_traits {
    ($wrapper:ty, $inner:ty) => {
        impl From<$inner> for $wrapper {
            #[inline(always)]
            fn from(inner: $inner) -> Self {
                Self(inner)
            }
        }

        impl Into<$inner> for $wrapper {
            #[inline(always)]
            fn into(self) -> $inner {
                self.0
            }
        }
    };
    ($name:ident) => {
        impl_wrapper_traits!($name, tree_sitter::$name);
    };
}

// -------------------------------------------------------------------------------------------------
// Point

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Point(tree_sitter::Point);

impl_wrapper_traits!(Point);

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
        Ok(Point(tree_sitter::Point { row, column }))
    }
}

// -------------------------------------------------------------------------------------------------
// Range

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range(pub(crate) tree_sitter::Range);

impl_wrapper_traits!(Range);

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
        Ok(Range(tree_sitter::Range { start_byte, end_byte, start_point, end_point }))
    }
}

// -------------------------------------------------------------------------------------------------
// Language

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Language(tree_sitter::Language);

impl_wrapper_traits!(Language);

impl Transfer for Language {
    fn type_name() -> &'static str {
        "TreeSitterLanguage"
    }
}

impl<'e> FromLisp<'e> for Language {
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

// XXX: Don't juggle with alignment...
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

impl WrappedNode {
    #[inline]
    pub unsafe fn new(tree: SharedTree, node: Node) -> Self {
        let ptr = (&node as *const Node) as *const RawNode;
        let raw = ptr.read();
        Self { tree, raw }
    }

    #[inline]
    pub unsafe fn wrap(&self, node: Node) -> Self {
        let tree = self.tree.clone();
        Self::new(tree, node)
    }

    #[inline]
    pub fn inner(&self) -> &Node {
        let ptr = (&self.raw as *const RawNode) as *const Node;
        unsafe { &*ptr }
    }

    #[inline]
    pub fn inner_mut(&mut self) -> &mut Node {
        let ptr = (&mut self.raw as *mut RawNode) as *mut Node;
        unsafe { &mut *ptr }
    }
}

// -------------------------------------------------------------------------------------------------
// Cursor

// XXX: Don't juggle with alignment...
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

impl WrappedCursor {
    #[inline]
    pub unsafe fn new(tree: SharedTree, cursor: TreeCursor) -> Self {
        let ptr = (&cursor as *const TreeCursor) as *const RawCursor;
        let raw = ptr.read();
        Self { tree, raw }
    }

    #[inline]
    pub unsafe fn wrap(&self, cursor: TreeCursor) -> Self {
        let tree = self.tree.clone();
        Self::new(tree, cursor)
    }

    #[inline]
    pub fn inner(&self) -> &TreeCursor {
        let ptr = (&self.raw as *const RawCursor) as *const TreeCursor;
        unsafe { &*ptr }
    }

    #[inline]
    pub fn inner_mut(&mut self) -> &mut TreeCursor {
        let ptr = (&mut self.raw as *mut RawCursor) as *mut TreeCursor;
        unsafe { &mut *ptr }
    }
}

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
