use std::{
    cell::RefCell,
    rc::Rc,
    mem,
    marker::PhantomData,
};

use emacs::{defun, Env, Value, Result, IntoLisp, FromLisp, Transfer, Vector};

use tree_sitter::{Tree, Node, TreeCursor, Parser};
use std::cell::{Ref, RefMut};
use std::ops::{Deref, DerefMut};

pub fn shared<T>(t: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(t))
}

unsafe fn erase_lifetime<'t, T>(x: &'t T) -> &'static T {
    mem::transmute(x)
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

impl_newtype_traits!(Point);

impl IntoLisp<'_> for Point {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        let inner = self.0;
        env.call("vector", (inner.row, inner.column))
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
        env.call("vector", (
            inner.start_byte,
            inner.end_byte,
            Point(inner.start_point),
            Point(inner.end_point),
        ))
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

/// Wrapper around `tree_sitter::Node` that can have 'static lifetime, by keeping a ref-counted
/// reference to the underlying tree.
#[derive(Clone)]
pub struct RNode {
    tree: SharedTree,
    inner: Node<'static>,
}

pub struct RNodeBorrow<'e> {
    #[allow(unused)]
    reft: Ref<'e, Tree>,
    node: &'e Node<'e>,
}

impl<'e> Deref for RNodeBorrow<'e> {
    type Target = Node<'e>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.node
    }
}

pub struct RNodeBorrowMut<'e> {
    #[allow(unused)]
    reft: RefMut<'e, Tree>,
    node: Node<'e>,
}

impl<'e> Deref for RNodeBorrowMut<'e> {
    type Target = Node<'e>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<'e> DerefMut for RNodeBorrowMut<'e> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

impl RNode {
    pub fn new<'e, F: Fn(&'e Tree) -> Node<'e>>(tree: SharedTree, f: F) -> Self {
        let rtree = unsafe { erase_lifetime(&*tree.borrow()) };
        let inner = unsafe { mem::transmute(f(rtree)) };
        Self { tree, inner }
    }

    pub fn clone_tree(&self) -> SharedTree {
        self.tree.clone()
    }

    pub fn map<'e, F: Fn(&Node<'e>) -> Node<'e>>(&self, f: F) -> Self {
        Self::new(self.clone_tree(), |_| f(&self.inner))
    }

    #[inline]
    pub fn borrow(&self) -> RNodeBorrow {
        let reft = self.tree.borrow();
        let node = &self.inner;
        RNodeBorrow { reft, node }
    }

    #[inline]
    pub fn borrow_mut(&mut self) -> RNodeBorrowMut {
        let reft = self.tree.borrow_mut();
        let node = self.inner;
        RNodeBorrowMut { reft, node }
    }
}

// -------------------------------------------------------------------------------------------------
// Cursor

/// Wrapper around `tree_sitter::TreeCursor` that can have 'static lifetime, by keeping a
/// ref-counted reference to the underlying tree.
pub struct RCursor {
    tree: SharedTree,
    inner: TreeCursor<'static>,
}

pub struct RCursorBorrow<'e> {
    #[allow(unused)]
    reft: Ref<'e, Tree>,
    cursor: &'e TreeCursor<'e>,
}

impl<'e> Deref for RCursorBorrow<'e> {
    type Target = TreeCursor<'e>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.cursor
    }
}

pub struct RCursorBorrowMut<'e> {
    #[allow(unused)]
    reft: RefMut<'e, Tree>,
    cursor: &'e mut TreeCursor<'e>,
}

impl<'e> Deref for RCursorBorrowMut<'e> {
    type Target = TreeCursor<'e>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.cursor
    }
}

impl<'e> DerefMut for RCursorBorrowMut<'e> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.cursor
    }
}

impl RCursor {
    pub fn new<'e, F: Fn(&'e Tree) -> TreeCursor<'e>>(tree: SharedTree, f: F) -> Self {
        let rtree = unsafe { erase_lifetime(&*tree.borrow()) };
        let inner = unsafe { mem::transmute(f(rtree)) };
        Self { tree, inner }
    }

    pub fn clone_tree(&self) -> SharedTree {
        self.tree.clone()
    }

    #[inline]
    pub fn borrow(&self) -> RCursorBorrow {
        let reft = self.tree.borrow();
        let cursor = &self.inner;
        RCursorBorrow { reft, cursor }
    }

    #[inline]
    pub fn borrow_mut<'e>(&'e mut self) -> RCursorBorrowMut {
        let reft: RefMut<'e, Tree> = self.tree.borrow_mut();
        // XXX: Explain the safety here.
        let cursor: &'e mut _ = unsafe { mem::transmute(&mut self.inner) };
        RCursorBorrowMut { reft, cursor }
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
impl_pred!(node_p, &RefCell<RNode>);
impl_pred!(cursor_p, &RefCell<RCursor>);
