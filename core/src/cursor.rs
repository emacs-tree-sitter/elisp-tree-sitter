use std::{
    cell::{Ref, RefCell},
    mem,
    ops::{Deref, DerefMut},
};

use emacs::{defun, Result, Value, GlobalRef};
use tree_sitter::{Tree, TreeCursor};

use crate::{
    types::{self, Shared, Either, BytePos},
    node::RNode,
    lang::Language,
};

// -------------------------------------------------------------------------------------------------

/// Wrapper around `tree_sitter::TreeCursor` that can have 'static lifetime, by keeping a
/// ref-counted reference to the underlying tree.
pub struct RCursor {
    tree: Shared<Tree>,
    inner: TreeCursor<'static>,
}

impl_pred!(cursor_p, &RefCell<RCursor>);

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
    reft: Ref<'e, Tree>,
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
    pub fn new<'e, F: FnOnce(&'e Tree) -> TreeCursor<'e>>(tree: Shared<Tree>, f: F) -> Self {
        let rtree = unsafe { types::erase_lifetime(&*tree.borrow()) };
        let inner = unsafe { mem::transmute(f(rtree)) };
        Self { tree, inner }
    }

    pub fn clone_tree(&self) -> Shared<Tree> {
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
        let reft: Ref<'e, Tree> = self.tree.borrow();
        // XXX: Explain the safety here.
        let cursor: &'e mut _ = unsafe { mem::transmute(&mut self.inner) };
        RCursorBorrowMut { reft, cursor }
    }
}

// -------------------------------------------------------------------------------------------------

/// Create a new cursor starting from the given TREE-OR-NODE.
///
/// A cursor allows you to walk a syntax tree more efficiently than is possible
/// using `tsc-get-' functions. It is a mutable object that is always on a certain
/// syntax node, and can be moved imperatively to different nodes.
///
/// If a tree is given, the returned cursor starts on its root node.
#[defun(user_ptr)]
fn make_cursor<'e>(
    tree_or_node: Either<'e, &'e Shared<Tree>, &'e RefCell<RNode>>,
) -> Result<RCursor> {
    match tree_or_node {
        Either::Left(tree, ..) => {
            Ok(RCursor::new(tree.clone(), |tree| tree.walk()))
        }
        Either::Right(node, ..) => {
            let node = node.borrow();
            Ok(RCursor::new(node.clone_tree(), |_| node.borrow().walk()))
        }
    }
}

/// Return CURSOR's current node.
#[defun]
fn current_node(cursor: &RCursor) -> Result<RNode> {
    Ok(RNode::new(cursor.clone_tree(), |_| cursor.borrow().node()))
}

/// Return the field id of CURSOR's current node.
/// Return nil if the current node doesn't have a field.
#[defun]
fn current_field_id(cursor: &RCursor) -> Result<Option<u16>> {
    Ok(cursor.borrow().field_id())
}

/// Return the field associated with CURSOR's current node, as a keyword.
/// Return nil if the current node is not associated with a field.
#[defun]
fn current_field(cursor: &RCursor) -> Result<Option<&'static GlobalRef>> {
    let cursor = cursor.borrow();
    let language: Language = cursor.reft.language().into();
    Ok(cursor.field_id().and_then(|id| language.info().field_name(id)))
}

macro_rules! defun_cursor_walks {
    ($($(#[$meta:meta])* $($lisp_name:literal)? fn $name:ident $( ( $( $param:ident $($into:ident)? : $itype:ty ),* ) )? -> $type:ty)*) => {
        $(
            $(#[$meta])*
            #[defun$((name = $lisp_name))?]
            fn $name(cursor: &mut RCursor, $( $( $param: $itype ),* )? ) -> Result<$type> {
                Ok(cursor.borrow_mut().$name( $( $( $param $(.$into())? ),* )? ))
            }
        )*
    };
}

defun_cursor_walks! {
    /// Move CURSOR to the first child of its current node.
    /// Return t if CURSOR successfully moved, nil if there were no children.
    fn goto_first_child -> bool

    /// Move CURSOR to the parent node of its current node.
    /// Return t if CURSOR successfully moved, nil if it was already on the root node.
    fn goto_parent -> bool

    /// Move CURSOR to the next sibling of its current node.
    /// Return t if CURSOR successfully moved, nil if there was no next sibling node.
    fn goto_next_sibling -> bool

    /// Move CURSOR to the first child that extends beyond the given BYTEPOS.
    /// Return the index of the child node if one was found, nil otherwise.
    "goto-first-child-for-byte" fn goto_first_child_for_byte(bytepos into: BytePos) -> Option<usize>
}

/// Re-initialize CURSOR to start at a different NODE.
#[defun]
fn reset_cursor(cursor: &mut RCursor, node: &RNode) -> Result<()> {
    Ok(cursor.borrow_mut().reset(*node.borrow()))
}
