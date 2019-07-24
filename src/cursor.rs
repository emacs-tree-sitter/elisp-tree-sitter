use emacs::{defun, Value, Result, ErrorKind};

use std::{cell::RefCell, mem};

use tree_sitter::TreeCursor;

use crate::types::{SharedTree, WrappedCursor, WrappedNode};
use crate::types::Either;

/// Create a new cursor starting from the given TREE-OR-NODE.
///
/// A cursor allows you to walk a syntax tree more efficiently than is possible
/// using `ts-get-' functions. It is a mutable object that is always on a certain
/// syntax node, and can be moved imperatively to different nodes.
///
/// If a tree is given, the returned cursor starts on its root node.
#[defun(user_ptr)]
fn make_cursor<'e>(
    tree_or_node: Either<'e, &'e SharedTree, &'e RefCell<WrappedNode>>,
) -> Result<WrappedCursor> {
    match tree_or_node {
        Either::Left(tree, ..) => {
            Ok(unsafe { WrappedCursor::new(tree.clone(), tree.borrow().walk()) })
        }
        Either::Right(node, ..) => {
            let node = node.borrow();
            Ok(unsafe { WrappedCursor::new(node.tree.clone(), node.inner().walk()) })
        }
    }
}

/// Return CURSOR's current node.
#[defun(user_ptr)]
fn current_node(cursor: &WrappedCursor) -> Result<WrappedNode> {
    Ok(unsafe { WrappedNode::new(cursor.tree.clone(), cursor.inner().node()) })
}

/// Return the field id of CURSOR's current node.
/// Return nil if the current node doesn't have a field.
#[defun]
fn current_field_id(cursor: &WrappedCursor) -> Result<Option<u16>> {
    Ok(cursor.inner().field_id())
}

/// Return the field name of CURSOR's current node.
/// Return nil if the current node doesn't have a field.
#[defun]
fn current_field_name(cursor: &WrappedCursor) -> Result<Option<&str>> {
    Ok(cursor.inner().field_name())
}

macro_rules! defun_cursor_walks {
    ($($(#[$meta:meta])* $($lisp_name:literal)? fn $name:ident $( ( $( $param:ident: $itype:ty ),* ) )? -> $type:ty)*) => {
        $(
            $(#[$meta])*
            #[defun$((name = $lisp_name))?]
            fn $name(cursor: &mut WrappedCursor, $( $( $param: $itype ),* )? ) -> Result<$type> {
                Ok(cursor.inner_mut().$name( $( $( $param ),* )? ))
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
/// Move CURSOR to the first child that extends beyond the given byte offset.
/// Return the index of the child node if one was found, nil otherwise.
"goto-first-child-for-byte" fn goto_first_child_for_index(index: usize) -> Option<usize>
}

/// Re-initialize CURSOR to start at a different NODE.
#[defun]
fn reset_cursor(cursor: &mut WrappedCursor, node: &WrappedNode) -> Result<()> {
    Ok(cursor.inner_mut().reset(*node.inner()))
}
