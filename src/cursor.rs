use emacs::{defun, Value, Result, ErrorKind};

use std::{
    cell::RefCell,
    mem,
};

use tree_sitter::TreeCursor;

use crate::types::{SharedTree, WrappedCursor, WrappedNode};
use crate::types::Either;

#[defun(user_ptr)]
fn make_cursor<'e>(tree_or_node: Either<'e, &'e SharedTree, &'e RefCell<WrappedNode>>) -> Result<WrappedCursor> {
    match tree_or_node {
        Either::Left(tree, ..) => {
            Ok(unsafe { WrappedCursor::new(tree.clone(), tree.borrow().walk()) })
        }
        Either::Right(node, ..) => {
            let node = node.borrow();
            Ok(unsafe { WrappedCursor::new( node.tree.clone(), node.inner().walk()) })
        }
    }
}

#[defun]
fn foo(s: Either<String, i64>) -> Result<String> {
    Ok(match s {
        Either::Left(s, ..) => s,
        Either::Right(i, ..) => format!("{}", i),
    })
}

#[defun(user_ptr)]
fn current_node(cursor: &WrappedCursor) -> Result<WrappedNode> {
    Ok(unsafe { WrappedNode::new(cursor.tree.clone(), cursor.inner().node()) })
}

#[defun]
fn current_field_id(cursor: &WrappedCursor) -> Result<Option<u16>> {
    Ok(cursor.inner().field_id())
}

#[defun]
fn current_field_name(cursor: &WrappedCursor) -> Result<Option<&str>> {
    Ok(cursor.inner().field_name())
}

macro_rules! mutator {
    ($($lisp_name:literal)? fn $name:ident $( ( $( $param:ident: $itype:ty ),* ) )? -> $type:ty) => {
        #[defun$((name = $lisp_name))?]
        fn $name(cursor: &mut WrappedCursor, $( $( $param: $itype ),* )? ) -> Result<$type> {
            Ok(cursor.inner_mut().$name( $( $( $param ),* )? ))
        }
    };
}

mutator!(fn goto_first_child -> bool);
mutator!(fn goto_parent -> bool);
mutator!(fn goto_next_sibling -> bool);
mutator!("goto-first-child-for-byte" fn goto_first_child_for_index(index: usize) -> Option<usize>);

#[defun]
fn reset_cursor(cursor: &mut WrappedCursor, node: &WrappedNode) -> Result<()> {
    Ok(cursor.inner_mut().reset(*node.inner()))
}
