use emacs::{defun, Value, Result, ErrorKind};

use std::{
    cell::RefCell,
    mem,
};

use tree_sitter::TreeCursor;

use super::types::{SharedTree, WrappedCursor, WrappedNode};
use crate::types::Either;

#[defun(user_ptr)]
fn walk<'e>(tree_or_node: Either<'e, &'e SharedTree, &'e RefCell<WrappedNode>>) -> Result<WrappedCursor> {
    match tree_or_node {
        Either::Left(tree, ..) => {
            Ok(unsafe { WrappedCursor::new(tree.clone(), tree.try_borrow()?.walk()) })
        }
        Either::Right(node, ..) => {
            let node = node.try_borrow()?;
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
