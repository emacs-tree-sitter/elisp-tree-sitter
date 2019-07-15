use emacs::{defun, Result};

use std::mem;

use tree_sitter::Node;

use super::types::{SharedTree, WrappedNode};

#[defun]
fn child_count(node: &WrappedNode) -> Result<i64> {
    Ok(node.inner().child_count() as i64)
}

#[defun]
fn kind_id(node: &WrappedNode) -> Result<i64> {
    Ok(node.inner().kind_id().into())
}
