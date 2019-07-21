use emacs::{defun, Value, Result, IntoLisp, Vector};

use std::{
    cell::RefCell,
    rc::Rc,
    mem,
};

use tree_sitter::{InputEdit, Point};

use crate::types::{SharedTree, WrappedNode, Range};

// XXX: If we pass a &, #[defun] will assume it's refcell-wrapped. If we pass a Value, we need
// .into_rust() boilerplate. This is a trick to avoid both.
type Tree<'a> = &'a SharedTree;

#[defun]
fn tree_to_sexp(tree: Tree) -> Result<String> {
    Ok(tree.borrow().root_node().to_sexp())
}

#[defun(user_ptr)]
fn root_node(tree: Tree) -> Result<WrappedNode> {
    Ok(unsafe { WrappedNode::new(tree.clone(), tree.borrow().root_node()) })
}

#[allow(clippy::too_many_arguments)]
#[defun]
fn edit_tree(
    tree: Tree,
    start_byte: usize,
    old_end_byte: usize,
    new_end_byte: usize,
    start_row: usize,
    start_column: usize,
    old_end_row: usize,
    old_end_column: usize,
    new_end_row: usize,
    new_end_column: usize,
) -> Result<()> {
    let edit = InputEdit {
        start_byte,
        old_end_byte,
        new_end_byte,
        start_position: Point { row: start_row, column: start_column },
        old_end_position: Point { row: old_end_row, column: old_end_column },
        new_end_position: Point { row: new_end_row, column: new_end_column },
    };
    tree.borrow_mut().edit(&edit);
    Ok(())
}

// TODO: walk_with_properties

#[defun]
fn changed_ranges<'e>(tree: Value<'e>, other_tree: Tree<'e>) -> Result<Vector<'e>> {
    let env = tree.env;
    let tree = tree.into_rust::<&SharedTree>()?.borrow();
    let other_tree = other_tree.borrow();
    let ranges = tree.changed_ranges(&*other_tree);
    let len = ranges.len();
    let vec = Vector(env.call("make-vector", &[
        len.into_lisp(env)?,
        env.intern("nil")?
    ])?);
    for (i, range) in ranges.into_iter().enumerate() {
        vec.set(i, Range(range))?;
    }
    Ok(vec)
}
