use emacs::{defun, Value, Result, Vector};

use tree_sitter::{InputEdit, Tree};

use crate::{
    types::{Shared, BytePos, Point, Range},
    lang::Language,
    node::RNode,
};

// XXX: If we pass a &, #[defun] will assume it's refcell-wrapped. If we pass a Value, we need
// .into_rust() boilerplate. This is a trick to avoid both.
pub(crate) type Borrowed<'e, T> = &'e Shared<T>;

impl_pred!(tree_p, &Shared<Tree>);

/// Return the language that was used to parse the syntax TREE.
#[defun(mod_in_name = true)]
fn language(tree: Borrowed<Tree>) -> Result<Language> {
    Ok(tree.borrow().language().clone().into())
}

/// Return the sexp representation of the syntax TREE, in a string.
#[defun(mod_in_name = true)]
fn to_sexp(tree: Borrowed<Tree>) -> Result<String> {
    Ok(tree.borrow().root_node().to_sexp())
}

/// Return the root node of the syntax TREE.
#[defun]
fn root_node(tree: Borrowed<Tree>) -> Result<RNode> {
    Ok(RNode::new(tree.clone(), |tree| tree.root_node()))
}

/// Edit the syntax TREE to keep it in sync with source code that has been edited.
///
/// You must describe the edit both in terms of byte positions and in terms of
/// (LINE-NUMBER . BYTE-COLUMN) coordinates.
///
/// LINE-NUMBER should be the number returned by `line-number-at-pos', which counts
/// from 1.
///
/// BYTE-COLUMN should count from 0, like Emacs's `current-column'. However, unlike
/// that function, it should count bytes, instead of displayed glyphs.
#[defun]
fn edit_tree(
    tree: Borrowed<Tree>,
    start_bytepos: BytePos,
    old_end_bytepos: BytePos,
    new_end_bytepos: BytePos,
    start_point: Point,
    old_end_point: Point,
    new_end_point: Point,
) -> Result<()> {
    let edit = InputEdit {
        start_byte: start_bytepos.into(),
        old_end_byte: old_end_bytepos.into(),
        new_end_byte: new_end_bytepos.into(),
        start_position: start_point.into(),
        old_end_position: old_end_point.into(),
        new_end_position: new_end_point.into(),
    };
    tree.borrow_mut().edit(&edit);
    Ok(())
}

/// Compare an edited OLD-TREE to NEW-TREE, both representing the same document.
///
/// This function returns a sequence of ranges whose syntactic structure has changed.
///
/// For this to work correctly, OLD-TREE must have been edited such that its ranges
/// match up to NEW-TREE. Generally, you'll want to call this function right after
/// calling one of the parsing functions, passing in the old tree that was passed
/// as a parameter and the new tree that was returned.
#[defun]
fn changed_ranges<'e>(old_tree: Value<'e>, new_tree: Borrowed<'e, Tree>) -> Result<Vector<'e>> {
    let env = old_tree.env;
    let old_tree = old_tree.into_rust::<Borrowed<Tree>>()?.borrow();
    let new_tree = new_tree.borrow();
    // TODO: Add a test to show that order is importance.
    let ranges = old_tree.changed_ranges(&*new_tree);
    let vec = env.make_vector(ranges.len(), ())?;
    for (i, range) in ranges.enumerate() {
        vec.set(i, Range(range))?;
    }
    Ok(vec)
}

/// Create a shallow copy of the syntax TREE.
///
/// This is not very useful currently, as Emacs Lisp threads are subjected to a GIL.
#[defun]
fn _clone_tree(tree: Borrowed<Tree>) -> Result<Shared<Tree>> {
    Ok(tree.clone())
}
