use emacs::{defun, Value, Result, Vector};

use tree_sitter::{InputEdit};

use crate::types::{SharedTree, WrappedNode, Range, Point, Language};

// XXX: If we pass a &, #[defun] will assume it's refcell-wrapped. If we pass a Value, we need
// .into_rust() boilerplate. This is a trick to avoid both.
type Tree<'a> = &'a SharedTree;

/// Return the language that was used to parse the syntax TREE.
#[defun(mod_in_name = true)]
fn language(tree: Tree) -> Result<Language> {
    Ok(tree.borrow().language().into())
}

/// Return the sexp representation of the syntax TREE, in a string.
#[defun(mod_in_name = true)]
fn to_sexp(tree: Tree) -> Result<String> {
    Ok(tree.borrow().root_node().to_sexp())
}

/// Return the root node of the syntax TREE.
#[defun(user_ptr)]
fn root_node(tree: Tree) -> Result<WrappedNode> {
    Ok(unsafe { WrappedNode::new(tree.clone(), tree.borrow().root_node()) })
}

/// Edit the syntax TREE to keep it in sync with source code that has been edited.
///
/// You must describe the edit both in terms of byte offsets and in terms of
/// `[row column]' coordinates, using zero-based indexing.
#[defun]
fn edit_tree(
    tree: Tree,
    start_byte: usize,
    old_end_byte: usize,
    new_end_byte: usize,
    start_point: Point,
    old_end_point: Point,
    new_end_point: Point,
) -> Result<()> {
    let edit = InputEdit {
        start_byte,
        old_end_byte,
        new_end_byte,
        start_position: start_point.into(),
        old_end_position: old_end_point.into(),
        new_end_position: new_end_point.into(),
    };
    tree.borrow_mut().edit(&edit);
    Ok(())
}

// TODO: walk_with_properties

/// Compare a new syntax TREE to an OLD-TREE representing the same document.
///
/// This function returns a vector of ranges whose syntactic structure has changed.
///
/// For this to work correctly, OLD-TREE must have been edited such that its ranges
/// match up to the new TREE. Generally, you'll want to call this function right
/// after calling one of the parsing functions, passing in the new tree that was
/// returned and the old tree that was passed as a parameter.
#[defun]
fn changed_ranges<'e>(tree: Value<'e>, old_tree: Tree<'e>) -> Result<Vector<'e>> {
    let env = tree.env;
    let tree = tree.into_rust::<&SharedTree>()?.borrow();
    let other_tree = old_tree.borrow();
    let ranges = tree.changed_ranges(&*other_tree);
    let len = ranges.len();
    let vec = Vector(env.call("make-vector", (len, ()))?);
    for (i, range) in ranges.enumerate() {
        vec.set(i, Range(range))?;
    }
    Ok(vec)
}

/// Create a shallow copy of the syntax TREE.
///
/// This is not very useful currently, as Emacs Lisp threads are subjected to a GIL.
#[defun(user_ptr(direct))]
fn _clone_tree(tree: Tree) -> Result<SharedTree> {
    Ok(tree.clone())
}
