#![allow(unused)]

use std::{
    cell::{RefCell, Ref},
    rc::Rc,
    mem,
    ops::Deref,
};

use emacs::{defun, Env, Result, Value, IntoLisp, FromLisp, Transfer};
use emacs::ResultExt;

use tree_sitter::{Parser, Tree, Point, InputEdit, Node, TreeCursor};
use libloading;

use self::types::{shared, Language, SharedTree};

mod types;
mod lang;
mod tree;
mod node;
mod cursor;

emacs::plugin_is_GPL_compatible! {}

#[emacs::module(mod_in_name = false, defun_prefix = "tree-sitter")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[defun(user_ptr)]
fn _parser() -> Result<Parser> {
    Ok(Parser::new())
}

#[defun]
fn _set_language(parser: &mut Parser, language: Language) -> Result<()> {
    parser.set_language(*language).unwrap();
    Ok(())
}

#[defun(user_ptr(direct))]
fn parse(parser: &mut Parser, read: Value, old_tree: Option<&SharedTree>) -> Result<SharedTree> {
    let env = read.env;
    let old_tree = match old_tree {
        Some(v) => Some(v.try_borrow()?),
        _ => None,
    };
    let old_tree = match &old_tree {
        Some(r) => Some(&**r),
        _ => None,
    };
    let input = |byte: usize, position: Point| -> String {
        let fragment = env
            .call(
                "funcall",
                &[
                    read,
                    byte.into_lisp(env).unwrap_or_propagate(),
                    position.row.into_lisp(env).unwrap_or_propagate(),
                    position.column.into_lisp(env).unwrap_or_propagate(),
                ],
            )
            .unwrap_or_propagate();
        fragment.into_rust::<String>().unwrap_or_propagate()
    };
    // TODO: Support error cases (None).
    let tree = parser.parse_buffering_with(input, old_tree).unwrap();
    Ok(shared(tree))
}

#[defun]
fn parse_string(parser: &mut Parser, s: String) -> Result<Option<SharedTree>> {
    let tree = parser.parse(s, None);
    Ok(tree.map(shared))
}

// -------------------------------------------------------------------------------------------------
// Scratch

#[defun]
fn edit_(parser: &mut Parser, tree: Option<&RefCell<Tree>>, edit: &InputEdit) -> Result<()> {
    let mut x = tree.map(|r| r.borrow_mut());
    let y = match &mut x {
        None => None,
        Some(r) => Some(&mut **r),
    };
    if let Some(t) = y { t.edit(edit) }
    unimplemented!()
}

#[defun]
fn edit__(parser: &mut Parser, tree: Value, edit: &InputEdit) -> Result<()> {
    let tree: Option<&RefCell<Tree>> = tree.into_rust()?;
    let mut x = tree.map(|r| r.borrow_mut());
    let y = match &mut x {
        None => None,
        Some(r) => Some(&mut **r),
    };
    if let Some(t) = y { t.edit(edit) }
    unimplemented!()
}

//#[defun]
//fn edit__(parser: &mut Parser, tree: Option<&RefCell<Tree>>, edit: &InputEdit) -> Result<()> {
//    let mut x = tree.map(|r| r.borrow_mut());
//    let y = x.map(|r| &mut *r);
//
////    let y = match x {
////        None => None,
////        Some(ref mut x) => Some(&mut **x)
////    };
//    y.map(|t| t.edit(edit));
//    unimplemented!()
//}

#[defun(user_ptr(direct))]
fn parse_string__(env: &Env, parser: &mut Parser, s: String) -> Result<SharedTree> {
    // TODO: Support error cases (None).
    let tree = parser.parse(s, None).unwrap();
    Ok(shared(tree))
}

#[defun]
fn parse_string___(env: &Env, parser: &mut Parser, s: String) -> Result<RefCell<Tree>> {
    // TODO: Support error cases (None).
    let tree = parser.parse(s, None).unwrap();
    Ok(RefCell::new(tree))
}
