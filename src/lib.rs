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
mod tree;
mod node;
mod cursor;

emacs::plugin_is_GPL_compatible! {}

#[emacs::module(mod_in_name = false)]
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

/// Load language identified by NAME from tree-sitter's bin directory.
#[defun(user_ptr(direct))]
fn _load_language(name: String) -> Result<Language> {
    // TODO: Expand path properly.
    let path = format!("{}/.tree-sitter/bin/{}.so", std::env::var("HOME").unwrap(), name);
    let lib = libloading::Library::new(path)?;
    let tree_sitter_lang: libloading::Symbol<'_, unsafe extern "C" fn() -> _> =
        unsafe { lib.get(format!("tree_sitter_{}", name).as_bytes())? };
    let language: Language = unsafe { tree_sitter_lang() };
    // Avoid segmentation fault by not unloading the lib, as language is a static struct.
    mem::forget(lib);
    Ok(language)
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
