#![allow(unused)]

use emacs::{Env, Result, Value, IntoLisp};
use emacs::defun;
use emacs::ResultExt;

use std::mem;

use tree_sitter::{Parser, Tree, Point, Language};
use libloading;
use std::cell::RefCell;
use std::rc::Rc;

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
fn _set_language(parser: &mut Parser, language: &Language) -> Result<()> {
    parser.set_language(*language).unwrap();
    Ok(())
}

#[defun(user_ptr)]
fn _load_language(name: String) -> Result<Language> {
    // TODO: Expand path properly.
    let path = format!("{}/.tree-sitter/bin/{}.so", std::env::var("HOME").unwrap(), name);
    let lib = libloading::Library::new(path)?;
    let tree_sitter_lang: libloading::Symbol<'_, unsafe extern "C" fn() -> Language> =
        unsafe { lib.get(format!("tree_sitter_{}", name).as_bytes())? };
    let lang = unsafe { tree_sitter_lang() };
    // Avoid segmentation fault by not unloading the lib, as language is a static struct.
    mem::forget(lib);
    Ok(lang)
}

#[defun(user_ptr)]
fn parse(parser: &mut Parser, read: Value) -> Result<Tree> {
    let env = read.env;
    let input = |byte: usize, position: Point| -> String {
        let fragment = env
            .call(
                "funcall",
                &[
                    read,
                    (byte as i64).into_lisp(env).unwrap_or_propagate(),
                    (position.row as i64).into_lisp(env).unwrap_or_propagate(),
                    (position.column as i64).into_lisp(env).unwrap_or_propagate(),
                ],
            )
            .unwrap_or_propagate();
        fragment.into_rust::<String>().unwrap_or_propagate()
    };
    let tree = parser.parse_buffering_with(input, None).unwrap();
    Ok(tree)
}

#[defun(user_ptr)]
fn parse_string(parser: &mut Parser, s: String) -> Result<Tree> {
    let tree = parser.parse(s, None).unwrap();
    Ok(tree)
}

#[defun]
fn tree_to_sexp(tree: &Tree) -> Result<String> {
    Ok(tree.root_node().to_sexp())
}
