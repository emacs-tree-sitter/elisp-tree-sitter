#![allow(unused)]

use std::{
    cell::{RefCell, Ref},
    rc::Rc,
    mem,
};

use emacs::{Env, Result, Value, IntoLisp};
use emacs::defun;
use emacs::ResultExt;

use tree_sitter::{Parser, Tree, Point, Language, InputEdit, Node};
use libloading;

emacs::plugin_is_GPL_compatible! {}

#[emacs::module(mod_in_name = false)]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

const NODE_LEN: usize = std::mem::size_of::<Node>();

// XXX
type RawNode = [u8; NODE_LEN];

type SharedTree = Rc<RefCell<Tree>>;

fn shared<T>(t: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(t))
}

struct WrappedNode {
    tree: SharedTree,
    raw: RawNode,
}

impl WrappedNode {
    pub unsafe fn new(tree: &SharedTree, node: Node) -> Self {
        let ptr = (&node as *const Node) as *const RawNode;

//        let s = std::slice::from_raw_parts(ptr as *const u8, NODE_LEN);
//        let mut raw = [0u8; NODE_LEN];
//        raw.copy_from_slice(s);

        // XXX
        let mut raw = mem::MaybeUninit::uninit();
        std::ptr::copy_nonoverlapping(ptr, raw.as_mut_ptr(), 1);
        let raw = raw.assume_init();

        Self { tree: tree.clone(), raw }
    }

    pub fn borrow(&self) -> &Node {
        let ptr = (&self.raw as *const RawNode) as *const Node;
        unsafe { ptr.as_ref() }.unwrap()
    }
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

/// Load language identified by NAME from tree-sitter's bin directory.
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

#[defun(user_ptr(direct))]
fn parse(parser: &mut Parser, read: Value, old_tree: Option<Value>) -> Result<SharedTree> {
    let env = read.env;
    let old_tree = match old_tree {
        Some(v) => {
            Some(v.into_rust::<&SharedTree>()?.try_borrow()?)
        },
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
                    (byte as i64).into_lisp(env).unwrap_or_propagate(),
                    (position.row as i64).into_lisp(env).unwrap_or_propagate(),
                    (position.column as i64).into_lisp(env).unwrap_or_propagate(),
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

#[defun]
fn tree_to_sexp(tree: Value) -> Result<String> {
    let tree: &SharedTree = tree.into_rust()?;
    Ok(tree.try_borrow()?.root_node().to_sexp())
}

#[defun(user_ptr)]
fn root_node(tree: Value) -> Result<WrappedNode> {
    let tree: &SharedTree = tree.into_rust()?;
    Ok(unsafe { WrappedNode::new(tree, tree.try_borrow()?.root_node()) })
}

#[defun]
fn child_count(node: &WrappedNode) -> Result<i64> {
    Ok(node.borrow().child_count() as i64)
}

#[defun]
fn _edit(
    tree: Value,
    start_byte: i64,
    old_end_byte: i64,
    new_end_byte: i64,
    start_row: i64,
    start_column: i64,
    old_end_row: i64,
    old_end_column: i64,
    new_end_row: i64,
    new_end_column: i64,
) -> Result<()> {
    let tree: &SharedTree = tree.into_rust()?;
    let edit = InputEdit {
        start_byte: start_byte as usize,
        old_end_byte: old_end_byte as usize,
        new_end_byte: new_end_byte as usize,
        start_position: Point { row: start_row as usize, column: start_column as usize },
        old_end_position: Point { row: old_end_row as usize, column: old_end_column as usize },
        new_end_position: Point { row: new_end_row as usize, column: new_end_column as usize },
    };
    tree.try_borrow_mut()?.edit(&edit);
    Ok(())
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
    y.map(|t| t.edit(edit));
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
    y.map(|t| t.edit(edit));
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
