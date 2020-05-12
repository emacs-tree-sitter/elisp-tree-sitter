use libloading::{Symbol, Library};
use std::os;

use emacs::{Env, Result, defun};

mod types;
mod lang;
mod parser;
mod tree;
mod node;
mod cursor;
mod query;

emacs::plugin_is_GPL_compatible! {}

#[emacs::module(mod_in_name = false, defun_prefix = "ts")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[defun]
fn hack(_: &Env, path: String, name: String) -> Result<()> {
    // let lib = Library::new("/Applications/EmacsMac.app/Contents/MacOS/Emacs")?;
    // let lib = Library::new("/Applications/EmacsMac.app/Contents/MacOS/bin/emacsclient")?;
    let lib = Library::new(path)?;
    let sym: Symbol<*mut os::raw::c_void> = unsafe { lib.get(name.as_bytes()) }?;
    println!("{} -> {:#?}", name, sym);
    Ok(())
}