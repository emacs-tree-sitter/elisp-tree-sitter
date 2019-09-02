
use emacs::{Env, Result};

mod types;
mod lang;
mod parser;
mod tree;
mod node;
mod cursor;
mod highlight;

emacs::plugin_is_GPL_compatible! {}

#[emacs::module(mod_in_name = false, defun_prefix = "ts")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}
