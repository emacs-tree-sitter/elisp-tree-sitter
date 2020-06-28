
use emacs::{Env, Result};

mod types;
mod lang;
mod parser;
mod tree;
mod node;
mod cursor;
mod query;

emacs::plugin_is_GPL_compatible! {}

#[emacs::module(mod_in_name = false, defun_prefix = "ts")]
fn init(env: &Env) -> Result<()> {
    env.call("set", (env.intern("tree-sitter-dyn--version")?, option_env!("CARGO_PKG_VERSION")))?;
    Ok(())
}
