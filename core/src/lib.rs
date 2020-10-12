
use emacs::{Env, Result};

#[macro_use]
mod types;
mod lang;
mod parser;
mod tree;
mod node;
mod cursor;
mod query;

emacs::plugin_is_GPL_compatible! {}

#[emacs::module(name = "lts-dyn", defun_prefix = "lts", mod_in_name = false)]
fn init(env: &Env) -> Result<()> {
    env.call("set", (env.intern("lts-dyn--version")?, option_env!("CARGO_PKG_VERSION")))?;
    node::ERROR.set(env.intern("ERROR")?.make_global_ref()).expect("ERROR was already set");
    Ok(())
}
