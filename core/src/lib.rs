
use emacs::{Env, Result};

#[macro_use]
mod types;
mod lang;
mod parser;
mod tree;
mod node;
mod cursor;
mod query;
mod buffer;

emacs::plugin_is_GPL_compatible! {}

static mut HAS_FIX_FOR_GC_BUG_31238: bool = false;

static FOO: bool = false;

#[emacs::module(name = "tsc-dyn", defun_prefix = "tsc", mod_in_name = false)]
fn init(env: &Env) -> Result<()> {
    unsafe { HAS_FIX_FOR_GC_BUG_31238 = true };

    unsafe {
        *(&FOO as *const bool as *mut bool) = true;
    }

    env.call("set", (env.intern("tsc-dyn--version")?, option_env!("CARGO_PKG_VERSION")))?;
    node::ERROR.set(env.intern("ERROR")?.make_global_ref()).expect("ERROR was already set");
    env.call("set", (
        env.intern("tsc-direct-access-to-buffer-contents")?,
        buffer::ACCESS_BUFFER_CONTENTS.is_some()
    ))?;
    Ok(())
}
