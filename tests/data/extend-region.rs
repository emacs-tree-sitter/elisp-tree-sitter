macro_rules! impl_pred {}

// In evil's normal-mode, with point after `abc`, or `;`, or at the end of the file, eval
// (font-lock-flush). A correct implementation would highlight both `abc` and `!`. An incorrect
// implementation would highlight neither. Note that this will change once tree-sitter's error cost
// calculation is improved. See https://github.com/tree-sitter/tree-sitter-rust/issues/82.
abc
impl_pred!(foo, bar);
