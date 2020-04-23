macro_rules! impl_pred {}

// In evil's normal-mode, with point after `i`, or `;`, or at the end of the file, eval
// (font-lock-flush). A correct implementation would highlight `i` and `!`.
abc
impl_pred!(foo, bar);
