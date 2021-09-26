module.exports = grammar({
  // Set `tree-sitter-hl--extend-region-limit' to an appropriately small number, then call
  // `tree-sitter-hl--highlight-region' on the region from 1 to the position after
  // `compound_assignment_expr', so that the region is not extended.
  rules: {
    compound_assignment_expr: $ => [],
  }
})
