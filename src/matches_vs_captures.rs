use libloading::*;
use tree_sitter::*;
use std::fs;
use emacs::*;

#[defun]
fn _matches_vs_captures() -> Result<()> {
    let lib = Library::new("/Users/ubolonton/.tree-sitter/bin/json.so")?;
    let json: Symbol<'_, unsafe extern "C" fn() -> _> =
        unsafe { lib.get("tree_sitter_json".as_bytes())? };
    let language: Language = unsafe { json() };
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();
    let source = &fs::read_to_string("lisp/test-files/matches-vs-captures.json")?;
    let query_source = fs::read_to_string("langs/queries/json/highlights.scm")?;
    let tree = parser.parse(source, None).unwrap();
    let query = Query::new(language, &query_source).unwrap();
    let capture_names = query.capture_names();
    let (start, end) = (500, 1000);
    let text_callback = |node: Node| {
        &source[node.start_byte()..node.end_byte()]
    };

    println!(".matches");
    let mut cursor = QueryCursor::new();
    cursor.set_byte_range(start, end);
    let matches = cursor.matches(&query, tree.root_node(), text_callback);
    for m in matches {
        for c in m.captures {
            let capture_name = &capture_names[c.index as usize];
            if capture_name == "keyword" {
                println!("  {} . {} : {}",
                         c.node.start_byte(), c.node.end_byte(), text_callback(c.node));
            }
        }
    }

    println!(".captures");
    let mut cursor = QueryCursor::new();
    cursor.set_byte_range(start, end);
    let captures = cursor.captures(&query, tree.root_node(), text_callback);
    for (m, capture_index) in captures {
        let c = m.captures[capture_index];
        let capture_name = &capture_names[c.index as usize];
        if capture_name == "keyword" {
            println!("  {} . {} : {}",
                     c.node.start_byte(), c.node.end_byte(), text_callback(c.node));
        }
    }

    Ok(())
}
