use tree_sitter::Tree;

// todo: consider resusing global parser (maybe behind Mutex??)
pub fn parse(text: &str, maybe_tree: Option<&Tree>) -> Tree {
    let mut parser = tree_sitter::Parser::new();
    let language = tree_sitter_r::LANGUAGE;
    parser
        .set_language(&language.into())
        .expect("Error loading R parser");
    parser.parse(text, maybe_tree).unwrap()
}
