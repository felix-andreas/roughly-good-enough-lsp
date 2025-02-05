use indoc::indoc;

fn main() {
    let code = indoc! {r#"
		foo <- function() {}
		bar <- \(x) {}
		baz <- 4
		setClass("Person",
			slots = c(
				name = "character",
				age = "numeric"
			)
		)
		setClass(
		"Car", slots = c(
			name = 
		"character"))
		setGeneric("age", function(x) standardGeneric("age"))
		setGeneric("age<-", function(x, value) standardGeneric("age<-"))
		setMethod("age", "Person", function(x) x@age)
		setMethod("age<-", "Person", function(x, value) {
			x@age <- value
			x
		})
	"#};
    let mut parser = tree_sitter::Parser::new();
    let language = tree_sitter_r::LANGUAGE;
    parser
        .set_language(&language.into())
        .expect("Error loading R parser");
    let tree = parser.parse(code, None).unwrap();
    let root = tree.root_node();
    let mut cursor = root.walk();
    dbg!(cursor.goto_first_child());

    loop {
        dbg!(cursor.node());
        dbg!(cursor.node().child_count());
        dbg!(cursor.goto_first_child());
        while cursor.goto_next_sibling() {
            dbg!(cursor.node());
            dbg!(cursor.node().child_count());
        }
        cursor.goto_parent();

        if !cursor.goto_next_sibling() {
            break;
        }
    }
}
