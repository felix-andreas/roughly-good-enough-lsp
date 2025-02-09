#![allow(warnings)]
use {
    crate::utils,
    ropey::Rope,
    tree_sitter::{Node, Tree},
};

fn format(tree: &Tree, rope: &Rope) -> Option<String> {
    let mut cursor = tree.walk();
    // let mut stack = vec![];
    // todo: initizalize with cap
    let mut result = String::new();
    let mut i = 0;
    loop {
        i += 1;
        if i > 10000 {
            log::error!("reached recursion limit!");
            return None;
        }
        let node = cursor.node();

        let start = match node.kind() {
            "argument" => "argument",
            "arguments" => "arguments",
            "binary_operator" => "binary_operator",
            "braced_expression" => "braced_expression",
            "call" => "call",
            "complex" => "complex",
            "extract_operator" => "extract_operator",
            "float" => "float",
            "for_statement" => "for_statement",
            "function_definition" => "function_definition",
            "if_statement" => "if_statement",
            "integer" => "integer",
            "na" => "na",
            "namespace_operator" => "namespace_operator",
            "parameter" => "parameter",
            "parameters" => "parameters",
            "parenthesized_expression" => "parenthesized_expression",
            "program" => "program",
            "repeat_statement" => "repeat_statement",
            "string" => "string",
            "string_content" => "string_content",
            "subset" => "subset",
            "subset2" => "subset2",
            "unary_operator" => "unary_operator",
            "while_statement" => "while_statement",
            "!" => "!",
            "!=" => "!=",
            "\"" => "\"",
            "$" => "$",
            "&" => "&",
            "&&" => "&&",
            "'" => "'",
            "(" => "(",
            ")" => ")",
            "*" => "*",
            "**" => "**",
            "+" => "+",
            "-" => "-",
            "->" => "->",
            "->>" => "->>",
            "/" => "/",
            ":" => ":",
            "::" => "::",
            ":::" => ":::",
            ":=" => ":=",
            "<" => "<",
            "<-" => "<-",
            "<<-" => "<<-",
            "<=" => "<=",
            "=" => "=",
            "==" => "==",
            ">" => ">",
            ">=" => ">=",
            "?" => "?",
            "@" => "@",
            "L" => "L",
            "NA" => "NA",
            "NA_character_" => "NA_character_",
            "NA_complex_" => "NA_complex_",
            "NA_integer_" => "NA_integer_",
            "NA_real_" => "NA_real_",
            "[" => "[",
            "[[" => "[[",
            "\\" => "\\",
            "]" => "]",
            "]]" => "]]",
            "^" => "^",
            "break" => "break",
            "comma" => "comma",
            "comment" => "comment",
            "dot_dot_i" => "dot_dot_i",
            "dots" => "dots",
            "else" => "else",
            "escape_sequence" => "escape_sequence",
            "false" => "false",
            "for" => "for",
            "function" => "function",
            "i" => "i",
            "identifier" => "identifier",
            "if" => "if",
            "in" => "in",
            "inf" => "inf",
            "nan" => "nan",
            "next" => "next",
            "null" => "null",
            "repeat" => "repeat",
            "return" => "return",
            "special" => "special",
            "true" => "true",
            "while" => "while",
            "{" => "{",
            "|" => "|",
            "|>" => "|>",
            "||" => "||",
            "}" => "}",
            "~" => "~",
            unknown => {
                log::error!("UNKNOWN NODE KIND: {unknown}");
                return None;
            }
        };

        result.push_str(start);

        if cursor.goto_first_child() {
            println!("got to first child");
            continue;
        }

        if cursor.goto_next_sibling() {
            println!("got to first child");
            continue;
        }

        loop {
            if !cursor.goto_parent() {
                println!("no parent");
                dbg!(&result);
                return Some(result);
            }

            if cursor.goto_next_sibling() {
                println!("no next sibling");
                break;
            }
        }
    }
}

fn format_recursive(node: &Node, rope: &Rope) -> Option<String> {
    if !node.is_named() {
        return Some(node.kind().into());
    }

    let get_raw = || rope.byte_slice(node.byte_range()).to_string();

    Some(match node.kind() {
        "argument" => {
            // dbg!(node.child(0));
            // dbg!(node.child(1));
            // dbg!(node.child(2));
            // dbg!(node.child(3));
            let name = node.child(0)?;
            let maybe_value = node.child(2);

            let name = format_recursive(&name, rope)?;
            match maybe_value {
                Some(value) => format!("{name} = {}", format_recursive(&value, rope)?),
                None => name,
            }
        }
        "arguments" => {
            let mut cursor = node.walk();
            node.children_by_field_name("argument", &mut cursor)
                .map(|argument| format_recursive(&argument, rope).unwrap())
                .collect::<Vec<String>>()
                .join(", ")
        }
        "binary_operator" => {
            let lhs = node.child(0)?;
            let op = node.child(1)?;
            let rhs = node.child(2)?;
            format!(
                "{} {} {}",
                format_recursive(&lhs, rope)?,
                op.kind(),
                format_recursive(&rhs, rope)?
            )
        }
        "braced_expression" => {
            let open = node.child(0)?;
            let body = node.child(1)?;
            let close = node.child(2)?;

            let body_fmt = format_recursive(&body, rope)?;
            // we only indent if { and } are not on the same line
            if open.start_position().row == close.end_position().row {
                format!("{{ {body_fmt} }}",)
            } else {
                format!("{{\n{}\n}}", utils::indent_by(2, body_fmt))
            }
        }
        "call" => {
            let function = node.child(0)?;
            let arguments = node.child(1)?;

            let function_fmt = format_recursive(&function, rope)?;
            let arguments_fmt = format_recursive(&arguments, rope)?;
            if arguments.start_position().row == arguments.end_position().row {
                format!("{function_fmt}({arguments_fmt})",)
            } else {
                format!("{function_fmt}(\n{}\n)", utils::indent_by(2, arguments_fmt))
            }
        }
        "complex" => todo!(),
        "extract_operator" => todo!(),
        "float" => get_raw(),
        "for_statement" => todo!(),
        "function_definition" => {
            let name = node.child(0)?;
            let parameters = node.child(1)?;
            let body = node.child(2)?;
            format!(
                "{}({}) {}",
                format_recursive(&name, rope)?,
                format_recursive(&parameters, rope)?,
                format_recursive(&body, rope)?
            )
        }
        "if_statement" => {
            let condition = node.child(2)?;
            let consequence = node.child(4)?;
            let alternative = node.child(6);

            let condition = format_recursive(&condition, rope)?;
            let consequence = format_recursive(&consequence, rope)?;
            match alternative {
                Some(default) => {
                    format!(
                        "if {} {} else {}",
                        condition,
                        consequence,
                        format_recursive(&default, rope)?
                    )
                }
                None => {
                    format!("if {} {}", condition, consequence,)
                }
            }
        }
        "integer" => todo!(),
        "na" => "NA".into(),
        "namespace_operator" => todo!(),
        "parameter" => {
            let name = node.child(0)?;
            let maybe_default = node.child(2);

            let name = format_recursive(&name, rope)?;
            match maybe_default {
                Some(default) => format!("{name} = {}", format_recursive(&default, rope)?),
                None => name,
            }
        }
        "parameters" => {
            let mut cursor = node.walk();
            node.children_by_field_name("parameter", &mut cursor)
                .map(|parameter| format_recursive(&parameter, rope).unwrap())
                .collect::<Vec<String>>()
                .join(", ")
        }
        "parenthesized_expression" => {
            let open = node.child(0)?;
            let body = node.child(1)?;
            let close = node.child(2)?;

            let body_fmt = format_recursive(&body, rope)?;
            // we only indent if { and } are not on the same line
            if open.start_position().row == close.end_position().row {
                format!("({body_fmt})",)
            } else {
                format!("(\n{}\n)", utils::indent_by(2, body_fmt))
            }
        }
        "program" => format_recursive(&node.child(0)?, rope)?,
        "repeat_statement" => todo!(),
        "string" => todo!(),
        "string_content" => todo!(),
        "subset" => todo!(),
        "subset2" => todo!(),
        "unary_operator" => todo!(),
        "while_statement" => todo!(),
        // SIMPLE
        "break" => "break".into(),
        "comma" => ",".into(),
        "comment" => get_raw(),
        "dot_dot_i" => get_raw(),
        "dots" => "...".into(),
        "escape_sequence" => todo!(),
        "false" => "FALSE".into(),
        "identifier" => get_raw(),
        "inf" => "inf".into(),
        "nan" => "nan".into(),
        "next" => "next".into(),
        "null" => "NULL".into(),
        "return" => "return".into(),
        "true" => "TRUE".into(),
        unknown => {
            log::error!("UNKNOWN NODE KIND: {unknown}");
            return None;
        }
    })
}

// fn format_binary_operator(: &Tree, rope: &Rope) -> Option<String> {

// }

// fn traverse(tree: &Tree) -> String {
//     match tree.kind {}
// }

#[cfg(test)]
mod test {
    use {super::*, crate::index, indoc::indoc};

    #[test]
    fn test_format() {
        // let text = indoc! {r#"
        // 	foo <- function(..., a =4, c) {
        // 		if (true) {
        // 			a <- 1
        // 		} else {
        // 			b <- 2
        // 		}
        // 	}
        // "#};
        let text = indoc! {r#"
            (\(x) 2 * x)(a, 2 + 4)
		"#};
        let tree = index::parse(text, None);

        println!(
            "{:#?}",
            index::parse("((1 + 2) + 3)", None).root_node().to_sexp()
        );
        println!("{:#?}", tree.root_node().to_sexp());
        println!(
            "{}",
            format_recursive(&tree.root_node(), &Rope::from_str(text)).unwrap()
        );
    }
}
