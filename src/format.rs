#![allow(warnings)]
use {
    crate::utils,
    ropey::Rope,
    thiserror::Error,
    tree_sitter::{Node, Tree, TreeCursor},
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

#[derive(Error, Debug)]
pub enum FormatError {
    #[error("The node of type {kind} has missing children {raw}")]
    Missing { kind: &'static str, raw: String },
    #[error("The node has unknown type {kind}: {raw}")]
    Unknown { kind: &'static str, raw: String },
    #[error("Missing filed {field} for node of kind {kind}")]
    MissingField {
        kind: &'static str,
        field: &'static str,
    },
}

fn format_recursive(node: Node, rope: &Rope) -> Result<String, FormatError> {
    if node.is_missing() {
        let parent = node.parent().unwrap();
        return Err(FormatError::Missing {
            kind: parent.kind(),
            raw: rope.byte_slice(parent.byte_range()).to_string(),
        });
    }

    let kind = node.kind();
    if !node.is_named() {
        return Ok(kind.into());
    }

    let get_raw = || rope.byte_slice(node.byte_range()).to_string();
    let field = |field: &'static str| {
        node.child_by_field_name(field)
            .ok_or_else(|| FormatError::MissingField { kind: kind, field })
    };
    let field_optional = |field: &'static str| node.child_by_field_name(field);
    fn fields(
        field: &'static str,
        cursor: &mut TreeCursor,
        f: impl Fn(Node) -> Result<String, FormatError>,
    ) -> Result<Vec<String>, FormatError> {
        cursor
            .node()
            .children_by_field_name(field, cursor)
            .map(f)
            .collect::<Result<Vec<String>, FormatError>>()
    };
    let fmt = |node: Node| format_recursive(node, rope);

    // DEBUG
    // dbg!(node.child(0));
    // dbg!(node.child(1));
    // dbg!(node.child(2));
    // dbg!(node.child(3));

    Ok(match kind {
        "argument" => {
            let maybe_name = field_optional("name");
            let maybe_value = field_optional("value");
            match (maybe_name, maybe_value) {
                (Some(name), Some(value)) => format!("{name} = {value}"),
                (None, Some(value)) => format!("{value}"),
                (Some(name), None) => format!("{name}"),
                (None, None) => format!(""),
            }
        }
        "arguments" => {
            let mut cursor = node.walk();
            let mut multiline = None;
            let open = field("open")?;
            let close = field("close")?;
            let arguments = node
                .children_by_field_name("argument", &mut cursor)
                .map(|argument| {
                    let (start, end) = (argument.start_position().row, argument.end_position().row);
                    multiline = Some(multiline.map_or((start, end), |(s, e)| {
                        (usize::min(s, start), usize::max(e, end))
                    }));
                    format_recursive(argument, rope).unwrap()
                })
                .collect::<Vec<String>>();
            let seperator = if open.start_position().row == close.end_position().row {
                ", "
            } else {
                ",\n"
            };
            arguments.join(seperator)
            // match multiline {
            //     None => "".into(),
            //     Some((start, end)) if start == end => arguments.join(", "),
            //     _ => arguments.join(",\n"),
            // }
        }
        "binary_operator" => {
            format!(
                "{} {} {}",
                fmt(field("lhs")?)?,
                field("operator")?.kind(),
                fmt(field("rhs")?)?
            )
        }
        "braced_expression" => {
            let mut cursor = node.walk();
            let open = node.child_by_field_name("open").unwrap();
            let close = node.child_by_field_name("close").unwrap();
            let lines = fields("body", &mut cursor, fmt)?;

            // we only indent if { and } are not on the same line
            if open.start_position().row == close.end_position().row {
                if lines.is_empty() {
                    format!("{{}}")
                } else {
                    format!("{{ {} }}", lines.join("; "))
                }
            } else {
                format!("{{\n{}\n}}", utils::indent_by(2, lines.join("\n")))
            }
        }
        "call" => {
            let function = field("function")?;
            let arguments = field("arguments")?;

            let function_fmt = fmt(function)?;
            let arguments_fmt = fmt(arguments)?;
            if arguments.start_position().row == arguments.end_position().row {
                format!("{function_fmt}({arguments_fmt})",)
            } else {
                format!("{function_fmt}(\n{}\n)", utils::indent_by(2, arguments_fmt))
            }
        }
        "complex" => get_raw(),
        "extract_operator" => {
            let lhs = field("lhs")?;
            let op = field("operator")?;
            let maybe_rhs = field_optional("rhs");
            format!("{}{}{}", fmt(lhs)?, op.kind(), match maybe_rhs {
                Some(rhs) => fmt(rhs)?,
                None => "".into(),
            })
        }
        "float" => get_raw(),
        "for_statement" => {
            format!(
                "for {} in {} {}",
                fmt(field("variable")?)?,
                fmt(field("sequence")?)?,
                fmt(field("body")?)?
            )
        }
        "function_definition" => {
            let name = field("name")?;
            let parameters = field("parameters")?;
            let body = field("body")?;

            let name_fmt = fmt(name)?;
            let body_fmt = fmt(body)?;
            let parameters_fmt = fmt(parameters)?;
            if parameters.start_position().row == parameters.end_position().row {
                format!("{}({}) {}", name_fmt, parameters_fmt, body_fmt)
            } else {
                format!(
                    "{}(\n{}\n) {}",
                    name_fmt,
                    utils::indent_by(2, parameters_fmt),
                    body_fmt
                )
            }
        }
        "if_statement" => {
            let condition = field("condition")?;
            let consequence = field("consequence")?;
            let maybe_alternative = field_optional("alternative");

            let condition = fmt(condition)?;
            let consequence = fmt(consequence)?;
            match maybe_alternative {
                Some(alternative) => {
                    format!(
                        "if {} {} else {}",
                        condition,
                        consequence,
                        fmt(alternative)?
                    )
                }
                None => {
                    format!("if {} {}", condition, consequence,)
                }
            }
        }
        "integer" => get_raw(),
        "na" => "NA".into(),
        "namespace_operator" => {
            let lhs = field("lhs")?;
            let op = field("operator")?;
            let maybe_rhs = field_optional("rhs");
            format!("{}{}{}", fmt(lhs)?, op.kind(), match maybe_rhs {
                Some(rhs) => fmt(rhs)?,
                None => "".into(),
            })
        }
        "parameter" => {
            let name = field("name")?;
            let maybe_default = field_optional("default");

            let name = fmt(name)?;
            match maybe_default {
                Some(default) => format!("{name} = {}", fmt(default)?),
                None => name,
            }
        }
        "parameters" => {
            let mut cursor = node.walk();

            let mut multiline = None;
            let parameters = node
                .children_by_field_name("parameter", &mut cursor)
                .map(|parameter| {
                    let (start, end) =
                        (parameter.start_position().row, parameter.end_position().row);
                    multiline = Some(multiline.map_or((start, end), |(s, e)| {
                        (usize::min(s, start), usize::max(e, end))
                    }));
                    fmt(parameter).unwrap()
                })
                .collect::<Vec<String>>();
            match multiline {
                None => "".into(),
                Some((start, end)) if start == end => parameters.join(", "),
                _ => parameters.join(",\n"),
            }
        }
        "parenthesized_expression" => {
            let open = field("open")?;
            let body = field("body")?;
            let close = field("close")?;

            let body_fmt = fmt(body)?;
            // we only indent if { and } are not on the same line
            if open.start_position().row == close.end_position().row {
                format!("({body_fmt})",)
            } else {
                format!("(\n{}\n)", utils::indent_by(2, body_fmt))
            }
        }
        "program" => {
            let mut cursor = node.walk();
            node.children(&mut cursor)
                .map(fmt)
                .collect::<Result<Vec<String>, FormatError>>()?
                .join(if node.start_position().row == node.end_position().row {
                    "; "
                } else {
                    "\n"
                })
        }
        "repeat_statement" => {
            format!("repeat {}", fmt(field("body")?)?)
        }
        "string" => {
            let maybe_string_content = field_optional("content");
            match maybe_string_content {
                Some(string_content) => format!("\"{}\"", fmt(string_content)?),
                None => format!("\"\""),
            }
        }
        "string_content" => get_raw(),
        "subset" => {
            let function = field("function")?;
            let arguments = field("arguments")?;

            let function_fmt = fmt(function)?;
            let arguments_fmt = fmt(arguments)?;
            if arguments.start_position().row == arguments.end_position().row {
                format!("{function_fmt}[{arguments_fmt}]",)
            } else {
                format!("{function_fmt}[\n{}\n]", utils::indent_by(2, arguments_fmt))
            }
        }
        "subset2" => {
            let function = field("function")?;
            let arguments = field("arguments")?;

            let function_fmt = fmt(function)?;
            let arguments_fmt = fmt(arguments)?;
            if arguments.start_position().row == arguments.end_position().row {
                format!("{function_fmt}[[{arguments_fmt}]]",)
            } else {
                format!(
                    "{function_fmt}[[\n{}\n]]",
                    utils::indent_by(2, arguments_fmt)
                )
            }
        }
        "unary_operator" => format!("{}{}", fmt(field("operator")?)?, fmt(field("rhs")?)?),
        "while_statement" => {
            format!(
                "while ({}) {}",
                fmt(field("condition")?)?,
                fmt(field("body")?)?
            )
        }
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
            return Err(FormatError::Unknown {
                kind,
                raw: get_raw(),
            });
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
    // TODO: TEST COMMENTS

    fn fmt(text: &str) -> String {
        let tree = index::parse(text, None);

        // DEBUG
        dbg!(tree.root_node().to_sexp());
        format_recursive(tree.root_node(), &Rope::from_str(text)).unwrap()
    }

    #[test]
    fn test_binary_operator() {
        insta::assert_snapshot!(fmt(indoc! {r#"
            4 + 2
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            4 + 2*3
        "#}));
        // assignments
        insta::assert_snapshot!(fmt(indoc! {r#"
            x<-1
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            x<-1;y<-2
        "#}));
    }

    #[test]
    fn test_braced_expression() {
        insta::assert_snapshot!(fmt(indoc! {r#"
            {}
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            { 1L;2}
        "#}));
        // assignments
        insta::assert_snapshot!(fmt(indoc! {r#"
            {
                foo
                bar
            }
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            {foo;
                bar}
        "#}));
    }

    #[test]
    fn test_call() {
        insta::assert_snapshot!(fmt(indoc! {r#"
            list  (a = 1, b= 2)
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            list  (a = 1,
             b= 2)
        "#}));
    }

    #[test]
    fn test_function_definition() {
        insta::assert_snapshot!(fmt(indoc! {r#"
            function(a, b= "foo") {}
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            function( a
            , b=  "foo") {}
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            (\(a, b) a *  b)(2, 3)
        "#}));
        // TODO: make this work
        insta::assert_snapshot!(fmt(indoc! {r#"
            function(
                a , b=  "foo") {}
        "#}));
    }

    #[test]
    fn extract_operator() {
        insta::assert_snapshot!(fmt(indoc! {r#"
            foo@bar
            foo$bar
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            ( foo+ bar )@baz
        "#}));
        // insta::assert_snapshot!(fmt(indoc! {r#"
        //     list(foo = 1, bar =
        //     2)@baz
        // "#}));
    }
}
