#![allow(warnings)]
use {
    crate::{tree, utils},
    ropey::Rope,
    std::path::PathBuf,
    thiserror::Error,
    tree_sitter::{Node, Tree, TreeCursor},
};

pub fn run(files: &[PathBuf]) {
    if files.is_empty() {
        eprintln!("PLEASE PROVIDE A FILE!");
    }
    files.iter().for_each(|file| {
        eprintln!("SHOW DIFF FOR {file:?}");
        let text = std::fs::read_to_string(file).unwrap();
        let tree = tree::parse(&text, None);
        let rope = Rope::from_str(&text);
        match format(tree.root_node(), &rope) {
            Ok(fmt) => print_diff(&text, &fmt),
            Err(error) => {
                eprintln!("failed to format: {error}");
            }
        }
        eprintln!();
    });
}

// from: https://github.com/mitsuhiko/similar/blob/main/examples/terminal-inline.rs
pub fn print_diff(old: &str, new: &str) {
    use {
        console::{Style, style},
        similar::{ChangeTag, TextDiff},
    };

    struct Line(Option<usize>);

    impl std::fmt::Display for Line {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self.0 {
                None => write!(f, "    "),
                Some(idx) => write!(f, "{:<4}", idx + 1),
            }
        }
    }

    let diff = TextDiff::from_lines(old, new);

    for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
        if idx > 0 {
            println!("{:-^1$}", "-", 80);
        }
        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, s) = match change.tag() {
                    ChangeTag::Delete => ("-", Style::new().red()),
                    ChangeTag::Insert => ("+", Style::new().green()),
                    ChangeTag::Equal => (" ", Style::new().dim()),
                };
                print!(
                    "{}{} |{}",
                    style(Line(change.old_index())).dim(),
                    style(Line(change.new_index())).dim(),
                    s.apply_to(sign).bold(),
                );
                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        print!("{}", s.apply_to(value).underlined().on_black());
                    } else {
                        print!("{}", s.apply_to(value));
                    }
                }
                if change.missing_newline() {
                    println!();
                }
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

fn format(node: Node, rope: &Rope) -> Result<String, FormatError> {
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
    let fmt = |node: Node| format(node, rope);

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
                (Some(name), Some(value)) => format!("{} = {}", fmt(name)?, fmt(value)?),
                (None, Some(value)) => format!("{}", fmt(value)?),
                (Some(name), None) => format!("{}", fmt(name)?),
                (None, None) => format!(""),
            }
        }
        "arguments" => {
            let mut cursor = node.walk();
            let open = field("open")?;
            let close = field("close")?;
            let arguments = fields("argument", &mut cursor, fmt)?;
            let seperator = if open.start_position().row == close.end_position().row {
                ", "
            } else {
                ",\n"
            };
            arguments.join(seperator)
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
            if parameters_fmt.is_empty()
                || parameters.start_position().row == parameters.end_position().row
            {
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
            let open = field("open")?;
            let close = field("close")?;
            let parameters = fields("parameter", &mut cursor, fmt)?;
            let seperator = if open.start_position().row == close.end_position().row {
                ", "
            } else {
                ",\n"
            };
            parameters.join(seperator)
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

#[cfg(test)]
mod test {
    use {super::*, crate::tree, indoc::indoc};
    // TODO: TEST COMMENTS

    fn fmt(text: &str) -> String {
        let tree = tree::parse(text, None);

        // DEBUG
        dbg!(tree.root_node().to_sexp());
        format(tree.root_node(), &Rope::from_str(text)).unwrap()
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
            list  (a = 1, b= 2L ,c =3i  )
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            list  (a = 1,
             b= 2L)
        "#}));
    }

    #[test]
    fn test_function_definition() {
        insta::assert_snapshot!(fmt(indoc! {r#"
            function(a, b= "foo") {}
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            function(a,
            b=  "foo") {}
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            (\(a, b) a *  b)(2, 3)
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
            function(
                a , b=  "foo") {}
        "#}));
        insta::assert_snapshot!(fmt(indoc! {r#"
        	function(
            ) {}
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
        insta::assert_snapshot!(fmt(indoc! {r#"
            list(foo = 1, bar =
            2)@baz
        "#}));
    }
}
