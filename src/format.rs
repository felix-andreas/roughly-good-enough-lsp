use {
    crate::{
        cli, tree,
        utils::{self},
    },
    console::style,
    ignore::Walk,
    ropey::Rope,
    std::path::PathBuf,
    thiserror::Error,
    tree_sitter::Node,
};

pub fn run(maybe_files: Option<&[PathBuf]>, check: bool, diff: bool) -> bool {
    let root: Vec<PathBuf> = vec![".".into()];
    let files = maybe_files.unwrap_or(&root);

    let mut n_files = 0;
    let mut n_unformatted = 0;
    let mut n_errors = 0;
    files
        .iter()
        .flat_map(Walk::new)
        .filter_map(|e| e.ok())
        .map(|entry| entry.into_path())
        .filter(|path| {
            path.extension()
                .map(|ext| ext == "R" || ext == "r")
                .unwrap_or(false)
        })
        .for_each(|path| {
            n_files += 1;
            let old = std::fs::read_to_string(&path).unwrap();
            let tree = tree::parse(&old, None);
            let rope = Rope::from_str(&old);
            let new = match format(tree.root_node(), &rope) {
                Ok(new) => new,
                Err(_) => {
                    n_errors += 1;
                    cli::error(&format!("failed to format: {}", path.display()));
                    return;
                }
            };
            if old != new {
                n_unformatted += 1;
                if diff {
                    eprintln!("Diff in {}:", path.display());
                    print_diff(&old, &new);
                } else if check {
                    eprintln!("Would reformat: {}", style(path.display()).bold());
                } else if std::fs::write(&path, new).is_err() {
                    cli::error(&format!("failed to write to file: {}", path.display()));
                }
            }
        });

    if n_files == 0 {
        cli::warning("No R files found under the given path(s)");
        return true;
    }

    let (first, second) = if check {
        ("would be reformatted", "already formatted")
    } else {
        ("reformatted", "left unchanged")
    };

    eprintln!(
        "{} file{} {first}, {} file{} {second}",
        n_unformatted,
        if n_unformatted == 1 { "" } else { "s" },
        n_files - n_unformatted,
        if n_files - n_unformatted == 1 {
            ""
        } else {
            "s"
        },
    );

    n_unformatted == 0 && n_errors == 0
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
            eprintln!("{:-^1$}", "-", 80);
        }
        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, s) = match change.tag() {
                    ChangeTag::Delete => ("-", Style::new().red()),
                    ChangeTag::Insert => ("+", Style::new().green()),
                    ChangeTag::Equal => (" ", Style::new().dim()),
                };
                eprint!(
                    "{}{} |{}",
                    style(Line(change.old_index())).dim(),
                    style(Line(change.new_index())).dim(),
                    s.apply_to(sign).bold(),
                );
                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        eprint!("{}", s.apply_to(value).underlined().on_black());
                    } else {
                        eprint!("{}", s.apply_to(value));
                    }
                }
                if change.missing_newline() {
                    eprintln!();
                }
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum FormatError {
    #[error("Failed to parse node {raw} with parent of kind {kind}")]
    SyntaxError { kind: &'static str, raw: String },
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

pub fn format(node: Node, rope: &Rope) -> Result<String, FormatError> {
    format_rec(node, rope, false)
}

fn format_rec(node: Node, rope: &Rope, make_multiline: bool) -> Result<String, FormatError> {
    const INDENT_BY: usize = 2;

    let kind = node.kind();
    let fmt = |node: Node| format_rec(node, rope, false);
    let fmt_multiline = |node: Node, make_multiline: bool| format_rec(node, rope, make_multiline);
    let field = |field: &'static str| {
        node.child_by_field_name(field)
            .ok_or(FormatError::MissingField { kind, field })
    };
    let field_optional = |field: &'static str| node.child_by_field_name(field);
    let get_raw = || rope.byte_slice(node.byte_range()).to_string();
    let wrap_with_braces = |node: Node| -> Result<String, FormatError> {
        Ok(format!(
            "{{{}}}",
            utils::indent_by_with_newlines(INDENT_BY, fmt(node)?)
        ))
    };

    // note: currently we don't traverse open&close -> they never reach these conditions
    if node.is_error() {
        return Err(FormatError::SyntaxError {
            kind: node.parent().unwrap().kind(),
            raw: get_raw(),
        });
    }

    if node.is_missing() {
        return Err(FormatError::Missing {
            kind: node.parent().unwrap().kind(),
            raw: get_raw(),
        });
    }

    if node.kind() == "comment" {
        let raw = get_raw();
        // reformat comments like #foo to # foo but keep #' foo
        let prefix = raw
            .chars()
            .take_while(|c| c.is_ascii_punctuation())
            .collect::<String>();
        let content = raw
            .chars()
            .skip_while(|c| c.is_ascii_punctuation())
            .collect::<String>();
        let content = content.trim_end();
        let sep = if content.starts_with(char::is_whitespace) || content.is_empty() {
            ""
        } else {
            " "
        };
        return Ok(format!("{prefix}{sep}{content}"));
    }

    if node.is_extra() {
        log::warn!("node of kind {} is extra but is not a comment", node.kind());
    }

    if !node.is_named() {
        return Ok(get_raw());
    }

    let mut handles_comments = false;

    let result = match kind {
        "argument" => {
            let maybe_name = field_optional("name");
            let maybe_value = field_optional("value");

            // support the switch fallthrough
            let mut cursor = node.walk();
            let has_equal = node.children(&mut cursor).any(|node| node.kind() == "=");

            match (maybe_name, maybe_value) {
                (Some(name), Some(value)) => format!("{} = {}", fmt(name)?, fmt(value)?),
                (None, Some(value)) => (fmt(value)?).to_string(),
                (Some(name), None) if has_equal => format!("{} = ", fmt(name)?),
                (Some(name), None) => (fmt(name)?).to_string(),
                (None, None) => String::new(),
            }
        }
        "arguments" => {
            handles_comments = true;
            let mut cursor = node.walk();
            let open = field("open")?;
            let close = field("close")?;
            let mut maybe_prev_node = None;
            let is_multiline = open.start_position().row != close.end_position().row;

            let mut is_first_arg = true;
            node.children(&mut cursor)
                .skip(1)
                .take(node.child_count() - 2)
                .map(|child| {
                    let tmp = fmt(child)?;
                    let prev_node_local = maybe_prev_node;
                    maybe_prev_node = Some(child);
                    if child.kind() == "comment" {
                        return Ok(match prev_node_local {
                            Some(prev_node)
                                if prev_node.end_position().row == child.start_position().row =>
                            {
                                format!(" {tmp}")
                            }
                            Some(_) => format!("\n{tmp}"),
                            None => tmp.to_string(),
                        });
                    }
                    if child.kind() == "comma" {
                        is_first_arg = false;
                        return Ok(if prev_node_local
                            .map(|node| node.kind() == "comment")
                            .unwrap_or(false)
                        {
                            "\n,"
                        } else {
                            ","
                        }
                        .to_string());
                    }
                    let result = format!(
                        "{}{}",
                        if is_first_arg {
                            if prev_node_local
                                .map(|node| node.kind() == "comment")
                                .unwrap_or(false)
                            {
                                "\n"
                            } else {
                                ""
                            }
                        } else if is_multiline {
                            "\n"
                        } else {
                            " "
                        },
                        tmp
                    );
                    is_first_arg = false;
                    Ok(result)
                })
                .collect::<Result<String, FormatError>>()?
        }
        "binary_operator" => {
            handles_comments = true;
            let mut cursor = node.walk();
            let comments = node
                .children(&mut cursor)
                .filter(|node| node.kind() == "comment")
                .map(fmt)
                .collect::<Result<Vec<String>, FormatError>>()?
                .join(&format!("\n{}", " ".repeat(INDENT_BY)));

            let lhs = field("lhs")?;
            let operator = field("operator")?;
            let rhs = field("rhs")?;
            let is_multiline = lhs.end_position().row != rhs.start_position().row;
            let has_spacing = operator.kind() == ":";
            format!(
                "{}{}{}{}{}{}{}",
                fmt(lhs)?,
                if has_spacing { "" } else { " " },
                fmt(operator)?,
                if comments.is_empty() { "" } else { " " },
                comments,
                if is_multiline {
                    "\n"
                } else if has_spacing {
                    ""
                } else {
                    " "
                },
                if is_multiline {
                    utils::indent_by(INDENT_BY, fmt(rhs)?)
                } else {
                    fmt(rhs)?
                }
            )
        }
        "braced_expression" => {
            handles_comments = true;
            let mut cursor = node.walk();
            let is_multiline = node.start_position().row != node.end_position().row;

            let mut prev_end = None;
            let lines = node
                .children(&mut cursor)
                .skip(1)
                .take(node.child_count() - 2)
                .map(|child| {
                    let line = fmt(child)?;
                    let result = match prev_end {
                        Some(prev_end)
                            if child.kind() == "comment"
                                && prev_end == child.end_position().row =>
                        {
                            format!(" {}", line)
                        }
                        Some(prev_end) => {
                            format!(
                                "{}{}",
                                "\n".repeat(usize::clamp(
                                    child.start_position().row - prev_end,
                                    1,
                                    2
                                )),
                                line
                            )
                        }
                        None => line,
                    };
                    prev_end = Some(child.end_position().row);
                    Ok(result)
                })
                .collect::<Result<Vec<String>, FormatError>>()?;

            if lines.is_empty() {
                "{}".to_string()
            } else if is_multiline || make_multiline || lines.len() > 1 {
                format!(
                    "{{{}}}",
                    utils::indent_by_with_newlines(INDENT_BY, lines.join(""))
                )
            } else {
                format!("{{ {} }}", lines.join(""))
            }
        }
        "call" => {
            let function = field("function")?;
            let arguments = field("arguments")?;

            let function_fmt = fmt(function)?;
            let arguments_fmt = fmt(arguments)?;
            format!(
                "{function_fmt}({})",
                if arguments.start_position().row == arguments.end_position().row {
                    arguments_fmt
                } else {
                    utils::indent_by_with_newlines(INDENT_BY, arguments_fmt)
                }
            )
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
            let body = field("body")?;
            format!(
                "for ({} in {}) {}",
                fmt(field("variable")?)?,
                fmt(field("sequence")?)?,
                if body.kind() != "braced_expression" {
                    wrap_with_braces(body)?
                } else {
                    fmt_multiline(body, true)?
                },
            )
        }
        "function_definition" => {
            let name = field("name")?;
            let parameters = field("parameters")?;
            let body = field("body")?;
            let is_multiline = node.start_position().row != node.end_position().row;

            let parameters_fmt = fmt(parameters)?;
            format!(
                "{}({}) {}",
                fmt(name)?,
                if parameters_fmt.is_empty()
                    || parameters.start_position().row == parameters.end_position().row
                {
                    parameters_fmt
                } else {
                    utils::indent_by_with_newlines(INDENT_BY, parameters_fmt)
                },
                if is_multiline && body.kind() != "braced_expression" {
                    wrap_with_braces(body)?
                } else {
                    fmt_multiline(body, is_multiline)?
                },
            )
        }
        "if_statement" => {
            let condition = field("condition")?;
            let consequence = field("consequence")?;
            let maybe_alternative = field_optional("alternative");
            let is_multiline = node.start_position().row != node.end_position().row;
            let is_multiline_condition =
                condition.start_position().row != condition.end_position().row;

            format!(
                "if ({}) {}{}{}",
                if is_multiline_condition {
                    utils::indent_by_with_newlines(INDENT_BY, fmt(condition)?)
                } else {
                    fmt(condition)?
                },
                if is_multiline && consequence.kind() != "braced_expression" {
                    wrap_with_braces(consequence)?
                } else {
                    fmt_multiline(consequence, is_multiline)?
                },
                match maybe_alternative {
                    Some(_) => " else ",
                    None => "",
                },
                match maybe_alternative {
                    Some(alternative) =>
                        if is_multiline
                            && alternative.kind() != "braced_expression"
                            && alternative.kind() != "if_statement"
                        {
                            wrap_with_braces(alternative)?
                        } else {
                            fmt_multiline(alternative, is_multiline)?
                        },
                    None => "".into(),
                }
            )
        }
        "integer" => get_raw(),
        "na" => get_raw(),
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
            handles_comments = true;
            let open = field("open")?;
            let close = field("close")?;
            let is_multiline = open.start_position().row != close.end_position().row;

            let mut maybe_prev_node = None;
            let mut is_first_param = true;
            let mut cursor = node.walk();
            node.children(&mut cursor)
                .skip(1)
                .take(node.child_count() - 2)
                .map(|child| {
                    let tmp = fmt(child)?;
                    let prev_node_local = maybe_prev_node;
                    maybe_prev_node = Some(child);
                    if child.kind() == "comment" {
                        return Ok(match prev_node_local {
                            Some(prev_node)
                                if prev_node.end_position().row == child.start_position().row =>
                            {
                                format!(" {tmp}")
                            }
                            Some(_) => format!("\n{tmp}"),
                            None => tmp.to_string(),
                        });
                    }
                    if child.kind() == "comma" {
                        is_first_param = false;
                        return Ok(if prev_node_local
                            .map(|node| node.kind() == "comment")
                            .unwrap_or(false)
                        {
                            "\n,"
                        } else {
                            ","
                        }
                        .to_string());
                    }
                    let result = format!(
                        "{}{}",
                        if is_first_param {
                            if prev_node_local
                                .map(|node| node.kind() == "comment")
                                .unwrap_or(false)
                            {
                                "\n"
                            } else {
                                ""
                            }
                        } else if is_multiline {
                            "\n"
                        } else {
                            " "
                        },
                        tmp
                    );
                    is_first_param = false;
                    Ok(result)
                })
                .collect::<Result<String, FormatError>>()?
        }
        "parenthesized_expression" => {
            handles_comments = true;
            let mut cursor = node.walk();
            let open = node.child_by_field_name("open").unwrap();
            let close = node.child_by_field_name("close").unwrap();

            let mut prev_end = None;
            let lines = node
                .children(&mut cursor)
                .skip(1)
                .take(node.child_count() - 2)
                .map(|child| {
                    let line = fmt(child)?;
                    let result = match prev_end {
                        Some(prev_end)
                            if child.kind() == "comment"
                                && prev_end == child.end_position().row =>
                        {
                            format!(" {}", line)
                        }
                        Some(prev_end) => {
                            format!(
                                "{}{}",
                                "\n".repeat(usize::min(2, child.start_position().row - prev_end)),
                                line
                            )
                        }
                        None => line,
                    };
                    prev_end = Some(child.end_position().row);
                    Ok(result)
                })
                .collect::<Result<Vec<String>, FormatError>>()?;

            if lines.is_empty() {
                "()".to_string()
            } else {
                format!(
                    "({})",
                    if open.start_position().row == close.end_position().row {
                        lines.join("")
                    } else {
                        utils::indent_by_with_newlines(INDENT_BY, lines.join(""))
                    }
                )
            }
        }
        "program" => {
            handles_comments = true;
            let mut cursor = node.walk();
            let mut prev_end = None;
            node.children(&mut cursor)
                .map(|child| {
                    let tmp = fmt(child)?;
                    let result = match prev_end {
                        Some(prev_end)
                            if child.kind() == "comment"
                                && prev_end == child.end_position().row =>
                        {
                            format!(" {}", tmp)
                        }
                        Some(prev_end) => {
                            format!(
                                "{}{}",
                                "\n".repeat(usize::clamp(
                                    child.start_position().row - prev_end,
                                    1,
                                    2
                                )),
                                tmp
                            )
                        }
                        None => tmp,
                    };
                    prev_end = Some(child.end_position().row);
                    Ok(result)
                })
                .chain(std::iter::once(Ok("\n".into())))
                .collect::<Result<String, FormatError>>()?
        }
        "repeat_statement" => {
            format!("repeat {}", fmt_multiline(field("body")?, true)?)
        }
        "string" => {
            let maybe_string_content = field_optional("content");
            match maybe_string_content {
                Some(string_content) => {
                    let content = fmt(string_content)?;
                    if content.contains("\"") {
                        get_raw()
                    } else {
                        format!("\"{}\"", fmt(string_content)?)
                    }
                }
                None => "\"\"".to_string(),
            }
        }
        "string_content" => get_raw(),
        "subset" => {
            let function = field("function")?;
            let arguments = field("arguments")?;

            let function_fmt = fmt(function)?;
            let arguments_fmt = fmt(arguments)?;
            format!(
                "{function_fmt}[{}]",
                if arguments.start_position().row == arguments.end_position().row {
                    arguments_fmt
                } else {
                    utils::indent_by_with_newlines(INDENT_BY, arguments_fmt)
                }
            )
        }
        "subset2" => {
            let function = field("function")?;
            let arguments = field("arguments")?;

            let function_fmt = fmt(function)?;
            let arguments_fmt = fmt(arguments)?;
            format!(
                "{function_fmt}[[{}]]",
                if arguments.start_position().row == arguments.end_position().row {
                    arguments_fmt
                } else {
                    utils::indent_by_with_newlines(INDENT_BY, arguments_fmt)
                }
            )
        }
        "unary_operator" => {
            let operator = field("operator")?;
            let spacing = if operator.kind() == "~" { " " } else { "" };
            format!("{}{spacing}{}", fmt(operator)?, fmt(field("rhs")?)?)
        }
        "while_statement" => {
            let condition = field("condition")?;
            let body = field("body")?;
            let is_multiline_condition =
                condition.start_position().row != condition.end_position().row;

            format!(
                "while ({}) {}",
                if is_multiline_condition {
                    utils::indent_by_with_newlines(INDENT_BY, fmt(condition)?)
                } else {
                    fmt(condition)?
                },
                if body.kind() == "braced_expression" {
                    fmt_multiline(body, true)?
                } else {
                    wrap_with_braces(body)?
                },
            )
        }
        // SIMPLE
        "break" => "break".into(),
        "comma" => ",".into(),
        "comment" => get_raw(),
        "dot_dot_i" => get_raw(),
        "dots" => "...".into(),
        "escape_sequence" => get_raw(),
        "false" => "FALSE".into(),
        "identifier" => get_raw(),
        "inf" => "Inf".into(),
        "nan" => "NaN".into(),
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
    };

    Ok(if handles_comments {
        result
    } else {
        let mut cursor = node.walk();
        node.children(&mut cursor)
            .filter(|node| node.kind() == "comment")
            .map(fmt)
            .chain(std::iter::once(Ok(result)))
            .collect::<Result<Vec<String>, FormatError>>()?
            .join("\n")
    })
}

#[cfg(test)]
mod test {
    use {super::*, crate::tree, indoc::indoc};

    macro_rules! assert_fmt {
        ($input:expr) => {
            insta::assert_snapshot!(format_str(indoc! {$input}).unwrap());
        };
    }

    fn format_str(text: &str) -> Result<String, FormatError> {
        let tree = tree::parse(text, None);

        // DEBUG
        // dbg!(tree.root_node().to_sexp());
        // eprintln!("{}", utils::format_node(&tree.root_node()));
        format(tree.root_node(), &Rope::from_str(text))
    }

    #[test]
    fn binary_operator() {
        assert_fmt! {r#"
            4 + 2
            4 + 2*3
            4 +
                3 +
                    2 +
                        1
        "#};
        assert_fmt! {r#"
            1:10
        "#};
        // assignments
        assert_fmt! {r#"
            x<-1
        "#};
        assert_fmt! {r#"
            x<-1;y<-2
        "#};
        assert_fmt! {r#"
            foo |>
                bar
        "#};
        assert_fmt! {r#"
            foo %>% bar %>%
                    baz
        "#};
        assert_fmt! {r#"
            foo |> # foo
            #bar
            bar |>#bar
            baz |>
                qux
        "#};
    }

    #[test]
    fn braced_expression() {
        assert_fmt! {r#"
            {}
            {
            }
        "#};
        assert_fmt! {r#"
            { 1L;2}
        "#};
        assert_fmt! {r#"
            {
                foo
                bar
            }
        "#};
        assert_fmt! {r#"
            {foo;
                bar}
        "#};
        assert_fmt! {r#"
            {
                a # foo
            }
        "#};
        assert_fmt! {r#"
        	# foo
            { # bar
            #baz
            a
            # qux
            }
        "#};
        assert_fmt! {r#"
            {
                    # single line
                a # next


                # multi
                # line
                # comment
                b
            }
        "#};
        assert_fmt! {r#"
            {
                a

                b
            }
        "#};
    }

    #[test]
    fn call() {
        assert_fmt! {r#"
            list  (a = 1, b= 2L ,c =3i  )
        "#};
        assert_fmt! {r#"
            list  (a = 1,
             b= 2L)
        "#};
        assert_fmt! {r#"
            list  (
                # foo
                a = 1, #bar
                b= 2L) #baz
        "#};
        assert_fmt! {r#"
            foo  ( #foo
                # foo
                f
                # foo bar
                #   foo bar
                #   foo bar
                a = 1, #bar


                b= 2L) #baz

                # foo
        "#};
    }

    #[test]
    fn extract_operator() {
        assert_fmt! {r#"
            foo@bar
            foo$bar
            foo @ bar
            foo$  bar
        "#};
        assert_fmt! {r#"
            ( foo+ bar )@baz
        "#};
        assert_fmt! {r#"
            list(foo = 1, bar =
            2)@baz
        "#};
    }

    #[test]
    fn for_statement() {
        assert_fmt! {r#"
        	for (x in 1:2) {
                print(x)
            }
        	for (x in c(1, # foo
            2)) {
                print(x)
            }
            for (x in 1:3) #foo
            {
                x
            }
            for (x in 1:3)
            #foo
            {
                x
            }
        "#};

        assert_fmt! {r#"
            for (x in foo(
            bar)) baz
        "#};

        assert_fmt! {r#"
            for (x in foo( bar)) { baz }
            for (x in
            foo( bar)) { baz }
        "#};
    }

    #[test]
    fn function_definition() {
        assert_fmt! {r#"
            function(a, b= "foo") {}
        "#};
        assert_fmt! {r#"
            function(a,
            b=  "foo") {}
        "#};
        assert_fmt! {r#"
            (\(a, b) a *  b)(2, 3)
        "#};
        assert_fmt! {r#"
            function(
                a , b=  "foo") {}
        "#};
        assert_fmt! {r#"
        	function(
            ) {}
        "#};
        assert_fmt! {r#"
            function(
            	# foo
                foo, #foo
                #bar
                #  bar
                bar = 3 #bar
            ) {}
        "#};
        assert_fmt! {r#"
            function (a, 
            b) baz
        "#};
        assert_fmt! {r#"
            function (a,
            b) {baz}
        "#};
    }

    #[test]
    fn if_statement() {
        assert_fmt! {r#"
            if (a>b) {
                1
            } else {
            "foo"
            }
            x <- if (T) 4
            if (TRUE) #foo
            10

            if (foo <bar) {
            lala
            1} else if (1 >2) {2
            } else {3}
        "#};

        // multiline condition
        assert_fmt! {r#"
            if (any(
            sapply(foo)
            )) stop("")
        "#};

        // if else if else
        assert_fmt! {r#"
            if (
                TRUE
            ) {foo
             } else if (TRUE) {bar
            } else baz
        "#};

        assert_fmt! {r#"
            if (foo) {bar}
            if (foo) {bar} else {baz}
            if (foo) bar else {baz}
            if (
            foo) {bar}
            if (
            foo) {bar}
            if (foo)
                {bar}
        "#};
    }

    #[test]
    fn namespace_operator() {
        assert_fmt! {r#"
            foo::
            foo::bar
            foo::bar(1)
        "#}
    }

    #[test]
    fn parenthesized_expression() {
        assert_fmt! {r#"
            (1 +2 )
            (
            #foo
            1 +2 )
            x <- ( # com
            5
            )
            (
                a #foo
            )
            (
            	#foo
                a #bar
                #baz
            )
            ( #foo
                a #bar
            #baz
            )
            ( a #foo
            )
        "#};
    }

    #[test]
    fn program() {
        assert_fmt! {r#"
            # A simple comment
            x <- 1 + 2
            y <- x * 3
            z <- if (y > 5) {
            "greater"
            } else {
            "lesser"
            }
            result <- function(a, b = 2) {
            return(a + b)
            }
            list <- list(a = 1, b = 2, c = 3)
            for (i in 1:10) {
            print(i)
            }
            while (x < 10) {
            x <- x + 1
            }
            repeat {
            x <- x - 1
            if (x == 0) break
            }
            foo <- function(x) x^2
            bar <- foo(3)
            baz <- c(1, 2, 3)
            qux <- baz[1]
            quux <- baz[[1]]
            corge <- list(a = 1, b = 2)
            grault <- corge$a
            garply <- corge[["b"]]
            waldo <- TRUE
            fred <- FALSE
            plugh <- NULL
            xyzzy <- Inf
            thud <- NaN
        "#};
    }

    #[test]
    fn repeat_statement() {
        assert_fmt! {r#"
            repeat {
                print("Hello, world!")
            }
            repeat {
            }
            repeat { #foo
            }
            repeat #foo
            { }
        "#};
    }

    #[test]
    fn subset() {
        assert_fmt! {r#"
            foo[x]
            foo[x,   y]
            foo[1, 2 ]
            foo[x=1  , ,y  =3,4]
            foo[  ]
            foo[ , ]
            foo[, ,]
            foo[ x, ]
            foo[x,,]
            foo[,x ]
            foo[,,x]
            foo[x, ,y]
            foo[,,x,,y,,]
            foo[ #foo
            1,2,3
            ]
            foo[ #foo
            #bar
            1,2,3
            #baz
            ]
            foo[ #foo
            ,,a
            ]
        "#};
    }

    #[test]
    fn subset2() {
        assert_fmt! {r#"
            foo[[x ]]
            foo[[x,   y]]
            foo[[1, 2 ]]
            foo[[x=1  , ,y  =3,4]]
            foo[[  ]]
            foo[[ , ]]
            foo[[, ,]]
            foo[[ x, ]]
            foo[[x,,]]
            foo[[,x ]]
            foo[[,,x]]
            foo[[x, ,y]]
            foo[[,,x,,y,,]]
            foo[[ #foo
            1,2,3
            ]]
            foo[[ #foo
            #bar
            1,2,3
            #baz
            ]]
            foo[[ #foo
            ,,a
            ]]
        "#};
    }

    #[test]
    fn unary_operator() {
        assert_fmt! {r#"
            !a
            +a
            -a
            foo(!a, +   b)
            foo(- a , bar)
            !
            a
            -  b
            -42
            + 42
            !TRUE
            ~foo
            -foo + bar
            -  (foo + bar)
            ! foo && bar
            ~  foo | bar
        "#};
    }

    #[test]
    fn while_statement() {
        assert_fmt! {r#"
            while(x < 10)
            { print(x)
                x <- x + 1
            }
            while (x < 10) { #foo
                print(x) }
            while (x < 10)
            #foo
            {
                print(x)
            }
        "#};

        assert_fmt! {r#"
            while(foo(
            bar)) baz
        "#};

        assert_fmt! {r#"
            while(foo(
            bar)) {baz}
        "#};
    }

    // EDGE CASES
    #[test]
    fn comments_trailing_whitespace() {
        assert_eq!(
            "#'\n#' foo\nx <- 1\n",
            format_str("#' \n#' foo\nx<-1").unwrap(),
        )
    }

    #[test]
    fn switch_fallthrough() {
        assert_fmt! {r#"
            switch(foo,
                x = 1,
                "y" = 2,
                z = ,
                3
            )
        "#};
    }

    #[test]
    fn comment_formatting() {
        assert_fmt! {r#"
           #foo
           ##foo
           ## foo
           ### foo
           # # foo
           #    foo
           #'foo
           #
           # #
        "#};
    }

    #[test]
    fn semicolons_in_function() {
        assert_fmt! {r#"
            function(x) {
                names(foo[[x]]) <- bar; foo[x]
            }
        "#};
    }

    // LIBRARIES WITH SPECIAL FORMATTING
    #[test]
    fn data_table() {
        assert_fmt! {r#"
            ans <- flights[, .(arr_delay, dep_delay)]
            DT[,.(V4.Sum=sum(V4)), by=V1][order(-V1)]
            DT[,':='(V1=round(exp(V1),2), V2=LETTERS[4:6])][]
            DT[,lapply(.SD,sum),by=V2, # comment
                .SDcols=c("V3","V4")]
        "#};
    }

    #[test]
    fn dplyr() {
        assert_fmt! {r#"
            starwars %>% #foo
            group_by(species)   %>% #bar
            select(height, mass)%>% ###   baz
            summarise(
                    height = mean(height, na.rm = TRUE),
                mass = mean(mass, na.rm = TRUE)
            )
        "#};
    }

    #[test]
    fn purrr() {
        assert_fmt! {r#"
            library(purrr)

                    mtcars |>
                split(mtcars$cyl) |>  # from base R
            map(\(df) lm(mpg~wt, data    = df)) |>
            map(summary  ) |>
            map_dbl("r.squared"  )
        "#};
    }

    // ERROR CASES
    #[test]
    fn error() {
        let result = format_str(indoc! {r#"
            function
        "#});

        let Err(FormatError::SyntaxError { kind, raw }) = result else {
            panic!()
        };
        assert_eq!(kind, "program");
        assert_eq!(raw, "function");
    }

    #[test]
    fn missing() {
        assert_fmt! {r#"
            x <- 1
            function() { # missing function body
                x <- 2
                x <- 3
            x <- 3
        "#};

        // uncomment in case we want to throw an error instead
        // assert!(matches!(result, FormatError::Missing { "fuc",  }) )
    }

    // FROM
    // https://github.com/r-lib/tree-sitter-r/blob/main/test/corpus/literals.txt
    // https://github.com/r-lib/tree-sitter-r/blob/main/test/corpus/expressions.txt
    #[test]
    fn comments() {
        assert_fmt! {r#"
            # a comment'

            '# not a comment'


            '
            # still not a comment'
        "#}
    }

    #[test]
    fn constants() {
        assert_fmt! {r#"
            TRUE
            FALSE
            NULL
            Inf
            NaN
            NA
            NA_real_
            NA_character_
            NA_complex_
        "#}
    }

    #[test]
    fn identifiers() {
        assert_fmt! {r#"
            foo
            foo2
            foo.bar
            .foo.bar
            .__NAMESPACE__.
            foo_bar
            `_foo`
            `a "literal"`
            `another
            literal \` foo`
            `backslash followed by newline \
            `
            `\``
            # Pipe placeholder
            _
            # Recognized as a single `_foo` identifier, even if invalid R code (#71).
            _foo
            __foo
            _foo_
        "#}
    }

    #[test]
    fn strings() {
        assert_fmt! {r##"
            ""
            ''
            "foo"
            "foo
            bar"
            "#"
            ","
            "}"
            'foo'
            'foo
            bar'
            '#'
            ','
            '}'
        "##}
    }

    #[test]
    fn integers() {
        assert_fmt! {r#"
            12332L
            0L
            12L
            0xDEADL
            1e1L
            # Technically, R parses this as a float with a warning, but for our purposes this is good enough
            0.1L
        "#}
    }

    #[test]
    fn floats() {
        assert_fmt! {r#"
            .66
            .11
            123.4123
            .1234
            0xDEAD
            x <- -.66
            1e322
            1e-3
            1e+3
            1.8e10
            1.e10
            1e10
        "#}
    }

    #[test]
    fn dot_dot_i() {
        assert_fmt! {r#"
            ..1
            ..10
        "#}
    }
}
