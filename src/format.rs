use {
    crate::{
        cli, tree,
        utils::{self, indent_by},
    },
    console::style,
    ignore::Walk,
    ropey::Rope,
    std::path::PathBuf,
    thiserror::Error,
    tree_sitter::{Node, TreeCursor},
};

pub fn run(maybe_files: Option<&[PathBuf]>, check: bool, diff: bool) -> bool {
    let root: Vec<PathBuf> = vec![".".into()];
    let files = maybe_files.unwrap_or(&root);

    let mut n_files = 0;
    let mut n_unformatted = 0;
    let mut n_errors = 0;
    files
        .iter()
        .flat_map(|file| Walk::new(file).into_iter())
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
                } else {
                    if let Err(_) = std::fs::write(&path, new) {
                        cli::error(&format!("failed to write to file: {}", path.display()));
                    }
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
    let kind = node.kind();
    let fmt = |node: Node| format(node, rope);
    let field = |field: &'static str| {
        node.child_by_field_name(field)
            .ok_or(FormatError::MissingField { kind, field })
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
    }

    let get_raw = || rope.byte_slice(node.byte_range()).to_string();

    if node.is_extra() && node.kind() == "comment" {
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
        dbg!(&prefix, &content);
        let sep = if content.starts_with(char::is_whitespace) {
            ""
        } else {
            " "
        };
        return Ok(format!("{prefix}{sep}{content}"));
    }

    if node.is_missing() {
        let parent = node.parent().unwrap();
        return Err(FormatError::Missing {
            kind: parent.kind(),
            raw: get_raw(),
        });
    }

    if !node.is_named() {
        return Ok(get_raw());
    }

    Ok(match kind {
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
                    if child.kind() == "comma" {
                        return Ok(",".to_string());
                    }
                    if child.is_extra() {
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
                    let result = format!(
                        "{}{}",
                        if is_first_arg {
                            if prev_node_local.is_some() { "\n" } else { "" }
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
            let lhs = field("lhs")?;
            let operator = field("operator")?;
            let rhs = field("rhs")?;
            let is_multiline = lhs.end_position().row != rhs.start_position().row;
            let has_spacing = operator.kind() == ":";
            format!(
                "{}{}{}{}{}",
                fmt(lhs)?,
                if has_spacing { "" } else { " " },
                fmt(operator)?,
                if is_multiline {
                    "\n"
                } else if has_spacing {
                    ""
                } else {
                    " "
                },
                if is_multiline {
                    indent_by(2, fmt(rhs)?)
                } else {
                    fmt(rhs)?
                }
            )
        }
        "braced_expression" => {
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

            // we only indent if { and } are not on the same line
            if open.start_position().row == close.end_position().row {
                if lines.is_empty() {
                    "{}".to_string()
                } else {
                    format!("{{ {} }}", lines.join("; "))
                }
            } else {
                format!("{{\n{}\n}}", utils::indent_by(2, lines.join("")))
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
                "for ({} in {}) {}",
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
                        "if ({}) {} else {}",
                        condition,
                        consequence,
                        fmt(alternative)?
                    )
                }
                None => {
                    format!("if ({}) {}", condition, consequence,)
                }
            }
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
            format!("repeat {}", fmt(field("body")?)?)
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
        "unary_operator" => {
            let operator = field("operator")?;
            let spacing = if operator.kind() == "~" { " " } else { "" };
            format!("{}{spacing}{}", fmt(operator)?, fmt(field("rhs")?)?)
        }
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
    })
}

#[cfg(test)]
mod test {
    // consider testing: https://github.com/r-lib/tree-sitter-r/blob/a0d3e3307489c3ca54da8c7b5b4e0c5f5fd6953a/test/corpus/expressions.txt
    use {super::*, crate::tree, indoc::indoc};

    fn fmt(text: &str) -> String {
        let tree = tree::parse(text, None);

        // DEBUG
        dbg!(tree.root_node().to_sexp());
        format(tree.root_node(), &Rope::from_str(text)).unwrap()
    }

    macro_rules! assert_fmt {
        ($input:expr) => {
            insta::assert_snapshot!(fmt(indoc! {$input}));
        };
    }

    #[test]
    fn binary_operator() {
        assert_fmt! {r#"
            4 + 2
            4 + 2*3
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
    }

    #[test]
    fn braced_expression() {
        assert_fmt! {r#"
            {}
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
        // todo: make this work
        // assert_fmt! {r#"
        //     function(
        //     	# foo
        //         foo, #foo
        //         #bar
        //         #  bar
        //         bar = 3 #bar
        //     ) {}
        // "#};
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
    fn program() {
        assert_fmt! {r#"
        	"foo"
            if (1) {
            }
        "#};
    }

    // SPECIAL CASES
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

    // FROM https://github.com/r-lib/tree-sitter-r/blob/a0d3e3307489c3ca54da8c7b5b4e0c5f5fd6953a/test/corpus/literals.txt
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
