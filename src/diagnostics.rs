use {
    crate::{cli, tree},
    console::style,
    ignore::Walk,
    ropey::Rope,
    std::path::PathBuf,
    tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range},
    tree_sitter::{Node, TreeCursor},
};

pub fn run(maybe_files: Option<&[PathBuf]>) -> Result<(), ()> {
    let root: Vec<PathBuf> = vec![".".into()];
    let files = maybe_files.unwrap_or(&root);

    let mut n_files = 0;
    let mut n_errors = 0;
    for path in files
        .iter()
        .flat_map(Walk::new)
        .filter_map(|e| e.ok())
        .map(|entry| entry.into_path())
        .filter(|path| {
            path.extension()
                .map(|ext| ext == "R" || ext == "r")
                .unwrap_or(false)
        })
    {
        n_files += 1;
        let old = match std::fs::read_to_string(&path) {
            Ok(old) => old,
            Err(err) => {
                n_errors += 1;
                cli::error(&format!("failed to format: {}", path.display()));
                eprintln!("{err}");
                continue;
            }
        };
        let tree = tree::parse(&old, None);
        let rope = Rope::from_str(&old);
        let diagnostics = diagnostics_syntax(tree.root_node(), &rope);
        n_errors += diagnostics.len();

        for diagnostic in diagnostics {
            cli::error(&format!("{}", diagnostic.message));
            let range = diagnostic.range;
            eprintln!(
                "{} {}:{}:{}",
                style("  -->").bold().blue(),
                path.display(),
                range.start.line,
                range.start.character
            );

            let start_byte = rope.line_to_char(if range.start.line == 0 {
                0
            } else {
                range.start.line - 1
            } as usize);
            let end_byte = rope
                .try_char_to_byte(
                    rope.line_to_char(range.end.line as usize) + range.end.character as usize,
                )
                .unwrap();

            let lines = rope.slice(start_byte..end_byte);
            let width = range.end.line.to_string().len() + 1;
            let width_error = u32::max(
                1,
                if range.end.character > range.start.character {
                    range.end.character - range.start.character
                } else {
                    range.start.character - range.end.character
                },
            );
            for (i, line) in lines.lines().enumerate() {
                eprint!("{} {}", style(format!("{i:<width$}|")).blue().bold(), line);
            }
            eprintln!();
            eprintln!(
                "{}{}  {}",
                " ".repeat(width),
                " ".repeat(usize::min(
                    range.start.character as usize,
                    range.end.character as usize
                )),
                style("^".repeat(width_error as usize)).red().bold()
            );
            eprintln!(
                "{}{}  {}",
                " ".repeat(width),
                " ".repeat(usize::min(
                    range.start.character as usize,
                    range.end.character as usize
                )),
                style(diagnostic.message).red().bold()
            );

            eprintln!("\n")
        }
    }

    if n_files == 0 {
        cli::warning("No R files found under the given path(s)");
        return Err(());
    }

    if n_errors == 0 { Ok(()) } else { Err(()) }
}

pub fn diagnostics_syntax(node: Node, rope: &Rope) -> Vec<Diagnostic> {
    fn node_range(node: Node) -> Range {
        Range {
            start: Position {
                line: node.start_position().row as u32,
                character: node.start_position().column as u32,
            },
            end: Position {
                line: node.end_position().row as u32,
                character: node.end_position().column as u32,
            },
        }
    }

    fn diag(node: Node, message: String) -> Diagnostic {
        Diagnostic {
            message: message,
            severity: Some(DiagnosticSeverity::ERROR),
            range: node_range(node),
            code: None,
            code_description: None,
            source: None,
            related_information: None,
            tags: None,
            data: None,
        }
    }

    fn traverse(cursor: &mut TreeCursor, diagnostics: &mut Vec<Diagnostic>, rope: &Rope) -> bool {
        let node = cursor.node();
        if !(node.is_error() || node.has_error()) {
            return false;
        }

        // DEBUG
        // eprintln!(
        //     "{}_{:?}_{}",
        //     node.kind(),
        //     node.has_error(),
        //     rope.byte_slice(node.byte_range()),
        // );

        match node.kind() {
            "arguments"
            | "braced_expression"
            | "for_statement"
            | "if_statement"
            | "parameters"
            | "parenthesized_expression"
            | "while_statement" => {
                if let Some(open) = node.child_by_field_name("open") {
                    if let Some(close) = node.child_by_field_name("close") {
                        if close.is_missing() {
                            diagnostics.push(diag(
                                open,
                                format!("missing closing delimiter {}", close.kind()),
                            ));
                        }
                    }
                }
            }
            "binary_operator" => {
                if let Some(operator) = node.child_by_field_name("operator") {
                    if let Some(rhs) = node.child_by_field_name("rhs") {
                        if rhs.is_missing() {
                            diagnostics.push(diag(
                                operator,
                                format!("missing rhs for operator {}", operator.kind()),
                            ));
                        }
                    }
                }
            }
            "function_definition" => {
                if let Some(body) = node.child_by_field_name("body") {
                    if body.is_missing() {
                        diagnostics.push(diag(node, format!("missing function body")));
                    }
                }
            }
            _ => {}
        }

        let mut handled_error = false;
        // let child_kinds = vec![];
        if cursor.goto_first_child() {
            if node.is_error() {
                let child = cursor.node();
                match child.kind() {
                    "(" | "{" | "[" | "[[" => diagnostics.push(diag(
                        child,
                        format!("missing closing delimiter {}", child.kind()),
                    )),
                    _ => {}
                }
            }

            loop {
                handled_error |= traverse(cursor, diagnostics, rope);

                if !cursor.goto_next_sibling() {
                    break;
                }
            }

            cursor.goto_parent();
        }

        if !handled_error && node.is_error() {
            handled_error = true;
            let raw = rope.byte_slice(node.byte_range()).to_string();
            match raw.as_str() {
                ")" | "}" | "]" | "]]" => diagnostics.push(diag(
                    node,
                    format!("Syntax Error: unexpected closing delimiter {}", raw),
                )),
                _ => {
                    diagnostics.push(diag(node, format!("Syntax Error: unexpected {:?}", raw)));
                }
            }
        }

        handled_error
    }

    let mut diagnostics = Vec::new();
    let mut cursor = node.walk();
    traverse(&mut cursor, &mut diagnostics, rope);
    diagnostics
}
