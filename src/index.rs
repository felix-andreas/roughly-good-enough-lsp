use {dashmap::DashMap, std::path::Path, tower_lsp::lsp_types::*};

macro_rules! regex {
    ($re:literal $(,)?) => {{
        use {regex::Regex, std::sync::OnceLock};

        static RE: OnceLock<Regex> = OnceLock::new();
        RE.get_or_init(|| Regex::new($re).unwrap())
    }};
}

pub fn get_workspace_symbols(
    query: &str,
    symbols_map: &DashMap<Url, Vec<DocumentSymbol>>,
) -> Vec<SymbolInformation> {
    symbols_map
        .iter()
        .flat_map(|ref_multi| {
            let (url, symbols) = ref_multi.pair();
            filter_symbols(query, url, symbols)
        })
        .take(32) // limit amount
        .collect()
}

pub fn get_document_symbols(
    url: &Url,
    symbols_map: &DashMap<Url, Vec<DocumentSymbol>>,
) -> Vec<SymbolInformation> {
    let symbols = symbols_map.get(url).unwrap();
    filter_symbols("", url, &symbols)
}

fn filter_symbols(query: &str, url: &Url, symbols: &[DocumentSymbol]) -> Vec<SymbolInformation> {
    symbols
        .iter()
        .filter(|symbol| query == "" || symbol.name.starts_with(&query))
        .map(|symbol| {
            #[allow(deprecated)]
            SymbolInformation {
                name: symbol.name.to_string(),
                kind: symbol.kind,
                tags: None,
                deprecated: None,
                location: Location {
                    uri: url.to_owned(),
                    range: symbol.range,
                },
                container_name: None,
            }
        })
        .collect::<Vec<SymbolInformation>>()
}

pub fn full_index(symbols_map: &DashMap<Url, Vec<DocumentSymbol>>) {
    let cwd = std::env::current_dir().unwrap();
    if let Ok(entries) = std::fs::read_dir(cwd.join("R")) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();

                if !path.is_file() {
                    continue;
                }

                let Some(os_str) = path.extension() else {
                    continue;
                };
                let Some("r" | "R") = os_str.to_str() else {
                    continue;
                };

                let symbols = index_file(&path);
                let url = Url::from_file_path(path).unwrap();
                symbols_map.insert(url, symbols);
            }
        }
    }
}

pub fn update_file(symbols_map: &DashMap<Url, Vec<DocumentSymbol>>, url: &Url) {
    let symbols = index_file(url.path());
    symbols_map.insert(url.clone(), symbols);
}

pub fn index_file(path: impl AsRef<Path>) -> Vec<DocumentSymbol> {
    let Ok(text) = std::fs::read_to_string(path) else {
        log::debug!("couldn't read file!");
        return vec![];
    };
    index(&text)
}

fn index(text: &str) -> Vec<DocumentSymbol> {
    let newlines = regex!(r#"\r?\n"#);
    let newline_positions = newlines
        .captures_iter(text)
        .map(|captures| captures.get(0).unwrap().start())
        .collect::<Vec<usize>>();

    let assignments = regex!(r#"(?m)^([\w\.]+)\s*<-\s*(\\|function|\S)"#);
    assignments
        .captures_iter(text)
        .map(|captures| {
            let name = captures.get(1).unwrap();
            let kind = captures.get(2).unwrap();
            let name_end = name.end();
            let line = newline_positions.partition_point(|&x| name_end > x) as u32;
            let range = Range::new(
                Position::new(line, name.start() as u32),
                Position::new(line, name_end as u32),
            );
            #[allow(deprecated)]
            DocumentSymbol {
                name: name.as_str().to_string(),
                detail: None,
                kind: match kind.as_str() {
                    "\\(" | "function" => SymbolKind::FUNCTION,
                    _ => SymbolKind::VARIABLE,
                },
                tags: None,
                deprecated: None,
                range: range,
                selection_range: range,
                children: None,
            }
        })
        .collect()
}

#[cfg(test)]
mod test {
    use {super::index, indoc::indoc};

    #[test]
    fn test_indexing() {
        let symbols = index(indoc! {r#"
			foo <- function() {}
			bar <- \(x) {}
			baz <- 4
		"#});
        dbg!(symbols);
    }
}
