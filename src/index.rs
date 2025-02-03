use {dashmap::DashMap, ropey::Rope, std::path::Path, tower_lsp::lsp_types::*};

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
    let Some(symbols) = symbols_map.get(url) else {
        log::info!("failed to acquire symbols map");
        // todo: understand when this happens
        return vec![];
    };
    filter_symbols("", url, &symbols)
}

fn filter_symbols(query: &str, url: &Url, symbols: &[DocumentSymbol]) -> Vec<SymbolInformation> {
    symbols
        .iter()
        .filter(|symbol| query.is_empty() || symbol.name.starts_with(query))
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

pub fn index_full(symbols_map: &DashMap<Url, Vec<DocumentSymbol>>) -> Result<(), ()> {
    log::info!("build new index");
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
                let url = Url::from_file_path(path)?;
                symbols_map.insert(url, symbols);
            }
        }
    }
    Ok(())
}

pub fn index_update(symbols_map: &DashMap<Url, Vec<DocumentSymbol>>, url: &Url) {
    let Ok(path) = url.to_file_path() else {
        log::error!("failed to get file path for {url}");
        return;
    };
    log::info!("update index for {path:?}");
    let symbols = index_file(path);
    symbols_map.insert(url.clone(), symbols);
}

pub fn index_file(path: impl AsRef<Path>) -> Vec<DocumentSymbol> {
    let Ok(text) = std::fs::read_to_string(&path) else {
        log::info!("indexing: couldn't read file: '{:?}'!", path.as_ref());
        return vec![];
    };
    index(&text)
}

fn index(text: &str) -> Vec<DocumentSymbol> {
    let newlines = regex!(r#"\n"#);
    let newline_positions = newlines
        .captures_iter(text)
        .map(|captures| captures.get(0).unwrap().start())
        .collect::<Vec<usize>>();

    // todo: consider removing comments (gets tricky positions :/)
    // could replace comments with whitespace

    let globals = regex!(r#"(?m)^([\w\.]+)\s*<-\s*(\\\(|function|\S)"#);
    let symbols_globals = globals.captures_iter(text).map(|captures| {
        let name = captures.get(1).unwrap();
        let kind = captures.get(2).unwrap();
        let token_start = name.start();
        let line = newline_positions.partition_point(|&x| token_start > x) as u32;
        let range = Range::new(
            Position::new(line, token_start as u32),
            Position::new(line, name.end() as u32),
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
            range,
            selection_range: range,
            children: None,
        }
    });

    let s4 = regex!(
        r#"(?m)(setClass|setGeneric|setMethod)\s*\(\s*["']([\w\.<-]+)["']\s*,\s*(?:["']([\w\.]+)["'])?"#
    );
    let symbolds_s4 = s4.captures_iter(text).map(|captures| {
        let kind = captures.get(1).unwrap();
        let first = captures.get(2).unwrap();
        let second = captures.get(3);
        let token_start = kind.start();
        let line = newline_positions.partition_point(|&x| token_start > x) as u32;
        let range = Range::new(
            Position::new(line, kind.start() as u32),
            Position::new(line, kind.end() as u32),
        );

        let (name, kind) = match kind.as_str() {
            "setClass" => (first.as_str().to_string(), SymbolKind::CLASS),
            "setGeneric" => (first.as_str().to_string(), SymbolKind::INTERFACE),
            "setMethod" => (
                format!(
                    "{} ({})",
                    first.as_str(),
                    second.map(|m| m.as_str()).unwrap_or("UNKNOWN")
                ),
                SymbolKind::METHOD,
            ),
            _ => ("UNKNOWN".to_string(), SymbolKind::VARIABLE),
        };

        #[allow(deprecated)]
        DocumentSymbol {
            name,
            detail: None,
            kind,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    });

    symbols_globals.chain(symbolds_s4).collect()
}

// DIAGNOSTICS

pub async fn compute_diagnostics(uri: Url, _: &Rope) {
    log::debug!("compute diagnostics for {uri}");
    // let newlines = regex!(r#"\n"#);
    // let newline_positions = newlines
    //     .captures_iter(&rope)
    //     .map(|captures| captures.get(0).unwrap().start())
    //     .collect::<Vec<usize>>();

    // let assignments = regex!(r#"(?m)(\s*)([\w\.]+)\s*<-"#);
    // // todo: consider join_all from futures
    // let diagnostics = assignments.captures_iter(&rope).map(|captures| {
    //     let name = captures.get(1).unwrap();
    //     let kind = captures.get(2).unwrap();
    //     let token_start = name.start();
    //     let line = newline_positions.partition_point(|&x| token_start > x) as u32;
    //     let range = Range::new(
    //         Position::new(line, token_start as u32),
    //         Position::new(line, name.end() as u32),
    //     );
    // });
    // let mut diagnostics = HashMap::from([(uri.clone(), Vec::new())]);
    // https://github.com/jfecher/ante/blob/5f7446375bc1c6c94b44a44bfb89777c1437aaf5/ante-ls/src/main.rs#L252
}

#[cfg(test)]
mod test {
    use {super::index, indoc::indoc, tower_lsp::lsp_types::SymbolKind};

    #[test]
    fn test_indexing() {
        let symbols = index(indoc! {r#"
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
		"#});

        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);

        assert_eq!(symbols[1].name, "bar");
        assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);

        assert_eq!(symbols[2].name, "baz");
        assert_eq!(symbols[2].kind, SymbolKind::VARIABLE);

        assert_eq!(symbols[3].name, "Person");
        assert_eq!(symbols[3].kind, SymbolKind::CLASS);

        assert_eq!(symbols[4].name, "Car");
        assert_eq!(symbols[4].kind, SymbolKind::CLASS);

        assert_eq!(symbols[5].name, "age");
        assert_eq!(symbols[5].kind, SymbolKind::INTERFACE);

        assert_eq!(symbols[6].name, "age<-");
        assert_eq!(symbols[6].kind, SymbolKind::INTERFACE);

        assert_eq!(symbols[7].name, "age (Person)");
        assert_eq!(symbols[7].kind, SymbolKind::METHOD);

        assert_eq!(symbols[8].name, "age<- (Person)");
        assert_eq!(symbols[8].kind, SymbolKind::METHOD);

        assert_eq!(symbols.len(), 9);
    }
}
