use {
    dashmap::DashMap,
    ropey::Rope,
    roughly::index,
    tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc::Result, lsp_types::*},
};

#[derive(Debug)]
struct Backend {
    client: Client,
    symbols_map: DashMap<Url, Vec<DocumentSymbol>>,
    document_map: DashMap<Url, Document>,
}

#[derive(Debug)]
struct Document {
    text: Rope,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    //
    // LIFE CYLE METHODS
    //

    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        log::info!("server :: initialize");
        if let Err(()) = index::index_full(&self.symbols_map) {
            self.client
                .show_message(MessageType::ERROR, "failed to index files")
                .await;
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["$".into(), "@".into()]),
                    ..Default::default()
                }),
                // definition_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: env!("CARGO_PKG_NAME").into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        log::info!("initialized")
    }

    async fn shutdown(&self) -> Result<()> {
        log::debug!("bye bye");
        Ok(())
    }
    //
    // TEXT SYNC
    //

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::debug!("did open {}", params.text_document.uri);
        let rope = Rope::from_str(&params.text_document.text);
        self.document_map
            .insert(params.text_document.uri.clone(), Document {
                text: rope.clone(),
            });
        index::compute_diagnostics(params.text_document.uri, &rope).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        log::debug!("did change {}", params.text_document.uri);
        self.document_map
            .alter(&params.text_document.uri, |_, mut document| {
                for change in params.content_changes {
                    if let Some(range) = change.range {
                        let range = util::lsp_range_to_rope_range(range, &document.text).unwrap();
                        document.text.remove(range.clone());
                        document.text.insert(range.start, &change.text);
                    } else {
                        document.text = Rope::from_str(&change.text)
                    }
                }
                document
            });
        if let Some(document) = self.document_map.get(&params.text_document.uri) {
            index::compute_diagnostics(params.text_document.uri, &document.text).await;
        };
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        log::debug!("did save {}", params.text_document.uri);
        if let Some(text) = params.text {
            let rope = Rope::from_str(&text);
            self.document_map
                .insert(params.text_document.uri.clone(), Document { text: rope });
        }

        index::index_update(&self.symbols_map, &params.text_document.uri);
        if let Some(document) = self.document_map.get(&params.text_document.uri) {
            index::compute_diagnostics(params.text_document.uri, &document.text).await;
        };
    }

    //
    // COMPLETION
    //

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        log::debug!("Request completion items for: {uri}");
        let position = params.text_document_position.position;
        // todo: proper error handling. make ropey, dashmap -> JSONRpc error
        let completions = || -> Option<Vec<CompletionItem>> {
            let rope = &self.document_map.get(&uri)?.text;
            let line = rope.get_line(position.line as usize)?;
            let mut query = String::new();
            for (i, char) in line.chars().enumerate() {
                if char.is_alphabetic() || char == '.' || (!query.is_empty() && char.is_numeric()) {
                    query.push(char)
                } else {
                    query.clear();
                }
                if i == (position.character - 1) as usize {
                    break;
                }
            }
            log::debug!("completion query: {query}");

            let symbols = index::get_workspace_symbols(&query, &self.symbols_map, 1000);

            const RESERVED_WORDS: &[&str] = &[
                "if",
                "else",
                "repeat",
                "while",
                "function",
                "for",
                "in",
                "next",
                "break",
                "TRUE",
                "FALSE",
                "NULL",
                "Inf",
                "NaN",
                "NA",
                "NA_integer_",
                "NA_real_",
                "NA_complex_",
                "NA_character_",
            ];

            Some(
                RESERVED_WORDS
                    .iter()
                    .map(|reserved_word| CompletionItem {
                        label: reserved_word.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    })
                    .chain(symbols.into_iter().map(|symbol| CompletionItem {
                        label: symbol.name,
                        kind: Some(match symbol.kind {
                            SymbolKind::FUNCTION => CompletionItemKind::FUNCTION,
                            SymbolKind::CLASS => CompletionItemKind::CLASS,
                            SymbolKind::METHOD => CompletionItemKind::METHOD,
                            _ => CompletionItemKind::VARIABLE,
                        }),
                        detail: None,
                        ..Default::default()
                    }))
                    .collect(),
            )
        }();

        Ok(completions.map(CompletionResponse::Array))
    }

    //
    // SYMBOLS
    //

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        Ok(Some(DocumentSymbolResponse::Flat(
            index::get_document_symbols(&params.text_document.uri, &self.symbols_map),
        )))
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        Ok(Some(index::get_workspace_symbols(
            &params.query,
            &self.symbols_map,
            32,
        )))
    }

    // async fn goto_definition(
    //     &self,
    //     params: GotoDefinitionParams,
    // ) -> Result<Option<GotoDefinitionResponse>> {
    //     // dbg!(params);
    //     Ok(None)
    // }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        symbols_map: DashMap::new(),
        document_map: DashMap::new(),
    });

    log::info!("starting language server ... listing for stdin");
    Server::new(stdin, stdout, socket).serve(service).await;
}

// UTILS

#[allow(unused)]
mod util {
    use {
        ropey::Rope,
        tower_lsp::lsp_types::{Position, Range},
    };

    pub fn position_to_index(position: Position, rope: &Rope) -> Result<usize, ropey::Error> {
        let line = position.line as usize;
        let line = rope.try_line_to_char(line)?;
        Ok(line + position.character as usize)
    }

    pub fn index_to_position(index: usize, rope: &Rope) -> Result<Position, ropey::Error> {
        let line = rope.try_char_to_line(index)?;
        let char = index - rope.line_to_char(line);
        Ok(Position {
            line: line as u32,
            character: char as u32,
        })
    }

    pub fn lsp_range_to_rope_range(
        range: Range,
        rope: &Rope,
    ) -> Result<std::ops::Range<usize>, ropey::Error> {
        let start = position_to_index(range.start, rope)?;
        let end = position_to_index(range.end, rope)?;
        Ok(start..end)
    }

    pub fn rope_range_to_lsp_range(
        range: std::ops::Range<usize>,
        rope: &Rope,
    ) -> Result<Range, ropey::Error> {
        let start = index_to_position(range.start, rope)?;
        let end = index_to_position(range.end, rope)?;
        Ok(Range { start, end })
    }
}
