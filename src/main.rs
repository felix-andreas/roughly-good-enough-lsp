use {
    dashmap::DashMap,
    ropey::Rope,
    roughly::{
        index::{self, parse},
        utils,
    },
    tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc::Result, lsp_types::*},
    tree_sitter::Tree,
};

#[derive(Debug)]
struct Backend {
    client: Client,
    symbols_map: DashMap<Url, Vec<DocumentSymbol>>,
    document_map: DashMap<Url, Document>,
}

#[derive(Debug)]
struct Document {
    rope: Rope,
    tree: Tree,
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

    // todo: read https://github.com/TenStrings/glicol-lsp/blob/77e97d9c687dc5d66871ad5ec91b6f049de2b8e8/src/main.rs#L76C2-L91C6
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::debug!("did open {}", params.text_document.uri);
        let rope = Rope::from_str(&params.text_document.text);
        let tree = index::parse(&params.text_document.text);
        index::compute_diagnostics(params.text_document.uri.clone(), &rope).await;

        self.document_map
            .insert(params.text_document.uri.clone(), Document {
                rope: rope,
                tree,
            });
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        log::debug!("did change {}", params.text_document.uri);
        self.document_map
            .alter(&params.text_document.uri, |_, mut document| {
                for change in params.content_changes {
                    if let Some(range) = change.range {
                        let range = utils::lsp_range_to_rope_range(range, &document.rope).unwrap();
                        document.rope.remove(range.clone());
                        document.rope.insert(range.start, &change.text);
                    } else {
                        document.rope = Rope::from_str(&change.text)
                    }
                }
                // TODO: update ast
                document
            });
        if let Some(document) = self.document_map.get(&params.text_document.uri) {
            index::compute_diagnostics(params.text_document.uri, &document.rope).await;
        };
    }

    // todo: read https://github.com/TenStrings/glicol-lsp/blob/77e97d9c687dc5d66871ad5ec91b6f049de2b8e8/src/main.rs#L76C2-L91C6
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        log::debug!("did save {}", params.text_document.uri);
        if let Some(text) = params.text {
            let rope = Rope::from_str(&text);
            self.document_map
                .insert(params.text_document.uri.clone(), Document {
                    rope: rope,
                    tree: parse(&text),
                });
        }

        index::index_update(&self.symbols_map, &params.text_document.uri);
        if let Some(document) = self.document_map.get(&params.text_document.uri) {
            index::compute_diagnostics(params.text_document.uri, &document.rope).await;
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
            let rope = &self.document_map.get(&uri)?.rope;
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
