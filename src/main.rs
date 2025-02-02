use {
    dashmap::DashMap,
    roughly_good_enough_lsp::index,
    tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc::Result, lsp_types::*},
};

#[derive(Debug)]
struct Backend {
    client: Client,
    symbols_map: DashMap<Url, Vec<DocumentSymbol>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        index::full_index(&self.symbols_map);
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
        self.client
            .log_message(MessageType::INFO, "server initialized 2!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn did_open(&self, _params: DidOpenTextDocumentParams) {
        log::debug!("did open");
    }

    async fn did_change(&self, _params: DidChangeTextDocumentParams) {
        log::debug!("did change");
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        index::update_file(&self.symbols_map, &params.text_document.uri);
        log::debug!("did save");
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        log::debug!("did close");
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query;
        let max_results = 10;
        Ok(Some(
            self.symbols_map
                .iter()
                .flat_map(|ref_multi| {
                    let (file, symbols) = ref_multi.pair();
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
                                    uri: file.to_owned(),
                                    range: symbol.range,
                                },
                                container_name: None,
                            }
                        })
                        .collect::<Vec<SymbolInformation>>()
                })
                .take(max_results)
                .collect(),
        ))
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("You're hovering!".to_string())),
            range: None,
        }))
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        symbols_map: DashMap::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
