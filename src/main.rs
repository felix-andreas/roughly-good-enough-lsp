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
        log::info!("server :: initialize");
        if let Err(()) = index::index_full(&self.symbols_map) {
            self.client
                .show_message(MessageType::ERROR, "failed to index files")
                .await;
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                completion_provider: Some(CompletionOptions::default()),
                document_symbol_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(false),
                        change: Some(TextDocumentSyncKind::NONE),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(false),
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

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        dbg!(&params);
        log::debug!("Request completion items");
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::debug!("did open {}", params.text_document.uri);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        log::debug!("did change {}", params.text_document.uri);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        log::debug!("did save {}", params.text_document.uri);
        index::index_update(&self.symbols_map, &params.text_document.uri);
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        log::debug!("did close {}", params.text_document.uri);
    }

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
        let query = params.query;
        Ok(Some(index::get_workspace_symbols(
            &query,
            &self.symbols_map,
        )))
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
