import { window, workspace, ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
  Executable
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(_context: ExtensionContext) {
  // uncomment to us TS server
  // const serverModule = context.asAbsolutePath(
  //   path.join('server', 'out', 'server.js')
  // );
  // const serverOptions: ServerOptions = {
  //   run: { module: serverModule, transport: TransportKind.ipc },
  //   debug: {
  //     module: serverModule,
  //     transport: TransportKind.ipc,
  //   }
  // };

  const command = process.env.SERVER_PATH || "roughly-good-enough-lsp";
  console.log(command)
  const run: Executable = {
    command,
    transport: TransportKind.stdio,
    options: {
      env: {
        ...process.env,
        RUST_LOG: "debug",
      },
    },
  };
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "r" }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
    },
  };

  client = new LanguageClient(
    'roughly-good-enough-lsp',
    'R(oughly good enough) LSP',
    serverOptions,
    clientOptions
  );

  client.start(); // this also launches the server
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
