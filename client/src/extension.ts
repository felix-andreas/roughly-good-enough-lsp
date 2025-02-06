import { window, commands, workspace, ExtensionContext } from 'vscode'

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
  Executable
} from 'vscode-languageclient/node'

let client: LanguageClient

export function activate(context: ExtensionContext) {
  const config = workspace.getConfiguration("roughly")
  const lspPath = config.get<string>("path", "roughly")

  const command = process.env.SERVER_PATH || lspPath
  console.log("using server command:", command)

  client = (() => {
    const run: Executable = {
      command,
      transport: TransportKind.stdio,
      options: {
        env: {
          ...process.env,
          RUST_LOG: "debug",
        },
      },
    }

    const serverOptions: ServerOptions = {
      run,
      debug: run,
    }

    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: "file", language: "r" }],
      synchronize: {
        // Notify the server about file changes to '.clientrc files contained in the workspace
        fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
      },
    }

    return new LanguageClient(
      'roughly',
      'Roughly',
      serverOptions,
      clientOptions
    )
  })()

  context.subscriptions.push(
    commands.registerCommand(
      "roughly.restartLanguageServer",
      async () => { await client.restart() }
    ),
  )

  context.subscriptions.push(
    commands.registerCommand(
      "roughly.startLanguageServer",
      async () => { await client.start() }
    ),
  )

  context.subscriptions.push(
    commands.registerCommand(
      "roughly.stopLanguageServer",
      async () => { await client.stop() }
    ),
  )

  context.subscriptions.push(
    workspace.onDidChangeConfiguration(async (change) => {
      if (
        change.affectsConfiguration("roughly.path", undefined)) {
        const choice = await window.showWarningMessage(
          "Configuration change requires restarting the language server",
          "Restart",
        )
        if (choice === "Restart") {
          await client.restart()
          setTimeout(() => {

            client.outputChannel.show()
          }, 1500)
        }
      }
    }),
  )

  client.start() // this also launches the server
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined
  }
  return client.stop()
}
