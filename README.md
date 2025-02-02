<div align="center">

# The R(oughly good enough) LSP 🤔

</div>

Welcome to the R(oughly good enough) LSP, where we skip the fancy AST parsing and got straight to the synatx (just regex), because who has time for that when your R project is already making your computer cry?

## TODOs

* Fix crash in `DashMap`
* Add autocomplete based on symbols
* Implement go to definition
* Add Indexing for R6
* Make server path configurable
* Add some simple diagnostics: Assignment and camelCase
* Use tree-sitter?
* Better error handling
* Also index the `NAMESPACE` file
* Warn if no server found
* Only one `package.json`
* Do we need `tsconfig.tsbuildinfo` in VCS?
* symbol ranges seem of (jump to symbol goes to end of line???)

## Features

* Indexing
  * Globals
  * S4
    * Classes
    * Generics
    * Methods
* Commands
  * Restart Language Server

## Project layout

Currenlty this extension assume that your `R` code has the following folder structure:

| Path        | Type      |
|-------------|-----------|
| `R`        | directory |
| `R/*.R`     | file      |
| `NAMESPACE` | file      |

## Installation

### Client

Bundle the client:

```
npm run package
```

Install the extension to VS Code:

```
code --install-extension roughly-good-enough-lsp-x.x.x.vsix
```

### Server

Build the server:

```
cargo build --release
```

Configure the client via the `settings.json` to use the server binary:

```json
"roughlyGoodEnoughLsp.path" = "<path>",
```

## Development

- Run `npm install` in this folder. This installs all necessary npm modules in the client
- Open VS Code on this folder.
- Press Ctrl+Shift+B to start compiling the client in [watch mode](https://code.visualstudio.com/docs/editor/tasks#:~:text=The%20first%20entry%20executes,the%20HelloWorld.js%20file.).
- Switch to the Run and Debug View in the Sidebar (Ctrl+Shift+D).
- Select `Launch Client` from the drop down (if it is not already).
- Press ▷ to run the launch config (F5).
- In the [Extension Development Host](https://code.visualstudio.com/api/get-started/your-first-extension#:~:text=Then%2C%20inside%20the%20editor%2C%20press%20F5.%20This%20will%20compile%20and%20run%20the%20extension%20in%20a%20new%20Extension%20Development%20Host%20window.) instance of VSCode, open a document with a `.R` extension.
  - Press <kbd>Ctrl</kbd> <kbd>Shift</kbd> <kbd>O</kbd> to search document symbols
  - Press <kbd>Ctrl</kbd> <kbd>T</kbd> to search workspace symbols

### Words of warning

The `launch.json` contains a setting:

```json
"autoAttachChildProcesses": true,
```

For me this led to the issue that the language server wasn't spawned because I had `CodeLLDB` from `nixpkgs` installed.


## References

* https://github.com/microsoft/vscode-extension-samples/tree/main/lsp-sample
* https://github.com/semanticart/lsp-from-scratch
* https://www.reddit.com/r/rust/comments/uu47mk/comment/i9dn0yg/

## License

[GNU General Public License v3.0](LICENSE)
