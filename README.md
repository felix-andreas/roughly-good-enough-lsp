<div align="center">

# R(oughly good enough) LSP 🤔 

</div>

Welcome to the R(oughly good enough) LSP, where we skip the fancy AST parsing and got straight to the synatx (just regex), because who has time for that when your R project is already making your computer cry?

## TODOs

* Also index methods and generics, and classes
* Implement go to definition
* Make server path configurable
* Add some simple diagnostics: Assignment and camelCase
* Create build for extension
* Add command to restart server
* Use tree-sitter?
* Add installation section

## Words of warning

The `launch.json` contains a setting:

```json
"autoAttachChildProcesses": true,
```

For me this led to the issue that the language server wasn't spawned because I had `CodeLLDB` from `nixpkgs` installed.

## Development

- Run `npm install` in this folder. This installs all necessary npm modules in both the client and server folder
- Open VS Code on this folder.
- Press Ctrl+Shift+B to start compiling the client and server in [watch mode](https://code.visualstudio.com/docs/editor/tasks#:~:text=The%20first%20entry%20executes,the%20HelloWorld.js%20file.).
- Switch to the Run and Debug View in the Sidebar (Ctrl+Shift+D).
- Select `Launch Client` from the drop down (if it is not already).
- Press ▷ to run the launch config (F5).
- In the [Extension Development Host](https://code.visualstudio.com/api/get-started/your-first-extension#:~:text=Then%2C%20inside%20the%20editor%2C%20press%20F5.%20This%20will%20compile%20and%20run%20the%20extension%20in%20a%20new%20Extension%20Development%20Host%20window.) instance of VSCode, open a document in 'plain text' language mode.
  - Type `j` or `t` to see `Javascript` and `TypeScript` completion.
  - Enter text content such as `AAA aaa BBB`. The extension will emit diagnostics for all words in all-uppercase.

## References

* https://github.com/microsoft/vscode-extension-samples/tree/main/lsp-sample
* https://github.com/semanticart/lsp-from-scratch
* https://www.reddit.com/r/rust/comments/uu47mk/comment/i9dn0yg/
