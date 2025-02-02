test:
	cargo test -- --nocapture

bundle *args:
	npm run package -- {{args}}

install:
	code --install-extension roughly-good-enough-lsp-*.vsix

linux:
	cargo build --release

windows:
	cargo build --release --target x86_64-pc-windows-gnu

build-release-bundle:
	rm -rf release
	mkdir release
	# build client
	just bundle --out release/roughly-good-enough-lsp.vsix
	# build server
	just linux
	just windows
	cp target/release/roughly-good-enough-lsp release
	cp target/x86_64-pc-windows-gnu/release/roughly-good-enough-lsp.exe release

publish:
	just build-release-bundle
	gh release upload nightly \
		"release/roughly-good-enough-lsp.vsix#VS Code extension" \
		"release/roughly-good-enough-lsp#LSP server (Linux)" \
		"release/roughly-good-enough-lsp.exe#LSP server (Windows)" \
		--clobber
