windows:
	cargo build --release --target x86_64-pc-windows-gnu

test:
	cargo test -- --nocapture

bundle:
	npm run package
