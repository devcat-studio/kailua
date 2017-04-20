# The Internals of Kailua

## Source Organization

Kailua is a [Rust] application, composed of multiple [crates][crates-and-modules]:

* [`kailua_env`](kailua_env/): Span information, source code management, scope mapping.

* [`kailua_diag`](kailua_diag/): Rudimentary localization and reporting facility.

* [`kailua_test`](kailua_test/): Testing harness for `kailua_syntax` and `kailua_check`. For those crates `cargo test` will include a battery of integration tests.

* [`kailua_syntax`](kailua_syntax/): Fully recoverable Lua lexer and parser. Aware of Kailua extensions as well.

* [`kailua_check`](kailua_check/): Main type checker for Kailua.

* [`kailua_vsc`](kailua_vsc/): [Visual Studio Code][VSCode] IDE extension.

  * [`kailua_langsvr`](kailua_langsvr/): A standalone language server for [Visual Studio Code][VSCode].

  * [`kailua_langsvr_protocol`](kailua_langsvr_protocol/): A macro-heavy portion of `kailua_langsvr`, separated in order to reduce compilation time.

<!-- -->

[crates-and-modules]: https://doc.rust-lang.org/book/crates-and-modules.html
[VSCode]: https://code.visualstudio.com/
