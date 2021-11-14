# Long

[![Build](https://github.com/maekawatoshiki/long/actions/workflows/ci.yml/badge.svg)](https://github.com/maekawatoshiki/long/actions/workflows/ci.yml)

We don't need a long time to compile C++.

Issues and PRs are always welcome.

# Run

```sh
git clone https://github.com/maekawatoshiki/long
cd long
export LONGCCPATH=`pwd`
rustup override set nightly
cargo test # --release
```

```sh
cargo run --example lex # panics because the parser is incomplete
```
