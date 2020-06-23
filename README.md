# silkworm

silkworm is an implementation of the [Yarn](https://github.com/YarnSpinnerTool/YarnSpinner/) interactive dialogue language in pure Rust.

## Work-in-progress

**silkworm is currently in the initial development phase. Any and all features advertised might not be implemented yet!**

- [x] Lexer
- [x] Parser
- [ ] Runtime
  - [ ] Source map
  - [ ] IR definition, AST validation and lowering
  - [ ] Interpreter
- [ ] User-facing API
- [ ] CLI tools?

## (Hopefully) Features

- Zero-copy parsing of plain text in dialogue.
- Support for a superset of Yarn 1.1, with useful features such as scoped identifiers and subroutines.
- Passive runtime for easy integration into any kind of program.
- In Rust!

## Differences from YarnSpinner

### Runtime

The silkworm runtime is passive and does not assume a real-time environment. As a result, commands like `wait` must be provided by the user, even though it is built-in in YarnSpinner.

It's also not currently planned to include implementations for YarnSpinner's built-in format functions (`select`, `plural` and `ordinal`), as development is being focused on the language itself. Users may provide their own format functions if needed.

### Keywords

In addition to YarnSpinner keywords and operators, a few currently unused keywords are reserved for forward compatibility.

See [docs/keywords.md](docs/keywords.md) for a full list of currently used and reserved keywords.

### Pragmas

silkworm supports pragmas, which are instructions placed in the source code that change silkworm's behavior. They take the form of `//# name(<arg>, ...)` or `//#! name(<arg>, ...)` comments. Pragmas may be disabled entirely using the runtime API.

Pragmas that start with `//#` precede the code they modify, and are referred to as "outer"-style pragmas. Pragmas that start with `//#!` are placed inside the blocks they modify, and are referred to as "inner"-style pragmas. Some pragmas can only be placed at the beginning of files. These pragmas must be in the "inner"-style and precede all nodes in the file, but may be preceded or followed by any number of regular comments.

By default, silkworm will produce warnings when it encounters pragmas that it does not understand.

See [docs/pragmas.md](docs/pragmas.md) for a full list of pragmas supported by silkworm.

### Language features

silkworm's dialect includes a number of useful extensions over Yarn 1.1. These features can be enabled or disabled individually on the runtime, or for individual files using [`feature` pragmas](docs/pragmas.md#feature-disable-feature) at the top of the file.

See [docs/features.md](docs/features.md) for a full list of language features supported by silkworm.

## License

silkworm is available under the MIT license.
