# cl-xml-kit

`cl-xml-kit` is an SBCL-oriented Common Lisp toolkit for XML 1.0 and Namespaces 1.0.

## Features

- XML 1.0 Fifth Edition well-formedness checks and Namespaces 1.0 processing
- Pull parsing with typed events and an immutable DOM
- Resource limits and external-entity rejection by default
- XML serialization and XML Test Suite Second Canonical Form serialization
- Source-preserving selection, edits, and formatting

## Requirements

- SBCL and ASDF
- Runtime dependencies: [`cl-codec-kit`](https://github.com/nerima-lisp/cl-codec-kit) and [`cl-parser-kit`](https://github.com/nerima-lisp/cl-parser-kit)
- Test dependency: [`cl-weave`](https://github.com/nerima-lisp/cl-weave)
- Repository scripts use [`cl-host-kit`](https://github.com/nerima-lisp/cl-host-kit)

## Quick start

From the repository root, run `nix develop -c sbcl` to start SBCL with the flake's
runtime dependencies available. Without Nix, install `cl-codec-kit` and
`cl-parser-kit` with your Common Lisp dependency manager, or check out their
sources and add the directories containing `cl-codec-kit.asd` and
`cl-parser-kit.asd` to `asdf:*central-registry*` before loading the system. The
snippet adds this checkout to the same registry. Run the snippet from the
repository root so `truename "./"` points to this checkout.

```lisp
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(asdf:load-system "cl-xml-kit")
(let ((document (cl-xml-kit:parse-xml "<root>hello</root>")))
  (write-line (cl-xml-kit:xml->string document)))
```

## Documentation

- [API reference](docs/src/api.md) lists every exported symbol, signature, return value, callback contract, and condition.
- [Architecture](docs/src/architecture.md) describes the parser, data model, APIs, and resource controls.
- [Migration to 2.0.0](docs/src/migration.md) lists the public API signature changes.
- [Benchmark guide](benchmark/README.md) covers benchmark runs and the XMLConf, coverage, and mutation checks.

## Development

CI's `Check flake` step runs `nix flake check --print-build-logs`. The flake defines `checks.xmlconf`, `checks.coverage`, `checks.coverage-scanner-fixtures`, and `checks.parser-kit-load`.

## License

`cl-xml-kit` is released under the MIT License. See [LICENSE](LICENSE).
