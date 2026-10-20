# cl-xml-kit

`cl-xml-kit` provides bounded XML parsing, pull events, an immutable DOM, serialization, and source-preserving editing for SBCL.

## Dependencies

The runtime dependencies are [`cl-codec-kit`](https://github.com/nerima-lisp/cl-codec-kit) and [`cl-parser-kit`](https://github.com/nerima-lisp/cl-parser-kit). Tests use [`cl-weave`](https://github.com/nerima-lisp/cl-weave), and repository scripts use [`cl-host-kit`](https://github.com/nerima-lisp/cl-host-kit).

## Documentation

The two GitHub links below open files on `main`, the branch used to build the published documentation site.

- [API reference](api.md) lists every exported symbol and its contract.
- [Architecture](architecture.md) covers the parser, data model, public APIs, and resource controls.
- [Migration to 2.0.0](migration.md) records the exported API changes and export comparison.
- [README quick start](https://github.com/nerima-lisp/cl-xml-kit/blob/main/README.md#quick-start) shows how to load the system and parse XML.
- [Benchmark guide](https://github.com/nerima-lisp/cl-xml-kit/blob/main/benchmark/README.md) covers benchmark runs and the XMLConf, coverage, and mutation checks.
