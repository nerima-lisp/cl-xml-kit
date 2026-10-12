# cl-xml-kit

`cl-xml-kit` is a Common Lisp XML 1.0 and Namespaces 1.0 toolkit for SBCL.
It provides bounded parsing, pull events, an immutable DOM, and XML
serialization.

## Features

- XML 1.0 Fifth Edition well-formedness checks
- Namespaces 1.0 namespace expansion and duplicate expanded-name detection
- Input from strings, character streams, and UTF-8 octet vectors
- Pull parsing with typed events and source positions
- Immutable document and element nodes
- XML and canonical serialization
- Resource limits and external-entity rejection by default

The public API is documented by the exports in `src/package.lisp` and the
design reference in [docs/design/architecture.md](docs/design/architecture.md).

## Requirements

- SBCL
- ASDF
- [`cl-codec-kit`](https://github.com/nerima-lisp/cl-codec-kit) at runtime
- [`cl-weave`](https://github.com/nerima-lisp/cl-weave) for the test system

External entity resolution, DTD validation, XPath, and XSLT are outside the
scope of this release. Resource limits should be configured for untrusted
inputs.

## Quick start

```lisp
(asdf:load-system "cl-xml-kit")

(let* ((document (cl-xml-kit:parse-xml
                  "<root xmlns:p=\"urn:example\"><p:item>text</p:item></root>"))
       (xml (cl-xml-kit:xml->string document)))
  (write-line xml))
```

For incremental processing, construct an `xml-parser` with
`make-xml-parser` and call `next-xml-event` until `xml-parser-eof-p` is true.
Use `make-xml-resource-limits` to bound depth, input size, attributes, and
other parser resources.

## Development

Run the test system with:

```sh
sbcl --script run-tests.lisp
```

The reproducible Nix checks are:

```sh
nix flake check --all-systems --no-write-lock-file --print-build-logs --keep-going
```

Build the documentation locally with:

```sh
mkdocs build -f docs/mkdocs.yml --strict
```

The XMLConf catalog profile can be audited with the XMLConf runner described
in [benchmark/README.md](benchmark/README.md).

## License

`cl-xml-kit` is released under the MIT License. See [LICENSE](LICENSE).
