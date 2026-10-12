# cl-xml-kit

`cl-xml-kit` is a Common Lisp XML 1.0 and Namespaces 1.0 toolkit for SBCL.
The library offers bounded parsing, pull events, an immutable DOM, and XML
serialization.

## Install

Load the ASDF system after making the repository visible to ASDF:

```lisp
(asdf:load-system "cl-xml-kit")
```

The runtime dependency is
[`cl-codec-kit`](https://github.com/nerima-lisp/cl-codec-kit). Tests use
[`cl-weave`](https://github.com/nerima-lisp/cl-weave).

## First parse

```lisp
(let ((document
        (cl-xml-kit:parse-xml
         "<root xmlns:p=\"urn:example\"><p:item>text</p:item></root>")))
  (cl-xml-kit:xml->string document))
```

Use `make-xml-parser` and `next-xml-event` for incremental processing. Use
`make-xml-resource-limits` when parsing input supplied by another system.
External entity resolution is rejected by default.

## Reference

The [architecture reference](https://github.com/nerima-lisp/cl-xml-kit/blob/main/docs/design/architecture.md) describes the parser
boundary, node model, event protocol, serialization, security controls, and
verification commands. The repository README covers development and release
commands.
