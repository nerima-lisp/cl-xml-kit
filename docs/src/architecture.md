# cl-xml-kit architecture and API

## 1. Scope and processing model

`cl-xml-kit` implements non-validating XML 1.0 processing and Namespaces in XML 1.0 processing for Common Lisp. It exposes a pull event reader, a DOM-style tree, XML serialization, and source-preserving selection and editing operations.

The parser accepts strings, character input streams, and octet vectors. Source-preserving operations accept a source string so they can retain unchanged lexical text.

The library does not validate DTD declarations, resolve external resources, or implement XPath, XSLT, XInclude, XML Catalogs, or schema languages. Its canonical serializer uses the XML Test Suite Second Canonical Form. That format is distinct from W3C Canonical XML.

### Parsing flow

The tokenizer uses `cl-parser-kit` modes for XML content and tag syntax. Content mode recognizes character data and markup such as references, comments, CDATA sections, processing instructions, and doctypes. An element tag opener switches tokenization to tag syntax; the tokenizer returns to content after the tag closes. The XML parser applies document-level or element-content rules to the resulting tokens.

`make-xml-parser` decodes and normalizes the complete input before returning a parser, so encoding or decoding errors anywhere in the input are signaled before it returns. Calls to `next-xml-event` then tokenize and parse only as far as the next event; a well-formedness error after valid content is signaled only when the reader reaches it.

`parse-xml` builds a document tree from the same parser. `map-xml-events` calls a function for each event, while `do-xml-events` provides iteration syntax over those events.

The source-preserving operations are `select-xml`, `edit-xml-set`, `edit-xml-delete`, `edit-xml-append`, and `format-xml`. They combine parsed structure with source spans. Each operation takes `success` and `failure` callbacks. The operation calls `success` once with the result or `failure` once with the condition, then returns the values from the callback it called.

The pull reader emits these event types. Fields not listed for an event are `nil`.

| Type | Meaning | Fields |
| --- | --- | --- |
| `:start-document` | Input is ready for parsing | position |
| `:doctype` | A doctype was read without DTD validation | text, position |
| `:start-element` | An element start tag was read | name, attributes, position |
| `:characters` | Character data was read | text, position |
| `:comment` | A comment was read | text, position |
| `:processing-instruction` | A processing instruction was read | target, text, position |
| `:end-element` | A matching end tag was read | name, position |
| `:skipped-external-entity` | An external entity reference was skipped | name, position, external-p |
| `:end-document` | The document has no more events | position |

The default external-resource policy is `:reject`. The `:skip` policy reports skipped references as `:skipped-external-entity` events and does not insert external replacement text.

## 2. XML and namespace conformance boundary

The parser follows XML 1.0 Fifth Edition and Namespaces in XML 1.0 Third Edition. It checks XML well-formedness and namespace constraints, but it does not perform DTD validity checking or apply declarations from external subsets.

Names use the XML 1.0 name productions. The parser resolves element and attribute names to namespace URI, local name, and lexical prefix. The default namespace applies to unprefixed elements, not to unprefixed attributes. It detects duplicate attributes by expanded name.

Input line endings convert CRLF and CR to LF before grammar parsing. Attribute values undergo XML whitespace and reference normalization. A parsed DOM coalesces adjacent character data into text nodes.

### Encoding profile

Character strings and character streams are already decoded. Their XML declaration may omit an encoding or specify UTF-8.

Octet input supports UTF-8, with or without a BOM, and UTF-16 when a BOM selects the byte order. The parser consumes the BOM. UTF-16 without a BOM and UTF-32 input are unsupported. An XML declaration that conflicts with the detected input encoding signals `xml-unsupported-encoding`.

## 3. Security and resource limits

The parser never reads an external DTD or external general or parameter entity. Under the default `:reject` policy, an external reference signals `xml-external-resource-disabled`. Under `:skip`, the parser omits external replacement text and reports the skipped reference to event consumers. No resolver callback or network access is available through the parsing API.

Resource limits are configured with an immutable `xml-resource-limits` value. The default values are:

`max-input-octets` applies to octet-vector input. Strings and character streams are bounded by `max-document-characters` after line-end normalization.

| Limit | Default | Counts |
| --- | ---: | --- |
| `max-input-octets` | 16,777,216 | Octets in one octet-vector input, including a BOM |
| `max-document-characters` | 16,777,216 | Decoded characters after line-end normalization |
| `max-output-characters` | 16,777,216 | Serialized output characters |
| `max-depth` | 512 | Open element depth |
| `max-elements` | 1,000,000 | Elements in one document |
| `max-attributes-per-element` | 1,024 | Attributes on one element |
| `max-name-length` | 1,024 | Characters in an XML name |
| `max-text-node-length` | 1,048,576 | Characters in one accumulated text node |
| `max-namespace-bindings` | 1,024 | In-scope namespace bindings, excluding the reserved `xml` binding |
| `max-entity-expansions` | 10,000 | Internal entity expansions |
| `max-expanded-characters` | 16,777,216 | Characters produced by entity expansion |
| `max-entity-depth` | 32 | Nested internal entity expansion depth |

Each configured limit is a positive integer. When a parse or serialization operation exceeds a configured limit, it signals `xml-resource-limit-exceeded`.

## 4. Data model and conditions

An `xml-name` stores a local name, namespace URI, and optional prefix. The prefix records the spelling in the source; namespace identity uses the URI and local name.

An `xml-position` stores the optional source name, a zero-based offset in normalized characters, and one-based line and column values. Parsed nodes and events carry positions where the input provides one.

The document tree preserves child and attribute order. It represents elements, attributes, text, comments, processing instructions, and doctypes as separate node types. Parsing coalesces adjacent character data. Event values are snapshots of the fields applicable to their event type.

The core input, parse, resource-limit, and serialization condition branches are:

```text
xml-kit-error
|- xml-input-error
|  `- xml-decode-error
|     `- xml-unsupported-encoding
|- xml-parse-error
|  |- xml-well-formedness-error
|  |- xml-namespace-error
|  |- xml-external-resource-disabled
|  `- xml-parser-state-error
|- xml-resource-limit-exceeded
|- xml-invalid-resource-limit
`- xml-serialization-error
```

Source-preserving operations also use the exported `xml-edit-error` family, including `xml-path-error`, `xml-unsupported-edit`, and `xml-edit-value-error`.

`xml->string` and `write-xml` serialize a tree as XML. Pretty printing inserts line breaks and indentation only when every child node is an element; it does not infer element-only content from DTD declarations. If an element has text, comments, or processing instructions among its children, pretty printing adds no breaks within that element. `xml->canonical-string` and `write-xml-canonical` produce XML Test Suite Second Canonical Form.

## 5. Dependencies and project structure

The ASDF systems declare these direct dependencies:

| System | Dependencies |
| --- | --- |
| `cl-xml-kit` | `cl-codec-kit`, `cl-parser-kit` |
| `cl-xml-kit/test` | `cl-xml-kit`, `cl-weave` |
| `cl-xml-kit/benchmark` | `cl-xml-kit`, `cl-weave`, `cl-host-kit` |

Repository scripts that use `scripts/script-support.lisp` also require `cl-host-kit`. The runtime, test, and benchmark component order is declared in `cl-xml-kit.asd`.

## References

- [XML 1.0 Fifth Edition](https://www.w3.org/TR/2008/REC-xml-20081126/)
- [Namespaces in XML 1.0 Third Edition](https://www.w3.org/TR/2009/REC-xml-names-20091208/)
