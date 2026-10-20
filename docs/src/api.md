# API reference

This page documents every symbol exported by `src/package.lisp`. The lambda lists and behavior below follow the current source. The [Migration to 2.0.0](migration.md) page compares earlier and 2.0.0 source snapshots and describes the change to continuation-passing style (CPS).

Lambda lists use Common Lisp syntax. Callable entries list lambda lists, return values, callback arguments, and conditions; tables state shared details once. Record types and condition types are type designators.

Parser inputs may be strings, character input streams, or octet vectors; source-preserving operations take source strings.

Each source-preserving operation calls exactly one callback: `success` receives the operation result, or `failure` receives the condition. The operation returns the values from the callback it called. Conditions signaled by callbacks propagate unchanged.

The documented immutable records have read-only slots, but string accessors return the stored strings directly, so callers can mutate those strings in place. Document children, element attributes, namespace declarations and children, and event attributes are returned as fresh list spines; changing a returned list does not change the stored list, though its members are shared.

See the architecture's [security and resource limits](architecture.md#3-security-and-resource-limits) for external-entity policies and defaults, and its [processing model](architecture.md#1-scope-and-processing-model) for canonicalization.

## Parsing and events

#### `parse-xml`

- **Lambda list:** `(input &key source-name (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Returns:** an immutable `xml-document` tree.
- **Conditions:** `xml-input-error`, `xml-decode-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, and `xml-resource-limit-exceeded`.

#### `make-xml-parser`

- **Lambda list:** `(input &key source-name (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Returns:** a forward-only parser after input decoding and line-end normalization.
- **Conditions:** `xml-input-error`, `xml-decode-error`, `xml-unsupported-encoding`, and `xml-resource-limit-exceeded`.

#### `next-xml-event`

- **Lambda list:** `(parser)`.
- **Returns:** the next `xml-event`, or `nil` after `:end-document`.
- **Conditions:** `xml-input-error`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-parser-state-error`, and `xml-resource-limit-exceeded`. Any `xml-kit-error` raised while parsing makes the parser fatal; later calls signal `xml-parser-state-error`.

#### `xml-parser-eof-p`

- **Lambda list:** `(parser)`.
- **Returns:** true when the parser has emitted `:end-document`; otherwise false.
- **Conditions:** `xml-input-error` when `parser` is not an XML parser.

#### `map-xml-events`

- **Lambda list:** `(function input &rest parser-options)`.
- **Returns:** `nil` after calling `function` once for each event in document order.
- **Callbacks:** `function` receives one argument, the event; its return value is ignored.
- **Parser options:** `parser-options` are passed to `make-xml-parser` as keyword arguments.
- **Conditions:** `xml-input-error`, `xml-decode-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, and `xml-resource-limit-exceeded`. Conditions signaled by `function` propagate unchanged.

#### `do-xml-events`

- **Lambda list:** `((event input &optional result) &body body)`.
- **Returns:** the value of `result` after the event loop; `event` is bound to each event, then to `nil` while `result` is evaluated.
- **Body:** runs once per event.
- **Conditions:** The parser conditions listed for `map-xml-events`; conditions signaled by `body` propagate unchanged.

#### `xml-event`

Immutable pull-event record type; not callable.

#### `make-xml-event`

- **Lambda list:** `(&key type name attributes text target position external-p)`.
- **Returns:** an immutable `xml-event`; fields not used by `type` are `nil`. `external-p` is ignored; `xml-event-external-p` is true only for `:skipped-external-entity` events.
- **Conditions:** `xml-input-error` for an unknown type or invalid field values.

The accepted `type` keywords and the other fields stored for each type are:

- `:start-document`: `position`
- `:doctype`: `text`, `position`
- `:start-element`: `name`, `attributes`, `position`
- `:characters`: `text`, `position`
- `:comment`: `text`, `position`
- `:processing-instruction`: `text`, `target`, `position`
- `:end-element`: `name`, `position`
- `:skipped-external-entity`: `name`, `position`; `xml-event-external-p` is true
- `:end-document`: `position`

#### `xml-event-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-event`; otherwise false.

### Event accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-event` value.

| Accessor | Returns |
| --- | --- |
| `xml-event-type` | the event type keyword |
| `xml-event-name` | the event `xml-name`, or `nil` |
| `xml-event-attributes` | a fresh list of event attributes |
| `xml-event-text` | the event text string, or `nil` |
| `xml-event-target` | the processing-instruction target, or `nil` |
| `xml-event-position` | the event `xml-position`, or `nil` |
| `xml-event-external-p` | true for a skipped external-entity event; otherwise false |

## Resource limits

#### `*default-xml-resource-limits*`

**Value:** The default immutable `xml-resource-limits` value used by parser and serializer entry points.

#### `make-xml-resource-limits`

- **Lambda list:** `(&key (max-input-octets +default-max-input-octets+) (max-document-characters +default-max-document-characters+) (max-output-characters +default-max-output-characters+) (max-depth +default-max-depth+) (max-elements +default-max-elements+) (max-attributes-per-element +default-max-attributes-per-element+) (max-name-length +default-max-name-length+) (max-text-node-length +default-max-text-node-length+) (max-namespace-bindings +default-max-namespace-bindings+) (max-entity-expansions +default-max-entity-expansions+) (max-expanded-characters +default-max-expanded-characters+) (max-entity-depth +default-max-entity-depth+))`.
- **Returns:** an immutable `xml-resource-limits` value.
- **Conditions:** `xml-invalid-resource-limit` if any value is not a positive integer.

#### `xml-resource-limits-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-resource-limits`; otherwise false.

### Resource-limit accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

| Accessor | Returns |
| --- | --- |
| `xml-resource-limits-max-input-octets` | the maximum octet-vector input size |
| `xml-resource-limits-max-document-characters` | the maximum normalized document character count |
| `xml-resource-limits-max-output-characters` | the maximum serialized output character count |
| `xml-resource-limits-max-depth` | the maximum open-element depth |
| `xml-resource-limits-max-elements` | the maximum number of elements |
| `xml-resource-limits-max-attributes-per-element` | the maximum number of attributes on one element |
| `xml-resource-limits-max-name-length` | the maximum XML name length |
| `xml-resource-limits-max-text-node-length` | the maximum accumulated text-node length |
| `xml-resource-limits-max-namespace-bindings` | the maximum in-scope namespace bindings, excluding the reserved `xml` binding |
| `xml-resource-limits-max-entity-expansions` | the maximum internal entity expansion count |
| `xml-resource-limits-max-expanded-characters` | the maximum characters produced by entity expansion |
| `xml-resource-limits-max-entity-depth` | the maximum nested internal entity expansion depth |

## Positions and names

Position offsets count normalized input characters. `xml-match-start` and `xml-match-end` use indexes in the original source string. These coordinate systems are distinct, and the API provides no conversion between them.

#### `xml-position`

Immutable source-position record type; not callable.

#### `make-xml-position`

- **Lambda list:** `(&key source (offset 0) (line 1) (column 1))`.
- **Returns:** an immutable position; offset is zero-based and line and column are one-based.
- **Conditions:** `xml-input-error` for invalid source, offset, line, or column values.

### Position accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-position` value.

| Accessor | Returns |
| --- | --- |
| `xml-position-source` | the source name, or `nil` |
| `xml-position-offset` | the zero-based offset in normalized characters |
| `xml-position-line` | the one-based line number |
| `xml-position-column` | the one-based column number |

#### `xml-name`

Immutable expanded-name record type; not callable.

#### `make-xml-name`

- **Lambda list:** `(&key local-name (namespace-uri "") prefix)`.
- **Returns:** an immutable expanded name; `prefix` records source spelling and may be `nil`.
- **Conditions:** `xml-input-error` if `local-name` is not a non-empty string or contains a colon, `namespace-uri` is not a string, or `prefix` is neither `nil` nor a string without a colon. It does not validate XML NCName syntax; callers must supply valid local names and prefixes.

### Name accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-name` value.

| Accessor | Returns |
| --- | --- |
| `xml-name-local-name` | the local name |
| `xml-name-namespace-uri` | the namespace URI |
| `xml-name-prefix` | the lexical prefix, or `nil` |

#### `xml-namespace-binding`

Immutable namespace-binding record type; not callable.

#### `make-xml-namespace-binding`

- **Lambda list:** `(&key prefix namespace-uri)`.
- **Returns:** an immutable namespace declaration; `nil` prefix means the default namespace.
- **Conditions:** `xml-input-error` if `prefix` is neither `nil` nor a string, or `namespace-uri` is not a string; `xml-namespace-error` if a non-`nil` prefix has an empty URI. It does not validate XML NCName syntax for `prefix`.

#### `xml-namespace-binding-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-namespace-binding`; otherwise false.

### Namespace-binding accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-namespace-binding` value.

| Accessor | Returns |
| --- | --- |
| `xml-namespace-binding-prefix` | the declared prefix, or `nil` for the default namespace |
| `xml-namespace-binding-namespace-uri` | the namespace URI |

## Tree nodes

#### `xml-document`

Immutable XML document record type; not callable.

#### `make-xml-document`

- **Lambda list:** `(&key children position end-position)`.
- **Returns:** an immutable document whose child list is copied in order; child types are not checked by the constructor.
- **Conditions:** `xml-input-error` for non-list children or positions that are not `nil` or `xml-position` values.

### Document accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-document` value.

| Accessor | Returns |
| --- | --- |
| `xml-document-children` | a fresh list of document children in source order |
| `xml-document-position` | the document start position, or `nil` |
| `xml-document-end-position` | the document end position, or `nil` |

#### `xml-element`

Immutable XML element record type; not callable.

#### `make-xml-element`

- **Lambda list:** `(&key name attributes namespace-declarations children position end-position)`.
- **Returns:** an immutable element with copied ordered lists; collection members are not checked by the constructor.
- **Conditions:** `xml-input-error` if `name` is not an `xml-name`, a collection field is not a list, or either position is not `nil` or an `xml-position`.

### Element accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-element` value.

| Accessor | Returns |
| --- | --- |
| `xml-element-name` | the expanded element name |
| `xml-element-attributes` | a fresh list of attributes in source order |
| `xml-element-namespace-declarations` | a fresh list of namespace declarations in source order |
| `xml-element-children` | a fresh list of child nodes in source order |
| `xml-element-position` | the start position, or `nil` |
| `xml-element-end-position` | the end position, or `nil` |

#### `xml-attribute`

Immutable XML attribute record type; not callable.

#### `make-xml-attribute`

- **Lambda list:** `(&key name value position end-position)`.
- **Returns:** an immutable attribute with a copied value string; XML character validity is checked during serialization.
- **Conditions:** `xml-input-error` if `name` is not an `xml-name`, `value` is not a string, or either position is not `nil` or an `xml-position`.

### Attribute accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-attribute` value.

| Accessor | Returns |
| --- | --- |
| `xml-attribute-name` | the expanded attribute name |
| `xml-attribute-value` | the stored attribute value string. Values parsed from XML are normalized by the parser; `make-xml-attribute` copies its supplied string without normalizing it. |
| `xml-attribute-position` | the start position, or `nil` |
| `xml-attribute-end-position` | the end position, or `nil` |

#### `xml-text`

Immutable XML text record type; not callable.

#### `make-xml-text`

- **Lambda list:** `(&key data position end-position)`.
- **Returns:** an immutable text node with a copied string; XML character validity is checked during serialization.
- **Conditions:** `xml-input-error` if `data` is not a string or either position is not `nil` or an `xml-position`.

### Text accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-text` value.

| Accessor | Returns |
| --- | --- |
| `xml-text-data` | the text data |
| `xml-text-position` | the start position, or `nil` |
| `xml-text-end-position` | the end position, or `nil` |

#### `xml-comment`

Immutable XML comment record type; not callable.

#### `make-xml-comment`

- **Lambda list:** `(&key data position end-position)`.
- **Returns:** an immutable comment node with comment content, without delimiters.
- **Conditions:** `xml-input-error` if `data` is not a string or either position is not `nil` or an `xml-position`. The constructor does not check XML characters or comment syntax; regular serialization rejects data containing `--` or ending in `-`.

### Comment accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-comment` value.

| Accessor | Returns |
| --- | --- |
| `xml-comment-data` | comment data without delimiters |
| `xml-comment-position` | the start position, or `nil` |
| `xml-comment-end-position` | the end position, or `nil` |

#### `xml-processing-instruction`

Immutable processing-instruction record type; not callable.

#### `make-xml-processing-instruction`

- **Lambda list:** `(&key target data position end-position)`.
- **Returns:** an immutable processing-instruction node; `data` may be `nil`.
- **Conditions:** `xml-input-error` if `target` is not a non-empty string without a colon, `data` is neither a string nor `nil`, or either position is not `nil` or an `xml-position`. It does not validate XML Name syntax; serializers reject target `xml` and data containing `?>`.

### Processing-instruction accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-processing-instruction` value.

| Accessor | Returns |
| --- | --- |
| `xml-processing-instruction-target` | the target |
| `xml-processing-instruction-data` | the data string, or `nil` |
| `xml-processing-instruction-position` | the start position, or `nil` |
| `xml-processing-instruction-end-position` | the end position, or `nil` |

#### `xml-doctype`

Immutable doctype record type; not callable.

#### `make-xml-doctype`

- **Lambda list:** `(&key root-name public-id system-id internal-subset position end-position)`.
- **Returns:** an immutable doctype node; it stores identifiers and subset text without resolving external identifiers.
- **Conditions:** `xml-input-error` if `root-name` is not a non-empty string without a colon, an optional identifier or subset is neither a string nor `nil`, or either position is not `nil` or an `xml-position`. It does not validate XML Name syntax for `root-name`.

### Doctype accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-doctype` value.

| Accessor | Returns |
| --- | --- |
| `xml-doctype-root-name` | the declared root name |
| `xml-doctype-public-id` | the public identifier, or `nil` |
| `xml-doctype-system-id` | the system identifier, or `nil` |
| `xml-doctype-internal-subset` | the internal subset, or `nil` |
| `xml-doctype-position` | the start position, or `nil` |
| `xml-doctype-end-position` | the end position, or `nil` |

### Node predicates

Each predicate takes `(value)` and returns true when it is an instance of the listed type; otherwise false.

| Predicate | Type |
| --- | --- |
| `xml-document-p` | `xml-document` |
| `xml-element-p` | `xml-element` |
| `xml-attribute-p` | `xml-attribute` |
| `xml-text-p` | `xml-text` |
| `xml-comment-p` | `xml-comment` |
| `xml-processing-instruction-p` | `xml-processing-instruction` |
| `xml-doctype-p` | `xml-doctype` |

## Serialization

The serializers check node structure, namespace constraints, XML characters, and the comment and processing-instruction restrictions listed below. They do not validate XML name character syntax; callers must provide valid XML names.

When `pretty-print` is true, `xml->string` and `write-xml` add LF line breaks and `indent` spaces around children when an element contains only elements. The inserted whitespace becomes text if the XML is parsed again. They do not add formatting whitespace to mixed content or child lists that contain comments or processing instructions.

#### `xml->string`

- **Lambda list:** `(object &key (pretty-print nil) (indent 2) (resource-limits *default-xml-resource-limits*))`.
- **Returns:** a fresh string containing serialized XML.
- **Conditions:** `xml-serialization-error` if `object` is not an XML node, document or element children are not nodes, element attributes are not attributes, a checked namespace/prefix constraint fails, content contains a character not allowed by XML, comment data contains `--` or ends in `-`, a processing instruction has target `xml` (case-insensitive) or data containing `?>`, `resource-limits` is not an `xml-resource-limits` value, or `indent` is not a non-negative integer; `xml-resource-limit-exceeded` when output exceeds its limit.

#### `xml->canonical-string`

- **Lambda list:** `(object &key (resource-limits *default-xml-resource-limits*))`.
- **Returns:** a fresh string in XML Test Suite Second Canonical Form, with comments omitted.
- **Conditions:** `xml-serialization-error` if `object` is not an XML node, document or element children are not nodes, element attributes are not attributes, a checked namespace/prefix constraint fails, content contains a character not allowed by XML, a processing instruction has target `xml` (case-insensitive) or data containing `?>`, or `resource-limits` is not an `xml-resource-limits` value; `xml-resource-limit-exceeded` when output exceeds its limit.

#### `write-xml`

- **Lambda list:** `(object stream &key (pretty-print nil) (indent 2) (resource-limits *default-xml-resource-limits*))`.
- **Returns:** the character output stream after writing serialized XML.
- **Conditions:** `xml-serialization-error` if `object` is not an XML node, document or element children are not nodes, element attributes are not attributes, a checked namespace/prefix constraint fails, content contains a character not allowed by XML, comment data contains `--` or ends in `-`, a processing instruction has target `xml` (case-insensitive) or data containing `?>`, `stream` is not a stream, `resource-limits` is not an `xml-resource-limits` value, or `indent` is not a non-negative integer; `xml-resource-limit-exceeded` when output exceeds its limit. Stream errors from Common Lisp propagate.

#### `write-xml-canonical`

- **Lambda list:** `(object stream &key (resource-limits *default-xml-resource-limits*))`.
- **Returns:** the character output stream after writing XML Test Suite Second Canonical Form, with comments omitted.
- **Conditions:** `xml-serialization-error` if `object` is not an XML node, document or element children are not nodes, element attributes are not attributes, a checked namespace/prefix constraint fails, content contains a character not allowed by XML, a processing instruction has target `xml` (case-insensitive) or data containing `?>`, `stream` is not a stream, or `resource-limits` is not an `xml-resource-limits` value; `xml-resource-limit-exceeded` when output exceeds its limit. Stream errors from Common Lisp propagate.

## Conditions

| Condition | Inherits from | Meaning |
| --- | --- | --- |
| `xml-kit-error` | `error` | Base condition for conditions signaled by `cl-xml-kit`. |
| `xml-input-error` | `xml-kit-error` | Input conversion or public-argument validation failed. |
| `xml-decode-error` | `xml-input-error` | An octet input could not be decoded. |
| `xml-unsupported-encoding` | `xml-decode-error` | The input encoding is outside the supported profile. |
| `xml-parse-error` | `xml-kit-error` | Base condition for XML grammar and parser-state failures. |
| `xml-well-formedness-error` | `xml-parse-error` | An XML well-formedness constraint failed. |
| `xml-namespace-error` | `xml-parse-error` | A Namespaces in XML constraint failed. |
| `xml-external-resource-disabled` | `xml-parse-error` | An external resource was requested under the rejecting policy. |
| `xml-parser-state-error` | `xml-parse-error` | A parser was used after a prior `cl-xml-kit` condition made it fatal. |
| `xml-resource-limit-exceeded` | `xml-kit-error` | A configured parsing or serialization limit was exceeded. |
| `xml-invalid-resource-limit` | `xml-kit-error` | A resource-limit constructor value was not a positive integer. |
| `xml-serialization-error` | `xml-kit-error` | Signaled for invalid node structure, checked namespace/prefix violations, characters disallowed by XML, an invalid output stream, or invalid options. Regular serialization also rejects comments containing `--` or ending in `-`; both serializers reject reserved PI targets and data containing `?>`. See Serialization for operation-specific conditions. XML name character syntax is not validated. |
| `xml-edit-error` | `xml-kit-error` | Base condition for source-preserving path and edit failures. |
| `xml-path-error` | `xml-edit-error` | A path is malformed, has no single target, or selects the wrong target kind. |
| `xml-unsupported-edit` | `xml-edit-error` | A selection, edit, or formatting operation is unsupported for the document's source structure. |
| `xml-edit-value-error` | `xml-edit-error` | An edit value or fragment was rejected. |

### Condition accessors

These accessors take `(condition)` and signal a Common Lisp `error` if the argument is not compatible with the listed condition type.

| Accessor | Returns | Condition type |
| --- | --- | --- |
| `xml-condition-message` | the diagnostic message string | `xml-kit-error` |
| `xml-condition-position` | the associated `xml-position`, or `nil` | `xml-kit-error` |
| `xml-condition-source` | the caller-provided source name, or `nil` | `xml-kit-error` |
| `xml-decode-error-octet` | the failing octet, or `nil` when unavailable | `xml-decode-error` |
| `xml-decode-error-encoding` | the encoding used for decoding | `xml-decode-error` |
| `xml-unsupported-encoding-encoding` | the unsupported encoding designator or name | `xml-unsupported-encoding` |
| `xml-unsupported-encoding-reason` | the reason the encoding is unsupported, or `nil` | `xml-unsupported-encoding` |
| `xml-well-formedness-error-constraint` | the failed well-formedness constraint, or `nil` | `xml-well-formedness-error` |
| `xml-well-formedness-error-expected` | the expected construct, or `nil` | `xml-well-formedness-error` |
| `xml-namespace-error-prefix` | the involved prefix, or `nil` | `xml-namespace-error` |
| `xml-namespace-error-namespace-uri` | the involved namespace URI, or `nil` | `xml-namespace-error` |
| `xml-external-resource-disabled-identifier` | the external resource identifier, or `nil` | `xml-external-resource-disabled` |
| `xml-external-resource-disabled-kind` | the external resource kind, or `nil` | `xml-external-resource-disabled` |
| `xml-parser-state-error-parser` | the parser that cannot continue, or `nil` | `xml-parser-state-error` |
| `xml-resource-limit-exceeded-kind` | the exceeded limit keyword | `xml-resource-limit-exceeded` |
| `xml-resource-limit-exceeded-value` | the observed value | `xml-resource-limit-exceeded` |
| `xml-resource-limit-exceeded-limit` | the configured limit | `xml-resource-limit-exceeded` |
| `xml-invalid-resource-limit-kind` | the invalid limit keyword | `xml-invalid-resource-limit` |
| `xml-invalid-resource-limit-value` | the invalid configured value | `xml-invalid-resource-limit` |
| `xml-serialization-error-object` | the object that could not be serialized, or `nil` | `xml-serialization-error` |
| `xml-serialization-error-path` | the path to the offending object, or `nil` | `xml-serialization-error` |
| `xml-edit-error-path` | the path supplied by the caller, or `nil` | `xml-edit-error` |
| `xml-edit-error-reason` | the keyword naming why the path or edit was refused, or `nil` | `xml-edit-error` |
| `xml-path-error-index` | the zero-based path index for a syntax error, or `nil` | `xml-path-error` |
| `xml-edit-value-error-cause` | the condition that made the value unacceptable, or `nil` | `xml-edit-value-error` |

## Source-preserving selection and editing

### Path syntax

Paths are absolute. The document-element step is `/QName` or `/*` and accepts no index or `[1]`; later element steps use the same forms and may include `[n]`, a positive one-based index among matching child elements. A path may end with `/@QName` or `/text()`. Names compare lexically as written, including prefixes. This library's path syntax is not XPath.

#### `select-xml`

- **Lambda list:** `(source path success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Callbacks:** `success` receives one argument, the list of selected `xml-match` values (possibly empty). `failure` receives one argument, the condition.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, `xml-path-error`, or `xml-unsupported-edit` to `failure`.

#### `xml-match`

Immutable source-selection match record type; not callable.

#### `xml-match-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-match`; otherwise false.

### Match accessors

Each accessor takes `(record)` and signals Common Lisp `type-error` if `record` is not an `xml-match` value.

| Accessor | Returns |
| --- | --- |
| `xml-match-kind` | `:element`, `:attribute`, or `:text` |
| `xml-match-path` | the canonical path with an index on every element step |
| `xml-match-name` | the lexical qualified name for element or attribute matches, or `nil` |
| `xml-match-value` | the decoded attribute or text value, or `nil` for element matches |
| `xml-match-start` | the inclusive start index in the original source string |
| `xml-match-end` | the exclusive end index in the original source string |
| `xml-match-element` | the matched `xml-element`, or the `xml-element` containing an attribute or text match |

`/text()` returns one match per selected element. Its value concatenates that element's direct text children, while its source span covers the full element content, including child markup. Element matches span the whole element; attribute matches span from the attribute name through its closing quote.

#### `edit-xml-set`

- **Lambda list:** `(source path value success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Callbacks:** `success` receives one argument, the edited source string. `failure` receives one condition argument.
- **Behavior:** Requires exactly one target. For an attribute path, it updates the value and preserves the existing quote, or inserts the missing attribute if the element portion of the path selects exactly one owner. For a text path, it replaces the element content with character data, retaining a sole CDATA section only when `value` contains neither `]]>` nor a carriage return. For an element path, it parses `value` as XML markup and requires exactly one element in the target's in-scope namespace context. `value` must be a string containing only XML characters. Namespace declarations cannot be edited, and text replacement is refused if the content contains an element, comment, processing instruction, or an internal entity reference that expands to markup.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, `xml-path-error`, `xml-unsupported-edit`, or `xml-edit-value-error` to `failure`.

#### `edit-xml-delete`

- **Lambda list:** `(source path success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Callbacks:** `success` receives one argument, the edited source string. `failure` receives one condition argument.
- **Behavior:** Deletes one attribute or element. Deleting an attribute also removes its preceding whitespace. Deleting an element also removes indentation when a preceding sibling is ordinary source text (not CDATA, a character reference, or an entity reference) containing only XML whitespace and the next sibling is absent, non-text, or another such run; it removes spaces or tabs before the element and the preceding line break when present. Namespace declarations and the document element cannot be deleted, and text paths are rejected.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, `xml-path-error`, `xml-unsupported-edit`, or `xml-edit-value-error` to `failure`.

#### `edit-xml-append`

- **Lambda list:** `(source path fragment success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Callbacks:** `success` receives one argument, the edited source string. `failure` receives one condition argument.
- **Behavior:** Requires one selected element; attribute and text paths are rejected. `fragment` must contain exactly one element in the target's namespace context, with surrounding XML whitespace removed. Inserts after the last child except trailing runs of XML whitespace written as ordinary source text (not CDATA, a character reference, or an entity reference), leaving those runs after the new element; when such runs are the only content, they stay before it. Empty elements are expanded, and existing indentation may be reused.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, `xml-path-error`, `xml-unsupported-edit`, or `xml-edit-value-error` to `failure`.

#### `format-xml`

- **Lambda list:** `(source success failure &key (indent 2) (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Callbacks:** `success` receives one argument, the formatted source string. `failure` receives one condition argument.
- **Behavior:** For an element with at least one element, comment, or processing-instruction child and no non-whitespace text, block layout removes whitespace-only text and puts each remaining child on its own line. It is suppressed within mixed content and under `xml:space="preserve"`; the nearest `xml:space="default"` resets preservation from an ancestor. The XML declaration, DOCTYPE, and leading BOM are retained; new line breaks use the first style in the source, or LF if there is none. Attribute quotes and syntax spacing are normalized, and CDATA becomes text. The output is reparsed; its tree must match the source except for source positions, merged adjacent text nodes, and whitespace-only text removed by layout.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, or `xml-unsupported-edit` to `failure`.

