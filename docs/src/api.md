# API reference

This page documents every symbol exported by `src/package.lisp`. The lambda lists and behavior below follow the current source. For the changes from the pre-CPS source snapshot, see [Migration to 2.0.0](migration.md).

Lambda lists use Common Lisp syntax. Callable entries list the lambda list, return value, callback arguments, and conditions. Record types and condition types are type designators; the default resource-limits variable holds a value rather than naming a function.

Each source-preserving operation calls exactly one callback: `success` receives the operation result, or `failure` receives the condition. The operation returns the values from the callback it called. Conditions signaled by callbacks propagate unchanged. Condition accessor functions signal a Common Lisp `error` for incompatible arguments; the API does not promise a more specific class.

## Parsing and events

#### `parse-xml`

- **Lambda list:** `(input &key source-name (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Returns:** an immutable `xml-document` tree.
- **Callbacks:** none.
- **Conditions:** `xml-input-error`, `xml-decode-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, and `xml-resource-limit-exceeded`.

#### `make-xml-parser`

- **Lambda list:** `(input &key source-name (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Returns:** a forward-only parser after input decoding and line-end normalization.
- **Callbacks:** none.
- **Conditions:** `xml-input-error`, `xml-decode-error`, `xml-unsupported-encoding`, and `xml-resource-limit-exceeded`.

#### `next-xml-event`

- **Lambda list:** `(parser)`.
- **Returns:** the next `xml-event`, or `nil` after `:end-document`.
- **Callbacks:** none.
- **Conditions:** `xml-input-error`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-parser-state-error` after a prior `cl-xml-kit` condition made the parser fatal, and `xml-resource-limit-exceeded`.

#### `xml-parser-eof-p`

- **Lambda list:** `(parser)`.
- **Returns:** true when the parser has emitted `:end-document`; otherwise false.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` when `parser` is not an XML parser.

#### `map-xml-events`

- **Lambda list:** `(function input &rest parser-options)`.
- **Returns:** `nil` after calling `function` once for each event in document order.
- **Callbacks:** `function` receives one argument, the event; its return value is ignored.
- **Conditions:** `xml-input-error`, `xml-decode-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, and `xml-resource-limit-exceeded`. Conditions signaled by `function` propagate unchanged.

#### `do-xml-events`

- **Lambda list:** `((event input &optional result) &body body)`.
- **Returns:** the value of `result` after the event loop; `event` is bound to each event, then to `nil` while `result` is evaluated.
- **Callbacks:** none; `body` runs once per event.
- **Conditions:** The parser conditions listed for `map-xml-events`; conditions signaled by `body` propagate unchanged.

#### `xml-event`

- **Lambda list:** None; this is an immutable pull-event record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-event`

- **Lambda list:** `(&key type name attributes text target position external-p)`.
- **Returns:** an immutable `xml-event`; fields not used by `type` are `nil`. The `external-p` argument is accepted but the stored value follows the event type.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` for an unknown type or invalid field values.

#### `xml-event-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-event`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-event-type`

- **Lambda list:** `(record)`.
- **Returns:** the event type keyword.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-event` value.

#### `xml-event-name`

- **Lambda list:** `(record)`.
- **Returns:** the event `xml-name`, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-event` value.

#### `xml-event-attributes`

- **Lambda list:** `(record)`.
- **Returns:** a fresh list of event attributes.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-event` value.

#### `xml-event-text`

- **Lambda list:** `(record)`.
- **Returns:** the event text string, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-event` value.

#### `xml-event-target`

- **Lambda list:** `(record)`.
- **Returns:** the processing-instruction target, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-event` value.

#### `xml-event-position`

- **Lambda list:** `(record)`.
- **Returns:** the event `xml-position`, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-event` value.

#### `xml-event-external-p`

- **Lambda list:** `(record)`.
- **Returns:** true for a skipped external-entity event; otherwise false.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-event` value.

## Resource limits

#### `*default-xml-resource-limits*`

- **Lambda list:** Not applicable; this is a special variable.
- **Returns:** Its value is the default immutable `xml-resource-limits` value used by parser and serializer entry points.
- **Callbacks:** None.
- **Conditions:** Reading the variable does not signal a `cl-xml-kit` condition.

#### `make-xml-resource-limits`

- **Lambda list:** `(&key (max-input-octets +default-max-input-octets+) (max-document-characters +default-max-document-characters+) (max-output-characters +default-max-output-characters+) (max-depth +default-max-depth+) (max-elements +default-max-elements+) (max-attributes-per-element +default-max-attributes-per-element+) (max-name-length +default-max-name-length+) (max-text-node-length +default-max-text-node-length+) (max-namespace-bindings +default-max-namespace-bindings+) (max-entity-expansions +default-max-entity-expansions+) (max-expanded-characters +default-max-expanded-characters+) (max-entity-depth +default-max-entity-depth+))`.
- **Returns:** an immutable `xml-resource-limits` value.
- **Callbacks:** none.
- **Conditions:** `xml-invalid-resource-limit` if any value is not a positive integer.

#### `xml-resource-limits-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-resource-limits`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-resource-limits-max-input-octets`

- **Lambda list:** `(record)`.
- **Returns:** the maximum octet-vector input size.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-document-characters`

- **Lambda list:** `(record)`.
- **Returns:** the maximum normalized document character count.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-output-characters`

- **Lambda list:** `(record)`.
- **Returns:** the maximum serialized output character count.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-depth`

- **Lambda list:** `(record)`.
- **Returns:** the maximum open-element depth.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-elements`

- **Lambda list:** `(record)`.
- **Returns:** the maximum number of elements.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-attributes-per-element`

- **Lambda list:** `(record)`.
- **Returns:** the maximum number of attributes on one element.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-name-length`

- **Lambda list:** `(record)`.
- **Returns:** the maximum XML name length.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-text-node-length`

- **Lambda list:** `(record)`.
- **Returns:** the maximum accumulated text-node length.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-namespace-bindings`

- **Lambda list:** `(record)`.
- **Returns:** the maximum in-scope namespace bindings, excluding the reserved `xml` binding.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-entity-expansions`

- **Lambda list:** `(record)`.
- **Returns:** the maximum internal entity expansion count.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-expanded-characters`

- **Lambda list:** `(record)`.
- **Returns:** the maximum characters produced by entity expansion.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

#### `xml-resource-limits-max-entity-depth`

- **Lambda list:** `(record)`.
- **Returns:** the maximum nested internal entity expansion depth.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-resource-limits` value.

## Positions and names

Position offsets count normalized input characters. `xml-match-start` and `xml-match-end` use indexes in the original source string. These coordinate systems are distinct, and the API provides no conversion between them.

#### `xml-position`

- **Lambda list:** None; this is an immutable source-position record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-position`

- **Lambda list:** `(&key source (offset 0) (line 1) (column 1))`.
- **Returns:** an immutable position; offset is zero-based and line and column are one-based.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` for invalid source, offset, line, or column values.

#### `xml-position-source`

- **Lambda list:** `(record)`.
- **Returns:** the source name, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-position` value.

#### `xml-position-offset`

- **Lambda list:** `(record)`.
- **Returns:** the zero-based offset in normalized characters.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-position` value.

#### `xml-position-line`

- **Lambda list:** `(record)`.
- **Returns:** the one-based line number.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-position` value.

#### `xml-position-column`

- **Lambda list:** `(record)`.
- **Returns:** the one-based column number.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-position` value.

#### `xml-name`

- **Lambda list:** None; this is an immutable expanded-name record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-name`

- **Lambda list:** `(&key local-name (namespace-uri "") prefix)`.
- **Returns:** an immutable expanded name; `prefix` records source spelling and may be `nil`.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` if `local-name` is not a non-empty string or contains a colon, `namespace-uri` is not a string, or `prefix` is neither `nil` nor a string without a colon. It does not validate XML NCName syntax; callers must supply valid local names and prefixes.

#### `xml-name-local-name`

- **Lambda list:** `(record)`.
- **Returns:** the local name.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-name` value.

#### `xml-name-namespace-uri`

- **Lambda list:** `(record)`.
- **Returns:** the namespace URI.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-name` value.

#### `xml-name-prefix`

- **Lambda list:** `(record)`.
- **Returns:** the lexical prefix, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-name` value.

#### `xml-namespace-binding`

- **Lambda list:** None; this is an immutable namespace-binding record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-namespace-binding`

- **Lambda list:** `(&key prefix namespace-uri)`.
- **Returns:** an immutable namespace declaration; `nil` prefix means the default namespace.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` if `prefix` is neither `nil` nor a string, or `namespace-uri` is not a string; `xml-namespace-error` if a non-`nil` prefix has an empty URI. It does not validate XML NCName syntax for `prefix`.

#### `xml-namespace-binding-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-namespace-binding`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-namespace-binding-prefix`

- **Lambda list:** `(record)`.
- **Returns:** the declared prefix, or `nil` for the default namespace.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-namespace-binding` value.

#### `xml-namespace-binding-namespace-uri`

- **Lambda list:** `(record)`.
- **Returns:** the namespace URI.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-namespace-binding` value.

## Tree nodes

#### `xml-document`

- **Lambda list:** None; this is an immutable XML document record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-document`

- **Lambda list:** `(&key children position end-position)`.
- **Returns:** an immutable document whose child list is copied in order; child types are not checked by the constructor.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` for non-list children or positions that are not `nil` or `xml-position` values.

#### `xml-document-children`

- **Lambda list:** `(record)`.
- **Returns:** a fresh list of document children in source order.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-document` value.

#### `xml-document-position`

- **Lambda list:** `(record)`.
- **Returns:** the document start position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-document` value.

#### `xml-document-end-position`

- **Lambda list:** `(record)`.
- **Returns:** the document end position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-document` value.

#### `xml-element`

- **Lambda list:** None; this is an immutable XML element record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-element`

- **Lambda list:** `(&key name attributes namespace-declarations children position end-position)`.
- **Returns:** an immutable element with copied ordered lists; collection members are not checked by the constructor.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` if `name` is not an `xml-name`, a collection field is not a list, or either position is not `nil` or an `xml-position`.

#### `xml-element-name`

- **Lambda list:** `(record)`.
- **Returns:** the expanded element name.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-element` value.

#### `xml-element-attributes`

- **Lambda list:** `(record)`.
- **Returns:** a fresh list of attributes in source order.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-element` value.

#### `xml-element-namespace-declarations`

- **Lambda list:** `(record)`.
- **Returns:** a fresh list of namespace declarations in source order.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-element` value.

#### `xml-element-children`

- **Lambda list:** `(record)`.
- **Returns:** a fresh list of child nodes in source order.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-element` value.

#### `xml-element-position`

- **Lambda list:** `(record)`.
- **Returns:** the start position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-element` value.

#### `xml-element-end-position`

- **Lambda list:** `(record)`.
- **Returns:** the end position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-element` value.

#### `xml-attribute`

- **Lambda list:** None; this is an immutable XML attribute record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-attribute`

- **Lambda list:** `(&key name value position end-position)`.
- **Returns:** an immutable attribute with a copied value string; XML character validity is checked during serialization.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` if `name` is not an `xml-name`, `value` is not a string, or either position is not `nil` or an `xml-position`.

#### `xml-attribute-name`

- **Lambda list:** `(record)`.
- **Returns:** the expanded attribute name.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-attribute` value.

#### `xml-attribute-value`

- **Lambda list:** `(record)`.
- **Returns:** the stored attribute value string. Values parsed from XML are normalized by the parser; `make-xml-attribute` copies its supplied string without normalizing it.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-attribute` value.

#### `xml-attribute-position`

- **Lambda list:** `(record)`.
- **Returns:** the start position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-attribute` value.

#### `xml-attribute-end-position`

- **Lambda list:** `(record)`.
- **Returns:** the end position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-attribute` value.

#### `xml-text`

- **Lambda list:** None; this is an immutable XML text record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-text`

- **Lambda list:** `(&key data position end-position)`.
- **Returns:** an immutable text node with a copied string; XML character validity is checked during serialization.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` if `data` is not a string or either position is not `nil` or an `xml-position`.

#### `xml-text-data`

- **Lambda list:** `(record)`.
- **Returns:** the text data.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-text` value.

#### `xml-text-position`

- **Lambda list:** `(record)`.
- **Returns:** the start position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-text` value.

#### `xml-text-end-position`

- **Lambda list:** `(record)`.
- **Returns:** the end position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-text` value.

#### `xml-comment`

- **Lambda list:** None; this is an immutable XML comment record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-comment`

- **Lambda list:** `(&key data position end-position)`.
- **Returns:** an immutable comment node with comment content, without delimiters.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` if `data` is not a string or either position is not `nil` or an `xml-position`. The constructor does not check XML characters or comment syntax; regular serialization rejects data containing `--` or ending in `-`.

#### `xml-comment-data`

- **Lambda list:** `(record)`.
- **Returns:** comment data without delimiters.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-comment` value.

#### `xml-comment-position`

- **Lambda list:** `(record)`.
- **Returns:** the start position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-comment` value.

#### `xml-comment-end-position`

- **Lambda list:** `(record)`.
- **Returns:** the end position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-comment` value.

#### `xml-processing-instruction`

- **Lambda list:** None; this is an immutable processing-instruction record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-processing-instruction`

- **Lambda list:** `(&key target data position end-position)`.
- **Returns:** an immutable processing-instruction node; `data` may be `nil`.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` if `target` is not a non-empty string without a colon, `data` is neither a string nor `nil`, or either position is not `nil` or an `xml-position`. It does not validate XML Name syntax; serializers reject target `xml` and data containing `?>`.

#### `xml-processing-instruction-target`

- **Lambda list:** `(record)`.
- **Returns:** the target.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-processing-instruction` value.

#### `xml-processing-instruction-data`

- **Lambda list:** `(record)`.
- **Returns:** the data string, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-processing-instruction` value.

#### `xml-processing-instruction-position`

- **Lambda list:** `(record)`.
- **Returns:** the start position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-processing-instruction` value.

#### `xml-processing-instruction-end-position`

- **Lambda list:** `(record)`.
- **Returns:** the end position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-processing-instruction` value.

#### `xml-doctype`

- **Lambda list:** None; this is an immutable doctype record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `make-xml-doctype`

- **Lambda list:** `(&key root-name public-id system-id internal-subset position end-position)`.
- **Returns:** an immutable doctype node; it stores identifiers and subset text without resolving external identifiers.
- **Callbacks:** none.
- **Conditions:** `xml-input-error` if `root-name` is not a non-empty string without a colon, an optional identifier or subset is neither a string nor `nil`, or either position is not `nil` or an `xml-position`. It does not validate XML Name syntax for `root-name`.

#### `xml-doctype-root-name`

- **Lambda list:** `(record)`.
- **Returns:** the declared root name.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-doctype` value.

#### `xml-doctype-public-id`

- **Lambda list:** `(record)`.
- **Returns:** the public identifier, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-doctype` value.

#### `xml-doctype-system-id`

- **Lambda list:** `(record)`.
- **Returns:** the system identifier, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-doctype` value.

#### `xml-doctype-internal-subset`

- **Lambda list:** `(record)`.
- **Returns:** the internal subset, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-doctype` value.

#### `xml-doctype-position`

- **Lambda list:** `(record)`.
- **Returns:** the start position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-doctype` value.

#### `xml-doctype-end-position`

- **Lambda list:** `(record)`.
- **Returns:** the end position, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-doctype` value.

#### `xml-document-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-document`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-element-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-element`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-attribute-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-attribute`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-text-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-text`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-comment-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-comment`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-processing-instruction-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-processing-instruction`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-doctype-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-doctype`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

## Serialization

The serializers check node structure, namespace constraints, XML characters, and the comment and processing-instruction restrictions listed below. They do not validate XML name character syntax; callers must provide valid XML names.

#### `xml->string`

- **Lambda list:** `(object &key (pretty-print nil) (indent 2) (resource-limits *default-xml-resource-limits*))`.
- **Returns:** a fresh string containing serialized XML.
- **Callbacks:** none.
- **Conditions:** `xml-serialization-error` if `object` is not an XML node, document or element children are not nodes, element attributes are not attributes, a checked namespace/prefix constraint fails, content contains a character not allowed by XML, comment data contains `--` or ends in `-`, a processing instruction has target `xml` (case-insensitive) or data containing `?>`, `resource-limits` is not an `xml-resource-limits` value, or `indent` is not a non-negative integer; `xml-resource-limit-exceeded` when output exceeds its limit.

#### `xml->canonical-string`

- **Lambda list:** `(object &key (resource-limits *default-xml-resource-limits*))`.
- **Returns:** a fresh string in XML Test Suite Second Canonical Form, with comments omitted.
- **Callbacks:** none.
- **Conditions:** `xml-serialization-error` if `object` is not an XML node, document or element children are not nodes, element attributes are not attributes, a checked namespace/prefix constraint fails, content contains a character not allowed by XML, a processing instruction has target `xml` (case-insensitive) or data containing `?>`, or `resource-limits` is not an `xml-resource-limits` value; `xml-resource-limit-exceeded` when output exceeds its limit.

#### `write-xml`

- **Lambda list:** `(object stream &key (pretty-print nil) (indent 2) (resource-limits *default-xml-resource-limits*))`.
- **Returns:** the character output stream after writing serialized XML.
- **Callbacks:** none.
- **Conditions:** `xml-serialization-error` if `object` is not an XML node, document or element children are not nodes, element attributes are not attributes, a checked namespace/prefix constraint fails, content contains a character not allowed by XML, comment data contains `--` or ends in `-`, a processing instruction has target `xml` (case-insensitive) or data containing `?>`, `stream` is not a stream, `resource-limits` is not an `xml-resource-limits` value, or `indent` is not a non-negative integer; `xml-resource-limit-exceeded` when output exceeds its limit. Stream errors from Common Lisp propagate.

#### `write-xml-canonical`

- **Lambda list:** `(object stream &key (resource-limits *default-xml-resource-limits*))`.
- **Returns:** the character output stream after writing XML Test Suite Second Canonical Form, with comments omitted.
- **Callbacks:** none.
- **Conditions:** `xml-serialization-error` if `object` is not an XML node, document or element children are not nodes, element attributes are not attributes, a checked namespace/prefix constraint fails, content contains a character not allowed by XML, a processing instruction has target `xml` (case-insensitive) or data containing `?>`, `stream` is not a stream, or `resource-limits` is not an `xml-resource-limits` value; `xml-resource-limit-exceeded` when output exceeds its limit. Stream errors from Common Lisp propagate.

## Conditions

#### `xml-kit-error`

- **Lambda list:** None; this is a condition type inheriting from `error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** Base condition for conditions signaled by cl-xml-kit.

#### `xml-input-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-kit-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** Input conversion or public-argument validation failed.

#### `xml-decode-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-input-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** An octet input could not be decoded.

#### `xml-unsupported-encoding`

- **Lambda list:** None; this is a condition type inheriting from `xml-decode-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The input encoding is outside the supported profile.

#### `xml-parse-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-kit-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** Base condition for XML grammar and parser-state failures.

#### `xml-well-formedness-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-parse-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** An XML well-formedness constraint failed.

#### `xml-namespace-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-parse-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** A Namespaces in XML constraint failed.

#### `xml-external-resource-disabled`

- **Lambda list:** None; this is a condition type inheriting from `xml-parse-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** An external resource was requested under the rejecting policy.

#### `xml-parser-state-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-parse-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** A parser was used after a prior `cl-xml-kit` condition made it fatal.

#### `xml-resource-limit-exceeded`

- **Lambda list:** None; this is a condition type inheriting from `xml-kit-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** A configured parsing or serialization limit was exceeded.

#### `xml-invalid-resource-limit`

- **Lambda list:** None; this is a condition type inheriting from `xml-kit-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** A resource-limit constructor value was not a positive integer.

#### `xml-serialization-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-kit-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** Signaled for invalid node structure, checked namespace/prefix violations, characters disallowed by XML, an invalid output stream, or invalid options. Regular serialization also rejects comments containing `--` or ending in `-`; both serializers reject reserved PI targets and data containing `?>`. See Serialization for operation-specific conditions. XML name character syntax is not validated.

#### `xml-condition-message`

- **Lambda list:** `(condition)`.
- **Returns:** the diagnostic message string.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-kit-error` condition.

#### `xml-condition-position`

- **Lambda list:** `(condition)`.
- **Returns:** the associated `xml-position`, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-kit-error` condition.

#### `xml-condition-source`

- **Lambda list:** `(condition)`.
- **Returns:** the caller-provided source name, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-kit-error` condition.

#### `xml-decode-error-octet`

- **Lambda list:** `(condition)`.
- **Returns:** the failing octet, or `nil` when unavailable.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-decode-error` condition.

#### `xml-decode-error-encoding`

- **Lambda list:** `(condition)`.
- **Returns:** the encoding used for decoding.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-decode-error` condition.

#### `xml-unsupported-encoding-encoding`

- **Lambda list:** `(condition)`.
- **Returns:** the unsupported encoding designator or name.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-unsupported-encoding` condition.

#### `xml-unsupported-encoding-reason`

- **Lambda list:** `(condition)`.
- **Returns:** the reason the encoding is unsupported, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-unsupported-encoding` condition.

#### `xml-well-formedness-error-constraint`

- **Lambda list:** `(condition)`.
- **Returns:** the failed well-formedness constraint, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-well-formedness-error` condition.

#### `xml-well-formedness-error-expected`

- **Lambda list:** `(condition)`.
- **Returns:** the expected construct, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-well-formedness-error` condition.

#### `xml-namespace-error-prefix`

- **Lambda list:** `(condition)`.
- **Returns:** the involved prefix, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-namespace-error` condition.

#### `xml-namespace-error-namespace-uri`

- **Lambda list:** `(condition)`.
- **Returns:** the involved namespace URI, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-namespace-error` condition.

#### `xml-external-resource-disabled-identifier`

- **Lambda list:** `(condition)`.
- **Returns:** the external resource identifier, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-external-resource-disabled` condition.

#### `xml-external-resource-disabled-kind`

- **Lambda list:** `(condition)`.
- **Returns:** the external resource kind, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-external-resource-disabled` condition.

#### `xml-parser-state-error-parser`

- **Lambda list:** `(condition)`.
- **Returns:** the parser that cannot continue, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-parser-state-error` condition.

#### `xml-resource-limit-exceeded-kind`

- **Lambda list:** `(condition)`.
- **Returns:** the exceeded limit keyword.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-resource-limit-exceeded` condition.

#### `xml-resource-limit-exceeded-value`

- **Lambda list:** `(condition)`.
- **Returns:** the observed value.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-resource-limit-exceeded` condition.

#### `xml-resource-limit-exceeded-limit`

- **Lambda list:** `(condition)`.
- **Returns:** the configured limit.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-resource-limit-exceeded` condition.

#### `xml-invalid-resource-limit-kind`

- **Lambda list:** `(condition)`.
- **Returns:** the invalid limit keyword.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-invalid-resource-limit` condition.

#### `xml-invalid-resource-limit-value`

- **Lambda list:** `(condition)`.
- **Returns:** the invalid configured value.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-invalid-resource-limit` condition.

#### `xml-serialization-error-object`

- **Lambda list:** `(condition)`.
- **Returns:** the object that could not be serialized, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-serialization-error` condition.

#### `xml-serialization-error-path`

- **Lambda list:** `(condition)`.
- **Returns:** the path to the offending object, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-serialization-error` condition.

#### `xml-edit-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-kit-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** Base condition for source-preserving path and edit failures.

#### `xml-path-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-edit-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** A path is malformed, has no single target, or selects the wrong target kind.

#### `xml-unsupported-edit`

- **Lambda list:** None; this is a condition type inheriting from `xml-edit-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** A selection, edit, or formatting operation is unsupported for the document's source structure.

#### `xml-edit-value-error`

- **Lambda list:** None; this is a condition type inheriting from `xml-edit-error`, not callable. **Returns/callbacks:** N/A.
- **Conditions:** An edit value or fragment was rejected.

#### `xml-edit-error-path`

- **Lambda list:** `(condition)`.
- **Returns:** the path supplied by the caller, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-edit-error` condition.

#### `xml-edit-error-reason`

- **Lambda list:** `(condition)`.
- **Returns:** the keyword naming why the path or edit was refused, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-edit-error` condition.

#### `xml-path-error-index`

- **Lambda list:** `(condition)`.
- **Returns:** the zero-based path index for a syntax error, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-path-error` condition.

#### `xml-edit-value-error-cause`

- **Lambda list:** `(condition)`.
- **Returns:** the condition that made the value unacceptable, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `error` if `condition` is not a compatible `xml-edit-value-error` condition.

## Source-preserving selection and editing

#### `select-xml`

- **Lambda list:** `(source path success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Returns:** the values returned by the callback that runs.
- **Callbacks:** `success` receives one argument, the list of selected `xml-match` values (possibly empty). `failure` receives one argument, the condition.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, `xml-path-error`, or `xml-unsupported-edit` to `failure`.

#### `xml-match`

- **Lambda list:** None; this is an immutable source-selection match record type, not callable. **Returns/callbacks:** N/A.
- **Conditions:** The type declaration itself does not signal a condition.

#### `xml-match-p`

- **Lambda list:** `(value)`.
- **Returns:** True when `value` is an instance of `xml-match`; otherwise false.
- **Callbacks:** None.
- **Conditions:** Does not signal a `cl-xml-kit` condition.

#### `xml-match-kind`

- **Lambda list:** `(record)`.
- **Returns:** `:element`, `:attribute`, or `:text`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-match` value.

#### `xml-match-path`

- **Lambda list:** `(record)`.
- **Returns:** the canonical path with an index on every element step.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-match` value.

#### `xml-match-name`

- **Lambda list:** `(record)`.
- **Returns:** the lexical qualified name for element or attribute matches, or `nil`.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-match` value.

#### `xml-match-value`

- **Lambda list:** `(record)`.
- **Returns:** the decoded attribute or text value, or `nil` for element matches.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-match` value.

#### `xml-match-start`

- **Lambda list:** `(record)`.
- **Returns:** the inclusive start index in the original source string.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-match` value.

#### `xml-match-end`

- **Lambda list:** `(record)`.
- **Returns:** the exclusive end index in the original source string.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-match` value.

#### `xml-match-element`

- **Lambda list:** `(record)`.
- **Returns:** the matched DOM element, or the element containing an attribute or text match.
- **Callbacks:** None.
- **Conditions:** Common Lisp `type-error` if `record` is not an `xml-match` value.

#### `edit-xml-set`

- **Lambda list:** `(source path value success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Returns:** the values returned by the callback that runs.
- **Callbacks:** `success` receives one argument, the edited source string. `failure` receives one condition argument.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, `xml-path-error`, `xml-unsupported-edit`, or `xml-edit-value-error` to `failure`.

#### `edit-xml-delete`

- **Lambda list:** `(source path success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Returns:** the values returned by the callback that runs.
- **Callbacks:** `success` receives one argument, the edited source string. `failure` receives one condition argument.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, `xml-path-error`, `xml-unsupported-edit`, or `xml-edit-value-error` to `failure`.

#### `edit-xml-append`

- **Lambda list:** `(source path fragment success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Returns:** the values returned by the callback that runs.
- **Callbacks:** `success` receives one argument, the edited source string. `failure` receives one condition argument.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, `xml-path-error`, `xml-unsupported-edit`, or `xml-edit-value-error` to `failure`.

#### `format-xml`

- **Lambda list:** `(source success failure &key (indent 2) (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`.
- **Returns:** the values returned by the callback that runs.
- **Callbacks:** `success` receives one argument, the formatted source string. `failure` receives one condition argument.
- **Conditions:** Passes `xml-input-error`, `xml-unsupported-encoding`, `xml-well-formedness-error`, `xml-namespace-error`, `xml-external-resource-disabled`, `xml-resource-limit-exceeded`, or `xml-unsupported-edit` to `failure`.

