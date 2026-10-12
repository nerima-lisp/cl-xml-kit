# cl-xml-kit architecture and API

## 1. Scope and goals

`cl-xml-kit` is an SBCL-oriented Common Lisp library for XML 1.0 Fifth Edition
non-validating processing and Namespaces in XML 1.0 Third Edition. Version 1.0
provides three related views of one input:

1. A pull event reader for forward-only processing.
2. A DOM-like tree for applications that need random access or construction of
   a new serialization tree.
3. A serializer that emits well-formed, namespace-aware XML.

The parser accepts a string, a character stream, or a vector of octets. String
and character-stream inputs already contain characters. Octet input is UTF-8,
with an optional UTF-8 BOM that is consumed before parsing. An octet input
beginning with a UTF-16 or UTF-32 BOM signals `xml-unsupported-encoding`; it is
not silently guessed or decoded as UTF-8. This is an explicit UTF-8 input
profile, not a claim that the profile alone implements every XML 1.0 encoding
option.

The parser performs XML line-end normalization, XML character validation,
attribute-value normalization, character-reference expansion, predefined
entity expansion, element matching, duplicate-attribute checks, and namespace
well-formedness checks. A DOM parse coalesces adjacent character data after
those normalizations. The pull reader may split character data into more than
one event, so consumers must use event boundaries rather than assume one event
per text node.

The serializer escapes text and attribute values, emits namespace declarations,
and supports optional pretty printing. Pretty printing never inserts whitespace
into mixed content. The default output is compact and deterministic for a
given tree.

### Non-goals for 1.0

- DTD validation, declaration-defaulted attributes, and notation validation.
- XPath, XSLT, XInclude, XML Catalogs, schema languages, and canonical XML.
- Resolving a URI, opening a file, making a network request, or invoking an
  application-supplied external entity resolver.
- A SAX callback API. A callback adapter may be added after the pull contract
  has stabilized.
- Automatic decoding of BOM-less UTF-16 or other encodings.
- Preserving lexical details that do not affect the XML information set, such
  as quote choice, entity spelling, attribute order as semantic data, or the
  exact empty-element tag spelling.

### Pull API decision

The XML specifications define processor behavior, not a pull or push API. The
core API is therefore pull-based because the caller controls advancement,
early termination, and interleaving of multiple readers. It also makes the
namespace scope and external-resource policy visible on the event that needs
it. A SAX adapter would require a second lifecycle and callback contract and is
not needed for the 1.0 scope.

The pull reader emits these event types:

| Type | Meaning | Populated fields |
| --- | --- | --- |
| `:start-document` | Input has passed encoding setup | position |
| `:doctype` | A doctype was recognized without validation | text, position |
| `:start-element` | An element start tag was read | name, attributes, position |
| `:characters` | Normalized character data | text, position |
| `:comment` | A comment was read | text, position |
| `:processing-instruction` | A processing instruction was read | target, text, position |
| `:end-element` | The matching end tag was read | name, position |
| `:skipped-external-entity` | An external entity was skipped under an explicit skip policy | name, position |
| `:end-document` | The document has no more events | position |

The default external-resource policy is `:reject`. `:skip` is available only
for callers that explicitly accept a non-expanding event stream. There is no
`:resolve` policy in 1.0.

## 2. XML and namespace conformance boundary

The implementation follows the normative productions and constraints in:

- [XML 1.0 Fifth Edition, W3C Recommendation](https://www.w3.org/TR/2008/REC-xml-20081126/), especially sections 2.2, 2.3, 2.11, 3.1, 3.3.3, 4.1, 4.2, 4.4, and 4.3.3.
- [Namespaces in XML 1.0 Third Edition, W3C Recommendation](https://www.w3.org/TR/2009/REC-xml-names-20091208/), especially sections 3, 4, 5, 6, and 7.

The XML 1.0 named well-formedness constraints are PEs in Internal Subset,
External Subset, PE Between Declarations, Element Type Match, Unique Att Spec,
No External Entity References, No `<` in Attribute Values, Legal Character,
Entity Declared, Parsed Entity, No Recursion, and In DTD. The parser enforces
the constraints it can observe in the document entity and internal subset.
It parses internal general-entity declarations needed for expansion, but does
not validate element declarations, notation declarations, or defaulted
attributes. External subset contents and errors inside unread external entities
are not inspected; an external identifier is handled by the resource policy.
This distinction is part of the non-validating profile rather than an
unqualified claim to validate every external subset constraint.

Names use XML 1.0 Fifth Edition `NameStartChar` and `NameChar` productions.
Namespace-aware names are split at the only permitted colon into `prefix` and
`local-name`. `xml` is bound only to its reserved namespace URI. `xmlns` is
reserved for declarations. A prefixed name requires an in-scope declaration;
an empty URI is allowed only for the default namespace undeclaration. The
default namespace applies to unprefixed elements, not unprefixed attributes.
Duplicate attributes are detected by expanded name, `(namespace-uri,
local-name)`, rather than by lexical spelling.

XML line-end normalization converts CRLF and CR to LF before grammar parsing.
Attribute normalization then expands character references and predefined or
internal entities and applies the XML whitespace rules. The DOM stores the
normalized value. A serializer always emits a safe lexical form rather than
attempting to preserve the original spelling.

### Encoding profile

UTF-8 is mandatory for octet input. A UTF-8 BOM is accepted and removed. A
UTF-16BE, UTF-16LE, UTF-32BE, or UTF-32LE BOM signals
`xml-unsupported-encoding`, with a reason slot of `:utf-16-not-supported` or
`:utf-32-not-supported`; the parser does not inspect later bytes to guess an
encoding. An XML declaration is accepted only when its `encoding`
value is absent or case-insensitively equal to `UTF-8`. Any other declaration
value signals `xml-unsupported-encoding` with reason
`:declaration-not-utf-8`. This rule applies to strings and character streams as
well: they are already decoded, but the declaration still cannot claim an
encoding outside this profile. `standalone="yes"` and `standalone="no"` are
parsed but have no validation effect. A UTF-8 BOM paired with a non-UTF-8
declaration is the same unsupported-encoding condition. Full XML 1.0 UTF-16
support is a future scope decision and requires a separate conformance result.

## 3. Security and resource limits

The parser never resolves external DTDs or external general or parameter
entities. No file, URI, socket, or resolver callback is reachable from the
runtime API. The default `:reject` policy signals
`xml-external-resource-disabled`; `:skip` produces a
`:skipped-external-entity` event and never inserts replacement text. The
serializer never emits a resolver instruction or accesses an external source.

Limits are part of the API contract. They are held in an immutable
`xml-resource-limits` value and copied into parser and serializer state. Every
counter is checked before allocation or recursive descent crosses the limit.
The boundary value is accepted; the first value above it signals
`xml-resource-limit-exceeded`.

| Limit | Default | Counts |
| --- | ---: | --- |
| `max-input-octets` | 16,777,216 | UTF-8 octets supplied to one parse |
| `max-document-characters` | 16,777,216 | decoded characters after line-end normalization |
| `max-output-characters` | 16,777,216 | serialized characters |
| `max-depth` | 512 | open element or entity nesting depth |
| `max-elements` | 1,000,000 | element start tags in one document |
| `max-attributes-per-element` | 1,024 | attributes on one element |
| `max-name-length` | 1,024 | characters in one XML or namespace name |
| `max-text-node-length` | 1,048,576 | characters accumulated in one DOM text node |
| `max-namespace-bindings` | 1,024 | active declarations on one element scope |
| `max-entity-expansions` | 10,000 | internal entity expansion operations |
| `max-expanded-characters` | 16,777,216 | characters produced by entity expansion |
| `max-entity-depth` | 32 | nested internal entity expansion depth |

These values follow the bounded-state pattern used by the sibling JSON parser,
while adding XML-specific entity, namespace, and name limits. Applications may
construct a new limit value for a trusted workload, but they must not mutate a
limit object during a parse. A limit condition has readers for `kind`, `value`,
`limit`, `source`, and `position`. Its `kind` is one of the limit names above.
An entity expansion is counted before replacement text is appended, so a
failed expansion cannot allocate an unbounded intermediate string.

Resource-limit conditions, malformed input, unsupported encodings, and
disabled external resources are fatal for the current parser. After a fatal
condition, calling `next-xml-event` again signals the same parser-state error;
the parser does not resume with partially trusted structure.

## 4. Data model

The public package is `cl-xml-kit`. All public definitions have docstrings. All
public entry points use `check-type` and signal package conditions rather than
bare `error` or `assert` calls.

### Names and positions

`xml-name` has these slots:

- `local-name`: a non-empty string without a colon.
- `namespace-uri`: a string, with the empty string meaning no namespace.
- `prefix`: a string or `nil`; it is lexical and is not used for equality.

`xml-position` has `source`, `offset`, `line`, and `column`. Offsets are
zero-based character offsets in the normalized input; lines and columns are
one-based. `source` is the caller-provided source name or `nil`. Every node,
event, and public parse condition carries a start position where one exists.
Nodes also expose an end position for a source range. Synthetic nodes created
by an application may have both positions set to `nil`.

`xml-namespace-binding` has `prefix` and `namespace-uri`. `prefix` is `nil`
for a default-namespace declaration and a string for a prefixed declaration.
The empty URI is legal only for a default-namespace undeclaration. Element
namespace-declaration lists contain these values in source order.

### Nodes

`xml-document` contains an ordered list of document children. Its children may
include one `xml-doctype`, comments, processing instructions, whitespace text
outside the root, and exactly one root `xml-element`. A parsed document always
contains one root element. `xml-element` contains an `xml-name`, an ordered
list of `xml-attribute`, an ordered list of namespace declarations, and an
ordered list of child nodes. `xml-attribute` contains an expanded name and its
normalized string value. Unprefixed attributes always have the empty namespace
URI, even when a default namespace is active.

`xml-text` stores character data. `xml-comment` stores comment text without
the delimiters. `xml-processing-instruction` stores a target and optional
data. `xml-doctype` stores the root name and the lexical internal/external
identifier without resolving it. A document tree does not expose DTD
validation declarations as semantic attributes.

Parsed trees are immutable by API contract. The constructors create fresh
values for an application to assemble before serialization, but v1.0 exports
no mutator or functional-update operation. Direct mutation of raw structure
slots is unsupported and is not safe while a serializer is running.

The following rules are stable API invariants:

- Child and attribute order is source order and is preserved by accessors.
- Adjacent text nodes are coalesced when created by parsing, including text
  separated only by character or internal entity references. Comments,
  processing instructions, and element boundaries stop coalescing.
- Expanded names compare by namespace URI and local name. Prefix differences
  alone do not change name identity.
- Lists returned by accessors are fresh immutable snapshots; callers do not
  receive parser-owned mutable lists.
- Position values describe input locations and are not recomputed after an
  application mutates a tree.

### Public API

The following symbols are the complete public export surface. Accessor
readers are exported because the structures are part of the public tree and
event contracts; implementation helpers remain internal.

#### Parsing and pull events

```text
parse-xml
make-xml-parser
next-xml-event
xml-parser-eof-p
xml-event
make-xml-event
xml-event-p
xml-event-type
xml-event-name
xml-event-attributes
xml-event-text
xml-event-target
xml-event-position
xml-event-external-p
```

`parse-xml` has the signature:

```lisp
(parse-xml input &key source-name resource-limits external-entity-policy)
```

It accepts a string, character input stream, or `(vector (unsigned-byte 8))`
and returns one `xml-document`. It signals `xml-input-error` or a more
specific condition on invalid input. `resource-limits` defaults to
`*default-xml-resource-limits*`; `external-entity-policy` defaults to
`:reject`.

`make-xml-parser` has the same input and keyword contract and returns one
forward-only parser. `next-xml-event` takes one parser and returns one event,
or `nil` at end of input. It signals `xml-parser-state-error` if called after a
fatal condition. `xml-parser-eof-p` returns true after the end-document event.
`xml-event` is a value structure with the constructor signature:

```lisp
(make-xml-event &key type name attributes text target position external-p)
```

Its optional fields are `nil` when they do not apply to its type. Parser-owned
events are immutable snapshots. `attributes` is a list of `xml-attribute` and
`external-p` is true only for a skipped external entity event.

#### Resource limits

```text
*default-xml-resource-limits*
make-xml-resource-limits
xml-resource-limits-p
xml-resource-limits-max-input-octets
xml-resource-limits-max-document-characters
xml-resource-limits-max-output-characters
xml-resource-limits-max-depth
xml-resource-limits-max-elements
xml-resource-limits-max-attributes-per-element
xml-resource-limits-max-name-length
xml-resource-limits-max-text-node-length
xml-resource-limits-max-namespace-bindings
xml-resource-limits-max-entity-expansions
xml-resource-limits-max-expanded-characters
xml-resource-limits-max-entity-depth
```

`make-xml-resource-limits` accepts only keyword arguments and returns a new
validated limit object. A non-positive limit signals
`xml-invalid-resource-limit`.

#### Tree and value structures

```text
xml-position
make-xml-position
xml-position-source
xml-position-offset
xml-position-line
xml-position-column

xml-name
make-xml-name
xml-name-local-name
xml-name-namespace-uri
xml-name-prefix

xml-namespace-binding
make-xml-namespace-binding
xml-namespace-binding-p
xml-namespace-binding-prefix
xml-namespace-binding-namespace-uri

xml-document
make-xml-document
xml-document-children
xml-document-position
xml-document-end-position

xml-element
make-xml-element
xml-element-name
xml-element-attributes
xml-element-namespace-declarations
xml-element-children
xml-element-position
xml-element-end-position

xml-attribute
make-xml-attribute
xml-attribute-name
xml-attribute-value
xml-attribute-position
xml-attribute-end-position

xml-text
make-xml-text
xml-text-data
xml-text-position
xml-text-end-position

xml-comment
make-xml-comment
xml-comment-data
xml-comment-position
xml-comment-end-position

xml-processing-instruction
make-xml-processing-instruction
xml-processing-instruction-target
xml-processing-instruction-data
xml-processing-instruction-position
xml-processing-instruction-end-position

xml-doctype
make-xml-doctype
xml-doctype-root-name
xml-doctype-public-id
xml-doctype-system-id
xml-doctype-internal-subset
xml-doctype-position
xml-doctype-end-position
```

The structure predicates generated by `defstruct` are public only when needed
by callers to distinguish node kinds: `xml-document-p`, `xml-element-p`,
`xml-attribute-p`, `xml-text-p`, `xml-comment-p`,
`xml-processing-instruction-p`, and `xml-doctype-p`.

#### Serialization

```text
xml->string
xml->canonical-string
write-xml
write-xml-canonical
```

`xml->string` has the signature:

```lisp
(xml->string object &key pretty-print indent resource-limits)
```

It returns a fresh string and accepts an `xml-document` or any serializable
node. `pretty-print` is false by default. `indent` is a non-negative string
width and defaults to 2 when pretty printing is enabled. `write-xml` has the
signature:

```lisp
(write-xml object stream &key pretty-print indent resource-limits)
```

It writes characters to a character output stream and returns the stream.
Both functions signal `xml-serialization-error` or
`xml-resource-limit-exceeded`. The serializer uses O(n) time and output space
for a tree with n serialized characters, excluding the caller's stream.

`xml->canonical-string` and `write-xml-canonical` use the XML Test Suite's
Second Canonical Form rules. They return a string or write to a character
stream respectively, take the same object and resource-limit inputs as the
ordinary serializer, and do not accept pretty-print options. This is a test
suite canonicalization API, not a claim to implement W3C Canonical XML.

#### Conditions

```text
xml-kit-error
xml-input-error
xml-decode-error
xml-unsupported-encoding
xml-parse-error
xml-well-formedness-error
xml-namespace-error
xml-external-resource-disabled
xml-parser-state-error
xml-resource-limit-exceeded
xml-invalid-resource-limit
xml-serialization-error
```

The condition inheritance is:

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

The complete public condition-reader set is:

```text
xml-condition-message
xml-condition-position
xml-condition-source
xml-decode-error-octet
xml-decode-error-encoding
xml-unsupported-encoding-encoding
xml-unsupported-encoding-reason
xml-well-formedness-error-constraint
xml-well-formedness-error-expected
xml-namespace-error-prefix
xml-namespace-error-namespace-uri
xml-external-resource-disabled-identifier
xml-external-resource-disabled-kind
xml-parser-state-error-parser
xml-resource-limit-exceeded-kind
xml-resource-limit-exceeded-value
xml-resource-limit-exceeded-limit
xml-invalid-resource-limit-kind
xml-invalid-resource-limit-value
xml-serialization-error-object
xml-serialization-error-path
```

All conditions derive from `xml-kit-error` and have a `:report` method. Every
condition-specific slot has a condition-specific reader. The shared readers are
`xml-condition-message`, `xml-condition-position`, and
`xml-condition-source` where applicable. Specific readers are:

- `xml-unsupported-encoding-encoding` and
  `xml-unsupported-encoding-reason`.
- `xml-well-formedness-error-constraint` and
  `xml-well-formedness-error-expected`.
- `xml-namespace-error-prefix` and `xml-namespace-error-namespace-uri`.
- `xml-external-resource-disabled-identifier` and
  `xml-external-resource-disabled-kind`.
- `xml-resource-limit-exceeded-kind`,
  `xml-resource-limit-exceeded-value`, and
  `xml-resource-limit-exceeded-limit`.
- `xml-parser-state-error-parser`.
- `xml-serialization-error-object` and
  `xml-serialization-error-path`.

`xml-decode-error` is the base for UTF-8 decoding failures.
`xml-unsupported-encoding` is both an input condition and a decode-specific
condition. `xml-input-error` is the base for input failures;
`xml-parse-error` is the base for grammar, namespace, external-resource, and
parser-state failures; and `xml-serialization-error` is the base for output
failures. `xml-resource-limit-exceeded` is shared by parse and serialization
operations. No public API signals a bare `error` for expected input, resource,
or serialization failures.

## 5. Dependencies and upstream changes

### Runtime dependency decision

The runtime depends on the sibling lower-layer system `cl-codec-kit` for UTF-8
octet decoding. Its public `octets-to-string` API and decode conditions match
the required boundary, and its implementation already rejects invalid leading,
continuation, overlong, surrogate, and out-of-range sequences. `cl-xml-kit`
still owns XML BOM stripping, input-size accounting, source positions, and the
mapping of codec conditions into `xml-decode-error`.

`cl-parser-kit` is not a runtime dependency. XML has a fixed grammar, and a
direct tokenizer/state machine avoids importing parser-combinator behavior,
token limits, and failure-return conventions that do not match the XML event
contract. `cl-boundary-kit` is also not a dependency because XML input bounds
are local parser limits, not a process or filesystem boundary.

This satisfies the dependency policy because the chosen dependency is a
same-organization lower-layer library, the dependency graph remains acyclic,
and the implementation uses a small, documented API surface. The ASDF
`:depends-on` entry is the dependency graph's source of truth. The release
stream must pin the sibling dependency to a released version in `flake.lock`
and record the dependency closure in the release review.

### Upstream changes

None required for v1.0.0. `cl-codec-kit` already provides the UTF-8 decoding
entry point and the position-bearing decode conditions needed by the input
adapter. If a future change requires streaming octet input with a retained
encoding declaration across chunks, it should first add an explicit stateful
decoder API upstream; `cl-xml-kit` must not infer that API by calling
`decode-prefix` with a BOM-sensing encoding.

## 6. File layout, dependency order, and ownership

The repository keeps a flat `src/` and `t/` layout. The W1 stream writes the
complete `src/package.lisp` export list and the complete `.asd` component lists
before other streams implement their files. After that point, each stream edits
only its assigned files. A stream may read another stream's files and consume
the interfaces listed below, but it must not edit them.

The five streams are:

- **W1 Foundation:** package, conditions, limits, data structures, ASDF, and
  test harness.
- **W2 Input:** character classes, UTF-8 input adaptation, and tokenization.
- **W3 Parser:** namespace scope, entities, parser state, pull events, and DOM
  construction.
- **W4 Serializer:** escaping, namespace declaration emission, and formatting.
- **W5 Verification:** property/fuzz tests, benchmarks, and the independent
  XMLConf runner. W5 has no `src/` owner and only writes verification files.

### ASDF component order

The runtime `:components` order is:

```text
package
conditions
resource-limits
nodes
events
char-class
input
tokenizer
namespace
entities
parser-state
parser
serializer
spans
path
verify
fragment
edit
format
```

The test system uses this order:

```text
package
runner
api-surface-test
conditions-test
resource-limits-test
char-class-test
input-test
tokenizer-test
namespace-test
entities-test
security-test
parser-test
dom-test
serializer-test
round-trip-test
spans-test
path-test
verify-test
fragment-test
edit-test
format-test
property-test
fuzz-test
xml-conformance-test
```

The `.asd` file remains the only component manifest. W1 owns its runtime and
test system declarations, version metadata, `:depends-on`, and
`:in-order-to`. W2 through W5 must not edit its ordinary declarations.

W5 may edit only the XMLConf fetch block explicitly reserved in `flake.nix`;
W1 owns every other `flake.nix` expression. This is a disjoint logical region,
not a second owner for the same Nix expression.

### Complete file ownership table

Every source and test file appears exactly once in this table.

| Stream | `src/` files owned | Corresponding `t/` files owned | Interfaces consumed |
| --- | --- | --- | --- |
| W1 | `src/package.lisp`, `src/conditions.lisp`, `src/resource-limits.lisp`, `src/nodes.lisp`, `src/events.lisp` | `t/package.lisp`, `t/runner.lisp`, `t/api-surface-test.lisp`, `t/conditions-test.lisp`, `t/resource-limits-test.lisp` | None. Publishes all exported names, conditions, limits, node slots, and event slots first. |
| W2 | `src/char-class.lisp`, `src/input.lisp`, `src/tokenizer.lisp` | `t/char-class-test.lisp`, `t/input-test.lisp`, `t/tokenizer-test.lisp` | W1 conditions, positions, and limits. `cl-codec-kit` UTF-8 API. Publishes token and input contracts to W3. |
| W3 | `src/namespace.lisp`, `src/entities.lisp`, `src/parser-state.lisp`, `src/parser.lisp` | `t/namespace-test.lisp`, `t/entities-test.lisp`, `t/parser-test.lisp`, `t/dom-test.lisp`, `t/security-test.lisp` | W1 nodes/events/conditions/limits and W2 tokenizer/input. Publishes valid event order and DOM invariants to W4. |
| W4 | `src/serializer.lisp` | `t/serializer-test.lisp`, `t/round-trip-test.lisp` | W1 node/name/limit contracts and W3 expanded-name, declaration, and text-coalescing invariants. |
| W5 | None | `t/property-test.lisp`, `t/fuzz-test.lisp`, `t/xml-conformance-test.lisp` | All stable public contracts. Owns benchmark artifacts, release-verification scripts, the XMLConf runner, exclusion manifest, and the XMLConf fetch block in `flake.nix`. |
| W6 | `src/spans.lisp`, `src/path.lisp`, `src/verify.lisp`, `src/fragment.lisp`, `src/edit.lisp`, `src/format.lisp` | `t/spans-test.lisp`, `t/path-test.lisp`, `t/verify-test.lisp`, `t/fragment-test.lisp`, `t/edit-test.lisp`, `t/format-test.lisp` | W2 token end offsets, W3 entity and DOCTYPE helpers and DOM nodes, and W1 conditions. Publishes the lexical span tree, `select-xml`, the source-preserving edit operations, and `format-xml`. |

### Shared build and project-file ownership

| Stream | Files or reserved regions | Ownership rule |
| --- | --- | --- |
| W1 | `cl-xml-kit.asd`, `src/package.lisp`, `t/package.lisp`, `t/runner.lisp`, `run-tests.lisp`, all of `.github/` except the W5 CI invocation region, and all of `flake.nix` except the XMLConf fetch block | W1 defines the complete export list, ASDF order, test entry point, baseline CI/Nix package, `cl-xml-kit/benchmark` ASDF system declaration, and `apps.bench`. |
| W5 | `benchmark/README.md`, `benchmark/package.lisp`, `benchmark/run.lisp`, `run-benchmarks.lisp`, `scripts/run-coverage.lisp`, `scripts/run-mutation.lisp`, `scripts/xmlconf-runner.lisp`, `scripts/xmlconf-exclusions.sexp`, the reserved XMLConf fetch block in `flake.nix`, and the conformance invocation region in `.github/workflows/ci.yml` | W5 adds only verification artifacts and the fixed XMLConf source/test derivation. W1 must leave the reserved block and invocation region intact. |

The existing `src/package.lisp`, `.asd`, `t/package.lisp`, and `t/runner.lisp`
are foundation-owned migration targets. W1 folds the current runner behavior
into the final test harness and owns the resulting `t/runner.lisp`; it is not a
second owner. No other stream edits those files.

### Dependency graph between streams

```text
W1 contracts
  -> W2 input/tokenizer
  -> W3 parser/DOM -> W4 serializer
  -> W5 verification and XMLConf consume W1-W4 and the W5 fetch contract
```

W2 can implement character classification after W1 fixes position and limit
slot names. W3 waits for W2's token shape and W1's event shape. W4 can begin
namespace-independent escaping after W1, but its final serializer tests wait
for W3's expanded-name and namespace-declaration invariants. W5 starts only
after W1-W4 interfaces are frozen. Its XMLConf portion additionally requires
the canonical serializer API before it can compare OUTPUT cases. This ordering prevents a
shared package or manifest file from becoming a parallel merge point.

## 7. Test strategy

Tests use `cl-weave`, live under `t/`, and follow the `<source>-test.lisp`
naming convention. Each `it` statement makes one observable claim in English.
The test system is deterministic: random generators use an explicit seed,
test input is in memory, and tests do not use wall-clock time, sleep, network,
ambient environment variables, or a shared filesystem.

### Unit and integration coverage

- `char-class-test` covers XML `Char`, `NameStartChar`, `NameChar`, `NCName`,
  forbidden surrogates, and `#xFFFE`/`#xFFFF`.
- `input-test` covers string, character stream, UTF-8 octet, UTF-8 BOM, invalid
  UTF-8, CRLF/CR normalization, and UTF-16/UTF-32 BOM rejection.
- `tokenizer-test` covers quoted attributes, references, comments, PI,
  CDATA, empty elements, and token positions.
- `namespace-test` covers default namespace scope, undeclaration, reserved
  prefixes, undeclared prefixes, and duplicate expanded attributes.
- `entities-test` covers predefined and character references, internal entity
  expansion, recursion, expansion count/size/depth limits, and external
  resource reject/skip behavior.
- `parser-test` covers prolog, doctype, one root, mixed content, matching
  end-tags, fatal error state, and event ordering.
- `dom-test` covers text coalescing, source order, positions, node predicates,
  and immutable snapshot behavior.
- `serializer-test` covers text/attribute escaping, legal character output,
  namespace declarations, prefix collisions, output limits, and pretty-print
  handling of element-only versus mixed content.
- `round-trip-test` covers parse and serialization of representative trees and
  compares normalized trees rather than lexical XML strings.
- `api-surface-test` verifies the complete export list and checks that every
  exported function has a docstring and documented signal/return contract.

Both expression and branch coverage must reach at least 90% and must not fall
below the previous release. Mutation tests must include the XML grammar or
serializer escape boundary and must retain a score of 1.0 under the
organization test standard. W5 owns `scripts/run-coverage.lisp` and
`scripts/run-mutation.lisp`. The coverage runner loads the same ASDF test
system as `run-tests.lisp`, asserts that the selected test and source inputs
are non-empty, prints expression and branch results, and exits nonzero below
either threshold. The mutation runner records the selected mutation set and
exits nonzero when any selected mutant survives. W1's `nix flake check` test
derivation runs the ordinary suite; coverage and mutation are explicit release
checks and are not silently skipped by an empty selector.

### Property and fuzz tests

The primary property is:

```lisp
(parse-xml (xml->string document))
  == (normalize-xml document)
```

The generator bounds depth, element count, attribute count, names, text, and
namespace bindings before constructing a tree. Normalization ignores prefix
choice, attribute order, quote choice, and empty-element spelling while keeping
expanded names, values, child order, and text content. A second property
asserts that arbitrary generated UTF-8 XML either returns a document/event
sequence or signals one of the documented `xml-kit-error` subtypes. It must
never leak an implementation condition or exhaust the stack.

Fuzz runs have an explicit trial count, fixed seed, per-case input limit, and
parser timeout. A failing seed and minimized input are retained as a test
fixture. Fuzzing never disables resource limits to make a case pass.

### W3C XML Conformance Test Suite

The research result in `.mediator/research/xmlconf.md` is reference-only and
is not committed with the library. W5 must not vendor the corpus. Its Nix test
derivation obtains the official archive with:

```nix
pkgs.fetchurl {
  url = "https://www.w3.org/XML/Test/xmlts20130923.tar.gz";
  sha256 = "9b61db9f5dbffa545f4b8d78422167083a8568c59bd1129f94138f936cf6fc1f";
}
```

The equivalent SRI hash is
`sha256-m2Hbn12/+lRfS414QiFnCDqFaMWb0RKflBOPk2z2/B8=`. The archive is not
redistributed in this MIT repository because the James Clark terms inside the
archive have not been confirmed for this form of third-party redistribution.
The Nix derivation must pass the fixed archive to the conformance test, and
local runs accept `XML_KIT_XMLCONF_DIR` naming an already extracted archive
directory. The runner must fail clearly when neither the derivation input nor
the local directory is available.

The selected profile is the approximately 1,857-case XML 1.0 Fifth Edition,
Namespaces 1.0 profile. W5 parses the catalog without modifying test files and
keeps exclusions as IDs and reasons in `scripts/xmlconf-exclusions.sexp`.
The rules are:

- `valid` and `invalid` are expected to be accepted by a non-validating
  processor. `invalid` does not mean invalid XML; it means validity errors
  that a non-validating processor need not reject.
- Namespace-aware `invalid` cases may also be rejected when the input violates
  a Namespaces 1.0 well-formedness constraint, because this profile enforces
  namespace binding rules rather than treating namespace processing as
  optional.
- `not-wf` is expected to be rejected.
- 65 `not-wf` cases whose failure is only inside an unread external entity are
  skipped with an explicit reason because this processor never reads external
  entities.
- `TYPE=error` cases are excluded from pass/fail.
- XML 1.1, other editions, other recommendations, and Namespace cases outside
  the selected profile are excluded.
- OUTPUT cases use the XML Test Suite Second Canonical Form. The 376 selected
  OUTPUT cases therefore require `xml->canonical-string` or
  `write-xml-canonical`; a successful parse without canonical comparison is
  not a pass.

The catalog analysis and exclusion handling are independent of the test data.
The runner records accepted, rejected, skipped, and excluded IDs so an
exclusion cannot silently reduce the selected set. W5 owns the runner,
exclusion file, Nix fetch block, test derivation, CI invocation, property
tests, and fuzz tests.

## 8. Performance plan

The benchmark scope is limited to public APIs whose cost is controlled by input
size or output size: UTF-8 parse, pull event iteration, DOM construction, and
serialization. Workloads include small documents, 1 MiB text, deep nesting up
to the configured limit, many attributes, namespace-heavy trees, and mixed
content. Each benchmark performs a correctness preflight before timing.

W1 declares a `cl-xml-kit/benchmark` ASDF system whose components are
`benchmark/package.lisp` and `benchmark/run.lisp`; W5 owns those component
files and `benchmark/README.md`. W1 exposes the harness through the Nix
`apps.bench` application. W5 also owns
`run-benchmarks.lisp`. The harness records the source revision, SBCL version,
host, workload seed, warmup count, sample count, allocation bytes, and
min/median/max measurements in machine-readable TSV. It uses at least two
warmups and ten samples, excludes full GC from timed sections, and does not use
`safety 0`.

The benchmark system is not a `checks.bench` CI gate. CI does not gate on
wall-clock time. Automated regression checks use allocation
and expected complexity order. Wall-clock comparisons are reported only as
same-host A/B measurements with their environment recorded. Public collection
APIs document their expected O(n) or O(n log n) cost in their docstrings.

## 9. Documentation and release constraints

The implementation wave must keep the English documentation pages
`docs/src/index.md`, `docs/src/getting-started.md`, and
`docs/src/reference/api.md` synchronized with this contract. The API reference
must include one heading per exported symbol, its signature, argument and
return tables, signals, an executable example, and related links. `mkdocs
build --strict` is the documentation gate. The repository does not add a
`CHANGELOG.md`; release history belongs in the GitHub Release description, and
the ASDF version is the single version source.

The architecture is complete when W1-W5 can implement without editing one
another's files, the export list and condition readers match the table above,
the resource defaults and fatal behavior are tested, and the pending XML
conformance research is explicitly resolved before release claims are made.

