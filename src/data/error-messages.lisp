;;;; src/data/error-messages.lisp

(in-package #:cl-xml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +xml-error-message-specs+
    '(;; Arguments and constructors
      (:label-must-be-string "~A must be a string.")
      (:label-must-be-string-or-nil "~A must be a string or NIL.")
      (:label-must-not-be-empty "~A must not be empty.")
      (:label-must-not-contain-colon "~A must not contain a colon.")
      (:label-must-be-list "~A must be a list.")
      (:label-must-be-function "~A must be a function designator.")
      (:label-must-be-xml-name "~A must be an XML-NAME.")
      (:label-must-be-xml-position "~A must be an XML-POSITION.")
      (:label-must-be-resource-limits "~A must be XML-RESOURCE-LIMITS.")
      (:label-must-be-resource-limits-value "~A must be XML resource limits.")
      (:resource-limits-value-required
       "RESOURCE-LIMITS must be an XML-RESOURCE-LIMITS value.")
      (:position-value-required "A position must be an XML-POSITION value.")
      (:offset-must-be-non-negative "OFFSET must be a non-negative integer.")
      (:line-must-be-positive "LINE must be a positive integer.")
      (:column-must-be-positive "COLUMN must be a positive integer.")
      (:indent-must-be-non-negative "INDENT must be a non-negative integer.")
      (:external-entity-policy-invalid
       "EXTERNAL-ENTITY-POLICY must be :REJECT or :SKIP.")
      (:invalid-resource-limit "Resource limit ~S must be a positive integer, not ~S.")
      (:unknown-event-type "Unknown XML event type ~S.")
      (:event-attributes-invalid
       "EVENT ATTRIBUTES must contain XML-ATTRIBUTE values.")
      (:unknown-token-kind "Unknown XML token kind ~S.")
      (:token-kind-needs-string "XML token kind ~S requires a string value.")
      (:token-kind-needs-nil "XML token kind ~S requires a NIL value.")
      (:token-end-invalid "TOKEN END must be a non-negative integer or NIL.")
      (:tokenizer-input-invalid "TOKENIZER INPUT must be a normalized string.")
      (:tokenizer-required "TOKENIZER must be an XML-TOKENIZER.")
      (:parser-required "PARSER must be an XML parser.")
      (:entity-context-required "CONTEXT must be an XML entity context.")
      (:entity-declaration-required
       "DECLARATION must be an XML entity declaration.")
      (:entity-name-invalid "ENTITY NAME must be a non-empty string.")
      (:internal-entity-needs-text
       "An internal ENTITY declaration needs replacement text.")
      (:external-entity-has-text
       "An external ENTITY declaration cannot have replacement text.")
      (:namespace-context-required
       "CONTEXT must be an XML-NAMESPACE-CONTEXT value.")
      (:namespace-binding-required
       "A namespace declaration must be an XML-NAMESPACE-BINDING.")
      (:attribute-entry-invalid "An attribute entry must be a (NAME . VALUE) pair.")
      (:stream-required "STREAM must be a character stream.")
      ;; Input
      (:input-limit-exceeded "XML input limit ~S exceeded: ~D is greater than ~D.")
      (:invalid-utf-8 "Invalid UTF-8 input at octet ~D.")
      (:invalid-utf-16 "Invalid UTF-16 input at octet ~D.")
      (:utf-16-needs-bom "UTF-16 input must begin with a byte order mark.")
      (:declaration-contradicts-bom
       "The XML declaration names ~A, but the byte order mark selects UTF-16.")
      (:encoding-not-supported "~A input is not supported.")
      (:input-type-unsupported
       "XML input must be a string, character input stream, or octet vector.")
      (:input-offset-out-of-range
       "XML input position offset must be between zero and the text length.")
      ;; Tokenizer
      (:resource-limit-exceeded-values "XML resource limit ~S exceeded: ~D > ~D.")
      (:illegal-character "Character U+~4,'0X is not legal XML character.")
      (:expected-name "Expected an XML Name.")
      (:empty-entity-reference "An entity reference must not be empty.")
      (:malformed-numeric-reference "Malformed numeric character reference.")
      (:numeric-reference-illegal
       "Numeric character reference names an illegal XML character.")
      (:malformed-entity-reference "Malformed entity reference.")
      (:unterminated-entity-reference "Unterminated entity reference.")
      (:unterminated-attribute-value "Unterminated quoted attribute value.")
      (:lt-in-attribute-value "A '<' is not allowed in an attribute value.")
      (:cdata-end-in-text "The character sequence ]]> is not allowed in text.")
      (:unterminated-comment "Unterminated XML comment.")
      (:double-hyphen-in-comment "XML comments must not contain '--'.")
      (:unterminated-cdata "Unterminated CDATA section.")
      (:pi-needs-target "A processing instruction needs a target.")
      (:pi-target-not-name "A processing instruction target must be an XML Name.")
      (:pi-target-not-separated
       "Processing-instruction target must be separated from its data.")
      (:unterminated-pi "Unterminated processing instruction.")
      (:doctype-needs-root-element "DOCTYPE must name a root element.")
      (:system-needs-literal "SYSTEM identifiers need a quoted system literal.")
      (:public-needs-public-literal "PUBLIC identifiers need a quoted public literal.")
      (:public-needs-system-literal "PUBLIC identifiers need a quoted system literal.")
      (:unexpected-doctype-content "Unexpected content in DOCTYPE declaration.")
      (:unterminated-doctype "Unterminated DOCTYPE declaration.")
      (:unmatched-bracket-in-doctype "Unmatched ']' in DOCTYPE.")
      (:expected-attribute-or-close "Expected an attribute name or tag close.")
      (:expected-equals-after-attribute "Expected '=' after an attribute name.")
      (:expected-quoted-value-after-equals
       "Expected a quoted attribute value after '='.")
      (:attributes-need-whitespace "Attributes must be separated by XML whitespace.")
      (:expected-another-attribute-or-close
       "Expected another attribute or tag close.")
      (:expected-close-after-end-tag-name "Expected '>' after an end-tag name.")
      (:unexpected-end-of-input "Unexpected end of XML input.")
      ;; Namespaces
      (:resource-limit-exceeded "XML resource limit ~S exceeded.")
      (:qname-not-string "A qualified name must be a non-empty string.")
      (:qname-two-colons "A qualified name may contain only one colon.")
      (:qname-not-two-ncnames "A qualified name must contain two NCNames.")
      (:unprefixed-name-not-ncname "An unprefixed name must be an NCName.")
      (:prefix-not-ncname "A namespace prefix must be an NCName.")
      (:xmlns-prefix-reserved-for-declarations
       "The xmlns prefix is reserved for declarations.")
      (:xml-prefixes-reserved "Prefixes beginning with xml are reserved.")
      (:prefixed-namespace-undeclared "A prefixed namespace cannot be undeclared.")
      (:only-xml-prefix-binds-xml-namespace
       "Only the xml prefix may bind the XML namespace.")
      (:xml-prefix-fixed-uri "The xml prefix has a fixed namespace URI.")
      (:xmlns-namespace-not-declarable "The XMLNS namespace cannot be declared.")
      (:prefix-declared-twice
       "A namespace prefix may be declared only once on an element.")
      (:xml-namespace-not-default "The XML namespace cannot be the default namespace.")
      (:declaration-prefix-not-ncname
       "A namespace declaration must name an NCName prefix.")
      (:xmlns-name-reserved "The xmlns name is reserved for namespace declarations.")
      (:prefix-not-bound "A prefixed name has no in-scope namespace binding.")
      (:duplicate-expanded-attribute "Two attributes have the same expanded name.")
      (:duplicate-attribute "An attribute name occurs more than once.")
      (:prefixed-binding-empty-uri
       "A prefixed binding cannot use an empty namespace URI.")
      ;; Entities
      (:entity-resource-limit-exceeded
       "XML entity resource limit ~S exceeded (~D > ~D).")
      (:external-entity-disabled "External ~A entity ~A is disabled.")
      (:entity-reference-needs-semicolon
       "An entity reference must end with a semicolon.")
      (:malformed-character-reference "Malformed XML character reference.")
      (:character-reference-illegal
       "XML character reference names an illegal character.")
      (:entity-value-malformed-reference
       "An entity value contains a malformed entity reference.")
      (:lt-in-attribute-replacement "An attribute value cannot contain '<'.")
      (:amp-in-attribute-replacement
       "An entity replacement cannot generate '&' in an attribute value.")
      (:recursive-entity "Recursive XML entity reference.")
      (:duplicate-entity "Duplicate XML entity declaration.")
      (:undeclared-entity "Undeclared XML entity ~A.")
      (:unescaped-ampersand-in-replacement
       "An entity replacement generated an unescaped ampersand.")
      (:unterminated-entity-declaration "Unterminated XML entity declaration.")
      (:entity-declaration-needs-name "An entity declaration needs a valid name.")
      (:entity-name-not-separated
       "An entity name must be separated from its declaration value.")
      (:entity-declaration-incomplete "An entity declaration is incomplete.")
      (:entity-declaration-needs-close "An entity declaration must end with '>'.")
      (:entity-declaration-needs-value "An entity declaration needs a quoted value.")
      (:external-entity-needs-system-id "An external entity needs a system identifier.")
      (:public-needs-system-id "PUBLIC needs a system identifier.")
      (:external-entity-declaration-incomplete
       "An external entity declaration is incomplete.")
      (:unterminated-subset-comment "Unterminated XML comment in the internal subset.")
      (:unterminated-markup-declaration "Unterminated markup declaration.")
      (:undeclared-parameter-entity "Undeclared parameter entity ~A.")
      (:conditional-section-unsupported
       "Conditional sections are not supported in the internal subset.")
      ;; Parser
      (:external-dtd-disabled "External DTD resolution is disabled.")
      (:single-doctype "Only one DOCTYPE is allowed.")
      (:end-tag-mismatch "End tag does not match its start tag.")
      (:xml-target-reserved "The XML target is reserved.")
      (:entity-closes-outside-fragment
       "An entity replacement cannot close an element outside its fragment.")
      (:unexpected-end-of-document "Unexpected end of document.")
      (:unexpected-token-in-content "Unexpected token in element content.")
      (:expected-declaration-keyword "Expected an XML declaration keyword.")
      (:expected-declaration-equals "Expected '=' in the XML declaration.")
      (:expected-declaration-value "Expected a quoted XML declaration value.")
      (:unterminated-declaration-value "An XML declaration value is unterminated.")
      (:declaration-not-utf-8 "The XML declaration must use UTF-8.")
      (:standalone-invalid "Standalone must be either 'yes' or 'no'.")
      (:declaration-version-required
       "XML declaration version information is required.")
      (:unsupported-version "Only XML version 1.0 is supported.")
      (:declaration-needs-whitespace
       "XML declaration attributes must be separated by XML whitespace.")
      (:unexpected-declaration-content "Unexpected content in the XML declaration.")
      (:text-outside-root "Non-whitespace text is outside the root.")
      (:entity-outside-root "An entity is outside the root.")
      (:declaration-not-first "The XML declaration must be first.")
      (:cdata-outside-root "CDATA is outside the root.")
      (:doctype-after-root "DOCTYPE must appear before the root element.")
      (:single-root "A document has one root element.")
      (:root-doctype-mismatch "The document root does not match DOCTYPE.")
      (:end-tag-without-element "An end tag has no open element.")
      (:root-required "A document needs a root element.")
      (:parser-fatal "The parser is in a fatal state.")
      ;; Serializer
      (:character-not-representable "Character U+~4,'0X cannot be represented in XML.")
      (:output-limit-exceeded "The serialized output exceeds the configured limit.")
      (:declarations-must-be-bindings
       "Namespace declarations must be XML namespace bindings.")
      (:xmlns-prefix-reserved "The xmlns prefix is reserved.")
      (:xml-prefix-reserved-uri "The xml prefix has a reserved namespace URI.")
      (:xmlns-uri-reserved "The xmlns namespace URI is reserved.")
      (:serialized-prefix-invalid "A serialized prefix is invalid.")
      (:xml-prefix-needs-reserved-uri "The xml prefix must use its reserved URI.")
      (:xmlns-prefix-names-node "The xmlns prefix cannot name an XML node.")
      (:declaration-conflicts-with-name
       "A namespace declaration conflicts with a node name.")
      (:element-attributes-invalid "Element attributes must be XML attributes.")
      (:element-children-invalid "Element children must be XML nodes.")
      (:repeated-namespace-declaration
       "An element cannot repeat a namespace declaration.")
      (:only-xml-binds-xml-namespace "Only xml may bind the XML namespace.")
      (:xmlns-attribute-reserved
       "The xmlns attribute is reserved for namespace declarations.")
      (:repeated-expanded-attribute
       "An element cannot repeat an expanded attribute name.")
      (:prefixed-attribute-needs-uri "A prefixed attribute must have a namespace URI.")
      (:document-children-invalid "Document children must be XML nodes.")
      (:comment-not-representable "Comment data is not representable in XML.")
      (:pi-target-xml-reserved "Processing-instruction target XML is reserved.")
      (:pi-data-contains-close "Processing-instruction data contains ?>.")
      (:object-not-serializable "OBJECT is not a serializable XML node.")
      ;; Paths and edits
      (:path-syntax "Malformed path at index ~D: expected ~A.")
      (:children-not-locatable
       "The children of <~A> cannot be located in the source: its content references entity ~A, whose replacement text contains markup.")
      (:value-illegal-character
       "VALUE has a character that XML does not allow, U+~4,'0X at index ~D.")
      (:namespace-declaration-not-editable
       "Namespace declarations cannot be edited as attributes.")
      (:path-matches-nothing "The path matches nothing.")
      (:path-matches-several "The path matches ~D elements, not one.")
      (:attlist-governs-attributes "An ATTLIST declaration governs the attributes of <~A>.")
      (:prefix-not-declared "The prefix ~A is not declared where <~A> starts.")
      (:content-references-markup-entity
       "The content of <~A> references entity ~A, whose replacement text contains markup.")
      (:content-not-text
       "The content of <~A> holds elements, comments, or processing instructions.")
      (:root-not-deletable "The document element cannot be deleted.")
      (:text-path-not-deletable "A text() path cannot be deleted.")
      (:append-needs-element "Only an element can have an element appended.")
      (:fragment-unacceptable "The fragment is not acceptable: ~A")
      (:fragment-not-single-element "The fragment must be exactly one element.")
      (:edit-exceeds-limit "The edited document exceeds a resource limit: ~A")
      (:edit-does-not-parse "The edited document does not parse: ~A")
      (:edit-differs "The edited document differs from the requested change.")
      (:format-differs "The formatted document differs from the source document."))
    "Condition messages by stable ID: (MESSAGE-ID FORMAT-CONTROL).

XML-MESSAGE and XML-ERROR read this table while expanding, so each text is
written once here and nowhere in the code that signals it."))
