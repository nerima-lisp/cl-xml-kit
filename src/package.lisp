;;;; src/package.lisp

(defpackage #:cl-xml-kit
  (:use #:cl)
  (:export
   ;; Parsing and pull events
   #:parse-xml #:make-xml-parser #:next-xml-event #:xml-parser-eof-p
   #:xml-event #:make-xml-event #:xml-event-p #:xml-event-type
   #:xml-event-name #:xml-event-attributes #:xml-event-text
   #:xml-event-target #:xml-event-position #:xml-event-external-p
   ;; Resource limits
   #:*default-xml-resource-limits* #:make-xml-resource-limits
   #:xml-resource-limits-p #:xml-resource-limits-max-input-octets
   #:xml-resource-limits-max-document-characters
   #:xml-resource-limits-max-output-characters #:xml-resource-limits-max-depth
   #:xml-resource-limits-max-elements
   #:xml-resource-limits-max-attributes-per-element
   #:xml-resource-limits-max-name-length
   #:xml-resource-limits-max-text-node-length
   #:xml-resource-limits-max-namespace-bindings
   #:xml-resource-limits-max-entity-expansions
   #:xml-resource-limits-max-expanded-characters
   #:xml-resource-limits-max-entity-depth
   ;; Positions and names
   #:xml-position #:make-xml-position #:xml-position-source
   #:xml-position-offset #:xml-position-line #:xml-position-column
   #:xml-name #:make-xml-name #:xml-name-local-name
   #:xml-name-namespace-uri #:xml-name-prefix
   #:xml-namespace-binding #:make-xml-namespace-binding
   #:xml-namespace-binding-p #:xml-namespace-binding-prefix
   #:xml-namespace-binding-namespace-uri
   ;; Tree nodes
   #:xml-document #:make-xml-document #:xml-document-children
   #:xml-document-position #:xml-document-end-position
   #:xml-element #:make-xml-element #:xml-element-name
   #:xml-element-attributes #:xml-element-namespace-declarations
   #:xml-element-children #:xml-element-position #:xml-element-end-position
   #:xml-attribute #:make-xml-attribute #:xml-attribute-name
   #:xml-attribute-value #:xml-attribute-position #:xml-attribute-end-position
   #:xml-text #:make-xml-text #:xml-text-data #:xml-text-position
   #:xml-text-end-position #:xml-comment #:make-xml-comment
   #:xml-comment-data #:xml-comment-position #:xml-comment-end-position
   #:xml-processing-instruction #:make-xml-processing-instruction
   #:xml-processing-instruction-target #:xml-processing-instruction-data
   #:xml-processing-instruction-position
   #:xml-processing-instruction-end-position
   #:xml-doctype #:make-xml-doctype #:xml-doctype-root-name
   #:xml-doctype-public-id #:xml-doctype-system-id
   #:xml-doctype-internal-subset #:xml-doctype-position
   #:xml-doctype-end-position
   #:xml-document-p #:xml-element-p #:xml-attribute-p #:xml-text-p
   #:xml-comment-p #:xml-processing-instruction-p #:xml-doctype-p
   ;; Serialization
   #:xml->string #:xml->canonical-string #:write-xml #:write-xml-canonical
   ;; Conditions
   #:xml-kit-error #:xml-input-error #:xml-decode-error
   #:xml-unsupported-encoding #:xml-parse-error
   #:xml-well-formedness-error #:xml-namespace-error
   #:xml-external-resource-disabled #:xml-parser-state-error
   #:xml-resource-limit-exceeded #:xml-invalid-resource-limit
   #:xml-serialization-error #:xml-condition-message
   #:xml-condition-position #:xml-condition-source
   #:xml-decode-error-octet #:xml-decode-error-encoding
   #:xml-unsupported-encoding-encoding #:xml-unsupported-encoding-reason
   #:xml-well-formedness-error-constraint
   #:xml-well-formedness-error-expected #:xml-namespace-error-prefix
   #:xml-namespace-error-namespace-uri
   #:xml-external-resource-disabled-identifier
   #:xml-external-resource-disabled-kind #:xml-parser-state-error-parser
   #:xml-resource-limit-exceeded-kind #:xml-resource-limit-exceeded-value
   #:xml-resource-limit-exceeded-limit #:xml-invalid-resource-limit-kind
   #:xml-invalid-resource-limit-value #:xml-serialization-error-object
   #:xml-serialization-error-path
   #:xml-edit-error #:xml-path-error #:xml-unsupported-edit
   #:xml-edit-value-error #:xml-edit-error-path #:xml-edit-error-reason
   #:xml-path-error-index #:xml-edit-value-error-cause
   ;; Source-preserving reading and editing
   #:select-xml #:xml-match #:xml-match-p #:xml-match-kind #:xml-match-path
   #:xml-match-name #:xml-match-value #:xml-match-start #:xml-match-end
   #:xml-match-element #:edit-xml-set #:edit-xml-delete
   #:edit-xml-append #:format-xml))

