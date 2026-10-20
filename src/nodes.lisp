;;;; src/nodes.lisp

(in-package #:cl-xml-kit)

(defun %string-or-nil (value label)
  (unless (or (null value) (stringp value))
    (xml-error 'xml-input-error :label-must-be-string-or-nil :format-arguments (label)))
  value)

(defun %string-value (value label &key nonempty no-colon)
  (check-argument value stringp xml-input-error :label-must-be-string
                  :format-arguments (label))
  (when (and nonempty (zerop (length value)))
    (xml-error 'xml-input-error :label-must-not-be-empty :format-arguments (label)))
  (when (and no-colon (find #\: value))
    (xml-error 'xml-input-error :label-must-not-contain-colon :format-arguments (label)))
  value)

(defun %copy-string (value)
  (and value (copy-seq value)))

(defun %list-value (value label)
  (check-argument value listp xml-input-error :label-must-be-list
                  :format-arguments (label))
  value)

(define-immutable-record xml-position
    (:constructor %make-xml-position
     :conc-name %xml-position-
     :predicate xml-position-p
     :predicate-doc "Return true when VALUE is an XML position."
     :structure-doc "An immutable source position in normalized XML input.")
  ((source nil xml-position-source "Return the source name of a position." :identity)
   (offset 0 xml-position-offset "Return the zero-based normalized character offset."
           :identity)
   (line 1 xml-position-line "Return the one-based source line." :identity)
   (column 1 xml-position-column "Return the one-based source column." :identity)))

(defun %position-value (value)
  (when value
    (unless (xml-position-p value)
      (xml-error 'xml-input-error :position-value-required)))
  value)

(defun make-xml-position (&rest options
                             &key source offset line column)
  "Return an immutable source position with zero-based OFFSET and one-based LINE and COLUMN."
  (unless (member :offset options)
    (setf offset 0))
  (unless (member :line options)
    (setf line 1))
  (unless (member :column options)
    (setf column 1))
  (%string-or-nil source "SOURCE")
  (unless (and (integerp offset) (not (minusp offset)))
    (xml-error 'xml-input-error :offset-must-be-non-negative))
  (unless (and (integerp line) (plusp line))
    (xml-error 'xml-input-error :line-must-be-positive))
  (unless (and (integerp column) (plusp column))
    (xml-error 'xml-input-error :column-must-be-positive))
  (%make-xml-position (%copy-string source) offset line column))

(define-immutable-record xml-name
    (:constructor %make-xml-name
     :conc-name %xml-name-
     :predicate xml-name-p
     :predicate-doc "Return true when VALUE is an XML name."
     :structure-doc "An immutable expanded XML name with an optional lexical prefix.")
  ((local-name "" xml-name-local-name "Return the local part of an XML name." :identity)
   (namespace-uri "" xml-name-namespace-uri "Return the namespace URI of an XML name."
                  :identity)
   (prefix nil xml-name-prefix "Return the lexical prefix of an XML name, or NIL."
           :identity)))

(defun make-xml-name (&rest options
                         &key local-name namespace-uri prefix)
  "Return an immutable expanded XML name; PREFIX is lexical and may be NIL."
  (unless (member :namespace-uri options)
    (setf namespace-uri ""))
  (%string-value local-name "LOCAL-NAME" :nonempty t :no-colon t)
  (%string-value namespace-uri "NAMESPACE-URI")
  (%string-or-nil prefix "PREFIX")
  (when (and prefix (find #\: prefix))
    (xml-error 'xml-input-error :label-must-not-contain-colon
               :format-arguments ("PREFIX")))
  (%make-xml-name (%copy-string local-name) (%copy-string namespace-uri)
                  (%copy-string prefix)))

(define-immutable-record xml-namespace-binding
    (:constructor %make-xml-namespace-binding
     :conc-name %xml-namespace-binding-
     :predicate xml-namespace-binding-p
     :predicate-doc "Return true when VALUE is an XML namespace binding."
     :structure-doc "An immutable namespace declaration in source order.")
  ((prefix nil xml-namespace-binding-prefix
           "Return a binding prefix, or NIL for default namespace." :identity)
   (namespace-uri "" xml-namespace-binding-namespace-uri
                  "Return a binding namespace URI." :identity)))

(defun make-xml-namespace-binding (&key prefix namespace-uri)
  "Return an immutable namespace binding; NIL PREFIX denotes the default namespace."
  (%string-or-nil prefix "PREFIX")
  (%string-value namespace-uri "NAMESPACE-URI")
  (when (and prefix (zerop (length namespace-uri)))
    (xml-error 'xml-namespace-error :prefixed-binding-empty-uri
               :condition-initargs (:prefix prefix :namespace-uri namespace-uri)))
  (%make-xml-namespace-binding (%copy-string prefix) (%copy-string namespace-uri)))

(define-immutable-record xml-document
    (:constructor %make-xml-document
     :conc-name %xml-document-
     :predicate xml-document-p
     :predicate-doc "Return true when VALUE is an XML document."
     :structure-doc "An immutable XML document node.")
  ((children nil xml-document-children "Return a fresh snapshot of document children."
             :copy-list)
   (position nil xml-document-position "Return a document's start position." :identity)
   (end-position nil xml-document-end-position "Return a document's end position."
                 :identity)))

(defun make-xml-document (&key children position end-position)
  "Return an immutable XML document containing CHILDREN in source order."
  (%position-value position)
  (%position-value end-position)
  (%make-xml-document (copy-list (%list-value children "CHILDREN"))
                      position end-position))

(define-immutable-record xml-element
    (:constructor %make-xml-element
     :conc-name %xml-element-
     :predicate xml-element-p
     :predicate-doc "Return true when VALUE is an XML element."
     :structure-doc "An immutable XML element node.")
  ((name nil xml-element-name "Return an element's expanded name." :identity)
   (attributes nil xml-element-attributes
               "Return a fresh snapshot of element attributes." :copy-list)
   (namespace-declarations nil xml-element-namespace-declarations
                           "Return a fresh snapshot of namespace declarations." :copy-list)
   (children nil xml-element-children "Return a fresh snapshot of element children."
             :copy-list)
   (position nil xml-element-position "Return an element's start position." :identity)
   (end-position nil xml-element-end-position "Return an element's end position."
                 :identity)))

(defun make-xml-element (&key name attributes namespace-declarations children
                              position end-position)
  "Return an immutable XML element with ordered attributes, declarations, and children."
  (check-argument name xml-name-p xml-input-error :label-must-be-xml-name
                  :format-arguments ("NAME"))
  (%list-value attributes "ATTRIBUTES")
  (%list-value namespace-declarations "NAMESPACE-DECLARATIONS")
  (%list-value children "CHILDREN")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-element name (copy-list attributes) (copy-list namespace-declarations)
                    (copy-list children) position end-position))

(define-immutable-record xml-attribute
    (:constructor %make-xml-attribute
     :conc-name %xml-attribute-
     :predicate xml-attribute-p
     :predicate-doc "Return true when VALUE is an XML attribute."
     :structure-doc "An immutable XML attribute node.")
  ((name nil xml-attribute-name "Return an attribute's expanded name." :identity)
   (value "" xml-attribute-value "Return an attribute's normalized value." :identity)
   (position nil xml-attribute-position "Return an attribute's start position." :identity)
   (end-position nil xml-attribute-end-position "Return an attribute's end position."
                 :identity)))

(defun make-xml-attribute (&key name value position end-position)
  "Return an immutable XML attribute with its normalized string VALUE."
  (check-argument name xml-name-p xml-input-error :label-must-be-xml-name
                  :format-arguments ("NAME"))
  (%string-value value "VALUE")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-attribute name (%copy-string value) position end-position))

(define-immutable-record xml-text
    (:constructor %make-xml-text
     :conc-name %xml-text-
     :predicate xml-text-p
     :predicate-doc "Return true when VALUE is an XML text node."
     :structure-doc "An immutable XML text node.")
  ((data "" xml-text-data "Return text node data." :identity)
   (position nil xml-text-position "Return a text node's start position." :identity)
   (end-position nil xml-text-end-position "Return a text node's end position." :identity)))

(defun make-xml-text (&key data position end-position)
  "Return an immutable XML text node."
  (%string-value data "DATA")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-text (%copy-string data) position end-position))

(define-immutable-record xml-comment
    (:constructor %make-xml-comment
     :conc-name %xml-comment-
     :predicate xml-comment-p
     :predicate-doc "Return true when VALUE is an XML comment node."
     :structure-doc "An immutable XML comment node.")
  ((data "" xml-comment-data "Return comment data without delimiters." :identity)
   (position nil xml-comment-position "Return a comment's start position." :identity)
   (end-position nil xml-comment-end-position "Return a comment's end position."
                 :identity)))

(defun make-xml-comment (&key data position end-position)
  "Return an immutable XML comment node without its delimiters."
  (%string-value data "DATA")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-comment (%copy-string data) position end-position))

(define-immutable-record xml-processing-instruction
    (:constructor %make-xml-processing-instruction
     :conc-name %xml-processing-instruction-
     :predicate xml-processing-instruction-p
     :predicate-doc "Return true when VALUE is a processing-instruction node."
     :structure-doc "An immutable XML processing-instruction node.")
  ((target "" xml-processing-instruction-target
           "Return a processing-instruction target." :identity)
   (data nil xml-processing-instruction-data
         "Return processing-instruction data, or NIL." :identity)
   (position nil xml-processing-instruction-position
             "Return a processing instruction's start position." :identity)
   (end-position nil xml-processing-instruction-end-position
                 "Return a processing instruction's end position." :identity)))

(defun make-xml-processing-instruction (&key target data position end-position)
  "Return an immutable processing-instruction node with optional DATA."
  (%string-value target "TARGET" :nonempty t :no-colon t)
  (%string-or-nil data "DATA")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-processing-instruction (%copy-string target) (%copy-string data)
                                    position end-position))

(define-immutable-record xml-doctype
    (:constructor %make-xml-doctype
     :conc-name %xml-doctype-
     :predicate xml-doctype-p
     :predicate-doc "Return true when VALUE is an XML doctype node."
     :structure-doc "An immutable XML doctype node.")
  ((root-name "" xml-doctype-root-name "Return a doctype root name." :identity)
   (public-id nil xml-doctype-public-id "Return a doctype public identifier, or NIL."
              :identity)
   (system-id nil xml-doctype-system-id "Return a doctype system identifier, or NIL."
              :identity)
   (internal-subset nil xml-doctype-internal-subset
                    "Return a doctype internal subset, or NIL." :identity)
   (position nil xml-doctype-position "Return a doctype's start position." :identity)
   (end-position nil xml-doctype-end-position "Return a doctype's end position."
                 :identity)))

(defun make-xml-doctype (&key root-name public-id system-id internal-subset
                              position end-position)
  "Return an immutable doctype node without resolving its external identifier."
  (%string-value root-name "ROOT-NAME" :nonempty t :no-colon t)
  (%string-or-nil public-id "PUBLIC-ID")
  (%string-or-nil system-id "SYSTEM-ID")
  (%string-or-nil internal-subset "INTERNAL-SUBSET")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-doctype (%copy-string root-name) (%copy-string public-id)
                     (%copy-string system-id) (%copy-string internal-subset)
                     position end-position))

