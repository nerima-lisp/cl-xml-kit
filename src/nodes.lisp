;;;; src/nodes.lisp

(in-package #:cl-xml-kit)

(defun %xml-invalid (message)
  (error 'xml-input-error :message message))

(defun %string-or-nil (value label)
  (unless (or (null value) (stringp value))
    (%xml-invalid (format nil "~A must be a string or NIL." label)))
  value)

(defun %string-value (value label &key (nonempty nil) (no-colon nil))
  (unless (stringp value)
    (%xml-invalid (format nil "~A must be a string." label)))
  (when (and nonempty (zerop (length value)))
    (%xml-invalid (format nil "~A must not be empty." label)))
  (when (and no-colon (find #\: value))
    (%xml-invalid (format nil "~A must not contain a colon." label)))
  value)

(defun %copy-string (value)
  (and value (copy-seq value)))

(defun %list-value (value label)
  (unless (listp value)
    (%xml-invalid (format nil "~A must be a list." label)))
  value)

(defstruct (xml-position
            (:constructor %make-xml-position (source offset line column))
            (:copier nil)
            (:predicate xml-position-p))
  "An immutable source position in normalized XML input."
  (source nil :read-only t)
  (offset 0 :read-only t)
  (line 1 :read-only t)
  (column 1 :read-only t))

(defun %position-value (value)
  (when value
    (unless (xml-position-p value)
      (%xml-invalid "A position must be an XML-POSITION value.")))
  value)

(defun make-xml-position (&key source (offset 0) (line 1) (column 1))
  "Return an immutable source position with zero-based OFFSET and one-based LINE and COLUMN."
  (%string-or-nil source "SOURCE")
  (unless (and (integerp offset) (not (minusp offset)))
    (%xml-invalid "OFFSET must be a non-negative integer."))
  (unless (and (integerp line) (plusp line))
    (%xml-invalid "LINE must be a positive integer."))
  (unless (and (integerp column) (plusp column))
    (%xml-invalid "COLUMN must be a positive integer."))
  (%make-xml-position (%copy-string source) offset line column))

(defstruct (xml-name
            (:constructor %make-xml-name (local-name namespace-uri prefix))
            (:copier nil)
            (:predicate xml-name-p))
  "An immutable expanded XML name with an optional lexical prefix."
  (local-name "" :read-only t)
  (namespace-uri "" :read-only t)
  (prefix nil :read-only t))

(defun make-xml-name (&key local-name (namespace-uri "") prefix)
  "Return an immutable expanded XML name; PREFIX is lexical and may be NIL."
  (%string-value local-name "LOCAL-NAME" :nonempty t :no-colon t)
  (%string-value namespace-uri "NAMESPACE-URI")
  (%string-or-nil prefix "PREFIX")
  (when (and prefix (find #\: prefix))
    (%xml-invalid "PREFIX must not contain a colon."))
  (%make-xml-name (%copy-string local-name) (%copy-string namespace-uri)
                  (%copy-string prefix)))

(defstruct (xml-namespace-binding
            (:constructor %make-xml-namespace-binding (prefix namespace-uri))
            (:copier nil))
  "An immutable namespace declaration in source order."
  (prefix nil :read-only t)
  (namespace-uri "" :read-only t))

(defun make-xml-namespace-binding (&key prefix namespace-uri)
  "Return an immutable namespace binding; NIL PREFIX denotes the default namespace."
  (%string-or-nil prefix "PREFIX")
  (%string-value namespace-uri "NAMESPACE-URI")
  (when (and prefix (zerop (length namespace-uri)))
    (error 'xml-namespace-error :message "A prefixed binding cannot use an empty namespace URI."
           :prefix prefix :namespace-uri namespace-uri))
  (%make-xml-namespace-binding (%copy-string prefix) (%copy-string namespace-uri)))

(defstruct (xml-document
            (:constructor %make-xml-document (children position end-position))
            (:conc-name %xml-document-)
            (:copier nil))
  "An immutable XML document node."
  (children nil :read-only t)
  (position nil :read-only t)
  (end-position nil :read-only t))

(defun make-xml-document (&key children position end-position)
  "Return an immutable XML document containing CHILDREN in source order."
  (%position-value position)
  (%position-value end-position)
  (%make-xml-document (copy-list (%list-value children "CHILDREN"))
                      position end-position))

(defstruct (xml-element
            (:constructor %make-xml-element
                (name attributes namespace-declarations children position end-position))
            (:conc-name %xml-element-)
            (:copier nil))
  "An immutable XML element node."
  (name nil :read-only t)
  (attributes nil :read-only t)
  (namespace-declarations nil :read-only t)
  (children nil :read-only t)
  (position nil :read-only t)
  (end-position nil :read-only t))

(defun make-xml-element (&key name attributes namespace-declarations children
                              position end-position)
  "Return an immutable XML element with ordered attributes, declarations, and children."
  (unless (xml-name-p name) (%xml-invalid "NAME must be an XML-NAME."))
  (%list-value attributes "ATTRIBUTES")
  (%list-value namespace-declarations "NAMESPACE-DECLARATIONS")
  (%list-value children "CHILDREN")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-element name (copy-list attributes) (copy-list namespace-declarations)
                    (copy-list children) position end-position))

(defstruct (xml-attribute
            (:constructor %make-xml-attribute (name value position end-position))
            (:conc-name %xml-attribute-)
            (:copier nil))
  "An immutable XML attribute node."
  (name nil :read-only t)
  (value "" :read-only t)
  (position nil :read-only t)
  (end-position nil :read-only t))

(defun make-xml-attribute (&key name value position end-position)
  "Return an immutable XML attribute with its normalized string VALUE."
  (unless (xml-name-p name) (%xml-invalid "NAME must be an XML-NAME."))
  (%string-value value "VALUE")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-attribute name (%copy-string value) position end-position))

(defstruct (xml-text
            (:constructor %make-xml-text (data position end-position))
            (:conc-name %xml-text-)
            (:copier nil))
  "An immutable XML text node."
  (data "" :read-only t)
  (position nil :read-only t)
  (end-position nil :read-only t))

(defun make-xml-text (&key data position end-position)
  "Return an immutable XML text node."
  (%string-value data "DATA")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-text (%copy-string data) position end-position))

(defstruct (xml-comment
            (:constructor %make-xml-comment (data position end-position))
            (:conc-name %xml-comment-)
            (:copier nil))
  "An immutable XML comment node."
  (data "" :read-only t)
  (position nil :read-only t)
  (end-position nil :read-only t))

(defun make-xml-comment (&key data position end-position)
  "Return an immutable XML comment node without its delimiters."
  (%string-value data "DATA")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-comment (%copy-string data) position end-position))

(defstruct (xml-processing-instruction
            (:constructor %make-xml-processing-instruction
                (target data position end-position))
            (:conc-name %xml-processing-instruction-)
            (:copier nil))
  "An immutable XML processing-instruction node."
  (target "" :read-only t)
  (data nil :read-only t)
  (position nil :read-only t)
  (end-position nil :read-only t))

(defun make-xml-processing-instruction (&key target data position end-position)
  "Return an immutable processing-instruction node with optional DATA."
  (%string-value target "TARGET" :nonempty t :no-colon t)
  (%string-or-nil data "DATA")
  (%position-value position)
  (%position-value end-position)
  (%make-xml-processing-instruction (%copy-string target) (%copy-string data)
                                    position end-position))

(defstruct (xml-doctype
            (:constructor %make-xml-doctype
                (root-name public-id system-id internal-subset position end-position))
            (:conc-name %xml-doctype-)
            (:copier nil))
  "An immutable XML doctype node."
  (root-name "" :read-only t)
  (public-id nil :read-only t)
  (system-id nil :read-only t)
  (internal-subset nil :read-only t)
  (position nil :read-only t)
  (end-position nil :read-only t))

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

(defun xml-document-children (object) (copy-list (%xml-document-children object)))
(defun xml-document-position (object) (%xml-document-position object))
(defun xml-document-end-position (object) (%xml-document-end-position object))
(defun xml-element-name (object) (%xml-element-name object))
(defun xml-element-attributes (object) (copy-list (%xml-element-attributes object)))
(defun xml-element-namespace-declarations (object)
  (copy-list (%xml-element-namespace-declarations object)))
(defun xml-element-children (object) (copy-list (%xml-element-children object)))
(defun xml-element-position (object) (%xml-element-position object))
(defun xml-element-end-position (object) (%xml-element-end-position object))
(defun xml-attribute-name (object) (%xml-attribute-name object))
(defun xml-attribute-value (object) (%xml-attribute-value object))
(defun xml-attribute-position (object) (%xml-attribute-position object))
(defun xml-attribute-end-position (object) (%xml-attribute-end-position object))
(defun xml-text-data (object) (%xml-text-data object))
(defun xml-text-position (object) (%xml-text-position object))
(defun xml-text-end-position (object) (%xml-text-end-position object))
(defun xml-comment-data (object) (%xml-comment-data object))
(defun xml-comment-position (object) (%xml-comment-position object))
(defun xml-comment-end-position (object) (%xml-comment-end-position object))
(defun xml-processing-instruction-target (object)
  (%xml-processing-instruction-target object))
(defun xml-processing-instruction-data (object)
  (%xml-processing-instruction-data object))
(defun xml-processing-instruction-position (object)
  (%xml-processing-instruction-position object))
(defun xml-processing-instruction-end-position (object)
  (%xml-processing-instruction-end-position object))
(defun xml-doctype-root-name (object) (%xml-doctype-root-name object))
(defun xml-doctype-public-id (object) (%xml-doctype-public-id object))
(defun xml-doctype-system-id (object) (%xml-doctype-system-id object))
(defun xml-doctype-internal-subset (object) (%xml-doctype-internal-subset object))
(defun xml-doctype-position (object) (%xml-doctype-position object))
(defun xml-doctype-end-position (object) (%xml-doctype-end-position object))

(loop for (name . text) in
      '((xml-position-source . "Return the source name of a position.")
        (xml-position-offset . "Return the zero-based normalized character offset.")
        (xml-position-line . "Return the one-based source line.")
        (xml-position-column . "Return the one-based source column.")
        (xml-name-local-name . "Return the local part of an XML name.")
        (xml-name-namespace-uri . "Return the namespace URI of an XML name.")
        (xml-name-prefix . "Return the lexical prefix of an XML name, or NIL.")
        (xml-namespace-binding-p . "Return true when VALUE is an XML namespace binding.")
        (xml-namespace-binding-prefix . "Return a binding prefix, or NIL for default namespace.")
        (xml-namespace-binding-namespace-uri . "Return a binding namespace URI.")
        (xml-document-p . "Return true when VALUE is an XML document.")
        (xml-element-p . "Return true when VALUE is an XML element.")
        (xml-attribute-p . "Return true when VALUE is an XML attribute.")
        (xml-text-p . "Return true when VALUE is an XML text node.")
        (xml-comment-p . "Return true when VALUE is an XML comment node.")
        (xml-processing-instruction-p . "Return true when VALUE is a processing-instruction node.")
        (xml-doctype-p . "Return true when VALUE is an XML doctype node.")
        (xml-document-children . "Return a fresh snapshot of document children.")
        (xml-document-position . "Return a document's start position.")
        (xml-document-end-position . "Return a document's end position.")
        (xml-element-name . "Return an element's expanded name.")
        (xml-element-attributes . "Return a fresh snapshot of element attributes.")
        (xml-element-namespace-declarations . "Return a fresh snapshot of namespace declarations.")
        (xml-element-children . "Return a fresh snapshot of element children.")
        (xml-element-position . "Return an element's start position.")
        (xml-element-end-position . "Return an element's end position.")
        (xml-attribute-name . "Return an attribute's expanded name.")
        (xml-attribute-value . "Return an attribute's normalized value.")
        (xml-attribute-position . "Return an attribute's start position.")
        (xml-attribute-end-position . "Return an attribute's end position.")
        (xml-text-data . "Return text node data.")
        (xml-text-position . "Return a text node's start position.")
        (xml-text-end-position . "Return a text node's end position.")
        (xml-comment-data . "Return comment data without delimiters.")
        (xml-comment-position . "Return a comment's start position.")
        (xml-comment-end-position . "Return a comment's end position.")
        (xml-processing-instruction-target . "Return a processing-instruction target.")
        (xml-processing-instruction-data . "Return processing-instruction data, or NIL.")
        (xml-processing-instruction-position . "Return a processing instruction's start position.")
        (xml-processing-instruction-end-position . "Return a processing instruction's end position.")
        (xml-doctype-root-name . "Return a doctype root name.")
        (xml-doctype-public-id . "Return a doctype public identifier, or NIL.")
        (xml-doctype-system-id . "Return a doctype system identifier, or NIL.")
        (xml-doctype-internal-subset . "Return a doctype internal subset, or NIL.")
        (xml-doctype-position . "Return a doctype's start position.")
        (xml-doctype-end-position . "Return a doctype's end position."))
      do (setf (documentation name 'function) text))

