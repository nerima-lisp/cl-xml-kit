;;;; src/serializer-node.lisp

(in-package #:cl-xml-kit)

(defun %canonical-notations (internal-subset)
  "Return the trimmed lines of INTERNAL-SUBSET that hold a NOTATION declaration, each ending in a newline."
  (with-output-to-string (output)
    (when internal-subset
      (with-input-from-string (input internal-subset)
        (loop for line = (read-line input nil nil)
              while line
              do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                   (when (search "<!NOTATION" trimmed)
                     (write-string trimmed output)
                     (write-char #\Newline output))))))))

(defun %write-canonical-doctype (state doctype root-name path)
  "Write the canonical DOCTYPE, which keeps only the NOTATION declarations and is omitted without them."
  (let ((notations (%canonical-notations (xml-doctype-internal-subset doctype))))
    (when (plusp (length notations))
      (%serializer-write state "<!DOCTYPE " doctype path)
      (%serializer-write state root-name doctype path)
      (%serializer-write state " [" doctype path)
      (%serializer-write state (string #\Newline) doctype path)
      (%serializer-write state notations doctype path)
      (%serializer-write state "]>" doctype path))))

(defun %write-quoted-id (state keyword id doctype path)
  (%serializer-write state keyword doctype path)
  (%write-escaped-attribute state id doctype path)
  (%serializer-write state "\"" doctype path))

(defun %write-doctype (state doctype path)
  (let ((root-name (xml-doctype-root-name doctype))
        (public-id (xml-doctype-public-id doctype))
        (system-id (xml-doctype-system-id doctype))
        (internal-subset (xml-doctype-internal-subset doctype)))
    (%check-xml-characters root-name doctype path)
    (when (xml-serializer-state-canonical state)
      (return-from %write-doctype
        (%write-canonical-doctype state doctype root-name path)))
    (%serializer-write state "<!DOCTYPE " doctype path)
    (%serializer-write state root-name doctype path)
    (cond
      (public-id
       (%write-quoted-id state " PUBLIC \"" public-id doctype path)
       (when system-id
         (%write-quoted-id state " \"" system-id doctype path)))
      (system-id
       (%write-quoted-id state " SYSTEM \"" system-id doctype path)))
    (when internal-subset
      (%serializer-write state " [" doctype path)
      (%serializer-write state internal-subset doctype path)
      (%serializer-write state "]" doctype path))
    (%serializer-write state ">" doctype path)))

(defun %write-document (state document scope level path)
  "Write the children of DOCUMENT; canonical output drops whitespace-only text between them."
  (let ((children (xml-document-children document))
        (canonical (xml-serializer-state-canonical state)))
    (unless (every #'xml-node-p children)
      (%serializer-error document path (xml-message :document-children-invalid)))
    (when children
      (let ((child-path (append path (list :child))))
        (dolist (child children)
          (unless (and canonical
                       (xml-text-p child)
                       (every #'xml-space-char-p (xml-text-data child)))
            (%write-node state child scope level child-path)))))))

(defun %write-attribute-node (state attribute path)
  (multiple-value-bind (local-name namespace-uri prefix)
      (%name-components (xml-attribute-name attribute) attribute path)
    (when (and prefix (zerop (length namespace-uri)))
      (%serializer-error attribute path (xml-message :prefixed-attribute-needs-uri)))
    (%write-attribute state local-name prefix attribute path)))

(defun %write-comment (state data object path)
  "Write a comment holding DATA, which must not contain -- or end with a hyphen."
  (%check-xml-characters data object path)
  (when (or (search "--" data)
            (and (plusp (length data)) (char= (char data (1- (length data))) #\-)))
    (%serializer-error object path (xml-message :comment-not-representable)))
  (%serializer-write state "<!--" object path)
  (%serializer-write state data object path)
  (%serializer-write state "-->" object path))

(defun %write-processing-instruction (state target data object path)
  "Write a processing instruction. Canonical output keeps the space after TARGET even without DATA."
  (%check-xml-characters target object path)
  (when (string-equal "xml" target)
    (%serializer-error object path (xml-message :pi-target-xml-reserved)))
  (when (and data (search "?>" data))
    (%serializer-error object path (xml-message :pi-data-contains-close)))
  (%serializer-write state "<?" object path)
  (%serializer-write state target object path)
  (when (or data (xml-serializer-state-canonical state))
    (%serializer-write state " " object path)
    (when data
      (%check-xml-characters data object path)
      (%serializer-write state data object path)))
  (%serializer-write state "?>" object path))

(defun %write-node (state object scope level path)
  "Write OBJECT. Every caller has checked it with XML-NODE-P, so the last clause
sees only a doctype."
  (cond
    ((xml-document-p object) (%write-document state object scope level path))
    ((xml-element-p object) (%write-element state object scope level path))
    ((xml-attribute-p object) (%write-attribute-node state object path))
    ((xml-text-p object)
     (%write-escaped-text state (xml-text-data object) object path))
    ((xml-comment-p object)
     (unless (xml-serializer-state-canonical state)
       (%write-comment state (xml-comment-data object) object path)))
    ((xml-processing-instruction-p object)
     (%write-processing-instruction state (xml-processing-instruction-target object)
                                    (xml-processing-instruction-data object) object path))
    (t (%write-doctype state object path))))

