;;;; src/format.lisp
;;;;
;;;; Re-indenting a document without touching its character data. Layout is
;;;; decided per element: content made only of elements, comments, and
;;;; processing instructions goes one node per line; anything else stays as it
;;;; is. The writer reuses the serializer's output state, escaping, and name and
;;;; namespace-declaration writers, but writes the declarations the source has
;;;; instead of recomputing them. The result is parsed again and must match the
;;;; source apart from the whitespace that the layout treats as insignificant.

(in-package #:cl-xml-kit)

(defstruct (format-state
            (:constructor %make-format-state (serializer indent style))
            (:copier nil))
  "Output state of one FORMAT-XML run.

SERIALIZER is the serializer state that counts and limits the output, INDENT the
spaces per level, and STYLE the line break style, :CRLF, :LF, or :CR."
  (serializer nil :read-only t)
  (indent 2 :read-only t)
  (style :lf :read-only t))

(defun %format-line-break (state)
  (%edit-convert-line-breaks (string #\Newline) (format-state-style state)))

(defun %format-write (state string object)
  (%serializer-write (format-state-serializer state) string object nil))

(defun %format-break (state level object)
  "End the line and indent the next one to LEVEL."
  (%format-write state (%format-line-break state) object)
  (%format-write state (make-string (* level (format-state-indent state))
                                    :initial-element #\Space)
                 object))

(defun %format-text (state string object)
  "Write STRING as character data, with line feeds as line breaks and carriage returns as references."
  (let ((escaped (%escape-text string nil)))
    (%format-write
     state
     (with-output-to-string (stream)
       (loop for character across escaped
             do (case character
                  (#\Return (write-string "&#13;" stream))
                  (#\Linefeed (write-string (%format-line-break state) stream))
                  (t (write-char character stream)))))
     object)))

(defun %format-whitespace-text-p (node)
  (and (xml-text-p node) (every #'xml-space-char-p (xml-text-data node))))

(defun %format-xml-space (element preserve)
  "Return whether the content of ELEMENT is to be preserved, given PRESERVE for its parent.

The nearest xml:space attribute decides: `preserve` keeps, `default` resets."
  (let ((attribute (find-if (lambda (attribute)
                              (let ((name (xml-attribute-name attribute)))
                                (and (string= (xml-name-local-name name) "space")
                                     (string= (xml-name-namespace-uri name)
                                              +xml-namespace-uri+))))
                            (xml-element-attributes element))))
    (cond
      ((null attribute) preserve)
      ((string= (xml-attribute-value attribute) "preserve") t)
      ((string= (xml-attribute-value attribute) "default") nil)
      (t preserve))))

(defun %format-block-children (children)
  "Return CHILDREN without whitespace-only text when what is left is only elements, comments, and processing instructions, at least one; else NIL."
  (let ((kept (remove-if #'%format-whitespace-text-p children)))
    (and kept
         (every (lambda (child)
                  (or (xml-element-p child) (xml-comment-p child)
                      (xml-processing-instruction-p child)))
                kept)
         kept)))

(declaim (ftype function %format-node))

(defun %format-element (state element level preserve literal)
  "Write ELEMENT. PRESERVE says an xml:space ancestor keeps content; LITERAL says an ancestor's mixed content does."
  (let* ((serializer (format-state-serializer state))
         (name (xml-element-name element))
         (local-name (xml-name-local-name name))
         (prefix (xml-name-prefix name))
         (preserve (%format-xml-space element preserve))
         (children (xml-element-children element))
         (lines (and (not preserve) (not literal) (%format-block-children children))))
    (%format-write state "<" element)
    (%write-name serializer local-name prefix element nil)
    (dolist (binding (xml-element-namespace-declarations element))
      (%write-namespace-declaration serializer binding element nil))
    (dolist (attribute (xml-element-attributes element))
      (let ((attribute-name (xml-attribute-name attribute)))
        (%format-write state " " attribute)
        (%write-name serializer (xml-name-local-name attribute-name)
                     (xml-name-prefix attribute-name) attribute nil)
        (%format-write state "=\"" attribute)
        (%write-escaped-attribute serializer (xml-attribute-value attribute) attribute nil)
        (%format-write state "\"" attribute)))
    (cond
      ((null children)
       (%format-write state "/>" element))
      (t
       (%format-write state ">" element)
       (if lines
           (progn
             (dolist (child lines)
               (%format-break state (1+ level) element)
               (%format-node state child (1+ level) preserve nil))
             (%format-break state level element))
           (dolist (child children)
             (%format-node state child (1+ level) preserve
                           (if preserve literal t))))
       (%format-write state "</" element)
       (%write-name serializer local-name prefix element nil)
       (%format-write state ">" element)))))

(defun %format-node (state node level preserve literal)
  (let ((serializer (format-state-serializer state))
        (style (format-state-style state)))
    (typecase node
      (xml-element (%format-element state node level preserve literal))
      (xml-text (%format-text state (xml-text-data node) node))
      (xml-comment
       (%write-node serializer
                    (make-xml-comment
                     :data (%edit-convert-line-breaks (xml-comment-data node) style))
                    nil 0 nil))
      (xml-processing-instruction
       (%write-node serializer
                    (make-xml-processing-instruction
                     :target (xml-processing-instruction-target node)
                     :data (let ((data (xml-processing-instruction-data node)))
                             (and data (%edit-convert-line-breaks data style))))
                    nil 0 nil)))))

(defun %format-document (context indent)
  "Return the formatted text of the document that CONTEXT holds."
  (let* ((source (path-context-source context))
         (spans (path-context-spans context))
         (declaration (span-document-declaration spans))
         (doctype (span-document-doctype spans))
         (style (span-document-line-break spans)))
    (with-output-to-string (stream)
      (let* ((state (%make-format-state
                     (%serializer-state stream
                                        :indent indent
                                        :resource-limits (path-context-resource-limits context))
                     indent style))
             (line-break (%format-line-break state)))
        (flet ((line (string object)
                 (%format-write state string object)
                 (%format-write state line-break object)))
          (when (char= (char source 0) (code-char #xFEFF))
            (%format-write state (string (code-char #xFEFF)) source))
          (when declaration
            (line (subseq source (span-node-start declaration) (span-node-end declaration))
                  declaration))
          (dolist (node (xml-document-children (path-context-dom context)))
            (typecase node
              (xml-doctype
               (line (subseq source (span-node-start doctype) (span-node-end doctype))
                     doctype))
              (xml-text
               (unless (%format-whitespace-text-p node)
                 (%format-node state node 0 nil nil)
                 (%format-write state line-break node)))
              (t
               (%format-node state node 0 nil nil)
               (%format-write state line-break node)))))))))

(defun %format-children-signature (children preserve document-p)
  "Return the signatures of CHILDREN, without the whitespace-only text that the layout treats as insignificant."
  (let* ((merged (%dom-normalize-children children nil))
         (kept (remove-if #'%format-whitespace-text-p merged)))
    (mapcar (lambda (child) (%format-signature child preserve))
            (if (or document-p
                    (and (not preserve)
                         kept
                         (every (lambda (child)
                                  (or (xml-element-p child) (xml-comment-p child)
                                      (xml-processing-instruction-p child)))
                                kept)))
                kept
                merged))))

(defun %format-signature (node &optional preserve)
  "Return a value that is EQUAL for nodes that differ only in layout whitespace."
  (typecase node
    (xml-document
     (cons :document (%format-children-signature (xml-document-children node) nil t)))
    (xml-element
     (let ((preserve (%format-xml-space node preserve)))
       (list* :element
              (let ((name (xml-element-name node)))
                (list (xml-name-local-name name) (xml-name-namespace-uri name)
                      (xml-name-prefix name)))
              (mapcar (lambda (attribute)
                        (let ((name (xml-attribute-name attribute)))
                          (list (xml-name-local-name name) (xml-name-namespace-uri name)
                                (xml-name-prefix name) (xml-attribute-value attribute))))
                      (xml-element-attributes node))
              (mapcar (lambda (binding)
                        (cons (xml-namespace-binding-prefix binding)
                              (xml-namespace-binding-namespace-uri binding)))
                      (xml-element-namespace-declarations node))
              (%format-children-signature (xml-element-children node) preserve nil))))
    (xml-text (list :text (xml-text-data node)))
    (xml-comment (list :comment (xml-comment-data node)))
    (xml-processing-instruction
     (list :processing-instruction (xml-processing-instruction-target node)
           (xml-processing-instruction-data node)))
    (xml-doctype
     (list :doctype (xml-doctype-root-name node) (xml-doctype-public-id node)
           (xml-doctype-system-id node) (xml-doctype-internal-subset node)))))

(defun %format-verify (context output)
  "Return OUTPUT when it parses to the document of CONTEXT apart from layout whitespace, else refuse."
  (unless (equal (%format-signature (path-context-dom context))
                 (%format-signature (%edit-reparse context output)))
    (%path-refuse context :verification-failed
                  "The formatted document differs from the source document."))
  output)

(defun format-xml (source &key (indent 2)
                               (resource-limits *default-xml-resource-limits*)
                               (external-entity-policy :reject))
  "Return SOURCE laid out with one node per line, indented by INDENT spaces per level.

The XML declaration and DOCTYPE are copied from SOURCE, a leading U+FEFF is kept,
and every line break is in the style of the first one in SOURCE. An element whose
content, apart from whitespace-only text, is only elements, comments, and
processing instructions puts each on its own line; any other element keeps its
content as it is, as does one under xml:space=\"preserve\". Attribute quotes
become double quotes, CDATA sections and references become escaped text, and
whitespace inside tags is normalized. The result is parsed again and must match
SOURCE apart from that whitespace, otherwise XML-UNSUPPORTED-EDIT with reason
:VERIFICATION-FAILED."
  (unless (stringp source)
    (error 'xml-input-error :message "SOURCE must be a string."))
  (unless (and (integerp indent) (not (minusp indent)))
    (error 'xml-input-error :message "INDENT must be a non-negative integer."))
  (let ((context (%path-context source nil
                                :resource-limits resource-limits
                                :external-entity-policy external-entity-policy)))
    (%format-verify context (%format-document context indent))))
