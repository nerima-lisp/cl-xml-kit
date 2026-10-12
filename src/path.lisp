;;;; src/path.lisp
;;;;
;;;; The absolute path language over lexical element names, and read-only
;;;; selection. Paths resolve against the span tree, which lists elements as
;;;; written in the source, and carry the matching DOM element along so a match
;;;; can report decoded values and namespace scope.

(in-package #:cl-xml-kit)

(defstruct (path-step
            (:constructor %make-path-step (name index))
            (:copier nil))
  "One element step. NAME is a lexical qualified name or :ANY, INDEX a 1-based
position among the children passing the name test, or NIL."
  (name nil :read-only t)
  (index nil :read-only t))

(defstruct (parsed-path
            (:constructor %make-parsed-path (steps target attribute-name))
            (:copier nil))
  "A parsed path: its element STEPS, then a TARGET of :ELEMENT, :ATTRIBUTE, or
:TEXT. ATTRIBUTE-NAME is the lexical name of an :ATTRIBUTE target."
  (steps nil :read-only t)
  (target :element :read-only t)
  (attribute-name nil :read-only t))

(defstruct (path-context
            (:constructor %make-path-context
                (source path dom spans resource-limits external-entity-policy))
            (:copier nil))
  "A parsed and scanned SOURCE that paths resolve against.

PATH is the caller's path string, kept for conditions. DOM is the PARSE-XML
document of SOURCE without a leading U+FEFF, and SPANS the span tree of SOURCE
itself. The options are those the caller passed."
  (source "" :read-only t)
  (path "" :read-only t)
  (dom nil :read-only t)
  (spans nil :read-only t)
  (resource-limits nil :read-only t)
  (external-entity-policy :reject :read-only t))

(defstruct (path-hit
            (:constructor %make-path-hit (element parent chain ordinals steps))
            (:copier nil))
  "An element reached by a path.

ELEMENT is the span element and PARENT the span element that contains it, NIL for
the document element. CHAIN lists the DOM elements from the matching one outward
to the document element. ORDINALS lists the 1-based positions among all element
children that lead from the document element to it, and STEPS the canonical step
strings, both innermost first."
  (element nil :read-only t)
  (parent nil :read-only t)
  (chain nil :read-only t)
  (ordinals nil :read-only t)
  (steps nil :read-only t))

(defstruct (xml-match
            (:constructor %make-xml-match (kind path name value start end element))
            (:copier nil)
            (:predicate xml-match-p))
  "One element, attribute, or text target that a path selected in a source string.

START and END are indices into that string, END exclusive."
  (kind :element :read-only t)
  (path "" :read-only t)
  (name nil :read-only t)
  (value nil :read-only t)
  (start 0 :read-only t)
  (end 0 :read-only t)
  (element nil :read-only t))

(defun %path-error (context reason message &key index)
  (error 'xml-path-error :path (path-context-path context) :reason reason
                         :index index :message message))

(defun %path-refuse (context reason message)
  (error 'xml-unsupported-edit :path (path-context-path context) :reason reason
                               :message message))

(defun %path-syntax-error (path index expectation)
  (error 'xml-path-error
         :path path :reason :syntax :index index
         :message (format nil "Malformed path at index ~D: expected ~A." index expectation)))

(defun %path-parse (path)
  "Parse PATH into a PARSED-PATH, or signal XML-PATH-ERROR :SYNTAX with the index of the fault."
  (unless (stringp path)
    (error 'xml-input-error :message "PATH must be a string."))
  (let ((length (length path))
        (index 0)
        (steps nil))
    (labels ((peek ()
               (and (< index length) (char path index)))
             (fail (expectation)
               (%path-syntax-error path index expectation))
             (read-ncname (expectation)
               (unless (and (peek) (xml-ncname-start-char-p (peek)))
                 (fail expectation))
               (loop while (and (peek) (xml-ncname-char-p (peek)))
                     do (incf index)))
             (read-qname (expectation)
               (let ((start index))
                 (read-ncname expectation)
                 (when (eql (peek) #\:)
                   (incf index)
                   (read-ncname "a local name after the colon"))
                 (subseq path start index)))
             (read-position ()
               (let ((start index))
                 (unless (and (peek) (char<= #\1 (peek) #\9))
                   (fail "a positive position"))
                 (loop while (and (peek) (char<= #\0 (peek) #\9))
                       do (incf index))
                 (prog1 (parse-integer path :start start :end index)
                   (unless (eql (peek) #\])
                     (fail "] after the position"))
                   (incf index))))
             (finish (target attribute-name)
               (unless (= index length)
                 (fail "the end of the path"))
               (%make-parsed-path (nreverse steps) target attribute-name)))
      (unless (eql (peek) #\/)
        (fail "a path starting with /"))
      (loop
        (incf index)
        (cond
          ((null (peek))
           (fail "a step after /"))
          ((and (char= (peek) #\@) steps)
           (incf index)
           (return (let ((name (read-qname "an attribute name")))
                     (finish :attribute name))))
          ((and steps
                (<= (+ index 6) length)
                (string= "text()" path :start2 index :end2 (+ index 6)))
           (incf index 6)
           (return (finish :text nil)))
          (t
           (let ((name (if (eql (peek) #\*)
                           (progn (incf index) :any)
                           (read-qname "an element name or *")))
                 (ordinal nil))
             (when (eql (peek) #\[)
               (incf index)
               (setf ordinal (read-position)))
             (push (%make-path-step name ordinal) steps)
             (cond
               ((null (peek))
                (return (finish :element nil)))
               ((char/= (peek) #\/)
                (fail "/ or the end of the path"))))))))))

(defun %strip-bom (string)
  (if (and (plusp (length string)) (char= (char string 0) (code-char #xFEFF)))
      (subseq string 1)
      string))

(defun %path-context (source path &key resource-limits external-entity-policy)
  "Parse and scan SOURCE for resolving PATH.

The DOM comes from PARSE-XML on SOURCE without a leading U+FEFF, which the parser
would otherwise reject; the span tree indexes SOURCE as given."
  (unless (stringp source)
    (error 'xml-input-error :message "SOURCE must be a string."))
  (let ((dom (parse-xml (%strip-bom source)
                        :resource-limits resource-limits
                        :external-entity-policy external-entity-policy)))
    (%make-path-context source path dom
                        (scan-xml-spans source
                                        :resource-limits resource-limits
                                        :external-entity-policy external-entity-policy)
                        resource-limits external-entity-policy)))

(defun %path-name-matches-p (step qname)
  (or (eq (path-step-name step) :any)
      (string= (path-step-name step) qname)))

(defun %path-markup-entity-reference (context element)
  "Return the name of an entity with markup that the content of the span ELEMENT references, or NIL."
  (let ((markup (span-document-markup-entity-names (path-context-spans context))))
    (when markup
      (dolist (child (span-element-children element))
        (when (span-text-p child)
          (let ((name (find-if (lambda (name) (member name markup :test #'string=))
                               (span-text-entity-names child))))
            (when name
              (return name))))))))

(defun %path-check-children-enumerable (context hit)
  "Refuse to enumerate the children of a HIT whose content references a markup entity."
  (let ((name (%path-markup-entity-reference context (path-hit-element hit))))
    (when name
      (%path-refuse
       context :entity-reference
       (format nil "The children of <~A> cannot be located in the source: its content references entity ~A, whose replacement text contains markup."
               (span-element-qname (path-hit-element hit)) name)))))

(defun %path-step-hits (context hit step)
  "Return the children of HIT that STEP selects, in document order."
  (%path-check-children-enumerable context hit)
  (let ((dom-children (coerce (remove-if-not #'xml-element-p
                                             (xml-element-children
                                              (car (path-hit-chain hit))))
                              'vector))
        (counts (make-hash-table :test #'equal))
        (matched 0)
        (hits nil))
    (loop for element in (remove-if-not #'span-element-p
                                        (span-element-children (path-hit-element hit)))
          for ordinal from 1
          do (let* ((qname (span-element-qname element))
                    (same-name (incf (gethash qname counts 0))))
               (when (%path-name-matches-p step qname)
                 (incf matched)
                 (when (or (null (path-step-index step))
                           (= matched (path-step-index step)))
                   (push (%make-path-hit
                          element (path-hit-element hit)
                          (cons (aref dom-children (1- ordinal)) (path-hit-chain hit))
                          (cons ordinal (path-hit-ordinals hit))
                          (cons (format nil "~A[~D]" qname same-name)
                                (path-hit-steps hit)))
                         hits)))))
    (nreverse hits)))

(defun %path-resolve-elements (context parsed)
  "Return the PATH-HITs that the element steps of PARSED select, in document order."
  (let* ((root (span-document-root (path-context-spans context)))
         (step (first (parsed-path-steps parsed)))
         (hits (when (and (%path-name-matches-p step (span-element-qname root))
                          (member (path-step-index step) '(nil 1)))
                 (list (%make-path-hit
                        root nil
                        (list (find-if #'xml-element-p
                                       (xml-document-children (path-context-dom context))))
                        nil
                        (list (format nil "~A[1]" (span-element-qname root))))))))
    (dolist (step (rest (parsed-path-steps parsed)) hits)
      (setf hits (loop for hit in hits
                       append (%path-step-hits context hit step))))))

(defun %path-hit-ordinal-path (hit)
  "Return the ordinals leading from the document element to HIT, outermost first."
  (reverse (path-hit-ordinals hit)))

(defun %path-find-attribute (element qname)
  "Return the attribute of the span ELEMENT written as QNAME, ignoring namespace declarations."
  (find-if (lambda (attribute)
             (and (not (span-attribute-declaration-p attribute))
                  (string= qname (span-attribute-qname attribute))))
           (span-element-attributes element)))

(defun %path-lexical-name (name)
  (let ((prefix (xml-name-prefix name)))
    (if prefix
        (concatenate 'string prefix ":" (xml-name-local-name name))
        (xml-name-local-name name))))

(defun %path-canonical (hit suffix)
  (with-output-to-string (stream)
    (dolist (step (reverse (path-hit-steps hit)))
      (write-char #\/ stream)
      (write-string step stream))
    (when suffix
      (write-char #\/ stream)
      (write-string suffix stream))))

(defun %path-text-value (dom-element)
  (with-output-to-string (stream)
    (dolist (child (xml-element-children dom-element))
      (when (xml-text-p child)
        (write-string (xml-text-data child) stream)))))

(defun %path-element-match (hit)
  (let ((element (path-hit-element hit)))
    (%make-xml-match :element (%path-canonical hit nil)
                     (span-element-qname element) nil
                     (span-node-start element) (span-node-end element)
                     (car (path-hit-chain hit)))))

(defun %path-attribute-match (hit attribute)
  (let ((qname (span-attribute-qname attribute))
        (dom-element (car (path-hit-chain hit))))
    (%make-xml-match :attribute (%path-canonical hit (concatenate 'string "@" qname))
                     qname
                     (xml-attribute-value
                      (find qname (xml-element-attributes dom-element)
                            :key (lambda (candidate)
                                   (%path-lexical-name (xml-attribute-name candidate)))
                            :test #'string=))
                     (span-attribute-name-start attribute)
                     (span-attribute-value-end attribute)
                     dom-element)))

(defun %path-text-match (hit)
  (let ((element (path-hit-element hit))
        (dom-element (car (path-hit-chain hit))))
    (%make-xml-match :text (%path-canonical hit "text()") nil
                     (%path-text-value dom-element)
                     (span-element-content-start element)
                     (span-element-content-end element)
                     dom-element)))

(defun select-xml (source path &key (resource-limits *default-xml-resource-limits*)
                                    (external-entity-policy :reject))
  "Return the XML-MATCH values that PATH selects in SOURCE, in document order.

PATH is absolute: element steps `/name` or `/*`, each with an optional 1-based
`[n]`, then optionally `/@name` or `/text()`. Names compare as written in
SOURCE, prefix included. The result is NIL when nothing matches. Indices in
a match refer to SOURCE itself."
  (let* ((parsed (%path-parse path))
         (context (%path-context source path
                                 :resource-limits resource-limits
                                 :external-entity-policy external-entity-policy))
         (hits (%path-resolve-elements context parsed)))
    (ecase (parsed-path-target parsed)
      (:element (mapcar #'%path-element-match hits))
      (:attribute
       (loop for hit in hits
             for attribute = (%path-find-attribute
                              (path-hit-element hit) (parsed-path-attribute-name parsed))
             when attribute
               collect (%path-attribute-match hit attribute)))
      (:text (mapcar #'%path-text-match hits)))))

(loop for (name . text) in
      '((xml-match-p . "Return true when VALUE is an XML-MATCH.")
        (xml-match-kind . "Return :ELEMENT, :ATTRIBUTE, or :TEXT.")
        (xml-match-path . "Return the canonical path of a match, with an index on every element step.")
        (xml-match-name . "Return the lexical qualified name of an element or attribute match, or NIL.")
        (xml-match-value . "Return the decoded value of an attribute or text match, or NIL.")
        (xml-match-start . "Return the index in the source where a match begins.")
        (xml-match-end . "Return the index in the source after a match.")
        (xml-match-element . "Return the DOM element that a match is, or belongs to."))
      do (setf (documentation name 'function) text))
