(in-package #:cl-xml-kit)

(defun %parser-element-limit (parser)
  (let ((value (1+ (xml-parser-element-count parser)))
        (limit (xml-resource-limits-max-elements (xml-parser-limits parser))))
    (when (> value limit)
      (%parser-limit-error parser :max-elements value limit))
    (incf (xml-parser-element-count parser))))

(defun %parser-depth-check (parser depth position)
  (let ((limit (xml-resource-limits-max-depth (xml-parser-limits parser))))
    (when (> depth limit)
      (%parser-limit-error parser :max-depth depth limit position))))

(defun %parser-characters (parser data position)
  "Return a characters event for DATA, or NIL when DATA is empty."
  (when (plusp (length data))
    (let ((length (incf (xml-parser-text-length parser) (length data)))
          (limit (xml-resource-limits-max-text-node-length
                  (xml-parser-limits parser))))
      (when (> length limit)
        (%parser-limit-error parser :max-text-node-length length limit position))
      (%parser-event parser :characters :text data :position position))))

(defun %parser-attribute-value-token (parser)
  (let ((token (%parser-next-token parser)))
    (unless (eq (xml-token-kind token) :quoted-value)
      (%parser-token-error parser token
                           (xml-message :expected-quoted-attribute-value)
                           :attribute-value :quoted-value))
    token))

(defun %parser-collect-attributes (parser)
  "Read a start tag through its close and return the raw (NAME . VALUE) entries,
the close token kind, and the close position."
  (let ((entries nil) (name nil) (count 0)
        (limit (xml-resource-limits-max-attributes-per-element
                (xml-parser-limits parser))))
    (loop for token = (%parser-next-token parser)
          do (case (xml-token-kind token)
               (:equals
                (let ((value-token (%parser-attribute-value-token parser)))
                  (when (> (incf count) limit)
                    (%parser-limit-error parser :max-attributes-per-element count limit))
                  (push (cons name (expand-xml-entity-references
                                    (xml-token-value value-token)
                                    (xml-parser-entities parser) :attribute-p t))
                        entries))
                (setf name nil))
               (:name
                (when name
                  (%parser-token-error parser token
                                       (xml-message :expected-equals-after-attribute)
                                       :attribute :equals))
                (setf name (xml-token-value token)))
               ((:tag-close :empty-element-close)
                (when name
                  (%parser-token-error parser token
                                       (xml-message :expected-equals-after-attribute)
                                       :attribute :equals))
                (return (values (nreverse entries) (xml-token-kind token)
                                (xml-token-position token))))
               (otherwise
                (%parser-token-error parser token
                                     (xml-message :unexpected-token-in-start-tag)
                                     :start-tag :attribute-or-close))))))

(defun %parser-start-element (parser name-token)
  "Read the start tag named by NAME-TOKEN, open its frame, and return its event."
  (let* ((parent (car (xml-parser-stack parser)))
         (context (if parent
                      (xml-parser-frame-context parent)
                      (make-xml-namespace-context)))
         (lexical-name (xml-token-value name-token))
         (position (xml-token-position name-token))
         (limits (xml-parser-limits parser)))
    (multiple-value-bind (entries close-kind close-position)
        (%parser-collect-attributes parser)
      (multiple-value-bind (attributes declarations child-context)
          (%namespace-resolve-attributes entries context
                                          :resource-limits limits :position position)
        (%parser-element-limit parser)
        (%parser-depth-check parser (1+ (xml-parser-depth parser)) position)
        (let ((name (%namespace-resolve-qname lexical-name child-context
                                               :resource-limits limits
                                               :position position))
              (empty-p (eq close-kind :empty-element-close)))
          (push (%make-xml-parser-frame lexical-name name declarations child-context
                                        (and empty-p close-position))
                (xml-parser-stack parser))
          (incf (xml-parser-depth parser))
          (when empty-p
            (setf (xml-parser-phase parser) :empty-end))
          (%parser-event parser :start-element :name name :attributes attributes
                                               :position position))))))

(defun %parser-close-empty-element (parser)
  (let ((frame (pop (xml-parser-stack parser))))
    (decf (xml-parser-depth parser))
    (setf (xml-parser-phase parser) :running)
    (%parser-event parser :end-element :name (xml-parser-frame-name frame)
                                       :position (xml-parser-frame-end-position frame))))

(defun %parser-end-element (parser name-token)
  "Read the end tag named by NAME-TOKEN, close the innermost element, and return its event."
  (let ((frame (car (xml-parser-stack parser))))
    (unless (xml-parser-frame-name frame)
      (%parser-token-error parser name-token
                           (xml-message :entity-closes-outside-fragment)
                           :element-type-match :start-tag))
    (unless (string= (xml-token-value name-token)
                     (xml-parser-frame-lexical-name frame))
      (%parser-token-error parser name-token (xml-message :end-tag-mismatch)
                           :element-type-match (xml-parser-frame-lexical-name frame)))
    (let ((close (%parser-next-token parser)))
      (unless (eq (xml-token-kind close) :tag-close)
        (%parser-token-error parser close (xml-message :expected-close-after-end-tag)
                             :end-tag :tag-close))
      (pop (xml-parser-stack parser))
      ;; Only a root with an end tag sets ROOT-CLOSED, so a reference after an
      ;; empty root is still accepted as document text.
      (when (zerop (decf (xml-parser-depth parser)))
        (setf (xml-parser-root-closed parser) t))
      (%parser-event parser :end-element :name (xml-parser-frame-name frame)
                                         :position (xml-token-position close)))))

(defun %parser-pi (parser token)
  (let* ((value (xml-token-value token))
         (split (position-if #'xml-space-char-p value))
         (target (if split (subseq value 0 split) value))
         (data (and split
                    (subseq value (or (position-if-not #'xml-space-char-p value
                                                       :start split)
                                      (length value))))))
    (when (string-equal target "xml")
      (%parser-token-error parser token (xml-message :xml-target-reserved)
                           :processing-instruction :non-xml-target))
    ;; Check the target as the node constructor does, so that the pull reader
    ;; rejects what PARSE-XML rejects.
    (%string-value target "TARGET" :nonempty t :no-colon t)
    (%parser-event parser :processing-instruction :target target :text data
                                                  :position (xml-token-position token))))

(defun %parser-enter-entity (parser declaration)
  "Continue parsing in the replacement text of the internal entity DECLARATION."
  (let* ((entities (xml-parser-entities parser))
         (source (with-output-to-string (stream)
                   (%begin-entity-expansion entities declaration)
                   (unwind-protect
                        (%materialize-entity-source-into
                         (xml-entity-declaration-replacement-text declaration)
                         entities stream)
                     (%end-entity-expansion entities)))))
    (%parser-push-tokenizer parser source)
    (push (%make-xml-parser-frame nil nil nil
                                  (xml-parser-frame-context (car (xml-parser-stack parser)))
                                  nil)
          (xml-parser-stack parser))
    nil))

(defun %parser-skipped-entity (parser declaration token)
  "Return the event for TOKEN, a reference to the external entity DECLARATION.

Under the :REJECT policy this signals instead."
  (%external-entity-result (xml-parser-entities parser) declaration)
  (%parser-event parser :skipped-external-entity
                 :name (%make-xml-name (xml-token-value token) "" nil)
                 :position (xml-token-position token)))

(defun %parser-entity-reference (parser token)
  "Return the event, if any, for a general entity reference in content."
  (let* ((name (xml-token-value token))
         (entities (xml-parser-entities parser))
         (declaration (gethash name (xml-entity-context-declarations entities))))
    (cond
      ((null declaration)
       (%parser-characters parser
                           (expand-xml-entity-references (format nil "&~A;" name)
                                                         entities)
                           (xml-token-position token)))
      ((not (xml-entity-declaration-external-p declaration))
       (%parser-enter-entity parser declaration))
      (t (%parser-skipped-entity parser declaration token)))))

(defun %parser-content-end (parser token)
  "Leave the entity replacement whose EOF TOKEN was read, or signal an unclosed element."
  (if (xml-parser-frame-name (car (xml-parser-stack parser)))
      (%parser-token-error parser token (xml-message :unexpected-end-of-document)
                           :well-formedness :end-tag)
      (pop (xml-parser-stack parser)))
  nil)

(defun %parser-content-token (parser token)
  "Interpret TOKEN inside an element; return its event, or NIL when it has none."
  (case (xml-token-kind token)
    ((:text :cdata)
     (%parser-characters parser (xml-token-value token) (xml-token-position token)))
    (:entity-reference (%parser-entity-reference parser token))
    (:comment
     (%parser-event parser :comment :text (xml-token-value token)
                                    :position (xml-token-position token)))
    (:processing-instruction (%parser-pi parser token))
    (:start-tag-open
     (%parser-start-element
      parser (%parser-expect-name parser (xml-message :expected-element-name) :start-tag)))
    (:end-tag-open
     (%parser-end-element
      parser (%parser-expect-name parser (xml-message :expected-end-tag-name) :end-tag)))
    (:eof (%parser-content-end parser token))
    (otherwise
     (%parser-token-error parser token (xml-message :unexpected-token-in-content)
                          :content nil))))

(defun %parser-step (parser)
  "Advance PARSER by one event and return it, or NIL after the end-document event."
  (ecase (xml-parser-phase parser)
    (:start
     (setf (xml-parser-phase parser) :running)
     (%parser-event parser :start-document
                    :position (xml-parser-start-position parser)))
    (:empty-end (%parser-close-empty-element parser))
    (:running
     (loop
       (let* ((token (%parser-next-token parser))
              (event (if (plusp (xml-parser-depth parser))
                         (%parser-content-token parser token)
                         (%parser-top-level-token parser token))))
         (when event
           (return event)))))
    (:done nil)))
