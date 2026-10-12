(in-package #:cl-xml-kit)

(defun %parser-tokenizer (parser)
  (or (xml-parser-tokenizer parser)
      (setf (xml-parser-tokenizer parser)
            (make-xml-tokenizer (xml-parser-input parser)
                                :source (xml-parser-source-name parser)
                                :limits (xml-parser-limits parser)))))

(defun %parser-next-token (parser)
  (loop
    for token = (next-xml-token (%parser-tokenizer parser))
    do (if (eq (xml-token-kind token) :eof)
           (progn
             (when (xml-parser-tokenizer-stack parser)
               (setf (xml-parser-tokenizer parser)
                     (pop (xml-parser-tokenizer-stack parser))))
             (return token))
           (return token))))

(defun %parser-push-tokenizer (parser source)
  (push (%parser-tokenizer parser) (xml-parser-tokenizer-stack parser))
  (setf (xml-parser-tokenizer parser)
        (make-xml-tokenizer source
                            :source (xml-parser-source-name parser)
                            :limits (xml-parser-limits parser))))

(defun %parser-token-error (parser token message constraint expected)
  (%parser-well-formedness-error parser message constraint expected
                                  (and token (xml-token-position token))))

(defun %parser-queue-event (parser event)
  (setf (xml-parser-queue parser)
        (nconc (xml-parser-queue parser) (list event))))

(defun %parser-raw-text (parser text &key attribute-p)
  (expand-xml-entity-references text (xml-parser-entities parser)
                                :attribute-p attribute-p))

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

(defun %parser-add-child (parser frame node)
  (if frame
      (push node (xml-parser-frame-children frame))
      (push node (xml-parser-document-children parser))))

(defun %parser-add-text (parser frame data position
                         &optional (queue-event-p t) (end-position position))
  (when (plusp (length data))
    (when queue-event-p
      (%parser-queue-event parser
                           (make-xml-event :type :characters :text data
                                           :position position)))
    (let* ((children (if frame (xml-parser-frame-children frame)
                         (xml-parser-document-children parser)))
           (last (car children))
           (limit (xml-resource-limits-max-text-node-length
                   (xml-parser-limits parser))))
      (if (xml-text-p last)
          (let ((length (+ (length (xml-text-data last)) (length data))))
            (when (> length limit)
              (%parser-limit-error parser :max-text-node-length length limit position))
            (let ((merged (make-xml-text
                           :data (concatenate 'string (xml-text-data last) data)
                           :position (xml-text-position last)
                           :end-position end-position)))
              (if frame
                  (setf (car (xml-parser-frame-children frame)) merged)
                  (setf (car (xml-parser-document-children parser)) merged))))
          (progn
            (when (> (length data) limit)
              (%parser-limit-error parser :max-text-node-length
                                   (length data) limit position))
            (%parser-add-child parser frame
                               (make-xml-text :data data
                                              :position position
                                              :end-position end-position)))))))

(defun %parser-parse-doctype (parser token)
  (let* ((body (xml-token-value token))
         (length (length body))
         (index 0))
    (loop while (and (< index length) (%xml-space-p (char body index)))
          do (incf index))
    (let ((start index))
      (loop while (and (< index length)
                       (%entity-name-char-p (char body index)))
            do (incf index))
      (when (= start index)
        (%parser-token-error parser token "DOCTYPE needs a root name."
                             :doctype :name))
      (let ((root-name (subseq body start index))
            (internal nil)
            (open (position #\[ body :start index))
            (close (position #\] body :from-end t)))
        (when (or (search "SYSTEM" body :start2 index :test #'char-equal)
                  (search "PUBLIC" body :start2 index :test #'char-equal))
          (when (eq (xml-parser-external-entity-policy parser) :reject)
            (error 'xml-external-resource-disabled
                   :message "External DTD resolution is disabled."
                   :identifier root-name :kind :external-dtd
                   :source (xml-parser-source-name parser)
                   :position (xml-token-position token))))
        (when (and open close (< open close))
          (setf internal (subseq body (1+ open) close)))
        (when (xml-parser-doctype-seen parser)
          (%parser-token-error parser token "Only one DOCTYPE is allowed."
                               :doctype :single-doctype))
        (setf (xml-parser-doctype-seen parser) t
              (xml-parser-doctype-root parser) root-name)
        (when internal
          (setf (xml-parser-entities parser)
                (parse-xml-internal-subset
                 internal :resource-limits (xml-parser-limits parser)
                 :external-entity-policy (xml-parser-external-entity-policy parser)
                 :source (xml-parser-source-name parser)
                 :position (xml-token-position token))))
        (%parser-queue-event parser
                             (make-xml-event :type :doctype :text body
                                             :position (xml-token-position token)))
        (%parser-add-child
         parser nil
         (make-xml-doctype :root-name root-name :internal-subset internal
                           :position (xml-token-position token)
                           :end-position (xml-token-position token)))))))

(defun %parser-collect-attributes (parser first-name)
  (declare (ignore first-name))
  (let ((entries nil) (name nil) (count 0))
    (loop for token = (%parser-next-token parser)
          do (case (xml-token-kind token)
               (:equals
                (let ((value-token (%parser-next-token parser)))
                  (unless (eq (xml-token-kind value-token) :quoted-value)
                    (%parser-token-error parser value-token
                                         "Expected a quoted attribute value."
                                         :attribute-value :quoted-value))
                  (incf count)
                  (let ((limit (xml-resource-limits-max-attributes-per-element
                                (xml-parser-limits parser))))
                    (when (> count limit)
                      (%parser-limit-error parser :max-attributes-per-element
                                           count limit)))
                  (push (cons name
                              (expand-xml-entity-references
                               (xml-token-value value-token)
                               (xml-parser-entities parser) :attribute-p t))
                        entries))
                (setf name nil))
               (:name
                (when name
                  (%parser-token-error parser token
                                       "Expected '=' after an attribute name."
                                       :attribute :equals))
                (setf name (xml-token-value token)))
               ((:tag-close :empty-element-close)
                (when name
                  (%parser-token-error parser token
                                       "Expected '=' after an attribute name."
                                       :attribute :equals))
                (return (values (nreverse entries) (xml-token-kind token)
                                (xml-token-position token))))
               (otherwise
                (%parser-token-error parser token "Unexpected token in a start tag."
                                     :start-tag :attribute-or-close))))))

(defun %parser-start-element (parser token)
  (let ((parent (car (xml-parser-stack parser)))
        (context (if (car (xml-parser-stack parser))
                     (xml-parser-frame-context (car (xml-parser-stack parser)))
                     (make-xml-namespace-context))))
    (multiple-value-bind (entries close-kind close-position)
        (%parser-collect-attributes parser (xml-token-value token))
      (multiple-value-bind (attributes declarations child-context)
          (%namespace-resolve-attributes entries context
                                          :resource-limits (xml-parser-limits parser)
                                          :position (xml-token-position token))
        (%parser-element-limit parser)
        (%parser-depth-check
         parser
         (1+ (count-if (lambda (frame)
                         (xml-parser-frame-name frame))
                       (xml-parser-stack parser)))
                             (xml-token-position token))
        (let* ((name (%namespace-resolve-element-name
                      (xml-token-value token) child-context
                      :resource-limits (xml-parser-limits parser)
                      :position (xml-token-position token)))
               (frame (%make-xml-parser-frame
                       (xml-token-value token) name attributes declarations
                       child-context nil (xml-token-position token)
                       (eq close-kind :empty-element-close) 0 nil nil)))
          (%parser-queue-event parser
                               (make-xml-event :type :start-element :name name
                                               :attributes attributes
                                               :position (xml-token-position token)))
          (if (eq close-kind :empty-element-close)
              (progn
                (%parser-add-child
                 parser parent
                 (make-xml-element
                  :name name :attributes attributes
                  :namespace-declarations declarations :children nil
                  :position (xml-token-position token)
                  :end-position close-position))
                (%parser-queue-event
                 parser (make-xml-event :type :end-element :name name
                                        :position close-position)))
              (progn
                (push frame (xml-parser-stack parser))
                (%parser-parse-contents parser frame))))))))

(defun %parser-finish-element (parser frame token)
  (unless (string= (xml-token-value token)
                   (xml-parser-frame-lexical-name frame))
    (%parser-token-error parser token "End tag does not match its start tag."
                         :element-type-match
                         (xml-parser-frame-lexical-name frame)))
  (let ((close (%parser-next-token parser)))
    (unless (eq (xml-token-kind close) :tag-close)
      (%parser-token-error parser close "Expected '>' after an end tag."
                           :end-tag :tag-close))
    (let ((parent (cadr (xml-parser-stack parser)))
          (element (make-xml-element
                    :name (xml-parser-frame-name frame)
                    :attributes (xml-parser-frame-attributes frame)
                    :namespace-declarations
                    (xml-parser-frame-namespace-declarations frame)
                    :children (nreverse (xml-parser-frame-children frame))
                    :position (xml-parser-frame-position frame)
                    :end-position (xml-token-position close))))
      (pop (xml-parser-stack parser))
      (setf (xml-parser-root-closed parser)
            (and (xml-parser-root-seen parser)
                 (null (xml-parser-stack parser))))
      (%parser-add-child parser parent element)
      (%parser-queue-event
       parser (make-xml-event :type :end-element
                              :name (xml-parser-frame-name frame)
                              :position (xml-token-position close))))))

(defun %parser-pi (parser token frame)
  (let* ((value (xml-token-value token))
         (split (position-if #'%xml-space-p value))
         (target (if split (subseq value 0 split) value))
         (data (and split (string-left-trim
                           '(#\Space #\Tab #\Newline #\Return)
                           (subseq value (1+ split))))))
    (when (string-equal target "xml")
      (%parser-token-error parser token "The XML target is reserved."
                           :processing-instruction :non-xml-target))
    (%parser-add-child parser frame
                       (make-xml-processing-instruction
                        :target target :data data
                        :position (xml-token-position token)
                        :end-position (xml-token-position token)))
    (%parser-queue-event parser
                         (make-xml-event :type :processing-instruction
                                         :target target :text data
                                         :position (xml-token-position token)))))

(defun %parser-parse-entity-fragment (parser frame declaration)
  (let ((source
          (with-output-to-string (stream)
            (%begin-entity-expansion (xml-parser-entities parser) declaration)
            (unwind-protect
                 (%materialize-entity-source-into
                  (xml-entity-declaration-replacement-text declaration)
                  (xml-parser-entities parser) stream)
              (%end-entity-expansion (xml-parser-entities parser))))))
    (%parser-push-tokenizer parser source)
    (let ((fragment
            (%make-xml-parser-frame
             nil nil nil nil
             (xml-parser-frame-context frame)
             nil (xml-parser-frame-position frame) nil 0 nil nil)))
      (push fragment (xml-parser-stack parser))
      (unwind-protect
           (%parser-parse-contents parser fragment t)
        (when (eq (car (xml-parser-stack parser)) fragment)
          (pop (xml-parser-stack parser))))
      (dolist (child (nreverse (xml-parser-frame-children fragment)))
        (if (xml-text-p child)
            (%parser-add-text parser frame
                              (xml-text-data child)
                              (xml-text-position child)
                              nil
                              (xml-text-end-position child))
            (%parser-add-child parser frame child))))))

(defun %parser-parse-contents (parser frame &optional fragment-p)
  (loop for token = (%parser-next-token parser)
        do (setf (xml-parser-root-closed parser)
                 (and (xml-parser-root-seen parser)
                      (null (xml-parser-stack parser))))
           (case (xml-token-kind token)
             (:text
              (%parser-add-text parser frame
                                (expand-xml-entity-references
                                 (xml-token-value token)
                                 (xml-parser-entities parser))
                                (xml-token-position token)))
             (:entity-reference
              (let ((declaration
                      (gethash (xml-token-value token)
                               (xml-entity-context-declarations
                                (xml-parser-entities parser)))))
                (if (and declaration
                         (not (xml-entity-declaration-external-p declaration)))
                    (%parser-parse-entity-fragment parser frame declaration)
                    (%parser-add-text parser frame
                                      (expand-xml-entity-references
                                       (format nil "&~A;" (xml-token-value token))
                                       (xml-parser-entities parser))
                                      (xml-token-position token)))))
             (:cdata
              (%parser-add-text parser frame (xml-token-value token)
                                (xml-token-position token)))
             (:comment
              (%parser-add-child parser frame
                                 (make-xml-comment :data (xml-token-value token)
                                                   :position (xml-token-position token)
                                                   :end-position (xml-token-position token)))
              (%parser-queue-event parser
                                   (make-xml-event :type :comment
                                                   :text (xml-token-value token)
                                                   :position (xml-token-position token))))

             (:processing-instruction (%parser-pi parser token frame))
             (:start-tag-open
              (let ((name (%parser-next-token parser)))
                (unless (eq (xml-token-kind name) :name)
                  (%parser-token-error parser name "Expected an element name."
                                       :start-tag :name))
                (%parser-start-element parser name)))
             (:end-tag-open
              (let ((name (%parser-next-token parser)))
                (unless (eq (xml-token-kind name) :name)
                  (%parser-token-error parser name "Expected an end-tag name."
                                       :end-tag :name))
                (when (and fragment-p (null (xml-parser-frame-name frame)))
                  (%parser-token-error
                   parser name
                   "An entity replacement cannot close an element outside its fragment."
                   :element-type-match :start-tag))
                (return (%parser-finish-element parser frame name))))
             (:eof
              (if fragment-p
                  (return)
                  (%parser-token-error parser token "Unexpected end of document."
                                       :well-formedness :end-tag)))
             (otherwise
              (%parser-token-error parser token "Unexpected token in element content."
                                   :content nil)))))

(defun %parser-xml-declaration-token-p (token)
  (let ((value (xml-token-value token)))
    (and (>= (length value) 3)
         (string= "xml" value :end2 3)
         (or (= (length value) 3)
             (%xml-space-p (char value 3))))))

(defun %parser-parse-xml-declaration (parser token)
  (let* ((body (xml-token-value token))
         (length (length body))
         (index 3))
    (labels ((fail (message expected)
               (%parser-token-error parser token message
                                    :xml-declaration expected))
             (space-p ()
               (and (< index length) (%xml-space-p (char body index))))
             (skip-space ()
               (let ((start index))
                 (loop while (space-p) do (incf index))
                 (> index start)))
             (literal-p (literal)
               (and (<= (+ index (length literal)) length)
                    (string= literal body :start2 index
                             :end2 (+ index (length literal)))))
             (read-keyword (keyword)
               (unless (literal-p keyword)
                 (fail "Expected an XML declaration keyword."
                       keyword))
               (incf index (length keyword)))
             (read-equals ()
               (skip-space)
               (unless (and (< index length) (char= (char body index) #\=))
                 (fail "Expected '=' in the XML declaration." :equals))
               (incf index)
               (skip-space))
             (read-quoted-value ()
               (unless (< index length)
                 (fail "Expected a quoted XML declaration value." :quoted-value))
               (let ((quote (char body index)))
                 (unless (or (char= quote #\") (char= quote #\'))
                   (fail "Expected a quoted XML declaration value." :quoted-value))
                 (incf index)
                 (let ((start index))
                   (loop while (and (< index length)
                                    (not (char= (char body index) quote)))
                         do (incf index))
                   (when (= index length)
                     (fail "An XML declaration value is unterminated." quote))
                   (prog1 (subseq body start index)
                     (incf index)))))
             (read-encoding ()
               (read-keyword "encoding")
               (read-equals)
               (let ((encoding (read-quoted-value)))
                 (unless (string-equal "UTF-8" encoding)
                   (error 'xml-unsupported-encoding
                          :encoding encoding
                          :reason :declaration-not-utf-8
                          :message "The XML declaration must use UTF-8."
                          :source (xml-parser-source-name parser)
                          :position (xml-token-position token)))))
             (read-standalone ()
               (read-keyword "standalone")
               (read-equals)
               (unless (member (read-quoted-value) '("yes" "no") :test #'string=)
                 (fail "Standalone must be either 'yes' or 'no'." '("yes" "no")))))
      (unless (skip-space)
        (fail "XML declaration version information is required." :version))
      (read-keyword "version")
      (read-equals)
      (unless (string= "1.0" (read-quoted-value))
        (fail "Only XML version 1.0 is supported." "1.0"))
      (let ((has-space (skip-space)))
        (when (and (not has-space) (< index length))
          (fail "XML declaration attributes must be separated by XML whitespace."
                :xml-whitespace))
        (when (< index length)
          (cond
            ((literal-p "encoding")
             (read-encoding)
             (let ((has-space (skip-space)))
               (when (and (not has-space) (< index length))
                 (fail "XML declaration attributes must be separated by XML whitespace."
                       :xml-whitespace))
               (when (< index length)
                 (read-standalone))))
            ((literal-p "standalone")
             (read-standalone))
            (t
             (fail "Expected an XML declaration keyword." '(:encoding :standalone))))
          (let ((has-space (skip-space)))
            (when (and (not has-space) (< index length))
              (fail "XML declaration attributes must be separated by XML whitespace."
                    :xml-whitespace))))
        (when (< index length)
          (fail "Unexpected content in the XML declaration." :end))))))

(defun %parser-build (parser)
  (setf (xml-parser-entities parser)
        (make-xml-entity-context
         :resource-limits (xml-parser-limits parser)
         :external-entity-policy (xml-parser-external-entity-policy parser)
         :source (xml-parser-source-name parser)))
  (loop for token = (%parser-next-token parser)
        do (case (xml-token-kind token)
             (:text
              (let ((text (xml-token-value token)))
                (unless (every #'%xml-space-p text)
                  (%parser-token-error parser token
                                       "Non-whitespace text is outside the root."
                                       :well-formedness :root-element))
                (when (xml-parser-root-seen parser)
                  (%parser-add-text parser nil
                                    (expand-xml-entity-references
                                     text (xml-parser-entities parser))
                                    (xml-token-position token)))))
             (:entity-reference
              (unless (and (xml-parser-root-seen parser)
                           (not (xml-parser-root-closed parser)))
                (%parser-token-error parser token "An entity is outside the root."
                                     :well-formedness :root-element))
              (%parser-add-text parser nil
                                (expand-xml-entity-references
                                 (format nil "&~A;" (xml-token-value token))
                                 (xml-parser-entities parser))
                                (xml-token-position token)))
             (:comment
              (%parser-add-child parser nil
                                 (make-xml-comment :data (xml-token-value token)
                                                   :position (xml-token-position token)
                                                   :end-position (xml-token-position token)))
              (%parser-queue-event parser
                                   (make-xml-event :type :comment
                                                   :text (xml-token-value token)
                                                   :position (xml-token-position token))))
             (:processing-instruction
              (if (%parser-xml-declaration-token-p token)
                  (if (and (xml-token-position token)
                           (zerop (xml-position-offset (xml-token-position token))))
                      (%parser-parse-xml-declaration parser token)
                      (%parser-token-error parser token
                                           "The XML declaration must be first."
                                           :xml-declaration :document-start))
                  (%parser-pi parser token nil)))
             (:cdata
              (%parser-token-error parser token "CDATA is outside the root."
                                   :well-formedness :root-element))
             (:doctype
              (when (xml-parser-root-seen parser)
                (%parser-token-error parser token
                                     "DOCTYPE must appear before the root element."
                                     :doctype :document-prolog))
              (%parser-parse-doctype parser token))
             (:start-tag-open
              (when (xml-parser-root-seen parser)
                (%parser-token-error parser token "A document has one root element."
                                     :well-formedness :single-root))
              (setf (xml-parser-root-seen parser) t)
              (let ((name (%parser-next-token parser)))
                (unless (eq (xml-token-kind name) :name)
                  (%parser-token-error parser name "Expected a root element name."
                                       :start-tag :name))
                (when (and (xml-parser-doctype-root parser)
                           (not (string= (xml-token-value name)
                                         (xml-parser-doctype-root parser))))
                  (%parser-token-error parser name
                                       "The document root does not match DOCTYPE."
                                       :doctype-root-match
                                       (xml-parser-doctype-root parser)))
                (%parser-start-element parser name)))
             (:end-tag-open
              (%parser-token-error parser token "An end tag has no open element."
                                   :element-type-match :start-tag))
             (:eof
              (unless (xml-parser-root-seen parser)
                (%parser-token-error parser token "A document needs a root element."
                                     :well-formedness :root-element))
              (unless (null (xml-parser-stack parser))
                (%parser-token-error parser token "The document ended in an element."
                                     :element-type-match :end-tag))
              (setf (xml-parser-root-closed parser) t)
              (return))))
  (setf (xml-parser-document-children parser)
        (nreverse (xml-parser-document-children parser)))
  (%parser-queue-event parser
                       (make-xml-event :type :end-document
                       :position (%parser-current-position parser))))

(defun %parser-input-string (input source limits)
  (let ((xml-input (make-xml-input input :source source :limits limits)))
    (values (xml-input-text xml-input) (xml-input-source xml-input))))

(defun make-xml-parser
    (input &key source-name
               (resource-limits *default-xml-resource-limits*)
               (external-entity-policy :reject))
  "Create a forward-only parser for XML input."
  (unless (or (null source-name) (stringp source-name))
    (error 'xml-input-error :message "SOURCE-NAME must be a string or NIL."))
  (unless (xml-resource-limits-p resource-limits)
    (error 'xml-input-error :message "RESOURCE-LIMITS must be XML-RESOURCE-LIMITS."))
  (unless (member external-entity-policy '(:reject :skip))
    (error 'xml-input-error :message "EXTERNAL-ENTITY-POLICY must be :REJECT or :SKIP."))
  (multiple-value-bind (text source)
      (%parser-input-string input source-name resource-limits)
    (%make-xml-parser
     text source resource-limits external-entity-policy
     nil
     0 1 1 (make-xml-position :source source :offset 0 :line 1 :column 1)
     :start nil nil nil nil nil nil nil nil nil 0 nil 0 0 0 nil)))

(defun next-xml-event (parser)
  "Return the next pull event, or NIL after END-DOCUMENT."
  (unless (xml-parser-p parser)
    (error 'xml-input-error :message "PARSER must be an XML parser."))
  (when (xml-parser-fatal parser)
    (error 'xml-parser-state-error :message "The parser is in a fatal state."
           :parser parser))
  (when (xml-parser-eof parser)
    (return-from next-xml-event nil))
  (handler-case
      (case (xml-parser-phase parser)
        (:start
         (setf (xml-parser-phase parser) :building)
         (make-xml-event :type :start-document
                         :position (xml-parser-start-position parser)))
        (:building
         (%parser-build parser)
         (setf (xml-parser-phase parser) :events)
         (next-xml-event parser))
        (:events
         (let ((event (pop (xml-parser-queue parser))))
           (if event
               (progn
                 (when (eq (xml-event-type event) :end-document)
                   (setf (xml-parser-eof parser) t))
                 event)
               (setf (xml-parser-eof parser) t))))
        (otherwise (setf (xml-parser-eof parser) t)))
    (xml-kit-error (condition)
      (setf (xml-parser-fatal parser) condition)
      (error condition))))

(defun xml-parser-eof-p (parser)
  "Return true when PARSER has emitted END-DOCUMENT."
  (unless (xml-parser-p parser)
    (error 'xml-input-error :message "PARSER must be an XML parser."))
  (xml-parser-eof parser))

(defun parse-xml
    (input &key source-name
               (resource-limits *default-xml-resource-limits*)
               (external-entity-policy :reject))
  "Parse INPUT into an immutable XML document."
  (let ((parser (make-xml-parser input :source-name source-name
                                 :resource-limits resource-limits
                                 :external-entity-policy external-entity-policy)))
    (loop while (next-xml-event parser))
    (make-xml-document :children (xml-parser-document-children parser)
                       :position (xml-parser-start-position parser)
                       :end-position (%parser-current-position parser))))

