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
  (let ((cell (list event)))
    (if (xml-parser-queue parser)
        (setf (cdr (xml-parser-queue-tail parser)) cell)
        (setf (xml-parser-queue parser) cell))
    (setf (xml-parser-queue-tail parser) cell)))

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

(defun %doctype-body-layout (body)
  "Return the root-name and internal-subset bounds of a DOCTYPE token BODY.

The values are the root-name start and end, then the internal-subset start and
end, which are NIL when BODY has no internal subset."
  (let ((length (length body))
        (index 0))
    (loop while (and (< index length) (xml-space-char-p (char body index)))
          do (incf index))
    (let ((start index))
      (loop while (and (< index length)
                       (xml-name-char-p (char body index)))
            do (incf index))
      (let ((open (position #\[ body :start index))
            (close (position #\] body :from-end t)))
        (if (and open close (< open close))
            (values start index (1+ open) close)
            (values start index nil nil))))))

(defun %parser-parse-doctype (parser token)
  (let ((body (xml-token-value token)))
    (multiple-value-bind (start index subset-start subset-end)
        (%doctype-body-layout body)
      (when (= start index)
        (%parser-token-error parser token (xml-message :doctype-needs-root-name)
                             :doctype :name))
      (let ((root-name (subseq body start index))
            (internal (and subset-start (subseq body subset-start subset-end))))
        (when (or (search "SYSTEM" body :start2 index :test #'char-equal)
                  (search "PUBLIC" body :start2 index :test #'char-equal))
          (when (eq (xml-parser-external-entity-policy parser) :reject)
            (xml-error 'xml-external-resource-disabled :external-dtd-disabled
                       :condition-initargs (:identifier root-name :kind :external-dtd
                   :source (xml-parser-source-name parser)
                                            :position (xml-token-position token)))))
        (when (xml-parser-doctype-seen parser)
          (%parser-token-error parser token (xml-message :single-doctype)
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
                                         (xml-message :expected-quoted-attribute-value)
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
    (%parser-token-error parser token (xml-message :end-tag-mismatch)
                         :element-type-match
                         (xml-parser-frame-lexical-name frame)))
  (let ((close (%parser-next-token parser)))
    (unless (eq (xml-token-kind close) :tag-close)
      (%parser-token-error parser close (xml-message :expected-close-after-end-tag)
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
         (split (position-if #'xml-space-char-p value))
         (target (if split (subseq value 0 split) value))
         (data (and split (string-left-trim
                           '(#\Space #\Tab #\Newline #\Return)
                           (subseq value (1+ split))))))
    (when (string-equal target "xml")
      (%parser-token-error parser token (xml-message :xml-target-reserved)
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
                  (%parser-token-error parser name (xml-message :expected-element-name)
                                       :start-tag :name))
                (%parser-start-element parser name)))
             (:end-tag-open
              (let ((name (%parser-next-token parser)))
                (unless (eq (xml-token-kind name) :name)
                  (%parser-token-error parser name (xml-message :expected-end-tag-name)
                                       :end-tag :name))
                (when (and fragment-p (null (xml-parser-frame-name frame)))
                  (%parser-token-error
                   parser name
                   (xml-message :entity-closes-outside-fragment)
                   :element-type-match :start-tag))
                (return (%parser-finish-element parser frame name))))
             (:eof
              (if fragment-p
                  (return)
                  (%parser-token-error parser token
                                       (xml-message :unexpected-end-of-document)
                                       :well-formedness :end-tag)))
             (otherwise
              (%parser-token-error parser token (xml-message :unexpected-token-in-content)
                                   :content nil)))))

(defun %parser-xml-declaration-token-p (token)
  (let ((value (xml-token-value token)))
    (and (>= (length value) 3)
         (string= "xml" value :end2 3)
         (or (= (length value) 3)
             (xml-space-char-p (char value 3))))))

(defun %parser-parse-xml-declaration (parser token)
  (let* ((body (xml-token-value token))
         (length (length body))
         (index 3))
    (labels ((fail (message expected)
               (%parser-token-error parser token message
                                    :xml-declaration expected))
             (space-p ()
               (and (< index length) (xml-space-char-p (char body index))))
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
                 (fail (xml-message :expected-declaration-keyword)
                       keyword))
               (incf index (length keyword)))
             (read-equals ()
               (skip-space)
               (unless (and (< index length) (char= (char body index) #\=))
                 (fail (xml-message :expected-declaration-equals) :equals))
               (incf index)
               (skip-space))
             (read-quoted-value ()
               (unless (< index length)
                 (fail (xml-message :expected-declaration-value) :quoted-value))
               (let ((quote (char body index)))
                 (unless (or (char= quote #\") (char= quote #\'))
                   (fail (xml-message :expected-declaration-value) :quoted-value))
                 (incf index)
                 (let ((start index))
                   (loop while (and (< index length)
                                    (not (char= (char body index) quote)))
                         do (incf index))
                   (when (= index length)
                     (fail (xml-message :unterminated-declaration-value) quote))
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
                          :message (xml-message :declaration-not-utf-8)
                          :source (xml-parser-source-name parser)
                          :position (xml-token-position token)))))
             (read-standalone ()
               (read-keyword "standalone")
               (read-equals)
               (unless (member (read-quoted-value) '("yes" "no") :test #'string=)
                 (fail (xml-message :standalone-invalid) '("yes" "no")))))
      (unless (skip-space)
        (fail (xml-message :declaration-version-required) :version))
      (read-keyword "version")
      (read-equals)
      (unless (string= "1.0" (read-quoted-value))
        (fail (xml-message :unsupported-version) "1.0"))
      (let ((has-space (skip-space)))
        (when (and (not has-space) (< index length))
          (fail (xml-message :declaration-needs-whitespace)
                :xml-whitespace))
        (when (< index length)
          (cond
            ((literal-p "encoding")
             (read-encoding)
             (let ((has-space (skip-space)))
               (when (and (not has-space) (< index length))
                 (fail (xml-message :declaration-needs-whitespace)
                       :xml-whitespace))
               (when (< index length)
                 (read-standalone))))
            ((literal-p "standalone")
             (read-standalone))
            (t
             (fail (xml-message :expected-declaration-keyword) '(:encoding :standalone))))
          (let ((has-space (skip-space)))
            (when (and (not has-space) (< index length))
              (fail (xml-message :declaration-needs-whitespace)
                    :xml-whitespace))))
        (when (< index length)
          (fail (xml-message :unexpected-declaration-content) :end))))))

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
                (unless (every #'xml-space-char-p text)
                  (%parser-token-error parser token
                                       (xml-message :text-outside-root)
                                       :well-formedness :root-element))
                (when (xml-parser-root-seen parser)
                  (%parser-add-text parser nil
                                    (expand-xml-entity-references
                                     text (xml-parser-entities parser))
                                    (xml-token-position token)))))
             (:entity-reference
              (unless (and (xml-parser-root-seen parser)
                           (not (xml-parser-root-closed parser)))
                (%parser-token-error parser token (xml-message :entity-outside-root)
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
                                           (xml-message :declaration-not-first)
                                           :xml-declaration :document-start))
                  (%parser-pi parser token nil)))
             (:cdata
              (%parser-token-error parser token (xml-message :cdata-outside-root)
                                   :well-formedness :root-element))
             (:doctype
              (when (xml-parser-root-seen parser)
                (%parser-token-error parser token
                                     (xml-message :doctype-after-root)
                                     :doctype :document-prolog))
              (%parser-parse-doctype parser token))
             (:start-tag-open
              (when (xml-parser-root-seen parser)
                (%parser-token-error parser token (xml-message :single-root)
                                     :well-formedness :single-root))
              (setf (xml-parser-root-seen parser) t)
              (let ((name (%parser-next-token parser)))
                (unless (eq (xml-token-kind name) :name)
                  (%parser-token-error parser name (xml-message :expected-root-name)
                                       :start-tag :name))
                (when (and (xml-parser-doctype-root parser)
                           (not (string= (xml-token-value name)
                                         (xml-parser-doctype-root parser))))
                  (%parser-token-error parser name
                                       (xml-message :root-doctype-mismatch)
                                       :doctype-root-match
                                       (xml-parser-doctype-root parser)))
                (%parser-start-element parser name)))
             (:end-tag-open
              (%parser-token-error parser token (xml-message :end-tag-without-element)
                                   :element-type-match :start-tag))
             (:eof
              (unless (xml-parser-root-seen parser)
                (%parser-token-error parser token (xml-message :root-required)
                                     :well-formedness :root-element))
              (unless (null (xml-parser-stack parser))
                (%parser-token-error parser token (xml-message :document-ended-in-element)
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
    (xml-error 'xml-input-error :label-must-be-string-or-nil
               :format-arguments ("SOURCE-NAME")))
  (check-argument resource-limits xml-resource-limits-p xml-input-error
                  :label-must-be-resource-limits
                  :format-arguments ("RESOURCE-LIMITS"))
  (unless (member external-entity-policy '(:reject :skip))
    (xml-error 'xml-input-error :external-entity-policy-invalid))
  (multiple-value-bind (text source)
      (%parser-input-string input source-name resource-limits)
    (%make-xml-parser
     text source resource-limits external-entity-policy
     nil
     0 1 1 (make-xml-position :source source :offset 0 :line 1 :column 1)
     :start nil nil nil nil nil nil nil nil nil 0 nil 0 0 0 nil nil)))

(defun next-xml-event (parser)
  "Return the next pull event, or NIL after END-DOCUMENT."
  (check-argument parser xml-parser-p xml-input-error :parser-required)
  (when (xml-parser-fatal parser)
    (xml-error 'xml-parser-state-error :parser-fatal
               :condition-initargs (:parser parser)))
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
  (check-argument parser xml-parser-p xml-input-error :parser-required)
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

