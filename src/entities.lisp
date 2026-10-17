;;;; src/entities.lisp

(in-package #:cl-xml-kit)

(defparameter +xml-predefined-entities+
  '(("amp" . #\&) ("lt" . #\<) ("gt" . #\>)
    ("apos" . #\') ("quot" . #\"))
  "The five XML predefined general entities.")

(defstruct (xml-entity-declaration
            (:constructor %make-xml-entity-declaration
                (name replacement-text parameter-p external-p public-id system-id))
            (:copier nil))
  "An internal entity declaration, or an external declaration marker."
  (name "" :read-only t)
  (replacement-text nil :read-only t)
  (parameter-p nil :read-only t)
  (external-p nil :read-only t)
  (public-id nil :read-only t)
  (system-id nil :read-only t))

(defstruct (xml-entity-context
            (:constructor %make-xml-entity-context
                (declarations parameter-declarations limits external-entity-policy
                 source position expansions expanded-characters expansion-stack
                 skipped-external-entities attlist-element-names)))
  "Mutable, parse-local state used while expanding XML entities."
  (declarations (make-hash-table :test #'equal))
  (parameter-declarations (make-hash-table :test #'equal))
  (limits *default-xml-resource-limits*)
  (external-entity-policy :reject)
  (source nil)
  (position nil)
  (expansions 0)
  (expanded-characters 0)
  (expansion-stack nil)
  (skipped-external-entities nil)
  (attlist-element-names nil))

(defun make-xml-entity-context (&key
                                  (resource-limits *default-xml-resource-limits*)
                                  (external-entity-policy :reject)
                                  source position)
  "Create parse-local entity state using RESOURCE-LIMITS and POLICY."
  (check-argument resource-limits xml-resource-limits-p xml-input-error
                  :label-must-be-resource-limits-value
                  :format-arguments ("RESOURCE-LIMITS"))
  (unless (member external-entity-policy '(:reject :skip))
    (xml-error 'xml-input-error :external-entity-policy-invalid))
  (unless (or (null source) (stringp source))
    (xml-error 'xml-input-error :label-must-be-string-or-nil
               :format-arguments ("SOURCE")))
  (when (and position (not (xml-position-p position)))
    (xml-error 'xml-input-error :label-must-be-xml-position
               :format-arguments ("POSITION")))
  (%make-xml-entity-context (make-hash-table :test #'equal)
                            (make-hash-table :test #'equal)
                            resource-limits external-entity-policy
                            (and source (copy-seq source)) position 0 0 nil nil nil))

(defun make-xml-entity-declaration (&key name replacement-text parameter-p
                                         external-p public-id system-id)
  "Create an internal entity declaration for parser-local use."
  (unless (and (stringp name) (plusp (length name)))
    (xml-error 'xml-input-error :entity-name-invalid))
  (when (and (not external-p) (not (stringp replacement-text)))
    (xml-error 'xml-input-error :internal-entity-needs-text))
  (when (and external-p replacement-text)
    (xml-error 'xml-input-error :external-entity-has-text))
  (%make-xml-entity-declaration (copy-seq name)
                                (and replacement-text (copy-seq replacement-text))
                                (not (null parameter-p)) (not (null external-p))
                                (and public-id (copy-seq public-id))
                                (and system-id (copy-seq system-id))))

(defun %entity-error (context message constraint &optional expected)
  (error 'xml-well-formedness-error
         :message message :constraint constraint :expected expected
         :source (xml-entity-context-source context)
         :position (xml-entity-context-position context)))

(defun %entity-limit-error (context kind value limit)
  (error 'xml-resource-limit-exceeded
         :message (xml-message :entity-resource-limit-exceeded kind value limit)
         :kind kind :value value :limit limit
         :source (xml-entity-context-source context)
         :position (xml-entity-context-position context)))

(defun %external-entity-error (context declaration)
  (let ((name (xml-entity-declaration-name declaration)))
    (error 'xml-external-resource-disabled
           :message (xml-message :external-entity-disabled
                                 (if (xml-entity-declaration-parameter-p declaration)
                                "parameter" "general") name)
           :identifier (or (xml-entity-declaration-system-id declaration) name)
           :kind (if (xml-entity-declaration-parameter-p declaration)
                     :parameter-entity :general-entity)
           :source (xml-entity-context-source context)
           :position (xml-entity-context-position context))))

(defun %skip-xml-space (text position)
  (loop while (and (< position (length text))
                   (xml-space-char-p (char text position)))
        do (incf position)
        finally (return position)))

(defun %entity-reference-end (text start context)
  (or (position #\; text :start start)
      (%entity-error context (xml-message :entity-reference-needs-semicolon)
                     :entity-reference ";")))

(defun %parse-character-reference (body context)
  (when (zerop (length body))
    (%entity-error context (xml-message :character-reference-empty)
                   :character-reference))
  (let* ((hex-p (and (> (length body) 2)
                     (char= (char body 0) #\#)
                     (char= (char body 1) #\x)))
         (decimal-p (and (> (length body) 1)
                         (char= (char body 0) #\#)
                         (not hex-p)))
         (digits (cond (hex-p (subseq body 2))
                       (decimal-p (subseq body 1))
                       (t nil))))
    (unless digits
      (%entity-error context (xml-message :malformed-character-reference)
                     :character-reference))
    (when (zerop (length digits))
      (%entity-error context (xml-message :character-reference-no-digits)
                     :character-reference))
    (let ((value 0)
          (radix (if hex-p 16 10)))
      (loop for character across digits
            for number = (xml-digit-weight character radix)
            do (unless number
                 (%entity-error context (xml-message :malformed-character-reference)
                                :character-reference))
               (setf value (+ (* value radix) number)))
      (unless (< value char-code-limit)
        (%entity-error context (xml-message :character-reference-illegal)
                       :legal-character))
      (let ((character (code-char value)))
        (unless (and character (xml-char-p character))
          (%entity-error context (xml-message :character-reference-illegal)
                         :legal-character))
        character))))

(defun %validate-entity-value (text context)
  (let ((position 0))
    (loop while (< position (length text))
          do (let ((marker (position #\& text :start position)))
               (unless marker
                 (return))
               (let* ((end (%entity-reference-end text (1+ marker) context))
                      (body (subseq text (1+ marker) end)))
                 (if (and (> (length body) 0)
                          (char= (char body 0) #\#))
                     (%parse-character-reference body context)
                     (progn
                       (unless (and (> (length body) 0)
                                    (xml-name-start-char-p (char body 0)))
                         (%entity-error
                          context
                          (xml-message :entity-value-malformed-reference)
                          :entity-reference))
                       (loop for character across (subseq body 1)
                             do (unless (xml-name-char-p character)
                                  (%entity-error
                                   context
                                   (xml-message :entity-value-malformed-reference)
                                   :entity-reference)))))
                 (setf position (1+ end)))))))

(defun %record-expanded-character (context character stream attribute-p
                                   &optional generated-p)
  (when (and attribute-p generated-p (char= character #\<))
    (%entity-error context (xml-message :lt-in-attribute-replacement)
                   :no-lt-in-attribute-values "a character other than <"))
  (when (and attribute-p generated-p (char= character #\&))
    (%entity-error context
                   (xml-message :amp-in-attribute-replacement)
                   :no-amp-in-attribute-values "a character other than &"))
  (let* ((limits (xml-entity-context-limits context))
         (limit (xml-resource-limits-max-expanded-characters limits))
         (next (1+ (xml-entity-context-expanded-characters context))))
    (when (> next limit)
      (%entity-limit-error context :max-expanded-characters next limit))
    (incf (xml-entity-context-expanded-characters context))
    (write-char character stream)))

(defun %begin-entity-expansion (context declaration)
  (let* ((limits (xml-entity-context-limits context))
         (next (1+ (xml-entity-context-expansions context)))
         (expansion-limit (xml-resource-limits-max-entity-expansions limits))
         (depth (1+ (length (xml-entity-context-expansion-stack context))))
         (depth-limit (xml-resource-limits-max-entity-depth limits))
         (name (xml-entity-declaration-name declaration)))
    (when (> next expansion-limit)
      (%entity-limit-error context :max-entity-expansions next expansion-limit))
    (when (> depth depth-limit)
      (%entity-limit-error context :max-entity-depth depth depth-limit))
    (when (member name (xml-entity-context-expansion-stack context)
                  :test #'string=)
      (%entity-error context (xml-message :recursive-entity)
                     :no-recursion name))
    (incf (xml-entity-context-expansions context))
    (push name (xml-entity-context-expansion-stack context))))

(defun %end-entity-expansion (context)
  (pop (xml-entity-context-expansion-stack context)))

(defun %entity-table (context parameter-p)
  (if parameter-p
      (xml-entity-context-parameter-declarations context)
      (xml-entity-context-declarations context)))

(defun %check-entity-name-length (context name)
  (let ((limit (xml-resource-limits-max-name-length
                (xml-entity-context-limits context))))
    (when (> (length name) limit)
      (%entity-limit-error context :max-name-length (length name) limit))))

(defun %declare-entity (context declaration)
  (let* ((table (%entity-table context
                              (xml-entity-declaration-parameter-p declaration)))
         (name (xml-entity-declaration-name declaration)))
    (when (gethash name table)
      (%entity-error context (xml-message :duplicate-entity)
                     :duplicate-entity-declaration name))
    (setf (gethash name table) declaration)
    declaration))

(defun xml-entity-context-declare (context declaration)
  "Register DECLARATION in CONTEXT and return it."
  (check-argument context xml-entity-context-p xml-input-error :entity-context-required)
  (check-argument declaration xml-entity-declaration-p xml-input-error
                  :entity-declaration-required)
  (%declare-entity context declaration))

(defun %external-entity-result (context declaration)
  (if (eq (xml-entity-context-external-entity-policy context) :reject)
      (%external-entity-error context declaration)
      (progn
        (pushnew (xml-entity-declaration-name declaration)
                 (xml-entity-context-skipped-external-entities context)
                 :test #'string=)
        nil)))

(defun %expand-general-references-into (text context stream attribute-p &optional expanding-p)
  (let ((length (length text))
        (position 0))
    (loop while (< position length)
          do (let ((character (char text position)))
               (if (char/= character #\&)
                   (progn
                     (if expanding-p
                         (%record-expanded-character context character stream
                                                      attribute-p t)
                         (write-char character stream))
                     (incf position))
                   (let* ((end (%entity-reference-end text (1+ position) context))
                          (body (subseq text (1+ position) end)))
                     (cond
                       ((and (> (length body) 0)
                             (char= (char body 0) #\#))
                        (%record-expanded-character
                         context (%parse-character-reference body context)
                         stream attribute-p expanding-p))
                       ((assoc body +xml-predefined-entities+ :test #'string=)
                        (%record-expanded-character
                         context (cdr (assoc body +xml-predefined-entities+
                                              :test #'string=))
                         stream attribute-p expanding-p))
                       (t
                          (%check-entity-name-length context body)
                          (let ((declaration
                                (gethash body
                                         (xml-entity-context-declarations context))))
                          (unless declaration
                            (%entity-error context
                                           (xml-message :undeclared-entity body)
                                           :entity-declared body))
                          (if (xml-entity-declaration-external-p declaration)
                              (%external-entity-result context declaration)
                              (progn
                                (%begin-entity-expansion context declaration)
                                (unwind-protect
                                     (%expand-general-references-into
                                      (xml-entity-declaration-replacement-text declaration)
                                      context stream attribute-p t)
                                  (%end-entity-expansion context)))))))
                     (setf position (1+ end))))))))

(defun expand-xml-entity-references (text context &key attribute-p)
  "Expand XML general and character references in TEXT."
  (check-argument text stringp xml-input-error :label-must-be-string
                  :format-arguments ("ENTITY TEXT"))
  (check-argument context xml-entity-context-p xml-input-error :entity-context-required)
  (with-output-to-string (stream)
    (%expand-general-references-into text context stream attribute-p)))

(defun %materialize-entity-source-into (text context stream)
  "Write an entity replacement as source for the XML tokenizer.

Character and predefined references are resolved before tokenization so that
they can produce markup. Internal general entities are expanded recursively;
the parser then validates the resulting replacement as XML content."
  (let ((length (length text))
        (position 0))
    (loop while (< position length)
          do (let ((character (char text position)))
               (if (char/= character #\&)
                   (progn
                     (%record-expanded-character context character stream nil)
                     (incf position))
                   (let* ((end (%entity-reference-end text (1+ position) context))
                          (body (subseq text (1+ position) end))
                          (character
                            (cond
                              ((and (> (length body) 0)
                                    (char= (char body 0) #\#))
                               (%parse-character-reference body context))
                              ((assoc body +xml-predefined-entities+ :test #'string=)
                               (cdr (assoc body +xml-predefined-entities+
                                             :test #'string=)))
                              (t nil))))
                     (if character
                         (progn
                           (when (char= character #\&)
                             (%entity-error
                              context
                              (xml-message :unescaped-ampersand-in-replacement)
                              :entity-reference))
                           (%record-expanded-character context character stream nil))
                         (progn
                           (%check-entity-name-length context body)
                           (let ((declaration
                                   (gethash body
                                            (xml-entity-context-declarations context))))
                             (unless declaration
                               (%entity-error
                                context (xml-message :undeclared-entity body)
                                :entity-declared body))
                             (if (xml-entity-declaration-external-p declaration)
                                 (%external-entity-result context declaration)
                                 (progn
                                   (%begin-entity-expansion context declaration)
                                   (unwind-protect
                                        (%materialize-entity-source-into
                                         (xml-entity-declaration-replacement-text
                                          declaration)
                                         context stream)
                                     (%end-entity-expansion context)))))))
                     (setf position (1+ end))))))))

(defun %quoted-string-end (text position context)
  (let ((quote (char text position)))
    (or (position quote text :start (1+ position))
        (%entity-error context (xml-message :unterminated-entity-declaration)
                       :entity-declaration))))

(defun %word-end (text position)
  (loop while (and (< position (length text))
                   (not (xml-space-char-p (char text position)))
                   (char/= (char text position) #\>))
        do (incf position)
        finally (return position)))

(defun %parse-entity-declaration-at (text start context)
  (let* ((length (length text))
         (position (%skip-xml-space text start))
         (parameter-p nil))
    (when (and (< position length) (char= (char text position) #\%))
      (setf parameter-p t
            position (%skip-xml-space text (1+ position))))
    (unless (and (< position length) (xml-name-start-char-p (char text position)))
      (%entity-error context (xml-message :entity-declaration-needs-name)
                     :entity-declaration))
    (let ((name-start position))
      (loop while (and (< position length)
                       (xml-name-char-p (char text position)))
            do (incf position))
      (let ((name (subseq text name-start position)))
        (%check-entity-name-length context name)
        (let ((separator position))
          (setf position (%skip-xml-space text position))
          (when (= separator position)
            (%entity-error
             context
             (xml-message :entity-name-not-separated)
             :entity-declaration)))
        (when (>= position length)
          (%entity-error context (xml-message :entity-declaration-incomplete)
                         :entity-declaration))
        (cond
          ((member (char text position) '(#\" #\'))
           (let* ((end (%quoted-string-end text position context))
                  (replacement (subseq text (1+ position) end))
                  (after (%skip-xml-space text (1+ end))))
             (%validate-entity-value replacement context)
             (unless (and (< after length) (char= (char text after) #\>))
               (%entity-error context (xml-message :entity-declaration-needs-close)
                              :entity-declaration))
             (values (make-xml-entity-declaration
                      :name name :replacement-text replacement
                      :parameter-p parameter-p)
                     (1+ after))))
          (t
           (let* ((keyword-end (%word-end text position))
                  (keyword (subseq text position keyword-end)))
             (unless (member keyword '("SYSTEM" "PUBLIC") :test #'string=)
               (%entity-error context (xml-message :entity-declaration-needs-value)
                              :entity-declaration))
             (setf position (%skip-xml-space text keyword-end))
             (unless (and (< position length)
                          (member (char text position) '(#\" #\')))
               (%entity-error context (xml-message :external-entity-needs-system-id)
                              :entity-declaration))
             (let* ((first-end (%quoted-string-end text position context))
                    (first-id (subseq text (1+ position) first-end))
                    (after (%skip-xml-space text (1+ first-end)))
                    (public-id (and (string= keyword "PUBLIC") first-id))
                    (system-id nil))
               (if (string= keyword "PUBLIC")
                   (progn
                     (unless (and (< after length)
                                  (member (char text after) '(#\" #\')))
                       (%entity-error context (xml-message :public-needs-system-id)
                                      :entity-declaration))
                     (let ((system-end (%quoted-string-end text after context)))
                       (setf system-id (subseq text (1+ after) system-end)
                             after (%skip-xml-space text (1+ system-end)))))
                   (setf system-id first-id))
               (unless (and (< after length) (char= (char text after) #\>))
                 (let ((closing (position #\> text :start after)))
                   (unless closing
                     (%entity-error context
                                    (xml-message :external-entity-declaration-incomplete)
                                    :entity-declaration))
                   (setf after closing)))
               (values (make-xml-entity-declaration
                        :name name :parameter-p parameter-p :external-p t
                        :public-id public-id :system-id system-id)
                       (1+ after))))))))))

(defun %skip-comment (text start context)
  (let ((end (search "-->" text :start2 start)))
    (if end
        (+ end 3)
        (%entity-error context (xml-message :unterminated-subset-comment)
                       :comment))))

(defun %skip-markup-declaration (text start context)
  (let ((quote nil)
        (position start))
    (loop while (< position (length text))
          for character = (char text position)
          do (cond
               (quote
                (when (char= character quote) (setf quote nil)))
               ((member character '(#\" #\')) (setf quote character))
               ((char= character #\>) (return (1+ position))))
             (incf position)
          finally (%entity-error context (xml-message :unterminated-markup-declaration)
                                 :markup-declaration))))

;; ATTLIST declarations are otherwise skipped; the names are kept for tools that
;; must not edit attributes governed by a declaration.
(defun %note-attlist-declaration (text start context)
  (let ((keyword-end (+ start 9)))
    (when (and (< keyword-end (length text))
               (string= "<!ATTLIST" text :start2 start :end2 keyword-end)
               (xml-space-char-p (char text keyword-end)))
      (let* ((name-start (%skip-xml-space text keyword-end))
             (name-end name-start))
        (when (and (< name-start (length text))
                   (xml-name-start-char-p (char text name-start)))
          (loop while (and (< name-end (length text))
                           (xml-name-char-p (char text name-end)))
                do (incf name-end))
          (pushnew (subseq text name-start name-end)
                   (xml-entity-context-attlist-element-names context)
                   :test #'string=))))))

(defun parse-xml-internal-subset (subset &key
                                         (resource-limits *default-xml-resource-limits*)
                                         (external-entity-policy :reject)
                                         source position)
  "Parse internal ENTITY declarations from SUBSET and return an entity context."
  (check-argument subset stringp xml-input-error :label-must-be-string
                  :format-arguments ("INTERNAL-SUBSET"))
  (let ((context (make-xml-entity-context
                  :resource-limits resource-limits
                  :external-entity-policy external-entity-policy
                  :source source :position position)))
    (labels ((process (text)
               (let ((length (length text))
                     (index 0))
                 (loop while (< index length)
                       do (cond
                            ((xml-space-char-p (char text index))
                             (incf index))
                            ((and (<= (+ index 4) length)
                                  (string= "<!--" text :start1 0 :end1 4
                                           :start2 index :end2 (+ index 4)))
                             (setf index (%skip-comment text (+ index 4) context)))
                            ((and (<= (+ index 8) length)
                                  (string= "<!ENTITY" text :start1 0 :end1 8
                                           :start2 index :end2 (+ index 8))
                                  (or (= (+ index 8) length)
                                      (xml-space-char-p (char text (+ index 8)))
                                      (char= (char text (+ index 8)) #\%)))
                             (multiple-value-bind (declaration next)
                                 (%parse-entity-declaration-at text (+ index 8) context)
                               (%declare-entity context declaration)
                               (setf index next)))
                            ((char= (char text index) #\%)
                             (let* ((end (%entity-reference-end text (1+ index) context))
                                    (name (subseq text (1+ index) end))
                                    (declaration
                                      (gethash name
                                               (xml-entity-context-parameter-declarations
                                                context))))
                               (%check-entity-name-length context name)
                               (unless declaration
                                 (%entity-error
                                  context
                                  (xml-message :undeclared-parameter-entity name)
                                  :entity-declared name))
                               (if (xml-entity-declaration-external-p declaration)
                                   (%external-entity-result context declaration)
                                   (progn
                                     (%begin-entity-expansion context declaration)
                                     (unwind-protect
                                          (process
                                           (xml-entity-declaration-replacement-text
                                            declaration))
                                       (%end-entity-expansion context))))
                               (setf index (1+ end))))
                            ((and (<= (+ index 3) length)
                                  (string= "<![" text :start2 index :end2 (+ index 3)))
                             (%entity-error
                              context
                              (xml-message :conditional-section-unsupported)
                              :markup-declaration))
                            ((and (< (+ index 1) length)
                                  (char= (char text index) #\<)
                                  (char= (char text (1+ index)) #\!))
                             (%note-attlist-declaration text index context)
                             (setf index (%skip-markup-declaration text index context)))
                            (t
                             (incf index)))))))
      (process subset))
    context))

(defun %xml-parse-internal-subset (subset &rest arguments)
  (apply #'parse-xml-internal-subset subset arguments))

(defun %xml-expand-entity-references (text context &rest arguments)
  (apply #'expand-xml-entity-references text context arguments))

