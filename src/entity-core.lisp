;;;; src/entity-core.lisp

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

(defun %name-end (text position)
  (loop while (and (< position (length text))
                   (xml-name-char-p (char text position)))
        do (incf position)
        finally (return position)))

(defun %xml-name-string-p (text)
  (and (plusp (length text))
       (xml-name-start-char-p (char text 0))
       (= (%name-end text 1) (length text))))

(defun %entity-reference-end (text start context)
  (or (position #\; text :start start)
      (%entity-error context (xml-message :entity-reference-needs-semicolon)
                     :entity-reference ";")))

(defun %parse-character-reference (body context)
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
  (loop with position = 0
        for marker = (position #\& text :start position)
        while marker
        do (let* ((end (%entity-reference-end text (1+ marker) context))
                  (body (subseq text (1+ marker) end)))
             (cond
               ((and (plusp (length body)) (char= (char body 0) #\#))
                (%parse-character-reference body context))
               ((not (%xml-name-string-p body))
                (%entity-error context (xml-message :entity-value-malformed-reference)
                               :entity-reference)))
             (setf position (1+ end)))))

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

(defun %declared-entity (context name parameter-p)
  "Return the declaration of entity NAME, signalling when it is undeclared."
  (%check-entity-name-length context name)
  (or (gethash name (%entity-table context parameter-p))
      (%entity-error context
                     (if parameter-p
                         (xml-message :undeclared-parameter-entity name)
                         (xml-message :undeclared-entity name))
                     :entity-declared name)))

(defun %reference-character (body context)
  "Return the character that reference BODY denotes, or NIL for a named entity."
  (if (and (plusp (length body)) (char= (char body 0) #\#))
      (%parse-character-reference body context)
      (cdr (assoc body +xml-predefined-entities+ :test #'string=))))

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
