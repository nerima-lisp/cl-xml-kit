(in-package #:cl-xml-kit)

(defun %doctype-body-layout (body)
  "Return the root-name and internal-subset bounds of a DOCTYPE token BODY.

The values are the root-name start and end, then the internal-subset start and
end, which are NIL when BODY has no internal subset. The tokenizer has checked
that BODY holds a root Name after its leading space."
  (let ((length (length body))
        (index 0))
    (loop while (xml-space-char-p (char body index))
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

(defun %parser-check-external-dtd (parser token body root-name index)
  (when (and (eq (xml-parser-external-entity-policy parser) :reject)
             (or (search "SYSTEM" body :start2 index :test #'char-equal)
                 (search "PUBLIC" body :start2 index :test #'char-equal)))
    (xml-error 'xml-external-resource-disabled :external-dtd-disabled
               :condition-initargs (:identifier root-name :kind :external-dtd
                                    :source (xml-parser-source-name parser)
                                    :position (xml-token-position token)))))

(defun %parser-doctype (parser token)
  (let ((body (xml-token-value token)))
    (multiple-value-bind (start index subset-start subset-end)
        (%doctype-body-layout body)
      (let ((root-name (subseq body start index)))
        (%parser-check-external-dtd parser token body root-name index)
        (when (xml-parser-doctype-seen parser)
          (%parser-token-error parser token (xml-message :single-doctype)
                               :doctype :single-doctype))
        (setf (xml-parser-doctype-seen parser) t
              (xml-parser-doctype-root parser) root-name)
        (when subset-start
          (setf (xml-parser-entities parser)
                (parse-xml-internal-subset
                 (subseq body subset-start subset-end)
                 :resource-limits (xml-parser-limits parser)
                 :external-entity-policy (xml-parser-external-entity-policy parser)
                 :source (xml-parser-source-name parser)
                 :position (xml-token-position token))))
        (%string-value root-name "ROOT-NAME" :nonempty t :no-colon t)
        (%parser-event parser :doctype :text body
                                       :position (xml-token-position token))))))

(defun %parser-xml-declaration-token-p (token)
  (let ((value (xml-token-value token)))
    (and (>= (length value) 3)
         (string= "xml" value :end2 3)
         (or (= (length value) 3)
             (xml-space-char-p (char value 3))))))

(defun %declaration-skip-space (body index)
  (or (position-if-not #'xml-space-char-p body :start index) (length body)))

(defmacro %declaration-fail (parser token message-id expected)
  `(%parser-token-error ,parser ,token (xml-message ,message-id) :xml-declaration ,expected))

(defun %declaration-quoted-value (parser token body index)
  "Return the quoted value at INDEX of the declaration BODY and the index after it."
  (let ((quote (and (< index (length body)) (char body index))))
    (unless (member quote '(#\" #\'))
      (%declaration-fail parser token :expected-declaration-value :quoted-value))
    (let ((end (position quote body :start (1+ index))))
      (unless end
        (%declaration-fail parser token :unterminated-declaration-value quote))
      (values (subseq body (1+ index) end) (1+ end)))))

(defun %declaration-keyword-p (body index keyword)
  (let ((end (+ index (length keyword))))
    (and (<= end (length body)) (string= keyword body :start2 index :end2 end))))

(defun %declaration-pseudo-attribute (parser token body index keyword)
  "Read KEYWORD = quoted-value at INDEX of the declaration BODY.

Return the value and the index after its closing quote."
  (unless (%declaration-keyword-p body index keyword)
    (%declaration-fail parser token :expected-declaration-keyword keyword))
  (let ((equals (%declaration-skip-space body (+ index (length keyword)))))
    (unless (and (< equals (length body)) (char= (char body equals) #\=))
      (%declaration-fail parser token :expected-declaration-equals :equals))
    (%declaration-quoted-value parser token body
                               (%declaration-skip-space body (1+ equals)))))

(defun %declaration-separator (parser token body index)
  "Skip the whitespace at INDEX, which must be present unless BODY ends there."
  (let ((next (%declaration-skip-space body index)))
    (when (and (= next index) (< index (length body)))
      (%declaration-fail parser token :declaration-needs-whitespace :xml-whitespace))
    next))

(defun %declaration-standalone (parser token body index)
  (multiple-value-bind (value next)
      (%declaration-pseudo-attribute parser token body index "standalone")
    (unless (member value '("yes" "no") :test #'string=)
      (%declaration-fail parser token :standalone-invalid '("yes" "no")))
    next))

(defun %parser-parse-xml-declaration (parser token)
  (let* ((body (xml-token-value token))
         (length (length body))
         (index (%declaration-skip-space body 3)))
    (when (= index 3)
      (%declaration-fail parser token :declaration-version-required :version))
    (multiple-value-bind (version next)
        (%declaration-pseudo-attribute parser token body index "version")
      (unless (string= "1.0" version)
        (%declaration-fail parser token :unsupported-version "1.0"))
      (setf index (%declaration-separator parser token body next)))
    (when (< index length)
      (cond
        ((%declaration-keyword-p body index "encoding")
         (setf index (%declaration-separator
                      parser token body
                      (nth-value 1 (%declaration-pseudo-attribute parser token body
                                                                  index "encoding"))))
         (when (< index length)
           (setf index (%declaration-standalone parser token body index))))
        ((%declaration-keyword-p body index "standalone")
         (setf index (%declaration-standalone parser token body index)))
        (t
         (%declaration-fail parser token :expected-declaration-keyword
                            '(:encoding :standalone))))
      (setf index (%declaration-separator parser token body index)))
    (when (< index length)
      (%declaration-fail parser token :unexpected-declaration-content :end))))

(defun %parser-top-level-pi (parser token)
  (cond
    ((not (%parser-xml-declaration-token-p token)) (%parser-pi parser token))
    ((zerop (xml-position-offset (xml-token-position token)))
     (%parser-parse-xml-declaration parser token)
     nil)
    (t (%parser-token-error parser token (xml-message :declaration-not-first)
                            :xml-declaration :document-start))))

(defun %parser-root-element (parser token)
  (when (xml-parser-root-seen parser)
    (%parser-token-error parser token (xml-message :single-root)
                         :well-formedness :single-root))
  (setf (xml-parser-root-seen parser) t)
  (let ((name (%parser-next-token parser))
        (root (xml-parser-doctype-root parser)))
    (when (and root (not (string= (xml-token-value name) root)))
      (%parser-token-error parser name (xml-message :root-doctype-mismatch)
                           :doctype-root-match root))
    (%parser-start-element parser name)))

(defun %parser-end-document (parser token)
  (unless (xml-parser-root-seen parser)
    (%parser-token-error parser token (xml-message :root-required)
                         :well-formedness :root-element))
  (setf (xml-parser-phase parser) :done)
  (%parser-event parser :end-document :position (xml-token-position token)))

(defun %parser-top-level-text (parser token)
  (let ((text (xml-token-value token)))
    (unless (every #'xml-space-char-p text)
      (%parser-token-error parser token (xml-message :text-outside-root)
                           :well-formedness :root-element))
    (when (xml-parser-root-seen parser)
      (%parser-characters parser text (xml-token-position token)))))

(defun %parser-top-level-reference (parser token)
  (unless (and (xml-parser-root-seen parser)
               (not (xml-parser-root-closed parser)))
    (%parser-token-error parser token (xml-message :entity-outside-root)
                         :well-formedness :root-element))
  (let* ((entities (xml-parser-entities parser))
         (declaration (gethash (xml-token-value token)
                               (xml-entity-context-declarations entities))))
    (if (and declaration (xml-entity-declaration-external-p declaration))
        (%parser-skipped-entity parser declaration token)
        (%parser-characters parser
                            (expand-xml-entity-references
                             (format nil "&~A;" (xml-token-value token))
                             entities)
                            (xml-token-position token)))))

(defun %parser-top-level-token (parser token)
  "Interpret TOKEN outside the root element; return its event, or NIL when it has none."
  (case (xml-token-kind token)
    (:text (%parser-top-level-text parser token))
    (:entity-reference (%parser-top-level-reference parser token))
    (:comment
     (%parser-event parser :comment :text (xml-token-value token)
                                    :position (xml-token-position token)))
    (:processing-instruction (%parser-top-level-pi parser token))
    (:cdata
     (%parser-token-error parser token (xml-message :cdata-outside-root)
                          :well-formedness :root-element))
    (:doctype
     (when (xml-parser-root-seen parser)
       (%parser-token-error parser token (xml-message :doctype-after-root)
                            :doctype :document-prolog))
     (%parser-doctype parser token))
    (:start-tag-open (%parser-root-element parser token))
    (:end-tag-open
     (%parser-token-error parser token (xml-message :end-tag-without-element)
                          :element-type-match :start-tag))
    (:eof (%parser-end-document parser token))))
