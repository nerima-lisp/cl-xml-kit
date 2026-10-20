;;;; src/entity-subset.lisp

(in-package #:cl-xml-kit)

(defun %quoted-string-end (text position context)
  (let ((quote (char text position)))
    (or (position quote text :start (1+ position))
        (%entity-error context (xml-message :unterminated-entity-declaration)
                       :entity-declaration))))

(defun %quote-at-p (text position)
  (and (< position (length text))
       (member (char text position) '(#\" #\'))))

(defun %close-at-p (text position)
  (and (< position (length text))
       (char= (char text position) #\>)))

(defun %text-starts-with-p (text index prefix)
  (let ((end (+ index (length prefix))))
    (and (<= end (length text))
         (string= prefix text :start2 index :end2 end))))

(defun %word-end (text position)
  (loop while (and (< position (length text))
                   (not (xml-space-char-p (char text position)))
                   (char/= (char text position) #\>))
        do (incf position)
        finally (return position)))

(defun %parse-entity-declaration-head (text start context)
  "Read the optional % and the entity name after <!ENTITY at START.

Return the name, whether it names a parameter entity, and the position of the
entity definition that follows the separating space."
  (let ((position (%skip-xml-space text start))
        (parameter-p nil))
    (when (and (< position (length text)) (char= (char text position) #\%))
      (setf parameter-p t
            position (%skip-xml-space text (1+ position))))
    (unless (and (< position (length text))
                 (xml-name-start-char-p (char text position)))
      (%entity-error context (xml-message :entity-declaration-needs-name)
                     :entity-declaration))
    (let* ((name-end (%name-end text position))
           (name (subseq text position name-end))
           (definition (%skip-xml-space text name-end)))
      (%check-entity-name-length context name)
      (when (= definition name-end)
        (%entity-error context (xml-message :entity-name-not-separated)
                       :entity-declaration))
      (when (>= definition (length text))
        (%entity-error context (xml-message :entity-declaration-incomplete)
                       :entity-declaration))
      (values name parameter-p definition))))

(defun %parse-internal-entity-definition (text position context name parameter-p)
  (let* ((end (%quoted-string-end text position context))
         (replacement (subseq text (1+ position) end))
         (after (%skip-xml-space text (1+ end))))
    (%validate-entity-value replacement context)
    (unless (%close-at-p text after)
      (%entity-error context (xml-message :entity-declaration-needs-close)
                     :entity-declaration))
    (values (make-xml-entity-declaration
             :name name :replacement-text replacement :parameter-p parameter-p)
            (1+ after))))

(defun %parse-external-id (text position context)
  "Read a SYSTEM or PUBLIC ExternalID at POSITION.

Return the public ID or NIL, the system ID, and the position after the IDs and
any following space."
  (let* ((keyword-end (%word-end text position))
         (keyword (subseq text position keyword-end))
         (public-p (string= keyword "PUBLIC")))
    (unless (or public-p (string= keyword "SYSTEM"))
      (%entity-error context (xml-message :entity-declaration-needs-value)
                     :entity-declaration))
    (setf position (%skip-xml-space text keyword-end))
    (unless (%quote-at-p text position)
      (%entity-error context (xml-message :external-entity-needs-system-id)
                     :entity-declaration))
    (let* ((first-end (%quoted-string-end text position context))
           (first-id (subseq text (1+ position) first-end))
           (after (%skip-xml-space text (1+ first-end))))
      (if (not public-p)
          (values nil first-id after)
          (progn
            (unless (%quote-at-p text after)
              (%entity-error context (xml-message :public-needs-system-id)
                             :entity-declaration))
            (let ((system-end (%quoted-string-end text after context)))
              (values first-id
                      (subseq text (1+ after) system-end)
                      (%skip-xml-space text (1+ system-end)))))))))

(defun %parse-external-entity-definition (text position context name parameter-p)
  (multiple-value-bind (public-id system-id after)
      (%parse-external-id text position context)
    (unless (%close-at-p text after)
      (setf after (or (position #\> text :start after)
                      (%entity-error
                       context (xml-message :external-entity-declaration-incomplete)
                       :entity-declaration))))
    (values (make-xml-entity-declaration
             :name name :parameter-p parameter-p :external-p t
             :public-id public-id :system-id system-id)
            (1+ after))))

(defun %parse-entity-declaration-at (text start context)
  "Parse the entity declaration whose name part begins at START.

Return the declaration and the position after its closing >."
  (multiple-value-bind (name parameter-p position)
      (%parse-entity-declaration-head text start context)
    (if (%quote-at-p text position)
        (%parse-internal-entity-definition text position context name parameter-p)
        (%parse-external-entity-definition text position context name parameter-p))))

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
    (when (and (%text-starts-with-p text start "<!ATTLIST")
               (< keyword-end (length text))
               (xml-space-char-p (char text keyword-end)))
      (let ((name-start (%skip-xml-space text keyword-end)))
        (when (and (< name-start (length text))
                   (xml-name-start-char-p (char text name-start)))
          (pushnew (subseq text name-start (%name-end text name-start))
                   (xml-entity-context-attlist-element-names context)
                   :test #'string=))))))

(defun %entity-declaration-start-p (text index)
  (and (%text-starts-with-p text index "<!ENTITY")
       (let ((after (+ index 8)))
         (or (= after (length text))
             (xml-space-char-p (char text after))
             (char= (char text after) #\%)))))

(defun %process-parameter-entity-reference (text index context)
  "Expand the parameter entity reference at INDEX and return the index after it."
  (let* ((end (%entity-reference-end text (1+ index) context))
         (declaration (%declared-entity context (subseq text (1+ index) end) t)))
    (if (xml-entity-declaration-external-p declaration)
        (%external-entity-result context declaration)
        (progn
          (%begin-entity-expansion context declaration)
          (unwind-protect
               (%process-internal-subset
                (xml-entity-declaration-replacement-text declaration) context)
            (%end-entity-expansion context))))
    (1+ end)))

(defun %process-internal-subset-item (text index context)
  "Process the internal subset item at INDEX in TEXT and return the next index."
  (cond
    ((xml-space-char-p (char text index))
     (1+ index))
    ((%text-starts-with-p text index "<!--")
     (%skip-comment text (+ index 4) context))
    ((%entity-declaration-start-p text index)
     (multiple-value-bind (declaration next)
         (%parse-entity-declaration-at text (+ index 8) context)
       (%declare-entity context declaration)
       next))
    ((char= (char text index) #\%)
     (%process-parameter-entity-reference text index context))
    ((%text-starts-with-p text index "<![")
     (%entity-error context (xml-message :conditional-section-unsupported)
                    :markup-declaration))
    ((%text-starts-with-p text index "<!")
     (%note-attlist-declaration text index context)
     (%skip-markup-declaration text index context))
    (t
     (1+ index))))

(defun %process-internal-subset (text context)
  (let ((index 0))
    (loop while (< index (length text))
          do (setf index (%process-internal-subset-item text index context)))))

(defun parse-xml-internal-subset (subset &key
                                         (resource-limits *default-xml-resource-limits*
                                                          resource-limits-supplied-p)
                                         (external-entity-policy :reject
                                                                 external-entity-policy-supplied-p)
                                         source position)
  "Parse internal ENTITY declarations from SUBSET and return an entity context."
  (declare (ignorable resource-limits-supplied-p
                      external-entity-policy-supplied-p))
  (check-argument subset stringp xml-input-error :label-must-be-string
                  :format-arguments ("INTERNAL-SUBSET"))
  (let ((context (make-xml-entity-context
                  :resource-limits resource-limits
                  :external-entity-policy external-entity-policy
                  :source source :position position)))
    (%process-internal-subset subset context)
    context))
