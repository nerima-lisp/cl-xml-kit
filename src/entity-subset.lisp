;;;; src/entity-subset.lisp

(in-package #:cl-xml-kit)

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
