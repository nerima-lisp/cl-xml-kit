;;;; src/entity-expansion.lisp

(in-package #:cl-xml-kit)

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

(defun %xml-expand-entity-references (text context &rest arguments)
  (apply #'expand-xml-entity-references text context arguments))
