(in-package #:cl-xml-kit)
(progn
  (defvar *xml-tokenizer-active* nil)
  (defvar *xml-tokenizer-actual-token-kind* nil)

  (defparameter *xml-tokenizer-parser-tokenizer*
    (labels ((starts-with-p (literal text index)
               (let ((end (+ index (length literal))))
                 (and (<= end (length text))
                      (string= literal text :start2 index :end2 end))))
             (tag-open-rule (end-tag-p)
               (let ((literal (if end-tag-p "</" "<"))
                     (kind (if end-tag-p :end-tag-open :start-tag-open))
                     (mode (if end-tag-p :end-tag :start-tag)))
                 (cl-parser-kit:make-token-rule
                  :type :xml-tag-token
                  :action (list :push mode)
                  :matcher
                  (lambda (text index)
                    (when (and (char= (char text index) #\<)
                               (if end-tag-p
                                   (starts-with-p "</" text index)
                                   (not (starts-with-p "</" text index)))
                               (not (%xml-tokenizer-special-markup-kind text index)))
                      (let* ((tokenizer *xml-tokenizer-active*)
                             (state (%xml-tokenizer-state tokenizer))
                             (position (%xml-tokenizer-state-index state)))
                        (%xml-tokenizer-consume-literal tokenizer literal)
                        (setf (%xml-tokenizer-state-tag-open-p state) t)
                        (multiple-value-bind (actual-kind value start end)
                            (%xml-tokenizer-token tokenizer kind nil position)
                          (declare (ignore start))
                          (setf *xml-tokenizer-actual-token-kind* actual-kind)
                          (values t (- end index) nil value))))))))
             (content-rule ()
               (cl-parser-kit:make-token-rule
                :type :xml-content-token
                :matcher
                (lambda (text index)
                  (let* ((tokenizer *xml-tokenizer-active*)
                         (character (char text index))
                         (kind (cond
                                 ((char= character #\&) :entity-reference)
                                 ((char= character #\<)
                                  (%xml-tokenizer-special-markup-kind text index))
                                 (t :text))))
                    (multiple-value-bind (actual-kind value start end)
                      (ecase kind
                        (:text (%xml-tokenizer-scan-text tokenizer))
                        (:entity-reference
                         (multiple-value-bind (value position)
                             (%xml-tokenizer-scan-entity tokenizer)
                           (%xml-tokenizer-token tokenizer :entity-reference
                                                 value position)))
                        (:comment (%xml-tokenizer-scan-comment tokenizer))
                        (:cdata (%xml-tokenizer-scan-cdata tokenizer))
                        (:doctype (%xml-tokenizer-scan-doctype tokenizer))
                        (:processing-instruction
                         (%xml-tokenizer-scan-processing-instruction tokenizer)))
                      (declare (ignore start))
                      (setf *xml-tokenizer-actual-token-kind* actual-kind)
                      (values t (- end index) nil value))))))
             (space-rule (after-value-p)
               (cl-parser-kit:make-token-rule
                :type :whitespace
                :skip-p t
                :matcher
                (lambda (text index)
                  (when (xml-space-char-p (char text index))
                    (let* ((tokenizer *xml-tokenizer-active*)
                           (state (%xml-tokenizer-state tokenizer)))
                      (%xml-tokenizer-skip-space tokenizer)
                      (when after-value-p
                        (setf (%xml-tokenizer-state-space-seen-p state) t))
                      (values t (- (%xml-tokenizer-state-index state) index)
                              nil nil))))))
             (tag-scanner-rule (scanner action)
               (cl-parser-kit:make-token-rule
                :type :xml-tag-token
                :action action
                :matcher
                (lambda (text index)
                  (declare (ignore text))
                  (let ((tokenizer *xml-tokenizer-active*))
                    (multiple-value-bind (actual-kind value start end)
                        (funcall scanner tokenizer)
                      (declare (ignore start))
                      (setf *xml-tokenizer-actual-token-kind* actual-kind)
                      (values t (- end index) nil value))))))
             (start-close-rule ()
               (cl-parser-kit:make-token-rule
                :type :xml-tag-token
                :action :pop
                :matcher
                (lambda (text index)
                  (when (or (starts-with-p "/>" text index)
                            (char= (char text index) #\>))
                    (let* ((tokenizer *xml-tokenizer-active*)
                           (literal (if (starts-with-p "/>" text index) "/>" ">"))
                           (kind (if (string= literal "/>")
                                     :empty-element-close :tag-close)))
                      (multiple-value-bind (actual-kind value start end)
                          (%xml-tokenizer-start-tag-close tokenizer kind literal)
                        (declare (ignore start))
                        (setf *xml-tokenizer-actual-token-kind* actual-kind)
                        (values t (- end index) nil value)))))))
             (end-close-rule ()
               (cl-parser-kit:make-token-rule
                :type :xml-tag-token
                :action :pop
                :matcher
                (lambda (text index)
                  (unless (xml-space-char-p (char text index))
                    (let ((tokenizer *xml-tokenizer-active*))
                      (multiple-value-bind (actual-kind value start end)
                          (%xml-tokenizer-next-end-tag-close tokenizer)
                        (declare (ignore start))
                        (setf *xml-tokenizer-actual-token-kind* actual-kind)
                        (values t (- end index) nil value)))))))
             (mode-rules ()
               (list
                (cons :content
                      (list (tag-open-rule nil) (tag-open-rule t) (content-rule)))
                (cons :start-tag
                      (list (tag-scanner-rule #'%xml-tokenizer-name-token
                                              '(:switch :start-after-element-name))))
                (cons :end-tag
                      (list (tag-scanner-rule #'%xml-tokenizer-name-token
                                              '(:switch :end-after-name))))
                (cons :start-after-element-name
                      (list (start-close-rule)
                            (space-rule nil)
                            (tag-scanner-rule
                             (lambda (tokenizer)
                               (%xml-tokenizer-next-attribute-name tokenizer nil))
                             '(:switch :start-after-attribute-name))))
                (cons :start-after-attribute-name
                      (list (space-rule nil)
                            (tag-scanner-rule #'%xml-tokenizer-next-equals
                                              '(:switch :start-after-equals))))
                (cons :start-after-equals
                      (list (space-rule nil)
                            (tag-scanner-rule #'%xml-tokenizer-next-quoted-value
                                              '(:switch :start-after-quote))))
                (cons :start-after-quote
                      (list (start-close-rule)
                            (space-rule t)
                            (tag-scanner-rule
                             (lambda (tokenizer)
                               (%xml-tokenizer-next-attribute-name tokenizer t))
                             '(:switch :start-after-attribute-name))))
                (cons :end-after-name
                      (list (end-close-rule) (space-rule nil))))))
      (cl-parser-kit:make-tokenizer
       :modes (mapcar (lambda (entry)
                        (cons (car entry) (coerce (cdr entry) 'vector)))
                      (mode-rules))
       :initial-mode :content
       :unknown-token-type :unknown)))

  (defun %xml-tokenizer-create-cursor (tokenizer)
    (let* ((input (%xml-tokenizer-input tokenizer))
           (limits (%xml-tokenizer-limits tokenizer))
           (source (%xml-tokenizer-source tokenizer)))
      (cl-parser-kit:make-tokenizer-cursor
       input *xml-tokenizer-parser-tokenizer*
       :max-source-length
       (xml-resource-limits-max-document-characters limits)
       :max-tokens (length input)
       :max-rules
       (loop for mode in (cl-parser-kit:tokenizer-modes *xml-tokenizer-parser-tokenizer*)
             sum (length (cdr mode)))
       :max-mode-depth 2
       :on-limit-exceeded
       (lambda (kind limit value offset line column)
         (ecase kind
           (:source-length
            (%xml-tokenizer-limit-fail
             tokenizer :max-document-characters value limit
             (make-xml-position :source source :offset offset :line line
                                :column column)))))
       :token-constructor
       (lambda (kind text value start end line column)
         (declare (ignore text))
         (make-xml-token
          :kind (if (or (eq kind :xml-content-token)
                        (eq kind :xml-tag-token))
                    *xml-tokenizer-actual-token-kind*
                    kind)
          :value value
          :position (make-xml-position :source source :offset start
                                       :line line :column column)
          :end end))))))

(defun next-xml-token (tokenizer)
  "Return the next lexical token. After the one-shot :eof token, return NIL
on later calls.

Token kind, value, and position follow `+XML-TOKEN-KINDS+`.

Tokenization errors signal XML-WELL-FORMEDNESS-ERROR at the offending
source position. The parser verifies that end-tag names match their start
tags.

Exceeding :max-document-characters, :max-name-length, or
:max-text-node-length signals XML-RESOURCE-LIMIT-EXCEEDED."
  (check-argument tokenizer xml-tokenizer-p xml-input-error :tokenizer-required)
  (let ((state (%xml-tokenizer-state tokenizer)))
    (when (%xml-tokenizer-state-eof-returned state)
      (return-from next-xml-token nil))
    (or (let ((*xml-tokenizer-active* tokenizer)
              (*xml-tokenizer-actual-token-kind* nil))
          (cl-parser-kit:tokenizer-cursor-next
           (%xml-tokenizer-cursor tokenizer)))
        (progn
          (when (%xml-tokenizer-state-tag-open-p state)
            (%xml-tokenizer-fail tokenizer
                                 (xml-message :unexpected-end-of-input)
                                 :constraint :well-formedness))
          (setf (%xml-tokenizer-state-eof-returned state) t)
          (make-xml-token :kind :eof
                          :position (%xml-tokenizer-current-position tokenizer)
                          :end (%xml-tokenizer-state-index state))))))
