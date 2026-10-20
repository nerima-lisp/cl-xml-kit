(in-package #:cl-xml-kit)
(defvar *xml-tokenizer-active* nil)
(defvar *xml-tokenizer-actual-token-kind* nil)
(defvar *xml-tokenizer-token-position* nil)

(defvar *xml-tokenizer-parser-tokenizer* nil)

(defun %xml-tokenizer-create-parser-tokenizer ()
  (labels ((starts-with-p (literal text index)
             (let ((end (+ index (length literal))))
               (and (<= end (length text))
                    (string= literal text :start2 index :end2 end))))
           (tag-open-rule ()
             (cl-parser-kit:make-token-rule
              :type :xml-tag-token
              :action (list :push :tag)
              :matcher
              (lambda (text index)
                (when (and (char= (char text index) #\<)
                           (not (%xml-tokenizer-special-markup-kind text index)))
                  (let* ((end-tag-p (starts-with-p "</" text index))
                         (literal (if end-tag-p "</" "<"))
                         (kind (if end-tag-p :end-tag-open :start-tag-open))
                         (tokenizer *xml-tokenizer-active*)
                         (state (%xml-tokenizer-state tokenizer))
                         (position (%xml-tokenizer-state-index state)))
                    (%xml-tokenizer-consume-literal tokenizer literal)
                    (setf (%xml-tokenizer-state-tag-open-p state) t
                          (%xml-tokenizer-state-tag-phase state)
                          (if end-tag-p :end-name :start-name))
                    (multiple-value-bind (actual-kind value start end)
                        (%xml-tokenizer-token tokenizer kind nil position)
                      (declare (ignore start))
                      (setf *xml-tokenizer-actual-token-kind* actual-kind)
                      (values t (- end index) nil value)))))))
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
           (tag-token-rule ()
             (cl-parser-kit:make-token-rule
              :type :xml-tag-token
              :matcher
              (lambda (text index)
                (declare (ignore text))
                (let* ((tokenizer *xml-tokenizer-active*)
                       (state (%xml-tokenizer-state tokenizer))
                       (phase (%xml-tokenizer-state-tag-phase state))
                       (before (%xml-tokenizer-state-index state)))
                  (unless (member phase (quote (:start-name :end-name)))
                    (%xml-tokenizer-skip-space tokenizer)
                    (when (and (eq phase :after-quote)
                               (/= before (%xml-tokenizer-state-index state)))
                      (setf (%xml-tokenizer-state-space-seen-p state) t))
                    (when (null (%xml-tokenizer-peek tokenizer))
                      (ecase phase
                        (:after-element-name
                         (%xml-tokenizer-next-attribute-name tokenizer nil))
                        (:after-attribute-name
                         (%xml-tokenizer-next-equals tokenizer))
                        (:after-equals
                         (%xml-tokenizer-next-quoted-value tokenizer))
                        (:after-quote
                         (%xml-tokenizer-next-attribute-name tokenizer t))
                        (:end-after-name
                         (%xml-tokenizer-next-end-tag-close tokenizer)))))
                  (setf *xml-tokenizer-token-position*
                        (%xml-tokenizer-current-position tokenizer))
                  (multiple-value-bind (next-phase actual-kind value start end)
                      (ecase phase
                        (:start-name
                         (multiple-value-bind (kind value start end)
                             (%xml-tokenizer-name-token tokenizer)
                           (values :after-element-name kind value start end)))
                        (:end-name
                         (multiple-value-bind (kind value start end)
                             (%xml-tokenizer-name-token tokenizer)
                           (values :end-after-name kind value start end)))
                        (:after-element-name
                         (multiple-value-bind (kind value start end)
                             (%xml-tokenizer-next-attribute-name tokenizer nil)
                           (values :after-attribute-name kind value start end)))
                        (:after-attribute-name
                         (multiple-value-bind (kind value start end)
                             (%xml-tokenizer-next-equals tokenizer)
                           (values :after-equals kind value start end)))
                        (:after-equals
                         (multiple-value-bind (kind value start end)
                             (%xml-tokenizer-next-quoted-value tokenizer)
                           (values :after-quote kind value start end)))
                        (:after-quote
                         (multiple-value-bind (kind value start end)
                             (%xml-tokenizer-next-attribute-name tokenizer t)
                           (values :after-attribute-name kind value start end)))
                        (:end-after-name
                         (multiple-value-bind (kind value start end)
                             (%xml-tokenizer-next-end-tag-close tokenizer)
                           (values :end-after-name kind value start end))))
                    (declare (ignore start))
                    (setf (%xml-tokenizer-state-tag-phase state) next-phase
                          *xml-tokenizer-actual-token-kind* actual-kind)
                    (values t (- end index) nil value))))))
           (tag-close-rule ()
             (cl-parser-kit:make-token-rule
              :type :xml-tag-token
              :action :pop
              :matcher
              (lambda (text index)
                (let* ((tokenizer *xml-tokenizer-active*)
                       (state (%xml-tokenizer-state tokenizer))
                       (phase (%xml-tokenizer-state-tag-phase state))
                       (close-index
                         (loop with position = index
                               while (and (< position (length text))
                                          (xml-space-char-p (char text position)))
                               do (incf position)
                               finally (return position)))
                       (literal
                         (case phase
                           ((:after-element-name :after-quote)
                            (when (< close-index (length text))
                              (case (char text close-index)
                                (#\> ">")
                                (#\/ (and (starts-with-p "/>" text close-index) "/>")))))
                           (:end-after-name
                            (when (and (< close-index (length text))
                                       (char= (char text close-index) #\>)) ">")))))
                  (when literal
                    (%xml-tokenizer-skip-space tokenizer)
                    (setf *xml-tokenizer-token-position*
                          (%xml-tokenizer-current-position tokenizer))
                    (let ((kind (if (string= literal "/>")
                                    :empty-element-close
                                    :tag-close)))
                      (multiple-value-bind (actual-kind value start end)
                          (if (eq phase :end-after-name)
                             (%xml-tokenizer-next-end-tag-close tokenizer)
                             (%xml-tokenizer-start-tag-close tokenizer kind literal))
                        (declare (ignore start))
                        (setf (%xml-tokenizer-state-tag-phase state) nil
                              *xml-tokenizer-actual-token-kind* actual-kind)
                        (values t (- end index) nil value))))))))
           (mode-rules ()
             (list
              (cons :content (list (tag-open-rule) (content-rule)))
              (cons :tag (list (tag-close-rule) (tag-token-rule))))))
    (cl-parser-kit:make-tokenizer
     :modes (mapcar (lambda (entry)
                      (cons (car entry) (coerce (cdr entry) 'vector)))
                    (mode-rules))
     :initial-mode :content
     :unknown-token-type :unknown)))

(defun %xml-tokenizer-create-cursor (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (source (%xml-tokenizer-source tokenizer))
         (parser-tokenizer
           (or *xml-tokenizer-parser-tokenizer*
               (setf *xml-tokenizer-parser-tokenizer*
                     (%xml-tokenizer-create-parser-tokenizer)))))
    (cl-parser-kit:make-tokenizer-cursor
     input parser-tokenizer
     ;; Match the accepted input length so the cursor default cannot reject it.
     :max-source-length (length input)
     :max-tokens (length input)
     :max-rules
     (loop for mode in (cl-parser-kit:tokenizer-modes parser-tokenizer)
           sum (length (cdr mode)))
     :max-mode-depth 2
     :token-constructor
     (lambda (kind text value start end line column)
       (declare (ignore text))
       (make-xml-token
        :kind (if (or (eq kind :xml-content-token)
                      (eq kind :xml-tag-token))
                  *xml-tokenizer-actual-token-kind*
                  kind)
        :value value
        :position (or *xml-tokenizer-token-position*
            (make-xml-position :source source :offset start
                               :line line :column column))
        :end end)))))

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
              (*xml-tokenizer-actual-token-kind* nil)
              (*xml-tokenizer-token-position* nil))
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
