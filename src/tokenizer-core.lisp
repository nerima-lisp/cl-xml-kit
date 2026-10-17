;;;; src/tokenizer-core.lisp

(in-package #:cl-xml-kit)

(define-immutable-record xml-token
    (:constructor %make-xml-token
     :conc-name %xml-token-
     :predicate xml-token-p
     :predicate-doc "Return true when VALUE is an XML token."
     :structure-doc "An immutable lexical token with its source position.

END is the exclusive normalized offset after the lexeme, or NIL when the token
was built without one.")
  ((kind nil xml-token-kind "Return a token's lexical kind." :identity)
   (value nil xml-token-value "Return a fresh copy of a token's lexical value, or NIL."
          :copy-seq)
   (position nil xml-token-position "Return a token's source position, or NIL." :identity)
   (end nil xml-token-end "Return a token's exclusive normalized end offset, or NIL."
        :identity)))

(defun make-xml-token (&key kind value position end)
  "Return an XML token using the kind-specific value representation.

The string-valued kinds contain delimiter-free lexical text as documented by
`+XML-TOKEN-KINDS+`; punctuation kinds and EOF require NIL. END, when
supplied, is the exclusive normalized offset after the lexeme."
  (let ((spec (assoc kind +xml-token-kinds+)))
    (unless spec
      (xml-error 'xml-input-error :unknown-token-kind :format-arguments (kind)))
    (if (eq (second spec) :string)
        (unless (stringp value)
          (xml-error 'xml-input-error :token-kind-needs-string :format-arguments (kind)))
        (when value
          (xml-error 'xml-input-error :token-kind-needs-nil :format-arguments (kind)))))
  (when (and position (not (xml-position-p position)))
    (xml-error 'xml-input-error :label-must-be-xml-position
               :format-arguments ("TOKEN POSITION")))
  (unless (typep end '(or null (integer 0)))
    (xml-error 'xml-input-error :token-end-invalid))
  (%make-xml-token kind (and value (copy-seq value)) position end))

(defstruct (%xml-tokenizer-state
            (:constructor %make-xml-tokenizer-state (index line column mode eof-returned))
            (:copier nil))
  (index 0)
  (line 1)
  (column 1)
  (mode :content)
  (eof-returned nil))

(defstruct (xml-tokenizer
            (:constructor %make-xml-tokenizer (input source limits))
            (:conc-name %xml-tokenizer-)
            (:copier nil)
            (:predicate nil))
  "A stateful tokenizer over normalized character input.

INPUT is a normalized string. SOURCE is an optional source name, LIMITS is an
XML-RESOURCE-LIMITS value, and the remaining slots are private cursor state.
The public input, source, and limits snapshots are immutable; NEXT-XML-TOKEN
advances only the private cursor."
  (input "" :read-only t)
  (source nil :read-only t)
  (limits nil :read-only t)
  (state (%make-xml-tokenizer-state 0 1 1 :content nil)))

(defun xml-tokenizer-p (value)
  "Return true when VALUE is an XML tokenizer."
  (typep value 'xml-tokenizer))

(defun make-xml-tokenizer (input &key source
                                      (limits *default-xml-resource-limits*))
  "Return a tokenizer for normalized INPUT with SOURCE and LIMITS.

NEXT-XML-TOKEN returns delimiter-free lexical values and source-start
positions according to `+XML-TOKEN-KINDS+`. The tokenizer owns a private
cursor, so repeated calls consume the input in order."
  (check-argument input stringp xml-input-error :tokenizer-input-invalid)
  (when (and source (not (stringp source)))
    (xml-error 'xml-input-error :label-must-be-string-or-nil
               :format-arguments ("TOKENIZER SOURCE")))
  (check-argument limits xml-resource-limits-p xml-input-error
                  :label-must-be-resource-limits
                  :format-arguments ("TOKENIZER LIMITS"))
  (%make-xml-tokenizer (copy-seq input) (and source (copy-seq source)) limits))

(defun xml-tokenizer-input (tokenizer)
  "Return a fresh copy of the tokenizer's normalized input."
  (copy-seq (%xml-tokenizer-input tokenizer)))

(defun xml-tokenizer-source (tokenizer)
  "Return the tokenizer's source name, or NIL."
  (and (%xml-tokenizer-source tokenizer) (copy-seq (%xml-tokenizer-source tokenizer))))

(defun xml-tokenizer-limits (tokenizer)
  "Return the tokenizer's immutable resource limits."
  (%xml-tokenizer-limits tokenizer))

(defun %xml-tokenizer-current-position (tokenizer)
  (let ((state (%xml-tokenizer-state tokenizer)))
    (make-xml-position :source (%xml-tokenizer-source tokenizer)
                       :offset (%xml-tokenizer-state-index state)
                       :line (%xml-tokenizer-state-line state)
                       :column (%xml-tokenizer-state-column state))))

(defun %xml-tokenizer-fail (tokenizer message &key constraint expected)
  (error 'xml-well-formedness-error
         :message message
         :position (%xml-tokenizer-current-position tokenizer)
         :source (%xml-tokenizer-source tokenizer)
         :constraint constraint
         :expected expected))

(defun %xml-tokenizer-limit-fail (tokenizer kind value limit)
  (error 'xml-resource-limit-exceeded
         :message (xml-message :resource-limit-exceeded-values kind value limit)
         :position (%xml-tokenizer-current-position tokenizer)
         :source (%xml-tokenizer-source tokenizer)
         :kind kind
         :value value
         :limit limit))

(defun %xml-tokenizer-starts-with-p (tokenizer string)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (start (%xml-tokenizer-state-index state))
         (end (+ start (length string))))
    (and (<= end (length input))
         (string= string input :start2 start :end2 end))))

(defun %xml-tokenizer-advance (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (index (%xml-tokenizer-state-index state))
         (character (char input index)))
    (unless (xml-char-p character)
      (%xml-tokenizer-fail tokenizer
                           (xml-message :illegal-character (char-code character))
                           :constraint :legal-character))
    (incf (%xml-tokenizer-state-index state))
    (if (char= character #\Newline)
        (progn
          (incf (%xml-tokenizer-state-line state))
          (setf (%xml-tokenizer-state-column state) 1))
        (incf (%xml-tokenizer-state-column state)))
    character))

(defun %xml-tokenizer-consume-literal (tokenizer literal)
  (loop repeat (length literal) do (%xml-tokenizer-advance tokenizer)))

(defun %xml-tokenizer-skip-space (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer)))
    (loop while (and (< (%xml-tokenizer-state-index state) (length input))
                     (xml-space-char-p (char input (%xml-tokenizer-state-index state))))
          do (%xml-tokenizer-advance tokenizer))))

(defun %xml-tokenizer-token (tokenizer kind value position)
  (make-xml-token :kind kind :value value :position position
                  :end (%xml-tokenizer-state-index
                        (%xml-tokenizer-state tokenizer))))

(defun %xml-tokenizer-check-name-limit (tokenizer length)
  (let ((limit (xml-resource-limits-max-name-length
                (%xml-tokenizer-limits tokenizer))))
    (when (> length limit)
      (%xml-tokenizer-limit-fail tokenizer :max-name-length length limit))))

(defun %xml-tokenizer-check-text-limit (tokenizer length)
  (let ((limit (xml-resource-limits-max-text-node-length
                (%xml-tokenizer-limits tokenizer))))
    (when (> length limit)
      (%xml-tokenizer-limit-fail tokenizer :max-text-node-length length limit))))
