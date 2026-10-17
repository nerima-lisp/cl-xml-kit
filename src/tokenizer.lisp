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

(defun %xml-tokenizer-scan-name (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (start (%xml-tokenizer-state-index state))
         (position (%xml-tokenizer-current-position tokenizer)))
    (unless (and (< start (length input))
                 (xml-name-start-char-p (char input start)))
      (%xml-tokenizer-fail tokenizer (xml-message :expected-name) :expected :name))
    (%xml-tokenizer-advance tokenizer)
    (loop while (and (< (%xml-tokenizer-state-index state) (length input))
                     (xml-name-char-p (char input (%xml-tokenizer-state-index state))))
          do (%xml-tokenizer-advance tokenizer))
    (let ((value (subseq input start (%xml-tokenizer-state-index state))))
      (%xml-tokenizer-check-name-limit tokenizer (length value))
      (values value position))))

(defun %xml-tokenizer-validate-entity-body (tokenizer body)
  (when (zerop (length body))
    (%xml-tokenizer-fail tokenizer (xml-message :empty-entity-reference)
                         :constraint :entity-reference))
  (if (char= (char body 0) #\#)
      (let* ((hexadecimal (and (> (length body) 1)
                               (char= (char body 1) #\x)))
             (digit-start (if hexadecimal 2 1))
             (radix (if hexadecimal 16 10)))
        (when (or (= digit-start (length body))
                  (loop for index from digit-start below (length body)
                        thereis (not (xml-digit-weight (char body index) radix))))
          (%xml-tokenizer-fail tokenizer
                               (xml-message :malformed-numeric-reference)
                               :constraint :character-reference))
        (let ((code (parse-integer body :start digit-start :radix radix)))
          (unless (and (<= 0 code) (< code char-code-limit) (xml-char-p (code-char code)))
            (%xml-tokenizer-fail tokenizer
                                 (xml-message :numeric-reference-illegal)
                                 :constraint :character-reference))))
      (progn
        (unless (xml-name-start-char-p (char body 0))
          (%xml-tokenizer-fail tokenizer (xml-message :malformed-entity-reference)
                               :constraint :entity-reference))
        (loop for index from 1 below (length body)
              unless (xml-name-char-p (char body index))
                do (%xml-tokenizer-fail tokenizer (xml-message :malformed-entity-reference)
                                        :constraint :entity-reference))
        (%xml-tokenizer-check-name-limit tokenizer (length body)))))

(defun %xml-tokenizer-scan-entity (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer)))
    (%xml-tokenizer-advance tokenizer)
    (let ((start (%xml-tokenizer-state-index state)))
      (loop while (and (< (%xml-tokenizer-state-index state) (length input))
                       (not (char= (char input (%xml-tokenizer-state-index state))
                                   #\;)))
            do (%xml-tokenizer-advance tokenizer))
      (when (or (= (%xml-tokenizer-state-index state) (length input))
                (not (char= (char input (%xml-tokenizer-state-index state)) #\;)))
        (%xml-tokenizer-fail tokenizer (xml-message :unterminated-entity-reference)
                             :constraint :entity-reference :expected #\;))
      (let ((body (subseq input start (%xml-tokenizer-state-index state))))
        (%xml-tokenizer-validate-entity-body tokenizer body)
        (%xml-tokenizer-advance tokenizer)
        (values body position)))))

(defun %xml-tokenizer-scan-quoted-value (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer))
         (quote (char input (%xml-tokenizer-state-index state))))
    (%xml-tokenizer-advance tokenizer)
    (let ((start (%xml-tokenizer-state-index state)))
      (loop
        (when (= (%xml-tokenizer-state-index state) (length input))
          (%xml-tokenizer-fail tokenizer (xml-message :unterminated-attribute-value)
                               :constraint :attribute-value :expected quote))
        (let ((character (char input (%xml-tokenizer-state-index state))))
          (cond
            ((char= character quote)
             (let ((end (%xml-tokenizer-state-index state)))
               (%xml-tokenizer-advance tokenizer)
               (return (values (subseq input start end) position))))
            ((char= character #\<)
             (%xml-tokenizer-fail tokenizer
                                  (xml-message :lt-in-attribute-value)
                                  :constraint :attribute-value))
            ((char= character #\&)
             (%xml-tokenizer-scan-entity tokenizer))
            (t (%xml-tokenizer-advance tokenizer))))))))

(defun %xml-tokenizer-scan-text (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer))
         (start (%xml-tokenizer-state-index state)))
    (loop while (and (< (%xml-tokenizer-state-index state) (length input))
                     (not (member (char input (%xml-tokenizer-state-index state))
                                  '(#\< #\&) :test #'char=)))
          do (%xml-tokenizer-advance tokenizer))
    (let ((value (subseq input start (%xml-tokenizer-state-index state))))
      (when (search "]]>" value)
        (%xml-tokenizer-fail tokenizer (xml-message :cdata-end-in-text)
                             :constraint :character-data))
      (%xml-tokenizer-check-text-limit tokenizer (length value))
      (%xml-tokenizer-token tokenizer :text value position))))

(defun %xml-tokenizer-scan-comment (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer)))
    (%xml-tokenizer-consume-literal tokenizer "<!--")
    (let ((start (%xml-tokenizer-state-index state)))
      (loop
        (when (= (%xml-tokenizer-state-index state) (length input))
          (%xml-tokenizer-fail tokenizer (xml-message :unterminated-comment)
                               :constraint :comment :expected "-->"))
        (cond
          ((%xml-tokenizer-starts-with-p tokenizer "-->")
           (let ((end (%xml-tokenizer-state-index state)))
             (when (and (> end start) (char= (char input (1- end)) #\-))
               (%xml-tokenizer-fail tokenizer
                                    (xml-message :comment-ends-with-hyphen)
                                    :constraint :comment))
             (%xml-tokenizer-consume-literal tokenizer "-->")
             (setf (%xml-tokenizer-state-mode state) :content)
             (return (%xml-tokenizer-token tokenizer :comment
                                           (subseq input start end) position))))
          ((and (char= (char input (%xml-tokenizer-state-index state)) #\-)
                (< (1+ (%xml-tokenizer-state-index state)) (length input))
                (char= (char input (1+ (%xml-tokenizer-state-index state))) #\-))
           (%xml-tokenizer-fail tokenizer (xml-message :double-hyphen-in-comment)
                                :constraint :comment))
          (t (%xml-tokenizer-advance tokenizer)))))))

(defun %xml-tokenizer-scan-cdata (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer)))
    (%xml-tokenizer-consume-literal tokenizer "<![CDATA[")
    (let ((start (%xml-tokenizer-state-index state)))
      (loop
        (when (= (%xml-tokenizer-state-index state) (length input))
          (%xml-tokenizer-fail tokenizer (xml-message :unterminated-cdata)
                               :constraint :cdata :expected "]]>") )
        (if (%xml-tokenizer-starts-with-p tokenizer "]]>")
            (let ((end (%xml-tokenizer-state-index state)))
              (%xml-tokenizer-consume-literal tokenizer "]]>")
              (setf (%xml-tokenizer-state-mode state) :content)
              (%xml-tokenizer-check-text-limit tokenizer (- end start))
              (return (%xml-tokenizer-token tokenizer :cdata
                                            (subseq input start end) position)))
            (%xml-tokenizer-advance tokenizer))))))

(defun %xml-tokenizer-validate-pi-body (tokenizer body)
  (let ((length (length body)))
    (when (zerop length)
      (%xml-tokenizer-fail tokenizer (xml-message :pi-needs-target)
                           :constraint :processing-instruction))
    (unless (xml-name-start-char-p (char body 0))
      (%xml-tokenizer-fail tokenizer (xml-message :pi-target-not-name)
                           :constraint :processing-instruction))
    (let ((index 1))
      (loop while (and (< index length)
                       (xml-name-char-p (char body index)))
            do (incf index))
      (let ((target (subseq body 0 index)))
        (%xml-tokenizer-check-name-limit tokenizer (length target))
        (when (and (< index length)
                   (not (xml-space-char-p (char body index))))
          (%xml-tokenizer-fail tokenizer
                               (xml-message :pi-target-not-separated)
                               :constraint :processing-instruction))))))

(defun %xml-tokenizer-scan-processing-instruction (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer)))
    (%xml-tokenizer-consume-literal tokenizer "<?")
    (let ((start (%xml-tokenizer-state-index state)))
      (loop
        (when (= (%xml-tokenizer-state-index state) (length input))
          (%xml-tokenizer-fail tokenizer (xml-message :unterminated-pi)
                               :constraint :processing-instruction :expected "?>"))
        (if (%xml-tokenizer-starts-with-p tokenizer "?>")
            (let ((end (%xml-tokenizer-state-index state)))
              (%xml-tokenizer-validate-pi-body tokenizer (subseq input start end))
              (%xml-tokenizer-consume-literal tokenizer "?>")
              (setf (%xml-tokenizer-state-mode state) :content)
              (return (%xml-tokenizer-token tokenizer :processing-instruction
                                            (subseq input start end) position)))
            (%xml-tokenizer-advance tokenizer))))))

(defun %xml-tokenizer-validate-doctype-body (tokenizer body)
  (let* ((length (length body))
         (index 0))
    (loop while (and (< index length) (xml-space-char-p (char body index)))
          do (incf index))
    (unless (and (< index length) (xml-name-start-char-p (char body index)))
      (%xml-tokenizer-fail tokenizer (xml-message :doctype-needs-root-element)
                           :constraint :doctype))
    (let ((start index))
      (incf index)
      (loop while (and (< index length)
                       (xml-name-char-p (char body index)))
            do (incf index))
      (%xml-tokenizer-check-name-limit tokenizer (- index start))
      (labels ((skip-space ()
                 (loop while (and (< index length)
                                  (xml-space-char-p (char body index)))
                       do (incf index)))
               (quoted-literal-p ()
                 (when (and (< index length)
                            (member (char body index) '(#\" #\')))
                   (let ((quote (char body index)))
                     (incf index)
                     (loop while (and (< index length)
                                      (char/= (char body index) quote))
                           do (incf index))
                     (when (< index length)
                       (incf index)
                       t))))
               (keyword-at-p (keyword)
                 (and (<= (+ index (length keyword)) length)
                      (string= keyword body :start2 index
                              :end2 (+ index (length keyword))))))
        (skip-space)
        (cond
          ((keyword-at-p "SYSTEM")
           (incf index (length "SYSTEM"))
           (unless (and (progn (skip-space) t) (quoted-literal-p))
             (%xml-tokenizer-fail tokenizer
                                  (xml-message :system-needs-literal)
                                  :constraint :doctype)))
          ((keyword-at-p "PUBLIC")
           (incf index (length "PUBLIC"))
           (unless (progn (skip-space) (quoted-literal-p))
             (%xml-tokenizer-fail tokenizer
                                  (xml-message :public-needs-public-literal)
                                  :constraint :doctype))
           (unless (progn (skip-space) (quoted-literal-p))
             (%xml-tokenizer-fail tokenizer
                                  (xml-message :public-needs-system-literal)
                                  :constraint :doctype))))
        (skip-space)
        (when (and (< index length) (char= (char body index) #\[))
          (let ((subset-depth 1)
                (quote nil))
            (incf index)
            (loop while (and (< index length) (plusp subset-depth)) do
              (let ((character (char body index)))
                (cond
                  (quote
                   (when (char= character quote) (setf quote nil)))
                  ((member character '(#\" #\')) (setf quote character))
                  ((char= character #\[) (incf subset-depth))
                  ((char= character #\]) (decf subset-depth)))
                (incf index)))
            (when (plusp subset-depth)
              (%xml-tokenizer-fail tokenizer (xml-message :unterminated-internal-subset)
                                   :constraint :doctype))))
        (skip-space)
        (unless (= index length)
          (%xml-tokenizer-fail tokenizer (xml-message :unexpected-doctype-content)
                               :constraint :doctype))))))

(defun %xml-tokenizer-scan-doctype (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer)))
    (%xml-tokenizer-consume-literal tokenizer "<!DOCTYPE")
    (let ((start (%xml-tokenizer-state-index state))
          (subset-depth 0)
          (quote nil))
      (loop
        (when (= (%xml-tokenizer-state-index state) (length input))
          (%xml-tokenizer-fail tokenizer (xml-message :unterminated-doctype)
                               :constraint :doctype :expected #\>))
        (let ((character (char input (%xml-tokenizer-state-index state))))
          (cond
            (quote
             (%xml-tokenizer-advance tokenizer)
             (when (char= character quote) (setf quote nil)))
            ((or (char= character #\") (char= character #\'))
             (setf quote character)
             (%xml-tokenizer-advance tokenizer))
            ((char= character #\[)
             (incf subset-depth)
             (%xml-tokenizer-advance tokenizer))
            ((char= character #\])
             (when (zerop subset-depth)
               (%xml-tokenizer-fail tokenizer (xml-message :unmatched-bracket-in-doctype)
                                    :constraint :doctype))
             (decf subset-depth)
             (%xml-tokenizer-advance tokenizer))
            ((and (char= character #\>) (zerop subset-depth))
             (let ((end (%xml-tokenizer-state-index state)))
               (%xml-tokenizer-validate-doctype-body tokenizer
                                                      (subseq input start end))
               (%xml-tokenizer-advance tokenizer)
               (setf (%xml-tokenizer-state-mode state) :content)
               (return (%xml-tokenizer-token tokenizer :doctype
                                             (subseq input start end) position))))
            (t (%xml-tokenizer-advance tokenizer))))))))

(defun %xml-tokenizer-start-tag-close (tokenizer kind length)
  (let* ((state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer)))
    (loop repeat length do (%xml-tokenizer-advance tokenizer))
    (setf (%xml-tokenizer-state-mode state) :content)
    (%xml-tokenizer-token tokenizer kind nil position)))

(defun %xml-tokenizer-next-content (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer))
         (character (char input (%xml-tokenizer-state-index state))))
    (cond
      ((char= character #\&)
       (multiple-value-bind (value token-position) (%xml-tokenizer-scan-entity tokenizer)
         (%xml-tokenizer-token tokenizer :entity-reference value token-position)))
      ((char= character #\<)
       (cond
         ((%xml-tokenizer-starts-with-p tokenizer "<!--")
          (%xml-tokenizer-scan-comment tokenizer))
         ((%xml-tokenizer-starts-with-p tokenizer "<![CDATA[")
          (%xml-tokenizer-scan-cdata tokenizer))
         ((%xml-tokenizer-starts-with-p tokenizer "<!DOCTYPE")
          (%xml-tokenizer-scan-doctype tokenizer))
         ((%xml-tokenizer-starts-with-p tokenizer "<?")
          (%xml-tokenizer-scan-processing-instruction tokenizer))
         ((%xml-tokenizer-starts-with-p tokenizer "</")
          (%xml-tokenizer-consume-literal tokenizer "</")
          (setf (%xml-tokenizer-state-mode state) :end-tag)
          (%xml-tokenizer-token tokenizer :end-tag-open nil position))
         (t
          (%xml-tokenizer-advance tokenizer)
          (setf (%xml-tokenizer-state-mode state) :start-tag)
          (%xml-tokenizer-token tokenizer :start-tag-open nil position))))
      (t (%xml-tokenizer-scan-text tokenizer)))))

(defun %xml-tokenizer-next-start-tag (tokenizer)
  (multiple-value-bind (value position) (%xml-tokenizer-scan-name tokenizer)
    (setf (%xml-tokenizer-state-mode (%xml-tokenizer-state tokenizer))
          :start-after-element-name)
    (%xml-tokenizer-token tokenizer :name value position)))

(defun %xml-tokenizer-next-start-after-element-name (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer)))
    (%xml-tokenizer-skip-space tokenizer)
    (cond
      ((%xml-tokenizer-starts-with-p tokenizer "/>")
       (%xml-tokenizer-start-tag-close tokenizer :empty-element-close 2))
      ((and (< (%xml-tokenizer-state-index state) (length input))
            (char= (char input (%xml-tokenizer-state-index state)) #\>))
       (%xml-tokenizer-start-tag-close tokenizer :tag-close 1))
      ((and (< (%xml-tokenizer-state-index state) (length input))
            (xml-name-start-char-p (char input (%xml-tokenizer-state-index state))))
       (multiple-value-bind (value position) (%xml-tokenizer-scan-name tokenizer)
         (setf (%xml-tokenizer-state-mode state) :start-after-attribute-name)
         (%xml-tokenizer-token tokenizer :name value position)))
      (t (%xml-tokenizer-fail tokenizer
                              (xml-message :expected-attribute-or-close)
                              :constraint :start-tag)))))

(defun %xml-tokenizer-next-start-after-attribute-name (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer)))
    (%xml-tokenizer-skip-space tokenizer)
    (if (and (< (%xml-tokenizer-state-index state) (length input))
             (char= (char input (%xml-tokenizer-state-index state)) #\=))
        (progn
          (let ((position (%xml-tokenizer-current-position tokenizer)))
            (%xml-tokenizer-advance tokenizer)
            (setf (%xml-tokenizer-state-mode state) :start-after-equals)
            (%xml-tokenizer-token tokenizer :equals nil position)))
        (%xml-tokenizer-fail tokenizer
                             (xml-message :expected-equals-after-attribute)
                             :constraint :attribute))))

(defun %xml-tokenizer-next-start-after-equals (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer)))
    (%xml-tokenizer-skip-space tokenizer)
    (if (and (< (%xml-tokenizer-state-index state) (length input))
             (member (char input (%xml-tokenizer-state-index state))
                     '(#\" #\') :test #'char=))
        (progn
          (setf (%xml-tokenizer-state-mode state) :start-after-quote)
          (multiple-value-bind (value position) (%xml-tokenizer-scan-quoted-value tokenizer)
            (%xml-tokenizer-token tokenizer :quoted-value value position)))
        (%xml-tokenizer-fail tokenizer
                             (xml-message :expected-quoted-value-after-equals)
                             :constraint :attribute-value))))

(defun %xml-tokenizer-next-start-after-quote (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer)))
    (let ((before (%xml-tokenizer-state-index state)))
      (%xml-tokenizer-skip-space tokenizer)
      (cond
        ((%xml-tokenizer-starts-with-p tokenizer "/>")
         (%xml-tokenizer-start-tag-close tokenizer :empty-element-close 2))
        ((and (< (%xml-tokenizer-state-index state) (length input))
              (char= (char input (%xml-tokenizer-state-index state)) #\>))
         (%xml-tokenizer-start-tag-close tokenizer :tag-close 1))
        ((and (< (%xml-tokenizer-state-index state) (length input))
              (xml-name-start-char-p (char input (%xml-tokenizer-state-index state))))
         (if (= before (%xml-tokenizer-state-index state))
             (%xml-tokenizer-fail tokenizer
                                  (xml-message :attributes-need-whitespace)
                                  :constraint :start-tag)
             (multiple-value-bind (value position) (%xml-tokenizer-scan-name tokenizer)
               (setf (%xml-tokenizer-state-mode state) :start-after-attribute-name)
               (%xml-tokenizer-token tokenizer :name value position))))
        (t (%xml-tokenizer-fail tokenizer
                                (xml-message :expected-another-attribute-or-close)
                                :constraint :start-tag))))))

(defun %xml-tokenizer-next-end-tag (tokenizer)
  (multiple-value-bind (value position) (%xml-tokenizer-scan-name tokenizer)
    (setf (%xml-tokenizer-state-mode (%xml-tokenizer-state tokenizer)) :end-after-name)
    (%xml-tokenizer-token tokenizer :name value position)))

(defun %xml-tokenizer-next-end-after-name (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer)))
    (%xml-tokenizer-skip-space tokenizer)
    (if (and (< (%xml-tokenizer-state-index state) (length input))
             (char= (char input (%xml-tokenizer-state-index state)) #\>))
        (%xml-tokenizer-start-tag-close tokenizer :tag-close 1)
        (%xml-tokenizer-fail tokenizer
                             (xml-message :expected-close-after-end-tag-name)
                             :constraint :end-tag))))

(defun next-xml-token (tokenizer)
  "Return the next XML token, then NIL after the one-shot EOF token.

The token kind/value/position meanings are fixed by `+XML-TOKEN-KINDS+`.
Malformed XML signals XML-WELL-FORMEDNESS-ERROR with the current source
position; configured name and text-node bounds signal
XML-RESOURCE-LIMIT-EXCEEDED. Every successful branch consumes input or
returns EOF, so malformed input cannot leave a zero-progress loop."
  (check-argument tokenizer xml-tokenizer-p xml-input-error :tokenizer-required)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer)))
    (when (%xml-tokenizer-state-eof-returned state)
      (return-from next-xml-token nil))
    (loop
      (when (= (%xml-tokenizer-state-index state) (length input))
        (unless (eq (%xml-tokenizer-state-mode state) :content)
          (%xml-tokenizer-fail tokenizer (xml-message :unexpected-end-of-input)
                               :constraint :well-formedness))
        (setf (%xml-tokenizer-state-eof-returned state) t)
        (return (%xml-tokenizer-token tokenizer :eof nil
                                      (%xml-tokenizer-current-position tokenizer))))
      (return
        (case (%xml-tokenizer-state-mode state)
          (:content (%xml-tokenizer-next-content tokenizer))
          (:start-tag (%xml-tokenizer-next-start-tag tokenizer))
          (:start-after-element-name
           (%xml-tokenizer-next-start-after-element-name tokenizer))
          (:start-after-attribute-name
           (%xml-tokenizer-next-start-after-attribute-name tokenizer))
          (:start-after-equals (%xml-tokenizer-next-start-after-equals tokenizer))
          (:start-after-quote (%xml-tokenizer-next-start-after-quote tokenizer))
          (:end-tag (%xml-tokenizer-next-end-tag tokenizer))
          (:end-after-name (%xml-tokenizer-next-end-after-name tokenizer))
          (otherwise
           (%xml-tokenizer-fail tokenizer (xml-message :unknown-tokenizer-state)
                                :constraint :tokenizer-state)))))))

