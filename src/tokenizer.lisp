(in-package #:cl-xml-kit)

(defparameter +xml-token-kinds+
  '(:text :name :quoted-value :entity-reference :comment :cdata
    :processing-instruction :doctype :start-tag-open :end-tag-open
    :tag-close :empty-element-close :equals :eof)
  "Allowed XML token kinds and their W3 consumer contract.

TEXT is a contiguous character-data run. NAME is one XML Name. QUOTED-VALUE
is the content of one attribute value with its surrounding quote removed;
entity references in it remain lexical (for example, \"a &amp; b\").
ENTITY-REFERENCE is the content between `&` and `;` (for example, \"amp\"
or \"#x26\"). COMMENT, CDATA, PROCESSING-INSTRUCTION, and DOCTYPE are the
contents of their corresponding XML delimiters removed. A processing
instruction value therefore contains its target and data, while a doctype
value contains the declaration body after `<!DOCTYPE` (including its
internal subset when present). START-TAG-OPEN, END-TAG-OPEN, TAG-CLOSE,
EMPTY-ELEMENT-CLOSE, EQUALS, and EOF have NIL values. Token positions are the
zero-based normalized input offset and one-based line and column at the first
character of the lexical construct. EOF is returned once at the final
position; subsequent calls return NIL.")

(defstruct (xml-token
            (:constructor %make-xml-token (kind value position end))
            (:conc-name %xml-token-)
            (:copier nil)
            (:predicate xml-token-p))
  "An immutable lexical token with its source position.

END is the exclusive normalized offset after the lexeme, or NIL when the token
was built without one."
  (kind nil :read-only t)
  (value nil :read-only t)
  (position nil :read-only t)
  (end nil :read-only t))

(defun make-xml-token (&key kind value position end)
  "Return an XML token using the kind-specific value representation.

The string-valued kinds contain delimiter-free lexical text as documented by
`+XML-TOKEN-KINDS+`; punctuation kinds and EOF require NIL. END, when
supplied, is the exclusive normalized offset after the lexeme."
  (unless (member kind +xml-token-kinds+)
    (error 'xml-input-error
           :message (format nil "Unknown XML token kind ~S." kind)))
  (if (member kind '(:text :name :quoted-value :entity-reference :comment
                     :cdata :processing-instruction :doctype))
      (unless (stringp value)
        (error 'xml-input-error
               :message (format nil "XML token kind ~S requires a string value."
                                kind)))
      (when value
        (error 'xml-input-error
               :message (format nil "XML token kind ~S requires a NIL value."
                                kind))))
  (when (and position (not (xml-position-p position)))
    (error 'xml-input-error
           :message "TOKEN POSITION must be an XML-POSITION."))
  (unless (typep end '(or null (integer 0)))
    (error 'xml-input-error
           :message "TOKEN END must be a non-negative integer or NIL."))
  (%make-xml-token kind (and value (copy-seq value)) position end))

(defun xml-token-kind (token)
  "Return a token's lexical kind."
  (%xml-token-kind token))

(defun xml-token-value (token)
  "Return a fresh copy of a token's lexical value, or NIL."
  (and (%xml-token-value token) (copy-seq (%xml-token-value token))))

(defun xml-token-position (token)
  "Return a token's source position, or NIL."
  (%xml-token-position token))

(defun xml-token-end (token)
  "Return a token's exclusive normalized end offset, or NIL."
  (%xml-token-end token))

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
            (:predicate xml-tokenizer-p))
  "A stateful tokenizer over normalized character input.

INPUT is a normalized string. SOURCE is an optional source name, LIMITS is an
XML-RESOURCE-LIMITS value, and the remaining slots are private cursor state.
The public input, source, and limits snapshots are immutable; NEXT-XML-TOKEN
advances only the private cursor."
  (input "" :read-only t)
  (source nil :read-only t)
  (limits nil :read-only t)
  (state (%make-xml-tokenizer-state 0 1 1 :content nil)))

(defun make-xml-tokenizer (input &key source
                                      (limits *default-xml-resource-limits*))
  "Return a tokenizer for normalized INPUT with SOURCE and LIMITS.

NEXT-XML-TOKEN returns delimiter-free lexical values and source-start
positions according to `+XML-TOKEN-KINDS+`. The tokenizer owns a private
cursor, so repeated calls consume the input in order."
  (unless (stringp input)
    (error 'xml-input-error :message "TOKENIZER INPUT must be a normalized string."))
  (when (and source (not (stringp source)))
    (error 'xml-input-error :message "TOKENIZER SOURCE must be a string or NIL."))
  (unless (xml-resource-limits-p limits)
    (error 'xml-input-error :message "TOKENIZER LIMITS must be XML-RESOURCE-LIMITS."))
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
         :message (format nil "XML resource limit ~S exceeded: ~D > ~D."
                          kind value limit)
         :position (%xml-tokenizer-current-position tokenizer)
         :source (%xml-tokenizer-source tokenizer)
         :kind kind
         :value value
         :limit limit))

(defun %xml-tokenizer-valid-char-p (character)
  (let ((code (char-code character)))
    (or (= code #x9) (= code #xA) (= code #xD)
        (<= #x20 code #xD7FF)
        (<= #xE000 code #xFFFD)
        (<= #x10000 code #x10FFFF))))

(defun %xml-tokenizer-space-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun %xml-tokenizer-name-start-p (character)
  (let ((code (char-code character)))
    (or (char= character #\:) (char= character #\_)
        (<= (char-code #\A) code (char-code #\Z))
        (<= (char-code #\a) code (char-code #\z))
        (<= #xC0 code #xD6) (<= #xD8 code #xF6)
        (<= #xF8 code #x2FF) (<= #x370 code #x37D)
        (<= #x37F code #x1FFF) (<= #x200C code #x200D)
        (<= #x2070 code #x218F) (<= #x2C00 code #x2FEF)
        (<= #x3001 code #xD7FF) (<= #xF900 code #xFDCF)
        (<= #xFDF0 code #xFFFD) (<= #x10000 code #xEFFFF))))

(defun %xml-tokenizer-name-char-p (character)
  (let ((code (char-code character)))
    (or (%xml-tokenizer-name-start-p character)
        (char= character #\-) (char= character #\.)
        (<= (char-code #\0) code (char-code #\9))
        (= code #xB7) (<= #x300 code #x36F) (<= #x203F code #x2040))))

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
    (unless (%xml-tokenizer-valid-char-p character)
      (%xml-tokenizer-fail tokenizer
                           (format nil "Character U+~4,'0X is not legal XML character."
                                   (char-code character))
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
                     (%xml-tokenizer-space-p
                      (char input (%xml-tokenizer-state-index state))))
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
                 (%xml-tokenizer-name-start-p (char input start)))
      (%xml-tokenizer-fail tokenizer "Expected an XML Name." :expected :name))
    (%xml-tokenizer-advance tokenizer)
    (loop while (and (< (%xml-tokenizer-state-index state) (length input))
                     (%xml-tokenizer-name-char-p
                      (char input (%xml-tokenizer-state-index state))))
          do (%xml-tokenizer-advance tokenizer))
    (let ((value (subseq input start (%xml-tokenizer-state-index state))))
      (%xml-tokenizer-check-name-limit tokenizer (length value))
      (values value position))))

(defun %xml-tokenizer-validate-entity-body (tokenizer body)
  (when (zerop (length body))
    (%xml-tokenizer-fail tokenizer "An entity reference must not be empty."
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
                               "Malformed numeric character reference."
                               :constraint :character-reference))
        (let ((code (parse-integer body :start digit-start :radix radix)))
          (unless (and (<= 0 code) (<= code #x10FFFF)
                       (%xml-tokenizer-valid-char-p (code-char code)))
            (%xml-tokenizer-fail tokenizer
                                 "Numeric character reference names an illegal XML character."
                                 :constraint :character-reference))))
      (progn
        (unless (%xml-tokenizer-name-start-p (char body 0))
          (%xml-tokenizer-fail tokenizer "Malformed entity reference."
                               :constraint :entity-reference))
        (loop for index from 1 below (length body)
              unless (%xml-tokenizer-name-char-p (char body index))
                do (%xml-tokenizer-fail tokenizer "Malformed entity reference."
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
        (%xml-tokenizer-fail tokenizer "Unterminated entity reference."
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
          (%xml-tokenizer-fail tokenizer "Unterminated quoted attribute value."
                               :constraint :attribute-value :expected quote))
        (let ((character (char input (%xml-tokenizer-state-index state))))
          (cond
            ((char= character quote)
             (let ((end (%xml-tokenizer-state-index state)))
               (%xml-tokenizer-advance tokenizer)
               (return (values (subseq input start end) position))))
            ((char= character #\<)
             (%xml-tokenizer-fail tokenizer
                                  "A '<' is not allowed in an attribute value."
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
        (%xml-tokenizer-fail tokenizer "The character sequence ]]> is not allowed in text."
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
          (%xml-tokenizer-fail tokenizer "Unterminated XML comment."
                               :constraint :comment :expected "-->"))
        (cond
          ((%xml-tokenizer-starts-with-p tokenizer "-->")
           (let ((end (%xml-tokenizer-state-index state)))
             (when (and (> end start) (char= (char input (1- end)) #\-))
               (%xml-tokenizer-fail tokenizer
                                    "An XML comment must not end with a hyphen."
                                    :constraint :comment))
             (%xml-tokenizer-consume-literal tokenizer "-->")
             (setf (%xml-tokenizer-state-mode state) :content)
             (return (%xml-tokenizer-token tokenizer :comment
                                           (subseq input start end) position))))
          ((and (char= (char input (%xml-tokenizer-state-index state)) #\-)
                (< (1+ (%xml-tokenizer-state-index state)) (length input))
                (char= (char input (1+ (%xml-tokenizer-state-index state))) #\-))
           (%xml-tokenizer-fail tokenizer "XML comments must not contain '--'."
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
          (%xml-tokenizer-fail tokenizer "Unterminated CDATA section."
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
      (%xml-tokenizer-fail tokenizer "A processing instruction needs a target."
                           :constraint :processing-instruction))
    (unless (%xml-tokenizer-name-start-p (char body 0))
      (%xml-tokenizer-fail tokenizer "A processing instruction target must be an XML Name."
                           :constraint :processing-instruction))
    (let ((index 1))
      (loop while (and (< index length)
                       (%xml-tokenizer-name-char-p (char body index)))
            do (incf index))
      (let ((target (subseq body 0 index)))
        (%xml-tokenizer-check-name-limit tokenizer (length target))
        (when (and (< index length)
                   (not (%xml-tokenizer-space-p (char body index))))
          (%xml-tokenizer-fail tokenizer
                               "Processing-instruction target must be separated from its data."
                               :constraint :processing-instruction))))))

(defun %xml-tokenizer-scan-processing-instruction (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer)))
    (%xml-tokenizer-consume-literal tokenizer "<?")
    (let ((start (%xml-tokenizer-state-index state)))
      (loop
        (when (= (%xml-tokenizer-state-index state) (length input))
          (%xml-tokenizer-fail tokenizer "Unterminated processing instruction."
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
    (loop while (and (< index length) (%xml-tokenizer-space-p (char body index)))
          do (incf index))
    (unless (and (< index length) (%xml-tokenizer-name-start-p (char body index)))
      (%xml-tokenizer-fail tokenizer "DOCTYPE must name a root element."
                           :constraint :doctype))
    (let ((start index))
      (incf index)
      (loop while (and (< index length)
                       (%xml-tokenizer-name-char-p (char body index)))
            do (incf index))
      (%xml-tokenizer-check-name-limit tokenizer (- index start))
      (labels ((skip-space ()
                 (loop while (and (< index length)
                                  (%xml-tokenizer-space-p (char body index)))
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
                                  "SYSTEM identifiers need a quoted system literal."
                                  :constraint :doctype)))
          ((keyword-at-p "PUBLIC")
           (incf index (length "PUBLIC"))
           (unless (progn (skip-space) (quoted-literal-p))
             (%xml-tokenizer-fail tokenizer
                                  "PUBLIC identifiers need a quoted public literal."
                                  :constraint :doctype))
           (unless (progn (skip-space) (quoted-literal-p))
             (%xml-tokenizer-fail tokenizer
                                  "PUBLIC identifiers need a quoted system literal."
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
              (%xml-tokenizer-fail tokenizer "Unterminated DOCTYPE internal subset."
                                   :constraint :doctype))))
        (skip-space)
        (unless (= index length)
          (%xml-tokenizer-fail tokenizer "Unexpected content in DOCTYPE declaration."
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
          (%xml-tokenizer-fail tokenizer "Unterminated DOCTYPE declaration."
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
               (%xml-tokenizer-fail tokenizer "Unmatched ']' in DOCTYPE."
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
            (%xml-tokenizer-name-start-p
             (char input (%xml-tokenizer-state-index state))))
       (multiple-value-bind (value position) (%xml-tokenizer-scan-name tokenizer)
         (setf (%xml-tokenizer-state-mode state) :start-after-attribute-name)
         (%xml-tokenizer-token tokenizer :name value position)))
      (t (%xml-tokenizer-fail tokenizer
                              "Expected an attribute name or tag close."
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
                             "Expected '=' after an attribute name."
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
                             "Expected a quoted attribute value after '='."
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
              (%xml-tokenizer-name-start-p
               (char input (%xml-tokenizer-state-index state))))
         (if (= before (%xml-tokenizer-state-index state))
             (%xml-tokenizer-fail tokenizer
                                  "Attributes must be separated by XML whitespace."
                                  :constraint :start-tag)
             (multiple-value-bind (value position) (%xml-tokenizer-scan-name tokenizer)
               (setf (%xml-tokenizer-state-mode state) :start-after-attribute-name)
               (%xml-tokenizer-token tokenizer :name value position))))
        (t (%xml-tokenizer-fail tokenizer
                                "Expected another attribute or tag close."
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
                             "Expected '>' after an end-tag name."
                             :constraint :end-tag))))

(defun next-xml-token (tokenizer)
  "Return the next XML token, then NIL after the one-shot EOF token.

The token kind/value/position meanings are fixed by `+XML-TOKEN-KINDS+`.
Malformed XML signals XML-WELL-FORMEDNESS-ERROR with the current source
position; configured name and text-node bounds signal
XML-RESOURCE-LIMIT-EXCEEDED. Every successful branch consumes input or
returns EOF, so malformed input cannot leave a zero-progress loop."
  (unless (xml-tokenizer-p tokenizer)
    (error 'xml-input-error :message "TOKENIZER must be an XML-TOKENIZER."))
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer)))
    (when (%xml-tokenizer-state-eof-returned state)
      (return-from next-xml-token nil))
    (loop
      (when (= (%xml-tokenizer-state-index state) (length input))
        (unless (eq (%xml-tokenizer-state-mode state) :content)
          (%xml-tokenizer-fail tokenizer "Unexpected end of XML input."
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
           (%xml-tokenizer-fail tokenizer "Unknown tokenizer state."
                                :constraint :tokenizer-state)))))))

(loop for (name . text) in
      '((xml-token-p . "Return true when VALUE is an XML token.")
        (xml-tokenizer-p . "Return true when VALUE is an XML tokenizer.")
        (xml-tokenizer-input . "Return a fresh copy of normalized tokenizer input.")
        (xml-tokenizer-source . "Return a tokenizer's source name, or NIL.")
        (xml-tokenizer-limits . "Return a tokenizer's resource limits.")
        (next-xml-token . "Return the next XML token, then NIL after one-shot EOF.")
        (make-xml-tokenizer . "Return a stateful tokenizer for normalized XML input."))
      do (setf (documentation name 'function) text))

