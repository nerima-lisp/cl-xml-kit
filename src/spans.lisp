;;;; src/spans.lisp
;;;;
;;;; Lexical span scanner for tools that edit XML without rewriting it. DOM
;;;; positions cannot serve: attributes and end tags have none, end positions
;;;; are not exclusive, and offsets count characters of the line-end-normalized
;;;; text. This scanner drives the tokenizer over that text and records where
;;;; each construct lies in the caller's original string.

(in-package #:cl-xml-kit)

(defstruct (span-node
            (:constructor %make-span-node (start end))
            (:copier nil))
  "A lexical construct and its extent in the caller's original string.

START and END are indices into that string, END exclusive."
  (start 0 :read-only t)
  (end 0 :read-only t))

(defstruct (span-comment
            (:include span-node)
            (:constructor %make-span-comment (start end))
            (:copier nil))
  "A comment, delimiters included.")

(defstruct (span-processing-instruction
            (:include span-node)
            (:constructor %make-span-processing-instruction (start end))
            (:copier nil))
  "A processing instruction other than the XML declaration, delimiters included.")

(defstruct (span-doctype
            (:include span-node)
            (:constructor %make-span-doctype
                (start end root-name internal-subset subset-start subset-end))
            (:copier nil))
  "A DOCTYPE declaration, `<!DOCTYPE` through `>`.

INTERNAL-SUBSET is the subset text between the brackets as the DOM reports it
(line ends normalized). SUBSET-START and SUBSET-END bound that text in the
original string. All three are NIL when there is no internal subset."
  (root-name "" :read-only t)
  (internal-subset nil :read-only t)
  (subset-start nil :read-only t)
  (subset-end nil :read-only t))

(defstruct (span-text
            (:include span-node)
            (:constructor %make-span-text
                (start end cdata-p single-cdata-p char-ref-p predefined-ref-p
                 entity-names))
            (:copier nil))
  "A maximal run of character data, CDATA sections, and references.

CDATA-P, CHAR-REF-P, and PREDEFINED-REF-P tell whether the run contains a CDATA
section, a character reference, or a reference to one of the five predefined
entities. SINGLE-CDATA-P is true when the run is exactly one CDATA section.
ENTITY-NAMES lists the other general entities referenced, once each, in order
of first appearance."
  (cdata-p nil :read-only t)
  (single-cdata-p nil :read-only t)
  (char-ref-p nil :read-only t)
  (predefined-ref-p nil :read-only t)
  (entity-names nil :read-only t))

(defstruct (span-attribute
            (:constructor %make-span-attribute
                (qname declaration-p space-start name-start name-end value-start
                 value-end quote-char))
            (:copier nil))
  "An attribute or namespace declaration inside a start tag.

QNAME is the name as written. DECLARATION-P marks `xmlns` and `xmlns:*`.
SPACE-START is where the whitespace before the name begins; NAME-START and
NAME-END bound the name. VALUE-START is the opening quote and VALUE-END the
index after the closing quote, so the value text lies between them exclusive of
the quotes. QUOTE-CHAR is the quote character used."
  (qname "" :read-only t)
  (declaration-p nil :read-only t)
  (space-start 0 :read-only t)
  (name-start 0 :read-only t)
  (name-end 0 :read-only t)
  (value-start 0 :read-only t)
  (value-end 0 :read-only t)
  (quote-char #\" :read-only t))

(defstruct (span-element
            (:include span-node)
            (:constructor %make-span-element)
            (:copier nil))
  "An element, `<` through the end of its end tag or `/>`.

QNAME is the name as written and NAME-END the index after it. ATTRIBUTES lists
SPAN-ATTRIBUTE values in source order. START-TAG-END is the index after `>` or
`/>`. CONTENT-START and CONTENT-END bound the content; for an empty element both
are the index of the `/`. END-TAG-START and END-TAG-END bound the end tag and are
NIL for an empty element. CHILDREN lists SPAN-ELEMENT, SPAN-TEXT, SPAN-COMMENT,
and SPAN-PROCESSING-INSTRUCTION values in source order."
  (qname "" :read-only t)
  (name-end 0 :read-only t)
  (attributes nil :read-only t)
  (empty-p nil :read-only t)
  (start-tag-end 0 :read-only t)
  (content-start 0 :read-only t)
  (content-end 0 :read-only t)
  (end-tag-start nil :read-only t)
  (end-tag-end nil :read-only t)
  (children nil :read-only t))

(defstruct (span-document
            (:constructor %make-span-document)
            (:copier nil))
  "The lexical tree of one document, with spans in the caller's original string.

DECLARATION is the XML declaration node or NIL. DOCTYPE is the SPAN-DOCTYPE or
NIL. CHILDREN lists, in source order, the DOCTYPE, comments, processing
instructions, and ROOT; whitespace between them is not recorded. LINE-BREAK is
the style of the first line break, :CRLF, :LF, or :CR, and :LF when the source
has none. MARKUP-ENTITY-NAMES lists the internal general entities whose
replacement text contains markup, and ATTLIST-ELEMENT-NAMES the element names
that appear in ATTLIST declarations of the internal subset."
  (declaration nil :read-only t)
  (doctype nil :read-only t)
  (children nil :read-only t)
  (root nil :read-only t)
  (line-break :lf :read-only t)
  (markup-entity-names nil :read-only t)
  (attlist-element-names nil :read-only t))

(defstruct (span-run
            (:constructor %make-span-run (start))
            (:copier nil))
  "Mutable accumulator for one text run, in normalized offsets."
  (start 0)
  (end 0)
  (token-count 0)
  (cdata-count 0)
  (char-ref-p nil)
  (predefined-ref-p nil)
  (entity-names nil))

(defstruct (span-frame
            (:constructor %make-span-frame
                (qname start name-end attributes start-tag-end))
            (:copier nil))
  "Mutable state for one open element, in original-string offsets."
  (qname "" :read-only t)
  (start 0 :read-only t)
  (name-end 0 :read-only t)
  (attributes nil :read-only t)
  (start-tag-end 0 :read-only t)
  (children nil)
  (run nil))

(defstruct (span-scanner
            (:constructor %make-span-scanner
                (tokenizer text offsets shift limits external-entity-policy))
            (:copier nil))
  "Mutable state for one scan.

TEXT is the normalized text. OFFSETS maps each normalized index, and the text
length, to an index in the original string; it is NIL when the two differ only
by the SHIFT of a skipped leading U+FEFF."
  (tokenizer nil :read-only t)
  (text "" :read-only t)
  (offsets nil :read-only t)
  (shift 0 :read-only t)
  (limits nil :read-only t)
  (external-entity-policy :reject :read-only t)
  (stack nil)
  (nodes nil)
  (declaration nil)
  (doctype nil)
  (root nil)
  (attlist-element-names nil)
  (markup-entity-names nil))

(defun %span-fail (token message)
  (error 'xml-well-formedness-error
         :message message :position (xml-token-position token)))

(defun %span-original (scanner index)
  (let ((offsets (span-scanner-offsets scanner)))
    (if offsets
        (aref offsets index)
        (+ index (span-scanner-shift scanner)))))

(defun %span-token-start (scanner token)
  (%span-original scanner (xml-position-offset (xml-token-position token))))

(defun %span-token-end (scanner token)
  (%span-original scanner (xml-token-end token)))

(defun %span-next-token (scanner)
  (next-xml-token (span-scanner-tokenizer scanner)))

(defun %span-flush-run (scanner frame)
  (let ((run (span-frame-run frame)))
    (when run
      (push (%make-span-text (%span-original scanner (span-run-start run))
                             (%span-original scanner (span-run-end run))
                             (plusp (span-run-cdata-count run))
                             (and (= (span-run-token-count run) 1)
                                  (= (span-run-cdata-count run) 1))
                             (span-run-char-ref-p run)
                             (span-run-predefined-ref-p run)
                             (reverse (span-run-entity-names run)))
            (span-frame-children frame))
      (setf (span-frame-run frame) nil))))

(defun %span-add-run-piece (frame token)
  (let ((run (or (span-frame-run frame)
                 (setf (span-frame-run frame)
                       (%make-span-run
                        (xml-position-offset (xml-token-position token)))))))
    (setf (span-run-end run) (xml-token-end token))
    (incf (span-run-token-count run))
    (case (xml-token-kind token)
      (:cdata (incf (span-run-cdata-count run)))
      (:entity-reference
       (let ((body (%xml-token-value token)))
         (cond
           ((char= (char body 0) #\#)
            (setf (span-run-char-ref-p run) t))
           ((assoc body +xml-predefined-entities+ :test #'string=)
            (setf (span-run-predefined-ref-p run) t))
           (t
            (pushnew body (span-run-entity-names run) :test #'string=))))))))

(defun %span-add-node (scanner node)
  (let ((frame (car (span-scanner-stack scanner))))
    (cond
      (frame
       (%span-flush-run scanner frame)
       (push node (span-frame-children frame)))
      (t
       (push node (span-scanner-nodes scanner))
       (when (span-element-p node)
         (setf (span-scanner-root scanner) node))))))

(defun %span-namespace-declaration-p (qname)
  (or (string= qname "xmlns")
      (and (>= (length qname) 6) (string= "xmlns:" qname :end2 6))))

(defun %span-attribute (scanner name-token value-token space-start)
  (let ((qname (%xml-token-value name-token))
        (value-offset (xml-position-offset (xml-token-position value-token))))
    (%make-span-attribute qname
                          (%span-namespace-declaration-p qname)
                          (%span-original scanner space-start)
                          (%span-token-start scanner name-token)
                          (%span-token-end scanner name-token)
                          (%span-token-start scanner value-token)
                          (%span-token-end scanner value-token)
                          (char (span-scanner-text scanner) value-offset))))

(defun %span-open-element (scanner open-token name-token attributes close-token)
  (let ((start (%span-token-start scanner open-token))
        (qname (%xml-token-value name-token))
        (name-end (%span-token-end scanner name-token))
        (start-tag-end (%span-token-end scanner close-token)))
    (if (eq (xml-token-kind close-token) :empty-element-close)
        (let ((slash (%span-token-start scanner close-token)))
          (%span-add-node scanner
                          (%make-span-element
                           :start start :end start-tag-end :qname qname
                           :name-end name-end :attributes attributes
                           :empty-p t :start-tag-end start-tag-end
                           :content-start slash :content-end slash)))
        (push (%make-span-frame qname start name-end attributes start-tag-end)
              (span-scanner-stack scanner)))))

(defun %span-scan-start-tag (scanner open-token)
  (let* ((name-token (%span-next-token scanner))
         (space-start (xml-token-end name-token))
         (attribute-name nil)
         (attributes nil))
    (loop for token = (%span-next-token scanner)
          do (ecase (xml-token-kind token)
               (:name (setf attribute-name token))
               (:equals)
               (:quoted-value
                (push (%span-attribute scanner attribute-name token space-start)
                      attributes)
                (setf space-start (xml-token-end token)))
               ((:tag-close :empty-element-close)
                (return (%span-open-element scanner open-token name-token
                                            (nreverse attributes) token)))))))

(defun %span-scan-end-tag (scanner open-token)
  (let* ((name-token (%span-next-token scanner))
         (close-token (%span-next-token scanner))
         (frame (pop (span-scanner-stack scanner))))
    (unless (and frame
                 (string= (%xml-token-value name-token) (span-frame-qname frame)))
      (%span-fail name-token "End tag does not match its start tag."))
    (%span-flush-run scanner frame)
    (let ((end-tag-start (%span-token-start scanner open-token))
          (end (%span-token-end scanner close-token)))
      (%span-add-node scanner
                      (%make-span-element
                       :start (span-frame-start frame) :end end
                       :qname (span-frame-qname frame)
                       :name-end (span-frame-name-end frame)
                       :attributes (span-frame-attributes frame)
                       :empty-p nil
                       :start-tag-end (span-frame-start-tag-end frame)
                       :content-start (span-frame-start-tag-end frame)
                       :content-end end-tag-start
                       :end-tag-start end-tag-start :end-tag-end end
                       :children (nreverse (span-frame-children frame)))))))

(defun %span-replacement-scan (text context)
  "Return whether TEXT holds markup once references resolve, and the named entities it references.

This follows `%materialize-entity-source-into`, which resolves character and
predefined references before the tokenizer sees the replacement."
  (let ((position 0)
        (references nil))
    (loop
      (let ((marker (position-if (lambda (character) (find character "<&"))
                                 text :start position)))
        (cond
          ((null marker)
           (return (values nil references)))
          ((char= (char text marker) #\<)
           (return (values t references)))
          (t
           (let* ((end (position #\; text :start marker))
                  (body (subseq text (1+ marker) end)))
             (cond
               ((char= (char body 0) #\#)
                (when (char= (%parse-character-reference body context) #\<)
                  (return (values t references))))
               ((string= body "lt")
                (return (values t references)))
               ((not (assoc body +xml-predefined-entities+ :test #'string=))
                (push body references)))
             (setf position (1+ end)))))))))

(defun %span-markup-entity-names (context)
  "Return the sorted names of internal general entities in CONTEXT that bring in markup.

An entity qualifies when its own replacement text holds markup or it references
a qualifying entity."
  (let ((referrers (make-hash-table :test #'equal))
        (marked (make-hash-table :test #'equal))
        (pending nil))
    (maphash
     (lambda (name declaration)
       (unless (xml-entity-declaration-external-p declaration)
         (multiple-value-bind (markup-p references)
             (%span-replacement-scan
              (xml-entity-declaration-replacement-text declaration) context)
           (dolist (reference references)
             (push name (gethash reference referrers)))
           (when markup-p
             (setf (gethash name marked) t)
             (push name pending)))))
     (xml-entity-context-declarations context))
    (loop while pending
          do (dolist (referrer (gethash (pop pending) referrers))
               (unless (gethash referrer marked)
                 (setf (gethash referrer marked) t)
                 (push referrer pending))))
    (sort (loop for name being the hash-keys of marked collect name)
          #'string<)))

(defun %span-scan-doctype (scanner token)
  (let ((body (%xml-token-value token))
        (body-start (+ (xml-position-offset (xml-token-position token))
                       (length "<!DOCTYPE"))))
    (multiple-value-bind (name-start name-end subset-start subset-end)
        (%doctype-body-layout body)
      (let ((subset (and subset-start (subseq body subset-start subset-end))))
        (setf (span-scanner-doctype scanner)
              (%make-span-doctype
               (%span-token-start scanner token) (%span-token-end scanner token)
               (subseq body name-start name-end) subset
               (and subset
                    (%span-original scanner (+ body-start subset-start)))
               (and subset
                    (%span-original scanner (+ body-start subset-end)))))
        (when subset
          (let ((context (parse-xml-internal-subset
                          subset
                          :resource-limits (span-scanner-limits scanner)
                          :external-entity-policy
                          (span-scanner-external-entity-policy scanner)
                          :position (xml-token-position token))))
            (setf (span-scanner-attlist-element-names scanner)
                  (xml-entity-context-attlist-element-names context)
                  (span-scanner-markup-entity-names scanner)
                  (%span-markup-entity-names context))))
        (%span-add-node scanner (span-scanner-doctype scanner))))))

(defun %span-scan-content (scanner token)
  (case (xml-token-kind token)
    ((:text :entity-reference :cdata)
     (%span-add-run-piece (car (span-scanner-stack scanner)) token))
    (:comment
     (%span-add-node scanner
                     (%make-span-comment (%span-token-start scanner token)
                                         (%span-token-end scanner token))))
    (:processing-instruction
     (%span-add-node scanner
                     (%make-span-processing-instruction
                      (%span-token-start scanner token)
                      (%span-token-end scanner token))))
    (:start-tag-open (%span-scan-start-tag scanner token))
    (:end-tag-open (%span-scan-end-tag scanner token))
    (otherwise (%span-fail token "Unexpected token in element content."))))

(defun %span-scan-top-level (scanner token)
  (case (xml-token-kind token)
    ;; Whitespace is not recorded. PARSE-XML also accepts a reference after an
    ;; empty root element, which the DOM keeps as document-level text.
    ((:text :entity-reference))
    (:comment
     (%span-add-node scanner
                     (%make-span-comment (%span-token-start scanner token)
                                         (%span-token-end scanner token))))
    (:processing-instruction
     (cond
       ((not (%parser-xml-declaration-token-p token))
        (%span-add-node scanner
                        (%make-span-processing-instruction
                         (%span-token-start scanner token)
                         (%span-token-end scanner token))))
       ((zerop (xml-position-offset (xml-token-position token)))
        (setf (span-scanner-declaration scanner)
              (%make-span-node (%span-token-start scanner token)
                               (%span-token-end scanner token))))
       (t (%span-fail token "The XML declaration must be first."))))
    (:doctype
     (when (span-scanner-doctype scanner)
       (%span-fail token "Only one DOCTYPE is allowed."))
     (%span-scan-doctype scanner token))
    (:start-tag-open
     (when (span-scanner-root scanner)
       (%span-fail token "A document has one root element."))
     (%span-scan-start-tag scanner token))
    (otherwise (%span-fail token "Unexpected token outside the root element."))))

(defun %span-offsets (source text shift)
  "Map each normalized index of TEXT, and its length, to an index in SOURCE.

Return NIL when no CRLF pair collapsed, so the map is the SHIFT alone."
  (when (search #(#\Return #\Linefeed) source)
    (let ((offsets (make-array (1+ (length text)) :element-type 'fixnum))
          (index shift))
      (dotimes (position (length text))
        (setf (aref offsets position) index)
        (incf index (if (and (char= (char source index) #\Return)
                             (< (1+ index) (length source))
                             (char= (char source (1+ index)) #\Linefeed))
                        2
                        1)))
      (setf (aref offsets (length text)) index)
      offsets)))

(defun %span-line-break (source)
  (let ((index (position-if (lambda (character)
                              (or (char= character #\Return)
                                  (char= character #\Linefeed)))
                            source)))
    (cond
      ((or (null index) (char= (char source index) #\Linefeed)) :lf)
      ((and (< (1+ index) (length source))
            (char= (char source (1+ index)) #\Linefeed))
       :crlf)
      (t :cr))))

(defun scan-xml-spans (source &key
                                (resource-limits *default-xml-resource-limits*)
                                (external-entity-policy :reject))
  "Return the SPAN-DOCUMENT of the well-formed XML string SOURCE.

Spans index SOURCE itself. A leading U+FEFF is skipped and every CRLF or CR is
read as the single line feed the parser sees. The scanner relies on PARSE-XML,
run with the same options, for well-formedness."
  (unless (stringp source)
    (error 'xml-input-error :message "XML span SOURCE must be a string."))
  (let* ((shift (if (and (plusp (length source))
                         (char= (char source 0) (code-char #xFEFF)))
                    1
                    0))
         (text (xml-input-text
                (make-xml-input (if (plusp shift) (subseq source shift) source)
                                :limits resource-limits)))
         (scanner (%make-span-scanner
                   (make-xml-tokenizer text :limits resource-limits)
                   text (%span-offsets source text shift) shift
                   resource-limits external-entity-policy))
         (eof (loop for token = (%span-next-token scanner)
                    until (eq (xml-token-kind token) :eof)
                    do (if (span-scanner-stack scanner)
                           (%span-scan-content scanner token)
                           (%span-scan-top-level scanner token))
                    finally (return token))))
    (when (span-scanner-stack scanner)
      (%span-fail eof "The document ended in an element."))
    (unless (span-scanner-root scanner)
      (%span-fail eof "A document needs a root element."))
    (%make-span-document
     :declaration (span-scanner-declaration scanner)
     :doctype (span-scanner-doctype scanner)
     :children (nreverse (span-scanner-nodes scanner))
     :root (span-scanner-root scanner)
     :line-break (%span-line-break source)
     :markup-entity-names (span-scanner-markup-entity-names scanner)
     :attlist-element-names (span-scanner-attlist-element-names scanner))))
