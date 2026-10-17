;;;; src/spans-api.lisp

(in-package #:cl-xml-kit)

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
  (let ((index (position-if #'%line-break-char-p source)))
    (cond
      ((or (null index) (char= (char source index) #\Linefeed)) :lf)
      ((and (< (1+ index) (length source))
            (char= (char source (1+ index)) #\Linefeed))
       :crlf)
      (t :cr))))

(defun %strip-bom (string)
  (if (and (plusp (length string)) (char= (char string 0) (code-char #xFEFF)))
      (subseq string 1)
      string))

(defun %source-parser (source resource-limits external-entity-policy)
  "Return a parser over SOURCE and the span scanner that observes its tokens.

The parser reads SOURCE without a leading U+FEFF, which it would otherwise
reject; the spans index SOURCE as given."
  (let* ((stripped (%strip-bom source))
         (shift (- (length source) (length stripped)))
         (parser (make-xml-parser stripped
                                  :resource-limits resource-limits
                                  :external-entity-policy external-entity-policy))
         (text (xml-parser-input parser))
         (scanner (%make-span-scanner text (%span-offsets source text shift) shift)))
    (setf (xml-parser-token-observer parser)
          (lambda (token) (%span-observe scanner token)))
    (values parser scanner)))

(defun %span-document (scanner parser source)
  "Return the SPAN-DOCUMENT that SCANNER recorded while PARSER read SOURCE to its end.

The internal subset, when there is one, is read from the entity context PARSER
built from it."
  (let* ((doctype (span-scanner-doctype scanner))
         (context (and doctype (span-doctype-internal-subset doctype)
                       (xml-parser-entities parser))))
    (%make-span-document
     :declaration (span-scanner-declaration scanner)
     :doctype doctype
     :children (nreverse (span-scanner-nodes scanner))
     :root (span-scanner-root scanner)
     :line-break (%span-line-break source)
     :markup-entity-names (and context (%span-markup-entity-names context))
     :attlist-element-names (and context
                                 (xml-entity-context-attlist-element-names context)))))

(defun scan-xml-spans (source &key
                                (resource-limits *default-xml-resource-limits*)
                                (external-entity-policy :reject))
  "Return the SPAN-DOCUMENT of the XML string SOURCE.

Spans index SOURCE itself. A leading U+FEFF is skipped and every CRLF or CR is
read as the single line feed the parser sees. The spans come from the tokens of
one parse, run with these options, which also checks well-formedness."
  (check-argument source stringp xml-input-error :label-must-be-string
                  :format-arguments ("XML span SOURCE"))
  (multiple-value-bind (parser scanner)
      (%source-parser source resource-limits external-entity-policy)
    (%parser-resume parser #'%continue-event)
    (%span-document scanner parser source)))
