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
  (check-argument source stringp xml-input-error :label-must-be-string
                  :format-arguments ("XML span SOURCE"))
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
      (%span-fail eof (xml-message :document-ended-in-element)))
    (unless (span-scanner-root scanner)
      (%span-fail eof (xml-message :root-required)))
    (%make-span-document
     :declaration (span-scanner-declaration scanner)
     :doctype (span-scanner-doctype scanner)
     :children (nreverse (span-scanner-nodes scanner))
     :root (span-scanner-root scanner)
     :line-break (%span-line-break source)
     :markup-entity-names (span-scanner-markup-entity-names scanner)
     :attlist-element-names (span-scanner-attlist-element-names scanner))))
