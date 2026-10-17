(in-package #:cl-xml-kit)

(define-immutable-record xml-input
    (:constructor %make-xml-input
     :conc-name %xml-input-
     :predicate xml-input-p
     :predicate-doc "Return true when VALUE is an XML input."
     :structure-doc "An immutable, normalized XML input view used by the tokenizer.

The TEXT slot is normalized XML character data. LINE-STARTS stores normalized
character offsets, so XML-INPUT-POSITION-AT can return positions without
retaining one position object per character.")
  ((text "" xml-input-text
         "Return a fresh copy of INPUT's normalized string for the tokenizer." :copy-seq)
   (source nil xml-input-source "Return INPUT's source name, or NIL, as a fresh string."
           :copy-seq)
   (line-starts #(0) nil nil :identity)
   (limits nil xml-input-limits "Return INPUT's immutable resource limits." :identity)))

(defun %input-position (source offset line column)
  (make-xml-position :source source :offset offset :line line :column column))

(defun %signal-input-limit (kind value limit source offset line column)
  (error 'xml-resource-limit-exceeded
         :kind kind
         :value value
         :limit limit
         :source source
         :position (%input-position source offset line column)
         :message (xml-message :input-limit-exceeded kind value limit)))

(defun %signal-unsupported-encoding (encoding reason message source)
  (error 'xml-unsupported-encoding
         :encoding encoding
         :reason reason
         :source source
         :position (%input-position source 0 1 1)
         :message message))

(defun %normalize-characters (text stream limits source)
  "Return the characters of TEXT, or of STREAM when TEXT is NIL, with CRLF and CR as LF.

The normalized length is checked against MAX-DOCUMENT-CHARACTERS."
  (let ((limit (xml-resource-limits-max-document-characters limits))
        (index 0)
        (end (if text (length text) 0))
        (offset 0)
        (line 1)
        (column 1)
        (pending nil))
    (flet ((next-character ()
             (cond
               (pending (shiftf pending nil))
               (text (when (< index end)
                       (prog1 (char text index) (incf index))))
               (t (read-char stream nil nil)))))
      (declare (inline next-character))
      (with-output-to-string (output)
        (loop for raw = (next-character)
              while raw
              do (let ((character raw))
                   (when (char= raw #\Return)
                     (let ((following (next-character)))
                       (unless (or (null following) (char= following #\Linefeed))
                         (setf pending following)))
                     (setf character #\Linefeed))
                   (when (>= offset limit)
                     (%signal-input-limit :max-document-characters (1+ offset) limit
                                          source offset line column))
                   (write-char character output)
                   (incf offset)
                   (if (char= character #\Linefeed)
                       (setf line (1+ line) column 1)
                       (incf column))))))))

(defun %line-starts (text)
  (let ((starts (make-array 1 :adjustable t :fill-pointer 1)))
    (setf (aref starts 0) 0)
    (loop for index from 0 below (length text)
          when (char= (char text index) #\Linefeed)
            do (vector-push-extend (1+ index) starts))
    starts))

(defun %declared-encoding (text)
  "Return the EncodingDecl value of the XML declaration that starts TEXT, or NIL.

Only a declaration with VersionInfo 1.0 directly followed by an EncodingDecl
yields a value; anything else returns NIL and is left to the parser."
  (let ((index 0)
        (end (or (search "?>" text) 0)))
    (labels ((skip-space ()
               (let ((start index))
                 (loop while (and (< index end) (xml-space-char-p (char text index)))
                       do (incf index))
                 (> index start)))
             (literal (string)
               (let ((after (+ index (length string))))
                 (when (and (<= after end) (string= string text :start2 index :end2 after))
                   (setf index after))))
             (quoted-value ()
               (let* ((quote (and (< index end) (char text index)))
                      (close (and (member quote '(#\" #\'))
                                  (position quote text :start (1+ index) :end end))))
                 (when close
                   (prog1 (subseq text (1+ index) close)
                     (setf index (1+ close))))))
             (pseudo-attribute (name)
               (and (literal name)
                    (progn (skip-space) (literal "="))
                    (progn (skip-space) (quoted-value)))))
      (and (literal "<?xml")
           (skip-space)
           (equal (pseudo-attribute "version") "1.0")
           (skip-space)
           (pseudo-attribute "encoding")))))

(defun %check-declared-encoding (text encoding source)
  "Signal XML-UNSUPPORTED-ENCODING when TEXT declares an encoding other than ENCODING.

ENCODING is :UTF-8 for character input and UTF-8 octets, or the UTF-16 byte
order a BOM selected."
  (let ((declared (%declared-encoding text)))
    (cond
      ((null declared))
      ((eq encoding :utf-8)
       (unless (string-equal declared "UTF-8")
         (%signal-unsupported-encoding declared :declaration-not-utf-8
                                       (xml-message :declaration-not-utf-8) source)))
      ((not (string-equal declared "UTF-16"))
       (%signal-unsupported-encoding
        declared :declaration-contradicts-bom
        (xml-message :declaration-contradicts-bom declared)
        source)))))

(defun %make-input (text encoding source limits)
  (%check-declared-encoding text encoding source)
  (%make-xml-input text (and source (copy-seq source))
                   (%line-starts text) limits))

(defun %position-after-decoded-prefix (text source)
  "Return the normalized position immediately after raw decoded TEXT."
  (let ((offset 0)
        (line 1)
        (column 1)
        (index 0)
        (length (length text)))
    (loop while (< index length)
          do (let ((character (char text index)))
               (incf index)
               (incf offset)
               (cond
                 ((or (char= character #\Return) (char= character #\Linefeed))
                  (when (and (char= character #\Return)
                             (< index length)
                             (char= (char text index) #\Linefeed))
                    (incf index))
                  (setf line (1+ line) column 1))
                 (t (incf column)))))
    (%input-position source offset line column)))

(defun %codec-failing-octet (condition octets position)
  (cond
    ((typep condition 'cl-codec-kit:invalid-leading-byte)
     (cl-codec-kit:invalid-leading-byte-octet condition))
    ((typep condition 'cl-codec-kit:invalid-continuation-byte)
     (cl-codec-kit:invalid-continuation-byte-octet condition))
    (t (aref octets position))))

(defun %signal-decode-error (condition octets start encoding source)
  (let* ((octet-position (cl-codec-kit:decode-error-position condition))
         (prefix (cl-codec-kit:octets-to-string
                  octets :start start :end octet-position :encoding encoding)))
    (error 'xml-decode-error
           :octet (%codec-failing-octet condition octets octet-position)
           :encoding encoding
           :source source
           :position (%position-after-decoded-prefix prefix source)
           :message (if (eq encoding :utf-8)
                        (xml-message :invalid-utf-8 octet-position)
                        (xml-message :invalid-utf-16 octet-position)))))

(defun %octet-encoding (octets source)
  "Return the encoding of OCTETS and the length of its byte order mark.

UTF-32 and UTF-16 without a byte order mark signal XML-UNSUPPORTED-ENCODING.
Input without a BOM whose leading octets contain a zero cannot be UTF-8 XML,
because U+0000 is not an XML character."
  (multiple-value-bind (encoding bom-length)
      (cl-codec-kit:detect-unicode-encoding octets)
    (case encoding
      ((:utf-32be :utf-32le)
       (%signal-unsupported-encoding :utf-32 :utf-32-not-supported
                                     (xml-message :encoding-not-supported "UTF-32")
                                     source))
      ((:utf-16be :utf-16le)
       (when (zerop bom-length)
         (%signal-unsupported-encoding
          :utf-16 :utf-16-bom-required
          (xml-message :utf-16-needs-bom) source))))
    (values encoding bom-length)))

(defun %decode-octets (octets source)
  "Return OCTETS decoded without their byte order mark, and the encoding used."
  (multiple-value-bind (encoding start) (%octet-encoding octets source)
    (handler-case
        (values (cl-codec-kit:octets-to-string octets :start start :encoding encoding)
                encoding)
      (cl-codec-kit:decode-error (condition)
        (%signal-decode-error condition octets start encoding source)))))

(defun %make-octet-input (octets source limits)
  (let ((octet-limit (xml-resource-limits-max-input-octets limits)))
    (when (> (length octets) octet-limit)
      (%signal-input-limit
       :max-input-octets (length octets) octet-limit source 0 1 1))
    (multiple-value-bind (decoded encoding) (%decode-octets octets source)
      (%make-input (%normalize-characters decoded nil limits source)
                   encoding source limits))))

(defun make-xml-input (input &key source (limits *default-xml-resource-limits*))
  "Return a normalized XML-INPUT for a string, character stream, or octet vector.

CRLF and CR are converted to LF. Octet input is UTF-8, with or without a BOM,
or UTF-16 with a BOM selecting its byte order; the BOM is consumed. UTF-32 and
UTF-16 without a BOM signal XML-UNSUPPORTED-ENCODING, as does an XML
declaration whose encoding differs from the input's: UTF-16 for BOM-marked
UTF-16 octets and UTF-8 otherwise. INPUT octets are checked against
MAX-INPUT-OCTETS before decoding, and normalized characters are checked
against MAX-DOCUMENT-CHARACTERS with an inclusive boundary.

The tokenizer consumes this result as follows:
`(make-xml-tokenizer (xml-input-text input) :source
  (xml-input-source input) :limits limits)`. The returned tokenizer therefore
receives the normalized string, not the original stream or octet vector."
  (when (and source (not (stringp source)))
    (xml-error 'xml-input-error :label-must-be-string-or-nil
               :format-arguments ("XML input SOURCE")))
  (check-argument limits xml-resource-limits-p xml-input-error
                  :label-must-be-resource-limits
                  :format-arguments ("XML input LIMITS"))
  (cond
    ((stringp input)
     (%make-input (%normalize-characters input nil limits source)
                  :utf-8 source limits))
    ((typep input '(vector (unsigned-byte 8)))
     (%make-octet-input input source limits))
    ((and (streamp input)
          (input-stream-p input)
          (nth-value 0 (subtypep (stream-element-type input) 'character)))
     (%make-input (%normalize-characters nil input limits source)
                  :utf-8 source limits))
    (t
     (xml-error 'xml-input-error :input-type-unsupported))))

(defun xml-input-position-at (input offset)
  "Return the normalized XML position at character OFFSET.

OFFSET is zero-based and may equal the text length to request the end
position. The returned line and column are one-based."
  (unless (and (integerp offset)
               (<= 0 offset)
               (<= offset (length (%xml-input-text input))))
    (xml-error 'xml-input-error :input-offset-out-of-range))
  (let* ((starts (%xml-input-line-starts input))
         (low 0)
         (high (1- (length starts))))
    (loop while (< low high)
          for middle = (ceiling (+ low high) 2)
          do (if (<= (aref starts middle) offset)
                 (setf low middle)
                 (setf high (1- middle))))
    (%input-position (%xml-input-source input)
                     offset
                     (1+ low)
                     (1+ (- offset (aref starts low))))))
