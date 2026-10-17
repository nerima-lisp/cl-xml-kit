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

(defun %normalize-from-reader (reader limits source)
  (let ((limit (xml-resource-limits-max-document-characters limits))
        (text nil))
    (setf text
          (with-output-to-string (output)
            (let ((offset 0)
                  (line 1)
                  (column 1)
                  (pending nil)
                  (pending-p nil))
              (labels ((next-character ()
                         (if pending-p
                             (prog1 pending (setf pending-p nil))
                             (funcall reader)))
                       (emit (character)
                         (when (>= offset limit)
                           (%signal-input-limit
                            :max-document-characters (1+ offset) limit
                            source offset line column))
                         (write-char character output)
                         (incf offset)
                         (if (char= character #\Linefeed)
                             (progn
                               (incf line)
                               (setf column 1))
                             (incf column))))
                (loop for character = (next-character)
                      while character
                      do (if (char= character #\Return)
                             (progn
                               (let ((next (next-character)))
                                 (when (and next
                                            (not (char= next #\Linefeed)))
                                   (setf pending next pending-p t)))
                               (emit #\Linefeed))
                             (emit character)))))))
    text))

(defun %line-starts (text)
  (let ((starts (make-array 1 :adjustable t :fill-pointer 1)))
    (setf (aref starts 0) 0)
    (loop for index from 0 below (length text)
          when (char= (char text index) #\Linefeed)
            do (vector-push-extend (1+ index) starts))
    starts))

(defun %make-input (text source limits)
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
          for character = (char text index)
          do (incf index)
             (when (char= character #\Return)
               (when (and (< index length)
                          (char= (char text index) #\Linefeed))
                 (incf index))
               (setf character #\Linefeed))
             (incf offset)
             (if (char= character #\Linefeed)
                 (progn
                   (incf line)
                   (setf column 1))
                 (incf column)))
    (%input-position source offset line column)))

(defun %codec-failing-octet (condition octets position)
  (cond
    ((typep condition 'cl-codec-kit:invalid-leading-byte)
     (cl-codec-kit:invalid-leading-byte-octet condition))
    ((typep condition 'cl-codec-kit:invalid-continuation-byte)
     (cl-codec-kit:invalid-continuation-byte-octet condition))
    ((and (integerp position) (<= 0 position) (< position (length octets)))
     (aref octets position))))

(defun %signal-decode-error (condition octets start source)
  (let* ((octet-position (cl-codec-kit:decode-error-position condition))
         (prefix (cl-codec-kit:octets-to-string
                  octets :start start :end octet-position :encoding :utf-8))
         (position (%position-after-decoded-prefix prefix source)))
    (error 'xml-decode-error
           :octet (%codec-failing-octet condition octets octet-position)
           :encoding :utf-8
           :source source
           :position position
           :message (xml-message :invalid-utf-8 octet-position))))

(defun %unsupported-encoding-bom (octets)
  (cond
    ((and (>= (length octets) 4)
          (or (and (= (aref octets 0) 0)
                   (= (aref octets 1) 0)
                   (= (aref octets 2) #xFE)
                   (= (aref octets 3) #xFF))
              (and (= (aref octets 0) #xFF)
                   (= (aref octets 1) #xFE)
                   (= (aref octets 2) 0)
                   (= (aref octets 3) 0))))
     (values :utf-32 :utf-32-not-supported))
    ((and (>= (length octets) 2)
          (or (and (= (aref octets 0) #xFE)
                   (= (aref octets 1) #xFF))
              (and (= (aref octets 0) #xFF)
                   (= (aref octets 1) #xFE))))
     (values :utf-16 :utf-16-not-supported))))

(defun %make-octet-input (octets source limits)
  (let ((octet-limit (xml-resource-limits-max-input-octets limits)))
    (when (> (length octets) octet-limit)
      (%signal-input-limit
       :max-input-octets (length octets) octet-limit source 0 1 1))
    (multiple-value-bind (encoding reason) (%unsupported-encoding-bom octets)
      (if encoding
          (error 'xml-unsupported-encoding
                 :encoding encoding
                 :reason reason
                 :source source
                 :position (%input-position source 0 1 1)
                 :message (xml-message :encoding-not-supported
                                       (string-upcase (symbol-name encoding))))
          (progn
            (let ((start (if (and (>= (length octets) 3)
                                   (= (aref octets 0) #xEF)
                                   (= (aref octets 1) #xBB)
                                   (= (aref octets 2) #xBF))
                             3
                             0)))
              (handler-case
                  (let ((decoded (cl-codec-kit:octets-to-string
                                   octets :start start :encoding :utf-8)))
                    (%make-input
                     (%normalize-from-reader
                      (let ((index 0))
                        (lambda ()
                          (when (< index (length decoded))
                            (prog1 (char decoded index) (incf index)))))
                      limits source)
                     source limits))
                (cl-codec-kit:decode-error (condition)
                  (%signal-decode-error condition octets start source)))))))))

(defun make-xml-input (input &key source
                                   (limits nil limits-supplied-p)
                                   (resource-limits nil resource-limits-supplied-p))
  "Return a normalized XML-INPUT for a string, character stream, or UTF-8 octets.

CRLF and CR are converted to LF. A UTF-8 BOM is consumed from octet input;
UTF-16 or UTF-32 BOM input signals XML-UNSUPPORTED-ENCODING. INPUT octets are checked
against MAX-INPUT-OCTETS before decoding, and normalized characters are
checked against MAX-DOCUMENT-CHARACTERS with an inclusive boundary.

The tokenizer consumes this result as follows:
`(make-xml-tokenizer (xml-input-text input) :source
  (xml-input-source input) :limits limits)`. The returned tokenizer therefore
receives the normalized string, not the original stream or octet vector."
  (when (and source (not (stringp source)))
    (xml-error 'xml-input-error :label-must-be-string-or-nil
               :format-arguments ("XML input SOURCE")))
  (when (and limits-supplied-p resource-limits-supplied-p)
    (xml-error 'xml-input-error :limits-specified-twice))
  (let ((selected-limits
          (cond
            (resource-limits-supplied-p resource-limits)
            (limits-supplied-p limits)
            (t *default-xml-resource-limits*))))
    (unless (xml-resource-limits-p selected-limits)
      (xml-error 'xml-input-error :label-must-be-resource-limits
                 :format-arguments ("XML input LIMITS")))
    (cond
      ((stringp input)
       (%make-input
        (%normalize-from-reader
         (let ((index 0))
           (lambda ()
             (when (< index (length input))
               (prog1 (char input index) (incf index)))))
         selected-limits source)
        source selected-limits))
      ((typep input '(vector (unsigned-byte 8)))
       (%make-octet-input input source selected-limits))
      ((and (streamp input)
            (input-stream-p input)
            (nth-value 0 (subtypep (stream-element-type input) 'character)))
       (%make-input
        (%normalize-from-reader
         (lambda () (read-char input nil nil)) selected-limits source)
        source selected-limits))
      (t
       (xml-error 'xml-input-error :input-type-unsupported)))))

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

