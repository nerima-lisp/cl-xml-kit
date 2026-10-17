(in-package #:cl-xml-kit/test)

(describe "line-end normalization"
  (it-each (((#x61 #xD #xA #x62 #xD #x63) (#x61 #xA #x62 #xA #x63))
            ((#xD #xD #xA) (#xA #xA)) ((#xD) (#xA)) ((#xA #xD) (#xA #xA))
            ((#xD #x20 #xD) (#xA #x20 #xA)) ((#x61) (#x61)) (() ()))
      "normalizes the codes ~S to ~S from a string and from a character stream"
      (codes expected)
    (let ((text (map 'string #'code-char codes))
          (normalized (map 'string #'code-char expected)))
      (expect (cl-xml-kit::xml-input-text (cl-xml-kit::make-xml-input text))
              :to-equal normalized)
      (expect (with-input-from-string (stream text)
                (cl-xml-kit::xml-input-text (cl-xml-kit::make-xml-input stream)))
              :to-equal normalized)))

  ;; The alphabet holds both line-end characters next to three ordinary ones so that
  ;; CR, LF, CRLF, and CRCRLF runs all occur within a few characters.
  (it-property "normalizes line ends and numbers lines as XML 1.0 section 2.11 requires"
      ((text (gen-string :max-length 20
                         :alphabet (coerce '(#\a #\< #\Space #\Return #\Newline) 'string))))
    (let* ((input (cl-xml-kit::make-xml-input text))
           (normalized (normalized-line-ends text)))
      (expect (cl-xml-kit::xml-input-text input) :to-equal normalized)
      (expect (loop for offset from 0 to (length normalized)
                    for position = (cl-xml-kit::xml-input-position-at input offset)
                    for line-start = (let ((newline (position #\Newline normalized
                                                              :end offset :from-end t)))
                                       (if newline (1+ newline) 0))
                    unless (and (= (xml-position-line position)
                                   (1+ (count #\Newline normalized :end offset)))
                                (= (xml-position-column position) (1+ (- offset line-start))))
                      collect offset)
              :to-equal nil))))

(describe "positions"
  (it-each ((0 1 1) (1 1 2) (2 2 1) (3 2 2) (4 3 1) (5 3 2))
      "places normalized offset ~D at line ~D, column ~D"
      (offset line column)
    (let* ((input (cl-xml-kit::make-xml-input (map 'string #'code-char
                                                   '(#x61 #xD #xA #x62 #xD #x63))
                                              :source "memory.xml"))
           (position (cl-xml-kit::xml-input-position-at input offset)))
      (expect (list (xml-position-source position) (xml-position-offset position)
                    (xml-position-line position) (xml-position-column position))
              :to-equal (list "memory.xml" offset line column))))

  (it "names the stream source in positions of stream input"
    (with-input-from-string (stream "first")
      (expect (xml-position-source
               (cl-xml-kit::xml-input-position-at
                (cl-xml-kit::make-xml-input stream :source "stream.xml") 3))
              :to-equal "stream.xml")))

  (it-each ((-1) (2) (1.5) (nil))
      "refuses the position offset ~S outside the one-character text"
      (offset)
    (expect (lambda () (cl-xml-kit::xml-input-position-at
                        (cl-xml-kit::make-xml-input "x") offset))
            :to-signal-xml
            '(xml-input-error
              (xml-condition-message
               "XML input position offset must be between zero and the text length.")))))

(describe "input records"
  (it "keeps copies of its text and source"
    (let* ((source (copy-seq "memory.xml"))
           (input (cl-xml-kit::make-xml-input "<a/>" :source source)))
      (setf (char source 0) #\X)
      (setf (char (cl-xml-kit::xml-input-text input) 0) #\X
            (char (cl-xml-kit::xml-input-source input) 0) #\X)
      (expect (list (cl-xml-kit::xml-input-p input) (cl-xml-kit::xml-input-text input)
                    (cl-xml-kit::xml-input-source input))
              :to-equal '(t "<a/>" "memory.xml"))))

  (it "defaults to no source and the default limits"
    (let ((input (cl-xml-kit::make-xml-input "x")))
      (expect (list (cl-xml-kit::xml-input-source input)
                    (eq (cl-xml-kit::xml-input-limits input) *default-xml-resource-limits*))
              :to-equal '(nil t))))

  (it "hands its normalized text, source, and limits to the tokenizer"
    (let* ((limits (make-xml-resource-limits :max-name-length 2))
           (input (cl-xml-kit::make-xml-input (map 'string #'code-char '(#x61 #xD #x62))
                                              :source "tokenizer.xml" :limits limits))
           (tokenizer (cl-xml-kit::make-xml-tokenizer
                       (cl-xml-kit::xml-input-text input)
                       :source (cl-xml-kit::xml-input-source input)
                       :limits (cl-xml-kit::xml-input-limits input))))
      (expect (list (cl-xml-kit::xml-tokenizer-input tokenizer)
                    (cl-xml-kit::xml-tokenizer-source tokenizer)
                    (eq (cl-xml-kit::xml-input-limits input) limits))
              :to-equal (list (format nil "a~%b") "tokenizer.xml" t))))

  (it-each (((42) "XML input must be a string, character input stream, or octet vector.")
            ((#(65 66)) "XML input must be a string, character input stream, or octet vector.")
            (("x" :source 1) "XML input SOURCE must be a string or NIL.")
            (("x" :limits nil) "XML input LIMITS must be XML-RESOURCE-LIMITS."))
      "refuses the input arguments ~S"
      (arguments message)
    (expect (lambda () (apply #'cl-xml-kit::make-xml-input arguments))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message))))

  (it "refuses an output stream as input"
    (expect (lambda () (cl-xml-kit::make-xml-input (make-string-output-stream)))
            :to-signal-xml
            '(xml-input-error
              (xml-condition-message
               "XML input must be a string, character input stream, or octet vector.")))))

(describe "input limits"
  (it-each (("ABC" 3) ("ABCD" 4))
      "accepts the ~S octets at the inclusive octet limit ~D"
      (text limit)
    (expect (cl-xml-kit::xml-input-text
             (cl-xml-kit::make-xml-input (sb-ext:string-to-octets text :external-format :utf-8)
                                         :limits (make-xml-resource-limits
                                                  :max-input-octets limit)))
            :to-equal text))

  (it "refuses octets one beyond the octet limit before decoding them"
    (expect (lambda () (cl-xml-kit::make-xml-input (octet-vector '(#x41 #x42 #xFF))
                                                   :source "big.xml"
                                                   :limits (make-xml-resource-limits
                                                            :max-input-octets 2)))
            :to-signal-xml '(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind :max-input-octets)
                             (xml-resource-limit-exceeded-value 3)
                             (xml-resource-limit-exceeded-limit 2)
                             (xml-condition-source "big.xml")
                             (xml-condition-message
                              "XML input limit :MAX-INPUT-OCTETS exceeded: 3 is greater than 2."))))

  (it-each (((#x61 #xD #xA #x62) 3) ((#x61 #x62 #x63) 3) (() 1))
      "accepts the normalized codes ~S at the inclusive character limit ~D"
      (codes limit)
    (let ((text (map 'string #'code-char codes)))
      (expect (cl-xml-kit::xml-input-text
               (cl-xml-kit::make-xml-input text :limits (make-xml-resource-limits
                                                         :max-document-characters limit)))
              :to-equal (normalized-line-ends text))))

  (it "counts the characters of stream input against the character limit"
    (expect (lambda ()
              (with-input-from-string (stream "abcd")
                (cl-xml-kit::make-xml-input stream :limits (make-xml-resource-limits
                                                            :max-document-characters 3))))
            :to-signal-xml '(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind :max-document-characters)
                             (xml-resource-limit-exceeded-value 4)
                             (xml-resource-limit-exceeded-limit 3))))

  (it "places the character-limit failure at the first character beyond the limit"
    (expect (lambda ()
              (cl-xml-kit::make-xml-input (format nil "ab~%cd")
                                          :limits (make-xml-resource-limits
                                                   :max-document-characters 4)))
            :to-signal-xml '(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-value 5)
                             (condition-place (nil 4 2 2))))))
