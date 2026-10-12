(in-package #:cl-xml-kit)

(declaim (inline xml-char-p
                 xml-space-char-p
                 xml-space-p
                 xml-name-start-char-p
                 xml-name-char-p
                 xml-pubid-char-p
                 xml-ncname-start-char-p
                 xml-ncname-char-p
                 xml-ncname-p))

(defun xml-char-p (character)
  "Return true when CHARACTER matches XML 1.0 Char [2]."
  (declare (type character character))
  (let ((code (char-code character)))
    (declare (type fixnum code))
    (or (= code #x9)
        (= code #xA)
        (= code #xD)
        (<= #x20 code #xD7FF)
        (<= #xE000 code #xFFFD)
        (<= #x10000 code #x10FFFF))))

(defun xml-space-char-p (character)
  "Return true when CHARACTER is one XML S [3] whitespace character."
  (declare (type character character))
  (let ((code (char-code character)))
    (declare (type fixnum code))
    (or (= code #x20)
        (= code #x9)
        (= code #xD)
        (= code #xA))))

(defun xml-space-p (value)
  "Return true when VALUE is a non-empty XML S production [3]."
  (declare (type simple-string value))
  (let ((length (length value)))
    (declare (type fixnum length))
    (and (plusp length)
         (loop for index fixnum below length
               always (xml-space-char-p (schar value index))))))

(defun xml-name-start-char-p (character)
  "Return true when CHARACTER matches XML NameStartChar [4]."
  (declare (type character character))
  (let ((code (char-code character)))
    (declare (type fixnum code))
    (or (char= character #\:)
        (char= character #\_)
        (<= #x41 code #x5A)
        (<= #x61 code #x7A)
        (<= #xC0 code #xD6)
        (<= #xD8 code #xF6)
        (<= #xF8 code #x2FF)
        (<= #x370 code #x37D)
        (<= #x37F code #x1FFF)
        (<= #x200C code #x200D)
        (<= #x2070 code #x218F)
        (<= #x2C00 code #x2FEF)
        (<= #x3001 code #xD7FF)
        (<= #xF900 code #xFDCF)
        (<= #xFDF0 code #xFFFD)
        (<= #x10000 code #xEFFFF))))

(defun xml-name-char-p (character)
  "Return true when CHARACTER matches XML NameChar [4a]."
  (declare (type character character))
  (let ((code (char-code character)))
    (declare (type fixnum code))
    (or (xml-name-start-char-p character)
        (char= character #\-)
        (char= character #\.)
        (<= #x30 code #x39)
        (= code #xB7)
        (<= #x300 code #x36F)
        (<= #x203F code #x2040))))

(defun xml-pubid-char-p (character)
  "Return true when CHARACTER matches XML PubidChar [13]."
  (declare (type character character))
  (let ((code (char-code character)))
    (declare (type fixnum code))
    (or (= code #x20)
        (= code #xD)
        (= code #xA)
        (<= #x41 code #x5A)
        (<= #x61 code #x7A)
        (<= #x30 code #x39)
        (char= character #\-)
        (char= character #\')
        (char= character #\()
        (char= character #\))
        (char= character #\+)
        (char= character #\,)
        (char= character #\.)
        (char= character #\/)
        (char= character #\:)
        (char= character #\=)
        (char= character #\?)
        (char= character #\;)
        (char= character #\!)
        (char= character #\*)
        (char= character #\#)
        (char= character #\@)
        (char= character #\$)
        (char= character #\_)
        (char= character #\%))))

(defun xml-ncname-start-char-p (character)
  "Return true when CHARACTER is an XML Namespaces NCName start character."
  (declare (type character character))
  (and (char/= character #\:)
       (xml-name-start-char-p character)))

(defun xml-ncname-char-p (character)
  "Return true when CHARACTER is an XML Namespaces NCName character."
  (declare (type character character))
  (let ((code (char-code character)))
    (declare (type fixnum code))
    (or (xml-ncname-start-char-p character)
        (char= character #\-)
        (char= character #\.)
        (<= #x30 code #x39)
        (= code #xB7)
        (<= #x300 code #x36F)
        (<= #x203F code #x2040))))

(defun xml-ncname-p (value)
  "Return true when VALUE is a valid XML Namespaces NCName."
  (declare (type simple-string value))
  (let ((length (length value)))
    (declare (type fixnum length))
    (and (plusp length)
         (xml-ncname-start-char-p (schar value 0))
         (loop for index fixnum from 1 below length
               always (xml-ncname-char-p (schar value index))))))

(defun xml-digit-weight (character radix)
  "Return the value of CHARACTER as an ASCII digit in RADIX 10 or 16, or NIL.

XML CharRef [66] admits only [0-9] and [0-9a-fA-F]. DIGIT-CHAR-P also accepts
digits of other scripts, so it cannot validate a reference."
  (declare (type character character)
           (type (member 10 16) radix))
  (let ((code (char-code character)))
    (declare (type fixnum code))
    (cond
      ((<= #x30 code #x39) (- code #x30))
      ((= radix 10) nil)
      ((<= #x41 code #x46) (- code #x37))
      ((<= #x61 code #x66) (- code #x57)))))
