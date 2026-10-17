(in-package #:cl-xml-kit)

(define-char-classes +xml-character-class-specs+)

(declaim (inline xml-space-p xml-ncname-p))

(defun xml-space-p (value)
  "Return true when VALUE is a non-empty XML S production [3]."
  (declare (type simple-string value))
  (let ((length (length value)))
    (declare (type fixnum length))
    (and (plusp length)
         (loop for index fixnum below length
               always (xml-space-char-p (schar value index))))))

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
  (cond
    ((char<= #\0 character #\9) (- (char-code character) (char-code #\0)))
    ((= radix 10) nil)
    ((char<= #\A character #\F) (+ 10 (- (char-code character) (char-code #\A))))
    ((char<= #\a character #\f) (+ 10 (- (char-code character) (char-code #\a))))))
