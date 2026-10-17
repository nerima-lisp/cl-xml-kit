(in-package #:cl-xml-kit/test)

(defun octet-vector (octets)
  "Return a fresh (UNSIGNED-BYTE 8) vector holding the list OCTETS.
Signals a type error when an element is not an octet, so a malformed row
fails instead of being truncated."
  (make-array (length octets) :element-type '(unsigned-byte 8) :initial-contents octets))

(defun utf-16-octets (string byte-order &key (bom t))
  "Encode STRING as UTF-16 in BYTE-ORDER (:BE or :LE), led by a BOM when BOM.
The encoder is written here from RFC 2781 rather than taken from a codec, so
it is an independent oracle for the decoder under test."
  (let ((units (if bom (list #xFEFF) '())))
    (loop for character across string
          for code = (char-code character)
          do (if (< code #x10000)
                 (push code units)
                 (let ((offset (- code #x10000)))
                   (push (+ #xD800 (ash offset -10)) units)
                   (push (+ #xDC00 (logand offset #x3FF)) units))))
    (octet-vector
     (loop for unit in (nreverse units)
           for high = (ash unit -8)
           for low = (logand unit #xFF)
           append (if (eq byte-order :be) (list high low) (list low high))))))

(defun normalized-line-ends (string)
  "Return STRING with each CRLF pair and each lone CR replaced by LF, the XML 1.0
section 2.11 rule, computed independently of the input layer."
  (with-output-to-string (output)
    (loop with after-return = nil
          for character across string
          do (unless (and after-return (char= character #\Newline))
               (write-char (if (char= character #\Return) #\Newline character) output))
             (setf after-return (char= character #\Return)))))

(defun condition-place (condition)
  "Return (SOURCE OFFSET LINE COLUMN) of the position CONDITION carries.
Signals an error when CONDITION has no position, so a row that expects a
place fails instead of comparing against NIL."
  (let ((position (or (xml-condition-position condition)
                      (error "~S carries no position." condition))))
    (list (xml-position-source position) (xml-position-offset position) (xml-position-line position)
          (xml-position-column position))))
