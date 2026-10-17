(in-package #:cl-xml-kit/test)

(defun input-at (value &rest arguments)
  (apply #'cl-xml-kit::make-xml-input value arguments))

(defun string-of (&rest characters)
  (coerce characters 'string))

(defun octets-of (&rest octets)
  (make-array (length octets)
              :element-type '(unsigned-byte 8)
              :initial-contents octets))

(it "normalizes strings and reports normalized positions"
  (let ((input (input-at (string-of #\a #\Return #\Linefeed #\b #\Return #\c)
                         :source "memory.xml")))
    (expect (cl-xml-kit::xml-input-p input) :to-be-truthy)
    (expect (cl-xml-kit::xml-input-text input)
            :to-equal (string-of #\a #\Linefeed #\b #\Linefeed #\c))
    (expect (cl-xml-kit::xml-input-source input) :to-equal "memory.xml")
    (let ((position (cl-xml-kit::xml-input-position-at input 2))
          (end (cl-xml-kit::xml-input-position-at input 5)))
      (expect (xml-position-offset position) :to-equal 2)
      (expect (xml-position-line position) :to-equal 2)
      (expect (xml-position-column position) :to-equal 1)
      (expect (xml-position-line end) :to-equal 3)
      (expect (xml-position-column end) :to-equal 2))))

(it "accepts character streams using the same normalization"
  (with-input-from-string
      (stream (string-of #\f #\i #\r #\s #\t #\Return #\s #\e #\c #\o #\n #\d
                         #\Linefeed #\t #\h #\i #\r #\d))
    (let ((input (input-at stream :source "stream.xml")))
      (expect (cl-xml-kit::xml-input-text input)
              :to-equal (string-of #\f #\i #\r #\s #\t #\Linefeed #\s #\e #\c
                                   #\o #\n #\d #\Linefeed #\t #\h #\i #\r #\d))
      (expect (xml-position-source
               (cl-xml-kit::xml-input-position-at input 6))
              :to-equal "stream.xml"))))

(it "strips a UTF-8 BOM from octet input"
  (let* ((octets (octets-of #xEF #xBB #xBF #x41 #x0D #x0A #x42))
         (input (input-at octets :source "utf8.xml")))
    (expect (cl-xml-kit::xml-input-text input)
            :to-equal (string-of #\A #\Linefeed #\B))
    (expect (xml-position-offset
             (cl-xml-kit::xml-input-position-at input 0))
            :to-equal 0)
    (expect (xml-position-line
             (cl-xml-kit::xml-input-position-at input 2))
            :to-equal 2)))

(defun utf-16-octets (string byte-order &key (bom t))
  "Encode STRING as UTF-16 in BYTE-ORDER (:BE or :LE), led by a BOM when BOM."
  (let ((units (if bom (list #xFEFF) '())))
    (loop for character across string
          for code = (char-code character)
          do (if (< code #x10000)
                 (push code units)
                 (let ((offset (- code #x10000)))
                   (push (+ #xD800 (ash offset -10)) units)
                   (push (+ #xDC00 (logand offset #x3FF)) units))))
    (apply #'octets-of
           (loop for unit in (nreverse units)
                 for high = (ash unit -8)
                 for low = (logand unit #xFF)
                 append (if (eq byte-order :be) (list high low) (list low high))))))

(defun caught-condition (type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (caught) (and (typep caught type) caught))))

(defparameter *utf-16-sample*
  (format nil "<?xml version=\"1.0\"?>~C~C<r a=\"~C\">x ~C &amp; y<!--c--><?p d?></r>"
          #\Return #\Linefeed (code-char #xE9) (code-char #x1D11E)))

(it "parses BOM-marked UTF-16BE and UTF-16LE into the UTF-8 document"
  (let ((expected (xml->string
                   (parse-xml (sb-ext:string-to-octets *utf-16-sample*
                                                       :external-format :utf-8)))))
    (dolist (byte-order '(:be :le))
      (expect (xml->string (parse-xml (utf-16-octets *utf-16-sample* byte-order)))
              :to-equal expected))))

(it "accepts a UTF-16 declaration on BOM-marked UTF-16 input"
  (dolist (byte-order '(:be :le))
    (expect (cl-xml-kit::xml-input-text
             (input-at (utf-16-octets "<?xml version='1.0' encoding='utf-16'?><r/>"
                                      byte-order)))
            :to-equal "<?xml version='1.0' encoding='utf-16'?><r/>")))

(it "rejects a declared encoding that contradicts the UTF-16 BOM"
  (dolist (byte-order '(:be :le))
    (dolist (declared '("UTF-8" "ISO-8859-1"))
      (let ((condition
              (caught-condition
               'xml-unsupported-encoding
               (lambda ()
                 (parse-xml (utf-16-octets
                             (format nil "<?xml version=\"1.0\" encoding=\"~A\"?><r/>"
                                     declared)
                             byte-order))))))
        (expect (typep condition 'xml-unsupported-encoding) :to-be-truthy)
        (expect (xml-unsupported-encoding-encoding condition) :to-equal declared)
        (expect (xml-unsupported-encoding-reason condition)
                :to-equal :declaration-contradicts-bom)))))

(it "rejects UTF-16 octet input without a BOM"
  (dolist (byte-order '(:be :le))
    (let ((condition
            (caught-condition
             'xml-unsupported-encoding
             (lambda ()
               (input-at (utf-16-octets "<?xml version=\"1.0\" encoding=\"UTF-16\"?><r/>"
                                        byte-order :bom nil))))))
      (expect (typep condition 'xml-unsupported-encoding) :to-be-truthy)
      (expect (xml-unsupported-encoding-encoding condition) :to-equal :utf-16)
      (expect (xml-unsupported-encoding-reason condition)
              :to-equal :utf-16-bom-required))))

(it "signals decode errors for unpaired surrogates and truncated UTF-16"
  ;; Each input decodes "a" and then fails at the code unit starting at octet 4.
  (dolist (case (list (list (octets-of #xFF #xFE #x61 0 #x00 #xD8 #x62 0) :utf-16le)
                      (list (octets-of #xFE #xFF 0 #x61 #xDC #x00) :utf-16be)
                      (list (octets-of #xFF #xFE #x61 0 #x3D #xD8) :utf-16le)
                      (list (octets-of #xFE #xFF 0 #x61 0) :utf-16be)))
    (destructuring-bind (octets encoding) case
      (let ((condition (caught-condition 'xml-decode-error
                                         (lambda () (input-at octets :source "s.xml")))))
        (expect (typep condition 'xml-decode-error) :to-be-truthy)
        (expect (typep condition 'xml-unsupported-encoding) :to-equal nil)
        (expect (xml-decode-error-encoding condition) :to-equal encoding)
        (expect (xml-decode-error-octet condition) :to-equal (aref octets 4))
        (expect (xml-position-offset (xml-condition-position condition)) :to-equal 1)))))

(it "applies the octet and character limits to UTF-16 input"
  (let ((octets (utf-16-octets "<a/>" :le)))
    (let ((condition (caught-condition
                      'xml-resource-limit-exceeded
                      (lambda ()
                        (input-at octets :limits (make-xml-resource-limits
                                                  :max-input-octets 9))))))
      (expect (xml-resource-limit-exceeded-kind condition) :to-equal :max-input-octets)
      (expect (xml-resource-limit-exceeded-value condition) :to-equal 10))
    (let ((condition (caught-condition
                      'xml-resource-limit-exceeded
                      (lambda ()
                        (input-at octets :limits (make-xml-resource-limits
                                                  :max-document-characters 3))))))
      (expect (xml-resource-limit-exceeded-kind condition)
              :to-equal :max-document-characters)
      (expect (xml-resource-limit-exceeded-value condition) :to-equal 4))
    (expect (cl-xml-kit::xml-input-text
             (input-at octets :limits (make-xml-resource-limits
                                       :max-input-octets 10
                                       :max-document-characters 4)))
            :to-equal "<a/>")))

(it "rejects both UTF-32 BOM byte orders with the profile condition"
  (dolist (octets (list (octets-of 0 0 #xFE #xFF #x00)
                        (octets-of #xFF #xFE 0 0 #x00)))
    (let ((condition nil))
      (handler-case (input-at octets)
        (xml-unsupported-encoding (caught) (setf condition caught)))
      (expect (typep condition 'xml-unsupported-encoding) :to-be-truthy)
      (expect (xml-unsupported-encoding-encoding condition) :to-equal :utf-32)
      (expect (xml-unsupported-encoding-reason condition)
              :to-equal :utf-32-not-supported))))

(it "maps codec decode failures to XML decode errors"
  (let ((condition nil))
    (handler-case (input-at (octets-of #xC3 #x28) :source "bad.xml")
      (xml-decode-error (caught) (setf condition caught)))
    (expect (typep condition 'xml-decode-error) :to-be-truthy)
    (expect (xml-decode-error-encoding condition) :to-equal :utf-8)
    (expect (xml-decode-error-octet condition) :to-equal #x28)
    (expect (xml-position-source (xml-condition-position condition))
            :to-equal "bad.xml")
    (expect (xml-position-offset (xml-condition-position condition))
            :to-equal 0)))

(it "reports a decode error at the normalized position after CRLF and CR"
  (let ((condition (caught-condition
                    'xml-decode-error
                    (lambda () (input-at (octets-of #x61 #x0D #x0A #x62 #x0D #x63 #xFF))))))
    (expect (list (xml-position-offset (xml-condition-position condition))
                  (xml-position-line (xml-condition-position condition))
                  (xml-position-column (xml-condition-position condition)))
            :to-equal '(5 3 2))))

(it "accepts the inclusive input-octet boundary and rejects the next byte"
  (let ((octets (octets-of #x41 #x42 #x43)))
    (expect (cl-xml-kit::xml-input-text
             (input-at octets
                       :limits (make-xml-resource-limits :max-input-octets 3)))
            :to-equal "ABC")
    (let ((condition nil))
      (handler-case
          (input-at octets
                    :limits (make-xml-resource-limits :max-input-octets 2))
        (xml-resource-limit-exceeded (caught) (setf condition caught)))
      (expect (xml-resource-limit-exceeded-kind condition)
              :to-equal :max-input-octets)
      (expect (xml-resource-limit-exceeded-value condition) :to-equal 3)
      (expect (xml-resource-limit-exceeded-limit condition) :to-equal 2))))

(it "counts normalized document characters with an inclusive boundary"
  (let ((limits (make-xml-resource-limits :max-document-characters 3)))
    (expect (cl-xml-kit::xml-input-text
             (input-at (string-of #\a #\Return #\Linefeed #\b) :limits limits))
            :to-equal (string-of #\a #\Linefeed #\b))
    (let ((condition nil))
      (handler-case (input-at "abcd" :limits limits)
        (xml-resource-limit-exceeded (caught) (setf condition caught)))
      (expect (xml-resource-limit-exceeded-kind condition)
              :to-equal :max-document-characters)
      (expect (xml-resource-limit-exceeded-value condition) :to-equal 4)
      (expect (xml-resource-limit-exceeded-limit condition) :to-equal 3))))

(it "passes normalized text and source to the tokenizer contract"
  (let* ((input (input-at (string-of #\a #\Return #\b) :source "tokenizer.xml"))
         (tokenizer
           (cl-xml-kit::make-xml-tokenizer
            (cl-xml-kit::xml-input-text input)
            :source (cl-xml-kit::xml-input-source input)
            :limits (cl-xml-kit::xml-input-limits input))))
    (expect (cl-xml-kit::xml-tokenizer-input tokenizer)
            :to-equal (string-of #\a #\Linefeed #\b))
    (expect (cl-xml-kit::xml-tokenizer-source tokenizer)
            :to-equal "tokenizer.xml")))

(it "rejects invalid input options and position offsets"
  (dolist (thunk (list
                  (lambda () (input-at "x" :source 1))
                  (lambda () (input-at "x" :limits nil))
                  (lambda () (input-at 42))
                  (lambda () (cl-xml-kit::xml-input-position-at
                               (input-at "x") -1))
                  (lambda () (cl-xml-kit::xml-input-position-at
                               (input-at "x") 2))))
    (expect (handler-case (progn (funcall thunk) nil)
              (xml-input-error () t))
            :to-be-truthy)))

