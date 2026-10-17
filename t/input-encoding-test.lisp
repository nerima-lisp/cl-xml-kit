(in-package #:cl-xml-kit/test)

(defparameter *utf-16-sample*
  (format nil "<?xml version=\"1.0\"?>~C~C<r a=\"~C\">x ~C &amp; y<!--c--><?p d?></r>"
          #\Return #\Linefeed (code-char #xE9) (code-char #x1D11E))
  "A document with CRLF, a Latin-1 letter, and a character outside the BMP, which
UTF-16 encodes as a surrogate pair.")

(describe "UTF-8 octets"
  (it "strips the byte order mark and normalizes the decoded text"
    (let ((input (cl-xml-kit::make-xml-input (octet-vector '(#xEF #xBB #xBF #x41 #x0D #x0A #x42))
                                             :source "utf8.xml")))
      (expect (cl-xml-kit::xml-input-text input) :to-equal (format nil "A~%B"))
      (expect (mapcar (lambda (offset)
                        (xml-position-line (cl-xml-kit::xml-input-position-at input offset)))
                      '(0 2))
              :to-equal '(1 2))))

  (it-each (((#xC3 #x28) #x28 ("bad.xml" 0 1 1) "Invalid UTF-8 input at octet 0.")
            ((#x41 #xFF #x42) #xFF ("bad.xml" 1 1 2) "Invalid UTF-8 input at octet 1.")
            ((#x41 #x80) #x80 ("bad.xml" 1 1 2) "Invalid UTF-8 input at octet 1.")
            ((#x41 #xE2 #x82) #xE2 ("bad.xml" 1 1 2) "Invalid UTF-8 input at octet 1."))
      "maps the codec failure in ~S to a decode error naming octet ~S"
      (octets octet place message)
    (expect (lambda () (cl-xml-kit::make-xml-input (octet-vector octets) :source "bad.xml"))
            :to-signal-xml `(xml-decode-error
                             (xml-decode-error-encoding :utf-8)
                             (xml-decode-error-octet ,octet)
                             (condition-place ,place)
                             (xml-condition-source "bad.xml")
                             (xml-condition-message ,message))))

  (it-each (((#x61 #x0D #x0A #x62 #x0D #x63 #xFF) (nil 5 3 2))
            ((#x61 #x0A #x62 #xFF) (nil 3 2 2))
            ((#x0D #x0D #xFF) (nil 2 3 1)))
      "places the decode error in ~S at its normalized place"
      (octets place)
    (expect (lambda () (cl-xml-kit::make-xml-input (octet-vector octets)))
            :to-signal-xml `(xml-decode-error (condition-place ,place)))))

(describe "declared encodings"
  (it-each (("<?xml version='1.0' encoding='UTF-8'?><r/>")
            ("<?xml version=\"1.0\" encoding=\"utf-8\"?><r/>")
            ("<?xml version='1.0'?><r/>")
            ("<?xml version='1.1' encoding='ISO-8859-1'?><r/>")
            ("<?xml version='1.0'encoding='ISO-8859-1'?><r/>")
            ("<?xml version='1.0' encoding='ISO-8859-1'")
            ("<r/><?xml version='1.0' encoding='ISO-8859-1'?>")
            ("<?xml version='1.0' encoding=ISO-8859-1?><r/>")
            ("<?xml version=?><r/>"))
      "leaves ~S to the parser instead of refusing its encoding"
      (text)
    (expect (cl-xml-kit::xml-input-text (cl-xml-kit::make-xml-input text)) :to-equal text))

  (it-each (("<?xml version='1.0' encoding='ISO-8859-1'?><r/>" "ISO-8859-1")
            ("<?xml  version = \"1.0\"  encoding = 'UTF-16' ?><r/>" "UTF-16")
            ((#\< #\? #\x #\m #\l #\Tab #\v #\e #\r #\s #\i #\o #\n #\= #\' #\1 #\. #\0 #\'
              #\Newline #\e #\n #\c #\o #\d #\i #\n #\g #\= #\' #\x #\' #\? #\>)
             "x"))
      "refuses character input that declares the encoding ~*~S"
      (text encoding)
    (expect (lambda () (cl-xml-kit::make-xml-input (coerce text 'string)))
            :to-signal-xml `(xml-unsupported-encoding
                             (xml-unsupported-encoding-encoding ,encoding)
                             (xml-unsupported-encoding-reason :declaration-not-utf-8)
                             (xml-condition-message "The XML declaration must use UTF-8."))))

  (it "refuses UTF-8 octets that declare another encoding"
    (expect (lambda ()
              (cl-xml-kit::make-xml-input
               (sb-ext:string-to-octets "<?xml version='1.0' encoding='Shift_JIS'?><r/>"
                                        :external-format :utf-8)))
            :to-signal-xml '(xml-unsupported-encoding
                             (xml-unsupported-encoding-encoding "Shift_JIS")
                             (xml-unsupported-encoding-reason :declaration-not-utf-8)))))

(describe "UTF-16 octets"
  (it-each ((:be) (:le))
      "parses BOM-marked UTF-16~A, declared or not, into the document the UTF-8 octets give"
      (byte-order)
    (let ((expected (xml->string
                     (parse-xml (sb-ext:string-to-octets *utf-16-sample*
                                                         :external-format :utf-8))))
          (declared (concatenate 'string "<?xml version=\"1.0\" encoding=\"UTF-16\"?>"
                                 (subseq *utf-16-sample* (length "<?xml version=\"1.0\"?>")))))
      (expect (xml->string (parse-xml (utf-16-octets *utf-16-sample* byte-order)))
              :to-equal expected)
      (expect (xml->string (parse-xml (utf-16-octets declared byte-order)))
              :to-equal expected)))

  (it-each ((:be "UTF-16") (:le "utf-16"))
      "decodes UTF-16~A octets declaring ~S into normalized text"
      (byte-order declared)
    (let ((text (format nil "<?xml version='1.0' encoding='~A'?>~C~C<r>~C</r>"
                        declared #\Return #\Linefeed (code-char #x1D11E))))
      (expect (cl-xml-kit::xml-input-text
               (cl-xml-kit::make-xml-input (utf-16-octets text byte-order)))
              :to-equal (normalized-line-ends text))))

  (it-each ((:be "UTF-8") (:le "UTF-8") (:be "ISO-8859-1") (:le "ISO-8859-1"))
      "refuses UTF-16~A octets that declare ~S"
      (byte-order declared)
    (expect (lambda ()
              (cl-xml-kit::make-xml-input
               (utf-16-octets (format nil "<?xml version=\"1.0\" encoding=\"~A\"?><r/>" declared)
                              byte-order)))
            :to-signal-xml `(xml-unsupported-encoding
                             (xml-unsupported-encoding-encoding ,declared)
                             (xml-unsupported-encoding-reason :declaration-contradicts-bom)
                             (xml-condition-message
                              ,(format nil "The XML declaration names ~A, ~
                                            but the byte order mark selects UTF-16."
                                       declared)))))

  (it-each ((:be) (:le))
      "refuses UTF-16~A octets without a byte order mark"
      (byte-order)
    (expect (lambda ()
              (cl-xml-kit::make-xml-input
               (utf-16-octets "<?xml version=\"1.0\" encoding=\"UTF-16\"?><r/>" byte-order
                              :bom nil)))
            :to-signal-xml '(xml-unsupported-encoding
                             (xml-unsupported-encoding-encoding :utf-16)
                             (xml-unsupported-encoding-reason :utf-16-bom-required)
                             (xml-condition-message
                              "UTF-16 input must begin with a byte order mark."))))

  ;; Each input decodes "a" and then fails at the code unit starting at octet index 4;
  ;; the third column is the value of the octet there.
  (it-each (((#xFF #xFE #x61 0 #x00 #xD8 #x62 0) :utf-16le #x00)
            ((#xFE #xFF 0 #x61 #xDC #x00) :utf-16be #xDC)
            ((#xFF #xFE #x61 0 #x3D #xD8) :utf-16le #x3D)
            ((#xFE #xFF 0 #x61 0) :utf-16be 0))
      "refuses the unpaired surrogate or truncated unit in ~S as ~S naming octet value ~S"
      (octets encoding octet)
    (expect (lambda () (cl-xml-kit::make-xml-input (octet-vector octets) :source "s.xml"))
            :to-signal-xml `(xml-decode-error
                             (type-of xml-decode-error)
                             (xml-decode-error-encoding ,encoding)
                             (xml-decode-error-octet ,octet)
                             (condition-place ("s.xml" 1 1 2))
                             (xml-condition-message "Invalid UTF-16 input at octet 4."))))

  (it-each ((:max-input-octets 9 10) (:max-document-characters 3 4))
      "applies ~S to UTF-16 input, refusing limit ~D for the value ~D"
      (kind limit value)
    (expect (lambda ()
              (cl-xml-kit::make-xml-input (utf-16-octets "<a/>" :le)
                                          :limits (make-xml-resource-limits kind limit)))
            :to-signal-xml `(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind ,kind)
                             (xml-resource-limit-exceeded-value ,value)
                             (xml-resource-limit-exceeded-limit ,limit))))

  (it "accepts UTF-16 input at both inclusive limits"
    (expect (cl-xml-kit::xml-input-text
             (cl-xml-kit::make-xml-input (utf-16-octets "<a/>" :le)
                                         :limits (make-xml-resource-limits
                                                  :max-input-octets 10
                                                  :max-document-characters 4)))
            :to-equal "<a/>")))

(it-each (((0 0 #xFE #xFF #x00)) ((#xFF #xFE 0 0 #x00)))
    "refuses the UTF-32 byte order mark in ~S"
    (octets)
  (expect (lambda () (cl-xml-kit::make-xml-input (octet-vector octets)))
          :to-signal-xml '(xml-unsupported-encoding
                           (xml-unsupported-encoding-encoding :utf-32)
                           (xml-unsupported-encoding-reason :utf-32-not-supported)
                           (xml-condition-message "UTF-32 input is not supported."))))
