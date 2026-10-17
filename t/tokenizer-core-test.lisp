(in-package #:cl-xml-kit/test)

(describe "token records"
  (it-each ((:text "t") (:name "n") (:quoted-value "v") (:entity-reference "amp")
            (:comment "c") (:cdata "d") (:processing-instruction "pi")
            (:doctype " r") (:start-tag-open nil) (:end-tag-open nil)
            (:tag-close nil) (:empty-element-close nil) (:equals nil) (:eof nil))
      "builds a ~S token holding the value ~S"
      (kind value)
    (let ((token (cl-xml-kit::make-xml-token :kind kind :value value :end 3)))
      (expect (list (cl-xml-kit::xml-token-p token) (cl-xml-kit::xml-token-kind token)
                    (cl-xml-kit::xml-token-value token) (cl-xml-kit::xml-token-end token))
              :to-equal (list t kind value 3))))

  (it "leaves the position and end of a token NIL when they are omitted"
    (let ((token (cl-xml-kit::make-xml-token :kind :eof)))
      (expect (list (cl-xml-kit::xml-token-position token)
                    (cl-xml-kit::xml-token-end token))
              :to-equal '(nil nil))))

  (it "copies the token value on construction and on every read"
    (let* ((value (copy-seq "name"))
           (token (cl-xml-kit::make-xml-token :kind :name :value value)))
      (setf (char value 0) #\X)
      (setf (char (cl-xml-kit::xml-token-value token) 1) #\Y)
      (expect (cl-xml-kit::xml-token-value token) :to-equal "name")))

  (it-each (((:kind :unknown :value "x") "Unknown XML token kind :UNKNOWN.")
            ((:kind :text :value nil) "XML token kind :TEXT requires a string value.")
            ((:kind :name :value name) "XML token kind :NAME requires a string value.")
            ((:kind :equals :value "=") "XML token kind :EQUALS requires a NIL value.")
            ((:kind :eof :position :start) "TOKEN POSITION must be an XML-POSITION.")
            ((:kind :eof :end -1) "TOKEN END must be a non-negative integer or NIL.")
            ((:kind :eof :end 1.5) "TOKEN END must be a non-negative integer or NIL."))
      "refuses the token arguments ~S"
      (arguments message)
    (expect (lambda () (apply #'cl-xml-kit::make-xml-token arguments))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message)))))

(describe "tokenizer construction"
  (it "snapshots the input and source it was given"
    (let* ((input (copy-seq "<a/>"))
           (source (copy-seq "s.xml"))
           (tokenizer (cl-xml-kit::make-xml-tokenizer input :source source)))
      (setf (char input 0) #\X (char source 0) #\X)
      (expect (list (cl-xml-kit::xml-tokenizer-input tokenizer)
                    (cl-xml-kit::xml-tokenizer-source tokenizer))
              :to-equal '("<a/>" "s.xml"))))

  (it "reports no source when none was given"
    (expect (cl-xml-kit::xml-tokenizer-source (cl-xml-kit::make-xml-tokenizer "x"))
            :to-be nil))

  (it-each (((42) "TOKENIZER INPUT must be a normalized string.")
            (("x" :source 1) "TOKENIZER SOURCE must be a string or NIL.")
            (("x" :limits nil) "TOKENIZER LIMITS must be XML-RESOURCE-LIMITS."))
      "refuses the tokenizer arguments ~S"
      (arguments message)
    (expect (lambda () (apply #'cl-xml-kit::make-xml-tokenizer arguments))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message)))))

(describe "cursor primitives"
  (it "peeks at the cursor character without consuming it and returns NIL at the end"
    (let ((tokenizer (cl-xml-kit::make-xml-tokenizer "a")))
      (expect (list (funcall 'cl-xml-kit::%xml-tokenizer-peek tokenizer)
                    (funcall 'cl-xml-kit::%xml-tokenizer-peek tokenizer)
                    (progn (cl-xml-kit::next-xml-token tokenizer)
                           (funcall 'cl-xml-kit::%xml-tokenizer-peek tokenizer)))
              :to-equal '(#\a #\a nil))))

  (it-each ((("a" 1 "b") "Character U+0001 is not legal XML character.")
            (("<a b='" 31 "'/>") "Character U+001F is not legal XML character.")
            (("<!--" 65534 "-->") "Character U+FFFE is not legal XML character."))
      "refuses the illegal character in ~S"
      (parts message)
    (expect (lambda () (drain-tokens (xml-text-of parts)))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :legal-character)
                             (xml-condition-message ,message))))

  (it "advances line and column across a newline"
    (expect (mapcar #'token-summary (drain-tokens (format nil "x~%<root>")
                                                  :source "positions.xml"))
            :to-equal (list (list :text (format nil "x~%") 0 1 1)
                            '(:start-tag-open nil 2 2 1) '(:name "root" 3 2 2)
                            '(:tag-close nil 7 2 6) '(:eof nil 8 2 7))))

  (it "stamps the tokenizer source on every token position"
    (expect (remove-duplicates
             (mapcar (lambda (token)
                       (xml-position-source (cl-xml-kit::xml-token-position token)))
                     (drain-tokens "<a>t</a>" :source "positions.xml"))
             :test #'equal)
            :to-equal '("positions.xml"))))

(describe "name and text-node limits"
  (it-each (("<long>" (:max-name-length 4) (:start-tag-open :name :tag-close :eof))
            ("abc" (:max-text-node-length 3) (:text :eof))
            ("<![CDATA[abc]]>" (:max-text-node-length 3) (:cdata :eof))
            ("&abcd;" (:max-name-length 4) (:entity-reference :eof))
            ("<?long?>" (:max-name-length 4) (:processing-instruction :eof))
            ("<!DOCTYPE long>" (:max-name-length 4) (:doctype :eof)))
      "accepts ~S at the inclusive limit ~S"
      (input limits kinds)
    (expect (mapcar #'cl-xml-kit::xml-token-kind
                    (drain-tokens input :limits (apply #'make-xml-resource-limits limits)))
            :to-equal kinds))

  (it-each (("<long>" :max-name-length 3 4)
            ("</long>" :max-name-length 3 4)
            ("<a long='1'/>" :max-name-length 3 4)
            ("abcd" :max-text-node-length 3 4)
            ("<![CDATA[abcd]]>" :max-text-node-length 3 4)
            ("&abcd;" :max-name-length 3 4)
            ("<?long?>" :max-name-length 3 4)
            ("<!DOCTYPE long>" :max-name-length 3 4))
      "refuses ~S above the ~S limit ~S"
      (input kind limit value)
    (expect (lambda () (drain-tokens input :limits (make-xml-resource-limits kind limit)))
            :to-signal-xml `(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind ,kind)
                             (xml-resource-limit-exceeded-limit ,limit)
                             (xml-resource-limit-exceeded-value ,value)))))
