(in-package #:cl-xml-kit/test)

(defun classify (predicate code)
  "Call the global function PREDICATE on the character with CODE.
The library declares its predicates inline, so its own callers run open-coded
copies; only a call through the symbol runs, and covers, the global definition."
  (funcall predicate (or (code-char code)
                         (error "Unicode code point #X~X is unavailable." code))))

(describe "name characters"
  ;; Expected values are read off XML 1.0 Fifth Edition, production [4]
  ;; NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6]
  ;;   | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D]
  ;;   | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF]
  ;;   | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
  ;; and production [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7
  ;;   | [#x0300-#x036F] | [#x203F-#x2040].
  ;; Namespaces in XML 1.0 [4] and [5] remove ":" from both for NCName.
  ;; Columns: code, NameStartChar, NameChar, NCName start, NCName character.
  (it-each ((#x2D nil t nil t) (#x2E nil t nil t) (#x2F nil nil nil nil)
            (#x30 nil t nil t) (#x39 nil t nil t) (#x3A t t nil nil) (#x3B nil nil nil nil)
            (#x40 nil nil nil nil) (#x41 t t t t) (#x5A t t t t) (#x5B nil nil nil nil)
            (#x5F t t t t) (#x60 nil nil nil nil) (#x61 t t t t) (#x7A t t t t)
            (#x7B nil nil nil nil) (#xB7 nil t nil t) (#xBF nil nil nil nil)
            (#xC0 t t t t) (#xD6 t t t t) (#xD7 nil nil nil nil) (#xD8 t t t t)
            (#xF6 t t t t) (#xF7 nil nil nil nil) (#xF8 t t t t) (#x2FF t t t t)
            (#x300 nil t nil t) (#x36F nil t nil t) (#x370 t t t t) (#x37D t t t t)
            (#x37E nil nil nil nil) (#x37F t t t t) (#x1FFF t t t t)
            (#x2000 nil nil nil nil) (#x200B nil nil nil nil) (#x200C t t t t)
            (#x200D t t t t) (#x200E nil nil nil nil) (#x203E nil nil nil nil)
            (#x203F nil t nil t) (#x2040 nil t nil t) (#x2041 nil nil nil nil)
            (#x2070 t t t t) (#x218F t t t t) (#x2190 nil nil nil nil)
            (#x2BFF nil nil nil nil) (#x2C00 t t t t) (#x2FEF t t t t)
            (#x2FF0 nil nil nil nil) (#x2FFF nil nil nil nil) (#x3000 nil nil nil nil)
            (#x3001 t t t t) (#xD7FF t t t t) (#xF8FF nil nil nil nil) (#xF900 t t t t)
            (#xFDCF t t t t) (#xFDD0 nil nil nil nil) (#xFDEF nil nil nil nil)
            (#xFDF0 t t t t) (#xFFFD t t t t) (#x10000 t t t t) (#xEFFFF t t t t)
            (#xF0000 nil nil nil nil))
      "classifies code #x~X as NameStartChar ~S, NameChar ~S, NCName start ~S, NCName char ~S"
      (code start name ncname-start ncname)
    (expect (list (classify 'cl-xml-kit::xml-name-start-char-p code)
                  (classify 'cl-xml-kit::xml-name-char-p code)
                  (classify 'cl-xml-kit::xml-ncname-start-char-p code)
                  (classify 'cl-xml-kit::xml-ncname-char-p code))
            :to-equal (list start name ncname-start ncname)))

  (it-each (((#x6E #x61 #x6D #x65) t) ((#x5F #x6E) t) ((#x61 #x2D #x2E #x5F #x39) t)
            ((#x6E #x2E) t) ((#x61 #x300) t) ((#xC0 #xB7) t) ((#x61) t)
            (() nil) ((#x61 #x3A #x62) nil) ((#x3A #x61) nil) ((#x31 #x6E) nil)
            ((#x2D #x6E) nil) ((#xB7 #x61) nil) ((#x61 #xFDD0) nil) ((#x61 #xD7) nil)
            ((#x61 #x20) nil))
      "classifies the code sequence ~S as an NCName ~S"
      (codes expected)
    (expect (funcall 'cl-xml-kit::xml-ncname-p (map 'simple-string #'code-char codes))
            :to-be expected)))

(describe "XML characters and whitespace"
  ;; XML 1.0 [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD]
  ;;   | [#x10000-#x10FFFF]
  (it-each ((#x0 nil) (#x8 nil) (#x9 t) (#xA t) (#xB nil) (#xC nil) (#xD t) (#xE nil)
            (#x1F nil) (#x20 t) (#xD7FF t) (#xD800 nil) (#xDFFF nil) (#xE000 t)
            (#xFDD0 t) (#xFFFD t) (#xFFFE nil) (#xFFFF nil) (#x10000 t) (#x10FFFF t))
      "classifies code #x~X as an XML Char ~S"
      (code expected)
    (expect (classify 'cl-xml-kit::xml-char-p code) :to-be expected))

  ;; XML 1.0 [3] S ::= (#x20 | #x9 | #xD | #xA)+
  (it-each ((#x20 t) (#x9 t) (#xD t) (#xA t) (#x41 nil) (#xB nil) (#xC nil) (#x85 nil)
            (#xA0 nil) (#x2028 nil) (#x3000 nil))
      "classifies code #x~X as an S character ~S"
      (code expected)
    (expect (classify 'cl-xml-kit::xml-space-char-p code) :to-be expected))

  (it-each (((#x20 #x9 #xD #xA) t) ((#x20) t) ((#x20 #x20 #x20) t) (() nil)
            ((#x20 #x78) nil) ((#x78 #x20) nil) ((#x20 #xA0) nil))
      "classifies the code sequence ~S as XML S ~S"
      (codes expected)
    (expect (funcall 'cl-xml-kit::xml-space-p (map 'simple-string #'code-char codes))
            :to-be expected)))

(describe "public identifier characters"
  ;; XML 1.0 [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9]
  ;;   | [-'()+,./:=?;!*#@$_%]
  (it "accepts every PubidChar"
    (expect (remove-if (lambda (character)
                         (funcall 'cl-xml-kit::xml-pubid-char-p character))
                       (concatenate 'string
                                    (list #\Space #\Return #\Newline)
                                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                    "0123456789-'()+,./:=?;!*#@$_%"))
            :to-equal ""))

  (it-each ((#x22) (#x26) (#x3C) (#x3E) (#x5B) (#x5C) (#x5D) (#x5E) (#x60) (#x7B) (#x7C)
            (#x7D) (#x7E) (#x9) (#x7F) (#xA0) (#xC0))
      "rejects code #x~X as a PubidChar"
      (code)
    (expect (classify 'cl-xml-kit::xml-pubid-char-p code) :to-be nil)))

(describe "character reference digits"
  ;; XML 1.0 [66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
  (it-each ((#\0 10 0) (#\5 10 5) (#\9 10 9) (#\a 10 nil) (#\F 10 nil) (#\/ 10 nil)
            (#\: 10 nil) (#\0 16 0) (#\9 16 9) (#\a 16 10) (#\f 16 15) (#\A 16 10)
            (#\F 16 15) (#\g 16 nil) (#\G 16 nil) (#\@ 16 nil) (#\` 16 nil))
      "weighs ~S in radix ~D as ~S"
      (character radix expected)
    (expect (cl-xml-kit::xml-digit-weight character radix) :to-be expected))

  ;; Arabic-Indic, Devanagari, and fullwidth digits, most of which DIGIT-CHAR-P
  ;; weighs on SBCL, and fullwidth letters.
  (it-each ((#x660) (#x665) (#x966) (#xFF10) (#xFF19) (#xFF21) (#xFF41))
      "gives code #x~X no weight in either radix"
      (code)
    (expect (list (cl-xml-kit::xml-digit-weight (code-char code) 10)
                  (cl-xml-kit::xml-digit-weight (code-char code) 16))
            :to-equal '(nil nil)))

  (it-sequential "kills every mutant of the digit weight function"
    (let ((results
            (mutation-results
             "src/char-class.lisp" 'cl-xml-kit::xml-digit-weight
             (lambda ()
               (equal (mapcar (lambda (row)
                                (funcall 'cl-xml-kit::xml-digit-weight
                                         (first row) (second row)))
                              '((#\0 10) (#\7 10) (#\a 10) (#\A 10) (#\3 16) (#\A 16)
                                (#\F 16) (#\a 16) (#\f 16) (#\G 16) (#\g 16) (#\/ 16)))
                      '(0 7 nil nil 3 10 15 10 15 nil nil nil))))))
      (expect results :to-satisfy #'consp)
      (expect (getf (assert-mutation-score results 1.0) :score) :to-equal 1.0))))
