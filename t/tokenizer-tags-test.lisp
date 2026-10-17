(in-package #:cl-xml-kit/test)

(describe "tag tokens"
  (it "tracks the line and column of each tag token"
    (expect (mapcar (lambda (token) (cons (first token) (cddr token)))
                    (mapcar #'token-summary (drain-tokens (format nil "<a~% b='x'/>"))))
            :to-equal '((:start-tag-open 0 1 1) (:name 1 1 2) (:name 4 2 2)
                        (:equals 5 2 3) (:quoted-value 6 2 4)
                        (:empty-element-close 9 2 7) (:eof 11 2 9))))

  (it-each ((("<a  x = 'v'  >") ((:start-tag-open nil) (:name "a") (:name "x") (:equals nil)
                                (:quoted-value "v") (:tag-close nil) (:eof nil)))
            (("<a" 9 "x=\"1\"" 10 "y='2'/>")
             ((:start-tag-open nil) (:name "a") (:name "x") (:equals nil)
              (:quoted-value "1") (:name "y") (:equals nil) (:quoted-value "2")
              (:empty-element-close nil) (:eof nil)))
            (("</a >") ((:end-tag-open nil) (:name "a") (:tag-close nil) (:eof nil)))
            (("<a b=\"x'y\" c='x\"y'>")
             ((:start-tag-open nil) (:name "a") (:name "b") (:equals nil)
              (:quoted-value "x'y") (:name "c") (:equals nil) (:quoted-value "x\"y")
              (:tag-close nil) (:eof nil))))
      "accepts whitespace and either quote in the tag ~S"
      (parts tokens)
    (expect (xml-text-of parts) :to-tokenize-as tokens)))

(it-each (("< a>" nil :name "Expected an XML Name.")
          ("</ a>" nil :name "Expected an XML Name.")
          ("<1a>" nil :name "Expected an XML Name.")
          ("<a!>" :start-tag nil "Expected an attribute name or tag close.")
          ("<a 1b>" :start-tag nil "Expected an attribute name or tag close.")
          ("<a x='1' !>" :start-tag nil "Expected another attribute or tag close.")
          ("<a x=\"1\"y=\"2\">" :start-tag nil
           "Attributes must be separated by XML whitespace.")
          ("<a b@c>" :attribute nil "Expected '=' after an attribute name.")
          ("<a b>" :attribute nil "Expected '=' after an attribute name.")
          ("<a x=y>" :attribute-value nil "Expected a quoted attribute value after '='.")
          ("</a/>" :end-tag nil "Expected '>' after an end-tag name.")
          ("<a" :well-formedness nil "Unexpected end of XML input.")
          ("<a b=" :well-formedness nil "Unexpected end of XML input.")
          ("</a" :well-formedness nil "Unexpected end of XML input."))
    "refuses the malformed tag ~S"
    (input constraint expected message)
  (expect (lambda () (drain-tokens input))
          :to-signal-xml `(xml-well-formedness-error
                           (xml-well-formedness-error-constraint ,constraint)
                           (xml-well-formedness-error-expected ,expected)
                           (xml-condition-message ,message))))
