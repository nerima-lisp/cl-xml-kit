(in-package #:cl-xml-kit/test)

;; Expected outcomes come from XML 1.0 Fifth Edition [4] NameStartChar and
;; [4a] NameChar, not from the library's character classes:
;;   NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6]
;;     | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D]
;;     | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF]
;;     | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
;;   NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F]
;;     | [#x203F-#x2040]
;; The rejected rows sit just outside a NameStartChar range (or are NameChar
;; only, like #x300 and #x36F); the accepted rows are range starts.

(describe "names that may not start an entity name"
  (it-each ((#xD7) (#xF7) (#x300) (#x36F) (#x37E) (#x2000) (#x2005) (#x200B) (#x2190)
            (#x2FF0) (#x3000) (#xE000) (#xFDD0))
      "rejects a general entity declared with first code point U+~4,'0X~:* (~S)"
      (code)
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset
               (format nil "<!ENTITY ~Ax 'v'>" (name-with-code-points code))))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-declaration)
                             (xml-condition-message
                              "An entity declaration needs a valid name."))))

  (it-each ((#xD7) (#xF7) (#x300) (#x36F) (#x37E) (#x2000) (#x2005) (#x200B) (#x2190)
            (#x2FF0) (#x3000) (#xE000) (#xFDD0))
      "rejects a parameter entity declared with first code point U+~4,'0X~:* (~S)"
      (code)
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset
               (format nil "<!ENTITY % ~Ax 'v'>" (name-with-code-points code))))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-declaration)
                             (xml-condition-message
                              "An entity declaration needs a valid name."))))

  (it-each ((#xD7) (#xF7) (#x300) (#x36F) (#x37E) (#x2000) (#x2005) (#x200B) (#x2190)
            (#x2FF0) (#x3000) (#xE000) (#xFDD0))
      "rejects a document whose DOCTYPE declares an entity with first code point U+~4,'0X~:* (~S)"
      (code)
    (expect (lambda ()
              (parse-xml (format nil "<!DOCTYPE r [<!ENTITY ~Ax 'v'>]><r/>"
                                 (name-with-code-points code))))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-declaration))))

  (it-each ((#xD7) (#xF7) (#x300) (#x36F) (#x37E) (#x2000) (#x2005) (#x200B) (#x2190)
            (#x2FF0) (#x3000) (#xE000) (#xFDD0))
      "rejects a content reference to a name with first code point U+~4,'0X~:* (~S)"
      (code)
    (expect (lambda ()
              (parse-xml (format nil "<r>&~Ax;</r>" (name-with-code-points code))))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-reference)
                             (xml-well-formedness-error-expected nil))))

  (it-each ((#xD7) (#xF7) (#x300) (#x36F) (#x37E) (#x2000) (#x2005) (#x200B) (#x2190)
            (#x2FF0) (#x3000) (#xE000) (#xFDD0))
      "rejects an attribute reference to a name with first code point U+~4,'0X~:* (~S)"
      (code)
    (expect (lambda ()
              (parse-xml (format nil "<r a='&~Ax;'/>" (name-with-code-points code))))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-reference)
                             (xml-well-formedness-error-expected nil))))

  (it-each ((#xD7) (#xF7) (#x300) (#x36F) (#x37E) (#x2000) (#x2005) (#x200B) (#x2190)
            (#x2FF0) (#x3000) (#xE000) (#xFDD0))
      "rejects an entity value reference to a name with first code point U+~4,'0X~:* (~S)"
      (code)
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset
               (format nil "<!ENTITY e '&~Ax;'>" (name-with-code-points code))))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-reference)
                             (xml-condition-message
                              "An entity value contains a malformed entity reference.")))))

(describe "names that may start an entity name"
  (it-each ((#xC0) (#xD8) (#xF8) (#x370) (#x37F) (#x200C) (#x200D) (#x2070) (#x10000))
      "expands a content reference to an entity whose name starts with code point U+~4,'0X~:* (~S)"
      (code)
    (let ((name (name-with-code-points code (char-code #\x))))
      (expect (parsed-root-text
               (format nil "<!DOCTYPE r [<!ENTITY ~A 'v'>]><r>&~A;</r>" name name))
              :to-equal "v")))

  (it-each ((#xC0) (#xD8) (#xF8) (#x370) (#x37F) (#x200C) (#x200D) (#x2070) (#x10000))
      "expands an attribute reference to an entity whose name starts with code point U+~4,'0X~:* (~S)"
      (code)
    (let ((name (name-with-code-points code (char-code #\x))))
      (expect (parsed-root-attribute
               (format nil "<!DOCTYPE r [<!ENTITY ~A 'v'>]><r a='&~A;'/>" name name) "a")
              :to-equal "v")))

  (it-each ((#xC0) (#xD8) (#xF8) (#x370) (#x37F) (#x200C) (#x200D) (#x2070) (#x10000))
      "expands a parameter entity whose name starts with code point U+~4,'0X~:* (~S)"
      (code)
    (let ((name (name-with-code-points code (char-code #\x))))
      (expect (cl-xml-kit::xml-entity-declaration-replacement-text
               (subset-declaration
                (format nil "<!ENTITY % ~A '<!ENTITY g \"v\">'>%~A;" name name) "g"))
              :to-equal "v"))))

(describe "characters after the first"
  (it-each ((#x300) (#x36F) (#x203F) (#x2040) (#xB7) (#x2D) (#x2E) (#x30))
      "accepts code point U+~4,'0X~:* (~S) after the first character of an entity name"
      (code)
    (let ((name (name-with-code-points (char-code #\a) code)))
      (expect (parsed-root-text
               (format nil "<!DOCTYPE r [<!ENTITY ~A 'v'>]><r>&~A;</r>" name name))
              :to-equal "v")))

  (it-each ((#xD7) (#xF7) (#x37E) (#x2000))
      "ends an entity name at code point U+~4,'0X~:* (~S) so the declaration is rejected"
      (code)
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset
               (format nil "<!ENTITY a~A 'v'>" (name-with-code-points code))))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-declaration)
                             (xml-condition-message
                              "An entity name must be separated from its declaration value.")))))

;; The alphabets hold range-start NameStartChars and NameChar-only characters
;; so generated names reach past the first character into [4a]. The colon is
;; left out: Namespaces in XML 1.0 section 7 forbids it in entity names.
(it-property "declares and expands any name built from NameStartChar and NameChar"
    ((start (gen-member (list #\a #\Z #\_ (code-char #xC0) (code-char #x370)
                              (code-char #x2070) (code-char #x3001) (code-char #x10000))))
     (rest (gen-string :max-length 6
                       :alphabet (coerce (list #\a #\- #\. #\0 (code-char #xB7)
                                               (code-char #x300) (code-char #x203F)
                                               (code-char #xD8))
                                         'string))))
  (let ((name (concatenate 'string (string start) rest)))
    (expect (parsed-root-text
             (format nil "<!DOCTYPE r [<!ENTITY ~A 'v'>]><r>&~A;</r>" name name))
            :to-equal "v")))
