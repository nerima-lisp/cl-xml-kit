(in-package #:cl-xml-kit/test)

(describe "expansion in text"
  (it "refuses text that is not a string"
    (expect (lambda ()
              (cl-xml-kit::expand-xml-entity-references
               42 (cl-xml-kit::make-xml-entity-context)))
            :to-signal-xml '(xml-input-error
                             (xml-condition-message "ENTITY TEXT must be a string."))))

  (it "refuses a context that is not an entity context"
    (expect (lambda () (cl-xml-kit::expand-xml-entity-references "x" :nope))
            :to-signal-xml '(xml-input-error
                             (xml-condition-message "CONTEXT must be an XML entity context."))))

  (it-each (("<!ENTITY greeting \"hello &amp; world\">" "before &greeting; after"
             "before hello & world after")
            ("<!ENTITY a '[&b;]'><!ENTITY b 'x'>" "&a;" "[x]")
            ("<!ENTITY e 'a&#65;b'>" "&e;" "aAb")
            ("" "plain text" "plain text"))
      "with the subset ~S expands ~S to ~S"
      (subset text expected)
    (expect (cl-xml-kit::expand-xml-entity-references
             text (cl-xml-kit::parse-xml-internal-subset subset))
            :to-equal expected))

  (it "expands parameter entities while collecting declarations"
    (expect (cl-xml-kit::expand-xml-entity-references
             "&answer;"
             (cl-xml-kit::parse-xml-internal-subset
              "<!ENTITY % declaration \"<!ENTITY answer '42'>\"> %declaration;"))
            :to-equal "42")))

;; WFC: No < in Attribute Values (XML 1.0 section 3.1) covers replacement text
;; referred to directly or indirectly; a generated & would start a reference.
(describe "expansion in attribute values"
  (it-each (("<!ENTITY e '&#60;'>" "&e;" :no-lt-in-attribute-values "a character other than <")
            ("<!ENTITY e \"&#38;\">" "&e;" :no-amp-in-attribute-values "a character other than &")
            ("<!ENTITY outer '[&inner;]'><!ENTITY inner '&#60;'>" "&outer;"
             :no-lt-in-attribute-values "a character other than <")
            ("<!ENTITY outer '[&inner;]'><!ENTITY inner 'a<b'>" "&outer;"
             :no-lt-in-attribute-values "a character other than <")
            ("<!ENTITY outer 'x<y'>" "&outer;"
             :no-lt-in-attribute-values "a character other than <"))
      "with the subset ~S rejects ~S in an attribute as ~S"
      (subset text constraint expected)
    (expect (lambda ()
              (cl-xml-kit::expand-xml-entity-references
               text (cl-xml-kit::parse-xml-internal-subset subset) :attribute-p t))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint ,constraint)
                             (xml-well-formedness-error-expected ,expected))))

  (it "rejects an entity with less-than when PARSE-XML reads an attribute"
    (expect (lambda () (parse-xml "<!DOCTYPE r [<!ENTITY a 'x<y'>]><r b='&a;'/>"))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :no-lt-in-attribute-values))))

  (it "accepts the predefined lt and less-than character references in an attribute value"
    (expect (cl-xml-kit::expand-xml-entity-references
             "a&lt;b&#60;c&#x3C;d" (cl-xml-kit::make-xml-entity-context) :attribute-p t)
            :to-equal "a<b<c<d"))

  (it "expands an internal entity inside a parsed attribute value"
    (expect (parsed-root-attribute "<!DOCTYPE r [<!ENTITY a 'x'>]><r b='[&a;]'/>" "b")
            :to-equal "[x]")))

(describe "expansion in content"
  (it "parses markup that a character reference in the replacement text produces"
    (let ((root (parsed-root "<!DOCTYPE r [<!ENTITY e '&#60;b/>'>]><r>&e;</r>")))
      (expect (mapcar (lambda (child) (xml-name-local-name (xml-element-name child)))
                      (xml-element-children root))
              :to-equal '("b"))))

  (it "parses markup that a nested entity brings into content"
    (let ((root (parsed-root
                 "<!DOCTYPE r [<!ENTITY a '[&b;]'><!ENTITY b '<i/>'>]><r>&a;</r>")))
      (expect (mapcar (lambda (child)
                        (if (xml-element-p child)
                            (xml-name-local-name (xml-element-name child))
                            (xml-text-data child)))
                      (xml-element-children root))
              :to-equal '("[" "i" "]"))))

  ;; The replacement text of '&#38;' is a lone ampersand (XML 1.0 section 4.4.5),
  ;; which is not a well-formed reference once it is included in content.
  (it "rejects a replacement text that is a lone ampersand"
    (expect (lambda () (parse-xml "<!DOCTYPE r [<!ENTITY e '&#38;'>]><r>&e;</r>"))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-reference)
                             (xml-condition-message
                              "An entity replacement generated an unescaped ampersand."))))

  (it "drops a skipped external entity that an internal entity refers to"
    (expect (parsed-root-text
             "<!DOCTYPE r [<!ENTITY a SYSTEM 'u'><!ENTITY b '[&a;]'>]><r>&b;</r>"
             :external-entity-policy :skip)
            :to-equal "[]")))

;; The generator emits each piece with its own expected character, so the oracle
;; is the table below rather than the expander. Plain pieces avoid & and ; so
;; that every & in the input starts one of the listed references.
(it-property "expands any mix of plain text and character or predefined references"
    ((pieces (gen-list (gen-member '(("a" . "a") ("Z" . "Z") (" " . " ") ("<" . "<")
                                     ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                                     ("&apos;" . "'") ("&quot;" . "\"")
                                     ("&#65;" . "A") ("&#x42;" . "B") ("&#x3c;" . "<")
                                     ("&#x10000;" . #.(string (code-char #x10000)))))
                       :max-length 12)))
  (expect (cl-xml-kit::expand-xml-entity-references
           (format nil "~{~A~}" (mapcar #'car pieces))
           (cl-xml-kit::make-xml-entity-context))
          :to-equal (format nil "~{~A~}" (mapcar #'cdr pieces))))
