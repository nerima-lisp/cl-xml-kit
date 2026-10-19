(in-package #:cl-xml-kit/test)

(describe "entity declarations"
  (it-each (("<!ENTITY e 'x" "Unterminated XML entity declaration.")
            ("<!ENTITY e SYSTEM 'x" "Unterminated XML entity declaration.")
            ("<!ENTITY" "An entity declaration needs a valid name.")
            ("<!ENTITY " "An entity declaration needs a valid name.")
            ("<!ENTITY % " "An entity declaration needs a valid name.")
            ("<!ENTITY e" "An entity name must be separated from its declaration value.")
            ("<!ENTITY e'x'>" "An entity name must be separated from its declaration value.")
            ("<!ENTITY e " "An entity declaration is incomplete.")
            ("<!ENTITY e 'x' junk>" "An entity declaration must end with '>'.")
            ("<!ENTITY e 'x'" "An entity declaration must end with '>'.")
            ("<!ENTITY e NDATA>" "An entity declaration needs a quoted value.")
            ("<!ENTITY e SYSTEM>" "An external entity needs a system identifier.")
            ("<!ENTITY e PUBLIC '-//P'>" "PUBLIC needs a system identifier.")
            ("<!ENTITY e PUBLIC '-//P' 's'" "An external entity declaration is incomplete.")
            ("<!ENTITY e SYSTEM 's'" "An external entity declaration is incomplete."))
      "rejects the declaration ~S saying ~S"
      (subset message)
    (expect (lambda () (cl-xml-kit::parse-xml-internal-subset subset))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-declaration)
                             (xml-well-formedness-error-expected nil)
                             (xml-condition-message ,message))))

  (it-each (("<!ENTITY e PUBLIC '-//P' 's'>" ("-//P" "s" t))
            ("<!ENTITY e PUBLIC \"-//P\" \"s\" >" ("-//P" "s" t))
            ("<!ENTITY e SYSTEM 's'>" (nil "s" t))
            ("<!ENTITY e SYSTEM 's' NDATA n>" (nil "s" t)))
      "reads the external identifiers of ~S as ~{~S~^ ~}"
      (subset expected)
    (let ((declaration (subset-declaration subset "e")))
      (expect (list (cl-xml-kit::xml-entity-declaration-public-id declaration)
                    (cl-xml-kit::xml-entity-declaration-system-id declaration)
                    (cl-xml-kit::xml-entity-declaration-external-p declaration))
              :to-equal expected)))

  (it "keeps general and parameter entities of the same name apart"
    (let ((context (cl-xml-kit::parse-xml-internal-subset
                    "<!ENTITY a 'general'><!ENTITY % a '<!ENTITY b \"parameter\">'>%a;")))
      (expect (cl-xml-kit::expand-xml-entity-references "&a;&b;" context)
              :to-equal "generalparameter")))

  (it "applies the name-length limit to a declared name"
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset
               "<!ENTITY abcd 'v'>"
               :resource-limits (make-xml-resource-limits :max-name-length 3)))
            :to-signal-xml '(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind :max-name-length)
                             (xml-resource-limit-exceeded-value 4)
                             (xml-resource-limit-exceeded-limit 3))))

  (it "reports the caller's source name on a refused declaration"
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset "<!ENTITY" :source "dtd.xml"))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-condition-source "dtd.xml")))))

(describe "other internal subset markup"
  (it-each (("<!-- unterminated" :comment
             "Unterminated XML comment in the internal subset.")
            ("<!ELEMENT r ANY" :markup-declaration "Unterminated markup declaration.")
            ("<!ATTLIST r a CDATA '>" :markup-declaration "Unterminated markup declaration.")
            ("<![INCLUDE[ ]]>" :markup-declaration
             "Conditional sections are not supported in the internal subset."))
      "rejects ~S with constraint ~S saying ~S"
      (subset constraint message)
    (expect (lambda () (cl-xml-kit::parse-xml-internal-subset subset))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint ,constraint)
                             (xml-condition-message ,message))))

  (it "refuses a subset that is not a string"
    (expect (lambda () (cl-xml-kit::parse-xml-internal-subset 42))
            :to-signal-xml '(xml-input-error
                             (xml-condition-message "INTERNAL-SUBSET must be a string."))))

  (it-each (("<!-- <!ENTITY e 'x'> --><!ENTITY f 'y'>")
            ("<!ELEMENT r ANY><!ATTLIST r a CDATA '>'><!ENTITY f 'y'>")
            ("<!NOTATION n SYSTEM \"<!ENTITY e 'x'>\"> <!ENTITY f 'y'>")
            ("<?pi data?><!ENTITY f 'y'>"))
      "declares only the entity outside comments and other declarations in ~S"
      (subset)
    (expect (list (subset-declaration subset "e")
                  (cl-xml-kit::xml-entity-declaration-replacement-text
                   (subset-declaration subset "f")))
            :to-equal '(nil "y")))

  (it "records the element names that ATTLIST declarations govern"
    (expect (sort (copy-list
                   (cl-xml-kit::xml-entity-context-attlist-element-names
                    (cl-xml-kit::parse-xml-internal-subset
                     "<!ATTLIST r a CDATA '>'><!ATTLIST  q b CDATA #IMPLIED><!ATTLIST><!ATTLIST r c CDATA #IMPLIED><!ATTLIST 1 c CDATA #IMPLIED>")))
                  #'string<)
            :to-equal '("q" "r")))
  (it "returns false for a quote check at the end of input"
    (expect (cl-xml-kit::%quote-at-p "" 0) :to-be nil))
  (it "ignores an ATTLIST declaration with no element name"
    (expect (cl-xml-kit::xml-entity-context-attlist-element-names
             (cl-xml-kit::parse-xml-internal-subset "<!ATTLIST >"))
            :to-be nil)))

(describe "parameter entity references"
  (it-each (("%p" :entity-reference ";")
            ("%undeclared;" :entity-declared "undeclared"))
      "rejects the subset ~S with constraint ~S and expected ~S"
      (subset constraint expected)
    (expect (lambda () (cl-xml-kit::parse-xml-internal-subset subset))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint ,constraint)
                             (xml-well-formedness-error-expected ,expected))))

  (it "refuses an external parameter entity under the reject policy"
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset "<!ENTITY % p SYSTEM 'x.dtd'>%p;"))
            :to-signal-xml '(xml-external-resource-disabled
                             (xml-external-resource-disabled-kind :parameter-entity)
                             (xml-external-resource-disabled-identifier "x.dtd")
                             (xml-condition-message
                              "External parameter entity p is disabled."))))

  (it "records a skipped external parameter entity"
    (expect (cl-xml-kit::xml-entity-context-skipped-external-entities
             (cl-xml-kit::parse-xml-internal-subset
              "<!ENTITY % p SYSTEM 'x.dtd'>%p;" :external-entity-policy :skip))
            :to-equal '("p"))))
