(in-package #:cl-xml-kit/test)

(describe "expansion limits"
  (it-each (("<!ENTITY one 'x'>"
             (:max-entity-expansions 1 :max-expanded-characters 1 :max-entity-depth 1)
             "&one;&one;" :max-entity-expansions 2 1)
            ("<!ENTITY two 'xx'>" (:max-expanded-characters 1) "&two;"
             :max-expanded-characters 2 1)
            ("<!ENTITY one '&two;'><!ENTITY two 'x'>" (:max-entity-depth 1) "&one;"
             :max-entity-depth 2 1)
            ("" (:max-name-length 3) "&long-name;" :max-name-length 9 3))
      "with the subset ~S and limits ~{~S~^ ~} stops ~S at ~S ~S > ~S"
      (subset limits text kind value limit)
    (let ((context (cl-xml-kit::parse-xml-internal-subset
                    subset :resource-limits (apply #'make-xml-resource-limits limits))))
      (expect (lambda () (cl-xml-kit::expand-xml-entity-references text context))
              :to-signal-xml `(xml-resource-limit-exceeded
                               (xml-resource-limit-exceeded-kind ,kind)
                               (xml-resource-limit-exceeded-value ,value)
                               (xml-resource-limit-exceeded-limit ,limit)))))

  (it-each (("<!ENTITY one 'x'>" (:max-entity-expansions 2) "&one;&one;" "xx")
            ("<!ENTITY two 'xx'>" (:max-expanded-characters 2) "&two;" "xx")
            ("<!ENTITY one '&two;'><!ENTITY two 'x'>" (:max-entity-depth 2) "&one;" "x")
            ("<!ENTITY abc 'x'>" (:max-name-length 3) "&abc;" "x"))
      "with the subset ~S and limits ~{~S~^ ~} expands ~S to ~S exactly at the limit"
      (subset limits text expected)
    (expect (cl-xml-kit::expand-xml-entity-references
             text (cl-xml-kit::parse-xml-internal-subset
                   subset :resource-limits (apply #'make-xml-resource-limits limits)))
            :to-equal expected))

  ;; Expansions are counted in document order: c, b, five a, b, then the third a
  ;; of the second b is the eleventh.
  (it-each (("<!DOCTYPE r [<!ENTITY x \"12345\">]><r>&x;</r>" (:max-expanded-characters 4)
             :max-expanded-characters 5 4)
            ("<!DOCTYPE r [<!ENTITY a 'x'><!ENTITY b '&a;&a;&a;&a;&a;'><!ENTITY c '&b;&b;&b;&b;&b;'>]><r>&c;</r>"
             (:max-entity-expansions 10) :max-entity-expansions 11 10)
            ("<!DOCTYPE r [<!ENTITY % p ''>%p;%p;]><r/>" (:max-entity-expansions 1)
             :max-entity-expansions 2 1)
            ("<!DOCTYPE r [<!ENTITY abcd 'v'>]><r/>" (:max-name-length 3)
             :max-name-length 4 3))
      "bounds the expansion of ~S under ~{~S~^ ~} at ~S ~S > ~S"
      (document limits kind value limit)
    (expect (lambda ()
              (parse-xml document :resource-limits (apply #'make-xml-resource-limits limits)))
            :to-signal-xml `(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind ,kind)
                             (xml-resource-limit-exceeded-value ,value)
                             (xml-resource-limit-exceeded-limit ,limit)))))

;; WFC: No Recursion (XML 1.0 section 4.1).
(describe "recursive entities"
  (it-each (("<!DOCTYPE r [<!ENTITY x \"&x;\">]><r>&x;</r>" "x")
            ("<!DOCTYPE r [<!ENTITY a '&b;'><!ENTITY b '&a;'>]><r>&a;</r>" "a")
            ("<!DOCTYPE r [<!ENTITY x '&x;'>]><r a='&x;'/>" "x")
            ("<!DOCTYPE r [<!ENTITY % p '%p;'>%p;]><r/>" "p"))
      "rejects ~S because entity ~S refers to itself"
      (document name)
    (expect (lambda () (parse-xml document))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :no-recursion)
                             (xml-well-formedness-error-expected ,name)
                             (xml-condition-message "Recursive XML entity reference."))))

  (it "rejects a declared entity that refers to itself during expansion"
    (let ((context (cl-xml-kit::make-xml-entity-context)))
      (cl-xml-kit::xml-entity-context-declare
       context (cl-xml-kit::make-xml-entity-declaration
                :name "loop" :replacement-text "&loop;"))
      (expect (lambda () (cl-xml-kit::expand-xml-entity-references "&loop;" context))
              :to-signal-xml '(xml-well-formedness-error
                               (xml-well-formedness-error-constraint :no-recursion)
                               (xml-well-formedness-error-expected "loop")))))

  (it "allows the same entity twice in sequence because neither use is nested"
    (expect (parsed-root-text "<!DOCTYPE r [<!ENTITY a 'x'><!ENTITY b '&a;&a;'>]><r>&b;&b;</r>")
            :to-equal "xxxx")))

(describe "external entities in documents"
  ;; Only the type is pinned: the parser refuses the DOCTYPE itself, with kind
  ;; :EXTERNAL-DTD, before the reference is reached.
  (it "does not resolve external entities"
    (expect (lambda ()
              (parse-xml
               "<!DOCTYPE r [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]><r>&xxe;</r>"))
            :to-signal-xml '(xml-external-resource-disabled)))

  (it "skips external entities only with the explicit skip policy"
    (expect (parsed-root-text
             "<!DOCTYPE r [<!ENTITY xxe SYSTEM \"http://127.0.0.1/\">]><r>a&xxe;b</r>"
             :external-entity-policy :skip)
            :to-equal "ab")))
