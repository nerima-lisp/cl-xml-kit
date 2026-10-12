(in-package #:cl-xml-kit/test)

(it "does not resolve external entities"
  (expect (handler-case
              (progn
                (parse-xml "<!DOCTYPE r [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]><r>&xxe;</r>")
                nil)
            (xml-external-resource-disabled () t))
          :to-equal t))

(it "skips external entities only with the explicit skip policy"
  (let ((document
          (parse-xml "<!DOCTYPE r [<!ENTITY xxe SYSTEM \"http://127.0.0.1/\">]><r>a&xxe;b</r>"
                     :external-entity-policy :skip)))
    (expect (xml-text-data
             (first (xml-element-children
                     (second (xml-document-children document)))))
            :to-equal "ab")))

(it "rejects recursive entities and bounded expansion blowup"
  (expect (handler-case
              (progn (parse-xml "<!DOCTYPE r [<!ENTITY x \"&x;\">]><r>&x;</r>") nil)
            (xml-well-formedness-error () t)) :to-equal t)
  (let ((limits (make-xml-resource-limits :max-expanded-characters 4)))
    (expect (handler-case
                (progn
                  (parse-xml "<!DOCTYPE r [<!ENTITY x \"12345\">]><r>&x;</r>"
                             :resource-limits limits)
                  nil)
              (xml-resource-limit-exceeded () t))
            :to-equal t)))

(it "enforces depth and attribute-count limits"
  (let ((limits (make-xml-resource-limits :max-depth 2
                                           :max-attributes-per-element 1)))
    (expect (handler-case
                (progn (parse-xml "<a><b><c/></b></a>" :resource-limits limits) nil)
              (xml-resource-limit-exceeded () t)) :to-equal t)
    (expect (handler-case
                (progn (parse-xml "<a x=\"1\" y=\"2\"/>" :resource-limits limits) nil)
              (xml-resource-limit-exceeded () t)) :to-equal t)))

