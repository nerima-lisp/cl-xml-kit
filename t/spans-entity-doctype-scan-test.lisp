;;;; t/spans-entity-doctype-scan-test.lisp
;;;;
;;;; Prolog, DOCTYPE, and content-markup spans, and the internal entities whose
;;;; replacement text brings in markup.

(in-package #:cl-xml-kit/test)

(describe "prolog and DOCTYPE spans"
  (it "records the XML declaration span"
    (expect (spans-slice +spans-fixture+
                         (cl-xml-kit::span-document-declaration (spans-scan +spans-fixture+)))
            :to-equal "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))

  (it "records no XML declaration when the source has none"
    (expect (cl-xml-kit::span-document-declaration (spans-scan "<a/>")) :to-equal nil))

  (it "records the DOCTYPE span and its root name"
    (let ((doctype (cl-xml-kit::span-document-doctype (spans-scan +spans-fixture+))))
      (expect (spans-slice +spans-fixture+ doctype)
              :to-equal (spans-lines "<!DOCTYPE root [~%  <!ENTITY e \"text\">~%  <!ENTITY m \"<b/>\">~%  <!ATTLIST item id ID #IMPLIED>~%]>"
                                     :crlf))
      (expect (cl-xml-kit::span-doctype-root-name doctype) :to-equal "root")))

  (it "records the internal subset span in the original string"
    (let ((doctype (cl-xml-kit::span-document-doctype (spans-scan +spans-fixture+))))
      (expect (subseq +spans-fixture+
                      (cl-xml-kit::span-doctype-subset-start doctype)
                      (cl-xml-kit::span-doctype-subset-end doctype))
              :to-equal (spans-lines "~%  <!ENTITY e \"text\">~%  <!ENTITY m \"<b/>\">~%  <!ATTLIST item id ID #IMPLIED>~%"
                                     :crlf))))

  (it "reads the internal subset text with line ends normalized, as the DOM does"
    (let ((subset (cl-xml-kit::span-doctype-internal-subset
                   (cl-xml-kit::span-document-doctype (spans-scan +spans-fixture+)))))
      (expect subset
              :to-equal (spans-lines "~%  <!ENTITY e \"text\">~%  <!ENTITY m \"<b/>\">~%  <!ATTLIST item id ID #IMPLIED>~%"))
      (expect subset
              :to-equal (xml-doctype-internal-subset
                         (first (xml-document-children (parse-xml (subseq +spans-fixture+ 1))))))))

  (it "records no internal subset for a DOCTYPE without one"
    (let ((doctype (cl-xml-kit::span-document-doctype (spans-scan "<!DOCTYPE a><a/>"))))
      (expect (list (cl-xml-kit::span-doctype-internal-subset doctype)
                    (cl-xml-kit::span-doctype-subset-start doctype)
                    (cl-xml-kit::span-doctype-subset-end doctype))
              :to-equal '(nil nil nil))))

  (it "records the top-level nodes in order without the XML declaration"
    (let ((children (cl-xml-kit::span-document-children (spans-scan +spans-fixture+))))
      (expect (mapcar #'spans-kind children)
              :to-equal '(:doctype :comment :processing-instruction :element :comment))
      (expect (mapcar (lambda (node) (spans-slice +spans-fixture+ node))
                      (remove-if (lambda (node) (member (spans-kind node) '(:element :doctype)))
                                 children))
              :to-equal '("<!-- top comment -->" "<?pi data?>" "<!-- trailing -->")))))

(it "records comments and processing instructions inside the root"
  (let ((children (cl-xml-kit::span-element-children (spans-fixture-element "root"))))
    (expect (spans-slice +spans-fixture+ (find :comment children :key #'spans-kind))
            :to-equal (spans-lines "<!-- inner~%       comment -->" :crlf))
    (expect (spans-slice +spans-fixture+ (find :processing-instruction children :key #'spans-kind))
            :to-equal "<?inner pi?>")))

(describe "entities whose replacement text holds markup"
  (it "lists the internal entities whose replacement text holds markup"
    (expect (cl-xml-kit::span-document-markup-entity-names (spans-scan +spans-fixture+))
            :to-equal '("m")))

  (it-each (("<!DOCTYPE r [<!ENTITY t \"x\"><!ENTITY m \"<b/>\"><!ENTITY n \"a&m;\"><!ENTITY c \"&#60;i/>\"><!ENTITY w \"&t;\">]><r/>"
             ("c" "m" "n"))
            ("<!DOCTYPE r [<!ENTITY a \"&#65;&#x42;\"><!ENTITY q \"&amp;&gt;&quot;\"><!ENTITY h \"&#x3C;h/>\">]><r/>"
             ("h"))
            ("<!DOCTYPE r [<!ENTITY l \"a&lt;b\">]><r/>" ("l")))
      "lists the markup entities of ~S as ~{~A~^ ~}, following entity and character references"
      (source names)
    (expect (spans-accepted-p source) :to-be-truthy)
    (expect (cl-xml-kit::span-document-markup-entity-names (spans-scan source))
            :to-equal names))

  (it "rejects a reference to an entity whose value holds a predefined lt reference, as markup"
    (expect (spans-accepted-p "<!DOCTYPE r [<!ENTITY l \"a&lt;b\">]><r>&l;</r>") :to-equal nil))

  (it "terminates on cyclic and long chains of entity declarations"
    (let ((source (with-output-to-string (stream)
                    (write-string "<!DOCTYPE r [<!ENTITY a \"&b;\"><!ENTITY b \"&a;<x/>\">" stream)
                    (loop for index from 0 below 3000
                          do (format stream "<!ENTITY c~D \"&c~D;\">" index (1+ index)))
                    (write-string "<!ENTITY c3000 \"<z/>\">]><r/>" stream))))
      (expect (spans-accepted-p source) :to-be-truthy)
      (expect (length (cl-xml-kit::span-document-markup-entity-names (spans-scan source)))
              :to-equal 3003)))

  (it "ignores external entity declarations when listing markup entities"
    (let ((source "<!DOCTYPE r [<!ENTITY x SYSTEM \"x.ent\"><!ENTITY y \"<b/>\">]><r/>"))
      (expect (spans-accepted-p source :external-entity-policy :skip) :to-be-truthy)
      (expect (cl-xml-kit::span-document-markup-entity-names
               (spans-scan source :external-entity-policy :skip))
              :to-equal '("y"))))

  (it "reports a reference to a markup entity that the DOM expands into elements"
    (let* ((source "<!DOCTYPE r [<!ENTITY m \"<b/>\">]><r>&m;</r>")
           (document (spans-scan source))
           (children (cl-xml-kit::span-element-children (cl-xml-kit::span-document-root document))))
      (expect (list (mapcar #'spans-kind children)
                    (cl-xml-kit::span-text-entity-names (first children))
                    (cl-xml-kit::span-document-markup-entity-names document))
              :to-equal '((:text) ("m") ("m")))
      (expect (mapcar #'xml-element-p
                      (xml-element-children
                       (find-if #'xml-element-p (xml-document-children (parse-xml source)))))
              :to-equal '(t)))))
