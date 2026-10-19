(in-package #:cl-xml-kit/test)

(describe "XML declaration"
  (it-each (("<?xml version=\"1.0\" ?><r/>")
            ("<?xml version = '1.0'?><r/>")
            ("<?xml version='1.0' encoding='utf-8'?><r/>")
            ("<?xml version='1.0' encoding='UTF-8' ?><r/>")
            ("<?xml version='1.0'  standalone = \"no\" ?><r/>")
            ("<?xml version='1.0' encoding='UTF-8' standalone='yes'?><r/>"))
      "accepts the declaration in ~S without adding a node" (input)
    (expect (mapcar #'xml-element-p (xml-document-children (parse-xml input)))
            :to-equal '(t)))

  (it-each (("<?xmlfoo bar?><r/>" "xmlfoo" "bar")
            ("<?xml-stylesheet href='a'?><r/>" "xml-stylesheet" "href='a'"))
      "reads ~S as a processing instruction, since only the exact target xml is reserved"
      (input target data)
    (let ((instruction (first (xml-document-children (parse-xml input)))))
      (expect (list (xml-processing-instruction-target instruction)
                    (xml-processing-instruction-data instruction))
              :to-equal (list target data))))

  (it "rejects XML declarations outside the UTF-8 profile"
    (expect (lambda () (parse-xml "<?xml version=\"1.0\" encoding=\"UTF-16\"?><root/>"))
            :to-signal-xml '(xml-unsupported-encoding
                             (xml-unsupported-encoding-encoding "UTF-16")
                             (xml-unsupported-encoding-reason :declaration-not-utf-8))))

  (it-each ((" <?xml version=\"1.0\"?><root/>" :document-start
             "The XML declaration must be first." 1)
            ("<r/><?xml version='1.0'?>" :document-start
             "The XML declaration must be first." 4)
            ("<?xml?><r/>" :version
             "XML declaration version information is required." 0)
            ("<?xml encoding=\"UTF-8\" version=\"1.0\"?><root/>" "version"
             "Expected an XML declaration keyword." 0)
            ("<?xml version '1.0'?><r/>" :equals
             "Expected '=' in the XML declaration." 0)
            ("<?xml version?><r/>" :equals
             "Expected '=' in the XML declaration." 0)
            ("<?xml version=1.0?><r/>" :quoted-value
             "Expected a quoted XML declaration value." 0)
            ("<?xml version=?><r/>" :quoted-value
             "Expected a quoted XML declaration value." 0)
            ("<?xml version='1.0?><r/>" #\'
             "An XML declaration value is unterminated." 0)
            ("<?xml version=\"1.1\"?><root/>" "1.0"
             "Only XML version 1.0 is supported." 0)
            ("<?xml version='1.0'encoding='UTF-8'?><r/>" :xml-whitespace
             "XML declaration attributes must be separated by XML whitespace." 0)
            ("<?xml version='1.0' bogus='x'?><r/>" (:encoding :standalone)
             "Expected an XML declaration keyword." 0)
            ("<?xml version='1.0' encoding='UTF-8' bogus='x'?><r/>" "standalone"
             "Expected an XML declaration keyword." 0)
            ("<?xml version=\"1.0\" standalone=\"maybe\"?><root/>" ("yes" "no")
             "Standalone must be either 'yes' or 'no'." 0)
            ("<?xml version='1.0' standalone='yes' x?><r/>" :end
             "Unexpected content in the XML declaration." 0)
            ("<?xml version='1.0' encoding='UTF-8' standalone='no' junk?><r/>" :end
             "Unexpected content in the XML declaration." 0))
      "rejects the XML declaration of ~S" (input expected message offset)
    (expect (lambda () (parse-xml input))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :xml-declaration)
                             (xml-well-formedness-error-expected ,expected)
                             (xml-condition-message ,message)
                             (condition-offset ,offset)))))

(describe "DOCTYPE"
  (it "accepts an external DTD under the skip policy without reading it"
    (let ((doctype (first (xml-document-children
                           (parse-xml "<!DOCTYPE r SYSTEM 'x.dtd'><r/>"
                                      :external-entity-policy :skip)))))
      (expect (xml-doctype-root-name doctype) :to-equal "r")))

  (it-each (("<!DOCTYPE r SYSTEM 'x.dtd'><r/>")
            ("<!DOCTYPE r PUBLIC 'p' 's'><r/>"))
      "refuses the external DTD of ~S under the default policy" (input)
    (expect (lambda () (parse-xml input))
            :to-signal-xml '(xml-external-resource-disabled
                             (xml-external-resource-disabled-identifier "r")
                             (xml-external-resource-disabled-kind :external-dtd)
                             (condition-offset 0))))

  (it-each (("<r/><!DOCTYPE r>" :doctype :document-prolog 4)
            ("<!DOCTYPE r><!DOCTYPE r><r/>" :doctype :single-doctype 12)
            ("<!DOCTYPE r><s/>" :doctype-root-match "r" 13))
      "rejects the DOCTYPE placement or root of ~S" (input constraint expected offset)
    (expect (lambda () (parse-xml input))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint ,constraint)
                             (xml-well-formedness-error-expected ,expected)
                             (condition-offset ,offset)))))

(describe "top level"
  (it "emits comments, processing instructions, and trailing space around the root"
    (expect (mapcar (lambda (event)
                      (list (xml-event-type event) (xml-event-text event)
                            (xml-event-target event)))
                    (pull-events "<!--c--><?p d?><r/><!--e--> "))
            :to-equal '((:start-document nil nil) (:comment "c" nil)
                        (:processing-instruction "d" "p") (:start-element nil nil)
                        (:end-element nil nil) (:comment "e" nil)
                        (:characters " " nil) (:end-document nil nil))))

  (it "rejects a skipped external entity reference after an empty root"
    (expect (lambda ()
              (pull-events (concatenate 'string +skip-doctype+ "<r/>&x;")
                           :external-entity-policy :skip))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :well-formedness)
                             (xml-well-formedness-error-expected :root-element)
                             (condition-offset ,(+ (length +skip-doctype+) 4)))))

  (it-each (("<r/><s/>" :single-root "A document has one root element." 4)
            ("" :root-element "A document needs a root element." 0)
            ("<![CDATA[x]]><r/>" :root-element "CDATA is outside the root." 0)
            ("&amp;<r/>" :root-element "An entity is outside the root." 0)
            ("<r/>&amp;" :root-element "An entity is outside the root." 4)
            ("<r/> &amp;" :root-element "An entity is outside the root." 5)
            ("<r/>&lt;x" :root-element "An entity is outside the root." 4)
            ("<r></r>&amp;" :root-element "An entity is outside the root." 7)
            ("text<r/>" :root-element "Non-whitespace text is outside the root." 0)
            ("<r/>text" :root-element "Non-whitespace text is outside the root." 4))
      "rejects the top-level content of ~S" (input expected message offset)
    (expect (lambda () (parse-xml input))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :well-formedness)
                             (xml-well-formedness-error-expected ,expected)
                             (xml-condition-message ,message)
                             (condition-offset ,offset))))

  (it "rejects an end tag with no open element"
    (expect (lambda () (parse-xml "</r>"))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :element-type-match)
                             (xml-well-formedness-error-expected :start-tag)
                             (condition-offset 0))))

  (it-each (("<?a:b data?><r/>" "TARGET must not contain a colon.")
            ("<!DOCTYPE a:r><a:r xmlns:a=\"urn:a\"/>" "ROOT-NAME must not contain a colon."))
      "rejects the colon in ~S while pulling, as PARSE-XML does" (input message)
    (expect (lambda () (pull-events input))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message)))
    (expect (lambda () (parse-xml input))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message)))))
