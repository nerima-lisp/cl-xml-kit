(in-package #:cl-xml-kit/test)

(it "accepts XML declarations, doctypes, namespaces, CDATA, and PIs"
  (let* ((children (xml-document-children
                    (parse-xml
                     "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><!DOCTYPE root><root xmlns:p=\"urn:p\"><p:item><![CDATA[x<y]]></p:item><?done value?></root>")))
         (item (first (xml-element-children (second children)))))
    (expect (list (xml-doctype-p (first children)) (xml-element-p (second children))
                  (length children))
            :to-equal '(t t 2))
    (expect (xml-name-namespace-uri (xml-element-name item)) :to-equal "urn:p")
    (expect (text-content item) :to-equal "x<y")))

(it-each (("<r/>" "")
          ("<r ></r >" "")
          ("<r>&lt;&gt;&amp;&apos;&quot;</r>" "<>&'\"")
          ("<r><![CDATA[]]></r>" "")
          ("<r><![CDATA[<&]]>]]&gt;</r>" "<&]]>"))
    "accepts the well-formed document ~S with root text ~S" (input text)
  (expect (text-content (document-element input)) :to-equal text))

(it "normalizes literal line ends to line feeds but keeps a referenced carriage return"
  (expect (text-content (document-element
                         (format nil "<r>a~C~Cb~Cc</r>" #\Return #\Newline #\Return)))
          :to-equal (format nil "a~%b~%c"))
  (expect (text-content (document-element "<r>a&#13;b</r>"))
          :to-equal (format nil "a~Cb" #\Return)))

(it-each (("<" (:well-formedness nil) "Unexpected end of XML input." 1)
          ("<root>" (:well-formedness :end-tag) "Unexpected end of document." 6)
          ("</root>" (:element-type-match :start-tag) "An end tag has no open element." 0)
          ("<root a=\"1\" a=\"2\"/>" (:unique-att-spec "a unique attribute name")
           "An attribute name occurs more than once." 1)
          ("<root><![CDATA[bad]]></root>tail" (:well-formedness :root-element)
           "Non-whitespace text is outside the root." 28)
          ("<r>]]></r>" (:character-data nil)
           "The character sequence ]]> is not allowed in text." 6))
    "rejects the malformed document ~S" (input constraint-and-expected message offset)
  (expect (lambda () (parse-xml input))
          :to-signal-xml `(xml-well-formedness-error
                           (xml-well-formedness-error-constraint ,(first constraint-and-expected))
                           (xml-well-formedness-error-expected ,(second constraint-and-expected))
                           (xml-condition-message ,message)
                           (condition-offset ,offset))))
