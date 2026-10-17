;;;; t/path-test.lisp
;;;;
;;;; SELECT-XML: element, attribute, and text matches, their canonical paths,
;;;; and their spans in the caller's string.

(in-package #:cl-xml-kit/test)

(defparameter +path-document+
  (spans-lines "<root xmlns=\"urn:default\" xmlns:p=\"urn:p\" a=\"1\">~%  <item id=\"1\">one</item>~%  <item id=\"2\">two</item>~%  <p:item p:id=\"x\">pre</p:item>~%  <group><item id=\"3\">three</item><empty/></group>~%  <item>three-b</item>~%</root>"
               :crlf)
  "A CRLF document with default and prefixed namespaces and repeated element names.")

(describe "element matches"
  (it "selects the document element with its span and DOM element"
    (let ((match (first (select-xml +path-document+ "/root"))))
      (expect (list (xml-match-kind match) (xml-match-path match) (xml-match-name match)
                    (xml-match-value match) (xml-match-start match) (xml-match-end match))
              :to-equal (list :element "/root[1]" "root" nil 0 (length +path-document+)))
      (expect (xml-name-local-name (xml-element-name (xml-match-element match)))
              :to-equal "root")))

  (it "selects child elements in document order with canonical paths"
    (expect (path-summaries +path-document+ "/root/*")
            :to-equal '((:element "/root[1]/item[1]") (:element "/root[1]/item[2]")
                        (:element "/root[1]/p:item[1]") (:element "/root[1]/group[1]")
                        (:element "/root[1]/item[3]"))))

  (it "reports the source span of each selected element"
    (expect (path-slices +path-document+ "/root/item")
            :to-equal '("<item id=\"1\">one</item>" "<item id=\"2\">two</item>"
                        "<item>three-b</item>")))

  (it-each (("/root/item[2]" ("<item id=\"2\">two</item>"))
            ("/root/item[3]" ("<item>three-b</item>"))
            ("/root/*[3]" ("<p:item p:id=\"x\">pre</p:item>"))
            ("/root/item[4]" nil))
      "counts the index of ~S among the children that pass the name test"
      (path slices)
    (expect (path-slices +path-document+ path) :to-equal slices))

  (it-each (("/other" ())
            ("/root[2]" ())
            ("/*" ("/root[1]"))
            ("/root[1]" ("/root[1]")))
      "tests the document element against the first step only, so ~S selects [~{~A~}]"
      (path canonical-paths)
    (expect (path-summaries +path-document+ path)
            :to-equal (mapcar (lambda (canonical) (list :element canonical)) canonical-paths)))

  (it "compares element names as written, prefix included"
    (expect (path-summaries +path-document+ "/root/p:item")
            :to-equal '((:element "/root[1]/p:item[1]")))
    (expect (length (select-xml +path-document+ "/root/item")) :to-equal 3))

  (it "descends through nested steps with an index on every canonical step"
    (expect (path-summaries +path-document+ "/root/group/item/@id")
            :to-equal '((:attribute "/root[1]/group[1]/item[1]/@id")))
    (expect (path-summaries +path-document+ "/root/group/*")
            :to-equal '((:element "/root[1]/group[1]/item[1]")
                        (:element "/root[1]/group[1]/empty[1]"))))

  (it "maps a nested match to its DOM element"
    (let ((match (first (select-xml +path-document+ "/root/group/item"))))
      (expect (xml-attribute-value (first (xml-element-attributes (xml-match-element match))))
              :to-equal "3"))))

(describe "attribute matches"
  (it "selects only the attributes written with the name, skipping elements without it"
    (expect (path-summaries +path-document+ "/root/item/@id")
            :to-equal '((:attribute "/root[1]/item[1]/@id") (:attribute "/root[1]/item[2]/@id")))
    (expect (mapcar #'xml-match-value (select-xml +path-document+ "/root/item/@id"))
            :to-equal '("1" "2")))

  (it "spans an attribute from its name through its closing quote"
    (expect (path-slices +path-document+ "/root/item/@id")
            :to-equal '("id=\"1\"" "id=\"2\"")))

  (it "reports the lexical name of an attribute and its owning DOM element"
    (let ((match (first (select-xml +path-document+ "/root/p:item/@p:id"))))
      (expect (list (xml-match-name match) (xml-match-value match)) :to-equal '("p:id" "x"))
      (expect (xml-name-local-name (xml-element-name (xml-match-element match)))
              :to-equal "item")))

  (it-each (("/root/@xmlns" nil)
            ("/root/@xmlns:p" nil)
            ("/root/@a" ("a=\"1\"")))
      "never selects a namespace declaration as an attribute, so ~S selects ~S"
      (path slices)
    (expect (path-slices +path-document+ path) :to-equal slices)))

(describe "text matches"
  (it "selects the direct character content of every matched element"
    (let ((matches (select-xml +path-document+ "/root/item/text()")))
      (expect (mapcar #'xml-match-value matches) :to-equal '("one" "two" "three-b"))
      (expect (mapcar #'xml-match-path matches)
              :to-equal '("/root[1]/item[1]/text()" "/root[1]/item[2]/text()"
                          "/root[1]/item[3]/text()"))
      (expect (path-slices +path-document+ "/root/item/text()")
              :to-equal '("one" "two" "three-b"))))

  (it "selects empty text for an element without content, at the slash of an empty tag"
    (let ((match (first (select-xml +path-document+ "/root/group/empty/text()"))))
      (expect (list (xml-match-value match) (- (xml-match-end match) (xml-match-start match)))
              :to-equal '("" 0))
      (expect (char +path-document+ (xml-match-start match)) :to-equal #\/)))

  (it "spans the whole content of an element that has child elements"
    (let ((match (first (select-xml +path-document+ "/root/group/text()"))))
      (expect (xml-match-value match) :to-equal "")
      (expect (subseq +path-document+ (xml-match-start match) (xml-match-end match))
              :to-equal "<item id=\"3\">three</item><empty/>")))

  (it "concatenates the direct text around child elements and decodes references"
    (let ((source "<m v=\"a&amp;b&#65;\">x&lt;<b/>y<![CDATA[<z>]]>&#x42;</m>"))
      (expect (xml-match-value (first (select-xml source "/m/text()"))) :to-equal "x<y<z>B")
      (expect (xml-match-value (first (select-xml source "/m/@v"))) :to-equal "a&bA"))))

(it "resolves the canonical path of every match back to exactly that match"
  (let ((matches (loop for path in '("/root/*" "/root/group/*" "/root/item/@id" "/root/item/text()")
                       append (select-xml +path-document+ path))))
    (expect (length matches) :to-equal 12)
    (expect (remove-if (lambda (match)
                         (equal (path-match-identities +path-document+ (xml-match-path match))
                                (list (path-match-identity match))))
                       matches)
            :to-equal nil)))

(it "indexes the caller's string when it starts with U+FEFF"
  (let* ((source (concatenate 'string +spans-bom+ +path-document+))
         (match (first (select-xml source "/root/item[2]"))))
    (expect (subseq source (xml-match-start match) (xml-match-end match))
            :to-equal "<item id=\"2\">two</item>")
    (expect (xml-match-start match) :to-equal (1+ (search "<item id=\"2\">" +path-document+)))))

(it "returns fresh matches for every call"
  (expect (eq (first (select-xml +path-document+ "/root"))
              (first (select-xml +path-document+ "/root")))
          :to-equal nil))
