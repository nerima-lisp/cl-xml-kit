;;;; t/spans-tag-node-scan-test.lisp
;;;;
;;;; Element, attribute, and text-run spans recorded from start tags, end tags,
;;;; and content tokens.

(in-package #:cl-xml-kit/test)

(describe "element spans"
  (it-each (("root"
             "<root xmlns=\"urn:default\" xmlns:p=\"urn:p\"~%      a='single'~%      b=\"double\">"
             "~%  <item id=\"1\">text &amp; &#65; &e; <![CDATA[<raw>]]></item>~%  <p:item xmlns:p=\"urn:other\"/>~%  <!-- inner~%       comment -->~%  <?inner pi?>~%  <empty />~%"
             "</root>")
            ("item" "<item id=\"1\">" "text &amp; &#65; &e; <![CDATA[<raw>]]>" "</item>"))
      "records the start tag, content, and end tag of the fixture element ~S"
      (qname start-tag content end-tag)
    (let ((element (spans-fixture-element qname)))
      (expect (list (spans-start-tag +spans-fixture+ element)
                    (spans-content +spans-fixture+ element)
                    (spans-end-tag +spans-fixture+ element)
                    (cl-xml-kit::span-element-empty-p element))
              :to-equal (list (spans-lines start-tag :crlf) (spans-lines content :crlf)
                              end-tag nil))
      (expect (spans-slice +spans-fixture+ element)
              :to-equal (spans-lines (concatenate 'string start-tag content end-tag) :crlf))))

  (it-each (("p:item" "<p:item xmlns:p=\"urn:other\"/>")
            ("empty" "<empty />"))
      "records the empty fixture element ~S with its content range at the slash"
      (qname text)
    (let* ((element (spans-fixture-element qname))
           (slash (cl-xml-kit::span-element-content-start element)))
      (expect (list (spans-slice +spans-fixture+ element)
                    (cl-xml-kit::span-element-empty-p element)
                    (cl-xml-kit::span-element-content-end element)
                    (subseq +spans-fixture+ slash
                            (cl-xml-kit::span-element-start-tag-end element))
                    (cl-xml-kit::span-element-end-tag-start element)
                    (cl-xml-kit::span-element-end-tag-end element))
              :to-equal (list text t slash "/>" nil nil))))

  (it "records lexical qualified names as written"
    (expect (mapcar (lambda (qname)
                      (cl-xml-kit::span-element-qname (spans-fixture-element qname)))
                    '("root" "item" "p:item" "empty"))
            :to-equal '("root" "item" "p:item" "empty")))

  (it "records the index after each element name"
    (expect (mapcar (lambda (qname)
                      (let ((element (spans-fixture-element qname)))
                        (subseq +spans-fixture+ (cl-xml-kit::span-node-start element)
                                (cl-xml-kit::span-element-name-end element))))
                    '("root" "item" "p:item" "empty"))
            :to-equal '("<root" "<item" "<p:item" "<empty")))

  (it "records the child sequence of the root element"
    (expect (mapcar #'spans-kind
                    (cl-xml-kit::span-element-children (spans-fixture-element "root")))
            :to-equal '(:text :element :text :element :text :comment :text
                        :processing-instruction :text :element :text))))

(describe "attribute spans"
  (it-each (("xmlns" "xmlns" "\"urn:default\"" " xmlns=\"urn:default\"")
            ("xmlns:p" "xmlns:p" "\"urn:p\"" " xmlns:p=\"urn:p\"")
            ("a" "a" "'single'" "~%      a='single'")
            ("b" "b" "\"double\"" "~%      b=\"double\""))
      "records the name, quoted value, and leading whitespace of the root attribute ~S"
      (qname name value text)
    (expect (spans-attribute-text +spans-fixture+
                                  (spans-attribute (spans-fixture-element "root") qname))
            :to-equal (list name value (spans-lines text :crlf))))

  (it "records the text between the quotes of each attribute value"
    (expect (mapcar (lambda (attribute)
                      (subseq +spans-fixture+
                              (1+ (cl-xml-kit::span-attribute-value-start attribute))
                              (1- (cl-xml-kit::span-attribute-value-end attribute))))
                    (cl-xml-kit::span-element-attributes (spans-fixture-element "root")))
            :to-equal '("urn:default" "urn:p" "single" "double")))

  (it "keeps whitespace around the equals sign and in the end tag inside their spans"
    (let* ((source (spans-lines "<a  x = \"1\"~%  y~%=~%'2'  ></a  >"))
           (element (spans-root source)))
      (expect (mapcar (lambda (qname)
                        (spans-attribute-text source (spans-attribute element qname)))
                      '("x" "y"))
              :to-equal (list (list "x" "\"1\"" "  x = \"1\"")
                              (list "y" "'2'" (spans-lines "~%  y~%=~%'2'"))))
      (expect (spans-start-tag source element)
              :to-equal (spans-lines "<a  x = \"1\"~%  y~%=~%'2'  >"))
      (expect (spans-end-tag source element) :to-equal "</a  >")))

  (it "records the quote character of each attribute"
    (expect (mapcar #'cl-xml-kit::span-attribute-quote-char
                    (append (cl-xml-kit::span-element-attributes (spans-fixture-element "root"))
                            (cl-xml-kit::span-element-attributes (spans-fixture-element "item"))))
            :to-equal '(#\" #\" #\' #\" #\")))

  (it "flags namespace declarations among the attributes in source order"
    (expect (mapcar (lambda (attribute)
                      (list (cl-xml-kit::span-attribute-qname attribute)
                            (cl-xml-kit::span-attribute-declaration-p attribute)))
                    (cl-xml-kit::span-element-attributes (spans-fixture-element "root")))
            :to-equal '(("xmlns" t) ("xmlns:p" t) ("a" nil) ("b" nil))))

  (it "records a prefix redeclared on a child element"
    (let ((attribute (spans-attribute (spans-fixture-element "p:item") "xmlns:p")))
      (expect (cl-xml-kit::span-attribute-declaration-p attribute) :to-be-truthy)
      (expect (spans-attribute-text +spans-fixture+ attribute)
              :to-equal (list "xmlns:p" "\"urn:other\"" " xmlns:p=\"urn:other\"")))))

(describe "text runs"
  (it "records a text run mixing text, references, and CDATA as one span with flags"
    (let ((run (first (cl-xml-kit::span-element-children (spans-fixture-element "item")))))
      (expect (spans-slice +spans-fixture+ run)
              :to-equal "text &amp; &#65; &e; <![CDATA[<raw>]]>")
      (expect (spans-text-flags run) :to-equal '(t nil t t ("e")))))

  (it "records a plain whitespace run with no flags set"
    (let ((run (first (cl-xml-kit::span-element-children (spans-fixture-element "root")))))
      (expect (spans-slice +spans-fixture+ run) :to-equal (spans-lines "~%  " :crlf))
      (expect (spans-text-flags run) :to-equal '(nil nil nil nil nil))))

  (it-each (("<a><![CDATA[x]]></a>" t)
            ("<a><![CDATA[]]></a>" t)
            ("<a><![CDATA[x]]>y</a>" nil)
            ("<a><![CDATA[x]]><![CDATA[y]]></a>" nil))
      "marks whether the content of ~S is exactly one CDATA section: ~S"
      (source single-p)
    (expect (cl-xml-kit::span-text-single-cdata-p
             (first (cl-xml-kit::span-element-children (spans-root source))))
            :to-equal single-p))

  (it "records a lone CDATA section as a text run spanning its delimiters"
    (let ((source "<a><![CDATA[x<y]]></a>"))
      (expect (spans-slice source (first (cl-xml-kit::span-element-children (spans-root source))))
              :to-equal "<![CDATA[x<y]]>")))

  (it "records each distinct general entity reference once, in order of appearance"
    (expect (cl-xml-kit::span-text-entity-names
             (first (cl-xml-kit::span-element-children
                     (spans-root
                      "<!DOCTYPE a [<!ENTITY x \"1\"><!ENTITY y \"2\">]><a>&y;&x;&y;</a>"))))
            :to-equal '("y" "x"))))
