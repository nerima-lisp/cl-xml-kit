;;;; t/serializer-element-test.lisp

(in-package #:cl-xml-kit/test)

(describe "namespace declarations"
  (it "emits required namespace declarations and suppresses redundant ones"
    (expect (xml->string (node-from-spec
                          '(:element "root" :uri "urn:test" :prefix "p"
                            :declarations (("p" "urn:test"))
                            :children ((:element "child" :uri "urn:test" :prefix "p"
                                        :declarations (("p" "urn:test")))))))
            :to-equal "<p:root xmlns:p=\"urn:test\"><p:child/></p:root>"))

  (it "declares the namespace of an element that declares nothing"
    (expect (xml->string (node-from-spec '(:element "a" :uri "urn:a" :prefix "p")))
            :to-equal "<p:a xmlns:p=\"urn:a\"/>"))

  (it "keeps a declaration that no name uses"
    (expect (xml->string (node-from-spec '(:element "a" :declarations (("q" "urn:q")))))
            :to-equal "<a xmlns:q=\"urn:q\"/>"))

  (it "undeclares a default namespace when a child has no namespace"
    (expect (xml->string (node-from-spec '(:element "root" :uri "urn:root"
                                           :children ((:element "child")))))
            :to-equal "<root xmlns=\"urn:root\"><child xmlns=\"\"/></root>"))

  (it "redeclares a prefix that a child binds to another URI"
    (expect (xml->string (node-from-spec '(:element "r" :uri "urn:a" :prefix "p"
                                           :children ((:element "c" :uri "urn:b"
                                                       :prefix "p")))))
            :to-equal "<p:r xmlns:p=\"urn:a\"><p:c xmlns:p=\"urn:b\"/></p:r>")))

(describe "attribute prefixes"
  (it-each (("an earlier attribute's generated prefix"
             (:element "root" :attributes ((:attribute "a" "1" :uri "urn:x")
                                           (:attribute "b" "2" :uri "urn:x")))
             "<root xmlns:nsA=\"urn:x\" nsA:a=\"1\" nsA:b=\"2\"/>")
            ("the element's own prefix"
             (:element "root" :uri "urn:x" :prefix "p"
              :attributes ((:attribute "a" "1" :uri "urn:x")))
             "<p:root xmlns:p=\"urn:x\" p:a=\"1\"/>")
            ("the predeclared xml prefix"
             (:element "root"
              :attributes ((:attribute "lang" "en" :uri "http://www.w3.org/XML/1998/namespace")))
             "<root xml:lang=\"en\"/>"))
      "reuses ~A for an unprefixed attribute in the namespace it is bound to"
      (label spec expected)
    (declare (ignore label))
    (expect (xml->string (node-from-spec spec)) :to-equal expected))

  (it "does not reuse a prefix that another attribute of the element names"
    (expect (xml->string
             (node-from-spec '(:element "r" :uri "urn:x" :prefix "p"
                               :children ((:element "c" :attributes
                                           ((:attribute "a" "1" :uri "urn:x")
                                            (:attribute "b" "2" :uri "urn:y" :prefix "p")))))))
            :to-equal (concatenate 'string
                                   "<p:r xmlns:p=\"urn:x\"><c xmlns:nsA=\"urn:x\""
                                   " xmlns:p=\"urn:y\" nsA:a=\"1\" p:b=\"2\"/></p:r>"))))

(describe "element refusals"
  (it-each (("a repeated namespace declaration"
             (:element "a" :declarations (("p" "urn:u") ("p" "urn:v")))
             "An element cannot repeat a namespace declaration." (:element))
            ("a default namespace bound to the XML namespace"
             (:element "a" :declarations ((nil "http://www.w3.org/XML/1998/namespace")))
             "Only xml may bind the XML namespace." (:element))
            ("another prefix bound to the XML namespace"
             (:element "a" :declarations (("p" "http://www.w3.org/XML/1998/namespace")))
             "Only xml may bind the XML namespace." (:element))
            ("an unprefixed attribute named xmlns"
             (:element "a" :attributes ((:attribute "xmlns" "v")))
             "The xmlns attribute is reserved for namespace declarations." (:element :attribute))
            ("an unprefixed attribute named XMLNS"
             (:element "a" :attributes ((:attribute "XMLNS" "v")))
             "The xmlns attribute is reserved for namespace declarations." (:element :attribute))
            ("a repeated attribute name"
             (:element "a" :attributes ((:attribute "b" "v") (:attribute "b" "w")))
             "An element cannot repeat an expanded attribute name." (:element :attribute))
            ("a repeated expanded attribute name under two prefixes"
             (:element "a" :attributes ((:attribute "b" "v" :uri "urn:u" :prefix "p")
                                        (:attribute "b" "w" :uri "urn:u" :prefix "q")))
             "An element cannot repeat an expanded attribute name." (:element :attribute))
            ("a prefixed attribute without a namespace"
             (:element "root" :attributes ((:attribute "id" "1" :prefix "p")))
             "A prefixed attribute must have a namespace URI." (:element :attribute))
            ("an attribute list holding a non-attribute"
             (:element "a" :attributes ((:object 3)))
             "Element attributes must be XML attributes." (:element))
            ("a child that is no node" (:element "a" :children ((:object 3)))
             "Element children must be XML nodes." (:element))
            ("a refusal inside a child element"
             (:element "a" :children ((:element "b" :children ((:comment "x--y")))))
             "Comment data is not representable in XML." (:element :child :element :child)))
      "refuses ~A"
      (label spec message path)
    (declare (ignore label))
    (expect (lambda () (xml->string (node-from-spec spec)))
            :to-signal-xml `(xml-serialization-error (xml-condition-message ,message)
                                                     (xml-serialization-error-path ,path)))))

(describe "layout"
  (it-each (("one child element" (:element "root" :children ((:element "child")))
             2 #.(format nil "<root>~%  <child/>~%</root>"))
            ("nested child elements" (:element "a" :children ((:element "b" :children ((:element "c")))))
             3 #.(format nil "<a>~%   <b>~%      <c/>~%   </b>~%</a>"))
            ("two child elements" (:element "a" :children ((:element "b") (:element "c")))
             0 #.(format nil "<a>~%<b/>~%<c/>~%</a>"))
            ("mixed text and element content" (:element "root" :children ("x" (:element "child")))
             2 "<root>x<child/></root>")
            ("a comment beside an element" (:element "root" :children ((:comment "c") (:element "child")))
             2 "<root><!--c--><child/></root>")
            ("an empty element" (:element "root") 2 "<root/>"))
      "pretty prints ~A~* at indent ~D as the expected layout~*"
      (label spec indent expected)
    (declare (ignore label))
    (expect (xml->string (node-from-spec spec) :pretty-print t :indent indent)
            :to-equal expected))

  (it "re-parses pretty-printed element-only content as the elements plus layout white space"
    (let ((root (first (xml-document-children
                        (parse-xml (xml->string
                                    (node-from-spec
                                     '(:element "root" :children
                                       ((:element "a" :children ((:element "b")))
                                        (:element "c"))))
                                    :pretty-print t))))))
      (expect (round-trip-signature root)
              :to-equal `(:element "" "root" nil
                                   ((:text ,(format nil "~%  "))
                                    (:element "" "a" nil
                                              ((:text ,(format nil "~%    "))
                                               (:element "" "b" nil nil)
                                               (:text ,(format nil "~%  "))))
                                    (:text ,(format nil "~%  "))
                                    (:element "" "c" nil nil)
                                    (:text ,(string #\Newline)))))))

  (it "writes an element without children as a start and end tag in canonical form"
    (expect (xml->canonical-string (node-from-spec '(:element "a" :children ((:element "b")))))
            :to-equal "<a><b></b></a>")))
