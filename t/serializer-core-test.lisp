;;;; t/serializer-core-test.lisp

(in-package #:cl-xml-kit/test)

(describe "escaping"
  (it "escapes text and attributes without changing ordinary content"
    (expect (xml->string (node-from-spec
                          '(:element "root" :attributes ((:attribute "value" "a\"&<>"))
                            :children ("a&<]]>\""))))
            :to-equal "<root value=\"a&quot;&amp;&lt;&gt;\">a&amp;&lt;]]&gt;\"</root>"))

  (it-each (("a&b" "a&amp;b")
            ("a<b" "a&lt;b")
            ("a>b" "a>b")
            ("x]]>y" "x]]&gt;y")
            ("]>" "]>")
            ("] ]>" "] ]>")
            (">" ">")
            ("q\"'" "q\"'")
            (#.(string (code-char #x1F600)) #.(string (code-char #x1F600)))
            ("plain" "plain"))
      "writes the text ~S as ~S"
      (data expected)
    (expect (xml->string (make-xml-text :data data)) :to-equal expected))

  (it-each (("a TAB LF b unchanged" #.(coerce '(#\a #\Tab #\Newline #\b) 'string)
             #.(coerce '(#\a #\Tab #\Newline #\b) 'string))
            ("a CR b as a&#13;b" #.(coerce '(#\a #\Return #\b) 'string) "a&#13;b"))
      "writes the text ~A~2*"
      (label data expected)
    (declare (ignore label))
    (expect (xml->string (make-xml-text :data data)) :to-equal expected))

  (it-each (("a&b" "a&b" "a&amp;b")
            ("a<b" "a<b" "a&lt;b")
            ("a>b" "a>b" "a&gt;b")
            ("q\"'" "q\"'" "q&quot;'")
            ("TAB LF CR" #.(coerce '(#\Tab #\Newline #\Return) 'string) "&#9;&#10;&#13;"))
      "writes the text ~A~* in canonical form as ~S"
      (label data expected)
    (declare (ignore label))
    (expect (xml->canonical-string (make-xml-text :data data)) :to-equal expected))

  (it-each (("a&b" "a&b" "v=\"a&amp;b\"")
            ("a<b" "a<b" "v=\"a&lt;b\"")
            ("a>b" "a>b" "v=\"a&gt;b\"")
            ("q\"'" "q\"'" "v=\"q&quot;'\"")
            ("TAB LF CR" #.(coerce '(#\Tab #\Newline #\Return) 'string) "v=\"&#9;&#10;&#13;\"")
            ("plain" "plain" "v=\"plain\""))
      "writes the attribute value ~A~* as ~S"
      (label value expected)
    (declare (ignore label))
    (expect (xml->string (make-xml-attribute :name (make-xml-name :local-name "v")
                                             :value value))
            :to-equal expected))

  (it-sequential "kills every mutant of the text escaping function"
    (let ((results
            (mutation-results
             "src/serializer-core.lisp" 'cl-xml-kit::%escape-text
             (lambda ()
               (and (equal (xml->string (make-xml-text :data "a&b<c>d]]>e"))
                           "a&amp;b&lt;c>d]]&gt;e")
                    (equal (xml->string (make-xml-text :data "plain")) "plain")
                    (equal (xml->canonical-string
                            (make-xml-text
                             :data (coerce '(#\> #\" #\Tab #\Newline #\Return) 'string)))
                           "&gt;&quot;&#9;&#10;&#13;"))))))
      (expect-not results :to-be nil)
      (expect (getf (assert-mutation-score results 1.0) :score) :to-equal 1.0))))

(describe "character and output checks"
  (it-each (("a text node" (:element "a" :children ((:text #.(string (code-char 1)))))
             (xml-serialization-error
              (xml-condition-message "Character U+0001 cannot be represented in XML.")
              (xml-serialization-error-path (:element :child))))
            ("an attribute value" (:element "a" :attributes ((:attribute "b" #.(string (code-char #xFFFE)))))
             (xml-serialization-error
              (xml-condition-message "Character U+FFFE cannot be represented in XML.")
              (xml-serialization-error-path (:element))))
            ("a namespace URI" (:element "a" :declarations (("p" #.(string (code-char 2)))))
             (xml-serialization-error
              (xml-condition-message "Character U+0002 cannot be represented in XML.")
              (xml-serialization-error-path (:element))))
            ("a comment" (:comment #.(string (code-char 3)))
             (xml-serialization-error
              (xml-condition-message "Character U+0003 cannot be represented in XML.")
              (xml-serialization-error-path nil)))
            ("processing-instruction data" (:pi "p" #.(string (code-char 4)))
             (xml-serialization-error
              (xml-condition-message "Character U+0004 cannot be represented in XML.")
              (xml-serialization-error-path nil)))
            ("a doctype root name" (:doctype #.(format nil "r~C" (code-char 5)))
             (xml-serialization-error
              (xml-condition-message "Character U+0005 cannot be represented in XML.")
              (xml-serialization-error-path nil))))
      "refuses a character that XML cannot represent in ~A"
      (label spec expected)
    (declare (ignore label))
    (expect (lambda () (xml->string (node-from-spec spec))) :to-signal-xml expected))

  (it-each ((3 5) (6 7))
      "refuses output past a limit of ~D characters at the write that reaches ~D"
      (limit value)
    (expect (lambda ()
              (xml->string (node-from-spec '(:element "root"))
                           :resource-limits (make-xml-resource-limits
                                             :max-output-characters limit)))
            :to-signal-xml `(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind :max-output-characters)
                             (xml-resource-limit-exceeded-value ,value)
                             (xml-resource-limit-exceeded-limit ,limit))))

  (it "writes output that exactly reaches the output limit"
    (expect (xml->string (node-from-spec '(:element "root"))
                         :resource-limits (make-xml-resource-limits :max-output-characters 7))
            :to-equal "<root/>")))

(describe "namespace names"
  (it-each (("a declaration that is no binding" (:element "a" :declarations ((:object 1)))
             "Namespace declarations must be XML namespace bindings." (:element))
            ("a declared xmlns prefix" (:element "a" :declarations (("xmlns" "urn:u")))
             "The xmlns prefix is reserved." (:element))
            ("the xml prefix declared for another URI"
             (:element "a" :declarations (("xml" "urn:u")))
             "The xml prefix has a reserved namespace URI." (:element))
            ("a prefix declared for the xmlns URI"
             (:element "a" :declarations (("p" "http://www.w3.org/2000/xmlns/")))
             "The xmlns namespace URI is reserved." (:element))
            ("a default namespace declared as the xmlns URI"
             (:element "a" :declarations ((nil "http://www.w3.org/2000/xmlns/")))
             "The xmlns namespace URI is reserved." (:element))
            ("an empty element prefix" (:element "a" :uri "urn:u" :prefix "")
             "A serialized prefix is invalid." (:element))
            ("an element named with the xml prefix and another URI"
             (:element "a" :uri "urn:u" :prefix "xml")
             "The xml prefix must use its reserved URI." (:element))
            ("an element named with the xmlns prefix" (:element "a" :uri "urn:u" :prefix "xmlns")
             "The xmlns prefix cannot name an XML node." (:element))
            ("an attribute named with the xml prefix and another URI"
             (:element "a" :attributes ((:attribute "b" "v" :uri "urn:u" :prefix "xml")))
             "The xml prefix must use its reserved URI." (:element :attribute))
            ("an attribute named with the xmlns prefix"
             (:element "a" :attributes ((:attribute "b" "v" :uri "urn:u" :prefix "xmlns")))
             "The xmlns prefix cannot name an XML node." (:element :attribute))
            ("an element prefix the element declares for another URI"
             (:element "root" :uri "urn:a" :prefix "p" :declarations (("p" "urn:b")))
             "A namespace declaration conflicts with a node name." (:element)))
      "refuses ~A"
      (label spec message path)
    (declare (ignore label))
    (expect (lambda () (xml->string (node-from-spec spec)))
            :to-signal-xml `(xml-serialization-error (xml-condition-message ,message)
                                                     (xml-serialization-error-path ,path))))

  (it "accepts a declaration of the xml prefix for its own URI and writes nothing for it"
    (expect (xml->string (node-from-spec
                          '(:element "a" :declarations
                            (("xml" "http://www.w3.org/XML/1998/namespace")))))
            :to-equal "<a/>"))

  (it "generates nsA for the first namespaced unprefixed attribute"
    (expect (xml->string (node-from-spec
                          '(:element "root" :attributes ((:attribute "id" "1" :uri "urn:attr")))))
            :to-equal "<root xmlns:nsA=\"urn:attr\" nsA:id=\"1\"/>"))

  (it "declares a generated prefix for each further namespaced unprefixed attribute"
    (expect (xml->string (node-from-spec
                          '(:element "root" :attributes ((:attribute "a" "1" :uri "urn:x")
                                                         (:attribute "b" "2" :uri "urn:y")))))
            :to-equal
            "<root xmlns:nsA=\"urn:x\" xmlns:nsB=\"urn:y\" nsA:a=\"1\" nsB:b=\"2\"/>"))

  (it "continues generated prefixes after nsZ with nsAA and then nsBA, low letter first"
    (let ((element (make-xml-element
                    :name (make-xml-name :local-name "root")
                    :attributes (loop for index from 0 below 28
                                      collect (make-xml-attribute
                                               :name (make-xml-name
                                                      :local-name "a"
                                                      :namespace-uri (format nil "urn:~D" index))
                                               :value "v")))))
      (expect (mapcar #'xml-namespace-binding-prefix
                      (xml-element-namespace-declarations
                       (first (xml-document-children (parse-xml (xml->string element))))))
              :to-equal '("nsA" "nsB" "nsC" "nsD" "nsE" "nsF" "nsG" "nsH" "nsI" "nsJ"
                          "nsK" "nsL" "nsM" "nsN" "nsO" "nsP" "nsQ" "nsR" "nsS" "nsT"
                          "nsU" "nsV" "nsW" "nsX" "nsY" "nsZ" "nsAA" "nsBA"))))

  (it "skips a generated prefix that the scope already binds"
    (expect (xml->string (node-from-spec
                          '(:element "root" :declarations (("nsA" "urn:other"))
                            :attributes ((:attribute "id" "1" :uri "urn:attr")))))
            :to-equal
            "<root xmlns:nsA=\"urn:other\" xmlns:nsB=\"urn:attr\" nsB:id=\"1\"/>")))

(describe "canonical attribute order"
  (it "uses Second Canonical Form for attributes and empty elements"
    (expect (xml->canonical-string
             (node-from-spec '(:element "root" :attributes ((:attribute "z" "&>")
                                                            (:attribute "a" "1")))))
            :to-equal "<root a=\"1\" z=\"&amp;&gt;\"></root>"))

  (it-each (("z" "" "a" "" "<r a=\"2\" z=\"1\"></r>")
            ("a" "urn:x" "b" "" "<r xmlns:nsA=\"urn:x\" b=\"2\" nsA:a=\"1\"></r>")
            ("k" "urn:y" "k" "urn:x" "<r xmlns:nsA=\"urn:x\" xmlns:nsB=\"urn:y\" nsA:k=\"2\" nsB:k=\"1\"></r>"))
      "orders attribute ~S in ~S after ~S in ~S in canonical output: ~S"
      (first-name first-uri second-name second-uri expected)
    (expect (xml->canonical-string
             (node-from-spec `(:element "r" :attributes
                                        ((:attribute ,first-name "1" :uri ,first-uri)
                                         (:attribute ,second-name "2" :uri ,second-uri)))))
            :to-equal expected)))
