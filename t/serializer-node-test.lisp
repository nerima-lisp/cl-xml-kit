;;;; t/serializer-node-test.lisp

(in-package #:cl-xml-kit/test)

(describe "doctypes"
  (it-each (((:doctype "root" :public "public" :system "system")
             "<!DOCTYPE root PUBLIC \"public\" \"system\">")
            ((:doctype "root" :public "public") "<!DOCTYPE root PUBLIC \"public\">")
            ((:doctype "root" :system "system") "<!DOCTYPE root SYSTEM \"system\">")
            ((:doctype "root" :system "a\"b&c") "<!DOCTYPE root SYSTEM \"a&quot;b&amp;c\">")
            ((:doctype "root" :subset "<!ELEMENT root ANY>")
             "<!DOCTYPE root [<!ELEMENT root ANY>]>")
            ((:doctype "root") "<!DOCTYPE root>"))
      "writes ~S as ~S"
      (spec expected)
    (expect (xml->string (node-from-spec spec)) :to-equal expected))

  (it-each (("a subset with a NOTATION line" (:doctype "root" :subset #.(format nil "<!ELEMENT root ANY>~%  <!NOTATION note SYSTEM \"note\">  "))
             #.(format nil "<!DOCTYPE root [~%<!NOTATION note SYSTEM \"note\">~%]>"))
            ("a subset without NOTATION lines"
             (:doctype "root" :public "p" :system "s" :subset "<!ELEMENT root ANY>") "")
            ("no subset" (:doctype "root" :system "s") ""))
      "keeps only the trimmed NOTATION lines of a doctype with ~A in canonical form~2*"
      (label spec expected)
    (declare (ignore label))
    (expect (xml->canonical-string (node-from-spec spec)) :to-equal expected)))

(describe "documents"
  (it "writes every node kind of a document"
    (expect (xml->string (node-from-spec
                          '(:document (:doctype "root")
                            (:element "root" :attributes ((:attribute "id" "1"))
                             :children ("text" (:comment "comment") (:pi "pi" "data"))))))
            :to-equal "<!DOCTYPE root><root id=\"1\">text<!--comment--><?pi data?></root>"))

  (it "omits comments and writes a processing instruction without data with a space in canonical form"
    (expect (xml->canonical-string (node-from-spec '(:document (:comment "ignored") (:pi "pi"))))
            :to-equal "<?pi ?>"))

  (it "writes a processing instruction without data and without a space in ordinary form"
    (expect (xml->string (node-from-spec '(:pi "pi"))) :to-equal "<?pi?>"))

  (it "omits document-level white space in canonical form"
    (expect (xml->canonical-string (parse-xml (format nil "~%  <root/>~%")))
            :to-equal "<root></root>"))

  (it "keeps document-level white space text in ordinary form"
    (expect (xml->string (node-from-spec '(:document " " (:element "root"))))
            :to-equal " <root/>"))

  (it "writes an attribute node as a name, an equals sign, and a quoted value"
    (expect (xml->string (node-from-spec '(:attribute "id" "a<b" :uri "urn:x" :prefix "p")))
            :to-equal "p:id=\"a&lt;b\"")))

(describe "node refusals"
  (it-each (("a document child that is no node" (:document (:object "not-a-node"))
             "Document children must be XML nodes." nil)
            ("comment data that ends with a hyphen" (:comment "ends-")
             "Comment data is not representable in XML." nil)
            ("comment data that holds --" (:comment "contains--double-hyphen")
             "Comment data is not representable in XML." nil)
            ("comment data inside a document" (:document (:comment "a--b"))
             "Comment data is not representable in XML." (:child))
            ("the processing-instruction target XML" (:pi "XML")
             "Processing-instruction target XML is reserved." nil)
            ("the processing-instruction target xMl" (:pi "xMl")
             "Processing-instruction target XML is reserved." nil)
            ("processing-instruction data that holds ?>" (:pi "pi" "bad?>data")
             "Processing-instruction data contains ?>." nil)
            ("a prefixed attribute node without a namespace" (:attribute "id" "1" :prefix "p")
             "A prefixed attribute must have a namespace URI." nil))
      "refuses ~A"
      (label spec message path)
    (declare (ignore label))
    (expect (lambda () (xml->string (node-from-spec spec)))
            :to-signal-xml `(xml-serialization-error (xml-condition-message ,message)
                                                     (xml-serialization-error-path ,path)))))
