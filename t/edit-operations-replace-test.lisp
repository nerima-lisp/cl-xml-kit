;;;; t/edit-operations-replace-test.lisp
;;;;
;;;; Replacing an element with a fragment through EDIT-XML-SET with an element
;;;; path.

(in-package #:cl-xml-kit/test)

(it-each (("/root/bare" "<renamed k='v'>text</renamed>" "<renamed k='v'>text</renamed>")
          ("/root/bare" " {tab}{cr}{lf}<renamed/>{lf} " "<renamed/>")
          ("/root/bare" "<l>{lf}  <e/>{lf}</l>" "<l>~%  <e/>~%</l>")
          ("/root/bare" "<l>{cr}{lf}<e/>{cr}</l>" "<l>~%<e/>~%</l>"))
    "replaces the empty element at ~S with the fragment ~S, written as ~S"
    (path fragment written)
  (expect (edit-xml-set +edit-source+ path (spans-lines fragment))
          :to-equal (edit-expected "<bare/>" written)))

(it "keeps comments, CDATA, and both quote styles of a replacing fragment verbatim"
  (let ((fragment "<n a='1' b=\"2\"><!-- c --><![CDATA[<x>]]><?p d?></n>"))
    (expect (edit-xml-set +edit-source+ "/root/spaced" fragment)
            :to-equal (edit-expected "<spaced />" fragment))))

(it "replaces a nested element written over several lines"
  (expect (edit-xml-set +edit-source+ "/root/list/entry" "<entry2/>")
          :to-equal (edit-expected-between "<entry" "state=\"on\"/>" "<entry2/>")))

(it "replaces the document element"
  (expect (edit-xml-set +edit-source+ "/root" "<root xmlns=\"urn:new\"><only/></root>")
          :to-equal (edit-expected-between "<root xmlns=\"urn:default\"" "</root>"
                                           "<root xmlns=\"urn:new\"><only/></root>")))

(it "replaces an element whose replacement just fits the depth limit where it lands"
  (expect (edit-xml-set "<r><x><y/></x></r>" "/r/x" "<a><b/></a>"
                        :resource-limits (make-xml-resource-limits :max-depth 3))
          :to-equal "<r><a><b/></a></r>"))

(it-each (("/root/bare" "<plain/>" "/root/plain" "urn:default")
          ("/root/bare" "<p:thing p:x=\"1\"/>" "/root/p:thing" "urn:p")
          ("/root/p:item" "<p:x/>" "/root/p:x" "urn:p"))
      "replaces ~S with ~S, resolving ~S in the scope above it to ~S"
      (path fragment selected uri)
  (expect (edit-namespace-of (edit-xml-set +edit-source+ path fragment) selected)
          :to-equal uri))
