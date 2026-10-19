;;;; t/edit-operations-append-test.lisp
;;;;
;;;; Appending an element through EDIT-XML-APPEND: where the fragment goes and
;;;; which line break and indentation precede it.

(in-package #:cl-xml-kit/test)

(it-each (("/root/list" "<added/>" "state=\"on\"/>" "~%    <added/>" 12 0)
          ("/root" "<added/>" "<?inner pi?>" "~%  <added/>" 12 0)
          ("/root/bare" "<n/>" "<bare/>" "><n/></bare>" 5 2)
          ("/root/spaced" "<n/>" "<spaced />" "><n/></spaced>" 8 2)
          ("/root/blank" "<n/>" "<blank>   </blank>" "<n/>" 10 0)
          ("/root/item" "<n/>" "<![CDATA[<raw>]]></item>" "<n/>" 17 0)
          ("/root/noted" "<n/>" "<noted><!-- c --></noted>" "<n/>" 17 0)
          ("/root/holder" "<n/>" "&m;</holder>" "<n/>" 3 0)
          ("/root/bare" "<l>{lf}  <e/>{lf}</l>" "<bare/>" "><l>~%  <e/>~%</l></bare>" 5 2)
          ("/root/bare" " {cr}{lf}<n/>{tab} " "<bare/>" "><n/></bare>" 5 2))
    "appends to ~S the fragment ~S by writing, after ~S, the text ~S"
    (path fragment needle written offset length)
  (expect (edit-apply :append +edit-source+ path (spans-lines fragment))
          :to-equal (edit-expected needle written :offset offset :length length)))

(it-each (("<r><p></p></r>" "/r/p" "<r><p><n/></p></r>")
          ("<r><a/><b/></r>" "/r" "<r><a/><b/><n/></r>")
          ("<r>~%{tab}<b/>~%{tab}<![CDATA[x]]></r>" "/r" "<r>~%{tab}<b/>~%{tab}<![CDATA[x]]><n/></r>")
          ("<r>~%  <a/>~%  text~%</r>" "/r" "<r>~%  <a/>~%  text~%<n/></r>")
          ("<r>~%  <a/><b/>~%</r>" "/r" "<r>~%  <a/><b/>~%  <n/>~%</r>")
          ("<r>~%~%    <a/>~%</r>" "/r" "<r>~%~%    <a/>~%    <n/>~%</r>")
          ("<r>{cr}{lf}{cr}{lf}  <a/>{cr}{lf}</r>" "/r" "<r>{cr}{lf}{cr}{lf}  <a/>{cr}{lf}  <n/>{cr}{lf}</r>"))
    "appends <n/> to ~S at ~S, giving ~S"
    (source path expected)
  (expect (edit-apply :append (spans-lines source) path "<n/>") :to-equal (spans-lines expected)))

(it-each ((:lf "<r>~%  <a/>~%</r>" "<r>~%  <a/>~%  <n/>~%</r>")
          (:crlf "<r>~%  <a/>~%</r>" "<r>~%  <a/>~%  <n/>~%</r>")
          (:cr "<r>~%  <a/>~%</r>" "<r>~%  <a/>~%  <n/>~%</r>")
          (:lf "<r>~%{tab}<a/>~%</r>" "<r>~%{tab}<a/>~%{tab}<n/>~%</r>")
          (:crlf "<r>~%{tab}<a/>~%</r>" "<r>~%{tab}<a/>~%{tab}<n/>~%</r>")
          (:cr "<r>~%{tab}<a/>~%</r>" "<r>~%{tab}<a/>~%{tab}<n/>~%</r>"))
    "copies the ~S indentation of ~S when appending, giving ~S"
    (style source expected)
  (expect (edit-apply :append (spans-lines source style) "/r" "<n/>")
          :to-equal (spans-lines expected style)))

(it-each (("/root/p:item" "<p:child/>" "/root/p:item/p:child" "urn:other")
          ("/root/list" "<p:child/>" "/root/list/p:child" "urn:p")
          ("/root/list" "<plain/>" "/root/list/plain" "urn:default"))
    "appends to ~S the fragment ~S, resolving ~S in the scope of the parent to ~S"
    (path fragment selected uri)
  (expect (edit-namespace-of (edit-apply :append +edit-source+ path fragment) selected)
          :to-equal uri))

(it "selects the appended element as the last child"
  (expect (mapcar #'xml-match-path
                  (path-select (edit-apply :append +edit-source+ "/root/list" "<added/>") "/root/list/*"))
          :to-equal '("/root[1]/list[1]/entry[1]" "/root[1]/list[1]/added[1]")))
