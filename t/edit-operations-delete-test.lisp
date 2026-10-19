;;;; t/edit-operations-delete-test.lisp
;;;;
;;;; Deleting an element through EDIT-XML-DELETE, with the line break and
;;;; indentation before it when nothing else is on its line.

(in-package #:cl-xml-kit/test)

(it-each (("/root/item" "~%  <item id=\"1\" name='n'>text &amp; &#65; &e; <![CDATA[<raw>]]></item>")
          ("/root/bare" "~%  <bare/>")
          ("/root/noted" "~%  <noted><!-- c --></noted>")
          ("/root/spaced" "~%  <spaced />")
          ("/root/list/entry"
           "~%    <entry~%        id=\"e1\"~%        label='first'~%        state=\"on\"/>"))
    "deletes ~S with the line break and indentation before it, ~S"
    (path text)
  (expect (edit-apply :delete +edit-source+ path nil)
          :to-equal (edit-expected (spans-lines text :crlf) "")))

(it-each (("<r><a/><b/></r>" "/r/b" "<r><a/></r>")
          ("<r><a/><b/></r>" "/r/a" "<r><b/></r>")
          ("<r>x <a/> y</r>" "/r/a" "<r>x  y</r>")
          ("<r>~%  <!-- c --><a/>~%</r>" "/r/a" "<r>~%  <!-- c -->~%</r>")
          ("<r>foo~%  <a/></r>" "/r/a" "<r>foo~%  </r>")
          ("<r>~%  <a/>~%bar</r>" "/r/a" "<r>~%  ~%bar</r>")
          ("<r>&#10;  <a/></r>" "/r/a" "<r>&#10;  </r>")
          ("<r>~%  <a/>~%</r>" "/r/a" "<r>~%</r>")
          ("<r>~%  <a/><b/></r>" "/r/a" "<r><b/></r>")
          ("<r>~%{tab}<a/>~%</r>" "/r/a" "<r>~%</r>"))
    "deletes from ~S the element at ~S, leaving ~S, so no character data changes"
    (source path expected)
  (expect (edit-apply :delete (spans-lines source) path nil) :to-equal (spans-lines expected)))

(it-each ((:lf "/r/a" "<r>~%  <b/>~%</r>")
          (:lf "/r/b" "<r>~%  <a/>~%</r>")
          (:crlf "/r/a" "<r>~%  <b/>~%</r>")
          (:crlf "/r/b" "<r>~%  <a/>~%</r>")
          (:cr "/r/a" "<r>~%  <b/>~%</r>")
          (:cr "/r/b" "<r>~%  <a/>~%</r>"))
    "removes the ~S line break before the element at ~S, leaving ~S"
    (style path expected)
  (expect (edit-apply :delete (spans-lines "<r>~%  <a/>~%  <b/>~%</r>" style) path nil)
          :to-equal (spans-lines expected style)))

(it "deletes an element that no longer selects after the edit"
  (expect (path-select (edit-apply :delete +edit-source+ "/root/bare" nil) "/root/bare") :to-equal nil))

(it-each (("<a/>" 0 0) ("  <a/>" 2 2) ("x~%  <a/>" 4 1) ("x{cr}{lf}{tab} <a/>" 5 1)
          ("x{cr}  <a/>" 4 1) ("x  <a/>" 3 3))
    "starts the removal of the element in ~S at ~D from index ~D"
    (source start removal-start)
  (expect (cl-xml-kit::%edit-removal-start (spans-lines source) start) :to-equal removal-start))
