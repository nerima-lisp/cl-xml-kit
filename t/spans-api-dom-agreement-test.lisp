;;;; t/spans-api-dom-agreement-test.lisp
;;;;
;;;; The span element tree that SCAN-XML-SPANS builds equals the DOM element
;;;; tree that PARSE-XML builds from the same source.

(in-package #:cl-xml-kit/test)

;;; The rows are the base documents of the generated corpus, read from the
;;; helper at read time so the table and the corpus cannot drift apart.
(it-each #.(spans-agreement-rows)
    "builds the same element tree as the DOM for ~S"
    (display source)
  (declare (ignore display))
  (destructuring-bind (spans dom) (spans-agreement source)
    (expect spans :to-equal dom)))

(it-each ((:lf) (:crlf) (:cr))
    "builds the same element tree as the DOM for the fixture with ~S line breaks"
    (style)
  (destructuring-bind (spans dom) (spans-agreement (spans-fixture-with (spans-line-break style)))
    (expect spans :to-equal dom)))

(it "builds the fixture element tree as written, prefixes and declarations included"
  (expect (first (spans-agreement (spans-fixture-with +spans-lf+)))
          :to-equal '("root" ("a" "b") 2 ("item" ("id") 0) ("p:item" nil 1) ("empty" nil 0))))
