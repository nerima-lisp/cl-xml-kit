;;;; t/path-syntax-test.lisp
;;;;
;;;; The path grammar: what %PATH-PARSE accepts, the index and expectation of
;;;; each syntax error, and canonical paths of generated paths.

(in-package #:cl-xml-kit/test)

(it-each (("" 0 "a path starting with /")
          ("a" 0 "a path starting with /")
          (" /a" 0 "a path starting with /")
          ("/" 1 "a step after /")
          ("/a/" 3 "a step after /")
          ("/a//b" 3 "an element name or *")
          ("/a b" 2 "/ or the end of the path")
          ("/a /b" 2 "/ or the end of the path")
          ("/*x" 2 "/ or the end of the path")
          ("/a*" 2 "/ or the end of the path")
          ("/1a" 1 "an element name or *")
          ("/a[0]" 3 "a positive position")
          ("/a[01]" 3 "a positive position")
          ("/a[x]" 3 "a positive position")
          ("/a[1" 4 "] after the position")
          ("/a[1]b" 5 "/ or the end of the path")
          ("/a:" 3 "a local name after the colon")
          ("/a:b:c" 4 "/ or the end of the path")
          ("/@id" 1 "an element name or *")
          ("/text()" 5 "/ or the end of the path")
          ("/a/@" 4 "an attribute name")
          ("/a/@@id" 4 "an attribute name")
          ("/a/@id/b" 6 "the end of the path")
          ("/a/@id[1]" 6 "the end of the path")
          ("/a/text()/b" 9 "the end of the path")
          ("/a/text()x" 9 "the end of the path"))
    "reports the path ~S as malformed at index ~D, expecting ~A"
    (path index expectation)
  (expect (lambda () (path-select "<a/>" path))
          :to-signal-xml (list 'xml-path-error
                               (list 'xml-edit-error-reason :syntax)
                               (list 'xml-path-error-index index)
                               (list 'xml-edit-error-path path)
                               (list 'xml-condition-message
                                     (format nil "Malformed path at index ~D: expected ~A."
                                             index expectation)))))

(it "rejects a non-ASCII digit as a position"
  (let ((path (format nil "/a[~C]" (code-char #x661))))
    (expect (lambda () (path-select "<a/>" path))
            :to-signal-xml (list 'xml-path-error
                                 '(xml-edit-error-reason :syntax)
                                 '(xml-path-error-index 3)
                                 (list 'xml-edit-error-path path)))))

(it-each (("/a") ("/a/b") ("/*") ("/a/*/c") ("/a[1]") ("/a/b[12]") ("/p:a/q:b[3]")
          ("/a/@id") ("/a/@p:id") ("/a/text()") ("/a/b[2]/@id") ("/a.b/c-d/_e")
          ("/a/b/text()"))
    "accepts the path form ~S"
    (path)
  (finishes (path-select "<a/>" path)))

(it-property "resolves the canonical path of every match of a generated path back to that match"
    ((document (corpus-gen-document))
     (path (path-gen-path)))
  (let ((source (corpus-render document)))
    (expect (loop for match in (path-select source path)
                  unless (equal (path-match-identities source (xml-match-path match))
                                (list (path-match-identity match)))
                    collect (xml-match-path match))
            :to-equal nil)))
