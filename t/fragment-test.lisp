;;;; t/fragment-test.lisp
;;;;
;;;; Fragments an edit writes: line-break conversion, the namespaces in scope
;;;; where they land, the wrapper they are parsed in, and what is refused.

(in-package #:cl-xml-kit/test)

(describe "line breaks and whitespace"
  (it-each (("a{cr}{lf}b{cr}c{lf}d" :lf "a{lf}b{lf}c{lf}d")
            ("a{cr}{lf}b{cr}c{lf}d" :crlf "a{cr}{lf}b{cr}{lf}c{cr}{lf}d")
            ("a{cr}{lf}b{cr}c{lf}d" :cr "a{cr}b{cr}c{cr}d")
            ("{cr}{cr}{lf}{lf}" :crlf "{cr}{lf}{cr}{lf}{cr}{lf}")
            ("plain text" :crlf "plain text"))
      "converts the line breaks of ~S to ~S as ~S"
      (input style expected)
    (expect (cl-xml-kit::%edit-convert-line-breaks (spans-lines input) style)
            :to-equal (spans-lines expected)))

  (it-each (("  a b  " "a b")
            ("{tab}{cr}{lf}<x/>{lf} " "<x/>")
            ("   " "")
            ("" ""))
      "trims the XML whitespace around ~S to ~S"
      (input expected)
    (expect (cl-xml-kit::%trim-xml-space (spans-lines input)) :to-equal expected)))

(describe "namespace scope"
  (it "collects the bindings in scope, letting an inner declaration replace an outer one"
    (expect (sort (copy-list
                   (cl-xml-kit::%edit-scope-bindings
                    (edit-chain "<a xmlns=\"d\" xmlns:p=\"u1\"><b xmlns:p=\"u2\" xmlns:q=\"u3\"><c/></b></a>")))
                  #'string< :key (lambda (binding) (or (car binding) "")))
            :to-equal '((nil . "d") ("p" . "u2") ("q" . "u3"))))

  (it "leaves out an undeclared default namespace"
    (expect (cl-xml-kit::%edit-scope-bindings
             (edit-chain "<a xmlns=\"d\"><b xmlns=\"\"><c/></b></a>"))
            :to-equal nil))

  (it "has no bindings outside every element"
    (expect (cl-xml-kit::%edit-scope-bindings nil) :to-equal nil))

  (it-each ((((nil . "urn:d") ("p" . "urn:p")) "<w xmlns=\"urn:d\" xmlns:p=\"urn:p\"><a/></w>")
            (nil "<w><a/></w>")
            ((("p" . "a\"b&c<d")) "<w xmlns:p=\"a&quot;b&amp;c&lt;d\"><a/></w>"))
      "wraps a fragment in an element that declares ~S, escaping each URI, as ~S"
      (bindings expected)
    (expect (cl-xml-kit::%edit-fragment-wrapper bindings "<a/>") :to-equal expected)))

(describe "parsing a fragment"
  (it "returns the one element of a fragment and the fragment without surrounding whitespace"
    (multiple-value-bind (element text)
        (fragment-element (spans-lines " {cr}{lf}<a b=\"1\"/>{tab} "))
      (expect (xml-name-local-name (xml-element-name element)) :to-equal "a")
      (expect text :to-equal "<a b=\"1\"/>")))

  (it-each (("<a/>" nil "urn:d")
            ("<p:a/>" "p" "urn:p")
            ("<q:a xmlns:q=\"urn:q\"/>" "q" "urn:q"))
      "parses ~S where it lands, in the namespace of prefix ~S, ~S"
      (fragment prefix uri)
    (let ((name (xml-element-name (fragment-element fragment))))
      (expect (list (xml-name-prefix name) (xml-name-namespace-uri name))
              :to-equal (list prefix uri))))

  (it "accepts a fragment that is within the limits of the caller"
    (expect (xml-element-attributes (fragment-element "<a x=\"1\" y=\"2\"/>"))
            :to-have-length 2)))

(describe "refused fragments"
  (it-each (("") ("   ") ("text") ("<a/><b/>") ("<a/> tail") ("head <a/>") ("<!--c--><a/>")
            ("<a/><!--c-->") ("<?p x?><a/>"))
      "refuses the fragment ~S, which is not exactly one element"
      (fragment)
    (expect (lambda () (fragment-element fragment))
            :to-signal-xml '(xml-edit-value-error
                             (xml-edit-error-reason :not-single-element)
                             (xml-edit-error-path "/target")
                             (edit-cause-type nil)
                             (xml-condition-message "The fragment must be exactly one element."))))

  (it-each (("<a>" :reject xml-well-formedness-error)
            ("<a></b>" :reject xml-well-formedness-error)
            ("<z:a/>" :reject xml-namespace-error)
            ("<a>&xxe;</a>" :reject xml-well-formedness-error)
            ("<!DOCTYPE a><a/>" :reject xml-well-formedness-error)
            ("<!DOCTYPE a><a/>" :skip xml-well-formedness-error)
            ("<!DOCTYPE a [<!ENTITY x SYSTEM \"x.ent\">]><a>&x;</a>" :reject xml-well-formedness-error)
            ("<!DOCTYPE a [<!ENTITY x SYSTEM \"x.ent\">]><a>&x;</a>" :skip xml-well-formedness-error)
            ("<?xml version=\"1.0\"?><a/>" :reject xml-well-formedness-error)
            ("<?xml version=\"1.0\"?><a/>" :skip xml-well-formedness-error)
            ("</w><a/>" :reject xml-well-formedness-error)
            ("a</w><w>b" :reject xml-well-formedness-error)
            ("<a/></w><w>" :reject xml-well-formedness-error))
      "refuses the fragment ~S under the ~S policy as malformed, keeping the ~S as its cause"
      (fragment policy cause-type)
    (expect (lambda () (fragment-element fragment :external-entity-policy policy))
            :to-signal-xml (list 'xml-edit-value-error
                                 '(xml-edit-error-reason :malformed-fragment)
                                 '(xml-edit-error-path "/target")
                                 (list 'edit-cause-type cause-type))))

  (it "refuses a fragment that breaks a resource limit of the caller"
    (expect (lambda ()
              (fragment-element "<a x=\"1\" y=\"2\" z=\"3\"/>"
                                :resource-limits (make-xml-resource-limits
                                                  :max-attributes-per-element 2)))
            :to-signal-xml '(xml-edit-value-error
                             (xml-edit-error-reason :resource-limit)
                             (xml-edit-error-path "/target")
                             (edit-cause-type xml-resource-limit-exceeded)
                             (edit-cause-limit-kind :max-attributes-per-element)))))
