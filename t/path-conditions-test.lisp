;;;; t/path-conditions-test.lisp
;;;;
;;;; What SELECT-XML refuses: steps below a markup entity reference, arguments
;;;; that are not strings, and sources that the caller's options reject.

(in-package #:cl-xml-kit/test)

(defparameter +path-entity-document+
  "<!DOCTYPE r [<!ENTITY m \"<b/>\"><!ENTITY t \"plain\">]><r x=\"1\">&m;<s>&t;<c/></s></r>"
  "A document whose root references an entity that expands to an element.")

(defparameter +path-nested-entity-document+
  "<!DOCTYPE r [<!ENTITY m \"<b/>\"><!ENTITY t \"plain\">]><r><s>&t;<c/></s><u>&m;</u></r>"
  "A document in which only a nested element references that entity.")

(describe "elements that reference a markup entity"
  (it-each (("/r/b") ("/r/*") ("/r/s") ("/r/s/c"))
      "refuses to enumerate the children of the root for ~S, which references a markup entity"
      (path)
    (expect (lambda () (select-xml +path-entity-document+ path))
            :to-signal-xml (list 'xml-unsupported-edit
                                 '(xml-edit-error-reason :entity-reference)
                                 (list 'xml-edit-error-path path)
                                 '(xml-condition-message
                                   "The children of <r> cannot be located in the source: its content references entity m, whose replacement text contains markup."))))

  (it-each (("/r/@x" ((:attribute "/r[1]/@x")))
            ("/r/text()" ((:text "/r[1]/text()")))
            ("/r" ((:element "/r[1]"))))
      "still selects ~S on an element that references a markup entity"
      (path summaries)
    (expect (path-summaries +path-entity-document+ path) :to-equal summaries))

  (it "enumerates the children of an element that references only a text entity"
    (expect (path-summaries +path-nested-entity-document+ "/r/s/*")
            :to-equal '((:element "/r[1]/s[1]/c[1]"))))

  (it "refuses only the step below the nested element that references a markup entity"
    (expect (path-summaries +path-nested-entity-document+ "/r/u")
            :to-equal '((:element "/r[1]/u[1]")))
    (expect (lambda () (select-xml +path-nested-entity-document+ "/r/u/*"))
            :to-signal-xml '(xml-unsupported-edit
                             (xml-edit-error-reason :entity-reference)
                             (xml-edit-error-path "/r/u/*")))))

(describe "arguments and options"
  (it-each (("<a/>" 42 "PATH must be a string.")
            (42 "/a" "SOURCE must be a string."))
      "rejects the source ~S and path ~S with an input error saying ~S"
      (source path message)
    (expect (lambda () (select-xml source path))
            :to-signal-xml (list 'xml-input-error (list 'xml-condition-message message))))

  (it "signals the parse condition for a source that is not well-formed"
    (expect (lambda () (select-xml "<a>" "/a"))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :well-formedness)
                             (xml-well-formedness-error-expected :end-tag))))

  (it "applies the caller's resource limits"
    (expect (lambda ()
              (select-xml "<a><b/></a>" "/a"
                          :resource-limits (make-xml-resource-limits :max-depth 1)))
            :to-signal-xml '(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind :max-depth)
                             (xml-resource-limit-exceeded-limit 1))))

  (it "applies the caller's external entity policy"
    (let ((source "<!DOCTYPE a SYSTEM \"a.dtd\"><a/>"))
      (expect (lambda () (select-xml source "/a"))
              :to-signal-xml '(xml-external-resource-disabled
                               (xml-external-resource-disabled-kind :external-dtd)))
      (expect (path-summaries source "/a" :external-entity-policy :skip)
              :to-equal '((:element "/a[1]"))))))
