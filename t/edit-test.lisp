;;;; t/edit-test.lisp
;;;;
;;;; What every public edit shares: a fresh result that keeps everything
;;;; outside the edited range, the caller's options, and argument checks.

(in-package #:cl-xml-kit/test)

(it-each ((:set "/root/@b" "x") (:delete "/root/@b" nil) (:append "/root/list" "<n/>"))
    "returns a fresh string from ~S at ~S and leaves the source unmodified"
    (operation path value)
  (let* ((source (copy-seq +edit-source+))
         (result (edit-apply operation source path value)))
    (expect (eq result source) :to-equal nil)
    (expect source :to-equal +edit-source+)))

(it "keeps the leading U+FEFF, the CRLF line breaks, and everything outside the range"
  (let ((result (edit-xml-set +edit-source+ "/root/@b" "x")))
    (expect (list (char result 0) (count #\Return result) (length result))
            :to-equal (list (code-char #xFEFF) (count #\Return +edit-source+)
                            (- (length +edit-source+) 5)))
    (expect (edit-within-p +edit-source+ result (search "double" +edit-source+)
                           (+ (search "double" +edit-source+) 6))
            :to-be-truthy)))

(it-each ((:set "<!DOCTYPE a SYSTEM \"a.dtd\"><a x=\"1\"/>" "/a/@x" "2"
           "<!DOCTYPE a SYSTEM \"a.dtd\"><a x=\"2\"/>")
          (:delete "<!DOCTYPE a SYSTEM \"a.dtd\"><a x=\"1\"/>" "/a/@x" nil
           "<!DOCTYPE a SYSTEM \"a.dtd\"><a/>")
          (:append "<!DOCTYPE a SYSTEM \"a.dtd\"><a/>" "/a" "<n/>"
           "<!DOCTYPE a SYSTEM \"a.dtd\"><a><n/></a>"))
    "applies the caller's external entity policy to the source of ~S on ~S"
    (operation source path value expected)
  (expect (lambda () (edit-apply operation source path value))
          :to-signal-xml '(xml-external-resource-disabled
                           (xml-external-resource-disabled-kind :external-dtd)))
  (expect (edit-apply operation source path value :external-entity-policy :skip)
          :to-equal expected))

(it-each ((:set 42 "/a/@x" "v" "SOURCE must be a string.")
          (:set "<a/>" 42 "v" "PATH must be a string.")
          (:set "<a/>" "/a/@x" 42 "VALUE must be a string.")
          (:delete 42 "/a/@x" nil "SOURCE must be a string.")
          (:delete "<a/>" 42 nil "PATH must be a string.")
          (:append 42 "/a" "<n/>" "SOURCE must be a string.")
          (:append "<a/>" 42 "<n/>" "PATH must be a string.")
          (:append "<a/>" "/a" 42 "VALUE must be a string."))
    "rejects ~S of the source ~S, path ~S, and value ~S with the input error ~S"
    (operation source path value message)
  (expect (lambda () (edit-apply operation source path value))
          :to-signal-xml (list 'xml-input-error (list 'xml-condition-message message))))

(it-each ((:set "/a/@x" "v") (:delete "/a/@x" nil) (:append "/a" "<n/>"))
    "signals the parse condition of a source that is not well-formed from ~S at ~S"
    (operation path value)
  (expect (lambda () (edit-apply operation "<a>" path value))
          :to-signal-xml '(xml-well-formedness-error
                           (xml-well-formedness-error-constraint :well-formedness)
                           (xml-well-formedness-error-expected :end-tag))))
