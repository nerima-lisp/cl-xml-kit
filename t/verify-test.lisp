;;;; t/verify-test.lisp
;;;;
;;;; Splicing, DOM rebuilding, document equivalence, and the verification that
;;;; every edit result passes before it is returned.

(in-package #:cl-xml-kit/test)

(describe "splicing"
  (it-each (("abcdef" 2 4 "XYZ" "abXYZef")
            ("abcdef" 3 3 "-" "abc-def")
            ("abcdef" 1 5 "" "af"))
      "splices ~S from ~D to ~D with ~S into ~S"
      (source start end replacement expected)
    (expect (cl-xml-kit::%edit-splice source start end replacement) :to-equal expected))

  (it "returns a fresh string and leaves the source alone"
    (let* ((source (copy-seq "abc"))
           (result (cl-xml-kit::%edit-splice source 0 0 "")))
      (expect (eq result source) :to-equal nil)
      (expect source :to-equal "abc"))))

(describe "rebuilding a DOM"
  (it "updates the element at an ordinal path and keeps its siblings"
    (let ((updated (cl-xml-kit::%dom-rebuild
                    (parse-xml "<!--c--><root><a/><b><c/><c/></b></root>") '(2 2)
                    (lambda (element)
                      (cl-xml-kit::%dom-element-with
                       element
                       :attributes (list (make-xml-attribute
                                          :name (make-xml-name :local-name "x")
                                          :value "1")))))))
      (expect (xml->string updated)
              :to-equal "<!--c--><root><a/><b><c/><c x=\"1\"/></b></root>")))

  (it "updates the document element for an empty ordinal path"
    (expect (xml->string
             (cl-xml-kit::%dom-rebuild (parse-xml "<root><a/></root>") nil
                                       (lambda (element)
                                         (cl-xml-kit::%dom-element-with element :children nil))))
            :to-equal "<root/>"))

  (it "removes the element child at an ordinal and keeps the other children"
    (expect (xml->string
             (make-xml-document
              :children (list (cl-xml-kit::%dom-without-element-child
                               (first (xml-document-children (parse-xml "<r>t<a/><b/><c/></r>")))
                               2))))
            :to-equal "<r>t<a/><c/></r>"))

  (it "does not modify the document it rebuilds"
    (let* ((document (parse-xml "<root><a/></root>"))
           (before (xml->string document)))
      (cl-xml-kit::%dom-rebuild document nil
                                (lambda (element)
                                  (cl-xml-kit::%dom-element-with element :children nil)))
      (expect (xml->string document) :to-equal before))))

(describe "document equivalence"
  (it "ignores positions, quote style, and the way a value was written"
    (expect (edit-equivalent-p "<a x='1&#50;'><b/></a>" "<a x=\"12\"><b></b></a>")
            :to-be-truthy))

  (it-each (("<a/>" "<b/>")
            ("<p:a xmlns:p=\"u\"/>" "<q:a xmlns:q=\"u\"/>")
            ("<a xmlns=\"u\"/>" "<a xmlns=\"v\"/>")
            ("<a x=\"1\"/>" "<a x=\"2\"/>")
            ("<a x=\"1\"/>" "<a y=\"1\"/>")
            ("<a x=\"1\" y=\"2\"/>" "<a y=\"2\" x=\"1\"/>")
            ("<a x=\"1\"/>" "<a x=\"1\" y=\"2\"/>")
            ("<a xmlns:p=\"u\"/>" "<a/>")
            ("<a xmlns:p=\"u\"/>" "<a xmlns:q=\"u\"/>")
            ("<a xmlns:p=\"u\"/>" "<a xmlns:p=\"v\"/>")
            ("<a>x</a>" "<a>y</a>")
            ("<a><!--x--></a>" "<a><!--y--></a>")
            ("<a><?p x?></a>" "<a><?p y?></a>")
            ("<a><?p x?></a>" "<a><?q x?></a>")
            ("<a><?p x?></a>" "<a><!--x--></a>")
            ("<a><!--x--></a>" "<a><?p x?></a>")
            ("<a>x</a>" "<a><!--x--></a>")
            ("<a><b/></a>" "<a>b</a>")
            ("<a><b/></a>" "<a><b/><b/></a>")
            ("<a><b/><c/></a>" "<a><c/><b/></a>")
            ("<!--x--><a/>" "<!--y--><a/>")
            ("<a/><!--x-->" "<a/>")
            ("<!DOCTYPE a><a/>" "<!--x--><a/>")
            ("<!DOCTYPE a [<!ENTITY e \"1\">]><a/>" "<!DOCTYPE a [<!ENTITY e \"2\">]><a/>")
            ("<!DOCTYPE a><a/>" "<!DOCTYPE b><b/>"))
      "tells ~S apart from ~S"
      (expected actual)
    (expect (edit-equivalent-p expected actual :external-entity-policy :skip) :to-equal nil))

  (it-each ((("a" "p" "s") ("a" "p" "s") t)
            (("a" "p" "s") ("a" "q" "s") nil)
            (("a" nil "s") ("a" nil "t") nil)
            (("a" nil "s") ("a" "p" "s") nil))
      "compares the DOCTYPE root names and public and system identifiers ~{~S~^ ~} and ~{~S~^ ~} as equivalent: ~S"
      (expected actual result)
    (expect (cl-xml-kit::%dom-document-equivalent-p (verify-doctype-document expected)
                                                    (verify-doctype-document actual) :none)
            :to-equal result))

  (it "never takes a child that is not a DOM node as equivalent, even to itself"
    (let ((document (make-xml-document :children (list "x"))))
      (expect (cl-xml-kit::%dom-document-equivalent-p document document :none) :to-equal nil)))

  (it "merges adjacent text before comparing children"
    (let ((split (cl-xml-kit::%dom-element-with
                  (first (xml-document-children (parse-xml "<a>ab</a>")))
                  :children (list (make-xml-text :data "a") (make-xml-text :data "b")))))
      (expect (cl-xml-kit::%dom-document-equivalent-p
               (parse-xml "<a>ab</a>") (make-xml-document :children (list split)) :none)
              :to-be-truthy)))

  (it-each (("<a><b/></a>" "<a>~%  <b/>~%</a>" :none nil)
            ("<a><b/></a>" "<a>~%  <b/>~%</a>" nil t)
            ("<a><b><c/></b></a>" "<a>~%<b><c/></b></a>" (1) nil)
            ("<a><b><c/></b></a>" "<a><b>~%<c/></b></a>" nil nil)
            ("<a><b><c/></b></a>" "<a>~%<b><c/></b></a>" nil t)
            ("<a><b><c/></b></a>" "<a><b>~%<c/></b></a>" (1) t)
            ("<a><b/></a>" "<a>x<b/></a>" nil nil))
      "compares ~S and ~S, ignoring whitespace-only text only in the element at ~S: ~S"
      (expected actual lenient result)
    (expect (edit-equivalent-p expected (spans-lines actual) :lenient lenient)
            :to-equal result)))

(describe "verifying a candidate"
  (it "returns a candidate that parses to the expected document"
    (let ((candidate "<root a=\"2\"><item>x</item></root>"))
      (expect (eq (edit-verify-result (edit-context "<root a=\"1\"><item>x</item></root>")
                                            candidate (parse-xml candidate))
                  candidate)
              :to-be-truthy)))

  (it "reads a candidate that starts with U+FEFF"
    (let ((candidate (concatenate 'string +spans-bom+ "<root a=\"2\"/>")))
      (expect (edit-verify-result (edit-context (concatenate 'string +spans-bom+ "<root a=\"1\"/>"))
               candidate (parse-xml "<root a=\"2\"/>"))
              :to-equal candidate)))

  (it "accepts whitespace differences in the lenient element"
    (let ((candidate (spans-lines "<root a=\"1\">~%<item>x</item>~%</root>")))
      (expect (edit-verify-result (edit-context "<root a=\"1\"><item>x</item></root>")
                                        candidate
                                        (parse-xml "<root a=\"1\"><item>x</item></root>")
                                        :lenient-ordinals nil)
              :to-equal candidate)))

  (it-each (("<root a=\"3\"><item>x</item></root>" "<root a=\"2\"><item>x</item></root>" :none
             "The edited document differs from the requested change.")
            ("<root a=\"2\"><item>x</root>" "<root a=\"1\"><item>x</item></root>" :none
             "The edited document does not parse: End tag does not match its start tag.")
            ("<root a=\"1\">~%<item>x</item>~%</root>" "<root a=\"1\"><item>x</item></root>" (1)
             "The edited document differs from the requested change.")
            ("<root a=\"1\">~%<item>x</item>~%</root>" "<root a=\"1\"><item>x</item></root>" :none
             "The edited document differs from the requested change."))
      "refuses the candidate ~S for the expected ~S with lenient ordinals ~S: ~A"
      (candidate expected lenient message)
    (expect (lambda ()
              (edit-verify-result (edit-context "<root a=\"1\"><item>x</item></root>")
                                        (spans-lines candidate) (parse-xml expected)
                                        :lenient-ordinals lenient))
            :to-signal-xml (list 'xml-unsupported-edit
                                 '(xml-edit-error-reason :verification-failed)
                                 '(xml-edit-error-path "/root")
                                 (list 'xml-condition-message message))))

  (it "refuses a candidate that breaks a parser limit of the caller, keeping the limit as the cause"
    (expect (lambda ()
              (edit-verify-result (edit-context "<root><a/></root>"
                             :resource-limits (make-xml-resource-limits :max-depth 2))
               "<root><a><b/></a></root>" (parse-xml "<root><a><b/></a></root>")))
            :to-signal-xml '(xml-edit-value-error
                             (xml-edit-error-reason :resource-limit)
                             (xml-edit-error-path "/root")
                             (edit-cause-limit-kind :max-depth))))

  (it "splices and verifies in one step"
    (expect (edit-finish-result (edit-context "<root a=\"1\"><item>x</item></root>")
                                      9 10 "2"
                                      (parse-xml "<root a=\"2\"><item>x</item></root>"))
            :to-equal "<root a=\"2\"><item>x</item></root>"))

  (it "refuses a splice whose result differs from the expected document"
    (expect (lambda ()
              (edit-finish-result (edit-context "<root a=\"1\"><item>x</item></root>")
                                        9 10 "2"
                                        (parse-xml "<root a=\"1\"><item>x</item></root>")))
            :to-signal-xml '(xml-unsupported-edit
                             (xml-edit-error-reason :verification-failed)
                             (xml-edit-error-path "/root")))))
(describe "verification callbacks"
  (it "calls success once after a splice and reparse"
    (let ((context (edit-context "<a x=\"1\"/>"))
          (expected (parse-xml "<a x=\"2\"/>"))
          (calls 0)
          (result nil))
      (cl-weave:with-continuation-result (candidate success)
          (cl-xml-kit::%edit-finish
           context 6 7 "2" expected
           (lambda (source)
             (incf calls)
             (funcall #'success source))
           #'error)
        (setf result candidate))
      (expect calls :to-equal 1)
      (expect result :to-equal "<a x=\"2\"/>")))

  (it "calls failure once with a reparse refusal"
    (let ((context (edit-context "<a/>"))
          (calls 0)
          (received nil))
      (cl-weave:with-continuation-result (condition failure)
          (cl-xml-kit::%edit-reparse
           context "<a>" #'identity
           (lambda (condition)
             (incf calls)
             (funcall #'failure condition)))
        (setf received condition))
      (expect calls :to-equal 1)
      (expect (typep received 'xml-unsupported-edit) :to-be-truthy)
      (expect (xml-edit-error-reason received) :to-equal :verification-failed)))

  (it "propagates errors signalled by either reparse callback"
    (let ((context (edit-context "<a/>"))
          (success-error (make-condition 'simple-error :format-control "success callback"))
          (failure-error (make-condition 'simple-error :format-control "failure callback")))
      (expect
       (handler-case
           (progn
             (cl-xml-kit::%edit-reparse
              context "<a/>"
              (lambda (document)
                (declare (ignore document))
                (error success-error))
              #'error)
             nil)
         (simple-error (condition) (eq condition success-error)))
       :to-be-truthy)
      (expect
       (handler-case
           (progn
             (cl-xml-kit::%edit-reparse
              context "<a>" #'identity
              (lambda (condition)
                (declare (ignore condition))
                (error failure-error)))
             nil)
         (simple-error (condition) (eq condition failure-error)))
       :to-be-truthy))))
