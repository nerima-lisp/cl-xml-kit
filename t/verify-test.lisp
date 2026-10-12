;;;; t/verify-test.lisp

(in-package #:cl-xml-kit/test)

(defun verify-context (source)
  (cl-xml-kit::%path-context source "/root"
                             :resource-limits *default-xml-resource-limits*
                             :external-entity-policy :reject))

(defun verify-refusal (thunk)
  "Return (CLASS REASON) of the XML-EDIT-ERROR that THUNK signals, or (:returned VALUE)."
  (handler-case (list :returned (funcall thunk))
    (xml-edit-error (caught)
      (list (type-of caught) (xml-edit-error-reason caught)))))

(defun verify-equivalent-p (expected actual &optional (lenient :none))
  (cl-xml-kit::%dom-document-equivalent-p (parse-xml expected) (parse-xml actual) lenient))

;;; Splicing

(it "replaces a range of the source and keeps every other character"
  (expect (cl-xml-kit::%edit-splice "abcdef" 2 4 "XYZ") :to-equal "abXYZef"))

(it "inserts at a point and deletes a range"
  (expect (cl-xml-kit::%edit-splice "abcdef" 3 3 "-") :to-equal "abc-def")
  (expect (cl-xml-kit::%edit-splice "abcdef" 1 5 "") :to-equal "af"))

(it "returns a fresh string and leaves the source alone"
  (let* ((source (copy-seq "abc"))
         (result (cl-xml-kit::%edit-splice source 0 0 "")))
    (expect (eq result source) :to-equal nil)
    (expect source :to-equal "abc")))

;;; Applying a change to a DOM

(it "updates the element at an ordinal path and keeps its siblings"
  (let* ((document (parse-xml "<root><a/><b><c/><c/></b></root>"))
         (updated (cl-xml-kit::%dom-rebuild
                   document '(2 2)
                   (lambda (element)
                     (cl-xml-kit::%dom-element-with
                      element
                      :attributes (list (make-xml-attribute
                                         :name (make-xml-name :local-name "x")
                                         :value "1")))))))
    (expect (cl-xml-kit::%dom-document-equivalent-p
             (parse-xml "<root><a/><b><c/><c x=\"1\"/></b></root>") updated :none)
            :to-be-truthy)))

(it "updates the document element for an empty ordinal path"
  (let ((updated (cl-xml-kit::%dom-rebuild
                  (parse-xml "<root><a/></root>") nil
                  (lambda (element) (cl-xml-kit::%dom-element-with element :children nil)))))
    (expect (cl-xml-kit::%dom-document-equivalent-p
             (parse-xml "<root/>") updated :none)
            :to-be-truthy)))

(it "does not modify the document it rebuilds"
  (let* ((document (parse-xml "<root><a/></root>"))
         (before (xml->string document)))
    (cl-xml-kit::%dom-rebuild document nil
                              (lambda (element)
                                (cl-xml-kit::%dom-element-with element :children nil)))
    (expect (xml->string document) :to-equal before)))

;;; Comparing documents

(it "ignores positions, quote style, and the way a value was written"
  (expect (verify-equivalent-p "<a x='1&#50;'><b/></a>" "<a x=\"12\"><b></b></a>")
          :to-be-truthy))

(it "tells documents apart by element name, prefix, and namespace"
  (expect (verify-equivalent-p "<a/>" "<b/>") :to-equal nil)
  (expect (verify-equivalent-p "<p:a xmlns:p=\"u\"/>" "<q:a xmlns:q=\"u\"/>") :to-equal nil)
  (expect (verify-equivalent-p "<a xmlns=\"u\"/>" "<a xmlns=\"v\"/>") :to-equal nil))

(it "tells documents apart by attribute value, name, and order"
  (expect (verify-equivalent-p "<a x=\"1\"/>" "<a x=\"2\"/>") :to-equal nil)
  (expect (verify-equivalent-p "<a x=\"1\"/>" "<a y=\"1\"/>") :to-equal nil)
  (expect (verify-equivalent-p "<a x=\"1\" y=\"2\"/>" "<a y=\"2\" x=\"1\"/>") :to-equal nil)
  (expect (verify-equivalent-p "<a x=\"1\"/>" "<a x=\"1\" y=\"2\"/>") :to-equal nil))

(it "tells documents apart by namespace declarations"
  (expect (verify-equivalent-p "<a xmlns:p=\"u\"/>" "<a/>") :to-equal nil)
  (expect (verify-equivalent-p "<a xmlns:p=\"u\"/>" "<a xmlns:q=\"u\"/>") :to-equal nil))

(it "tells documents apart by text, comments, processing instructions, and children"
  (expect (verify-equivalent-p "<a>x</a>" "<a>y</a>") :to-equal nil)
  (expect (verify-equivalent-p "<a><!--x--></a>" "<a><!--y--></a>") :to-equal nil)
  (expect (verify-equivalent-p "<a><?p x?></a>" "<a><?p y?></a>") :to-equal nil)
  (expect (verify-equivalent-p "<a><?p x?></a>" "<a><!--x--></a>") :to-equal nil)
  (expect (verify-equivalent-p "<a><b/></a>" "<a><b/><b/></a>") :to-equal nil)
  (expect (verify-equivalent-p "<a><b/><c/></a>" "<a><c/><b/></a>") :to-equal nil))

(it "tells documents apart by document-level nodes and the DOCTYPE"
  (expect (verify-equivalent-p "<!--x--><a/>" "<!--y--><a/>") :to-equal nil)
  (expect (verify-equivalent-p "<a/><!--x-->" "<a/>") :to-equal nil)
  (expect (verify-equivalent-p "<!DOCTYPE a [<!ENTITY e \"1\">]><a/>"
                               "<!DOCTYPE a [<!ENTITY e \"2\">]><a/>")
          :to-equal nil))

(it "merges adjacent text before comparing children"
  (let ((split (cl-xml-kit::%dom-element-with
                (first (xml-document-children (parse-xml "<a>ab</a>")))
                :children (list (make-xml-text :data "a") (make-xml-text :data "b")))))
    (expect (cl-xml-kit::%dom-document-equivalent-p
             (parse-xml "<a>ab</a>") (make-xml-document :children (list split)) :none)
            :to-be-truthy)))

(it "compares whitespace-only text strictly unless the element is the lenient one"
  (let ((compact "<a><b/></a>")
        (spaced (format nil "<a>~%  <b/>~%</a>")))
    (expect (verify-equivalent-p compact spaced) :to-equal nil)
    (expect (verify-equivalent-p compact spaced nil) :to-be-truthy)))

(it "ignores whitespace-only text only among the children of the lenient element"
  (let ((compact "<a><b><c/></b></a>")
        (outer-spaced (format nil "<a>~%<b><c/></b></a>"))
        (inner-spaced (format nil "<a><b>~%<c/></b></a>")))
    (expect (verify-equivalent-p compact outer-spaced '(1)) :to-equal nil)
    (expect (verify-equivalent-p compact inner-spaced nil) :to-equal nil)
    (expect (verify-equivalent-p compact outer-spaced nil) :to-be-truthy)
    (expect (verify-equivalent-p compact inner-spaced '(1)) :to-be-truthy)))

(it "keeps non-whitespace text significant even in the lenient element"
  (expect (verify-equivalent-p "<a><b/></a>" "<a>x<b/></a>" nil) :to-equal nil))

;;; Verifying a candidate

(defparameter +verify-source+ "<root a=\"1\"><item>x</item></root>")

(it "returns a candidate that parses to the expected document"
  (let ((context (verify-context +verify-source+))
        (candidate "<root a=\"2\"><item>x</item></root>"))
    (expect (eq (cl-xml-kit::%edit-verify context candidate (parse-xml candidate)) candidate)
            :to-be-truthy)))

(it "refuses a candidate that parses to a different document and returns no string"
  (let ((context (verify-context +verify-source+)))
    (expect (verify-refusal
             (lambda ()
               (cl-xml-kit::%edit-verify context "<root a=\"3\"><item>x</item></root>"
                                         (parse-xml "<root a=\"2\"><item>x</item></root>"))))
            :to-equal '(xml-unsupported-edit :verification-failed))))

(it "refuses a candidate that no longer parses"
  (let ((context (verify-context +verify-source+)))
    (expect (verify-refusal
             (lambda ()
               (cl-xml-kit::%edit-verify context "<root a=\"2\"><item>x</root>"
                                         (parse-xml +verify-source+))))
            :to-equal '(xml-unsupported-edit :verification-failed))))

(it "refuses a candidate that breaks a parser limit of the caller"
  (let ((context (cl-xml-kit::%path-context
                  "<root><a/></root>" "/root"
                  :resource-limits (make-xml-resource-limits :max-depth 2)
                  :external-entity-policy :reject)))
    (expect (verify-refusal
             (lambda ()
               (cl-xml-kit::%edit-verify context "<root><a><b/></a></root>"
                                         (parse-xml "<root><a><b/></a></root>"))))
            :to-equal '(xml-unsupported-edit :verification-failed))))

(it "reads a candidate that starts with U+FEFF"
  (let ((candidate (concatenate 'string +spans-bom+ "<root a=\"2\"/>"))
        (context (verify-context (concatenate 'string +spans-bom+ "<root a=\"1\"/>"))))
    (expect (cl-xml-kit::%edit-verify context candidate (parse-xml "<root a=\"2\"/>"))
            :to-equal candidate)))

(it "accepts whitespace differences in the lenient element only"
  (let ((context (verify-context +verify-source+))
        (candidate (format nil "<root a=\"1\">~%<item>x</item>~%</root>")))
    (expect (cl-xml-kit::%edit-verify context candidate (parse-xml +verify-source+)
                                      :lenient-ordinals nil)
            :to-equal candidate)
    (expect (verify-refusal
             (lambda ()
               (cl-xml-kit::%edit-verify context candidate (parse-xml +verify-source+))))
            :to-equal '(xml-unsupported-edit :verification-failed))))

(it "splices and verifies in one step"
  (let ((context (verify-context +verify-source+)))
    (expect (cl-xml-kit::%edit-finish context 9 10 "2"
                                      (parse-xml "<root a=\"2\"><item>x</item></root>"))
            :to-equal "<root a=\"2\"><item>x</item></root>")))

(it "refuses a splice whose result differs from the expected document"
  (let ((context (verify-context +verify-source+)))
    (expect (verify-refusal
             (lambda ()
               (cl-xml-kit::%edit-finish context 9 10 "2" (parse-xml +verify-source+))))
            :to-equal '(xml-unsupported-edit :verification-failed))))
