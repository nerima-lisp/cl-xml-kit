(in-package #:cl-xml-kit/test)

(describe "tree shape"
  (it "builds immutable DOM nodes with source order"
    (let* ((children (xml-document-children
                      (parse-xml "<!-- before --><p xmlns=\"urn:p\"><a/>text<b/></p>")))
           (root (second children)))
      (expect (xml-comment-p (first children)) :to-equal t)
      (expect (xml-name-namespace-uri (xml-element-name root)) :to-equal "urn:p")
      (expect (mapcar (lambda (node)
                        (if (xml-element-p node)
                            (xml-name-local-name (xml-element-name node))
                            (xml-text-data node)))
                      (xml-element-children root))
              :to-equal '("a" "text" "b"))))

  (it "does not apply the default namespace to attributes"
    (let* ((root (document-element "<p xmlns=\"urn:p\" id=\"x\"/>"))
           (attribute (first (xml-element-attributes root))))
      (expect (xml-name-namespace-uri (xml-element-name root)) :to-equal "urn:p")
      (expect (xml-name-namespace-uri (xml-attribute-name attribute)) :to-equal "")))

  (it "records namespace declarations on the element that makes them"
    (let ((root (document-element "<p:r xmlns:p='urn:p'><p:c/></p:r>")))
      (expect (mapcar (lambda (binding)
                        (list (xml-namespace-binding-prefix binding)
                              (xml-namespace-binding-namespace-uri binding)))
                      (xml-element-namespace-declarations root))
              :to-equal '(("p" "urn:p")))
      (expect (xml-element-namespace-declarations (first (xml-element-children root)))
              :to-equal nil)))

  (it "normalizes attributes, expands entities, and coalesces text"
    (let ((root (document-element
                 "<!DOCTYPE root [<!ENTITY hi \"there\">]><root a=\"x&amp;y\">one&hi;two</root>")))
      (expect (xml-attribute-value (first (xml-element-attributes root))) :to-equal "x&y")
      (expect (length (xml-element-children root)) :to-equal 1)
      (expect (xml-text-data (first (xml-element-children root))) :to-equal "onetheretwo")))

  (it "keeps whitespace after the root as a document-level text node"
    (let ((children (xml-document-children (parse-xml (format nil "<r/>~%")))))
      (expect (mapcar #'xml-text-p children) :to-equal '(nil t))
      (expect (xml-text-data (second children)) :to-equal (string #\Newline))))

  (it "parses nesting that recursive descent would exhaust the default stack on"
    (let* ((depth 100000)
           (input (with-output-to-string (stream)
                    (loop repeat depth do (write-string "<a>" stream))
                    (loop repeat depth do (write-string "</a>" stream))))
           (limits (make-xml-resource-limits :max-depth depth))
           (element (document-element input :resource-limits limits))
           (levels 1))
      (loop for children = (xml-element-children element)
            while children
            do (setf element (first children))
               (incf levels))
      (expect levels :to-equal depth)
      (expect (lambda ()
                (parse-xml (concatenate 'string "<a>" input "</a>") :resource-limits limits))
              :to-signal-xml '(xml-resource-limit-exceeded
                               (xml-resource-limit-exceeded-kind :max-depth)
                               (xml-resource-limit-exceeded-value 100001)
                               (xml-resource-limit-exceeded-limit 100000))))))

(describe "leaf nodes"
  (it-each (("<!DOCTYPE r [<!ENTITY e 'x'>]><r/>" "r" "<!ENTITY e 'x'>")
            ("<!DOCTYPE r><r/>" "r" nil)
            ("<!DOCTYPE  r  [ ]><r/>" "r" " "))
      "reads the root name and internal subset of the DOCTYPE in ~S" (input root-name subset)
    (let ((doctype (first (xml-document-children (parse-xml input)))))
      (expect (list (xml-doctype-root-name doctype) (xml-doctype-internal-subset doctype))
              :to-equal (list root-name subset))))

  (it-each (("<!DOCTYPE r SYSTEM 'a[b'><r/>")
            ("<!DOCTYPE r SYSTEM ']x['><r/>"))
      "reads no internal subset from the bracket in the system literal of ~S" (input)
    (let ((doctype (first (xml-document-children
                           (parse-xml input :external-entity-policy :skip)))))
      (expect (list (xml-doctype-root-name doctype) (xml-doctype-internal-subset doctype))
              :to-equal '("r" nil))))

  (it-each (("<r><?p?></r>" "p" nil)
            ("<r><?q  d e ?></r>" "q" "d e "))
      "splits the processing instruction in ~S into target and data" (input target data)
    (let ((instruction (first (xml-element-children (document-element input)))))
      (expect (list (xml-processing-instruction-target instruction)
                    (xml-processing-instruction-data instruction))
              :to-equal (list target data))))

  (it "keeps the data of a comment unchanged"
    (expect (xml-comment-data (first (xml-element-children (document-element "<r><!-- c --></r>"))))
            :to-equal " c "))
  (it "rejects external entity references when parse-xml policy is omitted"
    (expect (lambda ()
              (parse-xml "<!DOCTYPE root [<!ENTITY remote SYSTEM \"urn:example:remote\">]><root>&remote;</root>"))
            :to-signal-xml
            (quote (xml-external-resource-disabled
                    (xml-external-resource-disabled-kind :general-entity)
                    (xml-external-resource-disabled-identifier "urn:example:remote"))))))

(describe "positions"
  (it "records the end of the input as the document end position"
    (expect (xml-position-offset (xml-document-end-position (parse-xml (format nil "<a/>~%"))))
            :to-equal 5))

  (it "records each element from its name to its closing delimiter"
    (let* ((root (document-element "<r><e/></r>"))
           (child (first (xml-element-children root))))
      (expect (position-offsets (xml-element-position root) (xml-element-end-position root)
                       (xml-element-position child) (xml-element-end-position child))
              :to-equal '(1 10 4 5))))

  (it "spans a coalesced text node from its first piece to its last"
    (let ((text (first (xml-element-children (document-element "<r>a&amp;b</r>")))))
      (expect (position-offsets (xml-text-position text) (xml-text-end-position text))
              :to-equal '(3 9)))))
(describe "generated constructor defaults"
  (it "starts the DOM builder with empty state"
    (let ((builder (funcall #'cl-xml-kit::%make-dom-builder)))
      (expect (list (cl-xml-kit::dom-builder-frames builder)
                    (cl-xml-kit::dom-builder-children builder)
                    (cl-xml-kit::dom-builder-text builder)
                    (cl-xml-kit::dom-builder-text-start builder)
                    (cl-xml-kit::dom-builder-text-end builder)
                    (cl-xml-kit::dom-builder-end-position builder))
              :to-equal '(nil nil nil nil nil nil))))
  (it "initializes explicit and default frame fields"
    (let ((frame (cl-xml-kit::%make-dom-frame :start nil))
          (defaults (cl-xml-kit::%make-dom-frame-defaults)))
      (expect (list (cl-xml-kit::dom-frame-event frame)
                    (cl-xml-kit::dom-frame-namespace-declarations frame)
                    (cl-xml-kit::dom-frame-children frame))
              :to-equal (quote (:start nil nil)))
      (expect (list (cl-xml-kit::dom-frame-event defaults)
                    (cl-xml-kit::dom-frame-namespace-declarations defaults)
                    (cl-xml-kit::dom-frame-children defaults))
              :to-equal (quote (nil nil nil))))))
