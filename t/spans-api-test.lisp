;;;; t/spans-api-test.lisp
;;;;
;;;; SCAN-XML-SPANS: offsets into the caller's string, line-break style, ATTLIST
;;;; names, and the conditions it signals.

(in-package #:cl-xml-kit/test)

(describe "offsets into the caller's string"
  (it "parses the fixture only after its leading U+FEFF is removed"
    (expect (lambda () (parse-xml +spans-fixture+))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :well-formedness)
                             (xml-well-formedness-error-expected :root-element)))
    (expect (xml-document-p (parse-xml (subseq +spans-fixture+ 1))) :to-be-truthy))

  (it "shifts every span by one for a leading U+FEFF"
    (let ((plain (spans-scan (subseq +spans-fixture+ 1)))
          (marked (spans-scan +spans-fixture+)))
      (expect (list (- (cl-xml-kit::span-node-start (cl-xml-kit::span-document-root marked))
                       (cl-xml-kit::span-node-start (cl-xml-kit::span-document-root plain)))
                    (- (cl-xml-kit::span-node-end (cl-xml-kit::span-document-declaration marked))
                       (cl-xml-kit::span-node-end (cl-xml-kit::span-document-declaration plain)))
                    (cl-xml-kit::span-node-start (cl-xml-kit::span-document-declaration marked)))
              :to-equal '(1 1 1))))

  (it "maps spans back through collapsed CRLF pairs and lone CRs to the original string"
    (labels ((collapse (string)
               (with-output-to-string (stream)
                 (loop with index = 0
                       while (< index (length string))
                       do (let ((character (char string index)))
                            (cond
                              ((and (char= character #\Return)
                                    (< (1+ index) (length string))
                                    (char= (char string (1+ index)) #\Linefeed))
                               (write-char #\Linefeed stream)
                               (incf index 2))
                              ((char= character #\Return)
                               (write-char #\Linefeed stream)
                               (incf index))
                              (t (write-char character stream)
                                 (incf index)))))))
             (slices (source)
               (let ((slices nil))
                 (labels ((walk (node)
                            (push (spans-slice source node) slices)
                            (when (cl-xml-kit::span-element-p node)
                              (push (spans-start-tag source node) slices)
                              (push (spans-content source node) slices)
                              (dolist (attribute (cl-xml-kit::span-element-attributes node))
                                (push (third (spans-attribute-text source attribute)) slices))
                              (mapc #'walk (cl-xml-kit::span-element-children node)))))
                   (mapc #'walk (cl-xml-kit::span-document-children (spans-scan source))))
                 (nreverse slices))))
      (let ((expected (slices (spans-fixture-with +spans-lf+))))
        (expect (length expected) :to-be-greater-than 30)
        (expect (mapcar #'collapse (slices (spans-fixture-with +spans-crlf+)))
                :to-equal expected)
        (expect (mapcar #'collapse (slices (spans-fixture-with +spans-cr+)))
                :to-equal expected)))))

(it-each (("<a>x</a>" :lf)
          ("<a>{cr}{lf}x</a>" :crlf)
          ("<a>{lf}x</a>" :lf)
          ("<a>{cr}x</a>" :cr)
          ("<a>{cr}{lf}x{lf}</a>" :crlf)
          ("<a>{lf}x{cr}{lf}</a>" :lf)
          ("<a>{cr}{cr}x</a>" :cr)
          ("<a>x</a>{cr}" :cr))
    "records the first line-break style of ~S as ~S"
    (template style)
  (expect (cl-xml-kit::span-document-line-break (spans-scan (spans-lines template)))
          :to-equal style))

(describe "ATTLIST element names"
  (it "lists the element names of ATTLIST declarations"
    (expect (cl-xml-kit::span-document-attlist-element-names (spans-scan +spans-fixture+))
            :to-equal '("item")))

  (it "lists prefixed element names of ATTLIST declarations once each"
    (let ((source "<!DOCTYPE r [<!ATTLIST p:r a CDATA #IMPLIED><!ATTLIST p:r b CDATA #IMPLIED><!ATTLIST s c CDATA #IMPLIED>]><r/>"))
      (expect (spans-accepted-p source) :to-be-truthy)
      (expect (sort (copy-list (cl-xml-kit::span-document-attlist-element-names
                                (spans-scan source)))
                    #'string<)
              :to-equal '("p:r" "s"))))

  (it "finds ATTLIST declarations that arrive through a parameter entity"
    (let ((source "<!DOCTYPE r [<!ENTITY % decl \"<!ATTLIST q x CDATA #IMPLIED>\"> %decl;]><r/>"))
      (expect (spans-accepted-p source) :to-be-truthy)
      (expect (cl-xml-kit::span-document-attlist-element-names (spans-scan source))
              :to-equal '("q"))))

  (it-each (("<a/>") ("<!DOCTYPE a><a/>"))
      "lists no ATTLIST element names or markup entities for ~S, which has no internal subset"
      (source)
    (let ((document (spans-scan source)))
      (expect (list (cl-xml-kit::span-document-attlist-element-names document)
                    (cl-xml-kit::span-document-markup-entity-names document))
              :to-equal '(nil nil)))))

(describe "conditions"
  (it-each ((42 xml-input-error
                ((xml-condition-message "XML span SOURCE must be a string.")))
            ("<a></b>" xml-well-formedness-error
                       ((xml-well-formedness-error-constraint :element-type-match)
                        (xml-well-formedness-error-expected "a")))
            ("<a><b></b>" xml-well-formedness-error
                          ((xml-well-formedness-error-constraint :well-formedness)
                           (xml-well-formedness-error-expected :end-tag))))
      "rejects the source ~S with ~S"
      (source type slots)
    (expect (lambda () (spans-scan source)) :to-signal-xml (cons type slots)))

  (it-each (("" :well-formedness :root-element)
            ("<?xml version=\"1.0\"?>" :well-formedness :root-element)
            ("<a/><b/>" :well-formedness :single-root)
            ("<!DOCTYPE a><!DOCTYPE a><a/>" :doctype :single-doctype)
            ("<a><!DOCTYPE a></a>" :content nil)
            ("<a/><![CDATA[x]]>" :well-formedness :root-element)
            (" <?xml version=\"1.0\"?><a/>" :xml-declaration :document-start))
      "rejects the misplaced top-level structure of ~S, as PARSE-XML does, under ~S ~S"
      (source constraint expected)
    (expect (spans-accepted-p source) :to-equal nil)
    (expect (lambda () (spans-scan source))
            :to-signal-xml (list 'xml-well-formedness-error
                                 (list 'xml-well-formedness-error-constraint constraint)
                                 (list 'xml-well-formedness-error-expected expected))))

  (it "applies the caller's resource limits to the normalized text"
    (expect (lambda ()
              (spans-scan "<abc/>" :resource-limits (make-xml-resource-limits :max-name-length 2)))
            :to-signal-xml '(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind :max-name-length)
                             (xml-resource-limit-exceeded-value 3)
                             (xml-resource-limit-exceeded-limit 2)))))
