;;;; t/edit-generated-test.lisp
;;;;
;;;; Every edit kind applied to every element of the fixed-seed generated
;;;; documents changes the source only inside the targeted element.

(in-package #:cl-xml-kit/test)

(it-property "keeps attribute edits inside the target of each generated document"
    ((source (gen-member (subseq (spans-generated-documents) 0 200))))
  (let ((problems nil))
    (dolist (match (edit-element-matches source))
      (let* ((path (concatenate 'string (xml-match-path match) "/@zz"))
             (result (edit-refusal-or-result
                      (lambda () (edit-apply :set source path "v&'\" <t>")))))
        (cond
          ((eq result :attlist-declaration))
          ((and (stringp result)
                (edit-within-p source result (xml-match-start match) (xml-match-end match))
                (equal (xml-match-value (first (path-select result path))) "v&'\" <t>")))
          (t (push (list source path result) problems)))))
    (expect problems :to-equal nil)))

(it-property "removes only a selected attribute in generated documents"
    ((source (gen-member (subseq (spans-generated-documents) 0 200))))
  (let ((problems nil))
    (dolist (match (edit-element-matches source))
      (dolist (attribute (xml-element-attributes (xml-match-element match)))
        (let* ((path (concatenate 'string (xml-match-path match) "/@"
                                  (spans-lexical-qname (xml-attribute-name attribute))))
               (result (edit-refusal-or-result
                        (lambda () (edit-apply :delete source path nil)))))
          (cond
            ((eq result :attlist-declaration))
            ((and (stringp result)
                  (edit-within-p source result (xml-match-start match) (xml-match-end match))
                  (null (path-select result path))))
            (t (push (list source path result) problems))))))
    (expect problems :to-equal nil)))

(it-property "sets generated text or refuses mixed content"
    ((source (gen-member (edit-documents-without-empty-element-reference))))
  (let ((problems nil)
        (value (spans-lines "t&<>]]>~%x")))
    (dolist (match (edit-element-matches source))
      (let* ((path (concatenate 'string (xml-match-path match) "/text()"))
             (result (edit-refusal-or-result
                      (lambda () (edit-apply :set source path value)))))
        (cond
          ((eq result :mixed-content))
          ((and (stringp result)
                (edit-within-p source result (xml-match-start match) (xml-match-end match))
                (equal (xml-match-value (first (path-select result path))) value)))
          (t (push (list source path result) problems)))))
    (expect problems :to-equal nil)))

(it-property "replaces generated nested elements without changing surrounding source"
    ((source (gen-member (edit-documents-without-empty-element-reference))))
  (let ((problems nil)
        (fragment (spans-lines " <zz a=\"1\">~%t</zz>~%")))
    (dolist (match (edit-element-matches source))
      (when (find #\/ (xml-match-path match) :start 1)
        (let ((result (edit-apply :set source (xml-match-path match) fragment)))
          (unless (and (edit-within-p source result
                                     (xml-match-start match) (xml-match-end match))
                       (string= "<zz a=\"1\">" result
                                :start2 (xml-match-start match)
                                :end2 (+ (xml-match-start match) 10)))
            (push (list source (xml-match-path match) result) problems)))))
    (expect problems :to-equal nil)))

(it-property "deletes each generated nested element only within its source range"
    ((source (gen-member (edit-documents-without-empty-element-reference))))
  (let ((problems nil))
    (dolist (match (edit-element-matches source))
      (when (find #\/ (xml-match-path match) :start 1)
        (let ((result (edit-apply :delete source (xml-match-path match) nil)))
          (unless (and (edit-within-p source result
                                     (cl-xml-kit::%edit-removal-start
                                      source (xml-match-start match))
                                     (xml-match-end match))
                       (>= (- (length source) (length result))
                           (- (xml-match-end match) (xml-match-start match))))
            (push (list source (xml-match-path match) result) problems)))))
    (expect problems :to-equal nil)))

(it-property "appends to every element in generated documents without changing surrounding source"
    ((source (gen-member (edit-documents-without-empty-element-reference))))
  (let ((problems nil))
    (dolist (match (edit-element-matches source))
      (let ((result (edit-apply :append source (xml-match-path match) "<zz a=\"1\"/>")))
        (unless (and (edit-within-p source result (xml-match-start match) (xml-match-end match))
                     (search "<zz a=\"1\"/>" result :start2 (xml-match-start match)))
          (push (list source (xml-match-path match) result) problems))))
    (expect problems :to-equal nil)))
