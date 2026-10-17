;;;; t/edit-generated-test.lisp
;;;;
;;;; Every edit kind applied to every element of the fixed-seed generated
;;;; documents changes the source only inside the targeted element.

(in-package #:cl-xml-kit/test)

(it "changes only the targeted element when setting attributes of generated documents"
  (let ((edited 0) (problems nil))
    (dolist (source (subseq (spans-generated-documents) 0 200))
      (dolist (match (edit-element-matches source))
        (let* ((path (concatenate 'string (xml-match-path match) "/@zz"))
               (result (edit-refusal-or-result
                        (lambda () (edit-xml-set source path "v&'\" <t>")))))
          (cond
            ((eq result :attlist-declaration))
            ((and (stringp result)
                  (edit-within-p source result (xml-match-start match) (xml-match-end match))
                  (equal (xml-match-value (first (select-xml result path))) "v&'\" <t>"))
             (incf edited))
            (t (push (list source path result) problems))))))
    (expect problems :to-equal nil)
    (expect edited :to-be-greater-than 300)))

(it "removes only the attribute when deleting attributes of generated documents"
  (let ((edited 0) (problems nil))
    (dolist (source (subseq (spans-generated-documents) 0 200))
      (dolist (match (edit-element-matches source))
        (dolist (attribute (xml-element-attributes (xml-match-element match)))
          (let* ((path (concatenate 'string (xml-match-path match) "/@"
                                    (spans-lexical-qname (xml-attribute-name attribute))))
                 (result (edit-refusal-or-result (lambda () (edit-xml-delete source path)))))
            (cond
              ((eq result :attlist-declaration))
              ((and (stringp result)
                    (edit-within-p source result (xml-match-start match) (xml-match-end match))
                    (null (select-xml result path)))
               (incf edited))
              (t (push (list source path result) problems)))))))
    (expect problems :to-equal nil)
    (expect edited :to-be-greater-than 100)))

(it "sets the text of every generated element that is not mixed, and refuses the others as mixed"
  (let ((edited 0) (problems nil) (value (spans-lines "t&<>]]>~%x")))
    (dolist (source (edit-documents-without-empty-element-reference))
      (dolist (match (edit-element-matches source))
        (let* ((path (concatenate 'string (xml-match-path match) "/text()"))
               (result (edit-refusal-or-result (lambda () (edit-xml-set source path value)))))
          (cond
            ((eq result :mixed-content))
            ((and (stringp result)
                  (edit-within-p source result (xml-match-start match) (xml-match-end match))
                  (equal (xml-match-value (first (select-xml result path))) value))
             (incf edited))
            (t (push (list source path result) problems))))))
    (expect problems :to-equal nil)
    (expect edited :to-be-greater-than 300)))

(it "replaces every generated element below the document element with a fragment"
  (let ((edited 0) (problems nil) (fragment (spans-lines " <zz a=\"1\">~%t</zz>~%")))
    (dolist (source (edit-documents-without-empty-element-reference))
      (dolist (match (edit-element-matches source))
        (when (find #\/ (xml-match-path match) :start 1)
          (let ((result (edit-xml-set source (xml-match-path match) fragment)))
            (if (and (edit-within-p source result (xml-match-start match) (xml-match-end match))
                     (string= "<zz a=\"1\">" result :start2 (xml-match-start match)
                                                    :end2 (+ (xml-match-start match) 10)))
                (incf edited)
                (push (list source (xml-match-path match)) problems))))))
    (expect problems :to-equal nil)
    (expect edited :to-be-greater-than 100)))

(it "deletes every generated element below the document element"
  (let ((edited 0) (problems nil))
    (dolist (source (edit-documents-without-empty-element-reference))
      (dolist (match (edit-element-matches source))
        (when (find #\/ (xml-match-path match) :start 1)
          (let ((result (edit-xml-delete source (xml-match-path match))))
            (if (and (edit-within-p source result
                                    (cl-xml-kit::%edit-removal-start source (xml-match-start match))
                                    (xml-match-end match))
                     (>= (- (length source) (length result))
                         (- (xml-match-end match) (xml-match-start match))))
                (incf edited)
                (push (list source (xml-match-path match)) problems))))))
    (expect problems :to-equal nil)
    (expect edited :to-be-greater-than 100)))

(it "appends to every generated element and changes only inside it"
  (let ((edited 0) (problems nil))
    (dolist (source (edit-documents-without-empty-element-reference))
      (dolist (match (edit-element-matches source))
        (let ((result (edit-xml-append source (xml-match-path match) "<zz a=\"1\"/>")))
          (if (and (edit-within-p source result (xml-match-start match) (xml-match-end match))
                   (search "<zz a=\"1\"/>" result :start2 (xml-match-start match)))
              (incf edited)
              (push (list source (xml-match-path match)) problems)))))
    (expect problems :to-equal nil)
    (expect edited :to-be-greater-than 300)))
