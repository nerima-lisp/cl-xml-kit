;;;; src/verify.lisp
;;;;
;;;; Splicing and verification shared by the source-preserving edits. An edit
;;;; replaces one range of the source. The result is returned only when it
;;;; parses, with the caller's options, to the document that the requested
;;;; change implies; otherwise the edit is refused.

(in-package #:cl-xml-kit)

(defun %edit-splice (source start end replacement)
  "Return SOURCE with the characters from START to END replaced by REPLACEMENT."
  (concatenate 'string (subseq source 0 start) replacement (subseq source end)))

(defun %dom-element-with (element &key (attributes (xml-element-attributes element))
                                       (children (xml-element-children element)))
  "Return a copy of ELEMENT with other ATTRIBUTES or CHILDREN."
  (make-xml-element :name (xml-element-name element)
                    :attributes attributes
                    :namespace-declarations (xml-element-namespace-declarations element)
                    :children children
                    :position (xml-element-position element)
                    :end-position (xml-element-end-position element)))

(defun %dom-update (element ordinals function)
  "Return ELEMENT with the descendant at ORDINALS replaced by (FUNCALL FUNCTION descendant).

ORDINALS lists 1-based positions among element children; ELEMENT itself when empty."
  (if (null ordinals)
      (funcall function element)
      (let ((count 0))
        (%dom-element-with
         element
         :children (mapcar (lambda (child)
                             (if (and (xml-element-p child)
                                      (= (incf count) (first ordinals)))
                                 (%dom-update child (rest ordinals) function)
                                 child))
                           (xml-element-children element))))))

(defun %dom-without-element-child (element ordinal)
  "Return ELEMENT without its ORDINAL-th element child, counted from 1."
  (let ((count 0))
    (%dom-element-with
     element
     :children (loop for child in (xml-element-children element)
                     unless (and (xml-element-p child) (= (incf count) ordinal))
                       collect child))))

(defun %dom-rebuild (document ordinals function)
  "Return DOCUMENT with the element at ORDINALS, counted from the document element, updated by FUNCTION."
  (make-xml-document
   :children (let ((updated nil))
               (mapcar (lambda (child)
                         (if (and (not updated) (xml-element-p child))
                             (progn (setf updated t)
                                    (%dom-update child ordinals function))
                             child))
                       (xml-document-children document)))
   :position (xml-document-position document)
   :end-position (xml-document-end-position document)))

(defun %dom-normalize-children (children lenient-p)
  "Merge adjacent text nodes of CHILDREN and, when LENIENT-P, drop whitespace-only text."
  (let ((merged nil))
    (dolist (child children)
      (if (and (xml-text-p child) (xml-text-p (car merged)))
          (setf (car merged)
                (make-xml-text :data (concatenate 'string
                                                  (xml-text-data (car merged))
                                                  (xml-text-data child))))
          (push child merged)))
    (setf merged (nreverse merged))
    (if lenient-p
        (remove-if (lambda (child)
                     (and (xml-text-p child)
                          (every #'xml-space-char-p (xml-text-data child))))
                   merged)
        merged)))

(defun %dom-name-equal-p (expected actual)
  (and (string= (xml-name-local-name expected) (xml-name-local-name actual))
       (string= (xml-name-namespace-uri expected) (xml-name-namespace-uri actual))
       (equal (xml-name-prefix expected) (xml-name-prefix actual))))

(defun %dom-node-equivalent-p (expected actual reversed-ordinals lenient-ordinals)
  (typecase expected
    (xml-element
     (and (xml-element-p actual)
          (%dom-name-equal-p (xml-element-name expected) (xml-element-name actual))
          (let ((expected-attributes (xml-element-attributes expected))
                (actual-attributes (xml-element-attributes actual)))
            (and (= (length expected-attributes) (length actual-attributes))
                 (every (lambda (expected-attribute actual-attribute)
                          (and (%dom-name-equal-p (xml-attribute-name expected-attribute)
                                                  (xml-attribute-name actual-attribute))
                               (string= (xml-attribute-value expected-attribute)
                                        (xml-attribute-value actual-attribute))))
                        expected-attributes actual-attributes)))
          (let ((expected-bindings (xml-element-namespace-declarations expected))
                (actual-bindings (xml-element-namespace-declarations actual)))
            (and (= (length expected-bindings) (length actual-bindings))
                 (every (lambda (expected-binding actual-binding)
                          (and (equal (xml-namespace-binding-prefix expected-binding)
                                      (xml-namespace-binding-prefix actual-binding))
                               (string= (xml-namespace-binding-namespace-uri expected-binding)
                                        (xml-namespace-binding-namespace-uri actual-binding))))
                        expected-bindings actual-bindings)))
          (%dom-children-equivalent-p
           (xml-element-children expected) (xml-element-children actual)
           reversed-ordinals lenient-ordinals
           (equal (reverse reversed-ordinals) lenient-ordinals))))
    (xml-text (and (xml-text-p actual)
                   (string= (xml-text-data expected) (xml-text-data actual))))
    (xml-comment (and (xml-comment-p actual)
                      (string= (xml-comment-data expected) (xml-comment-data actual))))
    (xml-processing-instruction
     (and (xml-processing-instruction-p actual)
          (string= (xml-processing-instruction-target expected)
                   (xml-processing-instruction-target actual))
          (equal (xml-processing-instruction-data expected)
                 (xml-processing-instruction-data actual))))
    (xml-doctype
     (and (xml-doctype-p actual)
          (string= (xml-doctype-root-name expected) (xml-doctype-root-name actual))
          (equal (xml-doctype-public-id expected) (xml-doctype-public-id actual))
          (equal (xml-doctype-system-id expected) (xml-doctype-system-id actual))
          (equal (xml-doctype-internal-subset expected)
                 (xml-doctype-internal-subset actual))))
    (t nil)))

(defun %dom-children-equivalent-p (expected actual reversed-ordinals lenient-ordinals lenient-p)
  (let ((expected (%dom-normalize-children expected lenient-p))
        (actual (%dom-normalize-children actual lenient-p))
        (ordinal 0))
    (and (= (length expected) (length actual))
         (every (lambda (expected-child actual-child)
                  (%dom-node-equivalent-p
                   expected-child actual-child
                   (if (xml-element-p expected-child)
                       (cons (incf ordinal) reversed-ordinals)
                       reversed-ordinals)
                   lenient-ordinals))
                expected actual))))

(defun %dom-document-equivalent-p (expected actual lenient-ordinals)
  "Return true when ACTUAL has the structure of EXPECTED, ignoring positions.

Adjacent text merges. Whitespace-only text is also ignored among the children of
the element at LENIENT-ORDINALS, counted from the document element (NIL for the
document element itself), and nowhere else."
  (let ((expected (%dom-normalize-children (xml-document-children expected) nil))
        (actual (%dom-normalize-children (xml-document-children actual) nil)))
    (and (= (length expected) (length actual))
         (every (lambda (expected-child actual-child)
                  (%dom-node-equivalent-p expected-child actual-child nil
                                          lenient-ordinals))
                expected actual))))

(defun %edit-reparse (context candidate)
  "Return the document that CANDIDATE parses to, with the options of CONTEXT.

A resource limit that it exceeds signals XML-EDIT-VALUE-ERROR with reason
:RESOURCE-LIMIT and the condition as its cause, since the original document was
within the limits. Any other parse condition signals XML-UNSUPPORTED-EDIT with
reason :VERIFICATION-FAILED."
  (handler-case
      (parse-xml (%strip-bom candidate)
                 :resource-limits (path-context-resource-limits context)
                 :external-entity-policy (path-context-external-entity-policy context))
    (xml-resource-limit-exceeded (condition)
      (xml-error 'xml-edit-value-error :edit-exceeds-limit
                 :format-arguments (condition)
                 :condition-initargs (:path (path-context-path context)
                                      :reason :resource-limit :cause condition)))
    (error (condition)
      (%path-refuse context :verification-failed
                    (xml-message :edit-does-not-parse condition)))))

(defun %edit-verify (context candidate expected &key (lenient-ordinals :none))
  "Return CANDIDATE when it parses to EXPECTED, else refuse the edit.

A candidate that does not parse is refused as %EDIT-REPARSE describes, and a
difference from the EXPECTED document signals XML-UNSUPPORTED-EDIT with reason
:VERIFICATION-FAILED, so no unchecked string is returned."
  (unless (%dom-document-equivalent-p expected (%edit-reparse context candidate)
                                      lenient-ordinals)
    (%path-refuse context :verification-failed
                  (xml-message :edit-differs)))
  candidate)

(defun %edit-finish (context start end replacement expected &key (lenient-ordinals :none))
  "Replace the range from START to END of the source by REPLACEMENT and verify the result."
  (%edit-verify context
                (%edit-splice (path-context-source context) start end replacement)
                expected
                :lenient-ordinals lenient-ordinals))
