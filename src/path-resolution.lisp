(in-package #:cl-xml-kit)

(defun %path-context (source path success failure &key resource-limits external-entity-policy)
  "Call SUCCESS once with a context for SOURCE and PATH, or FAILURE once with the condition.

One parse feeds both the DOM and the span tree."
  (%call-with-callbacks
   (lambda ()
     (check-argument source stringp xml-input-error :label-must-be-string
                     :format-arguments ("SOURCE"))
     (multiple-value-bind (parser scanner)
         (%source-parser source resource-limits external-entity-policy)
       (let ((dom (%parser-document parser)))
         (%make-path-context source path dom (%span-document scanner parser source)
                             resource-limits external-entity-policy))))
   success failure))

(defun %path-name-matches-p (step qname)
  (or (eq (path-step-name step) :any)
      (string= (path-step-name step) qname)))

(defun %path-markup-entity-reference (context element)
  "Return the name of an entity with markup that the content of the span ELEMENT references, or NIL."
  (let ((markup (span-document-markup-entity-names (path-context-spans context))))
    (when markup
      (dolist (child (span-element-children element))
        (when (span-text-p child)
          (let ((name (find-if (lambda (name) (member name markup :test #'string=))
                               (span-text-entity-names child))))
            (when name
              (return name))))))))

(defun %path-check-children-enumerable (context hit)
  "Refuse to enumerate the children of a HIT whose content references a markup entity."
  (let ((name (%path-markup-entity-reference context (path-hit-element hit))))
    (when name
      (%path-refuse
       context :entity-reference
       (xml-message :children-not-locatable
                    (span-element-qname (path-hit-element hit)) name)))))

(defun %path-step-hits (context hit step)
  "Return the children of HIT that STEP selects, in document order."
  (%path-check-children-enumerable context hit)
  (let ((dom-children (coerce (remove-if-not #'xml-element-p
                                             (xml-element-children
                                              (car (path-hit-chain hit))))
                              'vector))
        (counts (make-hash-table :test #'equal))
        (matched 0)
        (hits nil))
    (loop for element in (remove-if-not #'span-element-p
                                        (span-element-children (path-hit-element hit)))
          for ordinal from 1
          do (let* ((qname (span-element-qname element))
                    (same-name (incf (gethash qname counts 0))))
               (when (%path-name-matches-p step qname)
                 (incf matched)
                 (when (or (null (path-step-index step))
                           (= matched (path-step-index step)))
                   (push (%make-path-hit
                          element (path-hit-element hit)
                          (cons (aref dom-children (1- ordinal)) (path-hit-chain hit))
                          (cons ordinal (path-hit-ordinals hit))
                          (cons (format nil "~A[~D]" qname same-name)
                                (path-hit-steps hit)))
                         hits)))))
    (nreverse hits)))

(defun %path-resolve-elements (context parsed success failure)
  "Call SUCCESS once with the list of PATH-HITs selected by the element steps of
PARSED, or FAILURE once with the condition."
  (%call-with-callbacks
   (lambda ()
     (let* ((root (span-document-root (path-context-spans context)))
            (step (first (parsed-path-steps parsed)))
            (hits (when (and (%path-name-matches-p step (span-element-qname root))
                             (member (path-step-index step) '(nil 1)))
                    (list (%make-path-hit
                           root nil
                           (list (find-if #'xml-element-p
                                          (xml-document-children (path-context-dom context))))
                           nil
                           (list (format nil "~A[1]" (span-element-qname root))))))))
       (dolist (step (rest (parsed-path-steps parsed)) hits)
         (setf hits (loop for hit in hits
                          append (%path-step-hits context hit step))))))
   success failure))

(defun %path-hit-ordinal-path (hit)
  "Return the ordinals leading from the document element to HIT, outermost first."
  (reverse (path-hit-ordinals hit)))

(defun %path-find-attribute (element qname)
  "Return the attribute of the span ELEMENT written as QNAME, ignoring namespace declarations."
  (find-if (lambda (attribute)
             (and (not (span-attribute-declaration-p attribute))
                  (string= qname (span-attribute-qname attribute))))
           (span-element-attributes element)))

(defun %path-lexical-name (name)
  (let ((prefix (xml-name-prefix name)))
    (if prefix
        (concatenate 'string prefix ":" (xml-name-local-name name))
        (xml-name-local-name name))))

(defun %path-canonical (hit suffix)
  (with-output-to-string (stream)
    (dolist (step (reverse (path-hit-steps hit)))
      (write-char #\/ stream)
      (write-string step stream))
    (when suffix
      (write-char #\/ stream)
      (write-string suffix stream))))

(defun %path-text-value (dom-element)
  (with-output-to-string (stream)
    (dolist (child (xml-element-children dom-element))
      (when (xml-text-p child)
        (write-string (xml-text-data child) stream)))))

(defun %path-element-match (hit)
  (let ((element (path-hit-element hit)))
    (%make-xml-match :element (%path-canonical hit nil)
                     (span-element-qname element) nil
                     (span-node-start element) (span-node-end element)
                     (car (path-hit-chain hit)))))

(defun %path-attribute-match (hit attribute)
  (let ((qname (span-attribute-qname attribute))
        (dom-element (car (path-hit-chain hit))))
    (%make-xml-match :attribute (%path-canonical hit (concatenate 'string "@" qname))
                     qname
                     (xml-attribute-value
                      (find qname (xml-element-attributes dom-element)
                            :key (lambda (candidate)
                                   (%path-lexical-name (xml-attribute-name candidate)))
                            :test #'string=))
                     (span-attribute-name-start attribute)
                     (span-attribute-value-end attribute)
                     dom-element)))

(defun %path-text-match (hit)
  (let ((element (path-hit-element hit))
        (dom-element (car (path-hit-chain hit))))
    (%make-xml-match :text (%path-canonical hit "text()") nil
                     (%path-text-value dom-element)
                     (span-element-content-start element)
                     (span-element-content-end element)
                     dom-element)))

(defun select-xml (source path success failure
                          &key (resource-limits *default-xml-resource-limits*)
                          (external-entity-policy :reject))
  "Call SUCCESS once with the list of XML-MATCH values selected by PATH, or
FAILURE once with the condition.

PATH is absolute: element steps /name or /*, each with an optional 1-based
[n], then optionally /@name or /text(). Names compare as written in SOURCE,
prefix included. Match indices refer to SOURCE itself."
  (flet ((selected-matches (parsed hits)
                           (ecase (parsed-path-target parsed)
                             (:element (mapcar #'%path-element-match hits))
                             (:attribute
                              (loop for hit in hits
                                    for attribute = (%path-find-attribute
                                                     (path-hit-element hit) (parsed-path-attribute-name parsed))
                                    when attribute
                                    collect (%path-attribute-match hit attribute)))
                             (:text (mapcar #'%path-text-match hits)))))
    (%path-parse
     path
     (lambda (parsed)
       (%path-context
        source path
        (lambda (context)
          (%path-resolve-elements
           context parsed
           (lambda (hits)
             (%call-with-callbacks
              (lambda () (selected-matches parsed hits))
              success failure))
           failure))
        failure
        :resource-limits resource-limits
        :external-entity-policy external-entity-policy))
     failure)))
