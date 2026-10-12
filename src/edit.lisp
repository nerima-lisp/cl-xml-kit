;;;; src/edit.lisp
;;;;
;;;; Source-preserving edits. Each edit replaces one range of the caller's
;;;; string and leaves every other character as it was; `%edit-finish` verifies
;;;; the result before it is returned.

(in-package #:cl-xml-kit)

(defun %edit-check-value (path value)
  "Require VALUE to be a string of XML characters."
  (unless (stringp value)
    (error 'xml-input-error :message "VALUE must be a string."))
  (let ((index (position-if-not #'xml-char-p value)))
    (when index
      (error 'xml-edit-value-error
             :path path :reason :invalid-character
             :message (format nil "VALUE has a character that XML does not allow, U+~4,'0X at index ~D."
                              (char-code (char value index)) index)))))

(defun %edit-refuse-namespace-declaration (path parsed)
  (when (%span-namespace-declaration-p (parsed-path-attribute-name parsed))
    (error 'xml-unsupported-edit
           :path path :reason :namespace-declaration
           :message "Namespace declarations cannot be edited as attributes.")))

(defun %edit-single-hit (context parsed)
  "Return the one element that the element steps of PARSED select."
  (let ((hits (%path-resolve-elements context parsed)))
    (cond
      ((null hits)
       (%path-error context :no-match "The path matches nothing."))
      ((rest hits)
       (%path-error context :multiple-matches
                    (format nil "The path matches ~D elements, not one." (length hits))))
      (t (first hits)))))

(defun %edit-check-attlist (context hit)
  "Refuse attribute edits on an element that an ATTLIST declaration governs."
  (let ((qname (span-element-qname (path-hit-element hit))))
    (when (member qname (span-document-attlist-element-names (path-context-spans context))
                  :test #'string=)
      (%path-refuse context :attlist-declaration
                    (format nil "An ATTLIST declaration governs the attributes of <~A>."
                            qname)))))

(defun %edit-escape-attribute (value quote-char)
  "Return VALUE escaped for use between QUOTE-CHAR quotes."
  (with-output-to-string (stream)
    (loop for character across value
          do (cond
               ((char= character #\&) (write-string "&amp;" stream))
               ((char= character #\<) (write-string "&lt;" stream))
               ((char= character quote-char)
                (write-string (if (char= quote-char #\") "&quot;" "&apos;") stream))
               ((char= character #\Tab) (write-string "&#9;" stream))
               ((char= character #\Linefeed) (write-string "&#10;" stream))
               ((char= character #\Return) (write-string "&#13;" stream))
               (t (write-char character stream))))))

(defun %edit-namespace-uri (chain prefix)
  "Return the namespace URI that PREFIX is bound to for the innermost element of CHAIN, or NIL."
  (let ((uri (dolist (element chain)
               (let ((binding (find prefix (xml-element-namespace-declarations element)
                                    :key #'xml-namespace-binding-prefix :test #'equal)))
                 (when binding
                   (return (xml-namespace-binding-namespace-uri binding)))))))
    (cond
      ((and uri (plusp (length uri))) uri)
      ((string= prefix "xml") +xml-namespace-uri+))))

(defun %edit-first-attribute-quote (element)
  "Return the quote character of the first attribute in document order below ELEMENT, or NIL."
  (let ((attribute (first (span-element-attributes element))))
    (if attribute
        (span-attribute-quote-char attribute)
        (some #'%edit-first-attribute-quote
              (remove-if-not #'span-element-p (span-element-children element))))))

(defun %edit-line-break-before-p (source attribute)
  "Return true when the whitespace before ATTRIBUTE contains a line break."
  (find-if (lambda (character)
             (or (char= character #\Return) (char= character #\Linefeed)))
           source
           :start (span-attribute-space-start attribute)
           :end (span-attribute-name-start attribute)))

(defun %edit-new-attribute-text (context element qname value)
  "Return the text that adds QNAME with VALUE after the last attribute of ELEMENT."
  (let* ((source (path-context-source context))
         (attributes (span-element-attributes element))
         (last-attribute (car (last attributes)))
         (quote-char (cond
                       (last-attribute (span-attribute-quote-char last-attribute))
                       ((%edit-first-attribute-quote
                         (span-document-root (path-context-spans context))))
                       (t #\"))))
    (concatenate 'string
                 (if (and last-attribute (%edit-line-break-before-p source last-attribute))
                     (subseq source (span-attribute-space-start last-attribute)
                             (span-attribute-name-start last-attribute))
                     " ")
                 qname "=" (string quote-char)
                 (%edit-escape-attribute value quote-char)
                 (string quote-char))))

(defun %edit-attribute-replaced (dom-element qname value)
  "Return the attributes of DOM-ELEMENT with the one written QNAME given VALUE."
  (mapcar (lambda (attribute)
            (if (string= qname (%path-lexical-name (xml-attribute-name attribute)))
                (make-xml-attribute :name (xml-attribute-name attribute) :value value)
                attribute))
          (xml-element-attributes dom-element)))

(defun %edit-attribute-added (context hit qname value)
  "Return the attributes of the DOM element of HIT followed by a new one, QNAME with VALUE.

An unbound prefix in QNAME signals XML-PATH-ERROR with reason :UNDECLARED-PREFIX."
  (let* ((colon (position #\: qname))
         (prefix (and colon (subseq qname 0 colon)))
         (uri (if prefix
                  (or (%edit-namespace-uri (path-hit-chain hit) prefix)
                      (%path-error context :undeclared-prefix
                                   (format nil "The prefix ~A is not declared where <~A> starts."
                                           prefix (span-element-qname (path-hit-element hit)))))
                  "")))
    (append (xml-element-attributes (car (path-hit-chain hit)))
            (list (make-xml-attribute
                   :name (make-xml-name :local-name (if colon (subseq qname (1+ colon)) qname)
                                        :namespace-uri uri :prefix prefix)
                   :value value)))))

(defun %edit-expected-attributes (context hit attributes)
  "Return the document that results from giving the element of HIT the DOM ATTRIBUTES."
  (%dom-rebuild (path-context-dom context) (%path-hit-ordinal-path hit)
                (lambda (element) (%dom-element-with element :attributes attributes))))

(defun %edit-set-attribute (context parsed value)
  (let* ((hit (%edit-single-hit context parsed))
         (element (path-hit-element hit))
         (qname (parsed-path-attribute-name parsed))
         (attribute (%path-find-attribute element qname)))
    (%edit-check-attlist context hit)
    (if attribute
        (%edit-finish context
                      (1+ (span-attribute-value-start attribute))
                      (1- (span-attribute-value-end attribute))
                      (%edit-escape-attribute value (span-attribute-quote-char attribute))
                      (%edit-expected-attributes
                       context hit
                       (%edit-attribute-replaced (car (path-hit-chain hit)) qname value)))
        (let ((point (if (span-element-attributes element)
                         (span-attribute-value-end (car (last (span-element-attributes element))))
                         (span-element-name-end element))))
          (%edit-finish context point point
                        (%edit-new-attribute-text context element qname value)
                        (%edit-expected-attributes
                         context hit
                         (%edit-attribute-added context hit qname value)))))))

(defun %edit-delete-attribute (context parsed)
  (let ((hit (%edit-single-hit context parsed))
        (qname (parsed-path-attribute-name parsed)))
    (%edit-check-attlist context hit)
    (let ((attribute (%path-find-attribute (path-hit-element hit) qname)))
      (unless attribute
        (%path-error context :no-match "The path matches nothing."))
      (%edit-finish context
                    (span-attribute-space-start attribute)
                    (span-attribute-value-end attribute)
                    ""
                    (%edit-expected-attributes
                     context hit
                     (remove qname (xml-element-attributes (car (path-hit-chain hit)))
                             :key (lambda (candidate)
                                    (%path-lexical-name (xml-attribute-name candidate)))
                             :test #'string=))))))

(defun edit-xml-set (source path value &key (resource-limits *default-xml-resource-limits*)
                                            (external-entity-policy :reject))
  "Return SOURCE with the target of PATH set to VALUE, changing nothing else.

PATH must select exactly one target. For `.../@name`, an existing attribute keeps
its quote character and only its value text changes; a missing attribute is
inserted after the last one. Values are escaped for the quote character in use.
The result is parsed again and must equal the original document with the change
applied, otherwise XML-UNSUPPORTED-EDIT with reason :VERIFICATION-FAILED."
  (let ((parsed (%path-parse path)))
    (when (eq (parsed-path-target parsed) :attribute)
      (%edit-refuse-namespace-declaration path parsed))
    (%edit-check-value path value)
    (let ((context (%path-context source path
                                  :resource-limits resource-limits
                                  :external-entity-policy external-entity-policy)))
      (ecase (parsed-path-target parsed)
        (:attribute (%edit-set-attribute context parsed value))))))

(defun edit-xml-delete (source path &key (resource-limits *default-xml-resource-limits*)
                                         (external-entity-policy :reject))
  "Return SOURCE without the target of PATH, changing nothing else.

PATH must select exactly one target. For `.../@name` the attribute and the
whitespace before it are removed. A `text()` path is not a deletable target and
signals XML-PATH-ERROR with reason :WRONG-TARGET."
  (let ((parsed (%path-parse path)))
    (ecase (parsed-path-target parsed)
      (:text
       (error 'xml-path-error
              :path path :reason :wrong-target
              :message "A text() path cannot be deleted."))
      (:attribute
       (%edit-refuse-namespace-declaration path parsed)
       (%edit-delete-attribute
        (%path-context source path
                       :resource-limits resource-limits
                       :external-entity-policy external-entity-policy)
        parsed)))))
