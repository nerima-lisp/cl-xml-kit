;;;; src/edit-support.lisp

(in-package #:cl-xml-kit)

(defun %edit-check-value (path value)
  "Require VALUE to be a string of XML characters."
  (check-argument value stringp xml-input-error :label-must-be-string
                  :format-arguments ("VALUE"))
  (let ((index (position-if-not #'xml-char-p value)))
    (when index
      (error 'xml-edit-value-error
             :path path :reason :invalid-character
             :message (xml-message :value-illegal-character
                                   (char-code (char value index)) index)))))

(defun %edit-refuse-namespace-declaration (path parsed)
  (when (%span-namespace-declaration-p (parsed-path-attribute-name parsed))
    (error 'xml-unsupported-edit
           :path path :reason :namespace-declaration
           :message (xml-message :namespace-declaration-not-editable))))

(defun %edit-single-hit (context parsed)
  "Return the one element that the element steps of PARSED select."
  (let ((hits (%path-resolve-elements context parsed)))
    (cond
      ((null hits)
       (%path-error context :no-match (xml-message :path-matches-nothing)))
      ((rest hits)
       (%path-error context :multiple-matches
                    (xml-message :path-matches-several (length hits))))
      (t (first hits)))))

(defun %edit-check-attlist (context hit)
  "Refuse attribute edits on an element that an ATTLIST declaration governs."
  (let ((qname (span-element-qname (path-hit-element hit))))
    (when (member qname (span-document-attlist-element-names (path-context-spans context))
                  :test #'string=)
      (%path-refuse context :attlist-declaration
                    (xml-message :attlist-governs-attributes qname)))))

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
                                   (xml-message :prefix-not-declared prefix
                                                (span-element-qname
                                                 (path-hit-element hit)))))
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
        (%path-error context :no-match (xml-message :path-matches-nothing)))
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
