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

(defun %edit-escape-text (value line-break)
  "Return VALUE as character data, with line feeds written as LINE-BREAK."
  (with-output-to-string (stream)
    (loop for character across value
          do (case character
               (#\& (write-string "&amp;" stream))
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (#\Return (write-string "&#13;" stream))
               (#\Linefeed (write-string (%edit-convert-line-breaks (string #\Linefeed)
                                                                    line-break)
                                         stream))
               (t (write-char character stream))))))

(defun %edit-text-content (element)
  "Return the one text run that makes up all the content of the span ELEMENT, or NIL."
  (let ((children (span-element-children element)))
    (and children (null (rest children)) (span-text-p (first children))
         (first children))))

(defun %edit-check-text-settable (context hit)
  "Refuse to replace content that holds elements, comments, processing instructions, or a markup entity."
  (let* ((element (path-hit-element hit))
         (entity (%path-markup-entity-reference context element)))
    (cond
      (entity
       (%path-refuse context :mixed-content
                     (format nil "The content of <~A> references entity ~A, whose replacement text contains markup."
                             (span-element-qname element) entity)))
      ((notevery #'span-text-p (span-element-children element))
       (%path-refuse context :mixed-content
                     (format nil "The content of <~A> holds elements, comments, or processing instructions."
                             (span-element-qname element)))))))

(defun %edit-set-text (context parsed value)
  (let ((hit (%edit-single-hit context parsed)))
    (%edit-check-text-settable context hit)
    (let* ((element (path-hit-element hit))
           (line-break (span-document-line-break (path-context-spans context)))
           (content (%edit-text-content element))
           (text (if (and content (span-text-single-cdata-p content)
                          (not (search "]]>" value)) (not (find #\Return value)))
                     (concatenate 'string "<![CDATA["
                                  (%edit-convert-line-breaks value line-break) "]]>")
                     (%edit-escape-text value line-break)))
           (empty-p (span-element-empty-p element)))
      (%edit-finish context
                    (span-element-content-start element)
                    (if empty-p
                        (span-element-start-tag-end element)
                        (span-element-content-end element))
                    (if empty-p
                        (concatenate 'string ">" text "</" (span-element-qname element) ">")
                        text)
                    (%dom-rebuild (path-context-dom context) (%path-hit-ordinal-path hit)
                                  (lambda (dom-element)
                                    (%dom-element-with
                                     dom-element
                                     :children (and (plusp (length value))
                                                    (list (make-xml-text :data value))))))))))

(defun %edit-replace-element (context parsed fragment)
  (let* ((hit (%edit-single-hit context parsed))
         (element (path-hit-element hit)))
    (multiple-value-bind (replacement text)
        (%edit-fragment-element context (rest (path-hit-chain hit)) fragment)
      (%edit-finish context (span-node-start element) (span-node-end element)
                    (%edit-convert-line-breaks
                     text (span-document-line-break (path-context-spans context)))
                    (%dom-rebuild (path-context-dom context) (%path-hit-ordinal-path hit)
                                  (lambda (dom-element)
                                    (declare (ignore dom-element))
                                    replacement))))))

(defun %edit-removal-start (source start)
  "Return where removing the element at START from SOURCE begins.

That is START, unless only spaces and tabs lie between the previous line break
and START; then it is the start of that line break, so no blank line remains."
  (let ((index start))
    (loop while (and (> index 0) (member (char source (1- index)) '(#\Space #\Tab)))
          do (decf index))
    (cond
      ((and (> index 1)
            (char= (char source (1- index)) #\Linefeed)
            (char= (char source (- index 2)) #\Return))
       (- index 2))
      ((and (> index 0) (member (char source (1- index)) '(#\Linefeed #\Return)))
       (1- index))
      (t start))))

(defun %edit-plain-whitespace-p (source node)
  "Return true when NODE is a text run of nothing but literal whitespace."
  (and (span-text-p node)
       (not (span-text-cdata-p node))
       (not (span-text-char-ref-p node))
       (not (span-text-predefined-ref-p node))
       (null (span-text-entity-names node))
       (loop for index from (span-node-start node) below (span-node-end node)
             always (xml-space-char-p (char source index)))))

(defun %edit-deletion-start (context hit)
  "Return where deleting the element of HIT begins.

The whitespace and line break before the element go with it only when the text
on both sides is plain whitespace, so no character data changes."
  (let* ((source (path-context-source context))
         (element (path-hit-element hit))
         (start (span-node-start element))
         (siblings (span-element-children (path-hit-parent hit)))
         (position (position element siblings))
         (previous (and (plusp position) (nth (1- position) siblings)))
         (next (nth (1+ position) siblings)))
    (if (and previous
             (%edit-plain-whitespace-p source previous)
             (or (null next) (not (span-text-p next))
                 (%edit-plain-whitespace-p source next)))
        (%edit-removal-start source start)
        start)))

(defun %edit-delete-element (context parsed)
  (let ((hit (%edit-single-hit context parsed)))
    (when (null (path-hit-ordinals hit))
      (%path-refuse context :root-element "The document element cannot be deleted."))
    (let* ((element (path-hit-element hit))
           (ordinals (%path-hit-ordinal-path hit))
           (parent-ordinals (butlast ordinals)))
      (%edit-finish context
                    (%edit-deletion-start context hit)
                    (span-node-end element)
                    ""
                    (%dom-rebuild (path-context-dom context) parent-ordinals
                                  (lambda (parent)
                                    (%dom-without-element-child parent (car (last ordinals)))))
                    :lenient-ordinals parent-ordinals))))

(defun %edit-last-line-indentation (source parent)
  "Return the line break and indentation before the last element child of PARENT that follows whitespace with a line break, or NIL.

For a run of several line breaks, only the last line break and what follows it."
  (let ((found nil))
    (loop for (previous child) on (span-element-children parent)
          do (when (and (span-element-p child) (%edit-plain-whitespace-p source previous))
               (let ((break (position-if (lambda (character)
                                           (or (char= character #\Return)
                                               (char= character #\Linefeed)))
                                         source
                                         :start (span-node-start previous)
                                         :end (span-node-end previous)
                                         :from-end t)))
                 (when break
                   (when (and (char= (char source break) #\Linefeed)
                              (> break (span-node-start previous))
                              (char= (char source (1- break)) #\Return))
                     (decf break))
                   (setf found (subseq source break (span-node-end previous)))))))
    found))

(defun %edit-append-element (context parsed fragment)
  (let* ((hit (%edit-single-hit context parsed))
         (parent (path-hit-element hit))
         (source (path-context-source context)))
    (multiple-value-bind (appended text)
        (%edit-fragment-element context (path-hit-chain hit) fragment)
      (let* ((text (%edit-convert-line-breaks
                    text (span-document-line-break (path-context-spans context))))
             (last-node (find-if-not (lambda (node) (%edit-plain-whitespace-p source node))
                                     (span-element-children parent)
                                     :from-end t))
             (point (if last-node
                        (span-node-end last-node)
                        (span-element-content-end parent)))
             (empty-p (span-element-empty-p parent)))
        (%edit-finish context point
                      (if empty-p (span-element-start-tag-end parent) point)
                      (cond
                        (empty-p
                         (concatenate 'string ">" text "</" (span-element-qname parent) ">"))
                        ((and last-node (not (span-text-p last-node)))
                         (concatenate 'string
                                      (or (%edit-last-line-indentation source parent) "")
                                      text))
                        (t text))
                      (%dom-rebuild (path-context-dom context) (%path-hit-ordinal-path hit)
                                    (lambda (dom-parent)
                                      (%dom-element-with
                                       dom-parent
                                       :children (append (xml-element-children dom-parent)
                                                         (list appended)))))
                      :lenient-ordinals (%path-hit-ordinal-path hit))))))

(defun edit-xml-set (source path value &key (resource-limits *default-xml-resource-limits*)
                                            (external-entity-policy :reject))
  "Return SOURCE with the target of PATH set to VALUE, changing nothing else.

PATH must select exactly one target.
`.../@name`: an existing attribute keeps its quote character and only its value
text changes; a missing attribute is inserted after the last one.
`.../text()`: the whole content of the element is replaced by VALUE as character
data, or as a CDATA section when the content was exactly one; content that holds
elements, comments, or processing instructions is refused.
An element path: VALUE is a fragment of exactly one element that replaces the
element; it must parse where it is written, with the namespaces in scope there.
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
        (:attribute (%edit-set-attribute context parsed value))
        (:text (%edit-set-text context parsed value))
        (:element (%edit-replace-element context parsed value))))))

(defun edit-xml-delete (source path &key (resource-limits *default-xml-resource-limits*)
                                         (external-entity-policy :reject))
  "Return SOURCE without the target of PATH, changing nothing else.

PATH must select exactly one target. For `.../@name` the attribute and the
whitespace before it are removed. For an element path the element is removed,
with the whitespace and line break before it when nothing else is on its line;
the document element cannot be removed. A `text()` path is not a deletable
target and signals XML-PATH-ERROR with reason :WRONG-TARGET."
  (let ((parsed (%path-parse path)))
    (when (eq (parsed-path-target parsed) :text)
      (error 'xml-path-error
             :path path :reason :wrong-target
             :message "A text() path cannot be deleted."))
    (when (eq (parsed-path-target parsed) :attribute)
      (%edit-refuse-namespace-declaration path parsed))
    (let ((context (%path-context source path
                                  :resource-limits resource-limits
                                  :external-entity-policy external-entity-policy)))
      (ecase (parsed-path-target parsed)
        (:attribute (%edit-delete-attribute context parsed))
        (:element (%edit-delete-element context parsed))))))

(defun edit-xml-append (source path fragment
                        &key (resource-limits *default-xml-resource-limits*)
                             (external-entity-policy :reject))
  "Return SOURCE with the element FRAGMENT added as the last child of the element at PATH.

PATH must select exactly one element, otherwise XML-PATH-ERROR with reason
:WRONG-TARGET for an attribute or text() path, :NO-MATCH, or :MULTIPLE-MATCHES.
FRAGMENT must parse as exactly one element where it is written, with the
namespaces in scope there; surrounding XML whitespace is dropped. It goes after
the last child that is not whitespace, preceded by the line break and indentation
that precede the last indented element child, unless that child is character data,
which the indentation would change; an empty element gets it between a new start
tag end and end tag, and content that is only whitespace keeps it before the
fragment. The result is parsed again and must equal the original document
with the element added."
  (let ((parsed (%path-parse path)))
    (unless (eq (parsed-path-target parsed) :element)
      (error 'xml-path-error
             :path path :reason :wrong-target
             :message "Only an element can have an element appended."))
    (%edit-check-value path fragment)
    (%edit-append-element (%path-context source path
                                         :resource-limits resource-limits
                                         :external-entity-policy external-entity-policy)
                          parsed fragment)))
