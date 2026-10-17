;;;; src/edit-operations.lisp

(in-package #:cl-xml-kit)

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
                     (xml-message :content-references-markup-entity
                                  (span-element-qname element) entity)))
      ((notevery #'span-text-p (span-element-children element))
       (%path-refuse context :mixed-content
                     (xml-message :content-not-text (span-element-qname element)))))))

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
      (%path-refuse context :root-element (xml-message :root-not-deletable)))
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
               (let ((break (position-if #'%line-break-char-p source
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
