;;;; src/serializer-core.lisp

(in-package #:cl-xml-kit)

(defstruct (xml-serializer-state
            (:constructor %make-xml-serializer-state
                (stream resource-limits pretty-print indent canonical output-count
                        generated-prefix-index)))
  stream
  resource-limits
  pretty-print
  indent
  canonical
  (output-count 0)
  (generated-prefix-index 0))

(defun %serializer-error (object path message)
  (error 'xml-serialization-error
         :message message
         :object object
         :path path))

(defun %check-xml-characters (string object path)
  (loop for character across string
        unless (xml-char-p character)
          do (%serializer-error object path
                                (xml-message :character-not-representable
                                             (char-code character)))))

(defun %serializer-emit (state string)
  "Write STRING, whose characters are known to be XML characters, within the output limit."
  (let* ((old-count (xml-serializer-state-output-count state))
         (new-count (+ old-count (length string)))
         (limit (xml-resource-limits-max-output-characters
                 (xml-serializer-state-resource-limits state))))
    (when (> new-count limit)
      (xml-error 'xml-resource-limit-exceeded :output-limit-exceeded
                 :condition-initargs (:kind :max-output-characters
                                      :value new-count :limit limit)))
    (write-string string (xml-serializer-state-stream state))
    (setf (xml-serializer-state-output-count state) new-count)))

(defun %serializer-write (state string object path)
  (%check-xml-characters string object path)
  (%serializer-emit state string))

(defun %markup-character-p (character)
  "Return true when CHARACTER is one that some text or attribute context escapes."
  (case character
    ((#\& #\< #\> #\" #\Tab #\Newline #\Return) t)))

(defun %escape-text (string canonical)
  "Return STRING escaped as character data, or STRING itself when nothing needs escaping."
  (if (notany #'%markup-character-p string)
      string
      (with-output-to-string (output)
        (loop for index from 0 below (length string)
              for character = (char string index)
              do (cond
                   ((char= character #\&) (write-string "&amp;" output))
                   ((char= character #\<) (write-string "&lt;" output))
                   ((and (char= character #\>)
                         (or canonical
                             (and (>= index 2)
                                  (char= (char string (1- index)) #\])
                                  (char= (char string (- index 2)) #\]))))
                    (write-string "&gt;" output))
                   ((and canonical (char= character #\"))
                    (write-string "&quot;" output))
                   ((and canonical (char= character #\Tab))
                    (write-string "&#9;" output))
                   ((and canonical (char= character #\Newline))
                    (write-string "&#10;" output))
                   ((and canonical (char= character #\Return))
                    (write-string "&#13;" output))
                   (t (write-char character output)))))))

(defun %escape-attribute (string)
  "Return STRING escaped as a double-quoted attribute value, or STRING itself when nothing needs escaping."
  (if (notany #'%markup-character-p string)
      string
      (with-output-to-string (output)
        (loop for character across string
              do (case character
                   (#\& (write-string "&amp;" output))
                   (#\< (write-string "&lt;" output))
                   (#\> (write-string "&gt;" output))
                   (#\" (write-string "&quot;" output))
                   (#\Tab (write-string "&#9;" output))
                   (#\Newline (write-string "&#10;" output))
                   (#\Return (write-string "&#13;" output))
                   (otherwise (write-char character output)))))))

(defun %binding (prefix bindings)
  (assoc prefix bindings :test #'equal))

(defun %put-binding (prefix uri bindings)
  (acons prefix uri (remove prefix bindings :key #'car :test #'equal)))

(defun %same-uri-p (prefix uri bindings)
  (let ((binding (%binding prefix bindings)))
    (or (and (null prefix) (zerop (length uri)) (null binding))
        (and binding (string= (cdr binding) uri)))))

(defun %validate-binding (binding object path)
  (unless (xml-namespace-binding-p binding)
    (%serializer-error object path (xml-message :declarations-must-be-bindings)))
  (let ((prefix (xml-namespace-binding-prefix binding))
        (uri (xml-namespace-binding-namespace-uri binding)))
    (when (and prefix (string= prefix "xmlns"))
      (%serializer-error object path (xml-message :xmlns-prefix-reserved)))
    (when (and prefix (string= prefix "xml")
               (not (string= uri +xml-namespace-uri+)))
      (%serializer-error object path (xml-message :xml-prefix-reserved-uri)))
    (when (and (string= uri +xmlns-namespace-uri+)
               (not (and prefix (string= prefix "xmlns"))))
      (%serializer-error object path (xml-message :xmlns-uri-reserved)))
    (values prefix uri)))

(defun %name-components (name object path)
  (unless (xml-name-p name)
    (%serializer-error object path (xml-message :serialized-name-invalid)))
  (let ((local-name (xml-name-local-name name))
        (namespace-uri (xml-name-namespace-uri name))
        (prefix (xml-name-prefix name)))
    (when (or (zerop (length local-name)) (find #\: local-name))
      (%serializer-error object path (xml-message :serialized-local-name-invalid)))
    (when (and prefix (or (zerop (length prefix)) (find #\: prefix)))
      (%serializer-error object path (xml-message :serialized-prefix-invalid)))
    (values local-name namespace-uri prefix)))

(defun %generated-prefix (state bindings)
  (loop
    (incf (xml-serializer-state-generated-prefix-index state))
    (let ((index (1- (xml-serializer-state-generated-prefix-index state))))
      (let ((prefix
              (with-output-to-string (output)
                (write-string "ns" output)
                (loop
                  (write-char (code-char (+ (char-code #\A) (mod index 26)))
                              output)
                  (setf index (1- (floor index 26)))
                  (when (minusp index)
                    (return))))))
      (unless (or (string= prefix "xml")
                  (string= prefix "xmlns")
                  (%binding prefix bindings))
        (return prefix))))))

(defun %pretty-indent (state level)
  (%serializer-emit state (make-string (* level (xml-serializer-state-indent state))
                                       :initial-element #\Space)))

(defun %element-only-children-p (children)
  (and children (every #'xml-element-p children)))

(defun %canonical-attribute< (left right)
  (let* ((left-name (xml-attribute-name left))
         (right-name (xml-attribute-name right))
         (left-uri (xml-name-namespace-uri left-name))
         (right-uri (xml-name-namespace-uri right-name))
         (left-local (xml-name-local-name left-name))
         (right-local (xml-name-local-name right-name)))
    (or (string< left-uri right-uri)
        (and (string= left-uri right-uri)
             (string< left-local right-local)))))

(defun %effective-attributes (attributes canonical)
  (if canonical
      (stable-sort (copy-list attributes) #'%canonical-attribute<)
      attributes))

(defun %write-escaped-text (state string object path)
  (%check-xml-characters string object path)
  (%serializer-emit state (%escape-text string (xml-serializer-state-canonical state))))

(defun %write-escaped-attribute (state string object path)
  (%check-xml-characters string object path)
  (%serializer-emit state (%escape-attribute string)))

(defun %required-binding (prefix uri scope declarations object path)
  (when (and prefix (string= prefix "xml"))
    (unless (string= uri +xml-namespace-uri+)
      (%serializer-error object path (xml-message :xml-prefix-needs-reserved-uri))))
  (when (and prefix (string= prefix "xmlns"))
    (%serializer-error object path (xml-message :xmlns-prefix-names-node)))
  (cond
    ((%same-uri-p prefix uri scope)
     (values scope declarations))
    ((find prefix declarations :key #'xml-namespace-binding-prefix :test #'equal)
     (%serializer-error object path
                        (xml-message :declaration-conflicts-with-name)))
    (t
     (let ((binding (make-xml-namespace-binding :prefix prefix :namespace-uri uri)))
       (values (%put-binding prefix uri scope)
               (append declarations (list binding)))))))

(defun %write-namespace-declaration (state binding object path)
  (multiple-value-bind (prefix uri) (%validate-binding binding object path)
    (%serializer-write state " xmlns" object path)
    (when prefix
      (%serializer-write state ":" object path)
      (%serializer-write state prefix object path))
    (%serializer-write state "=\"" object path)
    (%write-escaped-attribute state uri object path)
    (%serializer-write state "\"" object path)))

(defun %write-name (state local-name prefix object path)
  (when prefix
    (%serializer-write state prefix object path)
    (%serializer-write state ":" object path))
  (%serializer-write state local-name object path))

(defun %write-attribute (state local-name prefix attribute path)
  "Write ATTRIBUTE as its name, from LOCAL-NAME and PREFIX, an equals sign, and its quoted value."
  (%write-name state local-name prefix attribute path)
  (%serializer-write state "=\"" attribute path)
  (%write-escaped-attribute state (xml-attribute-value attribute) attribute path)
  (%serializer-write state "\"" attribute path))

(declaim (ftype function %write-node))

(defun xml-node-p (object)
  "Return true when OBJECT is any serializable XML node."
  (or (xml-document-p object) (xml-element-p object) (xml-attribute-p object)
      (xml-text-p object) (xml-comment-p object)
      (xml-processing-instruction-p object) (xml-doctype-p object)))
