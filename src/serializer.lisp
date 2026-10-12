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

(defun %serializer-error (object path control &rest arguments)
  (error 'xml-serialization-error
         :message (apply #'format nil control arguments)
         :object object
         :path path))

(defun %valid-xml-character-p (character)
  (let ((code (char-code character)))
    (or (= code #x9)
        (= code #xA)
        (= code #xD)
        (<= #x20 code #xD7FF)
        (<= #xE000 code #xFFFD)
        (<= #x10000 code #x10FFFF))))

(defun %check-xml-characters (string object path)
  (loop for character across string
        unless (%valid-xml-character-p character)
          do (%serializer-error object path
                                "Character U+~4,'0X cannot be represented in XML."
                                (char-code character))))

(defun %serializer-write (state string object path)
  (%check-xml-characters string object path)
  (let* ((old-count (xml-serializer-state-output-count state))
         (new-count (+ old-count (length string)))
         (limit (xml-resource-limits-max-output-characters
                 (xml-serializer-state-resource-limits state))))
    (when (> new-count limit)
      (error 'xml-resource-limit-exceeded
             :message "The serialized output exceeds the configured limit."
             :kind :max-output-characters :value new-count :limit limit))
    (write-string string (xml-serializer-state-stream state))
    (setf (xml-serializer-state-output-count state) new-count)))

(defun %escape-text (string canonical)
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
               (t (write-char character output))))))

(defun %escape-attribute (string)
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
               (otherwise (write-char character output))))))

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
    (%serializer-error object path "Namespace declarations must be XML namespace bindings."))
  (let ((prefix (xml-namespace-binding-prefix binding))
        (uri (xml-namespace-binding-namespace-uri binding)))
    (when (and prefix (string= prefix "xmlns"))
      (%serializer-error object path "The xmlns prefix is reserved."))
    (when (and prefix (string= prefix "xml")
               (not (string= uri +xml-namespace-uri+)))
      (%serializer-error object path "The xml prefix has a reserved namespace URI."))
    (when (and (string= uri +xmlns-namespace-uri+)
               (not (and prefix (string= prefix "xmlns"))))
      (%serializer-error object path "The xmlns namespace URI is reserved."))
    (values prefix uri)))

(defun %name-components (name object path)
  (unless (xml-name-p name)
    (%serializer-error object path "A serialized name must be an XML name."))
  (let ((local-name (xml-name-local-name name))
        (namespace-uri (xml-name-namespace-uri name))
        (prefix (xml-name-prefix name)))
    (when (or (zerop (length local-name)) (find #\: local-name))
      (%serializer-error object path "A serialized local name is invalid."))
    (when (and prefix (or (zerop (length prefix)) (find #\: prefix)))
      (%serializer-error object path "A serialized prefix is invalid."))
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

(defun %pretty-indent (state level object path)
  (%serializer-write state
                     (make-string (* level (xml-serializer-state-indent state))
                                  :initial-element #\Space)
                     object path))

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
  (let ((escaped (%escape-text string (xml-serializer-state-canonical state))))
    (%serializer-write state escaped object path)))

(defun %write-escaped-attribute (state string object path)
  (%check-xml-characters string object path)
  (%serializer-write state (%escape-attribute string) object path))

(defun %required-binding (prefix uri scope declarations object path)
  (when (and prefix (string= prefix "xml"))
    (unless (string= uri +xml-namespace-uri+)
      (%serializer-error object path "The xml prefix must use its reserved URI.")))
  (when (and prefix (string= prefix "xmlns"))
    (%serializer-error object path "The xmlns prefix cannot name an XML node."))
  (cond
    ((%same-uri-p prefix uri scope)
     (values scope declarations))
    ((%binding prefix declarations)
     (%serializer-error object path
                        "A namespace declaration conflicts with a node name."))
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

(declaim (ftype function %write-node))

(defun xml-node-p (object)
  "Return true when OBJECT is any serializable XML node."
  (or (xml-document-p object) (xml-element-p object) (xml-attribute-p object)
      (xml-text-p object) (xml-comment-p object)
      (xml-processing-instruction-p object) (xml-doctype-p object)))

(defun %write-element (state element scope level path)
  (let* ((name (xml-element-name element))
         (attributes (%effective-attributes
                      (xml-element-attributes element)
                      (xml-serializer-state-canonical state)))
         (source-declarations (xml-element-namespace-declarations element))
         (children (xml-element-children element))
         (object element)
         (element-path (append path (list :element)))
         (local-name nil)
         (namespace-uri nil)
         (prefix nil))
    (multiple-value-setq (local-name namespace-uri prefix)
      (%name-components name object element-path))
    (unless (every #'xml-attribute-p attributes)
      (%serializer-error object element-path "Element attributes must be XML attributes."))
    (unless (every #'xml-node-p children)
      (%serializer-error object element-path "Element children must be XML nodes."))
    (let ((current-scope scope)
          (declarations nil)
          (attribute-prefixes nil)
          (seen-declaration-prefixes nil)
          (seen-attribute-names nil))
      (dolist (binding source-declarations)
        (multiple-value-bind (binding-prefix binding-uri)
            (%validate-binding binding object element-path)
          (when (member binding-prefix seen-declaration-prefixes :test #'equal)
            (%serializer-error object element-path
                               "An element cannot repeat a namespace declaration."))
          (push binding-prefix seen-declaration-prefixes)
          (when (and (string= binding-uri +xml-namespace-uri+)
                     (not (and binding-prefix (string= binding-prefix "xml"))))
            (%serializer-error object element-path "Only xml may bind the XML namespace."))
          (unless (%same-uri-p binding-prefix binding-uri current-scope)
            (push binding declarations)
            (setf current-scope (%put-binding binding-prefix binding-uri current-scope)))))
      (setf declarations (nreverse declarations))
      (multiple-value-setq (current-scope declarations)
        (%required-binding prefix namespace-uri current-scope declarations
                           object element-path))
      (when (and (null prefix) (zerop (length namespace-uri)))
        (multiple-value-setq (current-scope declarations)
          (%required-binding nil "" current-scope declarations
                             object element-path)))
      (dolist (attribute attributes)
        (let* ((attribute-name (xml-attribute-name attribute))
               (attribute-path (append element-path (list :attribute)))
               (attribute-local nil)
               (attribute-uri nil)
               (attribute-prefix nil))
          (multiple-value-setq (attribute-local attribute-uri attribute-prefix)
            (%name-components attribute-name attribute attribute-path))
          (when (and (null attribute-prefix)
                     (string-equal attribute-local "xmlns"))
            (%serializer-error attribute attribute-path
                               "The xmlns attribute is reserved for namespace declarations."))
          (let ((expanded-name (cons attribute-uri attribute-local)))
            (when (member expanded-name seen-attribute-names :test #'equal)
              (%serializer-error attribute attribute-path
                                 "An element cannot repeat an expanded attribute name."))
            (push expanded-name seen-attribute-names))
          (when (and (null attribute-prefix) (plusp (length attribute-uri)))
            (setf attribute-prefix (%generated-prefix state current-scope))
            (multiple-value-setq (current-scope declarations)
              (%required-binding attribute-prefix attribute-uri current-scope
                                 declarations attribute attribute-path)))
          (when attribute-prefix
            (when (zerop (length attribute-uri))
              (%serializer-error attribute attribute-path
                                 "A prefixed attribute must have a namespace URI."))
            (multiple-value-setq (current-scope declarations)
              (%required-binding attribute-prefix attribute-uri current-scope
                                 declarations attribute attribute-path)))
          (push (list attribute attribute-local attribute-prefix) attribute-prefixes)))
      (setf attribute-prefixes (nreverse attribute-prefixes))
      (%serializer-write state "<" object element-path)
      (%write-name state local-name prefix object element-path)
      (dolist (binding declarations)
        (%write-namespace-declaration state binding object element-path))
      (dolist (attribute-info attribute-prefixes)
        (destructuring-bind (attribute attribute-local attribute-prefix) attribute-info
          (%serializer-write state " " attribute element-path)
          (%write-name state attribute-local attribute-prefix attribute element-path)
          (%serializer-write state "=\"" attribute element-path)
          (%write-escaped-attribute state (xml-attribute-value attribute)
                                     attribute element-path)
          (%serializer-write state "\"" attribute element-path)))
      (cond
        ((and (null children)
              (not (xml-serializer-state-canonical state)))
         (%serializer-write state "/>" object element-path))
        (t
         (%serializer-write state ">" object element-path)
         (if (and (xml-serializer-state-pretty-print state)
                  (%element-only-children-p children))
             (progn
               (dolist (child children)
                 (%serializer-write state "\n" object element-path)
                 (%pretty-indent state (1+ level) object element-path)
                 (%write-node state child current-scope (1+ level)
                              (append element-path (list :child))))
               (%serializer-write state "\n" object element-path)
               (%pretty-indent state level object element-path))
             (dolist (child children)
               (%write-node state child current-scope (1+ level)
                            (append element-path (list :child)))))
         (%serializer-write state "</" object element-path)
         (%write-name state local-name prefix object element-path)
         (%serializer-write state ">" object element-path))))))

(defun %write-doctype (state doctype path)
  (let ((root-name (xml-doctype-root-name doctype))
        (public-id (xml-doctype-public-id doctype))
        (system-id (xml-doctype-system-id doctype))
        (internal-subset (xml-doctype-internal-subset doctype)))
    (%check-xml-characters root-name doctype path)
    (when (xml-serializer-state-canonical state)
      (let ((notations
              (with-output-to-string (output)
                (when internal-subset
                  (with-input-from-string (input internal-subset)
                    (loop for line = (read-line input nil nil)
                          while line
                          when (search "<!NOTATION" (string-trim '(#\Space #\Tab) line))
                            do (write-string (string-trim '(#\Space #\Tab) line) output)
                               (write-char #\Newline output)))))))
        (unless (plusp (length notations))
          (return-from %write-doctype nil))
        (%serializer-write state "<!DOCTYPE " doctype path)
        (%serializer-write state root-name doctype path)
        (%serializer-write state " [" doctype path)
        (%serializer-write state (string #\Newline) doctype path)
        (%serializer-write state notations doctype path)
        (%serializer-write state "]>" doctype path)
        (return-from %write-doctype nil)))
    (%serializer-write state "<!DOCTYPE " doctype path)
    (%serializer-write state root-name doctype path)
    (cond
      (public-id
       (%serializer-write state " PUBLIC \"" doctype path)
       (%write-escaped-attribute state public-id doctype path)
       (%serializer-write state "\"" doctype path)
       (when system-id
         (%serializer-write state " \"" doctype path)
         (%write-escaped-attribute state system-id doctype path)
         (%serializer-write state "\"" doctype path)))
      (system-id
       (%serializer-write state " SYSTEM \"" doctype path)
       (%write-escaped-attribute state system-id doctype path)
       (%serializer-write state "\"" doctype path)))
    (when internal-subset
      (%serializer-write state " [" doctype path)
      (%serializer-write state internal-subset doctype path)
      (%serializer-write state "]" doctype path))
    (%serializer-write state ">" doctype path)))

(defun %write-node (state object scope level path)
  (cond
    ((xml-document-p object)
     (let ((children (xml-document-children object)))
       (unless (every #'xml-node-p children)
         (%serializer-error object path "Document children must be XML nodes."))
       (dolist (child children)
         (unless (and (xml-serializer-state-canonical state)
                      (xml-text-p child)
                      (every #'%xml-space-p (xml-text-data child)))
           (%write-node state child scope level (append path (list :child)))))))
    ((xml-element-p object)
     (%write-element state object scope level path))
    ((xml-attribute-p object)
     (multiple-value-bind (local-name namespace-uri prefix)
         (%name-components (xml-attribute-name object) object path)
       (when (and prefix (zerop (length namespace-uri)))
         (%serializer-error object path "A prefixed attribute must have a namespace URI."))
       (%write-name state local-name prefix object path)
       (%serializer-write state "=\"" object path)
       (%write-escaped-attribute state (xml-attribute-value object) object path)
       (%serializer-write state "\"" object path)))
    ((xml-text-p object)
     (%write-escaped-text state (xml-text-data object) object path))
    ((xml-comment-p object)
     (when (xml-serializer-state-canonical state)
       (return-from %write-node nil))
     (let ((data (xml-comment-data object)))
       (%check-xml-characters data object path)
       (when (or (search "--" data) (and (plusp (length data))
                                         (char= (char data (1- (length data))) #\-)))
         (%serializer-error object path "Comment data is not representable in XML."))
       (%serializer-write state "<!--" object path)
       (%serializer-write state data object path)
       (%serializer-write state "-->" object path)))
    ((xml-processing-instruction-p object)
     (let ((target (xml-processing-instruction-target object))
           (data (xml-processing-instruction-data object)))
       (%check-xml-characters target object path)
       (when (string-equal "xml" target)
         (%serializer-error object path "Processing-instruction target XML is reserved."))
       (when (and data (search "?>" data))
         (%serializer-error object path "Processing-instruction data contains ?>."))
       (%serializer-write state "<?" object path)
       (%serializer-write state target object path)
       (when (or data (xml-serializer-state-canonical state))
         (%serializer-write state " " object path)
         (when data
           (%check-xml-characters data object path)
           (%serializer-write state data object path)))
       (%serializer-write state "?>" object path)))
    ((xml-doctype-p object)
     (%write-doctype state object path))
    (t
     (%serializer-error object path "Object is not a serializable XML node."))))

(defun %serializer-state (stream &key pretty-print indent resource-limits canonical)
  (unless (streamp stream)
    (%serializer-error stream nil "STREAM must be a character stream."))
  (unless (xml-resource-limits-p resource-limits)
    (%serializer-error resource-limits nil "RESOURCE-LIMITS must be XML resource limits."))
  (unless (and (integerp indent) (not (minusp indent)))
    (%serializer-error indent nil "INDENT must be a non-negative integer."))
  (%make-xml-serializer-state stream resource-limits pretty-print indent canonical 0 0))

(defun %write-xml (object stream &key (pretty-print nil) (indent 2)
                                           resource-limits (canonical nil))
  (unless (or (xml-document-p object) (xml-element-p object)
              (xml-attribute-p object) (xml-text-p object)
              (xml-comment-p object) (xml-processing-instruction-p object)
              (xml-doctype-p object))
    (%serializer-error object nil "OBJECT is not a serializable XML node."))
  (let ((state (%serializer-state stream :pretty-print pretty-print
                                  :indent indent :resource-limits resource-limits
                                  :canonical canonical)))
    (%write-node state object (list (cons "xml" +xml-namespace-uri+)) 0 nil)
    stream))

(defun write-xml (object stream &key (pretty-print nil) (indent 2)
                                   (resource-limits *default-xml-resource-limits*))
  "Write OBJECT as XML to character STREAM and return STREAM."
  (%write-xml object stream :pretty-print pretty-print :indent indent
              :resource-limits resource-limits :canonical nil))

(defun xml->string (object &key (pretty-print nil) (indent 2)
                                (resource-limits *default-xml-resource-limits*))
  "Return OBJECT serialized as a fresh XML string."
  (with-output-to-string (stream)
    (write-xml object stream :pretty-print pretty-print :indent indent
               :resource-limits resource-limits)))

(defun write-xml-canonical (object stream
                             &key (resource-limits *default-xml-resource-limits*))
  "Write OBJECT in XML Test Suite Second Canonical Form to STREAM."
  (%write-xml object stream :resource-limits resource-limits :canonical t))

(defun xml->canonical-string
    (object &key (resource-limits *default-xml-resource-limits*))
  "Return OBJECT in XML Test Suite Second Canonical Form as a fresh string."
  (with-output-to-string (stream)
    (write-xml-canonical object stream :resource-limits resource-limits)))

(loop for (name . text) in
      '((write-xml . "Write an XML node to a character stream.")
        (xml->string . "Return an XML node as a string.")
        (write-xml-canonical . "Write an XML Test Suite canonical XML node to a character stream.")
        (xml->canonical-string . "Return an XML Test Suite canonical XML node as a string.")
        (xml-node-p . "Return true when OBJECT is a serializable XML node."))
      do (setf (documentation name 'function) text))

