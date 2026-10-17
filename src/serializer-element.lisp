;;;; src/serializer-element.lisp

(in-package #:cl-xml-kit)

(defun %repeated-declaration-p (bindings tail prefix)
  "Return true when a binding of BINDINGS before TAIL declares PREFIX."
  (loop for earlier on bindings
        until (eq earlier tail)
        thereis (equal (xml-namespace-binding-prefix (car earlier)) prefix)))

(defun %element-declarations (element scope path)
  "Return SCOPE extended by the declarations of ELEMENT, and those of them that SCOPE lacks."
  (let ((bindings (xml-element-namespace-declarations element))
        (declarations nil))
    (loop for tail on bindings
          for binding = (car tail)
          do (multiple-value-bind (prefix uri) (%validate-binding binding element path)
               (when (%repeated-declaration-p bindings tail prefix)
                 (%serializer-error element path
                                    (xml-message :repeated-namespace-declaration)))
               (when (and (string= uri +xml-namespace-uri+)
                          (not (and prefix (string= prefix "xml"))))
                 (%serializer-error element path
                                    (xml-message :only-xml-binds-xml-namespace)))
               (unless (%same-uri-p prefix uri scope)
                 (push binding declarations)
                 (setf scope (%put-binding prefix uri scope)))))
    (values scope (nreverse declarations))))

(defun %repeated-attribute-p (attributes tail uri local-name)
  "Return true when an attribute of ATTRIBUTES before TAIL has the expanded name URI, LOCAL-NAME."
  (loop for earlier on attributes
        until (eq earlier tail)
        thereis (let ((name (xml-attribute-name (car earlier))))
                  (and (string= (xml-name-namespace-uri name) uri)
                       (string= (xml-name-local-name name) local-name)))))

(defun %checked-attribute-name (attributes tail path)
  "Validate the name of the attribute at TAIL of ATTRIBUTES and return its URI and prefix."
  (let ((attribute (car tail)))
    (multiple-value-bind (local-name uri prefix)
        (%name-components (xml-attribute-name attribute) attribute path)
      (when (and (null prefix) (string-equal local-name "xmlns"))
        (%serializer-error attribute path (xml-message :xmlns-attribute-reserved)))
      (when (%repeated-attribute-p attributes tail uri local-name)
        (%serializer-error attribute path (xml-message :repeated-expanded-attribute)))
      (values uri prefix))))

(defun %attribute-bindings (state attributes scope declarations path)
  "Validate ATTRIBUTES and bind their namespaces.

Return SCOPE and DECLARATIONS extended by the bindings, and the prefix to write
for each attribute. A namespaced attribute without a prefix gets a generated one."
  (let ((path (and attributes (append path (list :attribute))))
        (prefixes nil))
    (loop for tail on attributes
          for attribute = (car tail)
          do (multiple-value-bind (uri prefix) (%checked-attribute-name attributes tail path)
               (cond
                 ((and (null prefix) (plusp (length uri)))
                  (setf prefix (%generated-prefix state scope)))
                 ((and prefix (zerop (length uri)))
                  (%serializer-error attribute path
                                     (xml-message :prefixed-attribute-needs-uri))))
               (when prefix
                 (multiple-value-setq (scope declarations)
                   (%required-binding prefix uri scope declarations attribute path)))
               (push prefix prefixes)))
    (values scope declarations (nreverse prefixes))))

(defun %write-start-tag (state element local-name prefix declarations attributes prefixes
                         path)
  (%serializer-write state "<" element path)
  (%write-name state local-name prefix element path)
  (dolist (binding declarations)
    (%write-namespace-declaration state binding element path))
  (loop for attribute in attributes
        for attribute-prefix in prefixes
        do (%serializer-write state " " attribute path)
           (%write-attribute state (xml-name-local-name (xml-attribute-name attribute))
                             attribute-prefix attribute path)))

(defun %write-children (state element children scope level path)
  "Write CHILDREN of ELEMENT, one per indented line when pretty printing element-only content."
  (when children
    (let ((child-path (append path (list :child)))
          (lines (and (xml-serializer-state-pretty-print state)
                      (%element-only-children-p children))))
      (dolist (child children)
        (when lines
          (%serializer-write state "\n" element path)
          (%pretty-indent state (1+ level)))
        (%write-node state child scope (1+ level) child-path))
      (when lines
        (%serializer-write state "\n" element path)
        (%pretty-indent state level)))))

(defun %write-element-content (state element children local-name prefix scope level path)
  "Close the start tag of ELEMENT, then write CHILDREN and the end tag unless it is empty."
  (cond
    ((and (null children) (not (xml-serializer-state-canonical state)))
     (%serializer-write state "/>" element path))
    (t
     (%serializer-write state ">" element path)
     (%write-children state element children scope level path)
     (%serializer-write state "</" element path)
     (%write-name state local-name prefix element path)
     (%serializer-write state ">" element path))))

(defun %write-element (state element scope level path)
  (let ((attributes (%effective-attributes (xml-element-attributes element)
                                           (xml-serializer-state-canonical state)))
        (children (xml-element-children element))
        (path (append path (list :element))))
    (multiple-value-bind (local-name uri prefix)
        (%name-components (xml-element-name element) element path)
      (unless (every #'xml-attribute-p attributes)
        (%serializer-error element path (xml-message :element-attributes-invalid)))
      (unless (every #'xml-node-p children)
        (%serializer-error element path (xml-message :element-children-invalid)))
      (multiple-value-bind (scope declarations) (%element-declarations element scope path)
        (multiple-value-setq (scope declarations)
          (%required-binding prefix uri scope declarations element path))
        (multiple-value-bind (scope declarations prefixes)
            (%attribute-bindings state attributes scope declarations path)
          (%write-start-tag state element local-name prefix declarations attributes prefixes
                            path)
          (%write-element-content state element children local-name prefix scope level
                                  path))))))
