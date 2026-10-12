(in-package #:cl-xml-kit)

(defparameter +xml-namespace-uri+
  "http://www.w3.org/XML/1998/namespace")

(defparameter +xmlns-namespace-uri+
  "http://www.w3.org/2000/xmlns/")

(defun %namespace-error (message &key prefix namespace-uri position)
  (error 'xml-namespace-error
         :message message
         :prefix prefix
         :namespace-uri namespace-uri
         :position position))

(defun %namespace-limit-error (kind value limit &key position)
  (error 'xml-resource-limit-exceeded
         :message (format nil "XML resource limit ~S exceeded." kind)
         :kind kind
         :value value
         :limit limit
         :position position))

(defun %namespace-check-name-limit (name resource-limits &key position)
  (let ((limits (or resource-limits *default-xml-resource-limits*)))
    (unless (xml-resource-limits-p limits)
      (%xml-invalid "RESOURCE-LIMITS must be an XML-RESOURCE-LIMITS value."))
    (let ((limit (xml-resource-limits-max-name-length limits))
          (value (length name)))
      (when (> value limit)
        (%namespace-limit-error :max-name-length value limit :position position))))
  name)

(defun %namespace-name-start-char-p (character)
  (let ((code (char-code character)))
    (or (= code #x5F)
        (<= #x41 code #x5A)
        (<= #x61 code #x7A)
        (<= #xC0 code #xD6)
        (<= #xD8 code #xF6)
        (<= #xF8 code #x2FF)
        (<= #x370 code #x37D)
        (<= #x37F code #x1FFF)
        (<= #x200C code #x200D)
        (<= #x2070 code #x218F)
        (<= #x2C00 code #x2FEF)
        (<= #x3001 code #xD7FF)
        (<= #xF900 code #xFDCF)
        (<= #xFDF0 code #xFFFD)
        (<= #x10000 code #xEFFFF))))

(defun %namespace-name-char-p (character)
  (let ((code (char-code character)))
    (or (%namespace-name-start-char-p character)
        (<= #x30 code #x39)
        (member character '(#\- #\.) :test #'char=)
        (= code #xB7)
        (<= #x300 code #x36F)
        (<= #x203F code #x2040))))

(defun %namespace-ncname-p (name)
  (and (stringp name)
       (plusp (length name))
       (not (find #\: name))
       (%namespace-name-start-char-p (char name 0))
       (loop for index from 1 below (length name)
             always (%namespace-name-char-p (char name index)))))

(defun %namespace-xml-prefix-p (prefix)
  (and (stringp prefix)
       (>= (length prefix) 3)
       (string-equal "xml" prefix :end2 3)))

(defun %namespace-split-qname (lexical-name &key resource-limits position)
  (unless (and (stringp lexical-name) (plusp (length lexical-name)))
    (%namespace-error "A qualified name must be a non-empty string."
                      :position position))
  (%namespace-check-name-limit lexical-name resource-limits :position position)
  (let ((colon (position #\: lexical-name)))
    (when (and colon (position #\: lexical-name :start (1+ colon)))
      (%namespace-error "A qualified name may contain only one colon."
                        :position position))
    (if colon
        (let ((prefix (subseq lexical-name 0 colon))
              (local-name (subseq lexical-name (1+ colon))))
          (unless (and (%namespace-ncname-p prefix)
                       (%namespace-ncname-p local-name))
            (%namespace-error "A qualified name must contain two NCNames."
                              :prefix prefix :position position))
          (values prefix local-name))
        (progn
          (unless (%namespace-ncname-p lexical-name)
            (%namespace-error "An unprefixed name must be an NCName."
                              :position position))
          (values nil lexical-name)))))

(defstruct (xml-namespace-context
            (:constructor %make-xml-namespace-context (bindings))
            (:conc-name %xml-namespace-context-)
            (:copier nil))
  "An immutable snapshot of in-scope namespace bindings."
  (bindings nil :read-only t))

(defun %namespace-copy-bindings (bindings)
  (mapcar (lambda (binding)
            (cons (car binding) (copy-seq (cdr binding))))
          bindings))

(defun xml-namespace-context-bindings (context)
  "Return a fresh alist snapshot of CONTEXT's namespace bindings."
  (%namespace-copy-bindings (%xml-namespace-context-bindings context)))

(defun %namespace-binding-uri (context prefix)
  (cdr (assoc prefix (%xml-namespace-context-bindings context) :test #'equal)))

(defun %namespace-binding-present-p (context prefix)
  (not (null (assoc prefix (%xml-namespace-context-bindings context)
                     :test #'equal))))

(defun %namespace-declaration-prefix (binding)
  (xml-namespace-binding-prefix binding))

(defun %namespace-declaration-uri (binding)
  (xml-namespace-binding-namespace-uri binding))

(defun %namespace-validate-binding (binding &key position)
  (unless (xml-namespace-binding-p binding)
    (%xml-invalid "A namespace declaration must be an XML-NAMESPACE-BINDING."))
  (let ((prefix (%namespace-declaration-prefix binding))
        (uri (%namespace-declaration-uri binding)))
    (when (and prefix (not (%namespace-ncname-p prefix)))
      (%namespace-error "A namespace prefix must be an NCName."
                        :prefix prefix :namespace-uri uri :position position))
    (when (and prefix (string= prefix "xmlns"))
      (%namespace-error "The xmlns prefix is reserved for declarations."
                        :prefix prefix :namespace-uri uri :position position))
    (when (and prefix (%namespace-xml-prefix-p prefix)
               (not (string= prefix "xml")))
      (%namespace-error "Prefixes beginning with xml are reserved."
                        :prefix prefix :namespace-uri uri :position position))
    (when (and prefix (zerop (length uri)))
      (%namespace-error "A prefixed namespace cannot be undeclared."
                        :prefix prefix :namespace-uri uri :position position))
    (when (and (string= uri +xml-namespace-uri+)
               (not (and prefix (string= prefix "xml"))))
      (%namespace-error "Only the xml prefix may bind the XML namespace."
                        :prefix prefix :namespace-uri uri :position position))
    (when (and prefix (string= prefix "xml")
               (not (string= uri +xml-namespace-uri+)))
      (%namespace-error "The xml prefix has a fixed namespace URI."
                        :prefix prefix :namespace-uri uri :position position))
    (when (string= uri +xmlns-namespace-uri+)
      (%namespace-error "The XMLNS namespace cannot be declared."
                        :prefix prefix :namespace-uri uri :position position))
    binding))

(defun %namespace-binding-count (bindings)
  (count-if (lambda (binding) (not (equal (car binding) "xml")))
            bindings))

(defun %namespace-apply-bindings
    (context declarations &key resource-limits position)
  "Return CONTEXT extended by DECLARATIONS in source order."
  (unless (typep context 'xml-namespace-context)
    (%xml-invalid "CONTEXT must be an XML-NAMESPACE-CONTEXT value."))
  (%list-value declarations "DECLARATIONS")
  (let ((bindings (%namespace-copy-bindings
                   (%xml-namespace-context-bindings context)))
        (seen nil))
    (dolist (declaration declarations)
      (%namespace-validate-binding declaration :position position)
      (let ((prefix (%namespace-declaration-prefix declaration)))
        (when (member prefix seen :test #'equal)
          (%namespace-error "A namespace prefix may be declared only once on an element."
                            :prefix prefix
                            :namespace-uri (%namespace-declaration-uri declaration)
                            :position position))
        (push prefix seen)
        (let ((entry (assoc prefix bindings :test #'equal)))
          (if entry
              (setf (cdr entry) (copy-seq (%namespace-declaration-uri declaration)))
              (push (cons (copy-seq prefix)
                          (copy-seq (%namespace-declaration-uri declaration)))
                    bindings)))))
    (let ((limits (or resource-limits *default-xml-resource-limits*)))
      (unless (xml-resource-limits-p limits)
        (%xml-invalid "RESOURCE-LIMITS must be an XML-RESOURCE-LIMITS value."))
      (let ((value (%namespace-binding-count bindings))
            (limit (xml-resource-limits-max-namespace-bindings limits)))
        (when (> value limit)
          (%namespace-limit-error :max-namespace-bindings value limit
                                  :position position))))
    (%make-xml-namespace-context bindings)))

(defun make-xml-namespace-context (&key bindings resource-limits position)
  "Return a namespace context containing the reserved XML binding."
  (%list-value bindings "BINDINGS")
  (%namespace-apply-bindings
   (%make-xml-namespace-context
    (list (cons "xml" (copy-seq +xml-namespace-uri+))))
   bindings :resource-limits resource-limits :position position))

(defun %namespace-declaration-binding
    (lexical-name namespace-uri &key resource-limits position)
  "Return a binding for the namespace declaration attribute LEXICAL-NAME."
  (unless (stringp lexical-name)
    (%xml-invalid "A namespace attribute name must be a string."))
  (%namespace-check-name-limit lexical-name resource-limits :position position)
  (cond
    ((string= lexical-name "xmlns")
     (%string-value namespace-uri "NAMESPACE-URI")
     (when (string= namespace-uri +xml-namespace-uri+)
       (%namespace-error "The XML namespace cannot be the default namespace."
                         :namespace-uri namespace-uri :position position))
     (%namespace-validate-binding
      (%make-xml-namespace-binding nil (copy-seq namespace-uri))
      :position position))
    ((and (>= (length lexical-name) 6)
          (string= (subseq lexical-name 0 6) "xmlns:"))
     (let ((prefix (subseq lexical-name 6)))
       (%string-value namespace-uri "NAMESPACE-URI")
       (unless (%namespace-ncname-p prefix)
         (%namespace-error "A namespace declaration must name an NCName prefix."
                           :prefix prefix :namespace-uri namespace-uri
                           :position position))
       (%namespace-validate-binding
        (make-xml-namespace-binding :prefix prefix :namespace-uri namespace-uri)
        :position position)))
    (t nil)))

(defun %namespace-resolve-qname
    (lexical-name context &key attribute-p declaration-p resource-limits position)
  "Resolve LEXICAL-NAME against CONTEXT and return an XML-NAME."
  (unless (typep context 'xml-namespace-context)
    (%xml-invalid "CONTEXT must be an XML-NAMESPACE-CONTEXT value."))
  (multiple-value-bind (prefix local-name)
      (%namespace-split-qname lexical-name
                              :resource-limits resource-limits
                              :position position)
    (when (and (not declaration-p)
               (or (string= local-name "xmlns")
                   (and prefix (string= prefix "xmlns"))))
      (%namespace-error "The xmlns name is reserved for namespace declarations."
                        :prefix prefix :namespace-uri +xmlns-namespace-uri+
                        :position position))
    (if prefix
        (let ((uri (%namespace-binding-uri context prefix)))
          (unless (and (%namespace-binding-present-p context prefix)
                       (plusp (length uri)))
            (%namespace-error "A prefixed name has no in-scope namespace binding."
                              :prefix prefix :namespace-uri uri :position position))
          (when (and (string= prefix "xml")
                     (not (string= uri +xml-namespace-uri+)))
            (%namespace-error "The xml prefix has a fixed namespace URI."
                              :prefix prefix :namespace-uri uri :position position))
          (make-xml-name :local-name local-name :namespace-uri uri :prefix prefix))
        (let ((uri (if attribute-p
                       ""
                       (or (%namespace-binding-uri context nil) ""))))
          (make-xml-name :local-name local-name :namespace-uri uri)))))

(defun %namespace-resolve-element-name
    (lexical-name context &key resource-limits position)
  (%namespace-resolve-qname lexical-name context
                             :resource-limits resource-limits
                             :position position))

(defun %namespace-resolve-attribute-name
    (lexical-name context &key resource-limits position)
  (%namespace-resolve-qname lexical-name context :attribute-p t
                             :resource-limits resource-limits
                             :position position))

(defun %namespace-expanded-name-key (name)
  (list (xml-name-namespace-uri name) (xml-name-local-name name)))

(defun %namespace-check-unique-attributes (attributes &key position)
  "Signal Unique Att Spec when ATTRIBUTES share an expanded name."
  (let ((seen nil))
    (dolist (attribute attributes attributes)
      (unless (xml-attribute-p attribute)
        (%xml-invalid "An attribute must be an XML-ATTRIBUTE value."))
      (let ((name (xml-attribute-name attribute)))
        (unless (xml-name-p name)
          (%xml-invalid "An attribute name must be an XML-NAME value."))
        (let ((key (%namespace-expanded-name-key name)))
          (when (member key seen :test #'equal)
            (error 'xml-well-formedness-error
                   :message "Two attributes have the same expanded name."
                   :constraint :unique-att-spec
                   :expected "a unique expanded attribute name"
                   :position (or (xml-attribute-position attribute) position)))
          (push key seen))))))

(defun %namespace-attribute-entry (entry)
  (unless (and (consp entry) (stringp (car entry)))
    (%xml-invalid "An attribute entry must be a (NAME . VALUE) pair."))
  (unless (stringp (cdr entry))
    (%xml-invalid "An attribute value must be a string."))
  (values (car entry) (cdr entry)))

(defun %namespace-resolve-attributes
    (attribute-entries context &key resource-limits position)
  "Resolve raw (NAME . VALUE) attributes in one namespace scope."
  (%list-value attribute-entries "ATTRIBUTE-ENTRIES")
  (let ((declarations nil)
        (ordinary nil)
        (seen-lexical nil))
    (dolist (entry attribute-entries)
      (multiple-value-bind (lexical-name value)
          (%namespace-attribute-entry entry)
        (when (member lexical-name seen-lexical :test #'string=)
          (error 'xml-well-formedness-error
                 :message "An attribute name occurs more than once."
                 :constraint :unique-att-spec
                 :expected "a unique attribute name"
                 :position position))
        (push lexical-name seen-lexical)
        (let ((declaration (%namespace-declaration-binding
                            lexical-name value
                            :resource-limits resource-limits
                            :position position)))
          (if declaration
              (push declaration declarations)
              (push (cons lexical-name value) ordinary)))))
    (let ((ordered-declarations (nreverse declarations))
          (ordered-ordinary (nreverse ordinary)))
      (let ((child-context
              (%namespace-apply-bindings
               context ordered-declarations
               :resource-limits resource-limits :position position)))
        (let ((attributes
                (mapcar (lambda (entry)
                          (multiple-value-bind (lexical-name value)
                              (%namespace-attribute-entry entry)
                            (make-xml-attribute
                             :name (%namespace-resolve-attribute-name
                                    lexical-name child-context
                                    :resource-limits resource-limits
                                    :position position)
                             :value value)))
                        ordered-ordinary)))
          (%namespace-check-unique-attributes attributes :position position)
          (values attributes ordered-declarations child-context))))))

(defun %namespace-extend-context (context declarations &key resource-limits position)
  (%namespace-apply-bindings context declarations
                             :resource-limits resource-limits
                             :position position))

