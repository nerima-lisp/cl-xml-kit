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
         :message (xml-message :resource-limit-exceeded kind)
         :kind kind
         :value value
         :limit limit
         :position position))

(defun %namespace-limits (resource-limits)
  (let ((limits (or resource-limits *default-xml-resource-limits*)))
    (unless (xml-resource-limits-p limits)
      (xml-error 'xml-input-error :resource-limits-value-required))
    limits))

(defun %namespace-check-name-limit (name resource-limits &key position)
  (let ((limit (xml-resource-limits-max-name-length
                (%namespace-limits resource-limits)))
        (value (length name)))
    (when (> value limit)
      (%namespace-limit-error :max-name-length value limit :position position)))
  name)

(defun %namespace-ncname-p (name)
  (and (stringp name)
       (xml-ncname-p (coerce name 'simple-string))))

(defun %namespace-xml-prefix-p (prefix)
  (and (stringp prefix)
       (>= (length prefix) 3)
       (string-equal "xml" prefix :end2 3)))

(defun %namespace-split-qname (lexical-name &key resource-limits position)
  (unless (and (stringp lexical-name) (plusp (length lexical-name)))
    (%namespace-error (xml-message :qname-not-string)
                      :position position))
  (%namespace-check-name-limit lexical-name resource-limits :position position)
  (let ((colon (position #\: lexical-name)))
    (when (and colon (position #\: lexical-name :start (1+ colon)))
      (%namespace-error (xml-message :qname-two-colons)
                        :position position))
    (if colon
        (let ((prefix (subseq lexical-name 0 colon))
              (local-name (subseq lexical-name (1+ colon))))
          (unless (and (%namespace-ncname-p prefix)
                       (%namespace-ncname-p local-name))
            (%namespace-error (xml-message :qname-not-two-ncnames)
                              :prefix prefix :position position))
          (values prefix local-name))
        (progn
          (unless (%namespace-ncname-p lexical-name)
            (%namespace-error (xml-message :unprefixed-name-not-ncname)
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

(defun %namespace-validate-binding (binding &key position)
  (unless (xml-namespace-binding-p binding)
    (xml-error 'xml-input-error :namespace-binding-required))
  (let ((prefix (xml-namespace-binding-prefix binding))
        (uri (xml-namespace-binding-namespace-uri binding)))
    (when (and prefix (not (%namespace-ncname-p prefix)))
      (%namespace-error (xml-message :prefix-not-ncname)
                        :prefix prefix :namespace-uri uri :position position))
    (when (and prefix (string= prefix "xmlns"))
      (%namespace-error (xml-message :xmlns-prefix-reserved-for-declarations)
                        :prefix prefix :namespace-uri uri :position position))
    (when (and prefix (%namespace-xml-prefix-p prefix)
               (not (string= prefix "xml")))
      (%namespace-error (xml-message :xml-prefixes-reserved)
                        :prefix prefix :namespace-uri uri :position position))
    (when (and prefix (zerop (length uri)))
      (%namespace-error (xml-message :prefixed-namespace-undeclared)
                        :prefix prefix :namespace-uri uri :position position))
    (when (and (string= uri +xml-namespace-uri+)
               (not (and prefix (string= prefix "xml"))))
      (%namespace-error (xml-message :only-xml-prefix-binds-xml-namespace)
                        :prefix prefix :namespace-uri uri :position position))
    (when (and prefix (string= prefix "xml")
               (not (string= uri +xml-namespace-uri+)))
      (%namespace-error (xml-message :xml-prefix-fixed-uri)
                        :prefix prefix :namespace-uri uri :position position))
    (when (string= uri +xmlns-namespace-uri+)
      (%namespace-error (xml-message :xmlns-namespace-not-declarable)
                        :prefix prefix :namespace-uri uri :position position))
    binding))

(defun %namespace-check-binding-limit (bindings resource-limits position)
  (let ((value (count-if (lambda (binding) (not (equal (car binding) "xml")))
                         bindings))
        (limit (xml-resource-limits-max-namespace-bindings
                (%namespace-limits resource-limits))))
    (when (> value limit)
      (%namespace-limit-error :max-namespace-bindings value limit
                              :position position))))

(defun %namespace-apply-bindings
    (context declarations &key resource-limits position)
  "Return CONTEXT extended by DECLARATIONS in source order."
  (unless (typep context 'xml-namespace-context)
    (xml-error 'xml-input-error :namespace-context-required))
  (%list-value declarations "DECLARATIONS")
  (let ((bindings (%namespace-copy-bindings
                   (%xml-namespace-context-bindings context)))
        (seen nil))
    (dolist (declaration declarations)
      (%namespace-validate-binding declaration :position position)
      (let ((prefix (xml-namespace-binding-prefix declaration)))
        (when (member prefix seen :test #'equal)
          (%namespace-error (xml-message :prefix-declared-twice)
                            :prefix prefix
                            :namespace-uri (xml-namespace-binding-namespace-uri declaration)
                            :position position))
        (push prefix seen)
        (let ((entry (assoc prefix bindings :test #'equal)))
          (if entry
              (setf (cdr entry) (copy-seq (xml-namespace-binding-namespace-uri declaration)))
              (push (cons (copy-seq prefix)
                          (copy-seq (xml-namespace-binding-namespace-uri declaration)))
                    bindings)))))
    (%namespace-check-binding-limit bindings resource-limits position)
    (%make-xml-namespace-context bindings)))

(defun make-xml-namespace-context (&key bindings resource-limits position)
  "Return a namespace context containing the reserved XML binding."
  (%list-value bindings "BINDINGS")
  (%namespace-apply-bindings
   (%make-xml-namespace-context
    (list (cons "xml" (copy-seq +xml-namespace-uri+))))
   bindings :resource-limits resource-limits :position position))

(defun %namespace-declaration-name-p (qname)
  (or (string= qname "xmlns")
      (and (>= (length qname) 6) (string= "xmlns:" qname :end2 6))))

(defun %namespace-declaration-binding
    (lexical-name namespace-uri &key resource-limits position)
  "Return a binding for the namespace declaration attribute LEXICAL-NAME."
  (unless (stringp lexical-name)
    (xml-error 'xml-input-error :label-must-be-string
               :format-arguments ("A namespace attribute name")))
  (%namespace-check-name-limit lexical-name resource-limits :position position)
  (cond
    ((string= lexical-name "xmlns")
     (%string-value namespace-uri "NAMESPACE-URI")
     (when (string= namespace-uri +xml-namespace-uri+)
       (%namespace-error (xml-message :xml-namespace-not-default)
                         :namespace-uri namespace-uri :position position))
     (%namespace-validate-binding
      (%make-xml-namespace-binding nil (copy-seq namespace-uri))
      :position position))
    ((%namespace-declaration-name-p lexical-name)
     (let ((prefix (subseq lexical-name 6)))
       (%string-value namespace-uri "NAMESPACE-URI")
       (unless (%namespace-ncname-p prefix)
         (%namespace-error (xml-message :declaration-prefix-not-ncname)
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
    (xml-error 'xml-input-error :namespace-context-required))
  (multiple-value-bind (prefix local-name)
      (%namespace-split-qname lexical-name
                              :resource-limits resource-limits
                              :position position)
    (when (and (not declaration-p)
               (or (string= local-name "xmlns")
                   (and prefix (string= prefix "xmlns"))))
      (%namespace-error (xml-message :xmlns-name-reserved)
                        :prefix prefix :namespace-uri +xmlns-namespace-uri+
                        :position position))
    (if prefix
        (let ((uri (%namespace-binding-uri context prefix)))
          (unless (and (%namespace-binding-present-p context prefix)
                       (plusp (length uri)))
            (%namespace-error (xml-message :prefix-not-bound)
                              :prefix prefix :namespace-uri uri :position position))
          (when (and (string= prefix "xml")
                     (not (string= uri +xml-namespace-uri+)))
            (%namespace-error (xml-message :xml-prefix-fixed-uri)
                              :prefix prefix :namespace-uri uri :position position))
          (make-xml-name :local-name local-name :namespace-uri uri :prefix prefix))
        (let ((uri (if attribute-p
                       ""
                       (or (%namespace-binding-uri context nil) ""))))
          (make-xml-name :local-name local-name :namespace-uri uri)))))

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
        (xml-error 'xml-input-error :attribute-required))
      (let ((name (xml-attribute-name attribute)))
        (unless (xml-name-p name)
          (xml-error 'xml-input-error :attribute-name-required))
        (let ((key (%namespace-expanded-name-key name)))
          (when (member key seen :test #'equal)
            (xml-error 'xml-well-formedness-error :duplicate-expanded-attribute
                       :condition-initargs
                       (:constraint :unique-att-spec
                        :expected "a unique expanded attribute name"
                        :position (or (xml-attribute-position attribute) position))))
          (push key seen))))))

(defun %namespace-attribute-entry (entry)
  (unless (and (consp entry) (stringp (car entry)))
    (xml-error 'xml-input-error :attribute-entry-invalid))
  (unless (stringp (cdr entry))
    (xml-error 'xml-input-error :label-must-be-string
               :format-arguments ("An attribute value")))
  (values (car entry) (cdr entry)))

(defun %namespace-partition-attributes (attribute-entries resource-limits position)
  "Return the namespace declarations and the ordinary (NAME . VALUE) entries
of ATTRIBUTE-ENTRIES, each in source order."
  (let ((declarations nil)
        (ordinary nil)
        (seen-lexical nil))
    (dolist (entry attribute-entries)
      (multiple-value-bind (lexical-name value)
          (%namespace-attribute-entry entry)
        (when (member lexical-name seen-lexical :test #'string=)
          (xml-error 'xml-well-formedness-error :duplicate-attribute
                     :condition-initargs
                     (:constraint :unique-att-spec
                      :expected "a unique attribute name"
                      :position position)))
        (push lexical-name seen-lexical)
        (let ((declaration (%namespace-declaration-binding
                            lexical-name value
                            :resource-limits resource-limits
                            :position position)))
          (if declaration
              (push declaration declarations)
              (push (cons lexical-name value) ordinary)))))
    (values (nreverse declarations) (nreverse ordinary))))

(defun %namespace-resolve-attributes
    (attribute-entries context &key resource-limits position)
  "Resolve raw (NAME . VALUE) attributes in one namespace scope."
  (%list-value attribute-entries "ATTRIBUTE-ENTRIES")
  (multiple-value-bind (declarations ordinary)
      (%namespace-partition-attributes attribute-entries resource-limits position)
    (let* ((child-context (%namespace-apply-bindings
                           context declarations
                           :resource-limits resource-limits :position position))
           (attributes
             (mapcar (lambda (entry)
                       (make-xml-attribute
                        :name (%namespace-resolve-attribute-name
                               (car entry) child-context
                               :resource-limits resource-limits
                               :position position)
                        :value (cdr entry)))
                     ordinary)))
      (%namespace-check-unique-attributes attributes :position position)
      (values attributes declarations child-context))))
