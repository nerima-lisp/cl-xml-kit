(in-package #:cl-xml-kit/test)

(defun namespace-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (xml-namespace-error () t)))

(defun well-formedness-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (xml-well-formedness-error () t)))

(it "resolves default namespaces for elements but not attributes"
  (let* ((context (cl-xml-kit::make-xml-namespace-context))
         (child (cl-xml-kit::%namespace-apply-bindings
                 context
                 (list (make-xml-namespace-binding
                        :namespace-uri "urn:example"))))
         (element-name (cl-xml-kit::%namespace-resolve-element-name
                        "item" child))
         (attribute-name (cl-xml-kit::%namespace-resolve-attribute-name
                          "id" child)))
    (expect (xml-name-namespace-uri element-name) :to-equal "urn:example")
    (expect (xml-name-namespace-uri attribute-name) :to-equal "")
    (expect (xml-name-local-name element-name) :to-equal "item")))

(it "shadows prefixed bindings only in the child scope"
  (let* ((root (cl-xml-kit::make-xml-namespace-context
                :bindings (list (make-xml-namespace-binding
                                 :prefix "p" :namespace-uri "urn:one"))))
         (child (cl-xml-kit::%namespace-apply-bindings
                 root
                 (list (make-xml-namespace-binding
                        :prefix "p" :namespace-uri "urn:two"))))
         (root-name (cl-xml-kit::%namespace-resolve-element-name "p:item" root))
         (child-name (cl-xml-kit::%namespace-resolve-element-name "p:item" child)))
    (expect (xml-name-namespace-uri root-name) :to-equal "urn:one")
    (expect (xml-name-namespace-uri child-name) :to-equal "urn:two")))

(it "supports default namespace undeclaration"
  (let* ((context (cl-xml-kit::make-xml-namespace-context))
         (bound (cl-xml-kit::%namespace-apply-bindings
                 context
                 (list (make-xml-namespace-binding
                        :namespace-uri "urn:example"))))
         (unbound (cl-xml-kit::%namespace-apply-bindings
                   bound
                   (list (make-xml-namespace-binding :namespace-uri ""))))
         (name (cl-xml-kit::%namespace-resolve-element-name "item" unbound)))
    (expect (xml-name-namespace-uri name) :to-equal "")))

(it "enforces reserved and undeclared prefixes"
  (let ((context (cl-xml-kit::make-xml-namespace-context)))
    (expect (namespace-error-p
             (lambda ()
               (cl-xml-kit::%namespace-resolve-element-name "p:item" context)))
            :to-equal t)
    (expect (namespace-error-p
             (lambda ()
               (cl-xml-kit::%namespace-declaration-binding
                "xmlns:p" "")))
            :to-equal t)
    (let ((xml-binding
            (cl-xml-kit::%namespace-declaration-binding
             "xmlns:xml" cl-xml-kit::+xml-namespace-uri+)))
      (expect (xml-namespace-binding-prefix xml-binding) :to-equal "xml"))
    (expect (namespace-error-p
             (lambda ()
               (cl-xml-kit::%namespace-declaration-binding
                "xmlns:p" cl-xml-kit::+xml-namespace-uri+)))
            :to-equal t)
    (expect (namespace-error-p
             (lambda ()
               (cl-xml-kit::%namespace-declaration-binding
                "xmlns" cl-xml-kit::+xml-namespace-uri+)))
            :to-equal t)
    (expect (namespace-error-p
             (lambda ()
               (cl-xml-kit::%namespace-resolve-element-name "xmlns:item" context)))
            :to-equal t)))

(it "rejects malformed qualified names and duplicate declarations"
  (let ((context (cl-xml-kit::make-xml-namespace-context)))
    (expect (namespace-error-p
             (lambda ()
               (cl-xml-kit::%namespace-resolve-element-name "a:b:c" context)))
            :to-equal t)
    (expect (namespace-error-p
             (lambda ()
               (cl-xml-kit::%namespace-apply-bindings
                context
                (list (make-xml-namespace-binding
                       :prefix "p" :namespace-uri "urn:one")
                      (make-xml-namespace-binding
                       :prefix "p" :namespace-uri "urn:two")))))
            :to-equal t)))

(it "detects duplicate lexical attributes"
  (let ((context (cl-xml-kit::make-xml-namespace-context)))
    (expect (well-formedness-error-p
             (lambda ()
               (cl-xml-kit::%namespace-resolve-attributes
                (list (cons "id" "one") (cons "id" "two")) context)))
            :to-equal t)))

(it "reports duplicate expanded names after prefix resolution"
  (let ((context (cl-xml-kit::make-xml-namespace-context)))
    (expect (well-formedness-error-p
             (lambda ()
               (cl-xml-kit::%namespace-resolve-attributes
                (list (cons "xmlns:a" "urn:same")
                      (cons "xmlns:b" "urn:same")
                      (cons "a:id" "one")
                      (cons "b:id" "two"))
                context)))
            :to-equal t)))

(it "returns declarations and attributes in source order after resolution"
  (let ((context (cl-xml-kit::make-xml-namespace-context)))
    (multiple-value-bind (attributes declarations child)
        (cl-xml-kit::%namespace-resolve-attributes
         (list (cons "xmlns:a" "urn:a")
               (cons "xmlns:b" "urn:b")
               (cons "b:id" "two")
               (cons "a:class" "one"))
         context)
      (expect (mapcar #'xml-namespace-binding-prefix declarations)
              :to-equal (list "a" "b"))
      (expect (mapcar (lambda (attribute)
                        (xml-name-local-name (xml-attribute-name attribute)))
                      attributes)
              :to-equal (list "id" "class"))
      (expect (xml-name-namespace-uri
               (cl-xml-kit::%namespace-resolve-element-name "a:item" child))
              :to-equal "urn:a"))))

(it "enforces namespace resource limits before scope construction"
  (let ((context (cl-xml-kit::make-xml-namespace-context))
        (limits (make-xml-resource-limits :max-namespace-bindings 1)))
    (expect (handler-case
                (progn
                  (cl-xml-kit::%namespace-apply-bindings
                   context
                   (list (make-xml-namespace-binding
                          :prefix "a" :namespace-uri "urn:a")
                         (make-xml-namespace-binding
                          :prefix "b" :namespace-uri "urn:b"))
                   :resource-limits limits)
                  nil)
              (xml-resource-limit-exceeded () t))
            :to-equal t)))

(it "rejects invalid namespace binding forms and reserved URIs"
  (let ((context (cl-xml-kit::make-xml-namespace-context)))
    (dolist (binding
              (list
               (cl-xml-kit::%make-xml-namespace-binding "xmlns" "urn:x")
               (cl-xml-kit::%make-xml-namespace-binding "xmlfoo" "urn:x")
               (cl-xml-kit::%make-xml-namespace-binding "p" "")
               (cl-xml-kit::%make-xml-namespace-binding nil
                                                         cl-xml-kit::+xml-namespace-uri+)
               (cl-xml-kit::%make-xml-namespace-binding "xml" "urn:wrong")
               (cl-xml-kit::%make-xml-namespace-binding "p"
                                                         cl-xml-kit::+xmlns-namespace-uri+)))
      (expect (namespace-error-p
               (lambda ()
                 (cl-xml-kit::%namespace-apply-bindings
                  context (list binding))))
              :to-equal t))))

(it "validates namespace declaration names and contexts"
  (let ((context (cl-xml-kit::make-xml-namespace-context)))
    (expect (cl-xml-kit::%namespace-declaration-binding "item" "urn:x")
            :to-equal nil)
    (expect (namespace-error-p
             (lambda ()
               (cl-xml-kit::%namespace-declaration-binding "xmlns:" "urn:x")))
            :to-equal t)
    (expect (namespace-error-p
             (lambda ()
               (cl-xml-kit::%namespace-declaration-binding
                "xmlns" cl-xml-kit::+xml-namespace-uri+)))
            :to-equal t)
    (expect (handler-case
                (progn
                  (cl-xml-kit::%namespace-apply-bindings context :not-a-list)
                  nil)
              (xml-input-error () t))
            :to-equal t)
    (expect (handler-case
                (progn
                  (cl-xml-kit::%namespace-apply-bindings
                   context nil :resource-limits :not-limits)
                  nil)
              (xml-input-error () t))
            :to-equal t)))

