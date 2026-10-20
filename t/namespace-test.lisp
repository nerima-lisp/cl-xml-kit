(in-package #:cl-xml-kit/test)

;; Expected namespace outcomes follow Namespaces in XML 1.0 (Third Edition),
;; sections 3 (reserved prefixes), 5 (scoping and defaulting), and 6.3
;; (attribute uniqueness after expansion).

(describe "namespace resolution in documents"
  (it "applies the default namespace to elements but not to attributes"
    (let ((root (parsed-root "<r xmlns='urn:example' id='1'/>")))
      (expect (xml-element-name root) :to-be-xml-name '("r" "urn:example" nil))
      (expect (xml-attribute-name (first (xml-element-attributes root)))
              :to-be-xml-name '("id" "" nil))))

  (it "shadows a prefix binding only inside the child that redeclares it"
    (let ((children (xml-element-children
                     (parsed-root "<p:r xmlns:p='urn:one'><p:c xmlns:p='urn:two'/><p:d/></p:r>"))))
      (expect (mapcar (lambda (child) (xml-name-namespace-uri (xml-element-name child)))
                      children)
              :to-equal '("urn:two" "urn:one"))))

  (it "undeclares the default namespace with an empty value"
    (expect (xml-element-name
             (first (xml-element-children
                     (parsed-root "<r xmlns='urn:example'><c xmlns=''/></r>"))))
            :to-be-xml-name '("c" "" nil)))

  (it "binds the xml prefix without a declaration and accepts its own declaration"
    (let ((root (parsed-root
                 "<r xmlns:xml='http://www.w3.org/XML/1998/namespace' xml:lang='en'/>")))
      (expect (xml-attribute-name (first (xml-element-attributes root)))
              :to-be-xml-name (list "lang" +xml-namespace+ "xml"))
      (expect (mapcar #'xml-namespace-binding-prefix
                      (xml-element-namespace-declarations root))
              :to-equal '("xml"))))

  (it "returns declarations and attributes in source order"
    (let ((root (parsed-root
                 "<r xmlns:a='urn:a' xmlns:b='urn:b' b:id='two' a:class='one'><a:item/></r>")))
      (expect (mapcar #'xml-namespace-binding-prefix (xml-element-namespace-declarations root))
              :to-equal '("a" "b"))
      (expect (mapcar (lambda (attribute) (xml-name-parts (xml-attribute-name attribute)))
                      (xml-element-attributes root))
              :to-equal '(("id" "urn:b" "b") ("class" "urn:a" "a")))
      (expect (xml-element-name (first (xml-element-children root)))
              :to-be-xml-name '("item" "urn:a" "a")))))

(describe "namespace refusals in documents"
  (it-each (("<p:r/>" "p" nil)
            ("<r xmlns:p=''/>" "p" "")
            ("<r xmlns:p='http://www.w3.org/XML/1998/namespace'/>"
             "p" "http://www.w3.org/XML/1998/namespace")
            ("<r xmlns='http://www.w3.org/XML/1998/namespace'/>"
             nil "http://www.w3.org/XML/1998/namespace")
            ("<r xmlns='http://www.w3.org/2000/xmlns/'/>" nil "http://www.w3.org/2000/xmlns/")
            ("<r xmlns:p='http://www.w3.org/2000/xmlns/'/>" "p" "http://www.w3.org/2000/xmlns/")
            ("<r xmlns:xmlns='urn:x'/>" "xmlns" "urn:x")
            ("<r xmlns:xml='urn:x'/>" "xml" "urn:x")
            ("<xmlns:r/>" "xmlns" "http://www.w3.org/2000/xmlns/")
            ("<a:b:c/>" nil nil)
            ("<r a:='v' xmlns:a='urn:a'/>" "a" nil)
            ("<r :a='v'/>" "" nil)
            ("<r xmlns:='urn:x'/>" "" "urn:x"))
      "rejects ~S naming prefix ~S and namespace ~S"
      (document prefix namespace-uri)
    (expect (lambda () (parse-xml document))
            :to-signal-xml `(xml-namespace-error
                             (xml-namespace-error-prefix ,prefix)
                             (xml-namespace-error-namespace-uri ,namespace-uri))))

  (it-each (("<r id='1' id='2'/>" "a unique attribute name")
            ("<r xmlns:a='urn:same' xmlns:b='urn:same' a:id='1' b:id='2'/>"
             "a unique expanded attribute name"))
      "rejects the repeated attribute in ~S expecting ~S"
      (document expected)
    (expect (lambda () (parse-xml document))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :unique-att-spec)
                             (xml-well-formedness-error-expected ,expected))))

  ;; The always-present xml binding is not counted against the limit.
  (it-each (("<r xmlns:a='urn:a' xmlns:b='urn:b' xmlns:c='urn:c'/>" 2 3)
            ("<r xmlns:a='urn:a'><c xmlns:b='urn:b'/></r>" 1 2))
      "bounds the bindings in scope in ~S at ~S"
      (document limit value)
    (expect (lambda ()
              (parse-xml document :resource-limits
                         (make-xml-resource-limits :max-namespace-bindings limit)))
            :to-signal-xml `(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind :max-namespace-bindings)
                             (xml-resource-limit-exceeded-value ,value)
                             (xml-resource-limit-exceeded-limit ,limit))))

  (it "accepts exactly as many bindings as the limit allows"
    (expect (mapcar #'xml-namespace-binding-prefix
                    (xml-element-namespace-declarations
                     (parsed-root "<r xmlns:a='urn:a' xmlns:b='urn:b'/>"
                                  :resource-limits
                                  (make-xml-resource-limits :max-namespace-bindings 2))))
            :to-equal '("a" "b"))))

(describe "namespace resolver functions"
  (it-each (("" nil nil "A qualified name must be a non-empty string.")
            (42 nil nil "A qualified name must be a non-empty string.")
            ("1a" nil nil "An unprefixed name must be an NCName.")
            ("a:b:c" nil nil "A qualified name may contain only one colon.")
            ("p:item" "p" nil "A prefixed name has no in-scope namespace binding.")
            ("xmlns:item" "xmlns" "http://www.w3.org/2000/xmlns/"
             "The xmlns name is reserved for namespace declarations.")
            ("xmlns" nil "http://www.w3.org/2000/xmlns/"
             "The xmlns name is reserved for namespace declarations."))
      "refuses to resolve the qualified name ~S with prefix ~S, namespace ~S, saying ~S"
      (qname prefix namespace-uri message)
    (expect (lambda ()
              (cl-xml-kit::%namespace-resolve-qname
               qname (cl-xml-kit::make-xml-namespace-context)))
            :to-signal-xml `(xml-namespace-error
                             (xml-namespace-error-prefix ,prefix)
                             (xml-namespace-error-namespace-uri ,namespace-uri)
                             (xml-condition-message ,message))))

  (it "applies name limits and retains context slot defaults"
    (let ((defaults (cl-xml-kit::%make-xml-namespace-context-defaults)))
      (expect (cl-xml-kit::%xml-namespace-context-bindings defaults)
              :to-equal nil)
      (expect (lambda ()
                (cl-xml-kit::%namespace-resolve-qname
                 "abcd" (cl-xml-kit::make-xml-namespace-context)
                 :resource-limits (make-xml-resource-limits :max-name-length 3)))
              :to-signal-xml (quote (xml-resource-limit-exceeded
                                     (xml-resource-limit-exceeded-kind :max-name-length)
                                     (xml-resource-limit-exceeded-value 4)
                                     (xml-resource-limit-exceeded-limit 3))))))

  (it-each (("xmlns:p" "" "p" "")
            ("xmlns:" "urn:x" "" "urn:x")
            ("xmlns:1" "urn:x" "1" "urn:x")
            ("xmlns" "http://www.w3.org/XML/1998/namespace"
             nil "http://www.w3.org/XML/1998/namespace")
            ("xmlns:p" "http://www.w3.org/XML/1998/namespace"
             "p" "http://www.w3.org/XML/1998/namespace"))
      "refuses the declaration ~S = ~S naming prefix ~S and namespace ~S"
      (attribute-name value prefix namespace-uri)
    (expect (lambda () (cl-xml-kit::%namespace-declaration-binding attribute-name value))
            :to-signal-xml `(xml-namespace-error
                             (xml-namespace-error-prefix ,prefix)
                             (xml-namespace-error-namespace-uri ,namespace-uri))))

  (it "treats an attribute that is not a declaration as no binding"
    (expect (cl-xml-kit::%namespace-declaration-binding "item" "urn:x") :to-be nil))

  (it-each (("xmlns" "urn:x" "The xmlns prefix is reserved for declarations.")
            ("xmlfoo" "urn:x" "Prefixes beginning with xml are reserved.")
            ("1p" "urn:x" "A namespace prefix must be an NCName.")
            ("p" "" "A prefixed namespace cannot be undeclared.")
            (nil "http://www.w3.org/XML/1998/namespace"
             "Only the xml prefix may bind the XML namespace.")
            ("xml" "urn:wrong" "The xml prefix has a fixed namespace URI.")
            ("p" "http://www.w3.org/2000/xmlns/" "The XMLNS namespace cannot be declared."))
      "refuses to bind prefix ~S to ~S saying ~S"
      (prefix namespace-uri message)
    (expect (lambda ()
              (cl-xml-kit::%namespace-apply-bindings
               (cl-xml-kit::make-xml-namespace-context)
               (list (cl-xml-kit::%make-xml-namespace-binding prefix namespace-uri))))
            :to-signal-xml `(xml-namespace-error
                             (xml-namespace-error-prefix ,prefix)
                             (xml-namespace-error-namespace-uri ,namespace-uri)
                             (xml-condition-message ,message))))

  (it "refuses to declare one prefix twice in one scope"
    (expect (lambda ()
              (cl-xml-kit::%namespace-apply-bindings
               (cl-xml-kit::make-xml-namespace-context)
               (list (make-xml-namespace-binding :prefix "p" :namespace-uri "urn:one")
                     (make-xml-namespace-binding :prefix "p" :namespace-uri "urn:two"))))
            :to-signal-xml '(xml-namespace-error
                             (xml-namespace-error-prefix "p")
                             (xml-namespace-error-namespace-uri "urn:two"))))

  (it "enforces the binding limit before building the scope"
    (expect (lambda ()
              (cl-xml-kit::%namespace-apply-bindings
               (cl-xml-kit::make-xml-namespace-context)
               (list (make-xml-namespace-binding :prefix "a" :namespace-uri "urn:a")
                     (make-xml-namespace-binding :prefix "b" :namespace-uri "urn:b"))
               :resource-limits (make-xml-resource-limits :max-namespace-bindings 1)))
            :to-signal-xml '(xml-resource-limit-exceeded
                             (xml-resource-limit-exceeded-kind :max-namespace-bindings)
                             (xml-resource-limit-exceeded-value 2)
                             (xml-resource-limit-exceeded-limit 1))))
  (it "rejects non-string NCName candidates"
    (expect (cl-xml-kit::%namespace-ncname-p nil) :to-be nil)))

;; :CONTEXT in a row stands for a fresh namespace context.
(describe "namespace resolver arguments"
  (it-each ((cl-xml-kit::%namespace-apply-bindings (:context :not-a-list)
             "DECLARATIONS must be a list.")
            (cl-xml-kit::%namespace-apply-bindings (:context nil :resource-limits :bad)
             "RESOURCE-LIMITS must be an XML-RESOURCE-LIMITS value.")
            (cl-xml-kit::%namespace-apply-bindings (:bad nil)
             "CONTEXT must be an XML-NAMESPACE-CONTEXT value.")
            (cl-xml-kit::%namespace-apply-bindings (:context (:bad))
             "A namespace declaration must be an XML-NAMESPACE-BINDING.")
            (cl-xml-kit::make-xml-namespace-context (:bindings :bad)
             "BINDINGS must be a list.")
            (cl-xml-kit::%namespace-resolve-qname ("a" :bad)
             "CONTEXT must be an XML-NAMESPACE-CONTEXT value.")
            (cl-xml-kit::%namespace-declaration-binding (42 "urn:x")
             "A namespace attribute name must be a string.")
            (cl-xml-kit::%namespace-declaration-binding ("xmlns" 42)
             "NAMESPACE-URI must be a string.")
            (cl-xml-kit::%namespace-declaration-binding ("xmlns:p" 42)
             "NAMESPACE-URI must be a string.")
            (cl-xml-kit::%namespace-resolve-attributes (:bad :context)
             "ATTRIBUTE-ENTRIES must be a list.")
            (cl-xml-kit::%namespace-resolve-attributes ((:bad) :context)
             "An attribute entry must be a (NAME . VALUE) pair.")
            (cl-xml-kit::%namespace-resolve-attributes (((1 . "v")) :context)
             "An attribute entry must be a (NAME . VALUE) pair.")
            (cl-xml-kit::%namespace-resolve-attributes ((("a" . 1)) :context)
             "An attribute value must be a string."))
      "refuses ~A applied to ~{~S~^ ~} saying ~S"
      (function arguments message)
    (expect (lambda ()
              (apply function (substitute (cl-xml-kit::make-xml-namespace-context)
                                          :context arguments)))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message)))))

;; Prefixes and URIs come from small fixed alphabets; every prefix starts with
;; p so it is an NCName that no Namespaces rule reserves.
(it-property "resolves every element and attribute against the binding in scope"
    ((prefix (gen-map (lambda (tail) (concatenate 'string "p" tail))
                      (gen-string :max-length 4 :alphabet "abc-.1")))
     (outer (gen-string :min-length 1 :max-length 6 :alphabet "abcxyz"))
     (inner (gen-string :min-length 1 :max-length 6 :alphabet "abcxyz")))
  (let* ((outer-uri (concatenate 'string "urn:" outer))
         (inner-uri (concatenate 'string "urn:" inner))
         (root (parsed-root
                (format nil "<~A:r xmlns:~A='~A' ~A:a='1'><~A:c xmlns:~A='~A'/></~A:r>"
                        prefix prefix outer-uri prefix prefix prefix inner-uri prefix))))
    (expect (list (xml-name-parts (xml-element-name root))
                  (xml-name-parts (xml-attribute-name (first (xml-element-attributes root))))
                  (xml-name-parts (xml-element-name (first (xml-element-children root)))))
            :to-equal (list (list "r" outer-uri prefix)
                            (list "a" outer-uri prefix)
                            (list "c" inner-uri prefix)))))
