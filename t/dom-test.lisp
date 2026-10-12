(in-package #:cl-xml-kit/test)

(it "builds immutable DOM nodes with source order"
  (let* ((document (parse-xml "<!-- before --><p xmlns=\"urn:p\"><a/>text<b/></p>"))
         (children (xml-document-children document))
         (root (second children))
         (root-children (xml-element-children root)))
    (expect (xml-comment-p (first children)) :to-equal t)
    (expect (xml-name-namespace-uri (xml-element-name root)) :to-equal "urn:p")
    (expect (mapcar (lambda (node)
                      (if (xml-element-p node)
                          (xml-name-local-name (xml-element-name node))
                          (xml-text-data node)))
                    root-children)
            :to-equal '("a" "text" "b"))))

(it "does not apply the default namespace to attributes"
  (let* ((root (first (xml-document-children
                        (parse-xml "<p xmlns=\"urn:p\" id=\"x\"/>"))))
         (attribute (first (xml-element-attributes root))))
    (expect (xml-name-namespace-uri (xml-element-name root)) :to-equal "urn:p")
    (expect (xml-name-namespace-uri (xml-attribute-name attribute)) :to-equal "")))

(it "rejects duplicate expanded attribute names"
  (expect (handler-case
              (progn (parse-xml "<p xmlns:a=\"urn:x\" xmlns:b=\"urn:x\" a:id=\"1\" b:id=\"2\"/>") nil)
            (xml-well-formedness-error () t))
          :to-equal t))

(it "constructs and snapshots every public node type"
  (let* ((position (make-xml-position :source "nodes.xml" :offset 2))
         (end-position (make-xml-position :source "nodes.xml" :offset 8))
         (name (make-xml-name :local-name "root" :namespace-uri "urn:test"
                              :prefix "p"))
         (binding (make-xml-namespace-binding :prefix "p"
                                              :namespace-uri "urn:test"))
         (attribute (make-xml-attribute :name (make-xml-name :local-name "id")
                                        :value "1" :position position
                                        :end-position end-position))
         (text (make-xml-text :data "text" :position position
                              :end-position end-position))
         (comment (make-xml-comment :data "comment" :position position
                                    :end-position end-position))
         (processing-instruction
           (make-xml-processing-instruction :target "pi" :data "data"
                                            :position position
                                            :end-position end-position))
         (doctype (make-xml-doctype :root-name "root" :public-id "pub"
                                    :system-id "sys" :internal-subset "subset"
                                    :position position :end-position end-position))
         (element (make-xml-element :name name :attributes (list attribute)
                                    :namespace-declarations (list binding)
                                    :children (list text comment processing-instruction)
                                    :position position :end-position end-position))
         (document (make-xml-document :children (list element doctype)
                                      :position position :end-position end-position)))
    (expect (list (xml-position-source (xml-document-position document))
                  (xml-position-offset (xml-element-end-position element))
                  (xml-attribute-value (first (xml-element-attributes element)))
                  (xml-text-data text) (xml-comment-data comment)
                  (xml-processing-instruction-target processing-instruction)
                  (xml-processing-instruction-data processing-instruction)
                  (xml-doctype-public-id doctype)
                  (xml-doctype-system-id doctype)
                  (xml-doctype-internal-subset doctype))
            :to-equal '("nodes.xml" 8 "1" "text" "comment" "pi" "data"
                         "pub" "sys" "subset"))
    (let ((children (xml-document-children document)))
      (setf (cdr children) nil)
      (expect (length (xml-document-children document)) :to-equal 2))))

(it "exposes every node position and name accessor"
  (let* ((position (make-xml-position :source "accessors.xml" :offset 1))
         (end-position (make-xml-position :source "accessors.xml" :offset 9))
         (name (make-xml-name :local-name "root" :namespace-uri "urn:test"
                              :prefix "p"))
         (binding (make-xml-namespace-binding :prefix "p"
                                              :namespace-uri "urn:test"))
         (attribute (make-xml-attribute :name name :value "value"
                                        :position position
                                        :end-position end-position))
         (text (make-xml-text :data "text" :position position
                              :end-position end-position))
         (comment (make-xml-comment :data "comment" :position position
                                    :end-position end-position))
         (instruction (make-xml-processing-instruction
                       :target "pi" :data "data" :position position
                       :end-position end-position))
         (doctype (make-xml-doctype :root-name "root" :position position
                                    :end-position end-position))
         (element (make-xml-element :name name :attributes (list attribute)
                                    :namespace-declarations (list binding)
                                    :children (list text) :position position
                                    :end-position end-position))
         (document (make-xml-document :children (list element)
                                      :position position
                                      :end-position end-position)))
    (expect (list (xml-position-source position)
                  (xml-position-offset position)
                  (xml-position-line position)
                  (xml-position-column position)
                  (xml-name-local-name name)
                  (xml-name-namespace-uri name)
                  (xml-name-prefix name)
                  (xml-namespace-binding-p binding)
                  (xml-namespace-binding-prefix binding)
                  (xml-namespace-binding-namespace-uri binding)
                  (xml-document-position document)
                  (xml-document-end-position document)
                  (xml-element-name element)
                  (xml-element-attributes element)
                  (xml-element-namespace-declarations element)
                  (xml-element-children element)
                  (xml-element-position element)
                  (xml-element-end-position element)
                  (xml-attribute-name attribute)
                  (xml-attribute-position attribute)
                  (xml-attribute-end-position attribute)
                  (xml-text-position text)
                  (xml-text-end-position text)
                  (xml-comment-position comment)
                  (xml-comment-end-position comment)
                  (xml-processing-instruction-position instruction)
                  (xml-processing-instruction-end-position instruction)
                  (xml-doctype-root-name doctype)
                  (xml-doctype-position doctype)
                  (xml-doctype-end-position doctype))
            :to-be-truthy)
    (expect (list (xml-document-p document) (xml-element-p element)
                  (xml-attribute-p attribute) (xml-text-p text)
                  (xml-comment-p comment)
                  (xml-processing-instruction-p instruction)
                  (xml-doctype-p doctype))
            :to-equal '(t t t t t t t))))

(it "rejects invalid node constructor values"
  (dolist (form '((make-xml-position :offset -1)
                  (make-xml-position :source 1)
                  (make-xml-position :line 0)
                  (make-xml-position :column 0)
                  (make-xml-name :local-name "a:b")
                  (make-xml-name :local-name nil)
                  (make-xml-name :local-name "a" :namespace-uri 1)
                  (make-xml-name :local-name "a" :prefix 1)
                  (make-xml-name :local-name "a" :prefix "p:q")
                  (make-xml-namespace-binding :prefix 1 :namespace-uri "urn:x")
                  (make-xml-namespace-binding :prefix "p" :namespace-uri nil)
                  (make-xml-text :data nil)
                  (make-xml-attribute :name "id" :value "1")
                  (make-xml-attribute :name (make-xml-name :local-name "id") :value nil)
                  (make-xml-element :name "root")
                  (make-xml-element :name (make-xml-name :local-name "root")
                                    :attributes :not-a-list)
                  (make-xml-element :name (make-xml-name :local-name "root")
                                    :namespace-declarations :not-a-list)
                  (make-xml-element :name (make-xml-name :local-name "root")
                                    :children :not-a-list)
                  (make-xml-document :children :not-a-list)
                  (make-xml-doctype :root-name "")
                  (make-xml-doctype :root-name "root" :public-id 1)
                  (make-xml-processing-instruction :target "pi" :data 1)))
    (expect (handler-case (progn (eval form) nil)
              (xml-input-error () t))
            :to-be-truthy)))

