(in-package #:cl-xml-kit/test)

(defun sample-nodes ()
  "Return a plist of one node of every public type, each built with every field
supplied. Starts sit at offset 2 and ends at offset 8 of source nodes.xml."
  (let* ((position (make-xml-position :source "nodes.xml" :offset 2))
         (end-position (make-xml-position :source "nodes.xml" :offset 8 :line 3 :column 4))
         (name (make-xml-name :local-name "root" :namespace-uri "urn:test" :prefix "p"))
         (binding (make-xml-namespace-binding :prefix "p" :namespace-uri "urn:test"))
         (attribute (make-xml-attribute :name (make-xml-name :local-name "id") :value "1"
                                        :position position :end-position end-position))
         (text (make-xml-text :data "text" :position position :end-position end-position))
         (comment (make-xml-comment :data "comment" :position position
                                    :end-position end-position))
         (instruction (make-xml-processing-instruction :target "pi" :data "data"
                                                       :position position
                                                       :end-position end-position))
         (doctype (make-xml-doctype :root-name "root" :public-id "pub" :system-id "sys"
                                    :internal-subset "subset" :position position
                                    :end-position end-position))
         (element (make-xml-element :name name :attributes (list attribute)
                                    :namespace-declarations (list binding)
                                    :children (list text comment instruction)
                                    :position position :end-position end-position))
         (document (make-xml-document :children (list doctype element) :position position
                                      :end-position end-position)))
    (list :position position :end-position end-position :name name :binding binding
          :attribute attribute :text text :comment comment :pi instruction
          :doctype doctype :element element :document document)))

(describe "readers"
  (it-each ((:position xml-position-source "nodes.xml") (:position xml-position-offset 2)
            (:end-position xml-position-line 3) (:end-position xml-position-column 4)
            (:name xml-name-local-name "root") (:name xml-name-namespace-uri "urn:test")
            (:name xml-name-prefix "p") (:binding xml-namespace-binding-prefix "p")
            (:binding xml-namespace-binding-namespace-uri "urn:test")
            (:attribute xml-attribute-value "1") (:text xml-text-data "text")
            (:comment xml-comment-data "comment") (:pi xml-processing-instruction-target "pi")
            (:pi xml-processing-instruction-data "data") (:doctype xml-doctype-root-name "root")
            (:doctype xml-doctype-public-id "pub") (:doctype xml-doctype-system-id "sys")
            (:doctype xml-doctype-internal-subset "subset"))
      "reads the ~S field through ~S as ~S"
      (key reader expected)
    (expect (funcall reader (getf (sample-nodes) key)) :to-equal expected))

  (it-each ((:document xml-document-position xml-document-end-position)
            (:element xml-element-position xml-element-end-position)
            (:attribute xml-attribute-position xml-attribute-end-position)
            (:text xml-text-position xml-text-end-position)
            (:comment xml-comment-position xml-comment-end-position)
            (:pi xml-processing-instruction-position xml-processing-instruction-end-position)
            (:doctype xml-doctype-position xml-doctype-end-position))
      "reads the start and end positions of the ~S node through ~S and ~S"
      (key start-reader end-reader)
    (let ((node (getf (sample-nodes) key)))
      (expect (list (xml-position-offset (funcall start-reader node))
                    (xml-position-offset (funcall end-reader node)))
              :to-equal '(2 8))))

  (it "reads the names and nested nodes of an element and a document"
    (let* ((nodes (sample-nodes))
           (element (getf nodes :element)))
      (expect (list (xml-name-local-name (xml-element-name element))
                    (xml-name-local-name (xml-attribute-name (getf nodes :attribute)))
                    (mapcar #'xml-namespace-binding-namespace-uri
                            (xml-element-namespace-declarations element)))
              :to-equal '("root" "id" ("urn:test")))
      (expect (mapcar #'type-of (xml-element-children element))
              :to-equal '(xml-text xml-comment xml-processing-instruction))
      (expect (mapcar #'type-of (xml-document-children (getf nodes :document)))
              :to-equal '(xml-doctype xml-element))))

  (it-each (
      (xml-document-p :position nil) (xml-document-p :end-position nil) (xml-document-p :name nil)
      (xml-document-p :binding nil) (xml-document-p :attribute nil) (xml-document-p :text nil)
      (xml-document-p :comment nil) (xml-document-p :pi nil) (xml-document-p :doctype nil)
      (xml-document-p :element nil) (xml-document-p :document t)

      (xml-element-p :position nil) (xml-element-p :end-position nil) (xml-element-p :name nil)
      (xml-element-p :binding nil) (xml-element-p :attribute nil) (xml-element-p :text nil)
      (xml-element-p :comment nil) (xml-element-p :pi nil) (xml-element-p :doctype nil)
      (xml-element-p :element t) (xml-element-p :document nil)

      (xml-attribute-p :position nil) (xml-attribute-p :end-position nil) (xml-attribute-p :name nil)
      (xml-attribute-p :binding nil) (xml-attribute-p :attribute t) (xml-attribute-p :text nil)
      (xml-attribute-p :comment nil) (xml-attribute-p :pi nil) (xml-attribute-p :doctype nil)
      (xml-attribute-p :element nil) (xml-attribute-p :document nil)

      (xml-text-p :position nil) (xml-text-p :end-position nil) (xml-text-p :name nil)
      (xml-text-p :binding nil) (xml-text-p :attribute nil) (xml-text-p :text t)
      (xml-text-p :comment nil) (xml-text-p :pi nil) (xml-text-p :doctype nil) (xml-text-p :element nil)
      (xml-text-p :document nil)

      (xml-comment-p :position nil) (xml-comment-p :end-position nil) (xml-comment-p :name nil)
      (xml-comment-p :binding nil) (xml-comment-p :attribute nil) (xml-comment-p :text nil)
      (xml-comment-p :comment t) (xml-comment-p :pi nil) (xml-comment-p :doctype nil)
      (xml-comment-p :element nil) (xml-comment-p :document nil)

      (xml-processing-instruction-p :position nil) (xml-processing-instruction-p :end-position nil)
      (xml-processing-instruction-p :name nil) (xml-processing-instruction-p :binding nil)
      (xml-processing-instruction-p :attribute nil) (xml-processing-instruction-p :text nil)
      (xml-processing-instruction-p :comment nil) (xml-processing-instruction-p :pi t)
      (xml-processing-instruction-p :doctype nil) (xml-processing-instruction-p :element nil)
      (xml-processing-instruction-p :document nil)

      (xml-doctype-p :position nil) (xml-doctype-p :end-position nil) (xml-doctype-p :name nil)
      (xml-doctype-p :binding nil) (xml-doctype-p :attribute nil) (xml-doctype-p :text nil)
      (xml-doctype-p :comment nil) (xml-doctype-p :pi nil) (xml-doctype-p :doctype t)
      (xml-doctype-p :element nil) (xml-doctype-p :document nil)

      (xml-namespace-binding-p :position nil) (xml-namespace-binding-p :end-position nil)
      (xml-namespace-binding-p :name nil) (xml-namespace-binding-p :binding t)
      (xml-namespace-binding-p :attribute nil) (xml-namespace-binding-p :text nil)
      (xml-namespace-binding-p :comment nil) (xml-namespace-binding-p :pi nil)
      (xml-namespace-binding-p :doctype nil) (xml-namespace-binding-p :element nil)
      (xml-namespace-binding-p :document nil)
      )
      "answers ~S for the ~S sample as ~S"
      (predicate key expected)
    (let ((nodes (sample-nodes)))
      (expect (funcall predicate (getf nodes key)) :to-equal expected))))

(describe "defaults"
  (it-each ((make-xml-position () xml-position-source nil)
            (make-xml-position () xml-position-offset 0)
            (make-xml-position () xml-position-line 1)
            (make-xml-position () xml-position-column 1)
            (make-xml-name (:local-name "a") xml-name-namespace-uri "")
            (make-xml-name (:local-name "a") xml-name-prefix nil)
            (make-xml-namespace-binding (:namespace-uri "urn:x") xml-namespace-binding-prefix nil)
            (make-xml-processing-instruction (:target "pi") xml-processing-instruction-data nil)
            (make-xml-doctype (:root-name "r") xml-doctype-public-id nil)
            (make-xml-doctype (:root-name "r") xml-doctype-system-id nil)
            (make-xml-doctype (:root-name "r") xml-doctype-internal-subset nil)
            (make-xml-doctype (:root-name "r") xml-doctype-position nil)
            (make-xml-text (:data "") xml-text-end-position nil)
            (make-xml-document () xml-document-children nil)
            (make-xml-document () xml-document-position nil))
      "leaves ~S with arguments ~S reading ~S as ~S"
      (constructor arguments reader expected)
    (expect (funcall reader (apply constructor arguments)) :to-equal expected))

  (it "leaves an element without attributes, declarations, children, or positions"
    (let ((element (make-xml-element :name (make-xml-name :local-name "r"))))
      (expect (list (xml-element-attributes element)
                    (xml-element-namespace-declarations element)
                    (xml-element-children element) (xml-element-position element)
                    (xml-element-end-position element))
              :to-equal '(nil nil nil nil nil)))))

(describe "snapshots"
  (it-each ((make-xml-position () :source xml-position-source)
            (make-xml-name () :local-name xml-name-local-name)
            (make-xml-name (:local-name "a") :namespace-uri xml-name-namespace-uri)
            (make-xml-name (:local-name "a") :prefix xml-name-prefix)
            (make-xml-namespace-binding (:namespace-uri "urn:x") :prefix
             xml-namespace-binding-prefix)
            (make-xml-namespace-binding () :namespace-uri xml-namespace-binding-namespace-uri)
            (make-xml-text () :data xml-text-data)
            (make-xml-comment () :data xml-comment-data)
            (make-xml-processing-instruction () :target xml-processing-instruction-target)
            (make-xml-processing-instruction (:target "pi") :data
             xml-processing-instruction-data)
            (make-xml-doctype () :root-name xml-doctype-root-name)
            (make-xml-doctype (:root-name "r") :public-id xml-doctype-public-id)
            (make-xml-doctype (:root-name "r") :system-id xml-doctype-system-id)
            (make-xml-doctype (:root-name "r") :internal-subset xml-doctype-internal-subset))
      "keeps its own copy of the string passed to ~S~* as ~S"
      (constructor arguments key reader)
    (let* ((value (copy-seq "value"))
           (node (apply constructor key value arguments)))
      (setf (char value 0) #\X)
      (expect (funcall reader node) :to-equal "value")))

  (it "keeps its own copy of an attribute value changed later"
    (let* ((value (copy-seq "value"))
           (attribute (make-xml-attribute :name (make-xml-name :local-name "id")
                                          :value value)))
      (setf (char value 0) #\X)
      (expect (xml-attribute-value attribute) :to-equal "value")))

  (it-each ((:attributes xml-element-attributes)
            (:namespace-declarations xml-element-namespace-declarations)
            (:children xml-element-children))
      "isolates the element ~S list from the caller's list and from earlier reads"
      (key reader)
    (let* ((items (list :first :second))
           (element (make-xml-element :name (make-xml-name :local-name "r") key items)))
      (setf (cdr items) nil)
      (expect (funcall reader element) :to-equal '(:first :second))
      (setf (cdr (funcall reader element)) nil)
      (expect (funcall reader element) :to-equal '(:first :second))))

  (it "isolates the document children from the caller's list and from earlier reads"
    (let* ((children (list :first :second))
           (document (make-xml-document :children children)))
      (setf (cdr children) nil)
      (expect (xml-document-children document) :to-equal '(:first :second))
      (setf (cdr (xml-document-children document)) nil)
      (expect (xml-document-children document) :to-equal '(:first :second)))))

(describe "rejections"
  (it-each (((make-xml-position :offset -1) "OFFSET must be a non-negative integer.")
            ((make-xml-position :offset 1.5) "OFFSET must be a non-negative integer.")
            ((make-xml-position :source 1) "SOURCE must be a string or NIL.")
            ((make-xml-position :line 0) "LINE must be a positive integer.")
            ((make-xml-position :line 1.5) "LINE must be a positive integer.")
            ((make-xml-position :column 0) "COLUMN must be a positive integer.")
            ((make-xml-position :column "1") "COLUMN must be a positive integer.")
            ((make-xml-name :local-name "a:b") "LOCAL-NAME must not contain a colon.")
            ((make-xml-name :local-name nil) "LOCAL-NAME must be a string.")
            ((make-xml-name) "LOCAL-NAME must be a string.")
            ((make-xml-name :local-name "") "LOCAL-NAME must not be empty.")
            ((make-xml-name :local-name "a" :namespace-uri 1) "NAMESPACE-URI must be a string.")
            ((make-xml-name :local-name "a" :prefix 1) "PREFIX must be a string or NIL.")
            ((make-xml-name :local-name "a" :prefix "p:q") "PREFIX must not contain a colon.")
            ((make-xml-namespace-binding :prefix 1 :namespace-uri "urn:x")
             "PREFIX must be a string or NIL.")
            ((make-xml-namespace-binding :prefix "p" :namespace-uri nil)
             "NAMESPACE-URI must be a string.")
            ((make-xml-text :data nil) "DATA must be a string.")
            ((make-xml-comment :data 1) "DATA must be a string.")
            ((make-xml-attribute :name "id" :value "1") "NAME must be an XML-NAME.")
            ((make-xml-element :name "root") "NAME must be an XML-NAME.")
            ((make-xml-document :children :not-a-list) "CHILDREN must be a list.")
            ((make-xml-document :position :start) "A position must be an XML-POSITION value.")
            ((make-xml-text :data "t" :end-position 3)
             "A position must be an XML-POSITION value.")
            ((make-xml-doctype :root-name "") "ROOT-NAME must not be empty.")
            ((make-xml-doctype :root-name "a:b") "ROOT-NAME must not contain a colon.")
            ((make-xml-doctype :root-name "root" :public-id 1) "PUBLIC-ID must be a string or NIL.")
            ((make-xml-doctype :root-name "root" :system-id 1) "SYSTEM-ID must be a string or NIL.")
            ((make-xml-doctype :root-name "root" :internal-subset 1)
             "INTERNAL-SUBSET must be a string or NIL.")
            ((make-xml-processing-instruction :target "pi" :data 1)
             "DATA must be a string or NIL.")
            ((make-xml-processing-instruction :target "") "TARGET must not be empty."))
      "refuses ~S"
      (form message)
    (expect (lambda () (eval form))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message))))

  (it-each (((:value nil) "VALUE must be a string.")
            ((:value "1" :position :start) "A position must be an XML-POSITION value."))
      "refuses an attribute built with ~S"
      (arguments message)
    (expect (lambda () (apply #'make-xml-attribute :name (make-xml-name :local-name "id")
                              arguments))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message))))

  (it-each ((:attributes :x "ATTRIBUTES must be a list.")
            (:namespace-declarations :x "NAMESPACE-DECLARATIONS must be a list.")
            (:children :x "CHILDREN must be a list.")
            (:end-position 3 "A position must be an XML-POSITION value."))
      "refuses an element with a valid name and ~S set to ~S"
      (key value message)
    (expect (lambda () (make-xml-element :name (make-xml-name :local-name "root") key value))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message))))

  (it "refuses a prefixed namespace binding to the empty URI as a namespace error"
    (expect (lambda () (make-xml-namespace-binding :prefix "p" :namespace-uri ""))
            :to-signal-xml '(xml-namespace-error
                             (xml-namespace-error-prefix "p")
                             (xml-namespace-error-namespace-uri "")
                             (xml-condition-message
                              "A prefixed binding cannot use an empty namespace URI.")))))
