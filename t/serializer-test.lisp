(in-package #:cl-xml-kit/test)

(defun serializer-name (local-name &key (namespace-uri "") prefix)
  (make-xml-name :local-name local-name :namespace-uri namespace-uri
                 :prefix prefix))

(defun serializer-element (local-name &key namespace-uri prefix attributes
                                         declarations children)
  (make-xml-element
   :name (serializer-name local-name :namespace-uri (or namespace-uri "")
                          :prefix prefix)
   :attributes attributes :namespace-declarations declarations
   :children children))

(it "escapes text and attributes without changing ordinary content"
  (let* ((attribute (make-xml-attribute
                     :name (serializer-name "value")
                     :value "a\"&<>") )
         (element (serializer-element
                   "root" :attributes (list attribute)
                   :children (list (make-xml-text :data "a&<]]>\"")))))
    (expect (xml->string element)
            :to-equal "<root value=\"a&quot;&amp;&lt;&gt;\">a&amp;&lt;]]&gt;\"</root>")))

(it "uses Second Canonical Form for attributes and empty elements"
  (let ((element
          (serializer-element
           "root"
           :attributes (list (make-xml-attribute
                              :name (serializer-name "z") :value "&>")
                             (make-xml-attribute
                              :name (serializer-name "a") :value "1")))))
    (expect (xml->canonical-string element)
            :to-equal "<root a=\"1\" z=\"&amp;&gt;\"></root>")))

(it "emits required namespace declarations and suppresses redundant ones"
  (let* ((binding (make-xml-namespace-binding
                   :prefix "p" :namespace-uri "urn:test"))
         (child (serializer-element
                 "child" :namespace-uri "urn:test" :prefix "p"
                 :declarations (list binding)))
         (root (serializer-element
                "root" :namespace-uri "urn:test" :prefix "p"
                :declarations (list binding) :children (list child))))
    (expect (xml->string root)
            :to-equal "<p:root xmlns:p=\"urn:test\"><p:child/></p:root>")))

(it "generates a prefix for a namespaced unprefixed attribute"
  (let ((element
          (serializer-element
           "root"
           :attributes (list (make-xml-attribute
                              :name (serializer-name "id"
                                                      :namespace-uri "urn:attr")
                              :value "1")))))
    (expect (xml->string element)
            :to-equal "<root xmlns:nsA=\"urn:attr\" nsA:id=\"1\"/>")))

(it "declares a generated prefix for each further namespaced unprefixed attribute"
  (let ((element
          (serializer-element
           "root"
           :attributes (list (make-xml-attribute
                              :name (serializer-name "a" :namespace-uri "urn:x")
                              :value "1")
                             (make-xml-attribute
                              :name (serializer-name "b" :namespace-uri "urn:y")
                              :value "2")))))
    (expect (xml->string element)
            :to-equal
            "<root xmlns:nsA=\"urn:x\" xmlns:nsB=\"urn:y\" nsA:a=\"1\" nsB:b=\"2\"/>")))

(it "rejects an element name whose prefix the element declares for another URI"
  (let ((element (serializer-element
                  "root" :namespace-uri "urn:a" :prefix "p"
                  :declarations (list (make-xml-namespace-binding
                                       :prefix "p" :namespace-uri "urn:b")))))
    (expect (handler-case (progn (xml->string element) :returned)
              (xml-serialization-error (condition)
                (xml-condition-message condition)))
            :to-equal "A namespace declaration conflicts with a node name.")))

(it "undeclares a default namespace when a child has no namespace"
  (let* ((child (serializer-element "child"))
         (root (serializer-element "root" :namespace-uri "urn:root"
                                   :children (list child))))
    (expect (xml->string root)
            :to-equal "<root xmlns=\"urn:root\"><child xmlns=\"\"/></root>")))

(it "signals serialization and output-limit conditions"
  (let* ((invalid (serializer-element
                   "root" :children (list (make-xml-text :data (string (code-char 1))))))
         (invalid-condition nil)
         (limited-condition nil))
    (handler-case (xml->string invalid)
      (xml-serialization-error (condition)
        (setf invalid-condition condition)))
    (handler-case
        (xml->string (serializer-element "root")
                     :resource-limits
                     (make-xml-resource-limits :max-output-characters 3))
      (xml-resource-limit-exceeded (condition)
        (setf limited-condition condition)))
    (expect (typep invalid-condition 'xml-serialization-error) :to-be-truthy)
    (expect (typep limited-condition 'xml-resource-limit-exceeded) :to-be-truthy)))

(it "pretty prints element-only content but preserves mixed content"
  (let* ((element-only (serializer-element
                        "root" :children (list (serializer-element "child"))))
         (mixed (serializer-element
                 "root" :children (list (make-xml-text :data "x")
                                         (serializer-element "child")))))
    (expect (xml->string element-only :pretty-print t :indent 2)
            :to-equal "<root>\n  <child/>\n</root>")
    (expect (xml->string mixed :pretty-print t :indent 2)
            :to-equal "<root>x<child/></root>")))

(it "omits comments and normalizes no-data PI in canonical output"
  (let ((document (make-xml-document
                   :children (list (make-xml-comment :data "ignored")
                                   (make-xml-processing-instruction
                                    :target "pi")))))
    (expect (xml->canonical-string document) :to-equal "<?pi ?>")))

(it "omits document-level whitespace in canonical output"
  (let ((document (parse-xml (format nil "~%  <root/>~%"))))
    (expect (xml->canonical-string document) :to-equal "<root></root>")))

(it "serializes doctypes and filters canonical internal subsets"
  (expect (xml->string (make-xml-doctype :root-name "root"
                                         :public-id "public"
                                         :system-id "system"))
          :to-equal "<!DOCTYPE root PUBLIC \"public\" \"system\">")
  (expect (xml->string (make-xml-doctype :root-name "root"
                                         :system-id "system"))
          :to-equal "<!DOCTYPE root SYSTEM \"system\">")
  (expect (xml->string (make-xml-doctype
                        :root-name "root"
                        :internal-subset "<!ELEMENT root ANY>"))
          :to-equal "<!DOCTYPE root [<!ELEMENT root ANY>]>")
  (expect (xml->canonical-string
           (make-xml-doctype
            :root-name "root"
            :internal-subset
            (format nil "<!ELEMENT root ANY>~%<!NOTATION note SYSTEM \"note\">")))
          :to-equal
          (format nil "<!DOCTYPE root [~%<!NOTATION note SYSTEM \"note\">~%]>"))
  (expect (xml->canonical-string
           (make-xml-doctype :root-name "root"
                             :internal-subset "<!ELEMENT root ANY>"))
          :to-equal ""))

(it "writes every node kind and returns the supplied stream"
  (let* ((attribute (make-xml-attribute
                     :name (serializer-name "id") :value "1"))
         (element (serializer-element
                   "root"
                   :attributes (list attribute)
                   :children (list (make-xml-text :data "text")
                                   (make-xml-comment :data "comment")
                                   (make-xml-processing-instruction
                                    :target "pi" :data "data"))))
         (document (make-xml-document
                    :children (list (make-xml-doctype :root-name "root")
                                    element)))
         (stream (make-two-way-stream
                  (make-string-input-stream "")
                  (make-string-output-stream))))
    (unwind-protect
         (progn
           (expect (eq (write-xml attribute stream) stream) :to-be-truthy)
           (expect (eq (write-xml (make-xml-text :data "text") stream)
                       stream)
                   :to-be-truthy)
           (expect (xml->string document)
                   :to-equal "<!DOCTYPE root><root id=\"1\">text<!--comment--><?pi data?></root>"))
      (close stream))))

(it "rejects invalid serializer inputs and node content"
  (dolist (case (list
                 (list 1)
                 (list (make-xml-document :children (list "not-a-node")))
                 (list (serializer-element "root"
                                            :attributes
                                            (list (make-xml-attribute
                                                   :name (serializer-name "id"
                                                                           :prefix "p")
                                                   :value "1"))))
                 (list (make-xml-comment :data "ends-"))
                 (list (make-xml-comment :data "contains--double-hyphen"))
                 (list (make-xml-processing-instruction :target "XML"))
                 (list (make-xml-processing-instruction :target "pi"
                                                         :data "bad?>data"))))
    (let ((condition nil))
      (handler-case (apply #'xml->string case)
        (xml-serialization-error (caught) (setf condition caught)))
      (expect (typep condition 'xml-serialization-error) :to-be-truthy)))
  (let ((condition nil))
    (handler-case (write-xml (serializer-element "root") nil)
      (xml-serialization-error (caught) (setf condition caught)))
    (expect (typep condition 'xml-serialization-error) :to-be-truthy))
  (let ((condition nil))
    (handler-case (xml->string (serializer-element "root") :indent -1)
      (xml-serialization-error (caught) (setf condition caught)))
    (expect (typep condition 'xml-serialization-error) :to-be-truthy))
  (let ((condition nil))
    (handler-case (xml->string (serializer-element "root") :resource-limits nil)
      (xml-serialization-error (caught) (setf condition caught)))
    (expect (typep condition 'xml-serialization-error) :to-be-truthy)))

