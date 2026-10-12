(in-package #:cl-xml-kit/test)

(defparameter +expected-export-names+
  '("PARSE-XML" "MAKE-XML-PARSER" "NEXT-XML-EVENT" "XML-PARSER-EOF-P"
    "XML-EVENT" "MAKE-XML-EVENT" "XML-EVENT-P" "XML-EVENT-TYPE"
    "XML-EVENT-NAME" "XML-EVENT-ATTRIBUTES" "XML-EVENT-TEXT"
    "XML-EVENT-TARGET" "XML-EVENT-POSITION" "XML-EVENT-EXTERNAL-P"
    "*DEFAULT-XML-RESOURCE-LIMITS*" "MAKE-XML-RESOURCE-LIMITS"
    "XML-RESOURCE-LIMITS-P" "XML-RESOURCE-LIMITS-MAX-INPUT-OCTETS"
    "XML-RESOURCE-LIMITS-MAX-DOCUMENT-CHARACTERS"
    "XML-RESOURCE-LIMITS-MAX-OUTPUT-CHARACTERS" "XML-RESOURCE-LIMITS-MAX-DEPTH"
    "XML-RESOURCE-LIMITS-MAX-ELEMENTS"
    "XML-RESOURCE-LIMITS-MAX-ATTRIBUTES-PER-ELEMENT"
    "XML-RESOURCE-LIMITS-MAX-NAME-LENGTH"
    "XML-RESOURCE-LIMITS-MAX-TEXT-NODE-LENGTH"
    "XML-RESOURCE-LIMITS-MAX-NAMESPACE-BINDINGS"
    "XML-RESOURCE-LIMITS-MAX-ENTITY-EXPANSIONS"
    "XML-RESOURCE-LIMITS-MAX-EXPANDED-CHARACTERS"
    "XML-RESOURCE-LIMITS-MAX-ENTITY-DEPTH" "XML-POSITION" "MAKE-XML-POSITION"
    "XML-POSITION-SOURCE" "XML-POSITION-OFFSET" "XML-POSITION-LINE"
    "XML-POSITION-COLUMN" "XML-NAME" "MAKE-XML-NAME" "XML-NAME-LOCAL-NAME"
    "XML-NAME-NAMESPACE-URI" "XML-NAME-PREFIX" "XML-NAMESPACE-BINDING"
    "MAKE-XML-NAMESPACE-BINDING" "XML-NAMESPACE-BINDING-P"
    "XML-NAMESPACE-BINDING-PREFIX" "XML-NAMESPACE-BINDING-NAMESPACE-URI"
    "XML-DOCUMENT" "MAKE-XML-DOCUMENT" "XML-DOCUMENT-CHILDREN"
    "XML-DOCUMENT-POSITION" "XML-DOCUMENT-END-POSITION" "XML-ELEMENT"
    "MAKE-XML-ELEMENT" "XML-ELEMENT-NAME" "XML-ELEMENT-ATTRIBUTES"
    "XML-ELEMENT-NAMESPACE-DECLARATIONS" "XML-ELEMENT-CHILDREN"
    "XML-ELEMENT-POSITION" "XML-ELEMENT-END-POSITION" "XML-ATTRIBUTE"
    "MAKE-XML-ATTRIBUTE" "XML-ATTRIBUTE-NAME" "XML-ATTRIBUTE-VALUE"
    "XML-ATTRIBUTE-POSITION" "XML-ATTRIBUTE-END-POSITION" "XML-TEXT"
    "MAKE-XML-TEXT" "XML-TEXT-DATA" "XML-TEXT-POSITION"
    "XML-TEXT-END-POSITION" "XML-COMMENT" "MAKE-XML-COMMENT"
    "XML-COMMENT-DATA" "XML-COMMENT-POSITION" "XML-COMMENT-END-POSITION"
    "XML-PROCESSING-INSTRUCTION" "MAKE-XML-PROCESSING-INSTRUCTION"
    "XML-PROCESSING-INSTRUCTION-TARGET" "XML-PROCESSING-INSTRUCTION-DATA"
    "XML-PROCESSING-INSTRUCTION-POSITION"
    "XML-PROCESSING-INSTRUCTION-END-POSITION" "XML-DOCTYPE"
    "MAKE-XML-DOCTYPE" "XML-DOCTYPE-ROOT-NAME" "XML-DOCTYPE-PUBLIC-ID"
    "XML-DOCTYPE-SYSTEM-ID" "XML-DOCTYPE-INTERNAL-SUBSET"
    "XML-DOCTYPE-POSITION" "XML-DOCTYPE-END-POSITION" "XML-DOCUMENT-P"
    "XML-ELEMENT-P" "XML-ATTRIBUTE-P" "XML-TEXT-P" "XML-COMMENT-P"
    "XML-PROCESSING-INSTRUCTION-P" "XML-DOCTYPE-P" "XML->STRING"
    "XML->CANONICAL-STRING" "WRITE-XML" "WRITE-XML-CANONICAL" "XML-KIT-ERROR"
    "XML-INPUT-ERROR" "XML-DECODE-ERROR" "XML-UNSUPPORTED-ENCODING"
    "XML-PARSE-ERROR" "XML-WELL-FORMEDNESS-ERROR" "XML-NAMESPACE-ERROR"
    "XML-EXTERNAL-RESOURCE-DISABLED" "XML-PARSER-STATE-ERROR"
    "XML-RESOURCE-LIMIT-EXCEEDED" "XML-INVALID-RESOURCE-LIMIT"
    "XML-SERIALIZATION-ERROR" "XML-CONDITION-MESSAGE" "XML-CONDITION-POSITION"
    "XML-CONDITION-SOURCE" "XML-DECODE-ERROR-OCTET" "XML-DECODE-ERROR-ENCODING"
    "XML-UNSUPPORTED-ENCODING-ENCODING" "XML-UNSUPPORTED-ENCODING-REASON"
    "XML-WELL-FORMEDNESS-ERROR-CONSTRAINT"
    "XML-WELL-FORMEDNESS-ERROR-EXPECTED" "XML-NAMESPACE-ERROR-PREFIX"
    "XML-NAMESPACE-ERROR-NAMESPACE-URI"
    "XML-EXTERNAL-RESOURCE-DISABLED-IDENTIFIER"
    "XML-EXTERNAL-RESOURCE-DISABLED-KIND" "XML-PARSER-STATE-ERROR-PARSER"
    "XML-RESOURCE-LIMIT-EXCEEDED-KIND" "XML-RESOURCE-LIMIT-EXCEEDED-VALUE"
    "XML-RESOURCE-LIMIT-EXCEEDED-LIMIT" "XML-INVALID-RESOURCE-LIMIT-KIND"
    "XML-INVALID-RESOURCE-LIMIT-VALUE" "XML-SERIALIZATION-ERROR-OBJECT"
    "XML-SERIALIZATION-ERROR-PATH" "XML-EDIT-ERROR" "XML-PATH-ERROR"
    "XML-UNSUPPORTED-EDIT" "XML-EDIT-VALUE-ERROR" "XML-EDIT-ERROR-PATH"
    "XML-EDIT-ERROR-REASON" "XML-PATH-ERROR-INDEX" "XML-EDIT-VALUE-ERROR-CAUSE"
    "SELECT-XML" "XML-MATCH" "XML-MATCH-P" "XML-MATCH-KIND" "XML-MATCH-PATH"
    "XML-MATCH-NAME" "XML-MATCH-VALUE" "XML-MATCH-START" "XML-MATCH-END"
    "XML-MATCH-ELEMENT" "EDIT-XML-SET" "EDIT-XML-DELETE" "EDIT-XML-APPEND"))

(defun external-names (package)
  "Return the sorted names exported by PACKAGE."
  (sort (loop for symbol being the external-symbols of package
              collect (symbol-name symbol)) #'string<))

(it "exports exactly the architecture public API"
  (expect (external-names (find-package "CL-XML-KIT"))
          :to-equal (sort (copy-list +expected-export-names+) #'string<)))

(it "keeps only applicable fields on each event type"
  (let* ((name (make-xml-name :local-name "root"))
         (attribute (make-xml-attribute
                     :name (make-xml-name :local-name "id") :value "1"))
         (event (make-xml-event :type :start-element :name name
                                :attributes (list attribute) :text "ignored"
                                :target "ignored" :external-p t)))
    (expect (xml-event-text event) :to-equal nil)
    (expect (xml-event-target event) :to-equal nil)
    (expect (xml-event-external-p event) :to-equal nil)))

(it "constructs each event variant with only its applicable fields"
  (let* ((position (make-xml-position :source "events.xml" :offset 4))
         (name (make-xml-name :local-name "root"))
         (attribute (make-xml-attribute
                     :name (make-xml-name :local-name "id") :value "1"))
         (events (list
                  (make-xml-event :type :start-document :position position)
                  (make-xml-event :type :doctype :text "root" :position position)
                  (make-xml-event :type :start-element :name name
                                  :attributes (list attribute) :position position)
                  (make-xml-event :type :characters :text "text" :position position)
                  (make-xml-event :type :comment :text "comment" :position position)
                  (make-xml-event :type :processing-instruction :target "pi"
                                  :text "data" :position position)
                  (make-xml-event :type :end-element :name name :position position)
                  (make-xml-event :type :skipped-external-entity :name name
                                  :position position)
                  (make-xml-event :type :end-document :position position))))
    (expect (mapcar #'xml-event-type events)
            :to-equal '(:start-document :doctype :start-element :characters
                        :comment :processing-instruction :end-element
                        :skipped-external-entity :end-document))
    (expect (list (xml-event-text (second events))
                  (xml-event-name (third events))
                  (xml-event-attributes (third events))
                  (xml-event-target (sixth events))
                  (xml-event-position (first events))
                  (xml-event-external-p (eighth events)))
            :to-be-truthy)))

(it "rejects malformed event payloads"
  (dolist (form '((:type :unknown)
                  (:type :characters :text 1)
                  (:type :processing-instruction :target 1)
                  (:type :start-document :position :not-a-position)
                  (:type :start-element :name "root")
                  (:type :start-element :attributes :not-a-list)
                  (:type :start-element :attributes (list :not-an-attribute))))
    (expect (handler-case (progn (apply #'make-xml-event form) nil)
              (xml-input-error () t))
            :to-be-truthy)))

(it "snapshots event attribute lists"
  (let* ((attribute (make-xml-attribute
                     :name (make-xml-name :local-name "id") :value "1"))
         (event (make-xml-event :type :start-element
                                :name (make-xml-name :local-name "root")
                                :attributes (list attribute)))
         (attributes (xml-event-attributes event)))
    (setf (cdr attributes) nil)
    (expect (length (xml-event-attributes event)) :to-equal 1)))

(it "copies public strings and rejects prefixed empty bindings"
  (let* ((local-name (copy-seq "root"))
         (uri (copy-seq "urn:test"))
         (name (make-xml-name :local-name local-name :namespace-uri uri)))
    (setf (char local-name 0) #\X (char uri 0) #\X)
    (expect (xml-name-local-name name) :to-equal "root")
    (expect (xml-name-namespace-uri name) :to-equal "urn:test")
    (expect (handler-case (progn (make-xml-namespace-binding
                                  :prefix "p" :namespace-uri "") nil)
                 (xml-namespace-error () t))
            :to-equal t)))

