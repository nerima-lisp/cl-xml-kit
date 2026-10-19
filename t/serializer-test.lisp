;;;; t/serializer-test.lisp

(in-package #:cl-xml-kit/test)

(describe "writer APIs"
  (it-each (((:attribute "id" "1") "id=\"1\"")
            ("text" "text"))
      "writes ~S to the supplied stream as ~S and returns that stream"
      (spec expected)
    (let ((stream (make-string-output-stream)))
      (expect (write-xml (node-from-spec spec) stream) :to-be stream)
      (expect (get-output-stream-string stream) :to-equal expected)))

  (it "writes canonical output to the supplied stream and returns that stream"
    (let ((stream (make-string-output-stream)))
      (expect (write-xml-canonical (node-from-spec '(:element "a" :children ((:comment "c"))))
                                   stream)
              :to-be stream)
      (expect (get-output-stream-string stream) :to-equal "<a></a>")))

  (it "pretty prints through write-xml with the requested indent"
    (let ((stream (make-string-output-stream)))
      (write-xml (node-from-spec '(:element "a" :children ((:element "b")))) stream
                 :pretty-print t :indent 1)
      (expect (get-output-stream-string stream) :to-equal (format nil "<a>~% <b/>~%</a>"))))
  (it "uses two-space indentation by default through public APIs"
    (let* ((node (node-from-spec
                  (list :element "root"
                        :children (list (list :element "child")))))
           (expected (format nil "<root>~%  <child/>~%</root>"))
           (stream (make-string-output-stream)))
      (write-xml node stream :pretty-print t)
      (expect (get-output-stream-string stream) :to-equal expected)
      (expect (xml->string node :pretty-print t) :to-equal expected)))

  (it-each (((:object 1) () "OBJECT is not a serializable XML node." 1)
            ((:object "text") () "OBJECT is not a serializable XML node." "text")
            ((:element "root") (:indent -1) "INDENT must be a non-negative integer." -1)
            ((:element "root") (:indent 2.5) "INDENT must be a non-negative integer." 2.5)
            ((:element "root") (:resource-limits nil)
             "RESOURCE-LIMITS must be XML resource limits." nil))
      "refuses to write ~S with options ~S"
      (spec options message object)
    (expect (lambda () (apply #'xml->string (node-from-spec spec) options))
            :to-signal-xml `(xml-serialization-error
                             (xml-condition-message ,message)
                             (xml-serialization-error-object ,object)
                             (xml-serialization-error-path nil))))

  (it "refuses a stream argument that is not a stream"
    (expect (lambda () (write-xml (node-from-spec '(:element "root")) nil))
            :to-signal-xml '(xml-serialization-error
                             (xml-condition-message "STREAM must be a character stream.")
                             (xml-serialization-error-object nil))))

  (describe "internal serializer defaults"
    (it "initializes both state counters to zero"
      (let ((state (cl-xml-kit::%make-xml-serializer-state
                    (make-string-output-stream)
                    *default-xml-resource-limits* nil 2 nil)))
        (expect (cl-xml-kit::xml-serializer-state-output-count state)
                :to-equal 0)
        (expect (cl-xml-kit::xml-serializer-state-generated-prefix-index state)
                :to-equal 0)))

    (it "writes compact noncanonical output by default through %write-xml"
      (let ((stream (make-string-output-stream)))
        (cl-xml-kit::%write-xml
         (node-from-spec '(:element "root"
                           :children ((:comment "note") (:element "child"))))
         stream :resource-limits *default-xml-resource-limits*)
        (expect (get-output-stream-string stream)
                :to-equal "<root><!--note--><child/></root>")))

    (it "uses two-space indentation by default when %write-xml pretty-prints"
      (let ((stream (make-string-output-stream)))
        (cl-xml-kit::%write-xml
         (node-from-spec '(:element "root" :children ((:element "child"))))
         stream :pretty-print t :resource-limits *default-xml-resource-limits*)
        (expect (get-output-stream-string stream)
                :to-equal (format nil "<root>~%  <child/>~%</root>"))))))
