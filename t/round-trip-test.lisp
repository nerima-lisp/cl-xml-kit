(in-package #:cl-xml-kit/test)

(defun round-trip-attribute-signature (attribute)
  (let ((name (xml-attribute-name attribute)))
    (list (xml-name-namespace-uri name)
          (xml-name-local-name name)
          (xml-attribute-value attribute))))

(defun round-trip-signature (object &key (include-comments t))
  (cond
    ((xml-document-p object)
     (list :document
           (loop for child in (xml-document-children object)
                 for signature = (round-trip-signature
                                  child :include-comments include-comments)
                 unless (null signature)
                   collect signature)))
    ((xml-element-p object)
     (let ((name (xml-element-name object)))
       (list :element (xml-name-namespace-uri name) (xml-name-local-name name)
             (sort (mapcar #'round-trip-attribute-signature
                           (xml-element-attributes object))
                   #'string< :key (lambda (attribute)
                                    (format nil "~A~%~A" (first attribute)
                                            (second attribute))))
             (loop for child in (xml-element-children object)
                   for signature = (round-trip-signature
                                    child :include-comments include-comments)
                   unless (null signature)
                     collect signature))))
    ((xml-text-p object)
     (list :text (xml-text-data object)))
    ((xml-comment-p object)
     (when include-comments
       (list :comment (xml-comment-data object))))
    ((xml-processing-instruction-p object)
     (list :pi (xml-processing-instruction-target object)
           (or (xml-processing-instruction-data object) "")))
    ((xml-doctype-p object)
     (list :doctype (xml-doctype-root-name object)))
    (t (list :unknown))))

(defun round-trip-parse-serialize-parse (input &key (canonical nil))
  (let* ((before (parse-xml input))
         (serialized (if canonical
                         (xml->canonical-string before)
                         (xml->string before)))
         (after (parse-xml serialized)))
    (values before serialized after)))

(defun round-trip-generated-step (state)
  (setf (car state)
        (mod (+ (* 1103515245 (car state)) 12345) 2147483648))
  (car state))

(defun round-trip-generated-element (state depth)
  (let* ((name (format nil "n~C"
                       (code-char (+ (char-code #\A)
                                     (mod (round-trip-generated-step state)
                                          17)))))
         (value (nth (mod (round-trip-generated-step state) 4)
                     (list "a&<>'\"" "x]]>y"
                           (format nil "line~Cbreak~Creturn~C"
                                   #\Tab #\Newline #\Space)
                           "plain")))
         (attribute-value (nth (mod (round-trip-generated-step state) 3)
                               (list "a&>\"'"
                                     (format nil "line~Cbreak~Creturn~C"
                                             #\Tab #\Newline #\Return)
                                     "plain")))
         (children (if (zerop depth)
                       (list (make-xml-text :data value))
                       (list (make-xml-text :data value)
                             (round-trip-generated-element state (1- depth))
                             (make-xml-text :data "tail")))))
    (make-xml-element
     :name (make-xml-name :local-name name)
     :attributes (list (make-xml-attribute
                        :name (make-xml-name :local-name "value")
                        :value attribute-value))
     :children children)))

(it "round-trips escaped text, attributes, character references, and supplementary characters"
  (let ((input
          "<root attr=\"&gt;&amp;&quot;&apos;&#9;&#10;&#13;\">&lt;&gt;&amp;&quot;&apos; &#x1F600;</root>"))
    (multiple-value-bind (before serialized after)
        (round-trip-parse-serialize-parse input)
      (expect (round-trip-signature after)
              :to-equal (round-trip-signature before))
      (expect serialized :to-contain "&amp;")
      (expect serialized :to-contain "&#9;")
      (expect serialized :to-contain "&#10;")
      (expect serialized :to-contain "&#13;")
      (expect (round-trip-signature
               (parse-xml (xml->canonical-string before)))
               :to-equal (round-trip-signature before)))))

(it "round-trips default, prefixed, redeclared, and undeclared namespaces"
  (let ((input
          "<root xmlns=\"urn:default\" xmlns:p=\"urn:p1\" xmlns:a=\"urn:a\" a:id=\"root\"><p:child p:code=\"one\" xmlns:p=\"urn:p2\"><plain xmlns=\"\" a:kind=\"plain\"><p:leaf xmlns:p=\"urn:p3\"/></plain></p:child></root>"))
    (multiple-value-bind (before ordinary ordinary-after)
        (round-trip-parse-serialize-parse input)
      (declare (ignore ordinary))
      (expect (round-trip-signature ordinary-after)
              :to-equal (round-trip-signature before)))
    (multiple-value-bind (before canonical canonical-after)
        (round-trip-parse-serialize-parse input :canonical t)
      (declare (ignore canonical))
      (expect (round-trip-signature canonical-after :include-comments nil)
              :to-equal (round-trip-signature before :include-comments nil)))))

(it "generates a prefix for an unprefixed namespaced attribute and round-trips it"
  (let* ((document
           (make-xml-document
            :children
            (list
             (make-xml-element
              :name (make-xml-name :local-name "root")
              :attributes
              (list (make-xml-attribute
                     :name (make-xml-name :local-name "id"
                                          :namespace-uri "urn:attribute")
                     :value "7"))
              :children (list (make-xml-text :data "value"))))))
         (ordinary (xml->string document))
         (canonical (xml->canonical-string document)))
    (expect ordinary :to-contain "xmlns:nsA=\"urn:attribute\"")
    (expect ordinary :to-contain "nsA:id=\"7\"")
    (expect (round-trip-signature (parse-xml ordinary))
            :to-equal (round-trip-signature document))
    (expect (round-trip-signature (parse-xml canonical)
                                  :include-comments nil)
            :to-equal (round-trip-signature document :include-comments nil))))

(it "round-trips empty elements, PI, comments, and CDATA boundaries"
  (let ((input
          "<?before?><root><empty/><!--boundary--><![CDATA[cdata]]>text]]&gt;<?inside data?><!--tail--></root><?after data?>"))
    (multiple-value-bind (before ordinary ordinary-after)
        (round-trip-parse-serialize-parse input)
      (declare (ignore ordinary))
      (expect (round-trip-signature ordinary-after)
              :to-equal (round-trip-signature before)))
    (multiple-value-bind (before canonical canonical-after)
        (round-trip-parse-serialize-parse input :canonical t)
      (declare (ignore canonical))
      (expect (round-trip-signature canonical-after :include-comments nil)
              :to-equal (round-trip-signature before :include-comments nil)))))

(it "escapes a text ]]> boundary and rejects invalid comment data"
  (let* ((text (make-xml-text :data "left]]>right"))
         (text-document
           (make-xml-document
            :children (list (make-xml-element
                             :name (make-xml-name :local-name "root")
                             :children (list text)))))
         (comment-document
           (make-xml-document
            :children (list (make-xml-comment :data "not--valid")))))
    (let ((serialized (xml->string text-document)))
      (expect serialized :to-equal "<root>left]]&gt;right</root>")
      (expect (xml-text-data
               (first (xml-element-children
                       (first (xml-document-children
                               (parse-xml serialized))))))
              :to-equal "left]]>right"))
    (let ((condition nil))
      (handler-case (xml->string comment-document)
        (xml-serialization-error (caught)
          (setf condition caught)))
      (expect (typep condition 'xml-serialization-error) :to-be-truthy))))

(it "round-trips fixed-seed generated trees in ordinary and canonical forms"
  (let ((state (list 424242)))
    (loop repeat 8
      do
      (let* ((element (round-trip-generated-element state 2))
             (document (make-xml-document :children (list element)))
             (ordinary (parse-xml (xml->string document)))
             (canonical (parse-xml (xml->canonical-string document))))
        (expect (round-trip-signature ordinary)
                :to-equal (round-trip-signature document))
        (expect (round-trip-signature canonical :include-comments nil)
                :to-equal (round-trip-signature document :include-comments nil))))))

(it "round-trips a less-than character in an attribute value"
  (let* ((document
           (make-xml-document
            :children
            (list (make-xml-element
                   :name (make-xml-name :local-name "root")
                   :attributes (list (make-xml-attribute
                                      :name (make-xml-name :local-name "v")
                                      :value "a<b&c"))))))
         (serialized (xml->string document)))
    (expect serialized :to-contain "&lt;")
    (expect (xml-attribute-value
             (first (xml-element-attributes
                     (first (xml-document-children (parse-xml serialized))))))
            :to-equal "a<b&c")))

(defun round-trip-whitespace-strings ()
  "Return every string of one to three characters over a, space, tab, line feed, and carriage return."
  (let ((alphabet (list #\a #\Space #\Tab #\Newline #\Return))
        (strings (list "")))
    (loop repeat 3
          do (setf strings
                   (loop for prefix in strings
                         nconc (loop for character in alphabet
                                     collect (concatenate 'string prefix
                                                          (string character)))))
          append strings)))

(it "round-trips carriage returns, line feeds, and tabs in text and attribute values"
  (expect
   (loop for data in (round-trip-whitespace-strings)
         for element = (make-xml-element
                        :name (make-xml-name :local-name "root")
                        :attributes (list (make-xml-attribute
                                           :name (make-xml-name :local-name "value")
                                           :value data))
                        :children (list (make-xml-text :data data)))
         for root = (first (xml-document-children (parse-xml (xml->string element))))
         unless (and (equal (xml-attribute-value (first (xml-element-attributes root)))
                            data)
                     (equal (apply #'concatenate 'string
                                   (mapcar #'xml-text-data (xml-element-children root)))
                            data))
           collect data)
   :to-equal nil))
