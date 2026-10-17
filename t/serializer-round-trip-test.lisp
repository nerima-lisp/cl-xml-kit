;;;; t/serializer-round-trip-test.lisp

(in-package #:cl-xml-kit/test)

(defparameter +round-trip-sources+
  '("<root attr=\"&gt;&amp;&quot;&apos;&#9;&#10;&#13;\">&lt;&gt;&amp;&quot;&apos; &#x1F600;</root>"
    "<root xmlns=\"urn:default\" xmlns:p=\"urn:p1\" xmlns:a=\"urn:a\" a:id=\"root\"><p:child p:code=\"one\" xmlns:p=\"urn:p2\"><plain xmlns=\"\" a:kind=\"plain\"><p:leaf xmlns:p=\"urn:p3\"/></plain></p:child></root>"
    "<?before?><root><empty/><!--boundary--><![CDATA[cdata]]>text]]&gt;<?inside data?><!--tail--></root><?after data?>")
  "Sources holding escapes and references, default, prefixed, redeclared, and
undeclared namespaces, and empty elements, PIs, comments, and CDATA boundaries.")

(it-each ((0 :ordinary) (0 :canonical) (1 :ordinary) (1 :canonical) (2 :ordinary) (2 :canonical))
    "round-trips round-trip source ~D through ~S output"
    (index mode)
  (expect (parse-xml (nth index +round-trip-sources+)) :to-round-trip mode))

(it "writes escaped text, attribute white space references, and supplementary characters"
  (expect (xml->string (parse-xml (first +round-trip-sources+)))
          :to-equal #.(format nil "<root attr=\"&gt;&amp;&quot;'&#9;&#10;&#13;\">&lt;>&amp;\"' ~C</root>"
                              (code-char #x1F600))))

(it-each (("with an unprefixed namespaced attribute"
           (:element "root" :attributes ((:attribute "id" "7" :uri "urn:attribute"))
            :children ("value"))
           :ordinary)
          ("with an unprefixed namespaced attribute"
           (:element "root" :attributes ((:attribute "id" "7" :uri "urn:attribute"))
            :children ("value"))
           :canonical)
          ("with < and & in an attribute value"
           (:element "root" :attributes ((:attribute "v" "a<b&c"))) :ordinary)
          ("with ]]> in text" (:element "root" :children ("left]]>right")) :ordinary))
    "round-trips a tree ~A~* through ~S output"
    (label spec mode)
  (declare (ignore label))
  (expect (node-from-spec spec) :to-round-trip mode))

(it "writes a text ]]> boundary with an escaped greater-than sign"
  (expect (xml->string (node-from-spec '(:element "root" :children ("left]]>right"))))
          :to-equal "<root>left]]&gt;right</root>"))

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
         for element = (node-from-spec `(:element "root"
                                         :attributes ((:attribute "value" ,data))
                                         :children (,data)))
         for root = (first (xml-document-children (parse-xml (xml->string element))))
         unless (and (equal (xml-attribute-value (first (xml-element-attributes root)))
                            data)
                     (equal (apply #'concatenate 'string
                                   (mapcar #'xml-text-data (xml-element-children root)))
                            data))
           collect data)
   :to-equal nil))
