;;;; t/format-test.lisp

(in-package #:cl-xml-kit/test)

(describe "layout of small documents"
  (it-each (("<a><b/><c/></a>" 2 ("<a>" "  <b/>" "  <c/>" "</a>"))
            ("<a><b/></a>" 2 ("<a>" "  <b/>" "</a>"))
            ("<a><b><c>x</c></b></a>" 2 ("<a>" "  <b>" "    <c>x</c>" "  </b>" "</a>"))
            ("<a><b><c>x</c></b></a>" 4 ("<a>" "    <b>" "        <c>x</c>" "    </b>" "</a>"))
            ("<a><b/></a>" 0 ("<a>" "<b/>" "</a>"))
            ("<!--1--><?p 2?><a><!--3--><?p 4?><b><!--5--><?p 6?></b></a><!--7--><?p 8?>" 2
             ("<!--1-->" "<?p 2?>" "<a>" "  <!--3-->" "  <?p 4?>" "  <b>" "    <!--5-->"
              "    <?p 6?>" "  </b>" "</a>" "<!--7-->" "<?p 8?>"))
            ("<p>Hello <b><i>w</i></b>!</p>" 2 ("<p>Hello <b><i>w</i></b>!</p>"))
            ("<p>x <b>  <i>y</i> </b></p>" 2 ("<p>x <b>  <i>y</i> </b></p>"))
            ("<a>  </a>" 2 ("<a>  </a>"))
            ("<a></a>" 2 ("<a/>"))
            ("<a><b></b><c>  </c></a>" 2 ("<a>" "  <b/>" "  <c>  </c>" "</a>"))
            ("<a xml:space=\"preserve\"> <b/> <c><d/></c> </a>" 2
             ("<a xml:space=\"preserve\"> <b/> <c><d/></c> </a>"))
            ("<a xml:space=\"preserve\"><b xml:space=\"default\"><c/><d/></b></a>" 2
             ("<a xml:space=\"preserve\"><b xml:space=\"default\">" "    <c/>" "    <d/>"
              "  </b></a>"))
            ("<a xml:space=\"default\"><b xml:space=\"preserve\"><c><d/></c></b><e/></a>" 2
             ("<a xml:space=\"default\">" "  <b xml:space=\"preserve\"><c><d/></c></b>" "  <e/>"
              "</a>"))
            ("<a xml:space=\"other\"><b/></a>" 2 ("<a xml:space=\"other\">" "  <b/>" "</a>"))
            ("<a xmlns:q=\"urn:q\" q:space=\"preserve\"><b/></a>" 2
             ("<a xmlns:q=\"urn:q\" q:space=\"preserve\">" "  <b/>" "</a>")))
      "formats ~S at indent ~D as the expected lines~*"
      (source indent lines)
    (expect (format-xml source :indent indent) :to-equal (apply #'format-lines +format-lf+ lines)))

  (it-each (("a document indented differently"
             #.(format nil "<a>~%      <b>~%   <c/>~%      </b>~%<d/>~%</a>") 2
             ("<a>" "  <b>" "    <c/>" "  </b>" "  <d/>" "</a>"))
            ("blank lines between children"
             #.(format nil "<a>~%~%   ~%<b/>~%  ~%<c/>~%~%</a>") 2 ("<a>" "  <b/>" "  <c/>" "</a>")))
      "formats ~A~* at indent ~D as the expected lines~*"
      (label source indent lines)
    (declare (ignore label))
    (expect (format-xml source :indent indent) :to-equal (apply #'format-lines +format-lf+ lines))))

(describe "document level"
  (it "copies the XML declaration and the DOCTYPE verbatim and ends with one line break"
    (expect (format-xml "<?xml version='1.0'?><!DOCTYPE a [<!ENTITY x 'y'>]><a/>")
            :to-equal (format-lines +format-lf+ "<?xml version='1.0'?>"
                                    "<!DOCTYPE a [<!ENTITY x 'y'>]>" "<a/>")))

  (it "drops white space between the nodes of the document"
    (expect (format-xml (format nil "  <!--x-->~%~%  <a/>  ~%<!--y-->~%"))
            :to-equal (format-lines +format-lf+ "<!--x-->" "<a/>" "<!--y-->")))

  (it "formats the whole fixture"
    (expect (format-xml (format-source +format-lf+))
            :to-equal (apply #'format-lines +format-lf+ +format-expected-lines+)))

  (it "keeps a leading U+FEFF and writes CRLF line breaks for a CRLF source"
    (expect (format-xml (format-source +format-crlf+ t))
            :to-equal (concatenate 'string +format-bom+
                                   (apply #'format-lines +format-crlf+ +format-expected-lines+))))

  (it "writes CR line breaks for a CR source"
    (expect (format-xml (format-source +format-cr+))
            :to-equal (apply #'format-lines +format-cr+ +format-expected-lines+)))

  (it "takes the style of the first line break when a source mixes them"
    (expect (format-xml (format nil "<a>~C~C<b/>~C<c/></a>" #\Return #\Linefeed #\Linefeed))
            :to-equal (format-lines +format-crlf+ "<a>" "  <b/>" "  <c/>" "</a>")))

  (it-each (("character data"
             #.(format nil "<a>~C~Cx~C~Cy</a>" #\Return #\Linefeed #\Return #\Linefeed)
             ("<a>" "x" "y</a>"))
            ("a comment and a processing instruction"
             #.(format nil "<a><!--c~C~Cd--><?p e~C~Cf?></a>" #\Return #\Linefeed #\Return #\Linefeed)
             ("<a>" "  <!--c" "d-->" "  <?p e" "f?>" "</a>")))
      "writes the line breaks inside ~A~* in the CRLF style of the source~*"
      (label source lines)
    (declare (ignore label))
    (expect (format-xml source) :to-equal (apply #'format-lines +format-crlf+ lines)))

  (it "writes a carriage return in character data as a reference"
    (expect (format-xml "<a>x&#13;y</a>") :to-equal (format-lines +format-lf+ "<a>x&#13;y</a>"))))

(describe "what is not preserved"
  (it-each (("<a z='1' y=\"2\" x='3'/>" "<a z=\"1\" y=\"2\" x=\"3\"/>")
            ("<a><![CDATA[<x> & ]]></a>" "<a>&lt;x> &amp; </a>")
            ("<a>&#65;&#x42;&lt;&gt;&amp;&quot;&apos;</a>" "<a>AB&lt;>&amp;\"'</a>")
            ("<a b=\"&#65;&lt;&gt;&quot;&apos;&#9;\"/>" "<a b=\"A&lt;&gt;&quot;'&#9;\"/>"))
      "writes ~S as the single line ~S"
      (source line)
    (expect (format-xml source) :to-equal (format-lines +format-lf+ line)))

  (it "expands references to internal entities"
    (expect (format-xml "<!DOCTYPE a [<!ENTITY e \"x y\">]><a>&e;&e;</a>")
            :to-equal (format-lines +format-lf+ "<!DOCTYPE a [<!ENTITY e \"x y\">]>"
                                    "<a>x yx y</a>")))

  (it "normalizes white space inside tags and writes <a></a> as <a/>"
    (expect (format-xml (format nil "<a   x = '1'~%  y='2' ><b ></b ></a >"))
            :to-equal (format-lines +format-lf+ "<a x=\"1\" y=\"2\">" "  <b/>" "</a>")))

  (it-each (("<r xmlns=\"u\" xmlns:p=\"v\"><p:a xmlns:p=\"v\"><b xmlns=\"u\" xmlns:q=\"w\"/></p:a></r>"
             ("<r xmlns=\"u\" xmlns:p=\"v\">" "  <p:a xmlns:p=\"v\">"
              "    <b xmlns=\"u\" xmlns:q=\"w\"/>" "  </p:a>" "</r>"))
            ("<r xmlns=\"u\" xmlns:p=\"v\"><p:a xmlns:p=\"w\"><b xmlns=\"\"/></p:a></r>"
             ("<r xmlns=\"u\" xmlns:p=\"v\">" "  <p:a xmlns:p=\"w\">" "    <b xmlns=\"\"/>"
              "  </p:a>" "</r>")))
      "keeps the namespace declarations of ~S as written~*"
      (source lines)
    (expect (format-xml source) :to-equal (apply #'format-lines +format-lf+ lines))))

(describe "properties"
  (it "is idempotent on every fixture and indent"
    (expect (loop for source in (format-fixtures)
                  nconc (loop for indent in '(0 2 4)
                              for once = (format-xml source :indent indent)
                              unless (equal (format-xml once :indent indent) once)
                                collect (list indent source)))
            :to-equal nil))

  (it "keeps the elements, attributes, text, comments, and processing instructions of every fixture"
    (expect (loop for source in (format-fixtures)
                  unless (equal (format-content (format-xml source)) (format-content source))
                    collect source)
            :to-equal nil))

  (it-property "formats generated documents idempotently without losing content"
      ((raw (gen-document-spec))
       (break (gen-member (list +format-lf+ +format-crlf+ +format-cr+)))
       (prolog (gen-member '(:none :declaration :doctype :both)))
       (pretty (gen-boolean))
       (bom (gen-boolean)))
    (let* ((source (format-generated-source raw break prolog pretty bom))
           (once (format-xml source)))
      (expect (format-xml once) :to-equal once)
      (expect (format-content once) :to-equal (format-content source))))

  (it "leaves xml->string and canonical output as they were"
    (let ((document (parse-xml "<a x='1'><!--c--><b/>t<![CDATA[<]]></a>")))
      (format-xml "<a x='1'><b/></a>")
      (expect (xml->string document) :to-equal "<a x=\"1\"><!--c--><b/>t&lt;</a>")
      (expect (xml->string document :pretty-print t) :to-equal "<a x=\"1\"><!--c--><b/>t&lt;</a>")
      (expect (xml->canonical-string document) :to-equal "<a x=\"1\"><b></b>t&lt;</a>"))))

(describe "refusals"
  (it-each ((42 () (xml-input-error (xml-condition-message "SOURCE must be a string.")))
            ("<a/>" (:indent -1)
             (xml-input-error (xml-condition-message "INDENT must be a non-negative integer.")))
            ("<a/>" (:indent 2.5)
             (xml-input-error (xml-condition-message "INDENT must be a non-negative integer.")))
            ("<a/>" (:indent nil)
             (xml-input-error (xml-condition-message "INDENT must be a non-negative integer.")))
            ("<a>" ()
             (xml-well-formedness-error (xml-condition-message "Unexpected end of document.")
                                        (xml-well-formedness-error-expected :end-tag)))
            ("<!DOCTYPE a SYSTEM \"a.dtd\"><a><b/></a>" ()
             (xml-external-resource-disabled
              (xml-external-resource-disabled-kind :external-dtd)
              (xml-external-resource-disabled-identifier "a"))))
      "refuses to format ~S with options ~S"
      (source options expected)
    (expect (lambda () (apply #'format-xml source options)) :to-signal-xml expected))

  (it-each (((:max-output-characters 10)
             (xml-resource-limit-exceeded
              (xml-resource-limit-exceeded-kind :max-output-characters)
              (xml-resource-limit-exceeded-value 11)
              (xml-resource-limit-exceeded-limit 10)))
            ((:max-document-characters 12)
             (xml-edit-value-error (xml-edit-error-reason :resource-limit)
                                   (xml-edit-error-path nil)
                                   (format-cause-limit-kind :max-document-characters))))
      "refuses to format <a><b/></a> under the resource limits~{ ~S~}~*"
      (limits expected)
    (expect (lambda ()
              (format-xml "<a><b/></a>"
                          :resource-limits (apply #'make-xml-resource-limits limits)))
            :to-signal-xml expected))

  (it "formats under the caller's external entity policy"
    (expect (format-xml "<!DOCTYPE a SYSTEM \"a.dtd\"><a><b/></a>" :external-entity-policy :skip)
            :to-equal (format-lines +format-lf+ "<!DOCTYPE a SYSTEM \"a.dtd\">" "<a>" "  <b/>"
                                    "</a>")))

  (it-each (("changed text" "<a><b/>t</a>" "<a><b/>u</a>")
            ("a renamed element" "<a><b/>t</a>" "<a><c/>t</a>")
            ("an added attribute" "<a><b/>t</a>" "<a><b x=\"1\"/>t</a>")
            ("an added comment" "<a><b/>t</a>" "<a><b/>t<!--c--></a>")
            ("a comment added before the root" "<a><b/>t</a>" "<!--c--><a><b/>t</a>")
            ("an unclosed element" "<a><b/></a>" "<a><b></a>")
            ("white space added to mixed content" "<a>x<b/></a>" #.(format nil "<a>x ~%<b/></a>"))
            ("white space added to preserved content"
             "<a xml:space=\"preserve\"><b/></a>" "<a xml:space=\"preserve\"> <b/></a>"))
      "refuses to verify a formatted result with ~A~2*"
      (label source output)
    (declare (ignore label))
    (expect (lambda ()
              (cl-xml-kit::%format-verify
               (cl-xml-kit::%path-context source nil
                                          :resource-limits *default-xml-resource-limits*
                                          :external-entity-policy :reject)
               output))
            :to-signal-xml '(xml-unsupported-edit (xml-edit-error-reason :verification-failed)
                                                  (xml-edit-error-path nil))))

  (it "accepts a result that differs only in layout white space"
    (let ((output (format nil "<a>~%  <b/>~%  <c/>~%</a>~%")))
      (expect (cl-xml-kit::%format-verify
               (cl-xml-kit::%path-context "<a><b/><c/></a>" nil
                                          :resource-limits *default-xml-resource-limits*
                                          :external-entity-policy :reject)
               output)
              :to-be output))))
