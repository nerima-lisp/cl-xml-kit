;;;; t/format-test.lisp

(in-package #:cl-xml-kit/test)

(defun fmt-lines (break &rest lines)
  "Join LINES with BREAK and end with BREAK."
  (concatenate 'string (spans-join break lines) break))

(defun fmt-lf (&rest lines)
  (apply #'fmt-lines (string #\Linefeed) lines))

(defun fmt-crlf (&rest lines)
  (apply #'fmt-lines +spans-crlf+ lines))

(defparameter +fmt-source-lines+
  '("<?xml version='1.0' encoding='UTF-8' standalone='yes'?>"
    "<!-- before -->"
    "<?before pi?>"
    "<!DOCTYPE root ["
    "  <!ENTITY e \"text\">"
    "]>"
    "<root xmlns=\"urn:default\" xmlns:p=\"urn:p\"   a='single'   b=\"double\">"
    "<!-- inside comment -->"
    "<?inside pi?>"
    "<mixed>Hello <b>big <i>world</i></b>! &#65;&amp;&e;</mixed>"
    "<kept xml:space=\"preserve\">  <x/>  text  <y/>  </kept>"
    "<cdata><![CDATA[a < b]]></cdata>"
    "<p:item xmlns:p=\"urn:p\"><p:leaf p:k='v'/></p:item>"
    "<deep><l1><l2><l3><l4>t</l4></l3></l2></l1></deep>"
    "<empty></empty>"
    "<spaces>   </spaces>"
    "</root>"
    "<!-- after -->"
    "<?after pi?>")
  "A document in a loose layout that exercises every rule of FORMAT-XML.")

(defparameter +fmt-expected-lines+
  '("<?xml version='1.0' encoding='UTF-8' standalone='yes'?>"
    "<!-- before -->"
    "<?before pi?>"
    "<!DOCTYPE root ["
    "  <!ENTITY e \"text\">"
    "]>"
    "<root xmlns=\"urn:default\" xmlns:p=\"urn:p\" a=\"single\" b=\"double\">"
    "  <!-- inside comment -->"
    "  <?inside pi?>"
    "  <mixed>Hello <b>big <i>world</i></b>! A&amp;text</mixed>"
    "  <kept xml:space=\"preserve\">  <x/>  text  <y/>  </kept>"
    "  <cdata>a &lt; b</cdata>"
    "  <p:item xmlns:p=\"urn:p\">"
    "    <p:leaf p:k=\"v\"/>"
    "  </p:item>"
    "  <deep>"
    "    <l1>"
    "      <l2>"
    "        <l3>"
    "          <l4>t</l4>"
    "        </l3>"
    "      </l2>"
    "    </l1>"
    "  </deep>"
    "  <empty/>"
    "  <spaces>   </spaces>"
    "</root>"
    "<!-- after -->"
    "<?after pi?>")
  "The expected output of FORMAT-XML for the source lines, at indent 2.")

(defun fmt-source (break &optional bom)
  (concatenate 'string (if bom +spans-bom+ "")
               (spans-join break +fmt-source-lines+)))

;;; Layout of small documents

(it "puts each child of element-only content on its own line, indented by depth"
  (expect (format-xml "<a><b/><c/></a>") :to-equal (fmt-lf "<a>" "  <b/>" "  <c/>" "</a>"))
  (expect (format-xml "<a><b><c>x</c></b></a>")
          :to-equal (fmt-lf "<a>" "  <b>" "    <c>x</c>" "  </b>" "</a>")))

(it "indents by the requested number of spaces per level"
  (expect (format-xml "<a><b><c>x</c></b></a>" :indent 4)
          :to-equal (fmt-lf "<a>" "    <b>" "        <c>x</c>" "    </b>" "</a>"))
  (expect (format-xml "<a><b/></a>" :indent 0) :to-equal (fmt-lf "<a>" "<b/>" "</a>")))

(it "re-indents a document that is indented differently"
  (expect (format-xml (fmt-lf "<a>" "      <b>" "   <c/>" "      </b>" "<d/>" "</a>"))
          :to-equal (fmt-lf "<a>" "  <b>" "    <c/>" "  </b>" "  <d/>" "</a>")))

(it "keeps comments and processing instructions as nodes on their own lines at every level"
  (expect (format-xml "<!--1--><?p 2?><a><!--3--><?p 4?><b><!--5--><?p 6?></b></a><!--7--><?p 8?>")
          :to-equal (fmt-lf "<!--1-->" "<?p 2?>" "<a>" "  <!--3-->" "  <?p 4?>" "  <b>"
                            "    <!--5-->" "    <?p 6?>" "  </b>" "</a>" "<!--7-->" "<?p 8?>")))

(it "writes an element with any non-whitespace text inline with its content as it is"
  (expect (format-xml "<p>Hello <b><i>w</i></b>!</p>") :to-equal (fmt-lf "<p>Hello <b><i>w</i></b>!</p>"))
  (expect (format-xml "<p>x <b>  <i>y</i> </b></p>")
          :to-equal (fmt-lf "<p>x <b>  <i>y</i> </b></p>")))

(it "keeps whitespace-only content and writes an element without content as empty"
  (expect (format-xml "<a>  </a>") :to-equal (fmt-lf "<a>  </a>"))
  (expect (format-xml "<a></a>") :to-equal (fmt-lf "<a/>"))
  (expect (format-xml "<a><b></b><c>  </c></a>")
          :to-equal (fmt-lf "<a>" "  <b/>" "  <c>  </c>" "</a>")))

(it "drops the whitespace between children of element-only content"
  (expect (format-xml (fmt-lf "<a>" "" "   " "<b/>" "  " "<c/>" "" "</a>"))
          :to-equal (fmt-lf "<a>" "  <b/>" "  <c/>" "</a>")))

(it "keeps the content of an xml:space=preserve element unchanged, and its descendants'"
  (expect (format-xml "<a xml:space=\"preserve\"> <b/> <c><d/></c> </a>")
          :to-equal (fmt-lf "<a xml:space=\"preserve\"> <b/> <c><d/></c> </a>")))

(it "formats again below an xml:space=default descendant of a preserved element"
  (expect (format-xml "<a xml:space=\"preserve\"><b xml:space=\"default\"><c/><d/></b></a>")
          :to-equal (fmt-lf "<a xml:space=\"preserve\"><b xml:space=\"default\">"
                            "    <c/>" "    <d/>" "  </b></a>")))

(it "lets the nearest xml:space decide and ignores other values"
  (expect (format-xml "<a xml:space=\"default\"><b xml:space=\"preserve\"><c><d/></c></b><e/></a>")
          :to-equal (fmt-lf "<a xml:space=\"default\">"
                            "  <b xml:space=\"preserve\"><c><d/></c></b>" "  <e/>" "</a>"))
  (expect (format-xml "<a xml:space=\"other\"><b/></a>")
          :to-equal (fmt-lf "<a xml:space=\"other\">" "  <b/>" "</a>")))

(it "does not treat an attribute named space in another namespace as xml:space"
  (expect (format-xml "<a xmlns:q=\"urn:q\" q:space=\"preserve\"><b/></a>")
          :to-equal (fmt-lf "<a xmlns:q=\"urn:q\" q:space=\"preserve\">" "  <b/>" "</a>")))

;;; Document level

(it "copies the XML declaration and the DOCTYPE verbatim and ends with one line break"
  (expect (format-xml "<?xml version='1.0'?><!DOCTYPE a [<!ENTITY x 'y'>]><a/>")
          :to-equal (fmt-lf "<?xml version='1.0'?>" "<!DOCTYPE a [<!ENTITY x 'y'>]>" "<a/>")))

(it "drops whitespace between the nodes of the document"
  (expect (format-xml (format nil "  <!--x-->~%~%  <a/>  ~%<!--y-->~%"))
          :to-equal (fmt-lf "<!--x-->" "<a/>" "<!--y-->")))

(it "formats the whole fixture"
  (expect (format-xml (fmt-source (string #\Linefeed)))
          :to-equal (apply #'fmt-lf +fmt-expected-lines+)))

(it "keeps a leading U+FEFF and writes every line break in the style of the source"
  (expect (format-xml (fmt-source +spans-crlf+ t))
          :to-equal (concatenate 'string +spans-bom+ (apply #'fmt-crlf +fmt-expected-lines+)))
  (expect (format-xml (fmt-source (string #\Return)))
          :to-equal (apply #'fmt-lines (string #\Return) +fmt-expected-lines+)))

(it "uses line feeds for a source without line breaks"
  (expect (format-xml "<a><b/></a>") :to-equal (fmt-lf "<a>" "  <b/>" "</a>")))

(it "takes the style of the first line break when a source mixes them"
  (expect (format-xml (format nil "<a>~C~C<b/>~C<c/></a>" #\Return #\Linefeed #\Linefeed))
          :to-equal (fmt-crlf "<a>" "  <b/>" "  <c/>" "</a>")))

(it "writes line breaks inside character data in the style of the source"
  (expect (format-xml (format nil "<a>~C~Cx~C~Cy</a>" #\Return #\Linefeed #\Return #\Linefeed))
          :to-equal (fmt-crlf "<a>" "x" "y</a>"))
  (expect (format-xml (format nil "<a><!--c~C~Cd--><?p e~C~Cf?></a>"
                              #\Return #\Linefeed #\Return #\Linefeed))
          :to-equal (fmt-crlf "<a>" "  <!--c" "d-->" "  <?p e" "f?>" "</a>")))

(it "writes a carriage return in character data as a reference"
  (expect (format-xml "<a>x&#13;y</a>") :to-equal (fmt-lf "<a>x&#13;y</a>")))

;;; What is not preserved

(it "turns single quotes into double quotes and keeps the attribute order"
  (expect (format-xml "<a z='1' y=\"2\" x='3'/>") :to-equal (fmt-lf "<a z=\"1\" y=\"2\" x=\"3\"/>")))

(it "writes CDATA sections and references as escaped text"
  (expect (format-xml "<a><![CDATA[<x> & ]]></a>") :to-equal (fmt-lf "<a>&lt;x> &amp; </a>"))
  (expect (format-xml "<a>&#65;&#x42;&lt;&gt;&amp;&quot;&apos;</a>")
          :to-equal (fmt-lf "<a>AB&lt;>&amp;\"'</a>"))
  (expect (format-xml "<a b=\"&#65;&lt;&gt;&quot;&apos;&#9;\"/>")
          :to-equal (fmt-lf "<a b=\"A&lt;&gt;&quot;'&#9;\"/>")))

(it "expands references to internal entities"
  (expect (format-xml "<!DOCTYPE a [<!ENTITY e \"x y\">]><a>&e;&e;</a>")
          :to-equal (fmt-lf "<!DOCTYPE a [<!ENTITY e \"x y\">]>" "<a>x yx y</a>")))

(it "normalizes whitespace inside tags and writes <a></a> as <a/>"
  (expect (format-xml (format nil "<a   x = '1'~%  y='2' ><b ></b ></a >"))
          :to-equal (fmt-lf "<a x=\"1\" y=\"2\">" "  <b/>" "</a>")))

(it "keeps namespace declarations as written, including redundant ones"
  (expect (format-xml "<r xmlns=\"u\" xmlns:p=\"v\"><p:a xmlns:p=\"v\"><b xmlns=\"u\" xmlns:q=\"w\"/></p:a></r>")
          :to-equal (fmt-lf "<r xmlns=\"u\" xmlns:p=\"v\">" "  <p:a xmlns:p=\"v\">"
                            "    <b xmlns=\"u\" xmlns:q=\"w\"/>" "  </p:a>" "</r>")))

(it "keeps a redeclared prefix and an undeclared default namespace"
  (expect (format-xml "<r xmlns=\"u\" xmlns:p=\"v\"><p:a xmlns:p=\"w\"><b xmlns=\"\"/></p:a></r>")
          :to-equal (fmt-lf "<r xmlns=\"u\" xmlns:p=\"v\">" "  <p:a xmlns:p=\"w\">"
                            "    <b xmlns=\"\"/>" "  </p:a>" "</r>")))

;;; Properties

(defun fmt-strip-bom (string)
  (if (and (plusp (length string)) (char= (char string 0) (code-char #xFEFF)))
      (subseq string 1)
      string))

(defun fmt-fixtures ()
  (list (fmt-source (string #\Linefeed)) (fmt-source +spans-crlf+ t)
        (fmt-source (string #\Return)) +spans-fixture+ +edit-source+
        "<a><b/><c/></a>" "<p>x <b>y</b></p>" "<r xml:space=\"preserve\"> <a/> </r>"))

(defun fmt-content (source)
  "Return the comments, processing instructions, element names, attributes, and non-whitespace text of SOURCE in document order."
  (let ((items nil))
    (labels ((walk (node)
               (typecase node
                 (xml-comment (push (list :comment (xml-comment-data node)) items))
                 (xml-processing-instruction
                  (push (list :pi (xml-processing-instruction-target node)
                              (xml-processing-instruction-data node))
                        items))
                 (xml-text (unless (every #'cl-xml-kit::xml-space-char-p (xml-text-data node))
                             (push (list :text (xml-text-data node)) items)))
                 (xml-element
                  (push (list :element (cl-xml-kit::%path-lexical-name (xml-element-name node))
                              (mapcar (lambda (attribute)
                                        (cons (cl-xml-kit::%path-lexical-name
                                               (xml-attribute-name attribute))
                                              (xml-attribute-value attribute)))
                                      (xml-element-attributes node)))
                        items)
                  (mapc #'walk (xml-element-children node)))
                 (xml-document (mapc #'walk (xml-document-children node))))))
      (walk (parse-xml (fmt-strip-bom source))))
    (nreverse items)))

(it "is idempotent on every fixture and indent"
  (dolist (source (fmt-fixtures))
    (dolist (indent '(0 2 4))
      (let ((once (format-xml source :indent indent)))
        (expect (format-xml once :indent indent) :to-equal once)))))

(it "keeps the elements, attributes, text, comments, and processing instructions of every fixture"
  (dolist (source (fmt-fixtures))
    (expect (fmt-content (format-xml source)) :to-equal (fmt-content source))))

(it "formats generated documents idempotently without losing content"
  (let ((formatted 0))
    (dolist (source (remove-if (lambda (source) (search "/>&" source))
                               (subseq (spans-generated-documents) 0 200)))
      (let ((once (format-xml source)))
        (incf formatted)
        (expect (format-xml once) :to-equal once)
        (expect (fmt-content once) :to-equal (fmt-content source))))
    (expect (> formatted 150) :to-be-truthy)))

;;; Serializer output is unchanged

(it "leaves xml->string and canonical output as they were"
  (let ((document (parse-xml "<a x='1'><!--c--><b/>t<![CDATA[<]]></a>")))
    (format-xml "<a x='1'><b/></a>")
    (expect (xml->string document) :to-equal "<a x=\"1\"><!--c--><b/>t&lt;</a>")
    (expect (xml->string document :pretty-print t) :to-equal "<a x=\"1\"><!--c--><b/>t&lt;</a>")
    (expect (xml->canonical-string document) :to-equal "<a x=\"1\"><b></b>t&lt;</a>")))

;;; Failures

(it "rejects a source or indent of the wrong type with an input error"
  (dolist (thunk (list (lambda () (format-xml 42))
                       (lambda () (format-xml "<a/>" :indent -1))
                       (lambda () (format-xml "<a/>" :indent 2.5))
                       (lambda () (format-xml "<a/>" :indent nil))))
    (expect (typep (handler-case (funcall thunk) (xml-kit-error (caught) caught))
                   'xml-input-error)
            :to-be-truthy)))

(it "signals the parse condition for a source that is not well-formed"
  (expect (typep (handler-case (format-xml "<a>") (xml-kit-error (caught) caught))
                 'xml-well-formedness-error)
          :to-be-truthy))

(it "applies the caller's external entity policy to the source"
  (let ((source "<!DOCTYPE a SYSTEM \"a.dtd\"><a><b/></a>"))
    (expect (typep (handler-case (format-xml source) (xml-kit-error (caught) caught))
                   'xml-external-resource-disabled)
            :to-be-truthy)
    (expect (format-xml source :external-entity-policy :skip)
            :to-equal (fmt-lf "<!DOCTYPE a SYSTEM \"a.dtd\">" "<a>" "  <b/>" "</a>"))))

(it "refuses a result that exceeds a parse limit of the caller as a resource limit"
  (let* ((limits (make-xml-resource-limits :max-document-characters 12))
         (condition (handler-case (format-xml "<a><b/></a>" :resource-limits limits)
                      (xml-edit-value-error (caught) caught))))
    (expect (list (type-of condition) (xml-edit-error-reason condition)
                  (xml-edit-error-path condition)
                  (xml-resource-limit-exceeded-kind (xml-edit-value-error-cause condition)))
            :to-equal '(xml-edit-value-error :resource-limit nil :max-document-characters))))

(it "keeps the output limit of the serializer as a resource limit condition"
  (let ((limits (make-xml-resource-limits :max-output-characters 10)))
    (expect (xml-resource-limit-exceeded-kind
             (handler-case (format-xml "<a><b/></a>" :resource-limits limits)
               (xml-resource-limit-exceeded (caught) caught)))
            :to-equal :max-output-characters)))

(it "refuses a result that differs from the source"
  (let ((context (cl-xml-kit::%path-context "<a><b/>t</a>" nil
                                            :resource-limits *default-xml-resource-limits*
                                            :external-entity-policy :reject)))
    (flet ((refusal (output)
             (handler-case (list :returned (cl-xml-kit::%format-verify context output))
               (xml-unsupported-edit (caught)
                 (list (xml-edit-error-reason caught) (xml-edit-error-path caught))))))
      (expect (refusal "<a><b/>u</a>") :to-equal '(:verification-failed nil))
      (expect (refusal "<a><c/>t</a>") :to-equal '(:verification-failed nil))
      (expect (refusal "<a><b x=\"1\"/>t</a>") :to-equal '(:verification-failed nil))
      (expect (refusal "<a><b/>t<!--c--></a>") :to-equal '(:verification-failed nil))
      (expect (refusal "<!--c--><a><b/>t</a>") :to-equal '(:verification-failed nil)))))

(it "refuses a result that does not parse"
  (let ((context (cl-xml-kit::%path-context "<a><b/></a>" nil
                                            :resource-limits *default-xml-resource-limits*
                                            :external-entity-policy :reject)))
    (expect (handler-case (cl-xml-kit::%format-verify context "<a><b></a>")
              (xml-unsupported-edit (caught)
                (list (xml-edit-error-reason caught) (xml-edit-error-path caught))))
            :to-equal '(:verification-failed nil))))

(it "accepts a result that differs only in layout whitespace"
  (let ((context (cl-xml-kit::%path-context "<a><b/><c/></a>" nil
                                            :resource-limits *default-xml-resource-limits*
                                            :external-entity-policy :reject))
        (output (format nil "<a>~%  <b/>~%  <c/>~%</a>~%")))
    (expect (cl-xml-kit::%format-verify context output) :to-equal output)))

(it "does not ignore whitespace in mixed or preserved content when verifying"
  (let ((mixed (cl-xml-kit::%path-context "<a>x<b/></a>" nil
                                          :resource-limits *default-xml-resource-limits*
                                          :external-entity-policy :reject))
        (preserved (cl-xml-kit::%path-context "<a xml:space=\"preserve\"><b/></a>" nil
                                              :resource-limits *default-xml-resource-limits*
                                              :external-entity-policy :reject)))
    (expect (handler-case (cl-xml-kit::%format-verify mixed (format nil "<a>x ~%<b/></a>"))
              (xml-unsupported-edit (caught) (xml-edit-error-reason caught)))
            :to-equal :verification-failed)
    (expect (handler-case (cl-xml-kit::%format-verify
                           preserved (format nil "<a xml:space=\"preserve\"> <b/></a>"))
              (xml-unsupported-edit (caught) (xml-edit-error-reason caught)))
            :to-equal :verification-failed)))
