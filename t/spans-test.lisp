;;;; t/spans-test.lisp

(in-package #:cl-xml-kit/test)

(defparameter +spans-bom+ (string (code-char #xFEFF)))
(defparameter +spans-crlf+ (coerce '(#\Return #\Linefeed) 'string))
(defparameter +spans-lf+ (string #\Linefeed))
(defparameter +spans-cr+ (string #\Return))

(defun spans-join (separator lines)
  (with-output-to-string (stream)
    (loop for (line . more) on lines
          do (write-string line stream)
             (when more
               (write-string separator stream)))))

(defun spans-crlf (&rest lines)
  (spans-join +spans-crlf+ lines))

(defparameter +spans-fixture-lines+
  '("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<!DOCTYPE root ["
    "  <!ENTITY e \"text\">"
    "  <!ENTITY m \"<b/>\">"
    "  <!ATTLIST item id ID #IMPLIED>"
    "]>"
    "<!-- top comment -->"
    "<?pi data?>"
    "<root xmlns=\"urn:default\" xmlns:p=\"urn:p\""
    "      a='single'"
    "      b=\"double\">"
    "  <item id=\"1\">text &amp; &#65; &e; <![CDATA[<raw>]]></item>"
    "  <p:item xmlns:p=\"urn:other\"/>"
    "  <!-- inner"
    "       comment -->"
    "  <?inner pi?>"
    "  <empty />"
    "</root>"
    "<!-- trailing -->")
  "The fixture lines: XML declaration, DOCTYPE with an internal subset, markup
before and after the root, three attribute forms, and a redeclared prefix.")

(defparameter +spans-fixture+
  (concatenate 'string +spans-bom+ (spans-join +spans-crlf+ +spans-fixture-lines+))
  "The fixture with a leading U+FEFF and CRLF line breaks.")

(defun spans-fixture-with (separator)
  (spans-join separator +spans-fixture-lines+))

(defun spans-slice (source node)
  (subseq source (cl-xml-kit::span-node-start node) (cl-xml-kit::span-node-end node)))

(defun spans-start-tag (source element)
  (subseq source (cl-xml-kit::span-node-start element)
          (cl-xml-kit::span-element-start-tag-end element)))

(defun spans-content (source element)
  (subseq source (cl-xml-kit::span-element-content-start element)
          (cl-xml-kit::span-element-content-end element)))

(defun spans-end-tag (source element)
  (and (cl-xml-kit::span-element-end-tag-start element)
       (subseq source (cl-xml-kit::span-element-end-tag-start element)
               (cl-xml-kit::span-element-end-tag-end element))))

(defun spans-child-elements (element)
  (remove-if-not #'cl-xml-kit::span-element-p
                 (cl-xml-kit::span-element-children element)))

(defun spans-kind (node)
  (typecase node
    (cl-xml-kit::span-element :element)
    (cl-xml-kit::span-text :text)
    (cl-xml-kit::span-comment :comment)
    (cl-xml-kit::span-processing-instruction :processing-instruction)
    (cl-xml-kit::span-doctype :doctype)
    (t :other)))

(defun spans-fixture-elements ()
  "Return the scanned fixture, its root, and the item, p:item, and empty elements."
  (let* ((document (cl-xml-kit::scan-xml-spans +spans-fixture+))
         (root (cl-xml-kit::span-document-root document))
         (elements (spans-child-elements root)))
    (values document root (first elements) (second elements) (third elements))))

(defun spans-attribute (element qname)
  (find qname (cl-xml-kit::span-element-attributes element)
        :key #'cl-xml-kit::span-attribute-qname :test #'string=))

(defun spans-attribute-text (source attribute)
  "Return the name, value including quotes, and full text of ATTRIBUTE."
  (list (subseq source (cl-xml-kit::span-attribute-name-start attribute)
                (cl-xml-kit::span-attribute-name-end attribute))
        (subseq source (cl-xml-kit::span-attribute-value-start attribute)
                (cl-xml-kit::span-attribute-value-end attribute))
        (subseq source (cl-xml-kit::span-attribute-space-start attribute)
                (cl-xml-kit::span-attribute-value-end attribute))))

(defun spans-scan-condition (source &rest options)
  (handler-case (progn (apply #'cl-xml-kit::scan-xml-spans source options) nil)
    (xml-kit-error (caught) caught)))

(defun spans-accepted-p (source &rest options)
  "Return true when PARSE-XML accepts SOURCE.

A numeric reference such as &#1z; makes PARSE-XML signal a bare PARSE-ERROR."
  (handler-case (progn (apply #'parse-xml source options) t)
    ((or xml-kit-error parse-error) () nil)))

;;; Span positions

(it "parses the fixture only after its leading U+FEFF is removed"
  (expect (typep (handler-case (parse-xml +spans-fixture+)
                   (xml-well-formedness-error (caught) caught))
                 'xml-well-formedness-error)
          :to-be-truthy)
  (expect (xml-document-p (parse-xml (subseq +spans-fixture+ 1))) :to-be-truthy))

(it "records the XML declaration span"
  (let ((document (cl-xml-kit::scan-xml-spans +spans-fixture+)))
    (expect (spans-slice +spans-fixture+ (cl-xml-kit::span-document-declaration document))
            :to-equal "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")))

(it "records no XML declaration when the source has none"
  (expect (cl-xml-kit::span-document-declaration (cl-xml-kit::scan-xml-spans "<a/>"))
          :to-equal nil))

(it "records the DOCTYPE span"
  (let ((doctype (cl-xml-kit::span-document-doctype
                  (cl-xml-kit::scan-xml-spans +spans-fixture+))))
    (expect (spans-slice +spans-fixture+ doctype)
            :to-equal (spans-crlf "<!DOCTYPE root ["
                                  "  <!ENTITY e \"text\">"
                                  "  <!ENTITY m \"<b/>\">"
                                  "  <!ATTLIST item id ID #IMPLIED>"
                                  "]>"))
    (expect (cl-xml-kit::span-doctype-root-name doctype) :to-equal "root")))

(it "records the internal subset span in the original string"
  (let ((doctype (cl-xml-kit::span-document-doctype
                  (cl-xml-kit::scan-xml-spans +spans-fixture+))))
    (expect (subseq +spans-fixture+
                    (cl-xml-kit::span-doctype-subset-start doctype)
                    (cl-xml-kit::span-doctype-subset-end doctype))
            :to-equal (concatenate 'string +spans-crlf+
                                   (spans-crlf "  <!ENTITY e \"text\">"
                                               "  <!ENTITY m \"<b/>\">"
                                               "  <!ATTLIST item id ID #IMPLIED>"
                                               "")))))

(it "reads the internal subset text as the DOM does"
  (let ((doctype (cl-xml-kit::span-document-doctype
                  (cl-xml-kit::scan-xml-spans +spans-fixture+)))
        (dom-doctype (first (xml-document-children
                             (parse-xml (subseq +spans-fixture+ 1))))))
    (expect (cl-xml-kit::span-doctype-internal-subset doctype)
            :to-equal (xml-doctype-internal-subset dom-doctype))))

(it "records no internal subset for a DOCTYPE without one"
  (let ((doctype (cl-xml-kit::span-document-doctype
                  (cl-xml-kit::scan-xml-spans "<!DOCTYPE a><a/>"))))
    (expect (list (cl-xml-kit::span-doctype-internal-subset doctype)
                  (cl-xml-kit::span-doctype-subset-start doctype)
                  (cl-xml-kit::span-doctype-subset-end doctype))
            :to-equal '(nil nil nil))))

(it "records the start tag, content, and end tag of the root element"
  (multiple-value-bind (document root) (spans-fixture-elements)
    (declare (ignore document))
    (expect (spans-start-tag +spans-fixture+ root)
            :to-equal (spans-crlf "<root xmlns=\"urn:default\" xmlns:p=\"urn:p\""
                                  "      a='single'"
                                  "      b=\"double\">"))
    (expect (spans-end-tag +spans-fixture+ root) :to-equal "</root>")
    (expect (spans-content +spans-fixture+ root)
            :to-equal (concatenate
                       'string +spans-crlf+
                       (spans-crlf "  <item id=\"1\">text &amp; &#65; &e; <![CDATA[<raw>]]></item>"
                                   "  <p:item xmlns:p=\"urn:other\"/>"
                                   "  <!-- inner"
                                   "       comment -->"
                                   "  <?inner pi?>"
                                   "  <empty />"
                                   "")))
    (expect (spans-slice +spans-fixture+ root)
            :to-equal (concatenate 'string
                                   (spans-start-tag +spans-fixture+ root)
                                   (spans-content +spans-fixture+ root)
                                   "</root>"))))

(it "records the start tag, content, and end tag of a non-empty child element"
  (multiple-value-bind (document root item) (spans-fixture-elements)
    (declare (ignore document root))
    (expect (spans-start-tag +spans-fixture+ item) :to-equal "<item id=\"1\">")
    (expect (spans-content +spans-fixture+ item)
            :to-equal "text &amp; &#65; &e; <![CDATA[<raw>]]>")
    (expect (spans-end-tag +spans-fixture+ item) :to-equal "</item>")
    (expect (spans-slice +spans-fixture+ item)
            :to-equal "<item id=\"1\">text &amp; &#65; &e; <![CDATA[<raw>]]></item>")
    (expect (cl-xml-kit::span-element-empty-p item) :to-equal nil)))

(it "records an empty element with its content range at the slash"
  (multiple-value-bind (document root item prefixed) (spans-fixture-elements)
    (declare (ignore document root item))
    (let ((slash (cl-xml-kit::span-element-content-start prefixed)))
      (expect (spans-slice +spans-fixture+ prefixed)
              :to-equal "<p:item xmlns:p=\"urn:other\"/>")
      (expect (cl-xml-kit::span-element-empty-p prefixed) :to-be-truthy)
      (expect (cl-xml-kit::span-element-content-end prefixed) :to-equal slash)
      (expect (subseq +spans-fixture+ slash (cl-xml-kit::span-element-start-tag-end prefixed))
              :to-equal "/>")
      (expect (cl-xml-kit::span-element-end-tag-start prefixed) :to-equal nil)
      (expect (cl-xml-kit::span-element-end-tag-end prefixed) :to-equal nil))))

(it "puts the content range of a spaced empty element at the slash, after the space"
  (multiple-value-bind (document root item prefixed empty) (spans-fixture-elements)
    (declare (ignore document root item prefixed))
    (expect (spans-slice +spans-fixture+ empty) :to-equal "<empty />")
    (expect (subseq +spans-fixture+ (cl-xml-kit::span-element-content-start empty)
                    (cl-xml-kit::span-element-start-tag-end empty))
            :to-equal "/>")))

(it "records lexical qualified names as written"
  (multiple-value-bind (document root item prefixed empty) (spans-fixture-elements)
    (declare (ignore document))
    (expect (mapcar #'cl-xml-kit::span-element-qname (list root item prefixed empty))
            :to-equal '("root" "item" "p:item" "empty"))))

(it "records the index after each element name"
  (multiple-value-bind (document root item prefixed empty) (spans-fixture-elements)
    (declare (ignore document))
    (expect (mapcar (lambda (element)
                      (subseq +spans-fixture+ (cl-xml-kit::span-node-start element)
                              (cl-xml-kit::span-element-name-end element)))
                    (list root item prefixed empty))
            :to-equal '("<root" "<item" "<p:item" "<empty"))))

(it "records attribute names, quoted values, and leading whitespace"
  (multiple-value-bind (document root) (spans-fixture-elements)
    (declare (ignore document))
    (expect (mapcar (lambda (qname)
                      (spans-attribute-text +spans-fixture+ (spans-attribute root qname)))
                    '("xmlns" "xmlns:p" "a" "b"))
            :to-equal
            (list (list "xmlns" "\"urn:default\"" " xmlns=\"urn:default\"")
                  (list "xmlns:p" "\"urn:p\"" " xmlns:p=\"urn:p\"")
                  (list "a" "'single'" (concatenate 'string +spans-crlf+ "      a='single'"))
                  (list "b" "\"double\"" (concatenate 'string +spans-crlf+ "      b=\"double\""))))))

(it "records the text between the quotes of each attribute value"
  (multiple-value-bind (document root) (spans-fixture-elements)
    (declare (ignore document))
    (expect (mapcar (lambda (attribute)
                      (subseq +spans-fixture+
                              (1+ (cl-xml-kit::span-attribute-value-start attribute))
                              (1- (cl-xml-kit::span-attribute-value-end attribute))))
                    (cl-xml-kit::span-element-attributes root))
            :to-equal '("urn:default" "urn:p" "single" "double"))))

(it "keeps whitespace around the equals sign and in the end tag inside their spans"
  (let* ((source (format nil "<a  x = \"1\"~%  y~%=~%'2'  ></a  >"))
         (element (cl-xml-kit::span-document-root (cl-xml-kit::scan-xml-spans source))))
    (expect (mapcar (lambda (qname)
                      (spans-attribute-text source (spans-attribute element qname)))
                    '("x" "y"))
            :to-equal (list (list "x" "\"1\"" (format nil "  x = \"1\""))
                            (list "y" "'2'" (format nil "~%  y~%=~%'2'"))))
    (expect (spans-start-tag source element)
            :to-equal (format nil "<a  x = \"1\"~%  y~%=~%'2'  >"))
    (expect (spans-end-tag source element) :to-equal "</a  >")))

(it "records the quote character of each attribute"
  (multiple-value-bind (document root item) (spans-fixture-elements)
    (declare (ignore document))
    (expect (mapcar (lambda (attribute) (cl-xml-kit::span-attribute-quote-char attribute))
                    (append (cl-xml-kit::span-element-attributes root)
                            (cl-xml-kit::span-element-attributes item)))
            :to-equal '(#\" #\" #\' #\" #\"))))

(it "flags namespace declarations among the attributes in source order"
  (multiple-value-bind (document root) (spans-fixture-elements)
    (declare (ignore document))
    (expect (mapcar (lambda (attribute)
                      (list (cl-xml-kit::span-attribute-qname attribute)
                            (cl-xml-kit::span-attribute-declaration-p attribute)))
                    (cl-xml-kit::span-element-attributes root))
            :to-equal '(("xmlns" t) ("xmlns:p" t) ("a" nil) ("b" nil)))))

(it "records a prefix redeclared on a child element"
  (multiple-value-bind (document root item prefixed) (spans-fixture-elements)
    (declare (ignore document root item))
    (let ((attribute (spans-attribute prefixed "xmlns:p")))
      (expect (cl-xml-kit::span-attribute-declaration-p attribute) :to-be-truthy)
      (expect (spans-attribute-text +spans-fixture+ attribute)
              :to-equal (list "xmlns:p" "\"urn:other\"" " xmlns:p=\"urn:other\"")))))

(it "records the child sequence of the root element"
  (multiple-value-bind (document root) (spans-fixture-elements)
    (declare (ignore document))
    (expect (mapcar #'spans-kind (cl-xml-kit::span-element-children root))
            :to-equal '(:text :element :text :element :text :comment :text
                        :processing-instruction :text :element :text))))

(it "records a text run mixing text, references, and CDATA as one span with flags"
  (multiple-value-bind (document root item) (spans-fixture-elements)
    (declare (ignore document root))
    (let ((run (first (cl-xml-kit::span-element-children item))))
      (expect (spans-slice +spans-fixture+ run)
              :to-equal "text &amp; &#65; &e; <![CDATA[<raw>]]>")
      (expect (list (cl-xml-kit::span-text-cdata-p run)
                    (cl-xml-kit::span-text-single-cdata-p run)
                    (cl-xml-kit::span-text-char-ref-p run)
                    (cl-xml-kit::span-text-predefined-ref-p run)
                    (cl-xml-kit::span-text-entity-names run))
              :to-equal '(t nil t t ("e"))))))

(it "records a plain whitespace run with no flags set"
  (multiple-value-bind (document root) (spans-fixture-elements)
    (declare (ignore document))
    (let ((run (first (cl-xml-kit::span-element-children root))))
      (expect (spans-slice +spans-fixture+ run)
              :to-equal (concatenate 'string +spans-crlf+ "  "))
      (expect (list (cl-xml-kit::span-text-cdata-p run)
                    (cl-xml-kit::span-text-single-cdata-p run)
                    (cl-xml-kit::span-text-char-ref-p run)
                    (cl-xml-kit::span-text-predefined-ref-p run)
                    (cl-xml-kit::span-text-entity-names run))
              :to-equal '(nil nil nil nil nil)))))

(it "marks content that is exactly one CDATA section"
  (flet ((single-p (source)
           (cl-xml-kit::span-text-single-cdata-p
            (first (cl-xml-kit::span-element-children
                    (cl-xml-kit::span-document-root
                     (cl-xml-kit::scan-xml-spans source)))))))
    (expect (list (single-p "<a><![CDATA[x]]></a>")
                  (single-p "<a><![CDATA[]]></a>")
                  (single-p "<a><![CDATA[x]]>y</a>")
                  (single-p "<a><![CDATA[x]]><![CDATA[y]]></a>"))
            :to-equal '(t t nil nil))))

(it "records a lone CDATA section as a text run spanning its delimiters"
  (let* ((source "<a><![CDATA[x<y]]></a>")
         (run (first (cl-xml-kit::span-element-children
                      (cl-xml-kit::span-document-root
                       (cl-xml-kit::scan-xml-spans source))))))
    (expect (spans-slice source run) :to-equal "<![CDATA[x<y]]>")))

(it "records each distinct general entity reference once, in order of appearance"
  (let* ((source "<!DOCTYPE a [<!ENTITY x \"1\"><!ENTITY y \"2\">]><a>&y;&x;&y;</a>")
         (run (first (cl-xml-kit::span-element-children
                      (cl-xml-kit::span-document-root
                       (cl-xml-kit::scan-xml-spans source))))))
    (expect (cl-xml-kit::span-text-entity-names run) :to-equal '("y" "x"))))

(it "records comments and processing instructions inside the root"
  (multiple-value-bind (document root) (spans-fixture-elements)
    (declare (ignore document))
    (let ((children (cl-xml-kit::span-element-children root)))
      (expect (spans-slice +spans-fixture+ (find :comment children :key #'spans-kind))
              :to-equal (spans-crlf "<!-- inner" "       comment -->"))
      (expect (spans-slice +spans-fixture+
                           (find :processing-instruction children :key #'spans-kind))
              :to-equal "<?inner pi?>"))))

(it "records the top-level nodes in order without the XML declaration"
  (let* ((document (cl-xml-kit::scan-xml-spans +spans-fixture+))
         (children (cl-xml-kit::span-document-children document)))
    (expect (mapcar #'spans-kind children)
            :to-equal '(:doctype :comment :processing-instruction :element :comment))
    (expect (mapcar (lambda (node) (spans-slice +spans-fixture+ node))
                    (remove :element (remove :doctype children :key #'spans-kind)
                            :key #'spans-kind))
            :to-equal '("<!-- top comment -->" "<?pi data?>" "<!-- trailing -->"))))

(it "records the first line-break style"
  (flet ((style (source)
           (cl-xml-kit::span-document-line-break (cl-xml-kit::scan-xml-spans source))))
    (expect (list (style "<a>x</a>")
                  (style (spans-crlf "<a>" "x</a>"))
                  (style (spans-join +spans-lf+ '("<a>" "x</a>")))
                  (style (spans-join +spans-cr+ '("<a>" "x</a>")))
                  (style (format nil "<a>~C~Cx~C</a>" #\Return #\Linefeed #\Linefeed))
                  (style (format nil "<a>~Cx~C~C</a>" #\Linefeed #\Return #\Linefeed))
                  (style (format nil "<a>~C~Cx</a>" #\Return #\Return)))
            :to-equal '(:lf :crlf :lf :cr :crlf :lf :cr))))

(it "maps spans back through collapsed CRLF pairs to the original string"
  (labels ((collapse (string)
             (with-output-to-string (stream)
               (loop with index = 0
                     while (< index (length string))
                     do (let ((character (char string index)))
                          (cond
                            ((and (char= character #\Return)
                                  (< (1+ index) (length string))
                                  (char= (char string (1+ index)) #\Linefeed))
                             (write-char #\Linefeed stream)
                             (incf index 2))
                            ((char= character #\Return)
                             (write-char #\Linefeed stream)
                             (incf index))
                            (t (write-char character stream)
                               (incf index)))))))
           (slices (source)
             (let ((document (cl-xml-kit::scan-xml-spans source))
                   (slices nil))
               (labels ((walk (node)
                          (push (spans-slice source node) slices)
                          (when (cl-xml-kit::span-element-p node)
                            (push (spans-start-tag source node) slices)
                            (push (spans-content source node) slices)
                            (dolist (attribute (cl-xml-kit::span-element-attributes node))
                              (push (third (spans-attribute-text source attribute)) slices))
                            (mapc #'walk (cl-xml-kit::span-element-children node)))))
                 (mapc #'walk (cl-xml-kit::span-document-children document)))
               (nreverse slices))))
    (let ((expected (slices (spans-fixture-with +spans-lf+))))
      (expect (mapcar #'collapse (slices (spans-fixture-with +spans-crlf+)))
              :to-equal expected)
      (expect (mapcar #'collapse (slices (spans-fixture-with +spans-cr+)))
              :to-equal expected))))

(it "shifts every span by one for a leading U+FEFF"
  (let ((plain (cl-xml-kit::scan-xml-spans (subseq +spans-fixture+ 1)))
        (marked (cl-xml-kit::scan-xml-spans +spans-fixture+)))
    (expect (list (- (cl-xml-kit::span-node-start (cl-xml-kit::span-document-root marked))
                     (cl-xml-kit::span-node-start (cl-xml-kit::span-document-root plain)))
                  (- (cl-xml-kit::span-node-end (cl-xml-kit::span-document-declaration marked))
                     (cl-xml-kit::span-node-end (cl-xml-kit::span-document-declaration plain)))
                  (cl-xml-kit::span-node-start (cl-xml-kit::span-document-declaration marked)))
            :to-equal '(1 1 1))))

;;; Entities and ATTLIST declarations

(it "lists the internal entities whose replacement text holds markup"
  (expect (cl-xml-kit::span-document-markup-entity-names
           (cl-xml-kit::scan-xml-spans +spans-fixture+))
          :to-equal '("m")))

(it "follows markup through entity references and character references"
  (let ((source (concatenate 'string
                             "<!DOCTYPE r ["
                             "<!ENTITY t \"x\"><!ENTITY m \"<b/>\"><!ENTITY n \"a&m;\">"
                             "<!ENTITY c \"&#60;i/>\"><!ENTITY w \"&t;\">"
                             "]><r/>")))
    (expect (spans-accepted-p source) :to-be-truthy)
    (expect (cl-xml-kit::span-document-markup-entity-names
             (cl-xml-kit::scan-xml-spans source))
            :to-equal '("c" "m" "n"))))

(it "terminates on cyclic and long chains of entity declarations"
  (let ((source (with-output-to-string (stream)
                  (write-string "<!DOCTYPE r [<!ENTITY a \"&b;\"><!ENTITY b \"&a;<x/>\">" stream)
                  (loop for index from 0 below 3000
                        do (format stream "<!ENTITY c~D \"&c~D;\">" index (1+ index)))
                  (write-string "<!ENTITY c3000 \"<z/>\">]><r/>" stream))))
    (expect (spans-accepted-p source) :to-be-truthy)
    (expect (length (cl-xml-kit::span-document-markup-entity-names
                     (cl-xml-kit::scan-xml-spans source)))
            :to-equal 3003)))

(it "counts a predefined lt reference in an entity value as markup, as the parser does"
  (let ((declaration "<!DOCTYPE r [<!ENTITY l \"a&lt;b\">]>"))
    (expect (cl-xml-kit::span-document-markup-entity-names
             (cl-xml-kit::scan-xml-spans (concatenate 'string declaration "<r/>")))
            :to-equal '("l"))
    (expect (spans-accepted-p (concatenate 'string declaration "<r>&l;</r>"))
            :to-equal nil)))

(it "ignores external entity declarations when listing markup entities"
  (let ((source "<!DOCTYPE r [<!ENTITY x SYSTEM \"x.ent\"><!ENTITY y \"<b/>\">]><r/>"))
    (expect (spans-accepted-p source :external-entity-policy :skip) :to-be-truthy)
    (expect (cl-xml-kit::span-document-markup-entity-names
             (cl-xml-kit::scan-xml-spans source :external-entity-policy :skip))
            :to-equal '("y"))))

(it "reports a reference to a markup entity that the DOM expands into elements"
  (let* ((source "<!DOCTYPE r [<!ENTITY m \"<b/>\">]><r>&m;</r>")
         (document (cl-xml-kit::scan-xml-spans source))
         (root (cl-xml-kit::span-document-root document))
         (dom-root (find-if #'xml-element-p
                            (xml-document-children (parse-xml source)))))
    (expect (mapcar #'spans-kind (cl-xml-kit::span-element-children root))
            :to-equal '(:text))
    (expect (cl-xml-kit::span-text-entity-names
             (first (cl-xml-kit::span-element-children root)))
            :to-equal '("m"))
    (expect (cl-xml-kit::span-document-markup-entity-names document)
            :to-equal '("m"))
    (expect (length (xml-element-children dom-root)) :to-equal 1)
    (expect (xml-element-p (first (xml-element-children dom-root))) :to-be-truthy)))

(it "lists the element names of ATTLIST declarations"
  (expect (cl-xml-kit::span-document-attlist-element-names
           (cl-xml-kit::scan-xml-spans +spans-fixture+))
          :to-equal '("item")))

(it "lists prefixed element names of ATTLIST declarations once each"
  (let ((source (concatenate 'string
                             "<!DOCTYPE r [<!ATTLIST p:r a CDATA #IMPLIED>"
                             "<!ATTLIST p:r b CDATA #IMPLIED><!ATTLIST s c CDATA #IMPLIED>"
                             "]><r/>")))
    (expect (spans-accepted-p source) :to-be-truthy)
    (expect (sort (copy-list (cl-xml-kit::span-document-attlist-element-names
                              (cl-xml-kit::scan-xml-spans source)))
                  #'string<)
            :to-equal '("p:r" "s"))))

(it "finds ATTLIST declarations that arrive through a parameter entity"
  (let ((source (concatenate 'string
                             "<!DOCTYPE r [<!ENTITY % decl \"<!ATTLIST q x CDATA #IMPLIED>\">"
                             " %decl;]><r/>")))
    (expect (spans-accepted-p source) :to-be-truthy)
    (expect (cl-xml-kit::span-document-attlist-element-names
             (cl-xml-kit::scan-xml-spans source))
            :to-equal '("q"))))

(it "lists no ATTLIST element names for a document without a subset"
  (expect (cl-xml-kit::span-document-attlist-element-names
           (cl-xml-kit::scan-xml-spans "<a/>"))
          :to-equal nil))

;;; Failure modes

(it "rejects a source that is not a string with an input error"
  (expect (typep (spans-scan-condition 42) 'xml-input-error) :to-be-truthy))

(it "rejects a mismatched end tag"
  (expect (typep (spans-scan-condition "<a></b>") 'xml-well-formedness-error)
          :to-be-truthy))

(it "rejects an element that is never closed"
  (expect (typep (spans-scan-condition "<a><b></b>") 'xml-well-formedness-error)
          :to-be-truthy))

(it "rejects misplaced top-level structure, which PARSE-XML also rejects"
  (dolist (source '("" "<?xml version=\"1.0\"?>" "<a/><b/>"
                    "<!DOCTYPE a><!DOCTYPE a><a/>" "<a><!DOCTYPE a></a>"
                    "<a/><![CDATA[x]]>" " <?xml version=\"1.0\"?><a/>"))
    (expect (spans-accepted-p source) :to-equal nil)
    (expect (typep (spans-scan-condition source) 'xml-well-formedness-error)
            :to-be-truthy)))

(it "applies the caller's resource limits to the normalized text"
  (expect (typep (spans-scan-condition
                  "<abc/>"
                  :resource-limits (make-xml-resource-limits :max-name-length 2))
                 'xml-resource-limit-exceeded)
          :to-be-truthy))

;;; Agreement with the DOM

(defun spans-lexical-qname (name)
  (let ((prefix (xml-name-prefix name)))
    (if prefix
        (format nil "~A:~A" prefix (xml-name-local-name name))
        (xml-name-local-name name))))

(defun spans-dom-tree (element)
  (list* (spans-lexical-qname (xml-element-name element))
         (mapcar (lambda (attribute) (spans-lexical-qname (xml-attribute-name attribute)))
                 (xml-element-attributes element))
         (length (xml-element-namespace-declarations element))
         (loop for child in (xml-element-children element)
               when (xml-element-p child)
                 collect (spans-dom-tree child))))

(defun spans-tree (element)
  (let ((attributes (cl-xml-kit::span-element-attributes element)))
    (list* (cl-xml-kit::span-element-qname element)
           (loop for attribute in attributes
                 unless (cl-xml-kit::span-attribute-declaration-p attribute)
                   collect (cl-xml-kit::span-attribute-qname attribute))
           (count-if #'cl-xml-kit::span-attribute-declaration-p attributes)
           (mapcar #'spans-tree (spans-child-elements element)))))

(defun spans-agreement (source)
  "Return the span element tree and the DOM element tree of SOURCE without a BOM."
  (list (spans-tree (cl-xml-kit::span-document-root (cl-xml-kit::scan-xml-spans source)))
        (spans-dom-tree (find-if #'xml-element-p
                                 (xml-document-children (parse-xml source))))))

(defparameter +spans-agreement-documents+
  (list "<root/>"
        "<root a=\"1\" b='2'>text<child/>tail<child2>x</child2></root>"
        "<root attr=\"&gt;&amp;&quot;&apos;&#9;&#10;&#13;\">&lt;&gt;&amp;&quot;&apos; &#x1F600;</root>"
        "<root xmlns=\"urn:default\" xmlns:p=\"urn:p1\" xmlns:a=\"urn:a\" a:id=\"root\"><p:child p:code=\"one\" xmlns:p=\"urn:p2\"><plain xmlns=\"\" a:kind=\"plain\"><p:leaf xmlns:p=\"urn:p3\"/></plain></p:child></root>"
        "<?before?><root><empty/><!--boundary--><![CDATA[cdata]]>text]]&gt;<?inside data?><!--tail--></root><?after data?>"
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><!DOCTYPE root><root xmlns:p=\"urn:p\"><p:item><![CDATA[x<y]]></p:item><?done value?></root>"
        "<!DOCTYPE root [<!ENTITY e \"text\">]><root a=\"&e;\">&e;<x>&e;</x></root>"
        "<a><b><c><d/></c><c/></b><b/></a>"
        (format nil "<a  x = \"1\"   y~%=~%'2'  ></a  >"))
  "Documents whose span element trees must equal their DOM element trees.")

(it "builds the same element tree as the DOM for representative documents"
  (dolist (source +spans-agreement-documents+)
    (destructuring-bind (spans dom) (spans-agreement source)
      (expect spans :to-equal dom))))

(it "builds the same element tree as the DOM for the fixture in every line-break style"
  (dolist (separator (list +spans-lf+ +spans-crlf+ +spans-cr+))
    (destructuring-bind (spans dom) (spans-agreement (spans-fixture-with separator))
      (expect spans :to-equal dom))))

;;; Structure and robustness over generated documents

(defun spans-structure-problems (source element)
  "Return a list of the span invariants that ELEMENT or its descendants violate."
  (let ((problems nil))
    (labels ((note (control &rest arguments)
               (push (apply #'format nil control arguments) problems))
             (check-element (element)
               (let ((start (cl-xml-kit::span-node-start element))
                     (end (cl-xml-kit::span-node-end element))
                     (start-tag-end (cl-xml-kit::span-element-start-tag-end element))
                     (content-start (cl-xml-kit::span-element-content-start element))
                     (content-end (cl-xml-kit::span-element-content-end element)))
                 (unless (char= (char source start) #\<)
                   (note "~A does not start with <" (cl-xml-kit::span-element-qname element)))
                 (unless (char= (char source (1- end)) #\>)
                   (note "~A does not end with >" (cl-xml-kit::span-element-qname element)))
                 (unless (<= start (cl-xml-kit::span-element-name-end element)
                             start-tag-end end)
                   (note "~A tag bounds are out of order" (cl-xml-kit::span-element-qname element)))
                 (let ((previous (cl-xml-kit::span-element-name-end element)))
                   (dolist (attribute (cl-xml-kit::span-element-attributes element))
                     (let ((quote-char (cl-xml-kit::span-attribute-quote-char attribute)))
                       (unless (and (<= previous (cl-xml-kit::span-attribute-space-start attribute)
                                        (cl-xml-kit::span-attribute-name-start attribute)
                                        (cl-xml-kit::span-attribute-name-end attribute)
                                        (cl-xml-kit::span-attribute-value-start attribute))
                                    (< (cl-xml-kit::span-attribute-value-start attribute)
                                       (cl-xml-kit::span-attribute-value-end attribute)
                                       (1+ start-tag-end))
                                    (< (cl-xml-kit::span-attribute-space-start attribute)
                                       (cl-xml-kit::span-attribute-name-start attribute))
                                    (char= (char source (cl-xml-kit::span-attribute-value-start attribute))
                                           quote-char)
                                    (char= (char source (1- (cl-xml-kit::span-attribute-value-end attribute)))
                                           quote-char))
                         (note "attribute ~A has bad bounds"
                               (cl-xml-kit::span-attribute-qname attribute)))
                       (setf previous (cl-xml-kit::span-attribute-value-end attribute)))))
                 (if (cl-xml-kit::span-element-empty-p element)
                     (unless (and (= content-start content-end)
                                  (string= "/>" source :start2 content-start
                                                       :end2 (+ content-start 2))
                                  (= end start-tag-end (+ content-start 2))
                                  (null (cl-xml-kit::span-element-end-tag-start element)))
                       (note "empty element ~A has bad bounds"
                             (cl-xml-kit::span-element-qname element)))
                     (let ((end-tag-start (cl-xml-kit::span-element-end-tag-start element)))
                       (unless (and (= content-start start-tag-end)
                                    (= content-end end-tag-start)
                                    (<= content-start content-end)
                                    (string= "</" source :start2 end-tag-start
                                                         :end2 (+ end-tag-start 2))
                                    (= end (cl-xml-kit::span-element-end-tag-end element)))
                         (note "element ~A has bad content or end tag bounds"
                               (cl-xml-kit::span-element-qname element)))))
                 (let ((previous content-start)
                       (previous-kind nil))
                   (dolist (child (cl-xml-kit::span-element-children element))
                     (let ((kind (spans-kind child)))
                       (unless (and (<= previous (cl-xml-kit::span-node-start child))
                                    (< (cl-xml-kit::span-node-start child)
                                       (cl-xml-kit::span-node-end child))
                                    (<= (cl-xml-kit::span-node-end child) content-end))
                         (note "child of ~A is outside its content or out of order"
                               (cl-xml-kit::span-element-qname element)))
                       (when (and (eq kind :text) (eq previous-kind :text))
                         (note "adjacent text runs in ~A" (cl-xml-kit::span-element-qname element)))
                       (setf previous (cl-xml-kit::span-node-end child)
                             previous-kind kind)
                       (when (eq kind :element)
                         (check-element child))))))))
      (check-element element))
    (nreverse problems)))

(defparameter +spans-fragments+
  '("&amp;" "&#65;" "<!-- c -->" "<![CDATA[x]]>" "<q/>" "<q a='1'>t</q>" " z=\"1\""
    "<?p d?>" "text" "&e;" " " "  ")
  "Fragments the generated documents splice in.")

(defun spans-step (state limit)
  (setf (car state) (mod (+ (* 1103515245 (car state)) 12345) 2147483648))
  (mod (ash (car state) -8) limit))

(defun spans-mutate (state source)
  (let ((result source))
    (loop repeat (1+ (spans-step state 3))
          do (let ((index (spans-step state (1+ (length result)))))
               (setf result
                     (ecase (spans-step state 4)
                       (0 (concatenate 'string (subseq result 0 index)
                                       (nth (spans-step state (length +spans-fragments+))
                                            +spans-fragments+)
                                       (subseq result index)))
                       (1 (concatenate 'string (subseq result 0 index)
                                       (subseq result (min (length result)
                                                           (+ index 1 (spans-step state 5))))))
                       (2 (let ((break (position #\Linefeed result :start index)))
                            (if break
                                (concatenate 'string (subseq result 0 break)
                                             +spans-crlf+ (subseq result (1+ break)))
                                result)))
                       (3 (let ((break (position #\Linefeed result :start index)))
                            (if break
                                (concatenate 'string (subseq result 0 break)
                                             +spans-cr+ (subseq result (1+ break)))
                                result)))))))
    result))

(defvar *spans-generated-documents* nil)

(defun spans-generated-documents ()
  "Return fixed-seed mutations, of the agreement documents and the fixture, that PARSE-XML accepts."
  (or *spans-generated-documents*
      (setf *spans-generated-documents*
            (let ((state (list 20260704))
                  (bases (append +spans-agreement-documents+
                                 (list (spans-fixture-with +spans-lf+)
                                       (spans-fixture-with +spans-crlf+))))
                  (accepted nil))
              (loop repeat 1500
                    do (let ((source (spans-mutate
                                      state (nth (spans-step state (length bases)) bases))))
                         (when (spans-accepted-p source)
                           (push source accepted))))
              (nreverse accepted)))))

(it "generates enough accepted mutations to exercise the scanner"
  (expect (>= (length (spans-generated-documents)) 150) :to-be-truthy))

(it "keeps every span of the fixture and the generated documents consistent"
  (dolist (source (cons +spans-fixture+ (spans-generated-documents)))
    (let ((document (cl-xml-kit::scan-xml-spans source)))
      (expect (spans-structure-problems source (cl-xml-kit::span-document-root document))
              :to-equal nil))))

(it "matches the DOM element tree for generated documents without markup entities"
  (dolist (source (spans-generated-documents))
    (unless (cl-xml-kit::span-document-markup-entity-names (cl-xml-kit::scan-xml-spans source))
      (destructuring-bind (spans dom) (spans-agreement source)
        (expect spans :to-equal dom)))))

(it "scans generated documents behind a leading U+FEFF at shifted offsets"
  (dolist (source (spans-generated-documents))
    (let ((plain (cl-xml-kit::span-document-root (cl-xml-kit::scan-xml-spans source)))
          (marked (cl-xml-kit::span-document-root
                   (cl-xml-kit::scan-xml-spans (concatenate 'string +spans-bom+ source)))))
      (expect (list (- (cl-xml-kit::span-node-start marked) (cl-xml-kit::span-node-start plain))
                    (- (cl-xml-kit::span-node-end marked) (cl-xml-kit::span-node-end plain)))
              :to-equal '(1 1)))))
