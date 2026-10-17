;;;; t/helpers-format.lisp
;;;;
;;;; Fixtures and content extraction for the FORMAT-XML tests. The fixture
;;;; documents are kept here, not borrowed from the span and edit tests, so that
;;;; the format tests do not depend on how those files name their helpers.

(in-package #:cl-xml-kit/test)

(defparameter +format-lf+ (string #\Linefeed))
(defparameter +format-crlf+ (coerce '(#\Return #\Linefeed) 'string))
(defparameter +format-cr+ (string #\Return))
(defparameter +format-bom+ (string (code-char #xFEFF)))

(defun format-join (separator lines)
  "Return LINES joined with SEPARATOR between them; a non-string line signals a type error."
  (with-output-to-string (stream)
    (loop for (line . more) on lines
          do (write-string line stream)
             (when more
               (write-string separator stream)))))

(defun format-lines (break &rest lines)
  "Return LINES joined with the line break BREAK and ended with BREAK, as FORMAT-XML ends its output.
A non-string line signals a type error."
  (concatenate 'string (format-join break lines) break))

(defparameter +format-source-lines+
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

(defparameter +format-expected-lines+
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

(defun format-source (break &optional bom)
  "Return the source lines joined with BREAK, behind a U+FEFF when BOM is true; it never fails."
  (concatenate 'string (if bom +format-bom+ "") (format-join break +format-source-lines+)))

(defparameter +format-span-fixture-lines+
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
  "A document with an internal subset, a markup entity, attributes laid out over
several lines, and a multi-line comment; the span tests use the same lines.")

(defparameter +format-edit-fixture-lines+
  '("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<!DOCTYPE root ["
    "  <!ENTITY e \"text\">"
    "  <!ENTITY m \"<b/>\">"
    "  <!ATTLIST governed id ID #IMPLIED>"
    "]>"
    "<!-- top comment -->"
    "<?pi data?>"
    "<root xmlns=\"urn:default\" xmlns:p=\"urn:p\" xmlns:q=\"urn:p\" a='single' b=\"double\">"
    "  <item id=\"1\" name='n'>text &amp; &#65; &e; <![CDATA[<raw>]]></item>"
    "  <p:item xmlns:p=\"urn:other\" p:code=\"7\"/>"
    "  <list>"
    "    <entry"
    "        id=\"e1\""
    "        label='first'"
    "        state=\"on\"/>"
    "  </list>"
    "  <bare/>"
    "  <spaced />"
    "  <dup p:flag=\"1\"/>"
    "  <governed id=\"g\"/>"
    "  <holder x=\"1\">&m;</holder>"
    "  <cdata><![CDATA[a < b]]></cdata>"
    "  <refs>&#65;&amp;&e;</refs>"
    "  <blank>   </blank>"
    "  <nested><inner>x</inner></nested>"
    "  <noted><!-- c --></noted>"
    "  <!-- inner comment -->"
    "  <?inner pi?>"
    "</root>"
    "<!-- trailing -->")
  "A document with a markup entity reference in content, two prefixes for one URI,
and an attribute list laid out one attribute per line; the edit tests use the
same lines.")

(defun format-fixtures ()
  "Return the fixture documents of the idempotence and content checks; it never fails."
  (list (format-source +format-lf+) (format-source +format-crlf+ t) (format-source +format-cr+)
        (concatenate 'string +format-bom+ (format-join +format-crlf+ +format-span-fixture-lines+))
        (concatenate 'string +format-bom+ (format-join +format-crlf+ +format-edit-fixture-lines+))
        "<a><b/><c/></a>" "<p>x <b>y</b></p>" "<r xml:space=\"preserve\"> <a/> </r>"))

(defun format-content (source)
  "Return the comments, processing instructions, element names, attributes, and non-whitespace text of SOURCE in document order.
A SOURCE that does not parse signals the parse condition."
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
      (walk (parse-xml (string-left-trim +format-bom+ source))))
    (nreverse items)))

(defun format-generated-source (raw break prolog pretty bom)
  "Return a source document for the format property: the raw generated document RAW,
written as source or, when PRETTY, pretty printed by the serializer, every line feed
replaced by BREAK. PROLOG :DECLARATION, :DOCTYPE, or :BOTH puts an XML declaration,
a DOCTYPE naming an unprefixed root and declaring an entity, or both in front; BOM true puts
a U+FEFF before everything. RAW must come from GEN-DOCUMENT-SPEC; another shape
signals a destructuring error."
  (let* ((spec (generated-document raw :source t))
         (root (find :element (rest spec) :key #'first))
         (prefix (getf (cddr root) :prefix))
         (lines (append (and (member prolog '(:declaration :both)) (list "<?xml version='1.0'?>"))
                        ;; PARSE-XML refuses a DOCTYPE whose name has a colon, so
                        ;; a prefixed root goes without one.
                        (and (member prolog '(:doctype :both)) (null prefix)
                             (list (format nil "<!DOCTYPE ~A [<!ENTITY e 'x'>]>"
                                           (second root))))
                        (list (if pretty
                                  (xml->string (node-from-spec spec) :pretty-print t)
                                  (render-spec spec))))))
    (with-output-to-string (stream)
      (when bom
        (write-string +format-bom+ stream))
      (loop for character across (format-join +format-lf+ lines)
            do (if (char= character #\Linefeed)
                   (write-string break stream)
                   (write-char character stream))))))

(defun format-cause-limit-kind (condition)
  "Return the resource-limit kind of the cause of the XML-EDIT-VALUE-ERROR CONDITION.
A condition without a resource-limit cause signals NO-APPLICABLE-METHOD or a type
error, which fails the test that reads it."
  (xml-resource-limit-exceeded-kind (xml-edit-value-error-cause condition)))
