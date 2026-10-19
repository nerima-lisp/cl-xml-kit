;;;; t/helpers-spans.lisp
;;;;
;;;; Fixtures and accessors shared by the span, path, edit, and format tests.

(in-package #:cl-xml-kit/test)

(declaim (ftype (function (t &rest t) t) spans-scan))

(defparameter +spans-bom+ (string (code-char #xFEFF)))
(defparameter +spans-crlf+ (coerce '(#\Return #\Linefeed) 'string))
(defparameter +spans-lf+ (string #\Linefeed))
(defparameter +spans-cr+ (string #\Return))

(defun spans-join (separator lines)
  "Return LINES joined by SEPARATOR, with no separator after the last line."
  (with-output-to-string (stream)
    (loop for (line . more) on lines
          do (write-string line stream)
             (when more
               (write-string separator stream)))))

(defun spans-line-break (style)
  "Return the line break string of STYLE, :LF, :CRLF, or :CR.
Any other STYLE signals an ECASE error, so a mistyped table row fails its test."
  (ecase style
    (:lf +spans-lf+)
    (:crlf +spans-crlf+)
    (:cr +spans-cr+)))

(defparameter +spans-line-tokens+
  (list (cons "{cr}" +spans-cr+) (cons "{lf}" +spans-lf+)
        (cons "{tab}" (string #\Tab)))
  "The tokens SPANS-LINES expands to a single control character.")

(defun spans-lines (template &optional (style :lf))
  "Return TEMPLATE with each ~% replaced by the line break of STYLE, and {cr}, {lf}, and {tab} by that character.
Literal table rows can therefore spell control characters."
  (let ((break (spans-line-break style)))
    (with-output-to-string (stream)
      (loop with index = 0
            while (< index (length template))
            do (let ((token (find-if (lambda (token)
                                       (string= (car token) template
                                                :start2 index
                                                :end2 (min (length template)
                                                           (+ index (length (car token))))))
                                     +spans-line-tokens+)))
                 (cond
                   ((string= "~%" template :start2 index
                                           :end2 (min (length template) (+ index 2)))
                    (write-string break stream)
                    (incf index 2))
                   (token
                    (write-string (cdr token) stream)
                    (incf index (length (car token))))
                   (t
                    (write-char (char template index) stream)
                    (incf index))))))))

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
before and after the root, attributes in both quote styles and on continuation
lines, and a redeclared prefix.")

(defparameter +spans-fixture+
  (concatenate 'string +spans-bom+ (spans-join +spans-crlf+ +spans-fixture-lines+))
  "The fixture with a leading U+FEFF and CRLF line breaks.")

(defun spans-fixture-with (separator)
  "Return the fixture lines joined by SEPARATOR, without a U+FEFF."
  (spans-join separator +spans-fixture-lines+))

(defun spans-scan (source &rest options)
  "Return the span document of SOURCE; a scan failure propagates to the test."
  (cl-weave:with-continuation-result (document success)
      (apply #'cl-xml-kit::scan-xml-spans source #'success #'error options)
    document))

(defun spans-root (source)
  "Return the span element of the document element of SOURCE."
  (cl-xml-kit::span-document-root (spans-scan source)))

(defun spans-slice (source node)
  "Return the text of SOURCE that the span NODE covers."
  (subseq source (cl-xml-kit::span-node-start node) (cl-xml-kit::span-node-end node)))

(defun spans-start-tag (source element)
  "Return the start tag of the span ELEMENT in SOURCE."
  (subseq source (cl-xml-kit::span-node-start element)
          (cl-xml-kit::span-element-start-tag-end element)))

(defun spans-content (source element)
  "Return the content of the span ELEMENT in SOURCE."
  (subseq source (cl-xml-kit::span-element-content-start element)
          (cl-xml-kit::span-element-content-end element)))

(defun spans-end-tag (source element)
  "Return the end tag of the span ELEMENT in SOURCE, or NIL for an empty element."
  (and (cl-xml-kit::span-element-end-tag-start element)
       (subseq source (cl-xml-kit::span-element-end-tag-start element)
               (cl-xml-kit::span-element-end-tag-end element))))

(defun spans-child-elements (element)
  (remove-if-not #'cl-xml-kit::span-element-p
                 (cl-xml-kit::span-element-children element)))

(defun spans-kind (node)
  "Return a keyword naming the span record type of NODE, :OTHER for anything else."
  (typecase node
    (cl-xml-kit::span-element :element)
    (cl-xml-kit::span-text :text)
    (cl-xml-kit::span-comment :comment)
    (cl-xml-kit::span-processing-instruction :processing-instruction)
    (cl-xml-kit::span-doctype :doctype)
    (t :other)))

(defun spans-fixture-element (qname)
  "Return the span element QNAME of the fixture: root, or a child element of root.
Signals an error when the fixture has no such element, so a fixture edit fails loudly."
  (let ((root (spans-root +spans-fixture+)))
    (or (if (string= qname "root")
            root
            (find qname (spans-child-elements root)
                  :key #'cl-xml-kit::span-element-qname :test #'string=))
        (error "The span fixture has no element ~S." qname))))

(defun spans-attribute (element qname)
  "Return the span attribute of ELEMENT written as QNAME.
Signals an error when there is none, so a missing attribute fails the test."
  (or (find qname (cl-xml-kit::span-element-attributes element)
            :key #'cl-xml-kit::span-attribute-qname :test #'string=)
      (error "The span element has no attribute ~S." qname)))

(defun spans-attribute-text (source attribute)
  "Return the name, the value including quotes, and the text from the whitespace before the name through the closing quote of ATTRIBUTE."
  (list (subseq source (cl-xml-kit::span-attribute-name-start attribute)
                (cl-xml-kit::span-attribute-name-end attribute))
        (subseq source (cl-xml-kit::span-attribute-value-start attribute)
                (cl-xml-kit::span-attribute-value-end attribute))
        (subseq source (cl-xml-kit::span-attribute-space-start attribute)
                (cl-xml-kit::span-attribute-value-end attribute))))

(defun spans-text-flags (run)
  "Return the CDATA-P, SINGLE-CDATA-P, CHAR-REF-P, PREDEFINED-REF-P, and ENTITY-NAMES of the span text RUN."
  (list (cl-xml-kit::span-text-cdata-p run)
        (cl-xml-kit::span-text-single-cdata-p run)
        (cl-xml-kit::span-text-char-ref-p run)
        (cl-xml-kit::span-text-predefined-ref-p run)
        (cl-xml-kit::span-text-entity-names run)))

(defun spans-accepted-p (source &rest options)
  "Return true when PARSE-XML accepts SOURCE, NIL when it signals an XML-KIT-ERROR.
A condition that is not an XML-KIT-ERROR propagates and fails the caller."
  (handler-case (progn (apply #'parse-xml source options) t)
    (xml-kit-error () nil)))
