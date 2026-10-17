;;;; t/helpers-corpus.lisp
;;;;
;;;; Generated documents: the fixed-seed mutation corpus, the property-test
;;;; element trees, and the span invariants checked over both.

(in-package #:cl-xml-kit/test)

;;; Agreement with the DOM

(defun spans-lexical-qname (name)
  "Return the XML-NAME NAME as written, prefix included."
  (let ((prefix (xml-name-prefix name)))
    (if prefix
        (format nil "~A:~A" prefix (xml-name-local-name name))
        (xml-name-local-name name))))

(defun spans-dom-tree (element)
  "Return (QNAME ATTRIBUTE-QNAMES DECLARATION-COUNT . CHILD-TREES) of the DOM ELEMENT."
  (list* (spans-lexical-qname (xml-element-name element))
         (mapcar (lambda (attribute) (spans-lexical-qname (xml-attribute-name attribute)))
                 (xml-element-attributes element))
         (length (xml-element-namespace-declarations element))
         (loop for child in (xml-element-children element)
               when (xml-element-p child)
                 collect (spans-dom-tree child))))

(defun spans-tree (element)
  "Return the tree of the span ELEMENT in the shape SPANS-DOM-TREE gives a DOM element."
  (let ((attributes (cl-xml-kit::span-element-attributes element)))
    (list* (cl-xml-kit::span-element-qname element)
           (loop for attribute in attributes
                 unless (cl-xml-kit::span-attribute-declaration-p attribute)
                   collect (cl-xml-kit::span-attribute-qname attribute))
           (count-if #'cl-xml-kit::span-attribute-declaration-p attributes)
           (mapcar #'spans-tree (spans-child-elements element)))))

(defun spans-agreement (source)
  "Return the span element tree and the DOM element tree of SOURCE.
SOURCE must not start with U+FEFF, which PARSE-XML rejects.
A source that does not parse signals its parse condition into the caller."
  (list (spans-tree (spans-root source))
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
  "Base documents of the generated corpus, also checked one by one against the DOM.")

(defun spans-agreement-rows ()
  "Return an (DISPLAY SOURCE) table row for each agreement document.
DISPLAY spells each line feed of SOURCE as ~% so that a test name built from it
stays on one line."
  (mapcar (lambda (source)
            (list (with-output-to-string (stream)
                    (loop for character across source
                          do (if (char= character #\Linefeed)
                                 (write-string "~%" stream)
                                 (write-char character stream))))
                  source))
          +spans-agreement-documents+))

;;; Fixed-seed generated documents

(defparameter +spans-fragments+
  '("&amp;" "&#65;" "<!-- c -->" "<![CDATA[x]]>" "<q/>" "<q a='1'>t</q>" " z=\"1\""
    "<?p d?>" "text" "&e;" " " "  ")
  "Fragments the generated documents splice in: references, markup, an attribute,
and whitespace, so a mutation lands inside tags, content, and the prolog alike.")

(defun spans-replace-line-break (source index break)
  "Return SOURCE with its first line feed at or after INDEX replaced by BREAK, or SOURCE when there is none."
  (let ((position (position #\Linefeed source :start index)))
    (if position
        (concatenate 'string (subseq source 0 position) break (subseq source (1+ position)))
        source)))

(defun spans-mutate (state source)
  "Return SOURCE after one to three random insertions, deletions, or line-break rewrites.
STATE is the random state the choices are drawn from."
  (let ((result source))
    (loop repeat (1+ (random 3 state))
          do (let ((index (random (1+ (length result)) state)))
               (setf result
                     (ecase (random 4 state)
                       (0 (concatenate 'string (subseq result 0 index)
                                       (nth (random (length +spans-fragments+) state)
                                            +spans-fragments+)
                                       (subseq result index)))
                       (1 (concatenate 'string (subseq result 0 index)
                                       (subseq result (min (length result)
                                                           (+ index 1 (random 5 state))))))
                       (2 (spans-replace-line-break result index +spans-crlf+))
                       (3 (spans-replace-line-break result index +spans-cr+))))))
    result))

(defvar *spans-generated-documents* nil)

(defun spans-generated-documents ()
  "Return the fixed-seed mutations, of the agreement documents and the fixture, that PARSE-XML accepts.
The corpus is built once from the literal seed, so every run and every caller
sees the same documents. The generated span tests assert it holds at least 150;
callers that take the first 200 signal a bounds error if it ever holds fewer."
  (or *spans-generated-documents*
      (setf *spans-generated-documents*
            (let ((state (sb-ext:seed-random-state 20260704))
                  (bases (append +spans-agreement-documents+
                                 (list (spans-fixture-with +spans-lf+)
                                       (spans-fixture-with +spans-crlf+)))))
              (loop repeat 1500
                    for source = (spans-mutate state (nth (random (length bases) state) bases))
                    when (spans-accepted-p source)
                      collect source)))))

;;; Span invariants

(defun spans-attribute-problem-p (source attribute previous start-tag-end)
  "Return true when ATTRIBUTE, written after index PREVIOUS, has bounds out of order, quotes missing, or a value ending after START-TAG-END."
  (let ((quote-char (cl-xml-kit::span-attribute-quote-char attribute))
        (space-start (cl-xml-kit::span-attribute-space-start attribute))
        (name-start (cl-xml-kit::span-attribute-name-start attribute))
        (value-start (cl-xml-kit::span-attribute-value-start attribute))
        (value-end (cl-xml-kit::span-attribute-value-end attribute)))
    (not (and (<= previous space-start name-start
                  (cl-xml-kit::span-attribute-name-end attribute) value-start)
              (< value-start value-end (1+ start-tag-end))
              (< space-start name-start)
              (char= (char source value-start) quote-char)
              (char= (char source (1- value-end)) quote-char)))))

(defun spans-tag-problem-p (source element)
  "Return true when the tag, content, and end tag bounds of the span ELEMENT disagree with SOURCE."
  (let ((start (cl-xml-kit::span-node-start element))
        (end (cl-xml-kit::span-node-end element))
        (start-tag-end (cl-xml-kit::span-element-start-tag-end element))
        (content-start (cl-xml-kit::span-element-content-start element))
        (content-end (cl-xml-kit::span-element-content-end element))
        (end-tag-start (cl-xml-kit::span-element-end-tag-start element)))
    (not (and (char= (char source start) #\<)
              (char= (char source (1- end)) #\>)
              (<= start (cl-xml-kit::span-element-name-end element) start-tag-end end)
              (if (cl-xml-kit::span-element-empty-p element)
                  (and (= content-start content-end)
                       (string= "/>" source :start2 content-start :end2 (+ content-start 2))
                       (= end start-tag-end (+ content-start 2))
                       (null end-tag-start))
                  (and (= content-start start-tag-end)
                       (eql content-end end-tag-start)
                       (<= content-start content-end)
                       (string= "</" source :start2 end-tag-start :end2 (+ end-tag-start 2))
                       (= end (cl-xml-kit::span-element-end-tag-end element))))))))

(defun spans-structure-problems (source element)
  "Return a description of each span invariant that ELEMENT or a descendant violates in SOURCE, or NIL.
The invariants: tags start with < and end with >, bounds are ordered, attribute
values are quoted, children lie in order inside the content, and no two text runs
are adjacent. A violation is reported, never signalled, so the caller can name
every failing document at once."
  (let ((problems nil))
    (labels ((note (control element)
               (push (format nil control (cl-xml-kit::span-element-qname element)) problems))
             (check-element (element)
               (when (spans-tag-problem-p source element)
                 (note "~A has bad tag or content bounds" element))
               (let ((previous (cl-xml-kit::span-element-name-end element)))
                 (dolist (attribute (cl-xml-kit::span-element-attributes element))
                   (when (spans-attribute-problem-p
                          source attribute previous
                          (cl-xml-kit::span-element-start-tag-end element))
                     (note "an attribute of ~A has bad bounds" element))
                   (setf previous (cl-xml-kit::span-attribute-value-end attribute))))
               (let ((previous (cl-xml-kit::span-element-content-start element))
                     (previous-kind nil))
                 (dolist (child (cl-xml-kit::span-element-children element))
                   (unless (<= previous (cl-xml-kit::span-node-start child)
                               (1- (cl-xml-kit::span-node-end child))
                               (1- (cl-xml-kit::span-element-content-end element)))
                     (note "a child of ~A is outside its content or out of order" element))
                   (when (and (eq (spans-kind child) :text) (eq previous-kind :text))
                     (note "~A has adjacent text runs" element))
                   (setf previous (cl-xml-kit::span-node-end child)
                         previous-kind (spans-kind child))
                   (when (eq previous-kind :element)
                     (check-element child))))))
      (check-element element))
    (nreverse problems)))

(defun spans-shift-problems (source)
  "Return the (START END) offsets by which a leading U+FEFF moves the root span of SOURCE, or NIL when both are 1."
  (let* ((plain (spans-root source))
         (marked (spans-root (concatenate 'string +spans-bom+ source)))
         (shift (list (- (cl-xml-kit::span-node-start marked) (cl-xml-kit::span-node-start plain))
                      (- (cl-xml-kit::span-node-end marked) (cl-xml-kit::span-node-end plain)))))
    (unless (equal shift '(1 1))
      shift)))

;;; Generated element trees for property tests

(defun corpus-escape (text quote-char)
  "Return TEXT with &, <, >, and QUOTE-CHAR (when non-NIL) written as references."
  (with-output-to-string (stream)
    (loop for character across text
          do (case character
               (#\& (write-string "&amp;" stream))
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (t (if (eql character quote-char)
                      (write-string (if (char= quote-char #\") "&quot;" "&apos;") stream)
                      (write-char character stream)))))))

;;; Tag whitespace includes a line break so that CRLF and CR rendering moves
;;; offsets inside start tags, between attributes, and around the equals sign.
(defparameter +corpus-tag-spaces+ (list " " "  " (format nil "~%  ")))
(defparameter +corpus-equals+ (list "=" " = " (format nil "~%=")))

(defun corpus-gen-attributes ()
  "Generate up to three distinct attributes as (QNAME VALUE QUOTE-CHAR SPACE EQUALS) lists.
The names are fixed and each is present or absent, so no two can collide; the
value alphabet holds both quotes, & and <, a line feed, and a non-ASCII letter."
  (gen-map (lambda (choices)
             (loop for name in '("k" "p:m" "id")
                   for (present value quote-p space equals) in choices
                   when present
                     collect (list name value (if quote-p #\" #\') space equals)))
           (apply #'gen-tuple
                  (loop repeat 3
                        collect (gen-tuple (gen-boolean)
                                           (gen-string :max-length 6
                                                       :alphabet (format nil "a \"'&<~%~C"
                                                                         (code-char #xE9)))
                                           (gen-boolean)
                                           (gen-member +corpus-tag-spaces+)
                                           (gen-member +corpus-equals+))))))

(defun corpus-gen-leaf ()
  "Generate a text, comment, processing instruction, or CDATA node, or an empty element.
Comment and processing instruction alphabets omit - and ? so no delimiter can
appear early; the CDATA alphabet omits >, so ]]> cannot."
  (gen-one-of
   (gen-map (lambda (text) (list :text text))
            (gen-string :min-length 1 :max-length 6 :alphabet (format nil "xy &<>]~%")))
   (gen-map (lambda (text) (list :comment text)) (gen-string :max-length 4 :alphabet "c "))
   (gen-map (lambda (text) (list :pi text)) (gen-string :max-length 4 :alphabet "d "))
   (gen-map (lambda (text) (list :cdata text)) (gen-string :max-length 4 :alphabet "x<&]"))
   (gen-map (lambda (name) (list :element name nil nil))
            (gen-member '("a" "p:c" "x.y")))))

(defun corpus-gen-element (child)
  "Generate an element whose children come from the generator CHILD."
  (gen-map (lambda (parts) (cons :element parts))
           (gen-tuple (gen-member '("a" "b" "p:c" "d-e" "x.y"))
                      (corpus-gen-attributes)
                      (gen-list child :max-length 3))))

(defun corpus-gen-document ()
  "Generate a document element tree with at most four levels of elements, and a line break style."
  (gen-tuple (corpus-gen-element
              (gen-recursive (corpus-gen-leaf) #'corpus-gen-element :max-depth 2))
             (gen-member '(:lf :crlf :cr))))

(defun corpus-render-node (node stream)
  "Write NODE, a generated tree node, to STREAM as XML, leaving line breaks as line feeds for CORPUS-RENDER to convert."
  (destructuring-bind (kind &rest parts) node
    (ecase kind
      (:text (write-string (corpus-escape (first parts) nil) stream))
      (:comment (format stream "<!--~A-->" (first parts)))
      (:pi (format stream "<?pi ~A?>" (first parts)))
      (:cdata (format stream "<![CDATA[~A]]>" (first parts)))
      (:element
       (destructuring-bind (name attributes children) parts
         (format stream "<~A" name)
         (loop for (qname value quote-char space equals) in attributes
               do (format stream "~A~A~A~C~A~C" space qname equals quote-char
                          (corpus-escape value quote-char) quote-char))
         (if children
             (progn (write-char #\> stream)
                    (dolist (child children) (corpus-render-node child stream))
                    (format stream "</~A>" name))
             (write-string "/>" stream)))))))

(defun corpus-render (document)
  "Return the generated DOCUMENT, (TREE STYLE), as an XML string with STYLE line breaks.
The document element declares the prefix p that generated names use."
  (destructuring-bind (tree style) document
    (destructuring-bind (name attributes children) (rest tree)
      (let ((break (spans-line-break style)))
        (with-output-to-string (out)
          (loop for character
                  across (with-output-to-string (stream)
                           (corpus-render-node
                            (list :element name
                                  (cons (list "xmlns:p" "urn:p" #\" " " "=") attributes)
                                  children)
                            stream))
                do (if (char= character #\Linefeed)
                       (write-string break out)
                       (write-char character out))))))))
