(in-package #:cl-xml-kit/test)

;;; Agreement with the DOM

(defun spans-lexical-qname (name)
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
sees the same documents. The generated span tests assert it holds at least 200;
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

(defun spans-attribute-span-problems (source element)
  (let ((problems nil)
        (previous (cl-xml-kit::span-element-name-end element)))
    (dolist (attribute (cl-xml-kit::span-element-attributes element))
      (when (spans-attribute-problem-p
             source attribute previous
             (cl-xml-kit::span-element-start-tag-end element))
        (push (format nil "an attribute of ~A has bad bounds"
                      (cl-xml-kit::span-element-qname element))
              problems))
      (setf previous (cl-xml-kit::span-attribute-value-end attribute)))
    (nreverse problems)))

(defun spans-child-span-problems (source element)
  (loop with previous = (cl-xml-kit::span-element-content-start element)
        with previous-kind = nil
        for child in (cl-xml-kit::span-element-children element)
        for kind = (spans-kind child)
        append (let ((problems nil))
                 (unless (<= previous
                             (cl-xml-kit::span-node-start child)
                             (1- (cl-xml-kit::span-node-end child))
                             (1- (cl-xml-kit::span-element-content-end element)))
                   (push (format nil "a child of ~A is outside its content or out of order"
                                 (cl-xml-kit::span-element-qname element))
                         problems))
                 (when (and (eq kind :text) (eq previous-kind :text))
                   (push (format nil "~A has adjacent text runs"
                                 (cl-xml-kit::span-element-qname element))
                         problems))
                 (setf previous (cl-xml-kit::span-node-end child)
                       previous-kind kind)
                 (setf problems (nreverse problems))
                 (if (eq kind :element)
                     (nconc problems (spans-structure-problems source child))
                     problems))))

(defun spans-structure-problems (source element)
  "Return a description of each span invariant that ELEMENT or a descendant violates in SOURCE, or NIL.
The invariants: tags start with < and end with >, bounds are ordered, attribute
values are quoted, children lie in order inside the content, and no two text runs
are adjacent. A violation is reported, never signalled, so the caller can name
every failing document at once."
  (append
   (when (spans-tag-problem-p source element)
     (list (format nil "~A has bad tag or content bounds"
                   (cl-xml-kit::span-element-qname element))))
   (spans-attribute-span-problems source element)
   (spans-child-span-problems source element)))

(defun spans-shift-problems (source)
  "Return the (START END) offsets by which a leading U+FEFF moves the root span of SOURCE, or NIL when both are 1."
  (let* ((plain (spans-root source))
         (marked (spans-root (concatenate 'string +spans-bom+ source)))
         (shift (list (- (cl-xml-kit::span-node-start marked) (cl-xml-kit::span-node-start plain))
                      (- (cl-xml-kit::span-node-end marked) (cl-xml-kit::span-node-end plain)))))
    (unless (equal shift '(1 1))
      shift)))
