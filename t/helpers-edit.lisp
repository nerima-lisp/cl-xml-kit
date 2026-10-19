;;;; t/helpers-edit.lisp
;;;;
;;;; The edit fixture, expected-result builders, condition readers, and
;;;; matchers shared by the verify, fragment, and edit tests.

(in-package #:cl-xml-kit/test)

(defparameter +edit-lines+
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
  "A document with a declaration, a DOCTYPE with an internal subset, comments,
processing instructions, CDATA, references, both quote styles, default,
prefixed, and redeclared namespaces, and attributes laid out on one line and on
separate lines. The prefixes p and q name the same URI on root, governed is
named in an ATTLIST declaration, and holder references the markup entity m.")

(defparameter +edit-source+
  (concatenate 'string +spans-bom+ (spans-join +spans-crlf+ +edit-lines+))
  "The edit fixture with a leading U+FEFF and CRLF line breaks.")

(defun edit-range (needle &key (offset 0) length)
  "Return the start and end in +EDIT-SOURCE+ of LENGTH characters at OFFSET into the one occurrence of NEEDLE.
LENGTH defaults to the rest of NEEDLE. Signals an error when NEEDLE occurs zero or
several times, so an ambiguous expectation fails instead of pointing elsewhere."
  (let ((start (search needle +edit-source+)))
    (unless (and start (null (search needle +edit-source+ :start2 (1+ start))))
      (error "~S does not occur exactly once in the edit fixture." needle))
    (values (+ start offset)
            (+ start offset (or length (- (length needle) offset))))))

(defun edit-expected (needle replacement &key (offset 0) length)
  "Return +EDIT-SOURCE+ with the range EDIT-RANGE chooses replaced by REPLACEMENT.
REPLACEMENT is expanded by SPANS-LINES with CRLF line breaks."
  (multiple-value-bind (start end) (edit-range needle :offset offset :length length)
    (concatenate 'string (subseq +edit-source+ 0 start) (spans-lines replacement :crlf)
                 (subseq +edit-source+ end))))

(defun edit-expected-between (first-needle last-needle replacement)
  "Return +EDIT-SOURCE+ with everything from the first FIRST-NEEDLE through the last LAST-NEEDLE replaced.
REPLACEMENT is inserted as it is, unlike in EDIT-EXPECTED. Signals an error when
either needle is missing."
  (let ((start (search first-needle +edit-source+))
        (last (search last-needle +edit-source+ :from-end t)))
    (unless (and start last)
      (error "~S or ~S does not occur in the edit fixture." first-needle last-needle))
    (concatenate 'string (subseq +edit-source+ 0 start) replacement
                 (subseq +edit-source+ (+ last (length last-needle))))))

(defun edit-context (source &key (path "/root") (resource-limits *default-xml-resource-limits*)
                                 (external-entity-policy :reject))
  "Return the path context built for SOURCE and PATH.
A SOURCE that does not parse signals its parse condition into the caller."
  (cl-weave:with-continuation-result (context success)
      (cl-xml-kit::%path-context source path #'success #'error
                                 :resource-limits resource-limits
                                 :external-entity-policy external-entity-policy)
    context))
(defun edit-verify-result (context candidate expected &rest options)
  "Return CANDIDATE or signal the condition delivered by the verification callbacks."
  (cl-weave:with-continuation-result (result success)
      (apply #'cl-xml-kit::%edit-verify context candidate expected #'success #'error options)
    result))
(defun edit-finish-result (context start end replacement expected &rest options)
  "Return the checked source or signal the condition delivered by the edit callbacks."
  (cl-weave:with-continuation-result (result success)
      (apply #'cl-xml-kit::%edit-finish
             context start end replacement expected #'success #'error options)
    result))

(defun edit-chain (source)
  "Return the DOM elements of SOURCE from the innermost outward, following the first element child at each level."
  (let ((chain nil)
        (element (find-if #'xml-element-p (xml-document-children (parse-xml source)))))
    (loop while element
          do (push element chain)
             (setf element (find-if #'xml-element-p (xml-element-children element))))
    chain))

(defun edit-cause-type (condition)
  "Return the type of the cause of the XML-EDIT-VALUE-ERROR CONDITION, NIL when it has none."
  (let ((cause (xml-edit-value-error-cause condition)))
    (and cause (type-of cause))))

(defun edit-cause-limit-kind (condition)
  "Return the limit kind of the XML-RESOURCE-LIMIT-EXCEEDED cause of CONDITION.
A cause of another type signals a slot or type error, so the test fails."
  (xml-resource-limit-exceeded-kind (xml-edit-value-error-cause condition)))

(defun edit-namespace-of (source path)
  "Return the namespace URI of the element that the first match of PATH in SOURCE belongs to.
Signals an error when PATH matches nothing."
  (let ((match (or (first (path-select source path))
                   (error "~S matches nothing in the edited document." path))))
    (xml-name-namespace-uri (xml-element-name (xml-match-element match)))))

(defmatcher :to-select-value (source expected)
  (:description "Passes when the first match of PATH in the XML string SOURCE has the "
                "decoded value VALUE under EQUAL, where EXPECTED is (PATH VALUE). A "
                "path that matches nothing compares as :NO-MATCH.")
  (destructuring-bind (path value) expected
    (let* ((match (first (path-select source path)))
           (actual (if match (xml-match-value match) :no-match)))
      (values (equal actual value) actual value))))

(defun edit-changed-range (source result)
  "Return the index in SOURCE where RESULT first differs and the length of the suffix they share."
  (values (or (mismatch source result) (length source))
          (let ((mismatch (mismatch source result :from-end t)))
            (if mismatch (- (length source) mismatch) (length source)))))

(defun edit-element-matches (source)
  "Return the element matches of SOURCE at every depth, in document order.
The children of an element that references a markup entity are skipped, since
SELECT-XML refuses to enumerate them; every other condition propagates."
  (let ((matches nil))
    (labels ((walk (path)
               (dolist (match (handler-case (path-select source path)
                                (xml-unsupported-edit () nil)))
                 (push match matches)
                 (walk (concatenate 'string (xml-match-path match) "/*")))))
      (walk "/*"))
    (nreverse matches)))

(defun edit-equivalent-p (expected actual &key (lenient :none) (external-entity-policy :reject))
  "Return whether the parse of ACTUAL is equivalent to that of EXPECTED.
LENIENT is the lenient ordinals of %DOM-DOCUMENT-EQUIVALENT-P: :NONE, NIL for the
document element, or ordinals counted from it. Either string failing to parse
signals its parse condition into the caller."
  (cl-xml-kit::%dom-document-equivalent-p
   (parse-xml expected :external-entity-policy external-entity-policy)
   (parse-xml actual :external-entity-policy external-entity-policy)
   lenient))

(defparameter +fragment-source+
  "<target xmlns=\"urn:d\" xmlns:p=\"urn:p\"><inner/></target>"
  "A document whose innermost element has a default and a prefixed namespace in scope.")

(defun fragment-element (fragment &key (resource-limits *default-xml-resource-limits*)
                                    (external-entity-policy :reject))
  "Parse FRAGMENT inside <inner/>, the innermost element of +FRAGMENT-SOURCE+, as an edit would.
The context carries the path /target. Return the element and FRAGMENT without
the XML whitespace around it; a refusal signals into the caller."
  (cl-weave:with-continuation-values (result success)
      (cl-xml-kit::%edit-fragment-element
       (edit-context +fragment-source+ :path "/target" :resource-limits resource-limits
                                       :external-entity-policy external-entity-policy)
       (edit-chain +fragment-source+)
       fragment #'success #'error)
    (values-list result)))

(defun edit-apply (operation source path value &rest options)
  "Call the public edit that OPERATION names, capturing its success callback.
VALUE is passed to :SET and :APPEND but not to :DELETE. An unknown OPERATION
signals an ECASE error, so a mistyped table row fails."
  (ecase operation
    (:set
     (cl-weave:with-continuation-result (result success)
         (apply #'edit-xml-set source path value #'success #'error options)
       result))
    (:delete
     (cl-weave:with-continuation-result (result success)
         (apply #'edit-xml-delete source path #'success #'error options)
       result))
    (:append
     (cl-weave:with-continuation-result (result success)
         (apply #'edit-xml-append source path value #'success #'error options)
       result))))

(defun edit-within-p (source result start end)
  "Return true when RESULT equals SOURCE before START and after END of SOURCE."
  (multiple-value-bind (prefix suffix) (edit-changed-range source result)
    (and (>= prefix start) (>= suffix (- (length source) end)))))

(defun edit-refusal-or-result (thunk)
  "Return the string THUNK returns, or the reason of the XML-UNSUPPORTED-EDIT it signals.
Any other condition propagates and fails the test."
  (handler-case (funcall thunk)
    (xml-unsupported-edit (condition) (xml-edit-error-reason condition))))

(defun edit-documents-without-empty-element-reference ()
  "Return the first 200 generated documents, without those in which a reference follows />.
PARSE-XML accepts a reference after an empty document element, as in <a/>&#65;,
but rejects it once an edit gives that element an end tag, so every text and
append edit of such a document is refused for a reason outside the edit."
  (remove-if (lambda (source) (search "/>&" source))
             (subseq (spans-generated-documents) 0 200)))

(defun verify-doctype-document (doctype)
  "Return a document of a DOCTYPE, built from the list (ROOT-NAME PUBLIC-ID SYSTEM-ID), and an empty ROOT-NAME element.
PARSE-XML leaves the identifiers out of the DOM, so equivalence over them is checked on built documents."
  (destructuring-bind (root-name public-id system-id) doctype
    (make-xml-document
     :children (list (make-xml-doctype :root-name root-name :public-id public-id
                                       :system-id system-id)
                     (make-xml-element :name (make-xml-name :local-name root-name))))))
