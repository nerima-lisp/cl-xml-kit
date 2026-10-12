;;;; t/edit-test.lisp

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
    "  <!-- inner comment -->"
    "  <?inner pi?>"
    "</root>"
    "<!-- trailing -->")
  "A document with a leading U+FEFF, CRLF line breaks, a declaration, a DOCTYPE
with an internal subset, comments, processing instructions, CDATA, references,
both quote styles, default, prefixed, and redeclared namespaces, and attributes
laid out on one line and on separate lines.")

(defparameter +edit-source+
  (concatenate 'string +spans-bom+ (spans-join +spans-crlf+ +edit-lines+)))

(defun edit-range (needle &key (offset 0) length)
  "Return the start and end in +EDIT-SOURCE+ of LENGTH characters at OFFSET into the one occurrence of NEEDLE."
  (let ((start (search needle +edit-source+)))
    (assert start)
    (assert (null (search needle +edit-source+ :start2 (1+ start))))
    (values (+ start offset)
            (+ start offset (or length (- (length needle) offset))))))

(defun edit-expected (needle replacement &key (offset 0) length)
  "Return +EDIT-SOURCE+ with the range chosen by NEEDLE replaced by REPLACEMENT, and nothing else changed."
  (multiple-value-bind (start end) (edit-range needle :offset offset :length length)
    (concatenate 'string (subseq +edit-source+ 0 start) replacement
                 (subseq +edit-source+ end))))

(defun edit-set (path value &rest options)
  (apply #'edit-xml-set +edit-source+ path value options))

(defun edit-delete (path &rest options)
  (apply #'edit-xml-delete +edit-source+ path options))

(defun edit-line-break-and-indent (indent)
  (concatenate 'string +spans-crlf+ (make-string indent :initial-element #\Space)))

(defun edit-refusal (thunk)
  "Return (CLASS REASON PATH) of the XML-EDIT-ERROR that THUNK signals, or (:returned VALUE)."
  (handler-case (list :returned (funcall thunk))
    (xml-edit-error (caught)
      (list (type-of caught) (xml-edit-error-reason caught)
            (xml-edit-error-path caught)))))

;;; Replacing an existing attribute value

(it "replaces only the characters between double quotes"
  (expect (edit-set "/root/@b" "x")
          :to-equal (edit-expected "b=\"double\"" "x" :offset 3 :length 6)))

(it "replaces only the characters between single quotes"
  (expect (edit-set "/root/@a" "new value")
          :to-equal (edit-expected "a='single'" "new value" :offset 3 :length 6)))

(it "keeps a single quote and escapes it inside the value"
  (expect (edit-set "/root/item/@name" "it's \"a\" & b")
          :to-equal (edit-expected "name='n'" "it&apos;s \"a\" &amp; b"
                                   :offset 6 :length 1)))

(it "keeps a double quote and escapes it inside the value"
  (expect (edit-set "/root/item/@id" "say \"hi\" & 'bye'")
          :to-equal (edit-expected "id=\"1\"" "say &quot;hi&quot; &amp; 'bye'"
                                   :offset 4 :length 1)))

(it "writes TAB, LF, and CR as character references"
  (expect (edit-set "/root/item/@name"
                    (format nil "a~Cb~Cc~Cd" #\Tab #\Linefeed #\Return))
          :to-equal (edit-expected "name='n'" "a&#9;b&#10;c&#13;d" :offset 6 :length 1)))

(it "leaves greater-than, ]]>, and non-ASCII characters as they are"
  (let ((value (format nil "a>b ]]> ~C~C" (code-char #xE9) (code-char #x65E5))))
    (expect (edit-set "/root/@b" value)
            :to-equal (edit-expected "b=\"double\"" value :offset 3 :length 6))))

(it "escapes less-than in a replaced value"
  (expect (edit-set "/root/@b" "a<b<")
          :to-equal (edit-expected "b=\"double\"" "a&lt;b&lt;" :offset 3 :length 6))
  (expect (edit-set "/root/@a" "<'")
          :to-equal (edit-expected "a='single'" "&lt;&apos;" :offset 3 :length 6)))

(it "replaces a value with the empty string"
  (expect (edit-set "/root/@a" "")
          :to-equal (edit-expected "a='single'" "" :offset 3 :length 6)))

(it "replaces a prefixed attribute"
  (expect (edit-set "/root/p:item/@p:code" "8")
          :to-equal (edit-expected "p:code=\"7\"" "8" :offset 8 :length 1)))

(it "replaces a value inside a start tag laid out on separate lines"
  (expect (edit-set "/root/list/entry/@label" "second")
          :to-equal (edit-expected "label='first'" "second" :offset 7 :length 5)))

(it "writes a value that selects back to the value that was set"
  (dolist (value (list "plain" "" "a&b" "a<b>c" "&#65;" "say \"hi\" 'there'"
                       (format nil "t~Cl~Cr~Cx" #\Tab #\Linefeed #\Return)))
    (let ((edited (edit-set "/root/@b" value)))
      (expect (xml-match-value (first (select-xml edited "/root/@b"))) :to-equal value))))

;;; Adding an attribute

(it "adds an attribute to an element without attributes, after its name"
  (expect (edit-set "/root/bare/@id" "5")
          :to-equal (edit-expected "<bare/>" " id=\"5\"" :offset 5 :length 0)))

(it "adds an attribute before the space of a spaced empty tag"
  (expect (edit-set "/root/spaced/@id" "5")
          :to-equal (edit-expected "<spaced />" " id=\"5\"" :offset 7 :length 0)))

(it "adds an attribute after the last one, in its quote style"
  (expect (edit-set "/root/item/@new" "v")
          :to-equal (edit-expected "name='n'" " new='v'" :offset 8 :length 0)))

(it "escapes a new value for the quote character that it takes"
  (expect (edit-set "/root/item/@new" "it's \"q\"")
          :to-equal (edit-expected "name='n'" " new='it&apos;s \"q\"'" :offset 8 :length 0))
  (expect (edit-set "/root/@c" "it's \"q\"")
          :to-equal (edit-expected "b=\"double\"" " c=\"it's &quot;q&quot;\"" :offset 10 :length 0)))

(it "adds an attribute after the last of a root start tag"
  (expect (edit-set "/root/@c" "v")
          :to-equal (edit-expected "b=\"double\"" " c=\"v\"" :offset 10 :length 0)))

(it "copies the line break and indentation that precede the last attribute"
  (expect (edit-set "/root/list/entry/@extra" "x")
          :to-equal (edit-expected "state=\"on\""
                                   (concatenate 'string (edit-line-break-and-indent 8)
                                                "extra=\"x\"")
                                   :offset 10 :length 0)))

(it "copies the whitespace of a last attribute on its own line after a first attribute on the tag line"
  (expect (edit-xml-set (format nil "<root a=\"1\"~%      b=\"2\">x</root>") "/root/@c" "3")
          :to-equal (format nil "<root a=\"1\"~%      b=\"2\"~%      c=\"3\">x</root>")))

(it "copies the whitespace before the last attribute when only the last is on its own line"
  (expect (edit-xml-set (format nil "<r a=\"1\" b=\"2\"~%    c=\"3\"/>") "/r/@d" "v")
          :to-equal (format nil "<r a=\"1\" b=\"2\"~%    c=\"3\"~%    d=\"v\"/>")))

(it "uses a single space when the last attribute is inline even if an earlier one is not"
  (expect (edit-xml-set (format nil "<r a=\"1\"~%    b=\"2\" c=\"3\"/>") "/r/@d" "v")
          :to-equal (format nil "<r a=\"1\"~%    b=\"2\" c=\"3\" d=\"v\"/>")))

(it "adds a prefixed attribute whose prefix is declared on an ancestor"
  (expect (edit-set "/root/bare/@p:id" "5")
          :to-equal (edit-expected "<bare/>" " p:id=\"5\"" :offset 5 :length 0)))

(it "resolves a new prefix through a declaration on the element itself"
  (expect (edit-set "/root/p:item/@p:other" "v")
          :to-equal (edit-expected "p:code=\"7\"" " p:other=\"v\"" :offset 10 :length 0)))

(it "resolves a new prefix through the nearest declaration"
  (expect (edit-set "/root/p:item/@q:note" "n")
          :to-equal (edit-expected "p:code=\"7\"" " q:note=\"n\"" :offset 10 :length 0)))

(it "adds an attribute in the predeclared xml namespace"
  (expect (edit-set "/root/bare/@xml:lang" "en")
          :to-equal (edit-expected "<bare/>" " xml:lang=\"en\"" :offset 5 :length 0)))

(it "takes the quote of the first attribute in the document for an element without attributes"
  (let ((source "<r><a/><b x='1'/></r>"))
    (expect (edit-xml-set source "/r/a/@y" "v") :to-equal "<r><a y='v'/><b x='1'/></r>")))

(it "falls back to double quotes in a document without attributes"
  (expect (edit-xml-set "<r><a/></r>" "/r/a/@y" "v") :to-equal "<r><a y=\"v\"/></r>"))

(it "escapes less-than in a new attribute"
  (expect (edit-set "/root/bare/@id" "1<2")
          :to-equal (edit-expected "<bare/>" " id=\"1&lt;2\"" :offset 5 :length 0)))

(it "adds an attribute that selects back as the value that was set"
  (let ((edited (edit-set "/root/bare/@id" "a&b")))
    (expect (xml-match-value (first (select-xml edited "/root/bare/@id")))
            :to-equal "a&b")))

;;; Deleting an attribute

(it "deletes the first attribute with its leading whitespace"
  (expect (edit-delete "/root/item/@id")
          :to-equal (edit-expected " id=\"1\"" "")))

(it "deletes a middle attribute with its leading whitespace"
  (expect (edit-delete "/root/@a")
          :to-equal (edit-expected " a='single'" "")))

(it "deletes the last attribute with its leading whitespace"
  (expect (edit-delete "/root/@b")
          :to-equal (edit-expected " b=\"double\"" "")))

(it "deletes the only attribute of an element"
  (expect (edit-delete "/root/holder/@x")
          :to-equal (edit-expected " x=\"1\"" "")))

(it "deletes the only attribute of an empty element and keeps the slash"
  (expect (edit-delete "/root/dup/@p:flag")
          :to-equal (edit-expected " p:flag=\"1\"" "")))

(it "deletes an attribute together with the line break before it"
  (expect (edit-delete "/root/list/entry/@label")
          :to-equal (edit-expected (concatenate 'string (edit-line-break-and-indent 8)
                                                "label='first'")
                                   "")))

(it "deletes a prefixed attribute"
  (expect (edit-delete "/root/p:item/@p:code")
          :to-equal (edit-expected " p:code=\"7\"" "")))

(it "no longer selects a deleted attribute"
  (expect (select-xml (edit-delete "/root/@a") "/root/@a") :to-equal nil))

;;; Returned strings

(it "returns a fresh string and leaves the source unmodified"
  (let* ((source (copy-seq +edit-source+))
         (result (edit-xml-set source "/root/@b" "x")))
    (expect (eq result source) :to-equal nil)
    (expect source :to-equal +edit-source+)))

(it "keeps the leading U+FEFF, the CRLF line breaks, and everything outside the range"
  (let ((result (edit-set "/root/@b" "x")))
    (expect (char result 0) :to-equal (code-char #xFEFF))
    (expect (count #\Return result) :to-equal (count #\Return +edit-source+))
    (expect (length result) :to-equal (- (length +edit-source+) 5))))

(it "applies the caller's external entity policy to the source"
  (let ((source "<!DOCTYPE a SYSTEM \"a.dtd\"><a x=\"1\"/>"))
    (expect (typep (handler-case (edit-xml-set source "/a/@x" "2")
                     (xml-kit-error (caught) caught))
                   'xml-external-resource-disabled)
            :to-be-truthy)
    (expect (edit-xml-set source "/a/@x" "2" :external-entity-policy :skip)
            :to-equal "<!DOCTYPE a SYSTEM \"a.dtd\"><a x=\"2\"/>")))

;;; Refusals

(it "refuses a path that matches nothing"
  (expect (edit-refusal (lambda () (edit-set "/root/nothing/@a" "v")))
          :to-equal '(xml-path-error :no-match "/root/nothing/@a"))
  (expect (edit-refusal (lambda () (edit-delete "/root/nothing/@a")))
          :to-equal '(xml-path-error :no-match "/root/nothing/@a")))

(it "refuses to delete an attribute that the element does not have"
  (expect (edit-refusal (lambda () (edit-delete "/root/item/@missing")))
          :to-equal '(xml-path-error :no-match "/root/item/@missing")))

(it "refuses a path that matches more than one element"
  (expect (edit-refusal (lambda () (edit-set "/root/*/@id" "v")))
          :to-equal '(xml-path-error :multiple-matches "/root/*/@id"))
  (expect (edit-refusal (lambda () (edit-delete "/root/*/@id")))
          :to-equal '(xml-path-error :multiple-matches "/root/*/@id")))

(it "refuses a malformed path with its index"
  (let ((condition (handler-case (edit-set "/root//a/@b" "v")
                     (xml-path-error (caught) caught))))
    (expect (list (xml-edit-error-reason condition) (xml-path-error-index condition)
                  (xml-edit-error-path condition))
            :to-equal '(:syntax 6 "/root//a/@b"))))

(it "refuses to delete text()"
  (expect (edit-refusal (lambda () (edit-delete "/root/item/text()")))
          :to-equal '(xml-path-error :wrong-target "/root/item/text()")))

(it "refuses a new prefixed attribute whose prefix is not in scope"
  (expect (edit-refusal (lambda () (edit-set "/root/bare/@z:id" "v")))
          :to-equal '(xml-path-error :undeclared-prefix "/root/bare/@z:id")))

(it "refuses to set or delete a namespace declaration"
  (dolist (path '("/root/@xmlns" "/root/@xmlns:p" "/root/@xmlns:new"))
    (expect (edit-refusal (lambda () (edit-set path "urn:x")))
            :to-equal (list 'xml-unsupported-edit :namespace-declaration path))
    (expect (edit-refusal (lambda () (edit-delete path)))
            :to-equal (list 'xml-unsupported-edit :namespace-declaration path))))

(it "refuses attribute edits on an element named in an ATTLIST declaration"
  (dolist (path '("/root/governed/@id" "/root/governed/@other"))
    (expect (edit-refusal (lambda () (edit-set path "v")))
            :to-equal (list 'xml-unsupported-edit :attlist-declaration path)))
  (expect (edit-refusal (lambda () (edit-delete "/root/governed/@id")))
          :to-equal '(xml-unsupported-edit :attlist-declaration "/root/governed/@id")))

(it "refuses a value with a character that XML does not allow"
  (dolist (code '(0 8 #xB #xD800 #xFFFE #xFFFF))
    (let ((value (format nil "a~Cb" (code-char code))))
      (expect (edit-refusal (lambda () (edit-set "/root/@b" value)))
              :to-equal '(xml-edit-value-error :invalid-character "/root/@b")))))

(it "refuses a step below an element that references a markup entity"
  (expect (edit-refusal (lambda () (edit-set "/root/holder/b/@a" "v")))
          :to-equal '(xml-unsupported-edit :entity-reference "/root/holder/b/@a")))

(it "still edits the attributes of an element that references a markup entity"
  (expect (edit-set "/root/holder/@x" "2")
          :to-equal (edit-expected "x=\"1\"" "2" :offset 3 :length 1)))

(it "refuses an edit whose result would not be the requested document"
  (expect (edit-refusal (lambda () (edit-set "/root/dup/@q:flag" "2")))
          :to-equal '(xml-unsupported-edit :verification-failed "/root/dup/@q:flag")))

(it "refuses an edit whose result breaks a limit of the caller"
  (let ((limits (make-xml-resource-limits :max-attributes-per-element 1)))
    (expect (handler-case (edit-xml-set "<a x=\"1\"/>" "/a/@y" "2" :resource-limits limits)
              (xml-unsupported-edit (caught) (xml-edit-error-reason caught)))
            :to-equal :verification-failed)))

(it "rejects a source, path, or value that is not a string with an input error"
  (dolist (thunk (list (lambda () (edit-xml-set 42 "/a/@x" "v"))
                       (lambda () (edit-xml-set "<a/>" 42 "v"))
                       (lambda () (edit-xml-set "<a/>" "/a/@x" 42))
                       (lambda () (edit-xml-delete 42 "/a/@x"))
                       (lambda () (edit-xml-delete "<a/>" 42))))
    (expect (typep (handler-case (funcall thunk) (xml-kit-error (caught) caught))
                   'xml-input-error)
            :to-be-truthy)))

(it "signals the parse condition for a source that is not well-formed"
  (expect (typep (handler-case (edit-xml-set "<a>" "/a/@x" "v")
                   (xml-kit-error (caught) caught))
                 'xml-well-formedness-error)
          :to-be-truthy))

;;; Generated documents

(defun edit-element-matches (source)
  "Return the element matches of SOURCE at every depth, skipping below a markup-entity reference."
  (let ((matches nil))
    (labels ((walk (path)
               (dolist (match (handler-case (select-xml source path)
                                (xml-unsupported-edit () nil)))
                 (push match matches)
                 (walk (concatenate 'string (xml-match-path match) "/*")))))
      (walk "/*"))
    (nreverse matches)))

(defun edit-outside-p (source result start end)
  "Return true when SOURCE and RESULT agree before START and after END of SOURCE."
  (let ((prefix (or (mismatch source result) (length source)))
        (suffix (let ((mismatch (mismatch source result :from-end t)))
                  (if mismatch (- (length source) mismatch) (length source)))))
    (and (>= prefix start) (>= suffix (- (length source) end)))))

(it "changes only the targeted element when setting attributes of generated documents"
  (let ((edited 0))
    (dolist (source (subseq (spans-generated-documents) 0 200))
      (dolist (match (edit-element-matches source))
        (let* ((path (concatenate 'string (xml-match-path match) "/@zz"))
               (result (handler-case (edit-xml-set source path "v&'\" <t>")
                         (xml-unsupported-edit (caught)
                           (xml-edit-error-reason caught)))))
          (if (stringp result)
              (progn
                (incf edited)
                (expect (edit-outside-p source result (xml-match-start match)
                                        (xml-match-end match))
                        :to-be-truthy)
                (expect (xml-match-value (first (select-xml result path)))
                        :to-equal "v&'\" <t>"))
              (expect result :to-equal :attlist-declaration)))))
    (expect (> edited 300) :to-be-truthy)))

(it "removes only the attribute when deleting attributes of generated documents"
  (let ((edited 0))
    (dolist (source (subseq (spans-generated-documents) 0 200))
      (dolist (match (edit-element-matches source))
        (dolist (attribute (xml-element-attributes (xml-match-element match)))
          (let* ((path (concatenate 'string (xml-match-path match) "/@"
                                    (cl-xml-kit::%path-lexical-name
                                     (xml-attribute-name attribute))))
                 (result (handler-case (edit-xml-delete source path)
                           (xml-unsupported-edit (caught)
                             (xml-edit-error-reason caught)))))
            (if (stringp result)
                (progn
                  (incf edited)
                  (expect (edit-outside-p source result (xml-match-start match)
                                          (xml-match-end match))
                          :to-be-truthy)
                  (expect (select-xml result path) :to-equal nil))
                (expect result :to-equal :attlist-declaration))))))
    (expect (> edited 100) :to-be-truthy)))
