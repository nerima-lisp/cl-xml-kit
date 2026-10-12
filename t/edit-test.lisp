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
    "  <cdata><![CDATA[a < b]]></cdata>"
    "  <refs>&#65;&amp;&e;</refs>"
    "  <blank>   </blank>"
    "  <nested><inner>x</inner></nested>"
    "  <noted><!-- c --></noted>"
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

(defun edit-expected-between (first-needle last-needle replacement)
  "Return +EDIT-SOURCE+ with everything from FIRST-NEEDLE through the last LAST-NEEDLE replaced."
  (let ((start (search first-needle +edit-source+))
        (end (+ (search last-needle +edit-source+ :from-end t) (length last-needle))))
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

;;; Setting text

(it "turns an empty element into one with text, keeping the rest of the tag"
  (expect (edit-set "/root/bare/text()" "hi")
          :to-equal (edit-expected "<bare/>" ">hi</bare>" :offset 5 :length 2))
  (expect (edit-set "/root/spaced/text()" "hi")
          :to-equal (edit-expected "<spaced />" ">hi</spaced>" :offset 8 :length 2)))

(it "replaces text mixed with references and CDATA by escaped character data"
  (expect (edit-set "/root/item/text()" "a<b>&c")
          :to-equal (edit-expected "text &amp; &#65; &e; <![CDATA[<raw>]]>"
                                   "a&lt;b&gt;&amp;c")))

(it "keeps a CDATA section when the content is exactly one"
  (expect (edit-set "/root/cdata/text()" "x < y & z")
          :to-equal (edit-expected "<![CDATA[a < b]]>" "<![CDATA[x < y & z]]>")))

(it "falls back to escaped text when a CDATA section cannot hold the value"
  (expect (edit-set "/root/cdata/text()" "a]]>b")
          :to-equal (edit-expected "<![CDATA[a < b]]>" "a]]&gt;b"))
  (expect (edit-set "/root/cdata/text()" (format nil "a~Cb" #\Return))
          :to-equal (edit-expected "<![CDATA[a < b]]>" "a&#13;b")))

(it "replaces content made of character and entity references"
  (expect (edit-set "/root/refs/text()" "plain")
          :to-equal (edit-expected "&#65;&amp;&e;" "plain")))

(it "replaces whitespace-only content"
  (expect (edit-set "/root/blank/text()" "x")
          :to-equal (edit-expected "<blank>   </blank>" "x" :offset 7 :length 3)))

(it "removes the content when the value is empty"
  (expect (edit-set "/root/item/text()" "")
          :to-equal (edit-expected "text &amp; &#65; &e; <![CDATA[<raw>]]>" "")))

(it "writes a line feed in the document's line break style and a carriage return as a reference"
  (expect (edit-set "/root/refs/text()" (format nil "a~Cb" #\Linefeed))
          :to-equal (edit-expected "&#65;&amp;&e;" (format nil "a~C~Cb" #\Return #\Linefeed)))
  (expect (edit-set "/root/refs/text()" (format nil "a~C~Cb" #\Return #\Linefeed))
          :to-equal (edit-expected "&#65;&amp;&e;"
                                   (format nil "a&#13;~C~Cb" #\Return #\Linefeed))))

(it "keeps a document without carriage returns free of them when setting text"
  (expect (edit-xml-set (format nil "<r>~%  <a>x</a>~%</r>") "/r/a/text()"
                        (format nil "p~Cq" #\Linefeed))
          :to-equal (format nil "<r>~%  <a>p~Cq</a>~%</r>" #\Linefeed)))

(it "sets text that selects back as the value that was set"
  (dolist (value (list "plain" "" "  padded  " "a&b<c>d" "a]]>b" "<![CDATA[x]]>" "&#65;"
                       (format nil "l1~Cl2" #\Linefeed) (format nil "cr~Cx" #\Return)
                       (format nil "crlf~C~Cx" #\Return #\Linefeed)))
    (dolist (path '("/root/bare/text()" "/root/cdata/text()" "/root/item/text()"))
      (expect (xml-match-value (first (select-xml (edit-set path value) path)))
              :to-equal value))))

(it "refuses to replace content that holds elements, comments, or markup entities"
  (dolist (path '("/root/nested/text()" "/root/noted/text()" "/root/holder/text()"
                  "/root/text()"))
    (expect (edit-refusal (lambda () (edit-set path "v")))
            :to-equal (list 'xml-unsupported-edit :mixed-content path))))

(it "refuses text for a path that matches nothing or more than one element"
  (expect (edit-refusal (lambda () (edit-set "/root/nothing/text()" "v")))
          :to-equal '(xml-path-error :no-match "/root/nothing/text()"))
  (expect (edit-refusal (lambda () (edit-set "/root/*/text()" "v")))
          :to-equal '(xml-path-error :multiple-matches "/root/*/text()")))

(it "refuses text with a character that XML does not allow"
  (expect (edit-refusal (lambda () (edit-set "/root/bare/text()" (format nil "a~Cb" (code-char 1)))))
          :to-equal '(xml-edit-value-error :invalid-character "/root/bare/text()")))

;;; Replacing an element

(it "replaces an element with a fragment, verbatim"
  (expect (edit-set "/root/bare" "<renamed k='v'>text</renamed>")
          :to-equal (edit-expected "<bare/>" "<renamed k='v'>text</renamed>")))

(it "trims the XML whitespace around the fragment"
  (expect (edit-set "/root/bare" (format nil " ~C~C~C<renamed/>~C~C " #\Tab #\Return #\Linefeed
                                          #\Linefeed #\Space))
          :to-equal (edit-expected "<bare/>" "<renamed/>")))

(it "converts the line breaks of a fragment to the document's style"
  (expect (edit-set "/root/bare" (format nil "<l>~%  <e/>~%</l>"))
          :to-equal (edit-expected "<bare/>" (format nil "<l>~C~C  <e/>~C~C</l>"
                                                     #\Return #\Linefeed #\Return #\Linefeed)))
  (expect (edit-set "/root/bare" (format nil "<l>~C~C<e/>~C</l>" #\Return #\Linefeed #\Return))
          :to-equal (edit-expected "<bare/>" (format nil "<l>~C~C<e/>~C~C</l>"
                                                     #\Return #\Linefeed #\Return #\Linefeed))))

(it "keeps comments, CDATA, and both quote styles of a fragment verbatim"
  (let ((fragment "<n a='1' b=\"2\"><!-- c --><![CDATA[<x>]]><?p d?></n>"))
    (expect (edit-set "/root/spaced" fragment)
            :to-equal (edit-expected "<spaced />" fragment))))

(it "replaces a nested element"
  (expect (edit-set "/root/list/entry" "<entry2/>")
          :to-equal (edit-expected-between "<entry" "state=\"on\"/>" "<entry2/>")))

(it "puts a fragment in the default namespace in scope"
  (let ((result (edit-set "/root/bare" "<plain/>")))
    (expect (xml-name-namespace-uri
             (xml-element-name (xml-match-element (first (select-xml result "/root/plain")))))
            :to-equal "urn:default")))

(it "resolves a prefix declared above the replaced element"
  (let ((result (edit-set "/root/bare" "<p:thing p:x=\"1\"/>")))
    (expect (xml-name-namespace-uri
             (xml-element-name (xml-match-element (first (select-xml result "/root/p:thing")))))
            :to-equal "urn:p")))

(it "ignores the declarations of the element being replaced"
  (let ((result (edit-set "/root/p:item" "<p:x/>")))
    (expect (xml-name-namespace-uri
             (xml-element-name (xml-match-element (first (select-xml result "/root/p:x")))))
            :to-equal "urn:p")))

(it "replaces the document element"
  (expect (edit-set "/root" "<root xmlns=\"urn:new\"><only/></root>")
          :to-equal (edit-expected-between "<root xmlns=\"urn:default\"" "</root>"
                                           "<root xmlns=\"urn:new\"><only/></root>")))

(it "refuses a fragment that is not exactly one element"
  (dolist (fragment '("" "  " "text" "<a/><b/>" "<a/>x" "<!--c--><a/>"))
    (expect (edit-refusal (lambda () (edit-set "/root/bare" fragment)))
            :to-equal '(xml-edit-value-error :not-single-element "/root/bare"))))

(it "refuses a fragment that does not parse where it is written"
  (dolist (fragment '("<a>" "<a></b>" "<z:a/>" "<a>&xxe;</a>" "<a>&e;</a>"
                      "<!DOCTYPE a><a/>" "<?xml version=\"1.0\"?><a/>"
                      "<!DOCTYPE a [<!ENTITY x SYSTEM \"x.ent\">]><a>&x;</a>"))
    (expect (edit-refusal (lambda () (edit-set "/root/bare" fragment)))
            :to-equal '(xml-edit-value-error :malformed-fragment "/root/bare"))))

(it "keeps the parse condition of a malformed fragment as its cause"
  (flet ((cause-type (fragment &rest options)
           (type-of (xml-edit-value-error-cause
                     (handler-case (apply #'edit-set "/root/bare" fragment options)
                       (xml-edit-value-error (caught) caught))))))
    (expect (cause-type "<a>") :to-equal 'xml-well-formedness-error)
    (expect (cause-type "<z:a/>") :to-equal 'xml-namespace-error)
    (expect (cause-type "<!DOCTYPE a><a/>" :external-entity-policy :skip)
            :to-equal 'xml-well-formedness-error)))

(it "refuses a fragment that breaks a resource limit of the caller"
  (let ((limits (make-xml-resource-limits :max-attributes-per-element 3)))
    (expect (edit-refusal (lambda ()
                            (edit-xml-set "<r xmlns:p=\"urn:p\"><a/></r>" "/r/a"
                                          "<b x=\"1\" y=\"2\" z=\"3\" w=\"4\"/>"
                                          :resource-limits limits)))
            :to-equal '(xml-edit-value-error :malformed-fragment "/r/a"))))

(it "refuses a fragment that is too deep for the wrapper it is parsed in"
  (let ((limits (make-xml-resource-limits :max-depth 3)))
    (expect (edit-refusal (lambda ()
                            (edit-xml-set "<r><x><y/></x></r>" "/r/x/y" "<a><b><c/></b></a>"
                                          :resource-limits limits)))
            :to-equal '(xml-edit-value-error :malformed-fragment "/r/x/y"))))

(it "refuses a fragment that fits alone but exceeds the depth limit where it lands"
  (let ((limits (make-xml-resource-limits :max-depth 3)))
    (expect (edit-refusal (lambda ()
                            (edit-xml-set "<r><x><y/></x></r>" "/r/x/y" "<a><b/></a>"
                                          :resource-limits limits)))
            :to-equal '(xml-unsupported-edit :verification-failed "/r/x/y"))
    (expect (edit-xml-set "<r><x><y/></x></r>" "/r/x" "<a><b/></a>" :resource-limits limits)
            :to-equal "<r><a><b/></a></r>")))

(it "refuses a replacement of the document element that no longer matches the DOCTYPE"
  (expect (edit-refusal (lambda () (edit-set "/root" "<other/>")))
          :to-equal '(xml-unsupported-edit :verification-failed "/root")))

(it "refuses a fragment with a character that XML does not allow"
  (expect (edit-refusal (lambda () (edit-set "/root/bare" (format nil "<a>~C</a>" (code-char 0)))))
          :to-equal '(xml-edit-value-error :invalid-character "/root/bare")))

(it "refuses to replace a path that matches nothing or more than one element"
  (expect (edit-refusal (lambda () (edit-set "/root/nothing" "<a/>")))
          :to-equal '(xml-path-error :no-match "/root/nothing"))
  (expect (edit-refusal (lambda () (edit-set "/root/*" "<a/>")))
          :to-equal '(xml-path-error :multiple-matches "/root/*")))

;;; Deleting an element

(defun edit-line-and (text)
  "Return the line break and indentation before a child element, then TEXT."
  (concatenate 'string +spans-crlf+ "  " text))

(it "deletes the first child with the line break before it"
  (expect (edit-delete "/root/item")
          :to-equal (edit-expected (edit-line-and "<item id=\"1\" name='n'>text &amp; &#65; &e; <![CDATA[<raw>]]></item>")
                                   "")))

(it "deletes a middle child with the line break before it"
  (expect (edit-delete "/root/bare")
          :to-equal (edit-expected (edit-line-and "<bare/>") "")))

(it "deletes the last element child with the line break before it"
  (expect (edit-delete "/root/noted")
          :to-equal (edit-expected (edit-line-and "<noted><!-- c --></noted>") "")))

(it "deletes the only child and leaves the end tag on its own line"
  (expect (edit-delete "/root/list/entry")
          :to-equal (edit-expected (concatenate 'string +spans-crlf+ "    <entry" +spans-crlf+
                                                "        id=\"e1\"" +spans-crlf+
                                                "        label='first'" +spans-crlf+
                                                "        state=\"on\"/>")
                                   "")))

(it "deletes a spaced empty element"
  (expect (edit-delete "/root/spaced")
          :to-equal (edit-expected (edit-line-and "<spaced />") "")))

(it "removes only the element when other content shares its line"
  (expect (edit-xml-delete "<r><a/><b/></r>" "/r/b") :to-equal "<r><a/></r>")
  (expect (edit-xml-delete "<r><a/><b/></r>" "/r/a") :to-equal "<r><b/></r>")
  (expect (edit-xml-delete "<r>x <a/> y</r>" "/r/a") :to-equal "<r>x  y</r>")
  (expect (edit-xml-delete (format nil "<r>~%  <!-- c --><a/>~%</r>") "/r/a")
          :to-equal (format nil "<r>~%  <!-- c -->~%</r>")))

(it "removes the line break before an element in documents of every line break style"
  (dolist (break (list (string #\Linefeed) +spans-crlf+ (string #\Return)))
    (expect (edit-xml-delete (format nil "<r>~A  <a/>~A  <b/>~A</r>" break break break) "/r/a")
            :to-equal (format nil "<r>~A  <b/>~A</r>" break break))
    (expect (edit-xml-delete (format nil "<r>~A  <a/>~A  <b/>~A</r>" break break break) "/r/b")
            :to-equal (format nil "<r>~A  <a/>~A</r>" break break))))

(it "leaves the whitespace around a deleted element when it belongs to character data"
  (expect (edit-xml-delete (format nil "<r>foo~%  <a/></r>") "/r/a")
          :to-equal (format nil "<r>foo~%  </r>"))
  (expect (edit-xml-delete (format nil "<r>~%  <a/>~%bar</r>") "/r/a")
          :to-equal (format nil "<r>~%  ~%bar</r>"))
  (expect (edit-xml-delete (format nil "<r>&#10;  <a/></r>") "/r/a")
          :to-equal (format nil "<r>&#10;  </r>")))

(it "still removes the whitespace and line break when the text on both sides is plain whitespace"
  (expect (edit-xml-delete (format nil "<r>~%  <a/>~%</r>") "/r/a")
          :to-equal (format nil "<r>~%</r>"))
  (expect (edit-xml-delete (format nil "<r>~%  <a/><b/></r>") "/r/a")
          :to-equal (format nil "<r><b/></r>")))

(it "removes tab indentation together with the line break"
  (expect (edit-xml-delete (format nil "<r>~%~C<a/>~%</r>" #\Tab) "/r/a")
          :to-equal (format nil "<r>~%</r>")))

(it "deletes an element that no longer selects after the edit"
  (expect (select-xml (edit-delete "/root/bare") "/root/bare") :to-equal nil))

(it "refuses to delete the document element"
  (expect (edit-refusal (lambda () (edit-delete "/root")))
          :to-equal '(xml-unsupported-edit :root-element "/root"))
  (expect (edit-refusal (lambda () (edit-delete "/*")))
          :to-equal '(xml-unsupported-edit :root-element "/*")))

(it "refuses to delete a path that matches nothing or more than one element"
  (expect (edit-refusal (lambda () (edit-delete "/root/nothing")))
          :to-equal '(xml-path-error :no-match "/root/nothing"))
  (expect (edit-refusal (lambda () (edit-delete "/root/*")))
          :to-equal '(xml-path-error :multiple-matches "/root/*")))

(it "refuses to delete below an element that references a markup entity"
  (expect (edit-refusal (lambda () (edit-delete "/root/holder/b")))
          :to-equal '(xml-unsupported-edit :entity-reference "/root/holder/b")))

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

(defun edit-stage-c-documents ()
  "Return generated documents, without those that have a reference after an empty document element."
  (remove-if (lambda (source) (search "/>&" source))
             (subseq (spans-generated-documents) 0 200)))

(defun edit-removal-outside-p (source result start end)
  "Return true when SOURCE and RESULT agree outside the element from START to END, allowing the line break before it to go."
  (let ((prefix (or (mismatch source result) (length source)))
        (suffix (let ((mismatch (mismatch source result :from-end t)))
                  (if mismatch (- (length source) mismatch) (length source)))))
    (and (>= prefix (cl-xml-kit::%edit-removal-start source start))
         (>= suffix (- (length source) end)))))

(it "sets the text of every generated element that is not mixed, and refuses the others as mixed"
  (let ((edited 0))
    (dolist (source (edit-stage-c-documents))
      (dolist (match (edit-element-matches source))
        (let* ((path (concatenate 'string (xml-match-path match) "/text()"))
               (value (format nil "t&<>]]>~Cx" #\Linefeed))
               (result (handler-case (edit-xml-set source path value)
                         (xml-unsupported-edit (caught)
                           (xml-edit-error-reason caught)))))
          (if (stringp result)
              (progn
                (incf edited)
                (expect (edit-outside-p source result (xml-match-start match)
                                        (xml-match-end match))
                        :to-be-truthy)
                (expect (xml-match-value (first (select-xml result path))) :to-equal value))
              (expect result :to-equal :mixed-content)))))
    (expect (> edited 300) :to-be-truthy)))

(it "replaces every generated element below the document element with a fragment"
  (let ((edited 0))
    (dolist (source (edit-stage-c-documents))
      (dolist (match (edit-element-matches source))
        (when (find #\/ (xml-match-path match) :start 1)
          (let ((result (edit-xml-set source (xml-match-path match)
                                      (format nil " <zz a=\"1\">~%t</zz>~%"))))
            (incf edited)
            (expect (edit-outside-p source result (xml-match-start match)
                                    (xml-match-end match))
                    :to-be-truthy)
            (expect (subseq result (xml-match-start match) (+ (xml-match-start match) 10))
                    :to-equal "<zz a=\"1\">")))))
    (expect (> edited 100) :to-be-truthy)))

(it "deletes every generated element below the document element"
  (let ((edited 0))
    (dolist (source (edit-stage-c-documents))
      (dolist (match (edit-element-matches source))
        (when (find #\/ (xml-match-path match) :start 1)
          (let ((result (edit-xml-delete source (xml-match-path match))))
            (incf edited)
            (expect (edit-removal-outside-p source result (xml-match-start match)
                                            (xml-match-end match))
                    :to-be-truthy)
            (expect (>= (- (length source) (length result))
                        (- (xml-match-end match) (xml-match-start match)))
                    :to-be-truthy)))))
    (expect (> edited 100) :to-be-truthy)))
