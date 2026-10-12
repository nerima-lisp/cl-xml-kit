;;;; t/path-test.lisp

(in-package #:cl-xml-kit/test)

(defparameter +path-document+
  (spans-join +spans-crlf+
              '("<root xmlns=\"urn:default\" xmlns:p=\"urn:p\" a=\"1\">"
                "  <item id=\"1\">one</item>"
                "  <item id=\"2\">two</item>"
                "  <p:item p:id=\"x\">pre</p:item>"
                "  <group><item id=\"3\">three</item><empty/></group>"
                "  <item>three-b</item>"
                "</root>"))
  "A CRLF document with default and prefixed namespaces and repeated element names.")

(defun path-select (source path &rest options)
  (apply #'select-xml source path options))

(defun path-summaries (source path)
  "Return (KIND PATH) for every match of PATH in SOURCE."
  (mapcar (lambda (match) (list (xml-match-kind match) (xml-match-path match)))
          (path-select source path)))

(defun path-slices (source path)
  (mapcar (lambda (match)
            (subseq source (xml-match-start match) (xml-match-end match)))
          (path-select source path)))

(defun path-refusal (thunk)
  "Return (CLASS REASON PATH) of the XML-EDIT-ERROR that THUNK signals, or (:returned VALUE)."
  (handler-case (list :returned (funcall thunk))
    (xml-edit-error (caught)
      (list (type-of caught) (xml-edit-error-reason caught)
            (xml-edit-error-path caught)))))

;;; Reading

(it "selects the document element with its span and DOM element"
  (let ((match (first (path-select +path-document+ "/root"))))
    (expect (list (xml-match-kind match) (xml-match-path match) (xml-match-name match)
                  (xml-match-value match) (xml-match-start match))
            :to-equal '(:element "/root[1]" "root" nil 0))
    (expect (xml-match-end match) :to-equal (length +path-document+))
    (expect (xml-name-local-name (xml-element-name (xml-match-element match)))
            :to-equal "root")))

(it "selects child elements in document order with canonical paths"
  (expect (path-summaries +path-document+ "/root/*")
          :to-equal '((:element "/root[1]/item[1]") (:element "/root[1]/item[2]")
                      (:element "/root[1]/p:item[1]") (:element "/root[1]/group[1]")
                      (:element "/root[1]/item[3]"))))

(it "reports the source span of each selected element"
  (expect (path-slices +path-document+ "/root/item")
          :to-equal '("<item id=\"1\">one</item>" "<item id=\"2\">two</item>"
                      "<item>three-b</item>")))

(it "counts an index among the children that pass the name test"
  (expect (path-slices +path-document+ "/root/item[2]")
          :to-equal '("<item id=\"2\">two</item>"))
  (expect (path-slices +path-document+ "/root/item[3]")
          :to-equal '("<item>three-b</item>")))

(it "counts an index over all children for the wildcard"
  (expect (path-summaries +path-document+ "/root/*[3]")
          :to-equal '((:element "/root[1]/p:item[1]"))))

(it "returns nil when the index is beyond the matching children"
  (expect (path-select +path-document+ "/root/item[4]") :to-equal nil))

(it "tests the document element against the first step only"
  (expect (path-select +path-document+ "/other") :to-equal nil)
  (expect (path-select +path-document+ "/root[2]") :to-equal nil)
  (expect (path-summaries +path-document+ "/*") :to-equal '((:element "/root[1]")))
  (expect (path-summaries +path-document+ "/root[1]") :to-equal '((:element "/root[1]"))))

(it "compares element names as written, prefix included"
  (expect (path-summaries +path-document+ "/root/p:item")
          :to-equal '((:element "/root[1]/p:item[1]")))
  (expect (length (path-select +path-document+ "/root/item")) :to-equal 3))

(it "descends through nested steps with an index on every canonical step"
  (expect (path-summaries +path-document+ "/root/group/item/@id")
          :to-equal '((:attribute "/root[1]/group[1]/item[1]/@id")))
  (expect (path-summaries +path-document+ "/root/group/*")
          :to-equal '((:element "/root[1]/group[1]/item[1]")
                      (:element "/root[1]/group[1]/empty[1]"))))

(it "maps a nested match to its DOM element"
  (let* ((match (first (path-select +path-document+ "/root/group/item")))
         (id (first (xml-element-attributes (xml-match-element match)))))
    (expect (xml-attribute-value id) :to-equal "3")))

(it "selects only the attributes written with the name, skipping elements without it"
  (expect (path-summaries +path-document+ "/root/item/@id")
          :to-equal '((:attribute "/root[1]/item[1]/@id") (:attribute "/root[1]/item[2]/@id")))
  (expect (mapcar #'xml-match-value (path-select +path-document+ "/root/item/@id"))
          :to-equal '("1" "2")))

(it "spans an attribute from its name through its closing quote"
  (expect (path-slices +path-document+ "/root/item/@id")
          :to-equal '("id=\"1\"" "id=\"2\"")))

(it "reports the lexical name of an attribute and its owning DOM element"
  (let ((match (first (path-select +path-document+ "/root/p:item/@p:id"))))
    (expect (xml-match-name match) :to-equal "p:id")
    (expect (xml-match-value match) :to-equal "x")
    (expect (xml-name-local-name (xml-element-name (xml-match-element match)))
            :to-equal "item")))

(it "never selects a namespace declaration as an attribute"
  (expect (path-select +path-document+ "/root/@xmlns") :to-equal nil)
  (expect (path-select +path-document+ "/root/@xmlns:p") :to-equal nil)
  (expect (length (path-select +path-document+ "/root/@a")) :to-equal 1))

(it "selects the direct character content of every matched element"
  (expect (mapcar #'xml-match-value (path-select +path-document+ "/root/item/text()"))
          :to-equal '("one" "two" "three-b"))
  (expect (mapcar #'xml-match-path (path-select +path-document+ "/root/item/text()"))
          :to-equal '("/root[1]/item[1]/text()" "/root[1]/item[2]/text()"
                      "/root[1]/item[3]/text()"))
  (expect (path-slices +path-document+ "/root/item/text()")
          :to-equal '("one" "two" "three-b")))

(it "selects empty text for an element without content, at the slash of an empty tag"
  (let ((match (first (path-select +path-document+ "/root/group/empty/text()"))))
    (expect (xml-match-value match) :to-equal "")
    (expect (= (xml-match-start match) (xml-match-end match)) :to-be-truthy)
    (expect (char +path-document+ (xml-match-start match)) :to-equal #\/)))

(it "spans the whole content of an element that has child elements"
  (let ((match (first (path-select +path-document+ "/root/group/text()"))))
    (expect (xml-match-value match) :to-equal "")
    (expect (subseq +path-document+ (xml-match-start match) (xml-match-end match))
            :to-equal "<item id=\"3\">three</item><empty/>")))

(it "concatenates the direct text around child elements and decodes references"
  (let ((source "<m v=\"a&amp;b&#65;\">x&lt;<b/>y<![CDATA[<z>]]>&#x42;</m>"))
    (expect (xml-match-value (first (path-select source "/m/text()")))
            :to-equal "x<y<z>B")
    (expect (xml-match-value (first (path-select source "/m/@v")))
            :to-equal "a&bA")))

(it "resolves the canonical path of every match back to exactly that match"
  (dolist (path '("/root/*" "/root/group/*" "/root/item/@id" "/root/item/text()"))
    (dolist (match (path-select +path-document+ path))
      (let ((again (path-select +path-document+ (xml-match-path match))))
        (expect (length again) :to-equal 1)
        (expect (list (xml-match-kind (first again)) (xml-match-start (first again))
                      (xml-match-end (first again)))
                :to-equal (list (xml-match-kind match) (xml-match-start match)
                                (xml-match-end match)))))))

(it "indexes the caller's string when it starts with U+FEFF"
  (let* ((source (concatenate 'string +spans-bom+ +path-document+))
         (match (first (path-select source "/root/item[2]"))))
    (expect (subseq source (xml-match-start match) (xml-match-end match))
            :to-equal "<item id=\"2\">two</item>")
    (expect (xml-match-start match)
            :to-equal (1+ (search "<item id=\"2\">" +path-document+)))))

(it "returns fresh matches for every call"
  (expect (eq (first (path-select +path-document+ "/root"))
              (first (path-select +path-document+ "/root")))
          :to-equal nil))

;;; Entities that bring in markup

(defparameter +path-entity-document+
  "<!DOCTYPE r [<!ENTITY m \"<b/>\"><!ENTITY t \"plain\">]><r x=\"1\">&m;<s>&t;<c/></s></r>"
  "A document whose root references an entity that expands to an element.")

(defparameter +path-nested-entity-document+
  "<!DOCTYPE r [<!ENTITY m \"<b/>\"><!ENTITY t \"plain\">]><r><s>&t;<c/></s><u>&m;</u></r>"
  "A document in which only a nested element references that entity.")

(it "refuses to enumerate the children of an element that references a markup entity"
  (dolist (path '("/r/b" "/r/*" "/r/s" "/r/s/c"))
    (expect (path-refusal (lambda () (path-select +path-entity-document+ path)))
            :to-equal (list 'xml-unsupported-edit :entity-reference path))))

(it "still selects the attributes and text of an element that references a markup entity"
  (expect (path-summaries +path-entity-document+ "/r/@x")
          :to-equal '((:attribute "/r[1]/@x")))
  (expect (path-summaries +path-entity-document+ "/r/text()")
          :to-equal '((:text "/r[1]/text()")))
  (expect (path-summaries +path-entity-document+ "/r")
          :to-equal '((:element "/r[1]"))))

(it "enumerates the children of an element that references only a text entity"
  (expect (path-summaries +path-nested-entity-document+ "/r/s/*")
          :to-equal '((:element "/r[1]/s[1]/c[1]"))))

(it "refuses only the step below the nested element that references a markup entity"
  (expect (path-summaries +path-nested-entity-document+ "/r/u")
          :to-equal '((:element "/r[1]/u[1]")))
  (expect (path-refusal (lambda () (path-select +path-nested-entity-document+ "/r/u/*")))
          :to-equal '(xml-unsupported-edit :entity-reference "/r/u/*")))

;;; Malformed paths

(defun path-syntax-index (path)
  (handler-case (progn (select-xml "<a/>" path) :accepted)
    (xml-path-error (caught)
      (list (xml-edit-error-reason caught) (xml-path-error-index caught)
            (xml-edit-error-path caught)))))

(it "reports the index of a syntax error in the path"
  (dolist (case '(("" 0) ("a" 0) (" /a" 0) ("/" 1) ("/a/" 3) ("/a//b" 3)
                  ("/a b" 2) ("/a /b" 2) ("/*x" 2) ("/a*" 2) ("/1a" 1)
                  ("/a[0]" 3) ("/a[01]" 3) ("/a[x]" 3) ("/a[1" 4) ("/a[1]b" 5)
                  ("/a:" 3) ("/a:b:c" 4) ("/@id" 1) ("/text()" 5) ("/a/@" 4)
                  ("/a/@@id" 4) ("/a/@id/b" 6) ("/a/@id[1]" 6)
                  ("/a/text()/b" 9) ("/a/text()x" 9)))
    (expect (path-syntax-index (first case))
            :to-equal (list :syntax (second case) (first case)))))

(it "rejects a non-ASCII digit as a position"
  (expect (path-syntax-index (format nil "/a[~C]" (code-char #x661)))
          :to-equal (list :syntax 3 (format nil "/a[~C]" (code-char #x661)))))

(it "accepts every form of the path grammar"
  (dolist (path '("/a" "/a/b" "/*" "/a/*/c" "/a[1]" "/a/b[12]" "/p:a/q:b[3]" "/a/@id"
                  "/a/@p:id" "/a/text()" "/a/b[2]/@id" "/a.b/c-d/_e" "/a/b/text()"))
    (expect (path-syntax-index path) :to-equal :accepted)))

(it "rejects a path or source that is not a string with an input error"
  (expect (typep (handler-case (select-xml "<a/>" 42) (xml-kit-error (caught) caught))
                 'xml-input-error)
          :to-be-truthy)
  (expect (typep (handler-case (select-xml 42 "/a") (xml-kit-error (caught) caught))
                 'xml-input-error)
          :to-be-truthy))

;;; Source conditions and options

(it "signals the parse condition for a source that is not well-formed"
  (expect (typep (handler-case (select-xml "<a>" "/a") (xml-kit-error (caught) caught))
                 'xml-well-formedness-error)
          :to-be-truthy))

(it "applies the caller's resource limits"
  (expect (typep (handler-case
                     (select-xml "<a><b/></a>" "/a"
                                 :resource-limits (make-xml-resource-limits :max-depth 1))
                   (xml-kit-error (caught) caught))
                 'xml-resource-limit-exceeded)
          :to-be-truthy))

(it "applies the caller's external entity policy"
  (let ((source "<!DOCTYPE a SYSTEM \"a.dtd\"><a/>"))
    (expect (typep (handler-case (select-xml source "/a")
                     (xml-kit-error (caught) caught))
                   'xml-external-resource-disabled)
            :to-be-truthy)
    (expect (length (select-xml source "/a" :external-entity-policy :skip))
            :to-equal 1)))
