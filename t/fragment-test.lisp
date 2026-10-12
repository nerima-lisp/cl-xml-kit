;;;; t/fragment-test.lisp

(in-package #:cl-xml-kit/test)

(defun fragment-context (source &rest options)
  "Return a path context for SOURCE; OPTIONS override the default limits and policy."
  (apply #'cl-xml-kit::%path-context source "/target"
         (append options
                 (list :resource-limits *default-xml-resource-limits*
                       :external-entity-policy :reject))))

(defun fragment-chain (source)
  "Return the DOM elements from the innermost element of SOURCE outward, where SOURCE nests one child per level."
  (let ((chain nil)
        (element (first (xml-document-children (parse-xml source)))))
    (loop while element
          do (push element chain)
             (setf element (find-if #'xml-element-p (xml-element-children element))))
    chain))

(defun fragment-failure (thunk)
  "Return (REASON CAUSE-TYPE) of the XML-EDIT-VALUE-ERROR that THUNK signals, or (:returned)."
  (handler-case (progn (funcall thunk) (list :returned))
    (xml-edit-value-error (caught)
      (list (xml-edit-error-reason caught)
            (let ((cause (xml-edit-value-error-cause caught)))
              (and cause (type-of cause)))))))

;;; Line breaks

(it "converts every line break of a string to the requested style"
  (let ((input (format nil "a~C~Cb~Cc~Cd" #\Return #\Linefeed #\Return #\Linefeed)))
    (expect (cl-xml-kit::%edit-convert-line-breaks input :lf)
            :to-equal (format nil "a~Cb~Cc~Cd" #\Linefeed #\Linefeed #\Linefeed))
    (expect (cl-xml-kit::%edit-convert-line-breaks input :crlf)
            :to-equal (format nil "a~C~Cb~C~Cc~C~Cd" #\Return #\Linefeed #\Return #\Linefeed
                              #\Return #\Linefeed))
    (expect (cl-xml-kit::%edit-convert-line-breaks input :cr)
            :to-equal (format nil "a~Cb~Cc~Cd" #\Return #\Return #\Return))))

(it "leaves a string without line breaks as it is"
  (expect (cl-xml-kit::%edit-convert-line-breaks "plain text" :crlf) :to-equal "plain text"))

;;; Namespace scope

(it "collects the bindings in scope, letting an inner declaration replace an outer one"
  (let ((chain (fragment-chain "<a xmlns=\"d\" xmlns:p=\"u1\"><b xmlns:p=\"u2\" xmlns:q=\"u3\"><c/></b></a>")))
    (expect (sort (copy-list (cl-xml-kit::%edit-scope-bindings chain))
                  #'string< :key (lambda (binding) (or (car binding) "")))
            :to-equal '((nil . "d") ("p" . "u2") ("q" . "u3")))))

(it "leaves out an undeclared default namespace"
  (expect (cl-xml-kit::%edit-scope-bindings
           (fragment-chain "<a xmlns=\"d\"><b xmlns=\"\"><c/></b></a>"))
          :to-equal nil))

(it "has no bindings outside every element"
  (expect (cl-xml-kit::%edit-scope-bindings nil) :to-equal nil))

(it "wraps a fragment in an element that declares the bindings"
  (expect (cl-xml-kit::%edit-fragment-wrapper '((nil . "urn:d") ("p" . "urn:p")) "<a/>")
          :to-equal "<w xmlns=\"urn:d\" xmlns:p=\"urn:p\"><a/></w>")
  (expect (cl-xml-kit::%edit-fragment-wrapper nil "<a/>") :to-equal "<w><a/></w>"))

(it "escapes a namespace URI in the wrapper"
  (expect (cl-xml-kit::%edit-fragment-wrapper '(("p" . "a\"b&c<d")) "<a/>")
          :to-equal "<w xmlns:p=\"a&quot;b&amp;c&lt;d\"><a/></w>"))

;;; Parsing a fragment

(defparameter +fragment-source+
  "<target xmlns=\"urn:d\" xmlns:p=\"urn:p\"><inner/></target>")

(defun fragment-element (fragment &rest options)
  (let* ((context (apply #'fragment-context +fragment-source+ options))
         (chain (fragment-chain +fragment-source+)))
    (cl-xml-kit::%edit-fragment-element context chain fragment)))

(it "returns the one element of a fragment and the fragment without surrounding whitespace"
  (multiple-value-bind (element text)
      (fragment-element (format nil " ~C~C<a b=\"1\"/>~C " #\Return #\Linefeed #\Tab))
    (expect (xml-name-local-name (xml-element-name element)) :to-equal "a")
    (expect text :to-equal "<a b=\"1\"/>")))

(it "puts an unprefixed fragment element in the default namespace in scope"
  (expect (xml-name-namespace-uri (xml-element-name (fragment-element "<a/>")))
          :to-equal "urn:d"))

(it "resolves a prefix declared where the fragment is written"
  (let ((name (xml-element-name (fragment-element "<p:a/>"))))
    (expect (list (xml-name-prefix name) (xml-name-namespace-uri name))
            :to-equal '("p" "urn:p"))))

(it "keeps a namespace declaration that the fragment makes itself"
  (let ((element (fragment-element "<q:a xmlns:q=\"urn:q\"/>")))
    (expect (xml-name-namespace-uri (xml-element-name element)) :to-equal "urn:q")))

(it "refuses a fragment that is not exactly one element"
  (dolist (fragment '("" "   " "text" "<a/><b/>" "<a/> tail" "head <a/>" "<!--c--><a/>"
                      "<a/><!--c-->" "<?p x?><a/>"))
    (expect (fragment-failure (lambda () (fragment-element fragment)))
            :to-equal '(:not-single-element nil))))

(it "refuses a fragment that does not parse and keeps the parse condition"
  (expect (fragment-failure (lambda () (fragment-element "<a>")))
          :to-equal '(:malformed-fragment xml-well-formedness-error))
  (expect (fragment-failure (lambda () (fragment-element "<a></b>")))
          :to-equal '(:malformed-fragment xml-well-formedness-error)))

(it "refuses a prefix that is not in scope"
  (let ((failure (fragment-failure (lambda () (fragment-element "<z:a/>")))))
    (expect (first failure) :to-equal :malformed-fragment)
    (expect (subtypep (second failure) 'xml-parse-error) :to-be-truthy)))

(it "refuses a DOCTYPE or an XML declaration in a fragment"
  (dolist (fragment '("<!DOCTYPE a><a/>" "<!DOCTYPE a [<!ENTITY x SYSTEM \"x.ent\">]><a>&x;</a>"
                      "<?xml version=\"1.0\"?><a/>"))
    (expect (first (fragment-failure (lambda () (fragment-element fragment))))
            :to-equal :malformed-fragment)
    (expect (first (fragment-failure
                    (lambda () (fragment-element fragment :external-entity-policy :skip))))
            :to-equal :malformed-fragment)))

(it "refuses a reference to an entity that the fragment does not declare"
  (expect (fragment-failure (lambda () (fragment-element "<a>&xxe;</a>")))
          :to-equal '(:malformed-fragment xml-well-formedness-error)))

(it "refuses a fragment that tries to close the wrapper"
  (dolist (fragment '("</w><a/>" "a</w><w>b" "<a/></w><w>"))
    (expect (fragment-failure (lambda () (fragment-element fragment)))
            :to-equal '(:malformed-fragment xml-well-formedness-error))))

(it "accepts a fragment that is within the limits of the caller"
  (expect (fragment-failure (lambda () (fragment-element "<a x=\"1\" y=\"2\"/>")))
          :to-equal '(:returned)))

(it "refuses a fragment that breaks a resource limit of the caller"
  (let ((limits (make-xml-resource-limits :max-attributes-per-element 2)))
    (expect (fragment-failure
             (lambda ()
               (fragment-element "<a x=\"1\" y=\"2\" z=\"3\"/>" :resource-limits limits)))
            :to-equal '(:malformed-fragment xml-resource-limit-exceeded))))
