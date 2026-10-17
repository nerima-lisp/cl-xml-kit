(in-package #:cl-xml-kit/test)

(defparameter +xml-namespace+ "http://www.w3.org/XML/1998/namespace"
  "The namespace URI that Namespaces in XML 1.0 section 3 binds to the xml prefix.")

(defun xml-name-parts (name)
  "Return (LOCAL-NAME NAMESPACE-URI PREFIX) of the XML-NAME NAME."
  (list (xml-name-local-name name) (xml-name-namespace-uri name) (xml-name-prefix name)))

(defmatcher :to-be-xml-name (name expected)
  (:description "Passes when NAME is an XML-NAME whose (LOCAL-NAME NAMESPACE-URI PREFIX) "
                "equals EXPECTED under EQUAL. Any other object fails with the object "
                "itself as the actual value.")
  (let ((parts (first expected)))
    (if (typep name 'xml-name)
        (let ((actual (xml-name-parts name)))
          (values (equal actual parts) actual parts))
        (values nil name parts))))
