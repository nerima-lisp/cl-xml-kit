(in-package #:cl-xml-kit/test)

(defun parsed-root (source &rest parse-options)
  "Parse SOURCE with PARSE-XML and PARSE-OPTIONS and return its root element.
A refused document signals what PARSE-XML signals, so the calling test fails
with that condition; a document without an element returns NIL."
  (find-if #'xml-element-p
           (xml-document-children (apply #'parse-xml source parse-options))))

(defun parsed-root-text (source &rest parse-options)
  "Return the concatenated data of the text children of SOURCE's root element.
Element children contribute nothing, so markup that an entity was expected to
produce shows up as missing text rather than as an error."
  (apply #'concatenate 'string
         (mapcar #'xml-text-data
                 (remove-if-not #'xml-text-p
                                (xml-element-children
                                 (apply #'parsed-root source parse-options))))))

(defun parsed-root-attribute (source local-name &rest parse-options)
  "Return the value of the root attribute of SOURCE whose local name is LOCAL-NAME.
Return NIL when the root has no such attribute."
  (let ((attribute (find local-name
                         (xml-element-attributes
                          (apply #'parsed-root source parse-options))
                         :key (lambda (attribute)
                                (xml-name-local-name (xml-attribute-name attribute)))
                         :test #'string=)))
    (and attribute (xml-attribute-value attribute))))

(defun subset-declaration (subset name &rest subset-options)
  "Parse the internal subset SUBSET and return the general entity declaration NAME.
Return NIL when SUBSET declares no general entity NAME; a refused subset
signals what PARSE-XML-INTERNAL-SUBSET signals."
  (gethash name (cl-xml-kit::xml-entity-context-declarations
                 (apply #'cl-xml-kit::parse-xml-internal-subset subset subset-options))))

(defun name-with-code-points (&rest code-points)
  "Return the string whose characters have CODE-POINTS, which must all be valid codes."
  (map 'string #'code-char code-points))
