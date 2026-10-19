;;;; t/helpers-serializer.lisp
;;;;
;;;; Node specs, round-trip signatures, and an independent XML source renderer
;;;; for property tests. A node spec is literal data, so it can sit in an IT-EACH row:
;;;;
;;;;   "data" or (:text DATA [FORM])  a text node; FORM (:plain, :reference, or
;;;;                                  :cdata) only chooses how RENDER-SPEC writes it
;;;;   (:element LOCAL-NAME &key URI PREFIX ATTRIBUTES DECLARATIONS CHILDREN)
;;;;   (:attribute LOCAL-NAME VALUE &key URI PREFIX)
;;;;   (:comment DATA)  (:pi TARGET [DATA])
;;;;   (:doctype ROOT-NAME &key PUBLIC SYSTEM SUBSET)
;;;;   (:document CHILD...)
;;;;   (:object VALUE)                VALUE itself, for an argument that is no node
;;;;
;;;; DECLARATIONS holds (PREFIX URI) lists or (:object VALUE).

(in-package #:cl-xml-kit/test)

(defun binding-from-spec (spec)
  "Return the namespace binding (PREFIX URI) that SPEC describes, or the VALUE of (:object VALUE).
A prefixed binding with an empty URI signals the XML-NAMESPACE-ERROR of MAKE-XML-NAMESPACE-BINDING."
  (if (eq (first spec) :object)
      (second spec)
      (make-xml-namespace-binding :prefix (first spec) :namespace-uri (second spec))))

(defun node-from-spec (spec)
  "Return the DOM node that the node spec SPEC describes.
An unknown spec head signals a CASE-FAILURE, so a malformed table row fails its
test instead of building some other tree."
  (if (stringp spec)
      (make-xml-text :data spec)
      (destructuring-bind (kind &rest arguments) spec
        (ecase kind
          (:text (make-xml-text :data (first arguments)))
          (:element
           (destructuring-bind (local-name &key (uri "") prefix attributes declarations
                                                 children)
               arguments
             (make-xml-element
              :name (make-xml-name :local-name local-name :namespace-uri uri :prefix prefix)
              :attributes (mapcar #'node-from-spec attributes)
              :namespace-declarations (mapcar #'binding-from-spec declarations)
              :children (mapcar #'node-from-spec children))))
          (:attribute
           (destructuring-bind (local-name value &key (uri "") prefix) arguments
             (make-xml-attribute
              :name (make-xml-name :local-name local-name :namespace-uri uri :prefix prefix)
              :value value)))
          (:comment (make-xml-comment :data (first arguments)))
          (:pi (make-xml-processing-instruction :target (first arguments)
                                                :data (second arguments)))
          (:doctype
           (destructuring-bind (root-name &key public system subset) arguments
             (make-xml-doctype :root-name root-name :public-id public :system-id system
                               :internal-subset subset)))
          (:document (make-xml-document :children (mapcar #'node-from-spec arguments)))
          (:object (first arguments))))))

;;; Round-trip signatures

(defun merge-text-signatures (signatures)
  "Return SIGNATURES with adjacent (:TEXT ...) entries joined and empty text dropped.
It never fails; entries of other kinds pass through unchanged."
  (let ((merged nil))
    (dolist (signature signatures)
      (cond
        ((not (eq (first signature) :text)) (push signature merged))
        ((zerop (length (second signature))))
        ((eq (first (first merged)) :text)
         (setf (first merged)
               (list :text (concatenate 'string (second (first merged))
                                        (second signature)))))
        (t (push signature merged))))
    (nreverse merged)))

(defun round-trip-signature (node &key (comments t))
  "Return a value that is EQUAL for trees with the same XML information.
Prefixes, namespace declarations, attribute order, positions, and how text was
split into nodes do not count; COMMENTS NIL also ignores comments, as canonical
output does. A node of an unknown type yields (:UNKNOWN), which matches no real
signature."
  (flet ((children (nodes)
           (merge-text-signatures
            (loop for child in nodes
                  unless (and (not comments) (xml-comment-p child))
                    collect (round-trip-signature child :comments comments)))))
    (cond
      ((xml-document-p node) (cons :document (children (xml-document-children node))))
      ((xml-element-p node)
       (let ((name (xml-element-name node)))
         (list :element (xml-name-namespace-uri name) (xml-name-local-name name)
               (sort (mapcar (lambda (attribute)
                               (let ((name (xml-attribute-name attribute)))
                                 (list (xml-name-namespace-uri name)
                                       (xml-name-local-name name)
                                       (xml-attribute-value attribute))))
                             (xml-element-attributes node))
                     (lambda (left right)
                       (or (string< (first left) (first right))
                           (and (string= (first left) (first right))
                                (string< (second left) (second right))))))
               (children (xml-element-children node)))))
      ((xml-text-p node) (list :text (xml-text-data node)))
      ((xml-comment-p node) (list :comment (xml-comment-data node)))
      ((xml-processing-instruction-p node)
       (list :pi (xml-processing-instruction-target node)
             (or (xml-processing-instruction-data node) "")))
      ((xml-doctype-p node) (list :doctype (xml-doctype-root-name node)))
      (t (list :unknown)))))

(defun document-signature (node &key (comments t))
  "Return the ROUND-TRIP-SIGNATURE of NODE as the document that parsing its serialization yields.
A NODE that is not a document is wrapped as the only child, so a doctype or a
comment yields a document no parse can produce and its comparison fails."
  (if (xml-document-p node)
      (round-trip-signature node :comments comments)
      (cons :document (merge-text-signatures
                       (list (round-trip-signature node :comments comments))))))

(defmatcher :to-round-trip (node expected)
  (:description "Passes when NODE, serialized in the mode EXPECTED names (:ORDINARY "
                "through XML->STRING or :CANONICAL through XML->CANONICAL-STRING) and "
                "parsed again, has the DOCUMENT-SIGNATURE of NODE; the canonical "
                "comparison ignores comments. A mismatch fails with both signatures; "
                "a serialization or parse error propagates and fails the test.")
  (let* ((mode (first expected))
         (comments (ecase mode (:ordinary t) (:canonical nil)))
         (output (ecase mode
                   (:ordinary (xml->string node))
                   (:canonical (xml->canonical-string node))))
         (want (document-signature node :comments comments))
         (actual (round-trip-signature (parse-xml output) :comments comments)))
    (values (equal actual want) actual want)))

;;; Rendering a spec as XML source, independently of the serializer

(defun render-character-reference (character stream)
  "Write CHARACTER to STREAM as a hexadecimal character reference; it never fails."
  (format stream "&#x~X;" (char-code character)))

(defun render-escaped (string stream escapes)
  "Write STRING to STREAM, writing each character listed in ESCAPES as a character reference.
It does not check that the result is well-formed; the parse that follows does."
  (loop for character across string
        do (if (find character escapes)
               (render-character-reference character stream)
               (write-char character stream))))

(defun render-cdata (string stream)
  "Write STRING as CDATA sections. A carriage return, which a CDATA section cannot
keep, goes between sections as a reference, and ]]> is split across two sections."
  (write-string "<![CDATA[" stream)
  (loop for index from 0 below (length string)
        for character = (char string index)
        do (cond
             ((char= character #\Return)
              (write-string "]]>&#13;<![CDATA[" stream))
             ((and (char= character #\>) (>= index 2)
                   (string= "]]" string :start2 (- index 2) :end2 index))
              (write-string "]]><![CDATA[>" stream))
             (t (write-char character stream))))
  (write-string "]]>" stream))

(defun render-qname (prefix local-name stream)
  "Write PREFIX:LOCAL-NAME, or LOCAL-NAME alone when PREFIX is NIL, to STREAM; it never fails."
  (when prefix
    (write-string prefix stream)
    (write-char #\: stream))
  (write-string local-name stream))

(defun render-element (spec stream)
  "Write the element SPEC with a declaration for its own namespace and each attribute prefix.
SPEC must come from WELL-FORMED-SPEC; a prefix bound twice on one element is written
twice and the parse that follows refuses it."
  (destructuring-bind (local-name &key (uri "") prefix attributes children) (rest spec)
    (write-char #\< stream)
    (render-qname prefix local-name stream)
    (format stream " xmlns~@[:~A~]=\"~A\"" prefix uri)
    (let ((declared (list prefix "xml")))
      (dolist (attribute attributes)
        (destructuring-bind (name value &key uri prefix) (rest attribute)
          (declare (ignore name value))
          (unless (or (null prefix) (member prefix declared :test #'equal))
            (push prefix declared)
            (format stream " xmlns:~A=\"~A\"" prefix uri)))))
    (dolist (attribute attributes)
      (destructuring-bind (name value &key prefix &allow-other-keys) (rest attribute)
        (write-char #\Space stream)
        (render-qname prefix name stream)
        (write-string "=\"" stream)
        (render-escaped value stream '(#\& #\< #\" #\Tab #\Newline #\Return))
        (write-char #\" stream)))
    (write-char #\> stream)
    (dolist (child children)
      (render-node child stream))
    (write-string "</" stream)
    (render-qname prefix local-name stream)
    (write-char #\> stream)))

(defun render-node (spec stream)
  "Write the node spec SPEC to STREAM; a spec head or text form it does not know signals a CASE-FAILURE."
  (ecase (first spec)
    (:element (render-element spec stream))
    (:text
     (destructuring-bind (data form) (rest spec)
       (ecase form
         (:plain (render-escaped data stream '(#\& #\< #\> #\Return)))
         (:reference (render-escaped data stream data))
         (:cdata (render-cdata data stream)))))
    (:comment (format stream "<!--~A-->" (second spec)))
    (:pi (format stream "<?~A~@[ ~A~]?>" (second spec) (third spec)))
    (:document (dolist (child (rest spec)) (render-node child stream)))))

(defun render-spec (spec)
  "Return XML source text for the node spec SPEC, written by hand rather than by the serializer.
Text and attribute values escape with references, so the source parses to the tree
SPEC describes; an element without a namespace gets xmlns=\"\"."
  (with-output-to-string (stream)
    (render-node spec stream)))
