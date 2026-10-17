;;;; t/helpers-serializer.lisp
;;;;
;;;; Node specs, round-trip signatures, and the generators of the serializer
;;;; properties. A node spec is literal data, so it can sit in an IT-EACH row:
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

;;; Generators

(defparameter +gen-text-alphabet+
  (coerce (list #\a #\& #\< #\> #\] #\" #\' #\Space #\Tab #\Newline #\Return
                (code-char #xE9) (code-char #x1F600) (code-char #x10000))
          'string)
  "Characters for text and attribute values: every character a text or attribute
context escapes, the ]]> sequence, the three white space characters that attribute
normalization would rewrite, and a BMP and two supplementary characters.")

(defparameter +gen-markup-alphabet+
  (coerce (list #\a #\Space #\< #\& #\> #\] (code-char #xE9) (code-char #x1F600))
          'string)
  "Characters for comment and processing-instruction data. Hyphen, question mark, and
carriage return are left out: comments cannot hold -- or end in a hyphen, PI data
cannot hold ?>, and neither can keep a carriage return through parsing.")

(defparameter +gen-element-namespaces+
  '((nil "") (nil "urn:a") (nil "urn:b") ("p" "urn:a") ("p" "urn:c") ("q" "urn:b"))
  "Element (PREFIX URI) choices: no namespace, two default namespaces, and prefixes,
one bound to two URIs so that descendants redeclare it.")

(defparameter +gen-attribute-namespaces+
  '((nil "") ("r" "urn:a") ("r" "urn:d") ("q" "urn:b") ("xml" "http://www.w3.org/XML/1998/namespace")
    (nil "urn:a") (nil "urn:e"))
  "Attribute (PREFIX URI) choices. The unprefixed namespaced ones exist only in a DOM;
WELL-FORMED-SPEC drops their URI when the tree is to be written as source.")

(defun gen-node-spec ()
  "Return a generator of raw element-content specs: text, comments, PIs, and nested elements up to depth 3.
Its values are raw: pass them through WELL-FORMED-SPEC before building or rendering."
  (gen-recursive
   (gen-one-of
    (gen-tuple (gen-member '(:text)) (gen-string :max-length 6 :alphabet +gen-text-alphabet+)
               (gen-member '(:plain :reference :cdata)))
    (gen-tuple (gen-member '(:comment)) (gen-string :max-length 5 :alphabet +gen-markup-alphabet+))
    (gen-tuple (gen-member '(:pi)) (gen-member '("pi" "x-y" "t.1"))
               (gen-one-of (gen-member '(nil))
                           (gen-string :max-length 5 :alphabet +gen-markup-alphabet+))))
   (lambda (self) (gen-raw-element (gen-list self :max-length 4)))
   :max-depth 3))

(defun gen-raw-element (children)
  "Return a generator of raw element specs (:ELEMENT LOCAL-NAME (PREFIX URI) ATTRIBUTES CHILDREN).
CHILDREN is the generator of the child list; it is not checked."
  (gen-tuple (gen-member '(:element))
             (gen-member (list "a" "item" "x-y" "_z" "n.1" (string (code-char #xE9))))
             (gen-member +gen-element-namespaces+)
             (gen-list (gen-tuple (gen-member '("id" "k" "space" "v-1"))
                                  (gen-member +gen-attribute-namespaces+)
                                  (gen-one-of
                                   (gen-string :max-length 6 :alphabet +gen-text-alphabet+)
                                   (gen-member '("preserve" "default"))))
                       :max-length 3)
             children))

(defun gen-document-spec ()
  "Return a generator of raw document specs (MISC-BEFORE ROOT MISC-AFTER), for GENERATED-DOCUMENT."
  (let ((misc (gen-list (gen-one-of
                         (gen-tuple (gen-member '(:comment))
                                    (gen-string :max-length 5 :alphabet +gen-markup-alphabet+))
                         (gen-tuple (gen-member '(:pi)) (gen-member '("pi" "z"))
                                    (gen-string :max-length 4 :alphabet +gen-markup-alphabet+)))
                        :max-length 2)))
    (gen-tuple misc (gen-raw-element (gen-list (gen-node-spec) :max-length 4)) misc)))

(defun well-formed-attributes (raw-attributes element-prefix element-uri source)
  "Return node specs of RAW-ATTRIBUTES without repeated expanded names or prefixes rebound on one element.
A raw attribute that is not (LOCAL-NAME (PREFIX URI) VALUE) signals a destructuring error."
  (let ((bound (list (cons element-prefix element-uri)))
        (seen nil)
        (kept nil))
    (loop for (local-name (prefix uri) value) in raw-attributes
          do (when (and source (null prefix))
               (setf uri ""))
             (let ((binding (assoc prefix bound :test #'equal)))
               (unless (or (member (cons uri local-name) seen :test #'equal)
                           (and prefix binding (string/= (cdr binding) uri)))
                 (push (cons uri local-name) seen)
                 (when prefix (push (cons prefix uri) bound))
                 (push (list :attribute local-name value :uri uri :prefix prefix) kept))))
    (nreverse kept)))

(defun well-formed-spec (raw &key source)
  "Return the node spec of the raw generated spec RAW, made namespace-well-formed.
SOURCE true prepares it for RENDER-SPEC, which cannot write an unprefixed
namespaced attribute; PI data loses its leading white space, which parsing drops.
A raw spec of an unknown kind signals a CASE-FAILURE."
  (ecase (first raw)
    (:element
     (destructuring-bind (local-name (prefix uri) attributes children) (rest raw)
       (list :element local-name :uri uri :prefix prefix
             :attributes (well-formed-attributes attributes prefix uri source)
             :children (mapcar (lambda (child) (well-formed-spec child :source source))
                               children))))
    (:text raw)
    (:comment raw)
    (:pi (destructuring-bind (target data) (rest raw)
           (list :pi target (and data (string-left-trim '(#\Space) data)))))))

(defun generated-document (raw &key source)
  "Return the (:DOCUMENT ...) node spec of the raw document spec RAW from GEN-DOCUMENT-SPEC.
A RAW that is not a three-element list signals a destructuring error."
  (destructuring-bind (before root after) raw
    `(:document ,@(mapcar #'well-formed-spec before)
                ,(well-formed-spec root :source source)
                ,@(mapcar #'well-formed-spec after))))
