;;;; t/helpers-serializer-generators.lisp
;;;;
;;;; Generators and normalization helpers for serializer and format property tests.

(in-package #:cl-xml-kit/test)

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
