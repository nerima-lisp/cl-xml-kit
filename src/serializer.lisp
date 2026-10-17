;;;; src/serializer.lisp

(in-package #:cl-xml-kit)

(defun %serializer-state (stream &key pretty-print indent resource-limits canonical)
  (unless (streamp stream)
    (%serializer-error stream nil (xml-message :stream-required)))
  (unless (xml-resource-limits-p resource-limits)
    (%serializer-error resource-limits nil
                       (xml-message :label-must-be-resource-limits-value
                                    "RESOURCE-LIMITS")))
  (unless (and (integerp indent) (not (minusp indent)))
    (%serializer-error indent nil (xml-message :indent-must-be-non-negative)))
  (%make-xml-serializer-state stream resource-limits pretty-print indent canonical 0 0))

(defun %write-xml (object stream &key (pretty-print nil) (indent 2)
                          resource-limits (canonical nil))
  (unless (xml-node-p object)
    (%serializer-error object nil (xml-message :object-not-serializable)))
  (let ((state (%serializer-state stream :pretty-print pretty-print
                                  :indent indent :resource-limits resource-limits
                                  :canonical canonical)))
    (%write-node state object (list (cons "xml" +xml-namespace-uri+)) 0 nil)
    stream))

(defun write-xml (object stream &key (pretty-print nil) (indent 2)
                                   (resource-limits *default-xml-resource-limits*))
  "Write OBJECT as XML to character STREAM and return STREAM."
  (%write-xml object stream :pretty-print pretty-print :indent indent
              :resource-limits resource-limits :canonical nil))

(defun xml->string (object &key (pretty-print nil) (indent 2)
                                (resource-limits *default-xml-resource-limits*))
  "Return OBJECT serialized as a fresh XML string."
  (with-output-to-string (stream)
    (write-xml object stream :pretty-print pretty-print :indent indent
               :resource-limits resource-limits)))

(defun write-xml-canonical (object stream
                             &key (resource-limits *default-xml-resource-limits*))
  "Write OBJECT in XML Test Suite Second Canonical Form to STREAM."
  (%write-xml object stream :resource-limits resource-limits :canonical t))

(defun xml->canonical-string
    (object &key (resource-limits *default-xml-resource-limits*))
  "Return OBJECT in XML Test Suite Second Canonical Form as a fresh string."
  (with-output-to-string (stream)
    (write-xml-canonical object stream :resource-limits resource-limits)))

