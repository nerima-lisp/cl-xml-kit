;;;; src/data/serializer-node-rules.lisp

(in-package #:cl-xml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +xml-serializer-node-rules+
    '((xml-document-p %write-document)
      (xml-element-p %write-element)
      (xml-attribute-p %write-attribute-node)
      (xml-text-p %write-text-node)
      (xml-comment-p %write-comment-node)
      (xml-processing-instruction-p %write-processing-instruction-node)
      (xml-doctype-p %write-doctype-node)
      (:otherwise %write-doctype-node))))
