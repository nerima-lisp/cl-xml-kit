;;;; src/data/parser-token-rules.lisp

(in-package #:cl-xml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +xml-parser-token-rules+
    '((:content
       ((:text :cdata) %parser-content-character-data)
       (:entity-reference %parser-entity-reference)
       (:comment %parser-content-comment)
       (:processing-instruction %parser-pi)
       (:start-tag-open %parser-content-start-tag)
       (:end-tag-open %parser-content-end-tag)
       (:eof %parser-content-end)
       (:otherwise %parser-content-unexpected-token))
      (:top-level
       (:text %parser-top-level-text)
       (:entity-reference %parser-top-level-reference)
       (:comment %parser-top-level-comment)
       (:processing-instruction %parser-top-level-pi)
       (:cdata %parser-top-level-cdata)
       (:doctype %parser-top-level-doctype)
       (:start-tag-open %parser-root-element)
       (:end-tag-open %parser-top-level-end-tag)
       (:eof %parser-end-document)
       (:otherwise nil)))))
