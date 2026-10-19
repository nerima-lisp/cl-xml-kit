;;;; t/helpers-corpus.lisp
;;;; Helpers for generated XML corpus trees and rendering.

(in-package #:cl-xml-kit/test)

;;; Generated element trees for property tests

(defun corpus-escape (text quote-char)
  "Return TEXT with &, <, >, and QUOTE-CHAR (when non-NIL) written as references."
  (with-output-to-string (stream)
    (loop for character across text
          do (case character
               (#\& (write-string "&amp;" stream))
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (t (if (eql character quote-char)
                      (write-string (if (char= quote-char #\") "&quot;" "&apos;") stream)
                      (write-char character stream)))))))

;;; Tag whitespace includes a line break so that CRLF and CR rendering moves
;;; offsets inside start tags, between attributes, and around the equals sign.
(defparameter +corpus-tag-spaces+ (list " " "  " (format nil "~%  ")))
(defparameter +corpus-equals+ (list "=" " = " (format nil "~%=")))

(defun corpus-gen-attributes ()
  "Generate up to three distinct attributes as (QNAME VALUE QUOTE-CHAR SPACE EQUALS) lists.
The names are fixed and each is present or absent, so no two can collide; the
value alphabet holds both quotes, & and <, a line feed, and a non-ASCII letter."
  (gen-map (lambda (choices)
             (loop for name in '("k" "p:m" "id")
                   for (present value quote-p space equals) in choices
                   when present
                     collect (list name value (if quote-p #\" #\') space equals)))
           (apply #'gen-tuple
                  (loop repeat 3
                        collect (gen-tuple (gen-boolean)
                                           (gen-string :max-length 6
                                                       :alphabet (format nil "a \"'&<~%~C"
                                                                         (code-char #xE9)))
                                           (gen-boolean)
                                           (gen-member +corpus-tag-spaces+)
                                           (gen-member +corpus-equals+))))))

(defun corpus-gen-leaf ()
  "Generate a text, comment, processing instruction, or CDATA node, or an empty element.
Comment and processing instruction alphabets omit - and ? so no delimiter can
appear early; the CDATA alphabet omits >, so ]]> cannot."
  (gen-one-of
   (gen-map (lambda (text) (list :text text))
            (gen-string :min-length 1 :max-length 6 :alphabet (format nil "xy &<>]~%")))
   (gen-map (lambda (text) (list :comment text)) (gen-string :max-length 4 :alphabet "c "))
   (gen-map (lambda (text) (list :pi text)) (gen-string :max-length 4 :alphabet "d "))
   (gen-map (lambda (text) (list :cdata text)) (gen-string :max-length 4 :alphabet "x<&]"))
   (gen-map (lambda (name) (list :element name nil nil))
            (gen-member '("a" "p:c" "x.y")))))

(defun corpus-gen-element (child)
  "Generate an element whose children come from the generator CHILD."
  (gen-map (lambda (parts) (cons :element parts))
           (gen-tuple (gen-member '("a" "b" "p:c" "d-e" "x.y"))
                      (corpus-gen-attributes)
                      (gen-list child :max-length 3))))

(defun corpus-gen-document ()
  "Generate a document element tree with at most four levels of elements, and a line break style."
  (gen-tuple (corpus-gen-element
              (gen-recursive (corpus-gen-leaf) #'corpus-gen-element :max-depth 2))
             (gen-member '(:lf :crlf :cr))))

(defun corpus-render-node (node stream)
  "Write NODE, a generated tree node, to STREAM as XML, leaving line breaks as line feeds for CORPUS-RENDER to convert."
  (destructuring-bind (kind &rest parts) node
    (ecase kind
      (:text (write-string (corpus-escape (first parts) nil) stream))
      (:comment (format stream "<!--~A-->" (first parts)))
      (:pi (format stream "<?pi ~A?>" (first parts)))
      (:cdata (format stream "<![CDATA[~A]]>" (first parts)))
      (:element
       (destructuring-bind (name attributes children) parts
         (format stream "<~A" name)
         (loop for (qname value quote-char space equals) in attributes
               do (format stream "~A~A~A~C~A~C" space qname equals quote-char
                          (corpus-escape value quote-char) quote-char))
         (if children
             (progn (write-char #\> stream)
                    (dolist (child children) (corpus-render-node child stream))
                    (format stream "</~A>" name))
             (write-string "/>" stream)))))))

(defun corpus-render (document)
  "Return the generated DOCUMENT, (TREE STYLE), as an XML string with STYLE line breaks.
The document element declares the prefix p that generated names use."
  (destructuring-bind (tree style) document
    (destructuring-bind (name attributes children) (rest tree)
      (let ((break (spans-line-break style)))
        (with-output-to-string (out)
          (loop for character
                  across (with-output-to-string (stream)
                           (corpus-render-node
                            (list :element name
                                  (cons (list "xmlns:p" "urn:p" #\" " " "=") attributes)
                                  children)
                            stream))
                do (if (char= character #\Linefeed)
                       (write-string break out)
                       (write-char character out))))))))
