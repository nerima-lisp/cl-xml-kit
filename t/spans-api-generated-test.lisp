;;;; t/spans-api-generated-test.lisp
;;;;
;;;; Span invariants over the fixed-seed mutation corpus and over generated
;;;; element trees in every line-break style.

(in-package #:cl-xml-kit/test)

(it "generates enough accepted mutations to exercise the scanner"
  (expect (length (spans-generated-documents)) :to-be-greater-than-or-equal 200))

(it "keeps every span of the fixture and the generated documents consistent"
  (expect (loop for source in (cons +spans-fixture+ (spans-generated-documents))
                for problems = (spans-structure-problems
                                source (cl-xml-kit::span-document-root (spans-scan source)))
                when problems
                  collect (list source problems))
          :to-equal nil))

(it "matches the DOM element tree for generated documents without markup entities"
  (expect (loop for source in (spans-generated-documents)
                for (spans dom) = (spans-agreement source)
                unless (or (cl-xml-kit::span-document-markup-entity-names (spans-scan source))
                           (equal spans dom))
                  collect source)
          :to-equal nil))

(it "scans generated documents behind a leading U+FEFF at shifted offsets"
  (expect (loop for source in (spans-generated-documents)
                when (spans-shift-problems source)
                  collect source)
          :to-equal nil))

(it-property "scans generated element trees in every line-break style consistently with the DOM"
    ((document (corpus-gen-document)))
  (let ((source (corpus-render document)))
    (expect (spans-structure-problems source (spans-root source)) :to-equal nil)
    (destructuring-bind (spans dom) (spans-agreement source)
      (expect spans :to-equal dom))
    (expect (spans-shift-problems source) :to-equal nil)))

(it-fuzz "calls exactly one scanner callback for markup-like inputs"
    ((source (gen-string :max-length 40 :alphabet "<>/!?&;#:= abcXYZ")))
    (:trials 300)
  (let ((callback-count 0))
    (cl-xml-kit::scan-xml-spans
     source
     (lambda (document)
       (incf callback-count)
       (expect (cl-xml-kit::span-document-root document) :to-be-truthy))
     (lambda (condition)
       (incf callback-count)
       (expect (typep condition (find-class (quote condition)))
               :to-be-truthy)))
    (expect callback-count :to-equal 1)))
(it "initializes every reachable span record default"
  (let ((document (cl-xml-kit::%make-span-document))
        (element (cl-xml-kit::%make-span-element))
        (frame (cl-xml-kit::%make-span-frame "a" 1 2 nil 3))
        (run (cl-xml-kit::%make-span-run 4))
        (scanner (cl-xml-kit::%make-span-scanner "x" nil 0)))
    (expect
     (list (cl-xml-kit::span-document-declaration document)
           (cl-xml-kit::span-document-doctype document)
           (cl-xml-kit::span-document-children document)
           (cl-xml-kit::span-document-root document)
           (cl-xml-kit::span-document-line-break document)
           (cl-xml-kit::span-document-markup-entity-names document)
           (cl-xml-kit::span-document-attlist-element-names document)
           (cl-xml-kit::span-node-start element)
           (cl-xml-kit::span-node-end element)
           (cl-xml-kit::span-element-qname element)
           (cl-xml-kit::span-element-name-end element)
           (cl-xml-kit::span-element-attributes element)
           (cl-xml-kit::span-element-empty-p element)
           (cl-xml-kit::span-element-start-tag-end element)
           (cl-xml-kit::span-element-content-start element)
           (cl-xml-kit::span-element-content-end element)
           (cl-xml-kit::span-element-end-tag-start element)
           (cl-xml-kit::span-element-end-tag-end element)
           (cl-xml-kit::span-element-children element)
           (cl-xml-kit::span-frame-children frame)
           (cl-xml-kit::span-frame-run frame)
           (cl-xml-kit::span-run-end run)
           (cl-xml-kit::span-run-token-count run)
           (cl-xml-kit::span-run-cdata-count run)
           (cl-xml-kit::span-run-char-ref-p run)
           (cl-xml-kit::span-run-predefined-ref-p run)
           (cl-xml-kit::span-run-entity-names run)
           (cl-xml-kit::span-scanner-open-token scanner)
           (cl-xml-kit::span-scanner-name-token scanner)
           (cl-xml-kit::span-scanner-attribute-name scanner)
           (cl-xml-kit::span-scanner-attributes scanner)
           (cl-xml-kit::span-scanner-space-start scanner)
           (cl-xml-kit::span-scanner-stack scanner)
           (cl-xml-kit::span-scanner-nodes scanner)
           (cl-xml-kit::span-scanner-declaration scanner)
           (cl-xml-kit::span-scanner-doctype scanner)
           (cl-xml-kit::span-scanner-root scanner))
     :to-equal
     (list nil nil nil nil :lf nil nil
           0 0 "" 0 nil nil 0 0 0 nil nil nil
           nil nil
           0 0 0 nil nil nil
           nil nil nil nil nil nil nil nil nil nil))))

