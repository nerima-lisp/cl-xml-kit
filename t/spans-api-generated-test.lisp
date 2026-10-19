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
