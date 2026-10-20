;;;; t/helpers-path.lisp
;;;;
;;;; Accessors over SELECT-XML results and the generated paths of the path
;;;; property test.

(in-package #:cl-xml-kit/test)

(defun path-summaries (source path &rest options)
  "Return (KIND PATH) for every match of PATH in SOURCE under the SELECT-XML OPTIONS.
A SELECT-XML condition propagates to the caller."
  (mapcar (lambda (match) (list (xml-match-kind match) (xml-match-path match)))
          (apply #'path-select source path options)))

(defun path-select (source path &rest options)
  "Return the matches selected by PATH, or signal the condition delivered on failure."
  (let ((success-calls 0))
    (let ((matches
           (cl-weave:with-continuation-result (matches success)
               (apply #'select-xml source path
                      (lambda (selected)
                        (incf success-calls)
                        (funcall #'success selected))
                      #'error options)
             matches)))
      (expect success-calls :to-equal 1)
      matches)))

(defun path-slices (source path)
  "Return the text of SOURCE that each match of PATH spans; a SELECT-XML condition propagates."
  (mapcar (lambda (match)
            (subseq source (xml-match-start match) (xml-match-end match)))
          (path-select source path)))

(defun path-match-identity (match)
  "Return the KIND, START, and END of MATCH, which tell matches of one source apart."
  (list (xml-match-kind match) (xml-match-start match) (xml-match-end match)))

(defun path-match-identities (source path)
  "Return the identity of every match of PATH in SOURCE, in document order."
  (mapcar #'path-match-identity (path-select source path)))

;;; The step names are those CORPUS-GEN-DOCUMENT writes, plus one it never
;;; writes, so generated paths both hit and miss.
(defun path-gen-step ()
  "Generate one element step string: a name or *, with an optional position from 1 to 3."
  (gen-map (lambda (parts)
             (destructuring-bind (name index) parts
               (if index (format nil "~A[~D]" name index) name)))
           (gen-tuple (gen-member '("a" "b" "p:c" "d-e" "x.y" "*" "zz"))
                      (gen-one-of (gen-member '(nil)) (gen-integer :min 1 :max 3)))))

(defun path-gen-path ()
  "Generate a well-formed absolute path: /*, up to three more steps, then an element, attribute, or text() target.
The first step is always the wildcard so that every path reaches the document element."
  (gen-map (lambda (parts)
             (destructuring-bind (steps target) parts
               (format nil "/*~{/~A~}~A" steps target)))
           (gen-tuple (gen-list (path-gen-step) :max-length 3)
                      (gen-member '("" "/@k" "/@p:m" "/@id" "/text()")))))
