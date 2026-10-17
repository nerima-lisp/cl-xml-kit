(in-package #:cl-xml-kit/test)

(defun source-definition-form (relative-path name)
  "Return the top-level DEFUN of NAME read from the cl-xml-kit source RELATIVE-PATH.
Signals an error when the file defines no such function, so a rename fails the
mutation test instead of leaving it nothing to mutate."
  (let ((*package* (find-package '#:cl-xml-kit)))
    (with-open-file (stream (asdf:system-relative-pathname "cl-xml-kit" relative-path))
      (loop for form = (read stream nil stream)
            until (eq form stream)
            when (and (consp form) (eq (first form) 'defun) (eq (second form) name))
              do (return-from source-definition-form form))))
  (error "~A defines no function ~S" relative-path name))

(defun mutation-results (relative-path name survives-p)
  "Install each default mutation of the DEFUN of NAME in RELATIVE-PATH and return the results.
SURVIVES-P is called with the mutant installed and returns true when it cannot
tell the mutant from the original; an error it signals kills the mutant. The
loaded function object is reinstalled afterwards rather than re-evaluated from
source, so later tests and coverage still run the compiled original."
  (let ((original (fdefinition name)))
    (unwind-protect
         (run-mutations (source-definition-form relative-path name)
                        (lambda (mutant mutation)
                          (declare (ignore mutation))
                          (handler-bind ((warning #'muffle-warning))
                            (eval mutant))
                          (handler-case (funcall survives-p)
                            (error () nil))))
      (setf (fdefinition name) original))))
