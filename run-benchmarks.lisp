;;;; Bootstrap entry point for the cl-xml-kit benchmark application.

(require :asdf)

(defun script-directory ()
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename*
                               (error "Unable to determine script location"))))

(let ((root (script-directory)))
  (asdf:load-asd (merge-pathnames "cl-xml-kit.asd" root))
  (handler-case
      (asdf:load-system "cl-xml-kit")
    (error (condition)
      (format *error-output*
              "cl-xml-kit ASDF load unavailable; loading public package directly: ~A~%"
              condition)
      (load (merge-pathnames "src/package.lisp" root))))
  (handler-case
      (if (asdf:find-system "cl-xml-kit/benchmark" nil)
          (asdf:load-system "cl-xml-kit/benchmark")
          (error "cl-xml-kit/benchmark ASDF system is not defined"))
    (error (condition)
      (format *error-output*
              "cl-xml-kit/benchmark ASDF load unavailable; loading W5 files directly: ~A~%"
              condition)
      (load (merge-pathnames "benchmark/package.lisp" root))
      (load (merge-pathnames "benchmark/run.lisp" root))))
  (funcall (symbol-function
            (or (find-symbol "MAIN" "CL-XML-KIT/BENCHMARK")
                (error "cl-xml-kit/benchmark did not define MAIN")))))

