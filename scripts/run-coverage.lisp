;;;; Run the ordinary ASDF test system under SBCL coverage instrumentation.

(progn
  (require :asdf)
  (require :sb-cover)
  (load (merge-pathnames "script-support.lisp" *load-truename*)))
(progn
  (cl-xml-kit/script-support:load-project-definition)
  (asdf:load-system "cl-weave"))
(progn
  ;; Enable source-location logging before compiling the product.
  (declaim (optimize (sb-cover:store-coverage-data 3)))
  (sb-cover:enable-coverage-logging)
  (defun coverage-system-files (system-name)
    (let ((files nil)
          (system (or (asdf:find-system system-name nil)
                      (error "ASDF system ~A is not defined" system-name))))
      (labels ((visit (component)
                 (let ((path (ignore-errors (asdf:component-pathname component))))
                   (when (and path (host-kit:file-exists-p path)
                              (string-equal (pathname-type path) "lisp"))
                     (pushnew (truename path) files :test #'equal)))
                 (dolist (child (ignore-errors (asdf:component-children component)))
                   (visit child))))
        (visit system))
      (nreverse files)))
  (defun coverage-assert-inputs (label files)
    (unless files
      (error "Coverage ~A input set is empty" label))
    (dolist (file files)
      (unless (plusp (length (host-kit:read-file-octets file)))
        (error "Coverage ~A input is empty: ~A" label file)))
    files)
  (defun coverage-form-key (path form)
    (list path (getf form :kind) (getf form :start) (getf form :end)))
  (defun coverage-exclusion-key (entry)
    (list (getf entry :path) (getf entry :kind)
          (getf entry :start) (getf entry :end)))
  (defun coverage-read-exclusions (pathname)
    (labels ((valid-entry-p (entry)
               (let ((keys (and (listp entry)
                                (= (length entry) 10)
                                (loop for key in entry by #'cddr
                                      collect key))))
                 (and keys
                      (equal (sort (copy-list keys) #'string< :key #'symbol-name)
                             '(:end :kind :path :reason :start))
                      (stringp (getf entry :path))
                      (plusp (length (getf entry :path)))
                      (member (getf entry :kind) '(:expression :branch))
                      (integerp (getf entry :start))
                      (not (minusp (getf entry :start)))
                      (integerp (getf entry :end))
                      (> (getf entry :end) (getf entry :start))
                      (stringp (getf entry :reason))
                      (plusp (length
                              (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           (getf entry :reason))))))))
      (let ((*read-eval* nil))
        (with-open-file (stream pathname :direction :input)
          (let ((entries (read stream nil :eof)))
            (unless (and (listp entries)
                         (eq :eof (read stream nil :eof))
                         (every #'valid-entry-p entries))
              (error "Invalid coverage exclusion manifest: ~A" pathname))
            (unless (= (length entries)
                       (length (remove-duplicates
                                (mapcar #'coverage-exclusion-key entries)
                                :test #'equal)))
              (error "Coverage exclusion manifest contains duplicate entries"))
            entries)))))
  (defun coverage-check-exclusions (files manifest-path)
    (let* ((entries (coverage-read-exclusions manifest-path))
           (uncovered
             (loop for file in files
                   for path = (enough-namestring
                               (getf file :pathname)
                               (cl-xml-kit/script-support:repository-root))
                   append (mapcar (lambda (form) (coverage-form-key path form))
                                  (getf file :uncovered-forms))))
           (excluded (mapcar #'coverage-exclusion-key entries))
           (unique-uncovered (remove-duplicates (copy-list uncovered) :test #'equal))
           (missing (set-difference uncovered excluded :test #'equal))
           (stale (set-difference excluded uncovered :test #'equal)))
      (unless (= (length uncovered) (length unique-uncovered))
        (error "Coverage returned duplicate uncovered form keys"))
      (unless (and (null missing) (null stale))
        (error "Coverage exclusion mismatch: forms without reasons=~S; stale entries=~S"
               missing stale))
      (format t "coverage-exclusions matched=~D~%" (length entries))
      entries))
  (defun coverage-require-files-measured (source-files statistics)
    (let ((measured (mapcar (lambda (file) (truename (getf file :pathname)))
                            statistics)))
      (dolist (source source-files)
        (unless (member source measured :test #'equal)
          (error "Coverage recorded no forms for source file ~A" source)))
      (when (null statistics)
        (error "Coverage returned no source file statistics"))
      statistics))
  (defun coverage-emit-statistics (statistics minimum-expression minimum-branch)
    (unless (and (plusp (getf statistics :expression-total))
                 (plusp (getf statistics :branch-total)))
      (error "Coverage has an empty expression or branch denominator"))
    (format t "coverage-expressions=~D/~D minimum=~,2F%~%coverage-branches=~D/~D minimum=~,2F%~%"
            (getf statistics :expression-covered)
            (getf statistics :expression-total)
            minimum-expression
            (getf statistics :branch-covered)
            (getf statistics :branch-total)
            minimum-branch))
  (let* ((root (cl-xml-kit/script-support:repository-root))
         (source-directory (merge-pathnames "src/" root))
         (source-files
           (coverage-assert-inputs "source" (coverage-system-files "cl-xml-kit")))
         (test-files
           (coverage-assert-inputs "test" (coverage-system-files "cl-xml-kit/test")))
         (minimum-expression 97.57d0)
         (minimum-branch 99.51d0)
         (include-pathnames (list source-directory))
         (exclusion-path (merge-pathnames "scripts/coverage-exclusions.sexp" root)))
    (declare (ignore test-files))
    (asdf:operate 'asdf:compile-op "cl-xml-kit" :force t)
    (cl-weave:reset-coverage)
    (asdf:load-system "cl-xml-kit/test" :force t)
    (let* ((plan (cl-weave:collect-test-plan (cl-weave:root-suite)))
           (test-count (length plan)))
      (unless (plusp test-count)
        (error "Coverage test plan is empty"))
      (let* ((passed nil)
             (report
               (with-output-to-string (stream)
                 (setf passed
                       (cl-weave:run-all
                        :reporter :spec
                        :stream stream
                        :coverage t
                        :coverage-reset nil
                        :coverage-output "coverage.dat"
                        :coverage-report-directory "coverage-report/"
                        :coverage-include-pathnames include-pathnames
                        :coverage-minimum-expression minimum-expression
                        :coverage-minimum-branch minimum-branch
                        :timeout-ms 10000
                        :max-workers 1
                        :pass-with-no-tests nil)))))
        (unless (and passed
                     (search (format nil
                                     "~D passed, 0 skipped, 0 todo, 0 failed, 0 errored, ~D total"
                                     test-count test-count)
                             report :from-end t))
          (write-string report)
          (error "cl-weave coverage run did not pass every selected test"))
        (format t "coverage-tests selected=~D passed=~D skipped=0 todo=0 failed=0 errored=0~%"
                test-count test-count)
        (let* ((statistics
                 (cl-weave:coverage-statistics
                  :include-pathnames include-pathnames))
               (files
                 (coverage-require-files-measured
                  source-files
                  (cl-weave:coverage-file-statistics
                   :include-pathnames include-pathnames))))
          (coverage-emit-statistics statistics minimum-expression minimum-branch)
          (coverage-check-exclusions files exclusion-path))))))
