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
  (progn
  (defun coverage-top-level-forms (pathname)
    (let* ((source (uiop:read-file-string pathname :external-format :utf-8))
           (eof (gensym "EOF"))
           (*read-eval* t)
           (*package* (find-package :cl-user))
           (position 0)
           (forms nil))
      (loop
        (multiple-value-bind (form end)
            (read-from-string source nil eof
                              :start position
                              :preserve-whitespace t)
          (when (eq form eof)
            (return (nreverse forms)))
          (unless (> end position)
            (error "Source reader did not advance in ~A at ~D" pathname position))
          (push (list :start position :end end :form form) forms)
          (setf position end)
          (when (and (consp form)
                     (symbolp (first form))
                     (string-equal (symbol-name (first form)) "IN-PACKAGE"))
            (setf *package*
                  (or (find-package (second form))
                      (error "Unknown source package ~S in ~A"
                             (second form) pathname))))))))
  (defun coverage-form-name (value)
    (cond ((symbolp value) (string-upcase (symbol-name value)))
          ((stringp value) (string-upcase value))
          ((and (consp value) (symbolp (first value)))
           (coverage-form-name (first value)))))
  (defun coverage-named-form-identity (form)
    (when (and (consp form) (symbolp (first form)))
      (let ((name (coverage-form-name (second form))))
        (when name
          (list (string-upcase (symbol-name (first form))) name)))))
  (defun coverage-named-form-identities (forms)
    (loop for form in forms
          append
          (let ((operator (and (consp form) (symbolp (first form))
                              (string-upcase (symbol-name (first form))))))
            (cond ((and operator (string= operator "PROGN"))
                   (coverage-named-form-identities (rest form)))
                  ((and operator (string= operator "EVAL-WHEN"))
                   (coverage-named-form-identities (cddr form)))
                  (t (let ((identity (coverage-named-form-identity form)))
                       (if identity (list identity) nil)))))))
  (defun coverage-form-identity (form)
    (if (and (consp form) (symbolp (first form)))
        (let ((operator (string-upcase (symbol-name (first form)))))
          (values operator
                  (cond ((string= operator "IN-PACKAGE")
                         (coverage-form-name (second form)))
                        ((string= operator "DECLAIM")
                         (loop for declaration in (rest form)
                               for name = (and (consp declaration)
                                               (coverage-form-name (first declaration)))
                               when name collect name))
                        ((string= operator "EVAL-WHEN")
                         (coverage-named-form-identities (cddr form)))
                        (t (coverage-form-name (second form))))))
        (values "ANONYMOUS" nil)))
  (defun coverage-exclusion-name-valid-p (name)
    (labels ((non-empty-name-string-p (value)
               (and (stringp value)
                    (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))))))
      (or (non-empty-name-string-p name)
          (and (listp name) name
               (every #'non-empty-name-string-p name))
          (and (listp name) name
               (every (lambda (item)
                        (and (listp item)
                             (= (length item) 2)
                             (every #'non-empty-name-string-p item)))
                      name)))))
  (defun coverage-exclusion-allowed-p (entry)
    (let ((operator (getf entry :operator))
          (name (getf entry :name)))
      (and (eq (getf entry :kind) :expression)
           (or (member operator
                       '("IN-PACKAGE" "DECLAIM" "DEFPARAMETER" "DEFVAR"
                         "DEFCONSTANT" "DEFPACKAGE" "DEFINE-CONDITION" "DEFMACRO")
                       :test #'string=)
               (and (string= operator "EVAL-WHEN")
                    (consp name)
                    (every (lambda (definition)
                             (and (listp definition)
                                  (= (length definition) 2)
                                  (string= (first definition) "DEFPARAMETER")))
                           name))))))
  (defun coverage-form-key (path form top-level-forms)
    (let* ((start (getf form :start))
           (end (getf form :end))
           (owner (find-if
                   (lambda (candidate)
                     (and (<= (getf candidate :start) start)
                          (<= end (getf candidate :end))))
                   top-level-forms)))
      (unless owner
        (error "Coverage form has no top-level owner: ~A ~D-~D"
               path start end))
      (multiple-value-bind (operator name)
          (coverage-form-identity (getf owner :form))
        (list path (getf form :kind) operator name)))))

  (defun coverage-form-entry-key (entry)
    (list (getf entry :path) (getf entry :kind)
          (getf entry :operator) (getf entry :name)))
  (defun coverage-read-form-entries (pathname &optional follow-up-p)
    (labels ((valid-entry-p (entry)
               (let* ((keys (and (listp entry)
                                 (evenp (length entry))
                                 (loop for key in entry by #'cddr
                                       collect key)))
                      (expected-keys
                        (if follow-up-p
                            '(:action :kind :name :operator :owner :path :reason)
                            '(:kind :name :operator :path :reason))))
                 (and keys
                      (every #'keywordp keys)
                      (equal (sort (copy-list keys) #'string< :key #'symbol-name)
                             expected-keys)
                      (stringp (getf entry :path))
                      (plusp (length (getf entry :path)))
                      (eq (getf entry :kind) :expression)
                      (stringp (getf entry :operator))
                      (coverage-exclusion-name-valid-p (getf entry :name))
                      (stringp (getf entry :reason))
                      (plusp (length
                              (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           (getf entry :reason))))
                      (if follow-up-p
                          (and (member (getf entry :action) '(:test :delete))
                               (member (getf entry :owner) '(:w2 :w3 :p3)))
                          (coverage-exclusion-allowed-p entry))))))
      (let ((*read-eval* nil))
        (with-open-file (stream pathname :direction :input)
          (let ((entries (read stream nil :eof)))
            (unless (and (listp entries)
                         (eq :eof (read stream nil :eof))
                         (every #'valid-entry-p entries))
              (error "Invalid coverage ~A manifest: ~A"
                     (if follow-up-p "follow-up" "exclusion") pathname))
            (unless (= (length entries)
                       (length (remove-duplicates
                                (mapcar #'coverage-form-entry-key entries)
                                :test #'equal)))
              (error "Coverage ~A manifest contains duplicate entries"
                     (if follow-up-p "follow-up" "exclusion")))
            entries)))))
  (defun coverage-check-form-accounting
    (files manifest-path
     &optional (follow-up-path
                 (merge-pathnames "coverage-follow-ups.sexp" manifest-path)))
    (let* ((entries (coverage-read-form-entries manifest-path))
           (follow-ups (coverage-read-form-entries follow-up-path t))
           (source-forms
             (loop for file in files
                   for path = (enough-namestring
                               (getf file :pathname)
                               (cl-xml-kit/script-support:repository-root))
                   collect (cons path
                                 (coverage-top-level-forms (getf file :pathname)))))
           (uncovered
             (loop for file in files
                   for path = (enough-namestring
                               (getf file :pathname)
                               (cl-xml-kit/script-support:repository-root))
                   for top-level-forms = (cdr (assoc path source-forms :test #'string=))
                   append (mapcar (lambda (form)
                                    (coverage-form-key path form top-level-forms))
                                  (getf file :uncovered-forms))))
           (excluded (mapcar #'coverage-form-entry-key entries))
           (follow-up-keys (mapcar #'coverage-form-entry-key follow-ups))
           (unique-uncovered (remove-duplicates (copy-list uncovered) :test #'equal))
           (stale-exclusions (set-difference excluded unique-uncovered :test #'equal))
           (stale-follow-ups
             (set-difference follow-up-keys unique-uncovered :test #'equal))
           (unaccounted
             (set-difference unique-uncovered
                             (append excluded follow-up-keys)
                             :test #'equal))
           (overlap (intersection excluded follow-up-keys :test #'equal)))
      (dolist (entry (append entries follow-ups))
        (let* ((forms (cdr (assoc (getf entry :path) source-forms :test #'string=)))
               (targets
                 (remove-if-not
                  (lambda (candidate)
                    (multiple-value-bind (operator name)
                        (coverage-form-identity (getf candidate :form))
                      (and (string= operator (getf entry :operator))
                           (equal name (getf entry :name)))))
                  forms)))
          (unless (= (length targets) 1)
            (error "Coverage manifest must identify one top-level form: ~S (matches=~D)"
                   (coverage-form-entry-key entry) (length targets)))))
      (unless (null overlap)
        (error "Coverage forms cannot be both excluded and assigned for follow-up: ~S"
               overlap))
      (unless (null stale-exclusions)
        (error "Coverage exclusions are stale: ~S" stale-exclusions))
      (unless (null stale-follow-ups)
        (error "Coverage follow-ups are stale: ~S" stale-follow-ups))
      (unless (null unaccounted)
        (error "Uncovered forms lack a coverage manifest entry: ~S" unaccounted))
      (format t
              "coverage-exclusions matched=~D follow-ups=~D uncovered-form-keys=~D stale=0~%"
              (length (intersection excluded unique-uncovered :test #'equal))
              (length (intersection follow-up-keys unique-uncovered :test #'equal))
              (length unique-uncovered))
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
         (minimum-expression 97.49d0)
         (minimum-branch 99.43d0)
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
          (coverage-check-form-accounting files exclusion-path))))))
