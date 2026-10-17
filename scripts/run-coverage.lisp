;;;; Run the ordinary ASDF test system under SBCL coverage instrumentation.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (require :sb-cover))

(load (merge-pathnames "script-support.lisp" *load-truename*))

;; SB-COVER only reports files compiled after this policy is established.
(declaim (optimize (sb-cover:store-coverage-data 3)))

;; SB-COVER needs source-location metadata before instrumented compilation.
(sb-cover:enable-coverage-logging)

(defun coverage-system-files (system-name)
  (let ((files nil)
        (system (or (asdf:find-system system-name nil)
                    (error "ASDF system ~A is not defined" system-name))))
    (labels ((visit (component)
               (let ((path (ignore-errors (asdf:component-pathname component))))
                 (when (and path (probe-file path)
                            (string-equal (pathname-type path) "lisp"))
                   (pushnew (truename path) files :test #'equal)))
               (dolist (child (ignore-errors (asdf:component-children component)))
                 (visit child))))
      (visit system))
    (nreverse files)))

(defun coverage-assert-inputs (label files)
  (unless files (error "Coverage ~A input set is empty" label))
  (dolist (file files)
    (with-open-file (stream file :direction :input)
      (unless (plusp (file-length stream))
        (error "Coverage ~A input is empty: ~A" label file))))
  files)

(defun coverage-run-xmlconf ()
  (let ((directory (uiop:getenv "XML_KIT_XMLCONF_DIR")))
    (when directory
      (cl-xml-kit/script-support:load-xmlconf-modules)
      (format t "coverage-xmlconf~Cdirectory~C~A~%" #\Tab #\Tab directory)
      (uiop:symbol-call "CL-XML-KIT/XMLCONF" "RUN-XMLCONF" directory
                        :mode :execute))))

(defun coverage-number (line prefix)
  (when (and (>= (length line) (length prefix))
             (string= prefix line :end2 (length prefix)))
    (let ((end (or (position #\, line :start (length prefix))
                   (length line))))
      (parse-integer line :start (length prefix) :end end))))

(defun coverage-record-count (line)
  "Return the count after the comma of an LCOV DA or BA record, or NIL."
  (let ((comma (position #\, line :start 3)))
    (and comma (parse-integer line :start (1+ comma)))))

(defun coverage-metrics (path source-files)
  (let ((source-paths (make-hash-table :test #'equal)))
    (dolist (file source-files)
      (setf (gethash (namestring (truename file)) source-paths) t))
    (let ((expression-total 0) (expression-covered 0)
          (branch-total 0) (branch-covered 0)
          (source-file nil))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil nil)
              while line
              do (cond
                   ((and (>= (length line) 3)
                         (string= "SF:" line :end2 3))
                    (setf source-file
                          (gethash (namestring (truename (subseq line 3)))
                                   source-paths)))
                   ((string= "end_of_record" line)
                    (setf source-file nil))
                   ((not source-file))
                   ((coverage-number line "LF:")
                    (incf expression-total (coverage-number line "LF:")))
                   ((coverage-number line "LH:")
                    (incf expression-covered (coverage-number line "LH:")))
                   ((coverage-number line "BRF:")
                    (incf branch-total (coverage-number line "BRF:")))
                   ((coverage-number line "BRH:")
                    (incf branch-covered (coverage-number line "BRH:")))
                   ;; SBCL's native LCOV writer emits DA and BA records but
                   ;; omits the aggregate LF/LH/BRF/BRH records.
                   ((coverage-number line "DA:")
                    (incf expression-total)
                    (when (plusp (or (coverage-record-count line) 0))
                      (incf expression-covered)))
                   ((coverage-number line "BA:")
                    (incf branch-total)
                    (when (eql 2 (coverage-record-count line))
                      (incf branch-covered)))))
      (unless (plusp expression-total)
        (error "Coverage report has no expression records: ~A" path))
      (unless (plusp branch-total)
        (error "Coverage report has no branch records: ~A" path))
      (list :expression (* 100d0 (/ expression-covered expression-total))
            :branch (* 100d0 (/ branch-covered branch-total))
            :expression-covered expression-covered
            :expression-total expression-total
            :branch-covered branch-covered
            :branch-total branch-total)))))

(defun coverage-native-metrics (source-files)
  "Return expression and branch coverage from SB-COVER's instrumented paths.

Unlike LCOV line records, these counts exclude declaration-only source ranges
and preserve the expression paths that SB-COVER actually instruments."
  (let ((expression-total 0)
        (expression-covered 0)
        (branch-total 0)
        (branch-covered 0))
    (dolist (file source-files)
      (multiple-value-bind (counts states line-lengths branch-lines)
          (sb-cover::compute-file-states (namestring (truename file)))
        (declare (ignore states line-lengths branch-lines))
        (let ((expressions (getf counts :expression))
              (branches (getf counts :branch)))
          (incf expression-total (sb-cover::all-of expressions))
          (incf expression-covered (sb-cover::ok-of expressions))
          (incf branch-total (sb-cover::all-of branches))
          (incf branch-covered (sb-cover::ok-of branches))
          (format t "coverage-file~C~A~C~D/~D~C~D/~D~%"
                  #\Tab (namestring (enough-namestring file))
                  #\Tab (sb-cover::ok-of expressions)
                  (sb-cover::all-of expressions)
                  #\Tab (sb-cover::ok-of branches)
                  (sb-cover::all-of branches)))))
    (unless (plusp expression-total)
      (error "SB-COVER has no instrumented expression records"))
    (unless (plusp branch-total)
      (error "SB-COVER has no instrumented branch records"))
    (list :expression (* 100d0 (/ expression-covered expression-total))
          :branch (* 100d0 (/ branch-covered branch-total))
          :expression-covered expression-covered
          :expression-total expression-total
          :branch-covered branch-covered
          :branch-total branch-total)))

(defun coverage-emit (metrics)
  (format t "record~Cmetric~Ccovered~Ctotal~Cpercent~Cthreshold~%"
          #\Tab #\Tab #\Tab #\Tab #\Tab)
  (dolist (metric '(:expression :branch))
    (let* ((name (symbol-name metric))
           (covered (getf metrics
                         (getf (list :expression :expression-covered
                                     :branch :branch-covered)
                               metric)))
           (total (getf metrics
                       (getf (list :expression :expression-total
                                   :branch :branch-total)
                             metric)))
           (percent (getf metrics metric)))
      (format t "coverage~C~A~C~D~C~D~C~,2F~C~A~%"
              #\Tab name #\Tab covered #\Tab total #\Tab percent #\Tab "90.0"))))

(defun coverage-emit-line-metrics (metrics)
  (dolist (metric '(:expression :branch))
    (let* ((covered (getf metrics (getf (list :expression :expression-covered
                                             :branch :branch-covered)
                                       metric)))
           (total (getf metrics (getf (list :expression :expression-total
                                            :branch :branch-total)
                                      metric)))
           (percent (getf metrics metric)))
      (format t "coverage-line~C~A~C~D~C~D~C~,2F~%"
              #\Tab (symbol-name metric) #\Tab covered #\Tab total
              #\Tab percent))))

(let ((report (merge-pathnames (or (uiop:getenv "COVERAGE_LCOV") "coverage.lcov")
                               (cl-xml-kit/script-support:repository-root))))
  (cl-xml-kit/script-support:load-project-definition)
  (let ((source-files (coverage-assert-inputs "source"
                                             (coverage-system-files "cl-xml-kit"))))
    (coverage-assert-inputs "test" (coverage-system-files "cl-xml-kit/test"))
    ;; FORCE is required because an existing FASL may have been compiled before
    ;; SB-COVER instrumentation was enabled. Compile the product separately;
    ;; compiling the test system would register each test once at compile time
    ;; and again when its FASL is loaded.
    (asdf:operate 'asdf:compile-op "cl-xml-kit" :force t)
    ;; Include load-time definitions and initialization in the measured run.
    (sb-cover:reset-coverage)
    (asdf:operate 'asdf:load-op "cl-xml-kit/test" :force t)
    (cl-xml-kit/script-support:call-run-tests)
    (coverage-run-xmlconf)
    (sb-cover:lcov-report report)
    (let ((line-metrics (coverage-metrics report source-files))
          (metrics (coverage-native-metrics source-files)))
      (coverage-emit metrics)
      (coverage-emit-line-metrics line-metrics)
      (unless (and (>= (getf metrics :expression) 90d0)
                   (>= (getf metrics :branch) 90d0))
        (error "Coverage threshold failed: expression=~,2F branch=~,2F (required 90.0)"
               (getf metrics :expression) (getf metrics :branch))))))

