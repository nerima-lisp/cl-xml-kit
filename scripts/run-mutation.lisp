;;;; Run the ordinary test system and consume an explicit mutation result set.

(require :asdf)

(defun mutation-script-directory ()
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename*
                               (error "Unable to determine script location"))))

(defun mutation-split (text separator)
  (let ((parts nil) (start 0))
    (loop for end = (position separator text :start start)
          do (if end
                 (progn (push (subseq text start end) parts)
                        (setf start (1+ end)))
                 (progn (push (subseq text start) parts) (return))))
    (nreverse parts)))

(defun mutation-selector ()
  (let* ((text (or (uiop:getenv "MUTATION_SELECTOR") "xml-grammar,serializer-escape"))
         (items (mapcar (lambda (item)
                          (string-trim '(#\Space #\Tab) item))
                        (mutation-split text #\,))))
    (when (or (null items) (some #'(lambda (item) (zerop (length item))) items))
      (error "MUTATION_SELECTOR must select at least one non-empty mutant"))
    (when (/= (length items) (length (remove-duplicates items :test #'string=)))
      (error "MUTATION_SELECTOR contains duplicate mutant selectors"))
    items))

(defun mutation-run-tests ()
  (asdf:load-system "cl-xml-kit/test" :force t)
  (let* ((package (find-package "CL-XML-KIT/TEST"))
         (symbol (and package (find-symbol "RUN-TESTS" package))))
    (unless (and symbol (fboundp symbol))
      (error "The loaded cl-xml-kit/test system does not define RUN-TESTS"))
    (unless (funcall (symbol-function symbol))
      (error "cl-xml-kit/test returned NIL"))))

(defun mutation-result-text (root selector)
  (let ((result-file (uiop:getenv "MUTATION_RESULTS"))
        (runner (or (uiop:getenv "MUTATION_RUNNER")
                    (uiop:getenv "MUTATION_COMMAND"))))
    (cond
      ((and result-file (probe-file result-file))
       (uiop:read-file-string result-file))
      (runner
       (let* ((process
                (uiop:launch-program
                 (list runner "--root" (namestring root)
                       "--test-system" "cl-xml-kit/test"
                       "--selector" (format nil "~{~A~^,~}" selector))
                 :output :stream :error-output *error-output*))
              (output-stream (uiop:process-info-output process))
              (text (unwind-protect
                        (uiop:slurp-input-stream output-stream)
                      (close output-stream)))
              (exit-code (uiop:wait-process process)))
         (unless (eql exit-code 0)
           (error "Mutation runner exited with status ~A" exit-code))
         text))
      (t
       (error "No mutation result input. Set MUTATION_RESULTS or MUTATION_RUNNER.")))))

(defun mutation-results (text)
  (let ((results nil))
    (dolist (line (mutation-split text #\Newline))
      (unless (or (zerop (length line)) (char= (char line 0) #\#))
        (let ((fields (mutation-split line #\Tab)))
          (unless (or (string= (first fields) "mutant")
                      (string= (first fields) "record"))
            (unless (>= (length fields) 2)
              (error "Mutation result row needs mutant and status: ~S" line))
            (let ((mutant (first fields))
                  (status (string-downcase
                           (string-trim '(#\Space #\Tab) (second fields)))))
              (unless (member status '("killed" "survived" "error") :test #'string=)
                (error "Unknown mutation status ~S for ~A" status mutant))
              (push (list mutant status (third fields)) results))))))
    (nreverse results)))

(defun mutation-field (value)
  (let ((text (princ-to-string (or value ""))))
    (with-output-to-string (stream)
      (loop for character across text
            do (case character
                 (#\Tab (write-string "\\t" stream))
                 (#\Newline (write-string "\\n" stream))
                 (#\Return (write-string "\\r" stream))
                 (#\\ (write-string "\\\\" stream))
                 (t (write-char character stream)))))))

(defun mutation-fields (fields)
  (loop for field in fields
        for first = t then nil
        do (unless first (write-char #\Tab))
           (write-string (mutation-field field)))
  (terpri))

(defun mutation-emit (selector results)
  (mutation-fields '("record" "key" "value"))
  (mutation-fields (list "meta" "selector"
                         (format nil "~{~A~^,~}" selector)))
  (mutation-fields (list "meta" "selected_count" (length selector)))
  (mutation-fields '("record" "mutant" "status" "detail"))
  (dolist (result results)
    (mutation-fields (list "mutant" (first result) (second result)
                           (or (third result) "")))))

(let* ((root (uiop:pathname-parent-directory-pathname
              (mutation-script-directory)))
       (selector (mutation-selector)))
  (asdf:load-asd (merge-pathnames "cl-xml-kit.asd" root))
  ;; A mutation run is never allowed to bypass the ordinary test system.
  (mutation-run-tests)
  (let* ((results (mutation-results (mutation-result-text root selector)))
         (selected-results
           (remove-if-not (lambda (result)
                            (member (first result) selector :test #'string=))
                          results))
         (survivors (remove-if-not (lambda (result)
                                     (string= (second result) "survived"))
                                   selected-results))
         (errors (remove-if-not (lambda (result)
                                  (string= (second result) "error"))
                                selected-results)))
    (unless (and (= (length selected-results) (length selector))
                 (every (lambda (mutant)
                         (find mutant selected-results :key #'first :test #'string=))
                        selector))
      (error "Mutation result set does not cover every selected mutant"))
    (mutation-emit selector selected-results)
    (when (or survivors errors)
      (error "Mutation check failed; surviving mutants: ~{~A~^, ~}; error mutants: ~{~A~^, ~}"
             (mapcar #'first survivors) (mapcar #'first errors)))))

