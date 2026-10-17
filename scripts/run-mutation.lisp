;;;; Run the ordinary test system and consume an explicit mutation result set.

(require :asdf)

(load (merge-pathnames "script-support.lisp" *load-truename*))

(defparameter *mutation-usage*
  "Usage: sbcl --script scripts/run-mutation.lisp

Run the cl-xml-kit test system, then require every selected mutant to be
killed. The script takes no options; it is configured by the environment.

Environment:
  MUTATION_SELECTOR        comma-separated mutant names
                           (default: xml-grammar,serializer-escape)
  MUTATION_RESULTS         existing TSV result file to read
  MUTATION_RUNNER          program run with --root, --test-system, and
                           --selector when MUTATION_RESULTS is not a file
  MUTATION_RUNNER_TIMEOUT  seconds before MUTATION_RUNNER is killed and the
                           check fails (default: 3600)
")

(defparameter *default-runner-timeout-seconds* 3600)

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

(defun mutation-runner-text (runner selector)
  (multiple-value-bind (text exit-code)
      (cl-xml-kit/script-support:process-output-within
       (list runner
             "--root" (namestring (cl-xml-kit/script-support:repository-root))
             "--test-system" "cl-xml-kit/test"
             "--selector" (format nil "~{~A~^,~}" selector))
       (cl-xml-kit/script-support:positive-integer-setting
        "MUTATION_RUNNER_TIMEOUT" *default-runner-timeout-seconds*))
    (unless (eql exit-code 0)
      (error "Mutation runner exited with status ~A" exit-code))
    text))

(defun mutation-result-text (selector)
  (let ((result-file (uiop:getenv "MUTATION_RESULTS"))
        (runner (uiop:getenv "MUTATION_RUNNER")))
    (cond
      ((and result-file (probe-file result-file))
       (uiop:read-file-string result-file))
      (runner
       (mutation-runner-text runner selector))
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

(defun mutation-check (selector results)
  "Emit the selected RESULTS and fail unless every selected mutant was killed."
  (let* ((selected (remove-if-not (lambda (result)
                                    (member (first result) selector :test #'string=))
                                  results))
         (survivors (remove-if-not (lambda (result) (string= (second result) "survived"))
                                   selected))
         (errors (remove-if-not (lambda (result) (string= (second result) "error"))
                                selected)))
    (unless (and (= (length selected) (length selector))
                 (every (lambda (mutant)
                          (find mutant selected :key #'first :test #'string=))
                        selector))
      (error "Mutation result set does not cover every selected mutant"))
    (mutation-emit selector selected)
    (when (or survivors errors)
      (error "Mutation check failed; surviving mutants: ~{~A~^, ~}; error mutants: ~{~A~^, ~}"
             (mapcar #'first survivors) (mapcar #'first errors)))))

(defun mutation-main (arguments)
  (when arguments
    (error 'cl-xml-kit/script-support:script-usage-error
           :message (format nil "Unexpected argument ~A" (first arguments))))
  (let ((selector (mutation-selector)))
    (cl-xml-kit/script-support:load-project-definition)
    ;; A mutation run is never allowed to bypass the ordinary test system.
    (asdf:load-system "cl-xml-kit/test" :force t)
    (cl-xml-kit/script-support:call-run-tests)
    (mutation-check selector (mutation-results (mutation-result-text selector)))
    0))

(cl-xml-kit/script-support:run-script "run-mutation" *mutation-usage* #'mutation-main)
