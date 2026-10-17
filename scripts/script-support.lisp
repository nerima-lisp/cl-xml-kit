;;;; Definitions shared by the scripts in this directory and run-benchmarks.lisp.
;;;; Loading this file defines functions only.

(require :asdf)

(defpackage #:cl-xml-kit/script-support
  (:use #:cl)
  (:export #:script-path
           #:repository-root
           #:load-project-definition
           #:call-run-tests
           #:positive-integer-setting
           #:process-output-within
           #:script-usage-error
           #:load-xmlconf-modules
           #:run-script))

(in-package #:cl-xml-kit/script-support)

(defparameter *scripts-directory*
  (uiop:pathname-directory-pathname *load-truename*))

(defparameter *xmlconf-modules*
  '("xmlconf-catalog" "xmlconf-report" "xmlconf-execution" "xmlconf-options"))

(define-condition script-usage-error (error)
  ((message :initarg :message :reader script-usage-error-message))
  (:report (lambda (condition stream)
             (write-string (script-usage-error-message condition) stream))))

(defun script-path (name)
  (merge-pathnames name *scripts-directory*))

(defun repository-root ()
  (uiop:pathname-parent-directory-pathname *scripts-directory*))

(defun load-project-definition ()
  (asdf:load-asd (merge-pathnames "cl-xml-kit.asd" (repository-root))))

(defun call-run-tests ()
  (let* ((package (find-package "CL-XML-KIT/TEST"))
         (symbol (and package (find-symbol "RUN-TESTS" package))))
    (unless (and symbol (fboundp symbol))
      (error "The loaded cl-xml-kit/test system does not define RUN-TESTS"))
    (unless (funcall (symbol-function symbol))
      (error "cl-xml-kit/test returned NIL"))))

(defun positive-integer-setting (name default)
  "Return environment variable NAME as a positive decimal integer, or DEFAULT."
  (let ((text (uiop:getenv name)))
    (if (null text)
        default
        (let ((value (and (plusp (length text))
                          (every (lambda (character) (char<= #\0 character #\9))
                                 text)
                          (parse-integer text))))
          (unless (and value (plusp value))
            (error "~A must be a positive decimal integer, got ~S" name text))
          value))))

(defun process-output-within (command seconds)
  "Run COMMAND, a program and its arguments, and return its standard output
text and exit code. Standard error is inherited. When COMMAND does not exit
within SECONDS, kill its process group and signal an error."
  (let* ((process (sb-ext:run-program (first command) (rest command)
                                      :search t :wait nil
                                      :input nil :output :stream :error t))
         (output (sb-ext:process-output process))
         (finished nil))
    (unwind-protect
         (handler-case
             (sb-ext:with-timeout seconds
               (multiple-value-prog1
                   (values (uiop:slurp-input-stream :string output)
                           (progn (sb-ext:process-wait process)
                                  (sb-ext:process-exit-code process)))
                 (setf finished t)))
           (sb-ext:timeout ()
             (error "~A did not finish within ~D second~:P" (first command) seconds)))
      ;; With :INPUT NIL the child leads its own process group, and the group
      ;; cannot be reused before the unreaped leader is waited for below.
      (unless finished
        (sb-ext:process-kill process 9 :process-group)
        (sb-ext:process-wait process))
      (sb-ext:process-close process))))

(defun load-xmlconf-modules ()
  (dolist (module *xmlconf-modules*)
    (load (script-path (make-pathname :name module :type "lisp")))))

(defun run-script (name usage main)
  "Call MAIN with the command-line arguments and exit with its status.
--help prints USAGE; a usage error exits with status 2, any other error with 1."
  (let ((arguments (rest sb-ext:*posix-argv*)))
    (handler-case
        (if (intersection '("--help" "-h") arguments :test #'string=)
            (progn (write-string usage) (sb-ext:exit :code 0))
            (sb-ext:exit :code (funcall main arguments)))
      (script-usage-error (condition)
        (format *error-output* "~A: ~A~%~A" name condition usage)
        (sb-ext:exit :code 2))
      (error (condition)
        (format *error-output* "~A: ~A~%" name condition)
        (sb-ext:exit :code 1)))))
