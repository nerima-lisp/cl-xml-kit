;;;; Definitions shared by the scripts in this directory, run-tests.lisp, and
;;;; run-benchmarks.lisp.
;;;; Loading this file registers the repository and loads its host helpers.

(require :asdf)

(defpackage #:cl-xml-kit/script-support
  (:use #:cl)
  (:export #:script-path
           #:repository-root
           #:load-project-definition
           #:register-repository
           #:positive-integer-setting
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
  (register-repository)
  (asdf:load-asd (merge-pathnames "cl-xml-kit.asd" (repository-root))))

(defun register-repository ()
  "Make ASDF find this repository and inherit the caller dependency configuration."
  (asdf:initialize-source-registry
   `(:source-registry (:directory ,(repository-root)) :inherit-configuration)))
(progn
  (register-repository)
  (asdf:load-system "cl-host-kit"))
(setf *scripts-directory*
      (host-kit:pathname-directory-pathname *load-truename*))

(defun positive-integer-setting (name default)
  "Return environment variable NAME as a positive decimal integer, or DEFAULT."
  (let ((text (host-kit:getenv name)))
    (if (null text)
        default
        (let ((value (and (plusp (length text))
                          (every (lambda (character) (char<= #\0 character #\9))
                                 text)
                          (parse-integer text))))
          (unless (and value (plusp value))
            (error "~A must be a positive decimal integer, got ~S" name text))
          value))))

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
