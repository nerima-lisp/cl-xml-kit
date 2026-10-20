(progn
  (load (merge-pathnames "coverage-scanner.lisp" *load-truename*))

  (defun coverage-fixture-owner (source)
    (let ((*read-eval* nil)
          (eof (gensym "EOF")))
      (multiple-value-bind (form end)
          (read-from-string source nil eof :preserve-whitespace t)
        (when (eq form eof)
          (error "Coverage scanner fixture is empty"))
        (list :start 0 :end end :form form))))

  (defun coverage-fixture-expression (source text)
    (let ((start (search text source)))
      (unless start
        (error "Coverage scanner fixture text is missing: ~S" text))
      (list :kind :expression
            :start start
            :end (+ start (length text)))))

  (defun coverage-fixture-owner-uncovered (owner)
    (list (list :kind :expression
                :start (getf owner :start)
                :end (getf owner :end))))

  (let ((accepted 0) (rejected 0) (extended-accepted 0) (extended-rejected 0))
    (flet ((check-fixture (name expected owner source uncovered &optional base-p)
                          (let ((actual
                                 (coverage-owner-exclusion-allowed-p
                                  owner source uncovered)))
                            (unless (eql actual expected)
                              (error "Coverage scanner fixture ~A expected ~S, got ~S"
                                     name expected actual))
                            (if base-p
                                (if actual
                                    (incf accepted)
                                  (incf rejected))
                              (if actual
                                  (incf extended-accepted)
                                (incf extended-rejected))))))
      (let* ((source
               "(defun accepted-key-defaults (&key (policy :reject) (namespace-uri \"\") (optional nil) (offset 0)) (list policy namespace-uri optional offset))")
             (owner (coverage-fixture-owner source))
             (uncovered
               (mapcar (lambda (text)
                         (coverage-fixture-expression source text))
                       '("(policy :reject)" "(namespace-uri \"\")"
                         "(optional nil)" "(offset 0)"))))
        (check-fixture "constant keyword defaults" t owner source uncovered t))
      (let* ((source
               "(defstruct (accepted-constant-slots (:constructor make-accepted-constant-slots (policy namespace-uri optional offset))) \"slot doc\" (policy :reject) (namespace-uri \"\") (optional nil) (offset 1 :type integer))")
             (owner (coverage-fixture-owner source))
             (uncovered
               (mapcar (lambda (text)
                         (coverage-fixture-expression source text))
                       '("(policy :reject)" "(namespace-uri \"\")"
                         "(optional nil)" "(offset 1 :type integer)"))))
        (progn
          (check-fixture "constant slot initforms" t owner source uncovered t)
          (let* ((source
                   "(defstruct (accepted-optional-slot (:constructor make-accepted-optional-slot (value &optional (policy :constructor-default)))) (value nil) (policy :slot-default))")
                 (owner (coverage-fixture-owner source))
                 (uncovered
                   (list (coverage-fixture-expression
                          source "(policy :slot-default)"))))
            (check-fixture "optional constructor slot parameter" t owner source uncovered))
          (let* ((source
                   "(defstruct rejected-implicit-constructor (value nil))")
                 (owner (coverage-fixture-owner source))
                 (uncovered
                   (list (coverage-fixture-expression source "(value nil)"))))
            (check-fixture "implicit constructor literal slot default" t owner source uncovered))
          (let* ((source
                   "(defstruct (rejected-omitted-slot (:constructor make-rejected-omitted-slot (value))) (value nil) (policy :reject))")
                 (owner (coverage-fixture-owner source))
                 (uncovered
                   (list (coverage-fixture-expression
                          source "(policy :reject)"))))
            (check-fixture "explicit constructor literal slot default" t owner source uncovered))))
      (let* ((source
               "(defparameter +literal-table+ '((:alpha 1) (:beta \"value\")) \"table\")")
             (owner (coverage-fixture-owner source)))
        (check-fixture "quoted DEFPARAMETER data" t owner source
  (append (coverage-fixture-owner-uncovered owner)
          (list (coverage-fixture-expression
                 source "'((:alpha 1) (:beta \"value\"))")))))
      (let* ((source "(defparameter +namespace-uri+ \"urn:example\")")
             (owner (coverage-fixture-owner source)))
        (check-fixture "string DEFPARAMETER value" t owner source
                       (coverage-fixture-owner-uncovered owner)))
      (let* ((source "(defvar *active* nil \"active flag\")")
             (owner (coverage-fixture-owner source)))
        (check-fixture "literal DEFVAR initializer" t owner source
                       (coverage-fixture-owner-uncovered owner)))
      (let* ((source "(defvar *late-bound*)")
             (owner (coverage-fixture-owner source)))
        (check-fixture "uninitialized DEFVAR" t owner source
                       (coverage-fixture-owner-uncovered owner)))
      (let* ((source
               "(define-condition accepted-condition (error) ((value :initarg :value :initform nil :reader accepted-value :documentation \"value\")) (:documentation \"doc\"))")
             (owner (coverage-fixture-owner source)))
        (progn
          (check-fixture "constant DEFINE-CONDITION metadata" t owner source
                         (coverage-fixture-owner-uncovered owner))
          (let* ((source
                   "(define-condition accepted-no-init-condition (error) ((kind :initarg :kind :reader accepted-kind :documentation \"kind\")) (:documentation \"doc\"))")
                 (owner (coverage-fixture-owner source)))
            (check-fixture "DEFINE-CONDITION slot metadata" t owner source
                           (coverage-fixture-owner-uncovered owner)))
          (let* ((source
                   "(define-condition accepted-empty-condition (error) () (:documentation \"doc\"))")
                 (owner (coverage-fixture-owner source)))
            (check-fixture "empty DEFINE-CONDITION slots" t owner source
                           (coverage-fixture-owner-uncovered owner)))))
      (let* ((source
               "(defmacro rejected-macro (value) \"macro doc\" (list 'quote value))")
             (owner (coverage-fixture-owner source)))
        (progn
        (check-fixture "DEFMACRO with docstring" nil
               owner source (coverage-fixture-owner-uncovered owner))
        (let* ((source "(defmacro rejected-no-doc (value) (list 'quote value))")
               (owner (coverage-fixture-owner source)))
          (check-fixture "DEFMACRO without docstring" nil
                         owner source (coverage-fixture-owner-uncovered owner)))))
      (let* ((source
               "(eval-when (:compile-toplevel :load-toplevel :execute) (defparameter +table+ '((:kind :value))))")
             (owner (coverage-fixture-owner source)))
        (check-fixture "literal EVAL-WHEN DEFPARAMETER" t owner source
  (append (coverage-fixture-owner-uncovered owner)
          (list (coverage-fixture-expression
                 source "(defparameter +table+ '((:kind :value)))")))))
      (let* ((source
               "(defun rejected-dynamic-default (&key (token (gensym \"token\"))) token)")
             (owner (coverage-fixture-owner source))
             (uncovered
               (list (coverage-fixture-expression
                      source "(token (gensym \"token\"))"))))
        (check-fixture "dynamic default" nil owner source uncovered t))
      (let* ((source
               "(defun rejected-body-expression (&key (policy :reject)) (error \"uncovered\"))")
             (owner (coverage-fixture-owner source))
             (uncovered
               (list (coverage-fixture-expression
                      source "(error \"uncovered\")"))))
        (check-fixture "uncovered body" nil owner source uncovered t))
      (let* ((source
               "(defun rejected-special-default (&key (resource-limits *default-xml-resource-limits*)) resource-limits)")
             (owner (coverage-fixture-owner source))
             (uncovered
               (list (coverage-fixture-expression
                      source "(resource-limits *default-xml-resource-limits*)"))))
        (check-fixture "special variable default" nil owner source uncovered t))
      (let* ((source "(defstruct rejected-dynamic-slot (policy (gensym)))")
             (owner (coverage-fixture-owner source))
             (uncovered
               (list (coverage-fixture-expression
                      source "(policy (gensym))"))))
        (check-fixture "dynamic slot initform" nil owner source uncovered t))
      (let* ((source
               "(defun rejected-branch-kind (&key (policy :reject)) policy)")
             (owner (coverage-fixture-owner source))
             (uncovered
               (coverage-fixture-expression source "(policy :reject)")))
        (setf (getf uncovered :kind) :branch)
        (check-fixture "branch kind" nil owner source (list uncovered) t))
      (let* ((source
               "(defun rejected-mixed-uncovered (&key (policy :reject)) (error \"uncovered\"))")
             (owner (coverage-fixture-owner source))
             (uncovered
               (list (coverage-fixture-expression source "(policy :reject)")
                     (coverage-fixture-expression
                      source "(error \"uncovered\")"))))
        (check-fixture "mixed default and body" nil owner source uncovered t))
      (let* ((source
               "(defparameter +dynamic-table+ (compute-value))")
             (owner (coverage-fixture-owner source)))
        (check-fixture "dynamic DEFPARAMETER" nil owner source
                       (coverage-fixture-owner-uncovered owner)))
      (let* ((source "(defvar *dynamic-value* (compute-value))")
             (owner (coverage-fixture-owner source)))
        (check-fixture "dynamic DEFVAR" nil owner source
                       (coverage-fixture-owner-uncovered owner)))
      (let* ((source
               "(define-condition rejected-report-condition (error) () (:report (lambda (condition stream) (write-string \"report\" stream))))")
             (owner (coverage-fixture-owner source)))
        (check-fixture "executable DEFINE-CONDITION report" nil owner source
                       (coverage-fixture-owner-uncovered owner)))
      (let* ((source
               "(defmacro rejected-uncovered-macro (value) (if value (error \"uncovered\") nil))")
             (owner (coverage-fixture-owner source))
             (uncovered
               (list (coverage-fixture-expression
                      source "(error \"uncovered\")"))))
        (check-fixture "uncovered DEFMACRO body" nil owner source uncovered))
      (let* ((source
               "(eval-when (:compile-toplevel :load-toplevel :execute) (defparameter +dynamic+ (compute-value)))")
             (owner (coverage-fixture-owner source)))
        (check-fixture "dynamic EVAL-WHEN DEFPARAMETER" nil owner source
                       (coverage-fixture-owner-uncovered owner)))
      (let* ((source
               "(eval-when (:compile-toplevel :load-toplevel :execute) (defparameter +safe+ '(:value)) (setf *side-effect* t))")
             (owner (coverage-fixture-owner source)))
        (check-fixture "EVAL-WHEN executable sibling" nil owner source
                       (coverage-fixture-owner-uncovered owner)))
      (let* ((source "(defconstant +unsupported+ 7)")
             (owner (coverage-fixture-owner source)))
        (progn
          (check-fixture "unsupported DEFCONSTANT exclusion" nil owner source (coverage-fixture-owner-uncovered owner))
          (let* ((source "(defstruct (rejected-dynamic-constructor (:constructor make-rejected-dynamic-constructor (&optional (value (compute-value))))) (value nil))")
                 (owner (coverage-fixture-owner source))
                 (uncovered
                   (list (coverage-fixture-expression source "(compute-value)"))))
            (check-fixture "dynamic DEFSTRUCT constructor default" nil owner source uncovered)))))
    (unless (and (= accepted 2) (= rejected 6) (= extended-accepted 11) (= extended-rejected 10))
      (error "Coverage scanner fixture counts differ: accepted=~D rejected=~D extended-accepted=~D extended-rejected=~D" accepted rejected extended-accepted extended-rejected))
    (progn (format t "coverage-scanner-fixtures accepted=~D rejected=~D~%" accepted rejected) (format t "coverage-scanner-fixtures extended accepted=~D rejected=~D~%" extended-accepted extended-rejected))))

