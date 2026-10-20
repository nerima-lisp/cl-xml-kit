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

  (let ((accepted 0)
        (rejected 0))
    (flet ((check-fixture (name expected owner source uncovered)
             (let ((actual
                     (coverage-owner-exclusion-allowed-p
                      owner source uncovered)))
               (unless (eql actual expected)
                 (error "Coverage scanner fixture ~A expected ~S, got ~S"
                        name expected actual))
               (if actual
                   (incf accepted)
                   (incf rejected)))))
      (let* ((source
               "(defun accepted-key-defaults (&key (policy :reject) (namespace-uri \"\") (optional nil) (offset 0)) (list policy namespace-uri optional offset))")
             (owner (coverage-fixture-owner source))
             (uncovered
               (mapcar (lambda (text)
                         (coverage-fixture-expression source text))
                       '("(policy :reject)" "(namespace-uri \"\")"
                         "(optional nil)" "(offset 0)"))))
        (check-fixture "constant keyword defaults" t owner source uncovered))
      (let* ((source
               "(defstruct accepted-constant-slots \"slot doc\" (policy :reject) (namespace-uri \"\") (optional nil) (offset 1 :type integer))")
             (owner (coverage-fixture-owner source))
             (uncovered
               (mapcar (lambda (text)
                         (coverage-fixture-expression source text))
                       '("(policy :reject)" "(namespace-uri \"\")"
                         "(optional nil)" "(offset 1 :type integer)"))))
        (check-fixture "constant slot initforms" t owner source uncovered))
      (let* ((source
               "(defun rejected-dynamic-default (&key (token (gensym \"token\"))) token)")
             (owner (coverage-fixture-owner source))
             (uncovered
               (list (coverage-fixture-expression
                      source "(token (gensym \"token\"))"))))
        (check-fixture "dynamic default" nil owner source uncovered))
      (progn
        (let* ((source
                 "(defun rejected-body-expression (&key (policy :reject)) (error \"uncovered\"))")
               (owner (coverage-fixture-owner source))
               (uncovered
                 (list (coverage-fixture-expression
                        source "(error \"uncovered\")"))))
          (check-fixture "uncovered body" nil owner source uncovered))
        (let* ((source
                 "(defun rejected-special-default (&key (resource-limits *default-xml-resource-limits*)) resource-limits)")
               (owner (coverage-fixture-owner source))
               (uncovered
                 (list (coverage-fixture-expression
                        source "(resource-limits *default-xml-resource-limits*)"))))
          (check-fixture "special variable default" nil owner source uncovered))
        (let* ((source
                 "(defstruct rejected-dynamic-slot (policy (gensym)))")
               (owner (coverage-fixture-owner source))
               (uncovered
                 (list (coverage-fixture-expression
                        source "(policy (gensym))"))))
          (check-fixture "dynamic slot initform" nil owner source uncovered))
        (let* ((source
                 "(defun rejected-branch-kind (&key (policy :reject)) policy)")
               (owner (coverage-fixture-owner source))
               (uncovered
                 (coverage-fixture-expression source "(policy :reject)")))
          (setf (getf uncovered :kind) :branch)
          (check-fixture "branch kind" nil owner source (list uncovered)))
        (let* ((source
                 "(defun rejected-mixed-uncovered (&key (policy :reject)) (error \"uncovered\"))")
               (owner (coverage-fixture-owner source))
               (uncovered
                 (list (coverage-fixture-expression source "(policy :reject)")
                       (coverage-fixture-expression
                        source "(error \"uncovered\")"))))
          (check-fixture "mixed default and body" nil owner source uncovered))))
    (unless (and (= accepted 2) (= rejected 6))
      (error "Coverage scanner fixture counts differ: accepted=~D rejected=~D"
             accepted rejected))
    (format t "coverage-scanner-fixtures accepted=~D rejected=~D~%"
            accepted rejected)))

