(in-package #:cl-xml-kit/test)

(defun capture-condition (thunk &optional (type 'xml-kit-error))
  "Call THUNK and return the condition of TYPE that it signals, or NIL when it returns.
An error outside TYPE is not handled, so it reaches the runner and fails the test
instead of being mistaken for the refusal under test."
  (block capture
    (handler-bind ((error (lambda (condition)
                            (when (typep condition type)
                              (return-from capture condition)))))
      (funcall thunk)
      nil)))

(defun condition-reader-values (condition readers)
  "Return (READER VALUE) for each reader function name in READERS applied to CONDITION."
  (mapcar (lambda (reader) (list reader (funcall reader condition))) readers))

(defmatcher :to-signal-xml (thunk expected)
  (:description "Passes when calling THUNK signals an XML-KIT-ERROR of the type "
                "that heads EXPECTED, and each (READER VALUE) in the rest of EXPECTED "
                "holds under EQUAL. A normal return fails with :NO-CONDITION; an "
                "error that is not an XML-KIT-ERROR propagates.")
  (destructuring-bind (type &rest slots) (first expected)
    (let ((condition (capture-condition thunk)))
      (if (null condition)
          (values nil :no-condition (first expected))
          (let ((actual (cons (type-of condition)
                              (condition-reader-values condition
                                                       (mapcar #'first slots)))))
            (values (and (typep condition type)
                         (equal (rest actual) slots))
                    actual
                    (first expected)))))))
