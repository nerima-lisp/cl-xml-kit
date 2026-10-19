;;;; src/macros.lisp
;;;;
;;;; Definition macros over the tables in src/data/. The tables load after this
;;;; file, so the expansion-time helpers read them through SYMBOL-VALUE; every
;;;; expansion is complete code that consults no table at run time.

(in-package #:cl-xml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %message-control (message-id)
    (let ((entry (assoc message-id (symbol-value '+xml-error-message-specs+))))
      (unless entry
        (error "Unknown XML error message ID ~S." message-id))
      (second entry)))

  (defun %record-accessor (conc-name slot)
    (intern (concatenate 'string (symbol-name conc-name) (symbol-name slot))
            (symbol-package conc-name)))

  (defun %record-reader (conc-name slot reader documentation copy-policy)
    (let ((read `(,(%record-accessor conc-name slot) record)))
      `(defun ,reader (record)
         ,documentation
         ,(ecase copy-policy
            (:identity read)
            (:copy-list `(copy-list ,read))
            (:copy-seq `(copy-seq ,read))))))

  (defun %char-class-tests (name specs)
    "Return the CODES, CHARS, and RANGES of the class NAME, its inclusions merged."
    (let ((spec (assoc name specs)))
      (unless spec
        (error "Unknown XML character class ~S." name))
      (destructuring-bind (&key include codes chars ranges exclude-chars)
          (cddr spec)
        (let ((all-codes (copy-list codes))
              (all-chars (copy-list chars))
              (all-ranges (copy-list ranges)))
          (when include
            (multiple-value-bind (included-codes included-chars included-ranges)
                (%char-class-tests include specs)
              (setf all-codes (append included-codes all-codes)
                    all-chars (append included-chars all-chars)
                    all-ranges (append included-ranges all-ranges))))
          (dolist (excluded exclude-chars)
            (let ((code (char-code excluded)))
              (when (or (member code all-codes)
                        (some (lambda (range)
                                (<= (first range) code (second range)))
                              all-ranges))
                (error "Character class ~S can exclude only listed characters."
                       name)))
            (setf all-chars (remove excluded all-chars)))
          (values all-codes all-chars all-ranges))))))

(defmacro xml-message (message-id &rest format-arguments)
  "Expand to the message text that MESSAGE-ID selects in the message table.

With FORMAT-ARGUMENTS the expansion formats the control string with them;
without, it is the literal text. An unknown MESSAGE-ID is an expansion-time
error."
  (let ((control (%message-control message-id)))
    (cond
      (format-arguments `(format nil ,control ,@format-arguments))
      ((find #\~ control)
       (error "Message ~S needs format arguments." message-id))
      (t control))))

(defmacro xml-error (condition message-id &key format-arguments condition-initargs)
  "Signal CONDITION using the message control selected by MESSAGE-ID.

FORMAT-ARGUMENTS is a list of argument forms for the control string and
CONDITION-INITARGS a plist of further initargs, both written unevaluated."
  `(error ,condition
          :message (xml-message ,message-id ,@format-arguments)
          ,@condition-initargs))

(defmacro check-argument (value predicate condition message-id
                          &key format-arguments condition-initargs)
  "Evaluate VALUE once, signal CONDITION unless PREDICATE accepts it, and return VALUE.

PREDICATE names a function. CONDITION, MESSAGE-ID, FORMAT-ARGUMENTS, and
CONDITION-INITARGS are as for XML-ERROR, with CONDITION unquoted."
  (let ((argument (gensym "VALUE")))
    `(let ((,argument ,value))
       (unless (,predicate ,argument)
         (xml-error ',condition ,message-id
                    :format-arguments ,format-arguments
                    :condition-initargs ,condition-initargs))
       ,argument)))

(defmacro define-immutable-record
    (name (&key constructor conc-name predicate predicate-doc structure-doc) slots)
  "Define an immutable DEFSTRUCT, its predicate, and public readers from one specification.

Each slot is (SLOT-NAME INITFORM PUBLIC-READER READER-DOC COPY-POLICY). A NIL
PUBLIC-READER keeps the slot private. COPY-POLICY is :IDENTITY, :COPY-LIST, or
:COPY-SEQ, and the reader returns a copy under the latter two. CONSTRUCTOR
takes every slot positionally, in order."
  (let ((public (remove nil slots :key #'third)))
    `(progn
       (declaim (inline ,predicate ,@(mapcar #'third public)))
       (defstruct (,name
                   (:constructor ,constructor ,(mapcar #'first slots))
                   (:conc-name ,conc-name)
                   (:copier nil)
                   (:predicate nil))
         ,structure-doc
         ,@(mapcar (lambda (slot)
                     `(,(first slot) ,(second slot) :read-only t))
                   slots))
       (defun ,predicate (value)
         ,predicate-doc
         (typep value ',name))
       ,@(mapcar (lambda (slot)
                   (destructuring-bind (slot-name initform reader documentation
                                        copy-policy)
                       slot
                     (declare (ignore initform))
                     (%record-reader conc-name slot-name reader documentation
                                     copy-policy)))
                 public)
       ',name)))

(defmacro define-resource-limits
    (record-name constructor-name specs-variable
     &key default-instance default-instance-doc record-doc predicate-doc
       constructor-doc)
  "Define resource-limit defaults, validation, predicate, readers, and the default value from one specification table.

Each row of SPECS-VARIABLE is (KEYWORD SLOT DEFAULT-CONSTANT DEFAULT READER
READER-DOC). The constructor's keyword parameters and its validation follow
the row order."
  (let* ((specs (symbol-value specs-variable))
         (slots (mapcar #'second specs))
         (conc-name (intern (format nil "%~A-" (symbol-name record-name))
                            (symbol-package record-name)))
         (raw-constructor (intern (format nil "%MAKE-~A" (symbol-name record-name))
                                  (symbol-package record-name)))
         (predicate (intern (format nil "~A-P" (symbol-name record-name))
                            (symbol-package record-name))))
    `(progn
       ,@(mapcar (lambda (spec)
                   `(defconstant ,(third spec) ,(fourth spec)))
                 specs)
       (declaim (inline ,predicate ,@(mapcar #'fifth specs)))
       (defstruct (,record-name
                   (:constructor ,raw-constructor ,slots)
                   (:conc-name ,conc-name)
                   (:copier nil)
                   (:predicate nil))
         ,record-doc
         ,@(mapcar (lambda (spec)
                     `(,(second spec) ,(third spec) :read-only t))
                   specs))
       (defun ,constructor-name
           (&key ,@(mapcar (lambda (spec) `(,(second spec) ,(third spec))) specs))
         ,constructor-doc
         ,@(mapcar (lambda (spec)
                     (destructuring-bind (keyword slot &rest ignore) spec
                       (declare (ignore ignore))
                       `(unless (and (integerp ,slot) (plusp ,slot))
                          (xml-error 'xml-invalid-resource-limit
                                     :invalid-resource-limit
                                     :format-arguments (,keyword ,slot)
                                     :condition-initargs (:kind ,keyword
                                                          :value ,slot)))))
                   specs)
         (,raw-constructor ,@slots))
       (defun ,predicate (value)
         ,predicate-doc
         (typep value ',record-name))
       ,@(mapcar (lambda (spec)
                   (destructuring-bind (keyword slot constant default reader
                                        documentation)
                       spec
                     (declare (ignore keyword constant default))
                     (%record-reader conc-name slot reader documentation
                                     :identity)))
                 specs)
       (defparameter ,default-instance (,constructor-name)
         ,default-instance-doc))))

(defmacro define-char-classes (specs-variable)
  "Generate XML character predicates from the compile-time character-class table.

Each predicate compares the character and its code with the class's literal
codes, characters, and inclusive code ranges; nothing is looked up at run time."
  (let ((specs (symbol-value specs-variable)))
    `(progn
       (declaim (inline ,@(mapcar #'first specs)))
       ,@(mapcar
          (lambda (spec)
            (destructuring-bind (name documentation &rest options) spec
              (declare (ignore options))
              (multiple-value-bind (codes chars ranges)
                  (%char-class-tests name specs)
                `(defun ,name (character)
                   ,documentation
                   (declare (type character character))
                   (let ((code (char-code character)))
                     (declare (type fixnum code)
                              (ignorable code))
                     (or ,@(mapcar (lambda (code) `(= code ,code)) codes)
                         ,@(mapcar (lambda (char) `(char= character ,char)) chars)
                         ,@(mapcar (lambda (range)
                                     `(<= ,(first range) code ,(second range)))
                                   ranges)))))))
          specs))))

(defmacro define-parser-token-dispatch (function-name rules-variable context)
  "Define a parser token dispatcher from one context in a compile-time rule table."
  (let ((context-rules (assoc context (symbol-value rules-variable))))
    (unless context-rules
      (error "Unknown parser token dispatch context ~S in ~S."
             context rules-variable))
    `(defun ,function-name (parser token)
       (case (xml-token-kind token)
         ,@(mapcar
            (lambda (rule)
              (destructuring-bind (kind handler) rule
                `(,(if (eq kind :otherwise) 'otherwise kind)
                  ,(when handler `(,handler parser token)))))
            (cdr context-rules))))))

(defmacro define-serializer-node-dispatch (rules-variable)
  "Generate node predicates and writer dispatch from a compile-time rule table."
  (let* ((rules (symbol-value rules-variable))
         (node-rules (remove :otherwise rules :key #'first))
         (otherwise-rule (assoc :otherwise rules)))
    (unless (and otherwise-rule (second otherwise-rule))
      (error "Serializer node dispatch table ~S needs an :OTHERWISE handler."
             rules-variable))
    `(progn
       (defun xml-node-p (object)
         (or ,@(mapcar (lambda (rule) `(,(first rule) object)) node-rules)))
       (defun %write-node (state object scope level path)
         (cond
           ,@(mapcar
              (lambda (rule)
                `((,(first rule) object)
                  (,(second rule) state object scope level path)))
              node-rules)
           (t (,(second otherwise-rule) state object scope level path)))))))
