;;;; src/conditions.lisp

(in-package #:cl-xml-kit)

(define-condition xml-kit-error (error)
  ((message :initarg :message :initform "An XML kit error occurred."
            :reader xml-condition-message
            :documentation "A human-readable description of the failure.")
   (position :initarg :position :initform nil :reader xml-condition-position
             :documentation "The source position associated with the failure.")
   (source :initarg :source :initform nil :reader xml-condition-source
           :documentation "The caller-provided source name, when available."))
  (:report (lambda (condition stream)
             (write-string (xml-condition-message condition) stream)))
  (:documentation "Base condition for every condition signalled by CL-XML-KIT."))

(define-condition xml-input-error (xml-kit-error) ()
  (:documentation "Base condition for failures while obtaining XML input."))

(define-condition xml-decode-error (xml-input-error)
  ((octet :initarg :octet :initform nil :reader xml-decode-error-octet
          :documentation "The failing octet, when one is available.")
   (encoding :initarg :encoding :initform :utf-8
             :reader xml-decode-error-encoding
             :documentation "The encoding used for decoding."))
  (:documentation "A failure while decoding an octet input."))

(define-condition xml-unsupported-encoding (xml-decode-error)
  ((encoding :initarg :encoding :reader xml-unsupported-encoding-encoding
             :documentation "The unsupported encoding designator or name.")
   (reason :initarg :reason :initform nil :reader xml-unsupported-encoding-reason
           :documentation "The reason this encoding is outside the XML profile."))
  (:documentation "An input declares or uses an encoding outside the supported UTF-8 and UTF-16 inputs."))

(define-condition xml-parse-error (xml-kit-error) ()
  (:documentation "Base condition for XML grammar and parser-state failures."))

(define-condition xml-well-formedness-error (xml-parse-error)
  ((constraint :initarg :constraint :initform nil
               :reader xml-well-formedness-error-constraint
               :documentation "The XML well-formedness constraint that failed.")
   (expected :initarg :expected :initform nil
             :reader xml-well-formedness-error-expected
             :documentation "The construct expected at the failure position."))
  (:documentation "The input violates an XML well-formedness constraint."))

(define-condition xml-namespace-error (xml-parse-error)
  ((prefix :initarg :prefix :initform nil :reader xml-namespace-error-prefix
           :documentation "The prefix involved in the namespace failure.")
   (namespace-uri :initarg :namespace-uri :initform nil
                  :reader xml-namespace-error-namespace-uri
                  :documentation "The namespace URI involved in the failure."))
  (:documentation "The input violates a Namespaces in XML constraint."))

(define-condition xml-external-resource-disabled (xml-parse-error)
  ((identifier :initarg :identifier :initform nil
               :reader xml-external-resource-disabled-identifier
               :documentation "The external resource identifier.")
   (kind :initarg :kind :initform nil
         :reader xml-external-resource-disabled-kind
         :documentation "The kind of external resource requested."))
  (:documentation "An external resource was requested under the reject policy."))

(define-condition xml-parser-state-error (xml-parse-error)
  ((parser :initarg :parser :initform nil :reader xml-parser-state-error-parser
           :documentation "The parser that cannot continue."))
  (:documentation "A parser was used after a fatal condition or end of input."))

(define-condition xml-resource-limit-exceeded (xml-kit-error)
  ((kind :initarg :kind :reader xml-resource-limit-exceeded-kind
         :documentation "The resource-limit kind that was exceeded.")
   (value :initarg :value :reader xml-resource-limit-exceeded-value
          :documentation "The observed value beyond the limit.")
   (limit :initarg :limit :reader xml-resource-limit-exceeded-limit
          :documentation "The configured inclusive limit."))
  (:documentation "A parse or serialization resource limit was exceeded."))

(define-condition xml-invalid-resource-limit (xml-kit-error)
  ((kind :initarg :kind :reader xml-invalid-resource-limit-kind
         :documentation "The resource-limit kind with an invalid value.")
   (value :initarg :value :reader xml-invalid-resource-limit-value
          :documentation "The invalid configured value."))
  (:documentation "A resource limit was not a positive integer."))

(define-condition xml-serialization-error (xml-kit-error)
  ((object :initarg :object :initform nil :reader xml-serialization-error-object
           :documentation "The object that could not be serialized.")
   (path :initarg :path :initform nil :reader xml-serialization-error-path
         :documentation "The path to the offending object, when available."))
  (:documentation "A tree or output stream could not be serialized."))

(define-condition xml-edit-error (xml-kit-error)
  ((path :initarg :path :initform nil :reader xml-edit-error-path
         :documentation "The path the caller supplied.")
   (reason :initarg :reason :initform nil :reader xml-edit-error-reason
           :documentation "A keyword naming why the path or edit was refused."))
  (:documentation "Base condition for path resolution and source-preserving edits."))

(define-condition xml-path-error (xml-edit-error)
  ((index :initarg :index :initform nil :reader xml-path-error-index
          :documentation "The zero-based index into the path of a :SYNTAX error, else NIL."))
  (:documentation "A path is malformed, matches nothing, or names the wrong kind of target."))

(define-condition xml-unsupported-edit (xml-edit-error) ()
  (:documentation "An edit cannot be applied without risking the document."))

(define-condition xml-edit-value-error (xml-edit-error)
  ((cause :initarg :cause :initform nil :reader xml-edit-value-error-cause
          :documentation "The condition that made a fragment unacceptable, when there is one."))
  (:documentation "A value or fragment supplied to an edit is not acceptable."))
