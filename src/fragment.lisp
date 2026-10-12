;;;; src/fragment.lisp
;;;;
;;;; Fragments that an edit writes into a document are parsed first, inside a
;;;; wrapper element that declares the namespaces in scope where they land.
;;;; Anything the parser rejects there is refused before the source is touched.

(in-package #:cl-xml-kit)

(defun %edit-convert-line-breaks (string line-break)
  "Return STRING with each CRLF, CR, and LF replaced by the style LINE-BREAK, :CRLF, :LF, or :CR."
  (let ((replacement (ecase line-break
                       (:crlf (coerce '(#\Return #\Linefeed) 'string))
                       (:lf (string #\Linefeed))
                       (:cr (string #\Return))))
        (index 0))
    (with-output-to-string (stream)
      (loop while (< index (length string))
            do (let ((character (char string index)))
                 (cond
                   ((and (char= character #\Return)
                         (< (1+ index) (length string))
                         (char= (char string (1+ index)) #\Linefeed))
                    (write-string replacement stream)
                    (incf index 2))
                   ((or (char= character #\Return) (char= character #\Linefeed))
                    (write-string replacement stream)
                    (incf index))
                   (t
                    (write-char character stream)
                    (incf index))))))))

(defun %edit-scope-bindings (chain)
  "Return the namespace bindings in scope for the innermost element of CHAIN.

Each binding is a (PREFIX . URI) pair, with PREFIX NIL for the default namespace;
an undeclared default namespace is left out."
  (let ((bindings nil))
    (dolist (element (reverse chain))
      (dolist (binding (xml-element-namespace-declarations element))
        (let ((prefix (xml-namespace-binding-prefix binding)))
          (setf bindings (remove prefix bindings :key #'car :test #'equal))
          (push (cons prefix (xml-namespace-binding-namespace-uri binding)) bindings))))
    (remove-if (lambda (binding)
                 (and (null (car binding)) (zerop (length (cdr binding)))))
               (nreverse bindings))))

(defun %edit-fragment-wrapper (bindings fragment)
  "Return a document that wraps FRAGMENT in an element declaring BINDINGS."
  (with-output-to-string (stream)
    (write-string "<w" stream)
    (loop for (prefix . uri) in bindings
          do (write-string " xmlns" stream)
             (when prefix
               (write-char #\: stream)
               (write-string prefix stream))
             (write-string "=\"" stream)
             (write-string (%edit-escape-attribute uri #\") stream)
             (write-char #\" stream))
    (write-char #\> stream)
    (write-string fragment stream)
    (write-string "</w>" stream)))

(defun %edit-fragment-element (context chain fragment)
  "Parse FRAGMENT as it would stand inside the innermost element of CHAIN.

Return the one element it consists of, and FRAGMENT without the XML whitespace
around it. A fragment that does not parse in a wrapper that declares the
namespaces in scope signals XML-EDIT-VALUE-ERROR :MALFORMED-FRAGMENT, or
:RESOURCE-LIMIT for a limit of the caller, with the parse condition as its
cause; one that parses to anything but a single element signals
:NOT-SINGLE-ELEMENT."
  (let* ((path (path-context-path context))
         (trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) fragment))
         (wrapper (handler-case
                      (parse-xml (%edit-fragment-wrapper (%edit-scope-bindings chain) trimmed)
                                 :resource-limits (path-context-resource-limits context)
                                 :external-entity-policy
                                 (path-context-external-entity-policy context))
                    (error (condition)
                      (error 'xml-edit-value-error
                             :path path :cause condition
                             :reason (if (typep condition 'xml-resource-limit-exceeded)
                                         :resource-limit
                                         :malformed-fragment)
                             :message (format nil "The fragment is not acceptable: ~A"
                                              condition)))))
         (children (xml-element-children (first (xml-document-children wrapper)))))
    (unless (and children (null (rest children)) (xml-element-p (first children)))
      (error 'xml-edit-value-error
             :path path :reason :not-single-element
             :message "The fragment must be exactly one element."))
    (values (first children) trimmed)))
