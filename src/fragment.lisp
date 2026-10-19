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

(defun %trim-xml-space (string)
  "Return STRING without the XML whitespace at either end."
  (let ((start (position-if-not #'xml-space-char-p string)))
    (if start
        (subseq string start
                (1+ (position-if-not #'xml-space-char-p string :from-end t)))
        "")))

(defun %edit-fragment-element (context chain fragment success failure)
  "Call SUCCESS with the parsed element and trimmed fragment, or FAILURE with the condition."
  (%call-with-callbacks
   (lambda ()
     (let* ((path (path-context-path context))
            (trimmed (%trim-xml-space fragment))
            (wrapper
              (handler-case
                  (parse-xml (%edit-fragment-wrapper (%edit-scope-bindings chain) trimmed)
                             :resource-limits (path-context-resource-limits context)
                             :external-entity-policy
                             (path-context-external-entity-policy context))
                (error (condition) (xml-error 'xml-edit-value-error :fragment-unacceptable
                             :format-arguments (condition)
                             :condition-initargs
                             (:path path :cause condition
                              :reason (if (typep condition 'xml-resource-limit-exceeded)
                                          :resource-limit
                                          :malformed-fragment))))))
            (children (xml-element-children (first (xml-document-children wrapper)))))
       (unless (and children (null (rest children)) (xml-element-p (first children)))
         (xml-error 'xml-edit-value-error :fragment-not-single-element
                    :condition-initargs (:path path :reason :not-single-element)))
       (values (first children) trimmed)))
   success failure))
