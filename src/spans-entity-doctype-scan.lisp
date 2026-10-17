;;;; src/spans-entity-doctype-scan.lisp

(in-package #:cl-xml-kit)

(defun %span-replacement-scan (text context)
  "Return whether TEXT holds markup once references resolve, and the named entities it references.

This follows `%materialize-entity-source-into`, which resolves character and
predefined references before the tokenizer sees the replacement."
  (let ((position 0)
        (references nil))
    (loop
      (let ((marker (position-if (lambda (character) (find character "<&"))
                                 text :start position)))
        (cond
          ((null marker)
           (return (values nil references)))
          ((char= (char text marker) #\<)
           (return (values t references)))
          (t
           (let* ((end (position #\; text :start marker))
                  (body (subseq text (1+ marker) end)))
             (cond
               ((char= (char body 0) #\#)
                (when (char= (%parse-character-reference body context) #\<)
                  (return (values t references))))
               ((string= body "lt")
                (return (values t references)))
               ((not (assoc body +xml-predefined-entities+ :test #'string=))
                (push body references)))
             (setf position (1+ end)))))))))

(defun %span-markup-entity-names (context)
  "Return the sorted names of internal general entities in CONTEXT that bring in markup.

An entity qualifies when its own replacement text holds markup or it references
a qualifying entity."
  (let ((referrers (make-hash-table :test #'equal))
        (marked (make-hash-table :test #'equal))
        (pending nil))
    (maphash
     (lambda (name declaration)
       (unless (xml-entity-declaration-external-p declaration)
         (multiple-value-bind (markup-p references)
             (%span-replacement-scan
              (xml-entity-declaration-replacement-text declaration) context)
           (dolist (reference references)
             (push name (gethash reference referrers)))
           (when markup-p
             (setf (gethash name marked) t)
             (push name pending)))))
     (xml-entity-context-declarations context))
    (loop while pending
          do (dolist (referrer (gethash (pop pending) referrers))
               (unless (gethash referrer marked)
                 (setf (gethash referrer marked) t)
                 (push referrer pending))))
    (sort (loop for name being the hash-keys of marked collect name)
          #'string<)))

(defun %span-scan-doctype (scanner token)
  (let ((body (%xml-token-value token))
        (body-start (+ (xml-position-offset (xml-token-position token))
                       (length "<!DOCTYPE"))))
    (multiple-value-bind (name-start name-end subset-start subset-end)
        (%doctype-body-layout body)
      (let ((subset (and subset-start (subseq body subset-start subset-end))))
        (setf (span-scanner-doctype scanner)
              (%make-span-doctype
               (%span-token-start scanner token) (%span-token-end scanner token)
               (subseq body name-start name-end) subset
               (and subset
                    (%span-original scanner (+ body-start subset-start)))
               (and subset
                    (%span-original scanner (+ body-start subset-end)))))
        (%span-add-node scanner (span-scanner-doctype scanner))))))

(defun %span-scan-content (scanner token)
  (case (xml-token-kind token)
    ((:text :entity-reference :cdata)
     (%span-add-run-piece (car (span-scanner-stack scanner)) token))
    (:comment
     (%span-add-node scanner
                     (%make-span-comment (%span-token-start scanner token)
                                         (%span-token-end scanner token))))
    (:processing-instruction
     (%span-add-node scanner
                     (%make-span-processing-instruction
                      (%span-token-start scanner token)
                      (%span-token-end scanner token))))
    ((:start-tag-open :end-tag-open)
     (setf (span-scanner-open-token scanner) token))))

(defun %span-scan-top-level (scanner token)
  ;; Text, references, and EOF are not recorded; the parser rejects any token
  ;; out of place before the document ends.
  (case (xml-token-kind token)
    (:comment
     (%span-add-node scanner
                     (%make-span-comment (%span-token-start scanner token)
                                         (%span-token-end scanner token))))
    (:processing-instruction
     (cond
       ((not (%parser-xml-declaration-token-p token))
        (%span-add-node scanner
                        (%make-span-processing-instruction
                         (%span-token-start scanner token)
                         (%span-token-end scanner token))))
       ((zerop (xml-position-offset (xml-token-position token)))
        (setf (span-scanner-declaration scanner)
              (%make-span-node (%span-token-start scanner token)
                               (%span-token-end scanner token))))))
    (:doctype (%span-scan-doctype scanner token))
    (:start-tag-open (setf (span-scanner-open-token scanner) token))))

(defun %span-observe (scanner token)
  "Record TOKEN, which the parser has just read from the document, in SCANNER."
  (let ((open (span-scanner-open-token scanner)))
    (cond
      ((null open)
       (if (span-scanner-stack scanner)
           (%span-scan-content scanner token)
           (%span-scan-top-level scanner token)))
      ((eq (xml-token-kind open) :start-tag-open) (%span-start-tag-token scanner token))
      (t (%span-end-tag-token scanner token)))))
