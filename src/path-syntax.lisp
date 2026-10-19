(in-package #:cl-xml-kit)

(defun %path-error (context reason message &key index)
  (error 'xml-path-error :path (path-context-path context) :reason reason
                         :index index :message message))

(defun %path-refuse (context reason message)
  (error 'xml-unsupported-edit :path (path-context-path context) :reason reason
                               :message message))

(defun %path-syntax-error (path index expectation)
  (xml-error 'xml-path-error :path-syntax
             :format-arguments (index expectation)
             :condition-initargs (:path path :reason :syntax :index index)))

(defun %path-char (path index)
  (and (< index (length path)) (char path index)))

(defun %path-read-ncname (path index expectation)
  "Return the index after the NCName that starts at INDEX of PATH."
  (let ((character (%path-char path index)))
    (unless (and character (xml-ncname-start-char-p character))
      (%path-syntax-error path index expectation)))
  (loop for end from (1+ index)
        for character = (%path-char path end)
        while (and character (xml-ncname-char-p character))
        finally (return end)))

(defun %path-read-qname (path index expectation)
  "Return the qualified name that starts at INDEX of PATH, and the index after it."
  (let ((end (%path-read-ncname path index expectation)))
    (when (eql (%path-char path end) #\:)
      (setf end (%path-read-ncname path (1+ end) "a local name after the colon")))
    (values (subseq path index end) end)))

(defun %path-read-position (path index)
  "Return the position written at INDEX of PATH, and the index after its closing bracket."
  (let ((character (%path-char path index)))
    (unless (and character (char<= #\1 character #\9))
      (%path-syntax-error path index "a positive position")))
  (let ((end (loop for end from (1+ index)
                   for character = (%path-char path end)
                   while (and character (char<= #\0 character #\9))
                   finally (return end))))
    (unless (eql (%path-char path end) #\])
      (%path-syntax-error path end "] after the position"))
    (values (parse-integer path :start index :end end) (1+ end))))

(defun %path-read-element-step (path index)
  "Return the PATH-STEP written at INDEX of PATH, and the index after it."
  (multiple-value-bind (name end)
      (if (eql (%path-char path index) #\*)
          (values :any (1+ index))
          (%path-read-qname path index "an element name or *"))
    (if (eql (%path-char path end) #\[)
        (multiple-value-bind (ordinal after) (%path-read-position path (1+ end))
          (values (%make-path-step name ordinal) after))
        (values (%make-path-step name nil) end))))

(defun %path-read-segment (path index first-p)
  "Read the segment of PATH that starts at INDEX, just after a slash.

Return :STEP and its PATH-STEP, :ATTRIBUTE and the attribute name, or :TEXT and
NIL, then the index after the segment. The first segment is always a step."
  (cond
    ((null (%path-char path index))
     (%path-syntax-error path index "a step after /"))
    ((and (not first-p) (char= (char path index) #\@))
     (multiple-value-bind (name end)
         (%path-read-qname path (1+ index) "an attribute name")
       (values :attribute name end)))
    ((and (not first-p)
          (<= (+ index 6) (length path))
          (string= "text()" path :start2 index :end2 (+ index 6)))
     (values :text nil (+ index 6)))
    (t
     (multiple-value-bind (step end) (%path-read-element-step path index)
       (values :step step end)))))

(defun %path-parse (path success failure)
  "Call SUCCESS once with the parsed path, or FAILURE once with the condition."
  (%call-with-callbacks
   (lambda ()
     (check-argument path stringp xml-input-error :label-must-be-string
                     :format-arguments ("PATH"))
     (unless (eql (%path-char path 0) #\/)
       (%path-syntax-error path 0 "a path starting with /"))
     (let ((index 1)
           (steps nil))
       (loop
         (multiple-value-bind (kind value end) (%path-read-segment path index (null steps))
           (ecase kind
             (:step
              (push value steps)
              (cond
                ((= end (length path))
                 (return (%make-parsed-path (nreverse steps) :element nil)))
                ((char/= (char path end) #\/)
                 (%path-syntax-error path end "/ or the end of the path")))
              (setf index (1+ end)))
             ((:attribute :text)
              (unless (= end (length path))
                (%path-syntax-error path end "the end of the path"))
              (return (%make-parsed-path (nreverse steps) kind value))))))))
   success failure))
