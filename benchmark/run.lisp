;;;; Reproducible SBCL benchmark harness for cl-xml-kit.

(require :asdf)

(in-package #:cl-xml-kit/benchmark)

(defconstant +mib+ (* 1024 1024))
(defconstant +max-pull-events+ 1000000)

(defstruct workload id input)
(defstruct benchmark-case id operation workload input-bytes thunk validator)
(defstruct (rng (:constructor make-rng-state (state))) state)

(defun parse-nonnegative-integer (name text)
  (unless (and text (plusp (length text))
               (every (lambda (character)
                        (<= (char-code #\0) (char-code character)
                            (char-code #\9)))
                      text))
    (error "~A must contain a non-negative decimal integer, got ~S" name text))
  (parse-integer text))

(defun setting-integer (name fallback minimum)
  (let ((text (host-kit:getenv name)))
    (let ((value (if text
                     (parse-nonnegative-integer name text)
                     fallback)))
      (unless (>= value minimum)
        (error "~A must be at least ~D, got ~D" name minimum value))
      value)))

(defun split-selection (name text)
  (let ((items nil)
        (start 0))
    (labels ((add (end)
               (let ((item (string-trim '(#\Space #\Tab) (subseq text start end))))
                 (when (zerop (length item))
                   (error "~A contains an empty selector" name))
                 (push item items))))
      (loop for end = (position #\, text :start start)
            do (if end
                   (progn (add end) (setf start (1+ end)))
                   (progn (add (length text)) (return))))
      (nreverse items))))

(defun selected-p (selection value)
  (or (member "all" selection :test #'string=)
      (member value selection :test #'string=)))

(defun make-rng (seed)
  (make-rng-state (mod seed 2147483647)))

(defun next-random (random-state)
  (setf (rng-state random-state)
        (mod (+ (* 48271 (rng-state random-state)) 1) 2147483647)))

(defun shuffle-cases (cases random-state)
  (let ((result (copy-seq cases)))
    (loop for index downfrom (1- (length result)) above 0
          for other = (mod (next-random random-state) (1+ index))
          do (rotatef (aref result index) (aref result other)))
    result))

(defun api-function (name operation)
  (let ((package (find-package "CL-XML-KIT")))
    (multiple-value-bind (symbol status)
        (if package (find-symbol name package) (values nil nil))
      (unless (and status (fboundp symbol))
        (error 'benchmark-api-error
               :operation operation
               :symbol (format nil "CL-XML-KIT:~A" name)))
      (symbol-function symbol))))

(defun api-call (name operation &rest arguments)
  (apply (api-function name operation) arguments))

(defun assert-document (document operation)
  (unless document
    (error "Correctness preflight ~A returned NIL" operation))
  (unless (api-call "XML-DOCUMENT-P" operation document)
    (error "Correctness preflight ~A returned a non-document" operation))
  document)

(defun xml->octets (string)
  (let ((octets (make-array (length string)
                            :element-type '(unsigned-byte 8))))
    (loop for index below (length string)
          do (setf (aref octets index) (char-code (char string index))))
    octets))

(defun make-text-xml (seed)
  "Return XML whose text node contains exactly +MIB+ characters."
  (let ((token (format nil "seed-~D" (mod seed 1000000))))
    (with-output-to-string (stream)
      (write-string "<root><text>" stream)
      (loop for remaining downfrom +mib+ above 0 by (length token)
            do (write-string token stream :end (min (length token) remaining)))
      (write-string "</text></root>" stream))))

(defun make-deep-xml (&optional (depth 512))
  (with-output-to-string (stream)
    (write-string "<root>" stream)
    (loop repeat (1- depth) do (write-string "<n>" stream))
    (write-string "x" stream)
    (loop repeat (1- depth) do (write-string "</n>" stream))
    (write-string "</root>" stream)))

(defun make-attribute-xml (seed)
  (with-output-to-string (stream)
    (write-string "<root" stream)
    (dotimes (index 128)
      (format stream " a~D=\"v~D-~D\"" index index (mod seed 1000)))
    (write-string "/>" stream)))

(defun make-namespace-xml ()
  "Return a namespace-heavy, mixed-content XML workload."
  "<root xmlns=\"urn:root\" xmlns:a=\"urn:a\"><a:item id=\"1\">text</a:item><!--c--><?p d?><item>tail</item></root>")

(defun make-mixed-xml (seed)
  (format nil "<root>before<!--~D--><?p value?><item>middle&amp;end</item>after</root>"
          (mod seed 10000)))

(defun make-workloads (seed)
  (list (make-workload :id "small" :input
                       (format nil "<root><item id=\"~D\">text</item></root>"
                               (mod seed 10000)))
        (make-workload :id "text-1mib" :input (make-text-xml seed))
        (make-workload :id "deep-512" :input (make-deep-xml))
        (make-workload :id "attributes-128" :input (make-attribute-xml seed))
        (make-workload :id "namespaces" :input (make-namespace-xml))
        (make-workload :id "mixed-content" :input (make-mixed-xml seed))))

(defun make-parse-case (workload input operation)
  (make-benchmark-case
   :id (format nil "~A/~A" operation (workload-id workload))
   :operation operation
   :workload (workload-id workload)
   :input-bytes (length input)
   :thunk (lambda () (api-call "PARSE-XML" operation input))
   :validator (lambda ()
                (assert-document
                 (api-call "PARSE-XML" operation input) operation))))

(defun count-pull-events (input)
  (let ((parser (api-call "MAKE-XML-PARSER" "pull-events" input))
        (count 0))
    (loop while (not (api-call "XML-PARSER-EOF-P" "pull-events" parser))
          do (api-call "NEXT-XML-EVENT" "pull-events" parser)
             (incf count)
             (when (> count +max-pull-events+)
               (error "pull-events exceeded the event safety bound")))
    count))

(defun make-pull-case (workload)
  (let ((input (workload-input workload)))
    (make-benchmark-case
     :id (format nil "pull-events/~A" (workload-id workload))
     :operation "pull-events"
     :workload (workload-id workload)
     :input-bytes (length input)
     :thunk (lambda () (count-pull-events input))
     :validator
     (lambda ()
       (unless (plusp (count-pull-events input))
         (error "pull-events correctness preflight produced no events"))
       t))))

(defun make-serialize-case (workload)
  (let* ((input (workload-input workload))
         (document (assert-document
                    (api-call "PARSE-XML" "serialize-preflight" input)
                    "serialize-preflight")))
    (make-benchmark-case
     :id (format nil "serialize/~A" (workload-id workload))
     :operation "serialize"
     :workload (workload-id workload)
     :input-bytes (length input)
     :thunk (lambda () (api-call "XML->STRING" "serialize" document))
     :validator
     (lambda ()
       (let ((serialized (api-call "XML->STRING" "serialize" document)))
         (unless (and (stringp serialized) (plusp (length serialized)))
           (error "serialization correctness preflight returned empty output"))
         (assert-document
          (api-call "PARSE-XML" "serialize-round-trip" serialized)
          "serialize-round-trip"))))))

(defun make-cases (seed operation-selection workload-selection)
  (let ((cases nil))
    (dolist (workload (make-workloads seed))
      (when (selected-p workload-selection (workload-id workload))
        (when (selected-p operation-selection "parse")
          (push (make-parse-case workload (workload-input workload) "parse") cases))
        (when (and (string= (workload-id workload) "small")
                   (selected-p operation-selection "utf8-parse"))
          (let ((octets (xml->octets (workload-input workload))))
            (push (make-parse-case workload octets "utf8-parse") cases)))
        (when (and (string= (workload-id workload) "mixed-content")
                   (selected-p operation-selection "pull"))
          (push (make-pull-case workload) cases))
        (when (selected-p operation-selection "serialize")
          (push (make-serialize-case workload) cases))))
    (unless cases
      (error "Benchmark selector produced no cases"))
    (nreverse cases)))

(defun validate-cases (cases)
  (dolist (case cases)
    (unless (and (benchmark-case-thunk case)
                 (benchmark-case-validator case)
                 (plusp (benchmark-case-input-bytes case)))
      (error "Benchmark case ~A has incomplete or empty input"
             (benchmark-case-id case)))
    (handler-case
        (unless (funcall (benchmark-case-validator case))
          (error "validator returned NIL"))
      (error (condition)
        (error "Correctness preflight failed for ~A: ~A"
               (benchmark-case-id case) condition))))
  t)

(defun measure-sample (case)
  (sb-ext:gc :full t)
  (let* ((allocation-bytes nil)
        (measurement
          (cl-weave:measure
           (lambda ()
             (let ((start-bytes (sb-ext:get-bytes-consed)))
               (prog1 (funcall (benchmark-case-thunk case))
                 (setf allocation-bytes
                       (- (sb-ext:get-bytes-consed) start-bytes)))))
           :samples 1
           :iterations 1)))
    (unless (integerp allocation-bytes)
      (error "Benchmark case ~A did not record an allocation sample"
             (benchmark-case-id case)))
    (values measurement allocation-bytes)))

(defun main ()
  (let* ((seed (setting-integer "BENCH_SEED" 20261007 0))
         (warmups (setting-integer "BENCH_WARMUP" 2 2))
         (samples (setting-integer "BENCH_SAMPLES" 10 10))
         (operations (split-selection "BENCH_OPERATIONS"
                                      (or (host-kit:getenv "BENCH_OPERATIONS") "all")))
         (workload-selection
           (split-selection "BENCH_WORKLOADS"
                            (or (host-kit:getenv "BENCH_WORKLOADS") "all")))
         (cases (make-cases seed operations workload-selection))
         (random-state (make-rng seed)))
    (validate-cases cases)
    (format t "cl-xml-kit benchmark~%seed=~D warmups=~D samples=~D cases=~D~%"
            seed warmups samples (length cases))
    (dotimes (round warmups)
      (loop for case across (shuffle-cases (coerce cases 'vector) random-state)
            do (funcall (benchmark-case-thunk case))))
    (dotimes (round samples)
      (loop for case across (shuffle-cases (coerce cases 'vector) random-state)
            do (multiple-value-bind (measurement allocation-bytes)
                   (measure-sample case)
                 (unless (funcall (benchmark-case-validator case))
                   (error "Correctness check failed after sample for ~A"
                          (benchmark-case-id case)))
                 (format t "case=~A operation=~A workload=~A input-bytes=~D sample=~D time-ms=~,3F allocation-bytes=~D~%"
                         (benchmark-case-id case)
                         (benchmark-case-operation case)
                         (benchmark-case-workload case)
                         (benchmark-case-input-bytes case)
                         round
                         (cl-weave:median-ms measurement)
                         allocation-bytes))))
    t))

