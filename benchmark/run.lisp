;;;; Reproducible SBCL benchmark harness for cl-xml-kit.

(require :asdf)

(in-package #:cl-xml-kit/benchmark)

(defparameter *benchmark-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname
    (or *load-truename* *compile-file-truename*
        (error "benchmark/run.lisp has no load pathname")))))

(defconstant +mib+ (* 1024 1024))
(defconstant +max-pull-events+ 1000000)

(defstruct workload id input)
(defstruct benchmark-case id operation workload input-bytes thunk validator)
(defstruct (rng (:constructor make-rng-state (state))) state)

(defun env (name)
  (uiop:getenv name))

(defun ensure-sbcl ()
  #+sbcl t
  #-sbcl (error "The cl-xml-kit benchmark requires SBCL for allocation data."))

(defun parse-nonnegative-integer (name text)
  (unless (and text (plusp (length text))
               (every (lambda (character)
                        (<= (char-code #\0) (char-code character)
                            (char-code #\9)))
                      text))
    (error "~A must contain a non-negative decimal integer, got ~S" name text))
  (parse-integer text))

(defun setting-integer (name fallback minimum &optional alias)
  (let ((text (or (env name) (and alias (env alias)))))
    (let ((value (if text
                     (parse-nonnegative-integer (or name alias) text)
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

(defun api-type-p (name value)
  (let ((package (find-package "CL-XML-KIT")))
    (when package
      (multiple-value-bind (symbol status) (find-symbol name package)
        (when (and status (fboundp symbol))
          (funcall (symbol-function symbol) value))))))

(defun assert-document (document operation)
  (unless document
    (error "Correctness preflight ~A returned NIL" operation))
  (when (and (find-package "CL-XML-KIT")
             (multiple-value-bind (symbol status)
                 (find-symbol "XML-DOCUMENT-P" "CL-XML-KIT")
               (and status (fboundp symbol))))
    (unless (api-type-p "XML-DOCUMENT-P" document)
      (error "Correctness preflight ~A returned a non-document" operation)))
  document)

(defun xml->octets (string)
  (let ((octets (make-array (length string)
                            :element-type '(unsigned-byte 8))))
    (loop for index below (length string)
          do (setf (aref octets index) (char-code (char string index))))
    octets))

(defun make-text-xml (seed)
  (let ((target +mib+)
        (token (format nil "seed-~D" (mod seed 1000000))))
    (with-output-to-string (stream)
      (write-string "<root><text>" stream)
      (loop with count = 0
            while (< count target)
            do (write-string token stream)
               (incf count (length token)))
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

(defun validate-parse-result (value operation)
  (assert-document value operation)
  t)

(defun make-parse-case (workload input operation)
  (make-benchmark-case
   :id (format nil "~A/~A" operation (workload-id workload))
   :operation operation
   :workload (workload-id workload)
   :input-bytes (length input)
   :thunk (lambda () (api-call "PARSE-XML" operation input))
   :validator (lambda ()
                (validate-parse-result
                 (api-call "PARSE-XML" operation input) operation))))

(defun make-pull-case (workload)
  (let ((input (workload-input workload)))
    (make-benchmark-case
     :id (format nil "pull-events/~A" (workload-id workload))
     :operation "pull-events"
     :workload (workload-id workload)
     :input-bytes (length input)
     :thunk
     (lambda ()
       (let ((parser (api-call "MAKE-XML-PARSER" "pull-events" input))
             (count 0))
         (loop while (not (api-call "XML-PARSER-EOF-P" "pull-events" parser))
               do (api-call "NEXT-XML-EVENT" "pull-events" parser)
                  (incf count)
                  (when (> count +max-pull-events+)
                    (error "pull-events exceeded the event safety bound")))
         count))
     :validator
     (lambda ()
       (let ((parser (api-call "MAKE-XML-PARSER" "pull-events" input))
             (count 0))
         (loop while (not (api-call "XML-PARSER-EOF-P" "pull-events" parser))
               do (api-call "NEXT-XML-EVENT" "pull-events" parser)
                  (incf count)
                  (when (> count +max-pull-events+)
                    (error "pull-events correctness preflight exceeded bound")))
         (unless (plusp count)
           (error "pull-events correctness preflight produced no events"))
         t)))))

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
         (validate-parse-result
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
        (when (selected-p operation-selection "dom")
          (push (make-parse-case workload (workload-input workload) "dom") cases))
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
  #+sbcl (sb-ext:gc :full t)
  (let* ((start-time (get-internal-real-time))
         #+sbcl (start-bytes (sb-ext:get-bytes-consed))
         (value (funcall (benchmark-case-thunk case)))
         (end-time (get-internal-real-time))
         #+sbcl (end-bytes (sb-ext:get-bytes-consed))
         (ticks (- end-time start-time))
         (seconds (/ ticks (float internal-time-units-per-second 1d0))))
    (declare (ignore value))
    (list :ticks ticks
          :seconds seconds
          :allocation-bytes #+sbcl (- end-bytes start-bytes) #-sbcl 0)))

(defun median (numbers)
  (let ((sorted (sort (copy-seq numbers) #'<)))
    (if (oddp (length sorted))
        (elt sorted (floor (length sorted) 2))
        (/ (+ (elt sorted (1- (floor (length sorted) 2)))
              (elt sorted (floor (length sorted) 2)))
           2d0))))

(defun summary (samples key)
  (let ((numbers (mapcar (lambda (sample) (getf sample key)) samples)))
    (list :median (median numbers)
          :min (reduce #'min numbers)
          :max (reduce #'max numbers))))

(defun tsv-field (value)
  (let ((text (princ-to-string (or value "unknown"))))
    (with-output-to-string (stream)
      (loop for character across text
            do (case character
                 (#\Tab (write-string "\\t" stream))
                 (#\Newline (write-string "\\n" stream))
                 (#\Return (write-string "\\r" stream))
                 (#\\ (write-string "\\\\" stream))
                 (t (write-char character stream)))))))

(defun emit-fields (fields)
  (loop for field in fields
        for first = t then nil
        do (unless first (write-char #\Tab))
           (write-string (tsv-field field)))
  (terpri))

(defun command-text (program arguments &optional (empty-value "unknown"))
  (handler-case
      (let ((text (uiop:run-program (cons program arguments)
                                    :directory *benchmark-root*
                                    :output :string
                                    :error-output :output
                                    :ignore-error-status t)))
        (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (or text ""))))
          (if (plusp (length trimmed)) trimmed empty-value)))
    (error () empty-value)))

(defun source-value (relative-path)
  (let ((path (merge-pathnames relative-path *benchmark-root*)))
    (if (probe-file path)
        (command-text "shasum" (list "-a" "256" (namestring path)))
        "unknown")))

(defun metadata (seed operations workloads warmups samples)
  (list (list "schema" "cl-xml-kit-benchmark")
        (list "schema_version" "1")
        (list "source_root" (namestring *benchmark-root*))
        (list "revision" (command-text "git" (list "rev-parse" "HEAD")))
        (list "revision_tree" (command-text "git" (list "rev-parse" "HEAD^{tree}")))
        (list "git_dirty"
              (let ((status (command-text "git" (list "status" "--porcelain") "")))
                (if (string= status "unknown")
                    "unknown"
                    (if (plusp (length status)) "dirty" "clean"))))
        (list "sbcl" (lisp-implementation-version))
        (list "lisp_implementation" (lisp-implementation-type))
        (list "host" (machine-instance))
        (list "machine" (machine-type))
        (list "os" (software-type))
        (list "os_version" (software-version))
        (list "seed" seed)
        (list "operations" (format nil "~{~A~^,~}" operations))
        (list "workloads" (format nil "~{~A~^,~}" workloads))
        (list "warmup_rounds" warmups)
        (list "sample_rounds" samples)
        (list "timer_units_per_second" internal-time-units-per-second)
        (list "allocation_counter" "sb-ext:get-bytes-consed")
        (list "hash_benchmark_package" (source-value "benchmark/package.lisp"))
        (list "hash_benchmark_run" (source-value "benchmark/run.lisp"))
        (list "hash_benchmark_readme" (source-value "benchmark/README.md"))))

(defun emit-report (cases sample-table orders metadata-rows)
  (emit-fields '("record" "key" "value"))
  (dolist (row metadata-rows) (emit-fields (list "meta" (first row) (second row))))
  (emit-fields '("record" "phase" "round" "round_seed" "case_count" "case_ids"))
  (dolist (order orders)
    (emit-fields (list "order" (getf order :phase) (getf order :round)
                       (getf order :seed) (length (getf order :ids))
                       (format nil "~{~A~^,~}" (getf order :ids)))))
  (emit-fields '("record" "case_id" "operation" "workload" "samples"
                 "input_bytes" "wall_seconds_median" "wall_seconds_min"
                 "wall_seconds_max" "allocation_bytes_median"
                 "allocation_bytes_min" "allocation_bytes_max"))
  (dolist (case cases)
    (let* ((samples (gethash case sample-table))
           (time (summary samples :seconds))
           (allocation (summary samples :allocation-bytes)))
      (emit-fields
       (list "result" (benchmark-case-id case) (benchmark-case-operation case)
             (benchmark-case-workload case) (length samples)
             (benchmark-case-input-bytes case)
             (getf time :median) (getf time :min) (getf time :max)
             (getf allocation :median) (getf allocation :min)
             (getf allocation :max))))))

(defun main ()
  "Run correctness preflight, warmups, and reproducible benchmark samples."
  (ensure-sbcl)
  (let* ((seed (setting-integer "BENCH_SEED" 20261007 0))
         (warmups (setting-integer "BENCH_WARMUP" 2 2))
         (samples (setting-integer "BENCH_SAMPLES" 10 10 "BENCH_ITERATIONS"))
         (operations (split-selection "BENCH_OPERATIONS"
                                      (or (env "BENCH_OPERATIONS") "all")))
         (workload-selection (split-selection "BENCH_WORKLOADS"
                                              (or (env "BENCH_WORKLOADS") "all")))
         (cases (make-cases seed operations workload-selection))
         (random-state (make-rng seed))
         (sample-table (make-hash-table :test #'eq))
         (orders nil))
    (validate-cases cases)
    (format *error-output* "benchmark preflight passed: cases=~D seed=~D~%"
            (length cases) seed)
    (labels ((run-round (phase round measure-p)
               (let* ((round-seed (rng-state random-state))
                      (ordered (shuffle-cases (coerce cases 'vector) random-state)))
                 (push (list :phase phase :round round :seed round-seed
                             :ids (map 'list #'benchmark-case-id ordered))
                       orders)
                 (loop for case across ordered
                       do (if measure-p
                              (push (measure-sample case) (gethash case sample-table))
                              (funcall (benchmark-case-thunk case)))))))
      (dotimes (round warmups) (run-round "warmup" round nil))
      (dotimes (round samples) (run-round "sample" round t)))
    (emit-report cases sample-table (nreverse orders)
                 (metadata seed operations workload-selection warmups samples))
    t))

