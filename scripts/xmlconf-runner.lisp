;;;; XMLConf catalog analysis and execution boundary.

(require :asdf)

(defpackage #:cl-xml-kit/xmlconf
  (:use #:cl)
  (:export #:analyze-xmlconf
           #:run-xmlconf
           #:xmlconf-analysis
           #:xmlconf-analysis-catalog-root
           #:xmlconf-analysis-catalog-count
           #:xmlconf-analysis-test-count
           #:xmlconf-analysis-accepted
           #:xmlconf-analysis-rejected
           #:xmlconf-analysis-skipped
           #:xmlconf-analysis-excluded
           #:xmlconf-analysis-unmatched-exclusions))

(in-package #:cl-xml-kit/xmlconf)

(defparameter *selected-recommendations*
  '("XML1.0" "XML1.0-ERRATA4E" "NS1.0" "NS1.0-ERRATA1E"))

(defparameter *xmlconf-execution-mode* :catalog
  "Select catalog analysis by default; the command line can request execution.")

(defparameter *default-exclusions-file*
  (merge-pathnames "xmlconf-exclusions.sexp"
                   (make-pathname :directory
                                  (append (pathname-directory *load-truename*)
                                          nil)
                                  :name nil
                                  :type nil
                                  :defaults *load-truename*)))

(defstruct xmlconf-test
  id
  type
  entities
  recommendation
  version
  edition
  namespace
  output
  uri
  source
  base)

(defstruct xmlconf-case-result
  id
  category
  reason
  test)

(defstruct xmlconf-analysis
  catalog-root
  catalog-count
  test-count
  accepted
  rejected
  skipped
  excluded
  unmatched-exclusions
  output-count
  results)

(defun getenv (name)
  #+sbcl (sb-ext:posix-getenv name)
  #-sbcl nil)

(defun whitespacep (character)
  (member character '(#\Space #\Tab #\Newline #\Return #\Page)
          :test #'char=))

(defun skip-whitespace (text position)
  (loop while (and (< position (length text))
                   (whitespacep (char text position)))
        do (incf position)
        finally (return position)))

(defun token-end (text position)
  (loop while (and (< position (length text))
                   (not (whitespacep (char text position)))
                   (not (member (char text position)
                                '(#\< #\> #\= #\" #\' #\[ #\]))))
        do (incf position)
        finally (return position)))

(defun split-whitespace (text)
  (let ((result nil)
        (position 0))
    (loop while (< position (length text)) do
      (setf position (skip-whitespace text position))
      (when (< position (length text))
        (let ((end (token-end text position)))
          (push (subseq text position end) result)
          (setf position end))))
    (nreverse result)))

(defun string-attribute (attributes name &optional default)
  (let ((entry (find name attributes :key #'car :test #'string-equal)))
    (if entry (cdr entry) default)))

(defun markup-end (text start)
  (let ((delimiter nil)
        (subset-depth 0)
        (position start))
    (loop while (< position (length text)) do
      (let ((character (char text position)))
        (cond
          (delimiter
           (when (char= character delimiter)
             (setf delimiter nil)))
          ((or (char= character #\") (char= character #\'))
           (setf delimiter character))
          ((char= character #\[)
           (incf subset-depth))
          ((and (char= character #\]) (> subset-depth 0))
           (decf subset-depth))
          ((and (char= character #\>) (zerop subset-depth))
           (return-from markup-end position)))
        (incf position)))
    (error "Unterminated markup starting at character ~D" start)))

(defun quoted-end (text position)
  (let ((delimiter (char text position)))
    (unless (member delimiter '(#\" #\'))
      (error "Expected a quoted value at character ~D" position))
    (loop for end from (1+ position) below (length text)
          when (char= (char text end) delimiter)
            do (return end)
          finally (error "Unterminated quoted value at character ~D" position))))

(defun parse-attributes (text start end)
  (let ((attributes nil)
        (position start))
    (loop while (< position end) do
      (setf position (skip-whitespace text position))
      (when (and (< position end)
                 (char= (char text position) #\/))
        (incf position))
      (when (< position end)
        (let* ((name-start position)
               (name-end (token-end text position)))
          (when (= name-end position)
            (return))
          (setf position (skip-whitespace text name-end))
          (unless (and (< position end)
                       (char= (char text position) #\=))
            (error "Attribute ~A has no value" (subseq text position name-end)))
          (setf position (skip-whitespace text (1+ position)))
          (let ((value-end (quoted-end text position)))
            (push (cons (subseq text name-start name-end)
                        (subseq text (1+ position) value-end))
                  attributes)
            (setf position (1+ value-end))))))
    (nreverse attributes)))

(defun external-entity-declarations (text)
  (let ((entities nil)
        (position 0))
    (loop for marker = (search "<!ENTITY" text :start2 position)
          while marker do
      (let* ((start (+ marker (length "<!ENTITY")))
             (end (markup-end text marker))
             (cursor (skip-whitespace text start)))
        (when (and (< cursor end) (char= (char text cursor) #\%))
          (setf cursor (skip-whitespace text (1+ cursor))))
        (let ((name-end (token-end text cursor)))
          (when (< name-end end)
            (let ((system (search "SYSTEM" text :start2 name-end :end2 end
                                  :test #'char-equal)))
              (when system
                (let ((value-start
                        (skip-whitespace text (+ system (length "SYSTEM")))))
                  (when (< value-start end)
                    (let ((value-end (quoted-end text value-start)))
                      (when (< value-end end)
                        (push (cons (subseq text cursor name-end)
                                    (subseq text (1+ value-start) value-end))
                              entities)))))))))
        (setf position (1+ end))))
    entities))

(defun entity-references (text)
  (let ((references nil)
        (position 0))
    (loop for ampersand = (position #\& text :start position)
          while ampersand do
      (let ((semicolon (position #\; text :start (1+ ampersand))))
        (unless semicolon (return))
        (let ((name (subseq text (1+ ampersand) semicolon)))
          (unless (or (string= name "amp")
                      (string= name "lt")
                      (string= name "gt")
                      (string= name "quot")
                      (string= name "apos")
                      (and (> (length name) 1)
                           (char= (char name 0) #\#)))
            (push name references)))
        (setf position (1+ semicolon))))
    (nreverse references)))

(defun scan-catalog-tags (text source callback)
  (let ((position 0)
        (stack nil)
        (base nil))
    (labels ((pop-tag ()
               (when stack
                 (pop stack)
                 (setf base (cdr (first stack)))))
             (push-tag (name attributes self-closing)
               (unless self-closing
                 (let ((new-base (if (string-equal name "TESTCASES")
                                     (or (string-attribute attributes "xml:base")
                                         base)
                                     base)))
                   (push (cons name new-base) stack)
                   (setf base new-base)))))
      (loop for less = (position #\< text :start position)
            while less do
        (cond
          ((and (<= (+ less 4) (length text))
                (string= "<!--" text :start2 less :end2 (+ less 4)))
           (let ((end (search "-->" text :start2 (+ less 4))))
             (unless end (error "Unterminated XML comment in ~A" source))
             (setf position (+ end 3))))
          ((and (<= (+ less 9) (length text))
                (string= "<![CDATA[" text :start2 less :end2 (+ less 9)))
           (let ((end (search "]]>" text :start2 (+ less 9))))
             (unless end (error "Unterminated CDATA section in ~A" source))
             (setf position (+ end 3))))
          ((and (< (1+ less) (length text))
                (char= (char text (1+ less)) #\?))
           (let ((end (search "?>" text :start2 (+ less 2))))
             (unless end (error "Unterminated processing instruction in ~A" source))
             (setf position (+ end 2))))
          ((and (< (1+ less) (length text))
                (char= (char text (1+ less)) #\!))
           (setf position (1+ (markup-end text less))))
          ((and (< (1+ less) (length text))
                (char= (char text (1+ less)) #\/))
           (let ((end (markup-end text less)))
             (pop-tag)
             (setf position (1+ end))))
          (t
           (let* ((name-start (1+ less))
                  (name-end (token-end text name-start))
                  (end (markup-end text less))
                  (self-closing (char= (char text (1- end)) #\/))
                  (attributes (parse-attributes text name-end end))
                  (name (subseq text name-start name-end)))
             (when (string-equal name "TEST")
               (funcall callback attributes source base))
             (push-tag name attributes self-closing)
             (setf position (1+ end)))))))))

(defun read-catalog-file (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (with-output-to-string (text)
      (loop for character = (read-char stream nil nil)
            while character
            do (write-char character text)))))

(defun directory-pathname (path)
  (let ((pathname (pathname path)))
    (make-pathname :directory (pathname-directory pathname)
                   :name nil
                   :type nil
                   :defaults pathname)))

(defun existing-directory-p (path)
  (let ((found (probe-file path)))
    (and found (null (pathname-name (truename found))))))

(defun find-catalog-root (directory)
  (let* ((dir (directory-pathname directory))
         (direct (merge-pathnames "xmlconf.xml" dir))
         (nested (merge-pathnames "xmlconf/xmlconf.xml" dir)))
    (cond
      ((probe-file direct) (truename direct))
      ((probe-file nested) (truename nested))
      (t (error "XMLConf catalog root xmlconf.xml was not found below ~A" dir)))))

(defun resolve-entity-path (source system-id)
  (let ((path (merge-pathnames system-id (directory-pathname source))))
    (or (probe-file path)
        (error "XMLConf external catalog entity ~A from ~A was not found"
               system-id source))))

(defun load-exclusions (&optional (path *default-exclusions-file*))
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((entries (read stream nil nil)))
      (unless (listp entries)
        (error "XMLConf exclusions must be an association list: ~A" path))
      (let ((table (make-hash-table :test #'equal)))
        (dolist (entry entries table)
          (unless (and (consp entry)
                       (stringp (car entry))
                       (stringp (cdr entry)))
            (error "Invalid XMLConf exclusion entry ~S" entry))
          (when (gethash (car entry) table)
            (error "Duplicate XMLConf exclusion ID ~A" (car entry)))
          (setf (gethash (car entry) table) (cdr entry)))))))

(defun make-test-from-attributes (attributes source base)
  (let ((id (string-attribute attributes "ID")))
    (unless id (error "XMLConf TEST without ID in ~A: ~S" source attributes))
    (make-xmlconf-test
     :id id
     :type (string-downcase (string-attribute attributes "TYPE" ""))
     :entities (string-downcase (string-attribute attributes "ENTITIES" "none"))
     :recommendation (string-upcase
                      (string-attribute attributes "RECOMMENDATION" "XML1.0"))
     :version (string-attribute attributes "VERSION")
     :edition (string-attribute attributes "EDITION")
     :namespace (string-downcase
                 (or (string-attribute attributes "NAMESPACE")
                     (string-attribute attributes "NAMESPACES" "yes")))
     :output (string-attribute attributes "OUTPUT")
     :uri (string-attribute attributes "URI")
     :source source
     :base base)))

(defun collect-catalog (root)
  (let ((queue (list root))
        (visited (make-hash-table :test #'equal))
        (catalogs nil)
        (tests nil))
    (loop while queue do
      (let* ((current (pop queue))
             (key (namestring (truename current))))
        (unless (gethash key visited)
          (setf (gethash key visited) t)
          (let* ((text (read-catalog-file current))
                 (entities (external-entity-declarations text)))
            (push current catalogs)
            (scan-catalog-tags
             text current
             (lambda (attributes source base)
               (push (make-test-from-attributes attributes source base) tests)))
            (dolist (reference (entity-references text))
              (let ((system-id (cdr (assoc reference entities :test #'string=))))
                (when system-id
                  (push (resolve-entity-path current system-id) queue))))))))
    (values (nreverse catalogs) (nreverse tests))))

(defun profile-test-p (test)
  (and (member (xmlconf-test-recommendation test)
               *selected-recommendations* :test #'string=)
       (or (null (xmlconf-test-version test))
           (member "1.0" (split-whitespace (xmlconf-test-version test))
                   :test #'string=))
       (or (null (xmlconf-test-edition test))
           (member "5" (split-whitespace (xmlconf-test-edition test))
                   :test #'string=))
       (not (string= "no" (xmlconf-test-namespace test)))))

(defun profile-exclusion-reason (test)
  (cond
    ((not (member (xmlconf-test-recommendation test)
                  *selected-recommendations* :test #'string=))
     "RECOMMENDATION is outside XML 1.0 Fifth Edition + Namespaces 1.0")
    ((and (xmlconf-test-version test)
          (not (member "1.0" (split-whitespace (xmlconf-test-version test))
                       :test #'string=)))
     "VERSION is outside XML 1.0")
    ((and (xmlconf-test-edition test)
          (not (member "5" (split-whitespace (xmlconf-test-edition test))
                       :test #'string=)))
     "EDITION does not include 5")
    ((string= "no" (xmlconf-test-namespace test))
     "NAMESPACE=no is outside the Namespaces 1.0 profile")
    (t "not selected by the XMLConf profile")))

(defun analyze-xmlconf (directory &key (exclusions-file *default-exclusions-file*))
  (unless (existing-directory-p directory)
    (error "XML_KIT_XMLCONF_DIR is not an existing directory: ~A" directory))
  (let ((root (find-catalog-root directory)))
    (multiple-value-bind (catalogs tests) (collect-catalog root)
      (let ((exclusions (load-exclusions exclusions-file))
            (used-exclusions (make-hash-table :test #'equal))
            (results nil)
            (output-count 0))
        (dolist (test tests)
          (let ((category nil)
                (reason nil)
                (id (xmlconf-test-id test)))
            (cond
              ((string= "error" (xmlconf-test-type test))
               (setf category :excluded
                     reason "TYPE=error is excluded from pass/fail"))
              ((not (profile-test-p test))
               (setf category :excluded
                     reason (profile-exclusion-reason test)))
              ((and (string= "not-wf" (xmlconf-test-type test))
                    (gethash id exclusions))
               (setf category :skipped
                     reason (gethash id exclusions))
               (setf (gethash id used-exclusions) t))
              ((member (xmlconf-test-type test) '("valid" "invalid")
                       :test #'string=)
               (setf category :accepted
                     reason "non-validating processor is expected to accept this type"))
              ((string= "not-wf" (xmlconf-test-type test))
               (setf category :rejected
                     reason "non-validating processor is expected to reject not-wf"))
              (t
               (setf category :excluded
                     reason "unknown TEST TYPE")))
            (when (and (xmlconf-test-output test)
                       (member category '(:accepted :rejected)))
              (incf output-count))
            (push (make-xmlconf-case-result :id id
                                             :category category
                                             :reason reason
                                             :test test)
                  results)))
        (let ((unmatched nil))
          (maphash (lambda (id reason)
                     (declare (ignore reason))
                     (unless (gethash id used-exclusions)
                       (push id unmatched)))
                   exclusions)
          (make-xmlconf-analysis
           :catalog-root root
           :catalog-count (length catalogs)
           :test-count (length tests)
           :accepted (count :accepted results :key #'xmlconf-case-result-category)
           :rejected (count :rejected results :key #'xmlconf-case-result-category)
           :skipped (count :skipped results :key #'xmlconf-case-result-category)
           :excluded (count :excluded results :key #'xmlconf-case-result-category)
           :unmatched-exclusions (sort unmatched #'string<)
           :output-count output-count
           :results (nreverse results)))))))

(defun analysis-count (analysis category)
  (ecase category
    (:accepted (xmlconf-analysis-accepted analysis))
    (:rejected (xmlconf-analysis-rejected analysis))
    (:skipped (xmlconf-analysis-skipped analysis))
    (:excluded (xmlconf-analysis-excluded analysis))))

(defun print-analysis (analysis)
  (format t "XMLConf catalog analysis~%")
  (format t "  profile: XML 1.0 Fifth Edition + Namespaces 1.0~%")
  (format t "  catalog-root: ~A~%" (xmlconf-analysis-catalog-root analysis))
  (format t "  catalogs: ~D~%" (xmlconf-analysis-catalog-count analysis))
  (format t "  tests: ~D~%" (xmlconf-analysis-test-count analysis))
  (format t "  accepted: ~D~%" (analysis-count analysis :accepted))
  (format t "  rejected: ~D~%" (analysis-count analysis :rejected))
  (format t "  skipped: ~D~%" (analysis-count analysis :skipped))
  (format t "  excluded: ~D~%" (analysis-count analysis :excluded))
  (format t "  output-cases: ~D~%" (xmlconf-analysis-output-count analysis))
  (format t "  unmatched-exclusions: ~D~%"
          (length (xmlconf-analysis-unmatched-exclusions analysis)))
  analysis)

(defun read-text-file (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (with-output-to-string (text)
      (loop for character = (read-char stream nil nil)
            while character
            do (write-char character text)))))

(defun unsupported-encoding-bom-p (path)
  (with-open-file (stream path :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((octets (make-array 4 :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (let ((count (read-sequence octets stream)))
        (or (and (>= count 2)
                 (or (and (= (aref octets 0) #xFE)
                          (= (aref octets 1) #xFF))
                     (and (= (aref octets 0) #xFF)
                          (= (aref octets 1) #xFE))))
            (and (= count 4)
                 (or (and (= (aref octets 0) 0)
                          (= (aref octets 1) 0)
                          (= (aref octets 2) #xFE)
                          (= (aref octets 3) #xFF))
                     (and (= (aref octets 0) #xFF)
                          (= (aref octets 1) #xFE)
                          (= (aref octets 2) 0)
                          (= (aref octets 3) 0)))))))))

(defun xmlconf-external-resource-declaration-p (text)
  (labels ((resource-keyword-after-p (marker)
             (and marker
                  (or (search "SYSTEM" text :start2 marker :test #'char-equal)
                      (search "PUBLIC" text :start2 marker :test #'char-equal)))))
    (or (resource-keyword-after-p (search "<!DOCTYPE" text :test #'char-equal))
        (resource-keyword-after-p (search "<!ENTITY" text :test #'char-equal)))))

(defun xmlconf-profile-skip-reason (source)
  (cond
    ((unsupported-encoding-bom-p source)
     "input encoding is outside the UTF-8 profile")
    (t
     (handler-case
         (let ((text (read-text-file source)))
           (cond
             ((search "version=\"1.1\"" text :test #'char-equal)
              "XML 1.1 is outside the XML 1.0 profile")
             ((xmlconf-external-resource-declaration-p text)
               "external DTD or entity resolution is disabled by policy")
              ((or (search "<!ELEMENT" text :test #'char-equal)
                   (search "<!ATTLIST" text :test #'char-equal)
                   (search "<!NOTATION" text :test #'char-equal))
               "DTD validation declarations are outside the non-validating profile")))
       (error () nil)))))

(defun xmlconf-namespace-invalid-case-p (case)
  (let ((test (xmlconf-case-result-test case)))
    (and (string= "invalid" (xmlconf-test-type test))
         (string= "NS1.0" (xmlconf-test-recommendation test)))))

(defun xmlconf-test-path (test relative-path)
  (let* ((source-directory (directory-pathname (xmlconf-test-source test)))
         (base-directory (if (xmlconf-test-base test)
                             (directory-pathname
                              (merge-pathnames (xmlconf-test-base test)
                                               source-directory))
                             source-directory)))
    (probe-file (merge-pathnames relative-path base-directory))))

(defun load-parser-api ()
  (or (find-package "CL-XML-KIT")
      (progn
        (require :asdf)
        (asdf:initialize-source-registry
         `(:source-registry
           (:directory
            ,(make-pathname
              :directory (butlast (pathname-directory *load-truename*))
              :name nil
              :type nil
              :defaults *load-truename*))
           :inherit-configuration))
        (asdf:load-system "cl-xml-kit")
        (or (find-package "CL-XML-KIT")
            (error "ASDF loaded cl-xml-kit without creating its package")))))

(defun parser-function (name)
  (let ((symbol (find-symbol name (load-parser-api))))
    (unless (and symbol (fboundp symbol))
      (error "The cl-xml-kit API does not export ~A" name))
    (symbol-function symbol)))

(defun execute-xmlconf-case (case)
  (let* ((test (xmlconf-case-result-test case))
         (id (xmlconf-case-result-id case))
         (uri (xmlconf-test-uri test)))
    (cond
      ((null uri)
       (values :skipped id "TEST has no URI"))
      ((not (string= "none" (xmlconf-test-entities test)))
       (values :skipped id "external entity resolution is disabled by policy"))
      (t
       (let ((source (xmlconf-test-path test uri)))
         (unless source
           (return-from execute-xmlconf-case
             (values :failed id (format nil "input URI was not found: ~A" uri))))
         (let ((profile-reason (xmlconf-profile-skip-reason source)))
           (when profile-reason
             (return-from execute-xmlconf-case
               (values :skipped id profile-reason))))
         (handler-case
             (let ((document (funcall (parser-function "PARSE-XML")
                                      (read-text-file source)
                                      :source-name (namestring source))))
               (when (xmlconf-test-output test)
                 (let ((expected-path
                         (xmlconf-test-path test (xmlconf-test-output test))))
                   (unless expected-path
                     (return-from execute-xmlconf-case
                       (values :failed id "expected output file was not found")))
                   (unless (string=
                            (funcall (parser-function "XML->CANONICAL-STRING")
                                     document)
                            (funcall (parser-function "XML->CANONICAL-STRING")
                                     (funcall (parser-function "PARSE-XML")
                                              (read-text-file expected-path)
                                              :source-name (namestring expected-path))))
                     (return-from execute-xmlconf-case
                       (values :failed id "canonical output differs from XMLConf output")))))
               (if (eq (xmlconf-case-result-category case) :rejected)
                   (values :failed id "not-wf input was accepted")
                   (values :passed id "document parsed")))
           (error (condition)
             (if (or (eq (xmlconf-case-result-category case) :rejected)
                     (xmlconf-namespace-invalid-case-p case))
                 (values :passed id "document was rejected")
                 (values :failed id (princ-to-string condition))))))))))

(defun execute-xmlconf (analysis)
  (let ((passed 0)
        (failed 0)
        (skipped 0))
    (dolist (case (xmlconf-analysis-results analysis))
      (when (member (xmlconf-case-result-category case)
                    '(:accepted :rejected))
        (multiple-value-bind (status id reason)
            (execute-xmlconf-case case)
          (ecase status
            (:passed (incf passed))
            (:failed (incf failed))
            (:skipped (incf skipped)))
          (format t "XMLConf ~A ~A: ~A~%" status id reason))))
    (format t "XMLConf execution~%  passed: ~D~%  failed: ~D~%  skipped: ~D~%"
            passed failed skipped)
    (when (plusp failed)
      (error "XMLConf execution failed for ~D case~:P" failed))
    (values passed failed skipped)))

(defun run-xmlconf (directory &key (exclusions-file *default-exclusions-file*)
                                      (mode *xmlconf-execution-mode*))
  (ecase mode
    (:catalog
     (analyze-xmlconf directory :exclusions-file exclusions-file))
    (:execute
     (let ((analysis (analyze-xmlconf directory :exclusions-file exclusions-file)))
       (execute-xmlconf analysis)
       analysis))))

(defun command-line-argument (name arguments)
  (let ((position (position name arguments :test #'string=)))
    (when position
      (or (nth (1+ position) arguments)
          (error "Missing value after ~A" name)))))

(defun exclusions-path ()
  (or (getenv "XML_KIT_XMLCONF_EXCLUSIONS")
      *default-exclusions-file*))

(defun command-line-main (arguments)
  (when (or (member "--help" arguments :test #'string=)
            (member "-h" arguments :test #'string=))
    (format t "Usage: sbcl --script scripts/xmlconf-runner.lisp [--analyze|--run] [--catalog-dir DIR]~%")
    (format t "Reads XML_KIT_XMLCONF_DIR when --catalog-dir is omitted.~%")
    (return-from command-line-main 0))
  (let ((directory (or (command-line-argument "--catalog-dir" arguments)
                       (getenv "XML_KIT_XMLCONF_DIR"))))
    (unless directory
      (error "XML_KIT_XMLCONF_DIR is not set; pass --catalog-dir DIR or an extracted XMLConf directory"))
    (cond
      ((member "--run" arguments :test #'string=)
       (let ((*xmlconf-execution-mode* :execute))
         (run-xmlconf directory :exclusions-file (exclusions-path))))
      (t
       (print-analysis (run-xmlconf
                        directory
                        :exclusions-file (exclusions-path))))))
  0)

(defun script-entry-point-p ()
  ;; A loaded runner shares SBCL's argv with its caller, so argv alone cannot
  ;; distinguish `--script` from LOAD. The parser package is already present
  ;; when coverage loads this file and absent for the standalone runner.
  #+sbcl (null (find-package "CL-XML-KIT"))
  #-sbcl nil)

(when (script-entry-point-p)
  (handler-case
      (sb-ext:exit :code (command-line-main
                          #+sbcl (rest sb-ext:*posix-argv*)
                          #-sbcl nil))
    (error (condition)
      (format *error-output* "xmlconf-runner: ~A~%" condition)
      #+sbcl (sb-ext:exit :code 1))))

