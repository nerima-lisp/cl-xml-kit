;;;; XMLConf catalog reading, exclusions, and profile classification.

(defpackage #:cl-xml-kit/xmlconf
  (:use #:cl)
  (:export #:*runner-usage*
           #:analyze-xmlconf
           #:command-line-main
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

(defparameter *default-exclusions-file*
  (cl-xml-kit/script-support:script-path "xmlconf-exclusions.sexp"))

(defstruct xmlconf-test
  id type entities recommendation version edition namespace output uri source base)

(defstruct xmlconf-case-result
  id category reason test)

(defstruct xmlconf-analysis
  catalog-root catalog-count test-count accepted rejected skipped excluded
  unmatched-exclusions output-count results)

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

(defun special-markup-end (text less source)
  "Return the position after the comment, CDATA section, processing
instruction, or declaration starting at LESS, or NIL when LESS starts a tag."
  (flet ((starts-with-p (prefix)
           (let ((end (+ less (length prefix))))
             (and (<= end (length text))
                  (string= prefix text :start2 less :end2 end))))
         (after (terminator start what)
           (let ((end (search terminator text :start2 start)))
             (unless end (error "Unterminated ~A in ~A" what source))
             (+ end (length terminator)))))
    (cond
      ((starts-with-p "<!--") (after "-->" (+ less 4) "XML comment"))
      ((starts-with-p "<![CDATA[") (after "]]>" (+ less 9) "CDATA section"))
      ((starts-with-p "<?") (after "?>" (+ less 2) "processing instruction"))
      ((starts-with-p "<!") (1+ (markup-end text less))))))

(defun scan-catalog-tags (text source callback)
  "Call CALLBACK with the attributes, SOURCE, and xml:base of each TEST element."
  (let ((position 0)
        (stack nil)
        (base nil))
    (loop for less = (position #\< text :start position)
          while less do
      (setf position
            (or (special-markup-end text less source)
                (let ((end (markup-end text less)))
                  (if (char= (char text (1+ less)) #\/)
                      (when stack
                        (pop stack)
                        (setf base (cdr (first stack))))
                      (let* ((name-end (token-end text (1+ less)))
                             (name (subseq text (1+ less) name-end))
                             (attributes (parse-attributes text name-end end)))
                        (when (string-equal name "TEST")
                          (funcall callback attributes source base))
                        (unless (char= (char text (1- end)) #\/)
                          (when (string-equal name "TESTCASES")
                            (setf base (or (string-attribute attributes "xml:base")
                                           base)))
                          (push (cons name base) stack))))
                  (1+ end)))))))

(defun read-text-file (path)
  (host-kit:read-file-string path :external-format :utf-8))

(defun directory-pathname (path)
  (let ((pathname (pathname path)))
    (make-pathname :directory (pathname-directory pathname)
                   :name nil
                   :type nil
                   :defaults pathname)))

(defun existing-directory-p (path)
  (not (null (host-kit:directory-exists-p path))))

(defun find-catalog-root (directory)
  (let* ((dir (directory-pathname directory))
         (direct (merge-pathnames "xmlconf.xml" dir))
         (nested (merge-pathnames "xmlconf/xmlconf.xml" dir)))
    (or (host-kit:file-exists-p direct)
        (host-kit:file-exists-p nested)
        (error "XMLConf catalog root xmlconf.xml was not found below ~A" dir))))

(defun resolve-entity-path (source system-id)
  (let ((path (merge-pathnames system-id (directory-pathname source))))
    (or (host-kit:file-exists-p path)
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
          (let* ((text (read-text-file current))
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

(defun profile-exclusion-reason (test)
  "Return why TEST is outside the XMLConf profile, or NIL when it is selected."
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
     "NAMESPACE=no is outside the Namespaces 1.0 profile")))

(defun classify-test (test exclusions)
  (multiple-value-bind (category reason)
      (let ((type (xmlconf-test-type test))
            (profile-reason (profile-exclusion-reason test))
            (exclusion (gethash (xmlconf-test-id test) exclusions)))
        (cond
          ((string= "error" type)
           (values :excluded "TYPE=error is excluded from pass/fail"))
          (profile-reason
           (values :excluded profile-reason))
          ((and (string= "not-wf" type) exclusion)
           (values :skipped exclusion))
          ((member type '("valid" "invalid") :test #'string=)
           (values :accepted
                   "non-validating processor is expected to accept this type"))
          ((string= "not-wf" type)
           (values :rejected
                   "non-validating processor is expected to reject not-wf"))
          (t
           (values :excluded "unknown TEST TYPE"))))
    (make-xmlconf-case-result :id (xmlconf-test-id test)
                              :category category
                              :reason reason
                              :test test)))

(defun unmatched-exclusions (exclusions results)
  "Return the sorted exclusion IDs that skipped no selected not-wf case."
  (let ((unmatched nil))
    (maphash (lambda (id reason)
               (declare (ignore reason))
               (unless (find-if (lambda (result)
                                  (and (eq :skipped (xmlconf-case-result-category result))
                                       (string= id (xmlconf-case-result-id result))))
                                results)
                 (push id unmatched)))
             exclusions)
    (sort unmatched #'string<)))

(defun executed-output-case-p (result)
  (and (xmlconf-test-output (xmlconf-case-result-test result))
       (member (xmlconf-case-result-category result) '(:accepted :rejected))))

(defun analyze-xmlconf (directory &key (exclusions-file *default-exclusions-file*))
  (unless (existing-directory-p directory)
    (error "XML_KIT_XMLCONF_DIR is not an existing directory: ~A" directory))
  (let ((root (find-catalog-root directory)))
    (multiple-value-bind (catalogs tests) (collect-catalog root)
      (let* ((exclusions (load-exclusions exclusions-file))
             (results (mapcar (lambda (test) (classify-test test exclusions))
                              tests)))
        (flet ((category-count (category)
                 (count category results :key #'xmlconf-case-result-category)))
          (make-xmlconf-analysis
           :catalog-root root
           :catalog-count (length catalogs)
           :test-count (length tests)
           :accepted (category-count :accepted)
           :rejected (category-count :rejected)
           :skipped (category-count :skipped)
           :excluded (category-count :excluded)
           :unmatched-exclusions (unmatched-exclusions exclusions results)
           :output-count (count-if #'executed-output-case-p results)
           :results results))))))

