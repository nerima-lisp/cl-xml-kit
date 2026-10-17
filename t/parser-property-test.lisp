(in-package #:cl-xml-kit/test)

;;; Fuzzing: markup fragments around a root, so most inputs pass the prolog and
;;; reach the tag, attribute, reference, CDATA, and DOCTYPE paths.

(defun markup-fuzz-generator ()
  "Return a generator of strings made of XML markup fragments inside a likely root."
  (let ((fragment
          (gen-one-of
           (gen-member '("<" ">" "</" "/>" "<a" "</a>" "<a/>" "<!--" "-->" "<![CDATA["
                         "]]>" "<!DOCTYPE r" "<!ENTITY e 'v'>" "[" "]" "<?" "?>" "<?p d"
                         "&" ";" "&#" "&#x" "&e;" "&amp;" "&lt;" "\"" "'" "=" ":"
                         " " "xmlns:p=\"urn:p\"" "p:" " a='1'" "&#65;" "&#x41;"))
           ;; Name, digit, and delimiter characters, so fragments join into names,
           ;; numeric references, and attribute syntax as often as they break them.
           (gen-string :min-length 1 :max-length 3 :alphabet "ax1:<>/!?-[]&;#\"'= "))))
    (gen-map (lambda (parts) (apply #'concatenate 'string parts))
             (gen-tuple (gen-member '("" "<r>" "<?xml version='1.0'?><r>"
                                      "<!DOCTYPE r [<!ENTITY e 'v'>]><r>"))
                        (gen-map (lambda (pieces) (apply #'concatenate 'string pieces))
                                 (gen-list fragment :max-length 10))
                        (gen-member '("" "</r>"))))))

(it-fuzz "either returns a document or signals an XML-KIT-ERROR for markup-like input"
    ((input (markup-fuzz-generator)))
    (:trials 300)
  (let ((outcome (handler-case (parse-xml input)
                   (xml-kit-error () :rejected))))
    (expect (or (eq outcome :rejected) (xml-document-p outcome)) :to-be-truthy)))

;;; Round trip through an independent writer: a generated tree model is written
;;; as XML text by the functions below, parsed, and read back into the model.

(defun tree-model-generator ()
  "Return a generator of element models (:ELEMENT NAME ATTRIBUTES CHILDREN).
Attribute names are unique per element; comment text has no hyphen, so it can
never form -- or end in -."
  (let* ((name (gen-member '("a" "b" "item" "x1" "_y" "z-z" "w.v" "é")))
         (attributes (gen-map (lambda (pairs)
                                (remove-duplicates pairs :key #'first :test #'string=
                                                         :from-end t))
                              (gen-list (gen-tuple name (gen-string :max-length 4
                                                                    :alphabet "ab <>&\"'é"))
                                        :max-length 3)))
         (text (gen-map (lambda (data) (list :text data))
                        (gen-string :min-length 1 :max-length 5 :alphabet "ab <>&]é'\"")))
         (comment (gen-map (lambda (data) (list :comment data))
                           (gen-string :max-length 4 :alphabet "ab c<&")))
         (instruction (gen-map (lambda (pair) (cons :pi pair))
                               (gen-tuple (gen-member '("p" "t1" "x-y"))
                                          (gen-member '(nil "d" "d e" "<&>"))))))
    (gen-recursive
     (gen-map (lambda (pair) (list :element (first pair) (second pair) nil))
              (gen-tuple name attributes))
     (lambda (self)
       (gen-map (lambda (parts) (cons :element parts))
                (gen-tuple name attributes
                           (gen-list (gen-one-of self text comment instruction)
                                     :max-length 3))))
     :max-depth 3)))

(defun write-escaped (string stream replacements)
  (loop for character across string
        for replacement = (cdr (assoc character replacements))
        do (if replacement
               (write-string replacement stream)
               (write-char character stream))))

(defun write-model (node stream)
  "Write the XML text of the model NODE to STREAM without using CL-XML-KIT."
  (ecase (first node)
    (:element
     (destructuring-bind (name attributes children) (rest node)
       (format stream "<~A" name)
       (loop for (attribute value) in attributes
             do (format stream " ~A=\"" attribute)
                (write-escaped value stream '((#\& . "&amp;") (#\< . "&lt;") (#\" . "&quot;")))
                (write-char #\" stream))
       (if children
           (progn (write-char #\> stream)
                  (dolist (child children) (write-model child stream))
                  (format stream "</~A>" name))
           (write-string "/>" stream))))
    (:text (write-escaped (second node) stream
                          '((#\& . "&amp;") (#\< . "&lt;") (#\> . "&gt;") (#\é . "&#xE9;"))))
    (:comment (format stream "<!--~A-->" (second node)))
    (:pi (format stream "<?~A~@[ ~A~]?>" (second node) (third node)))))

(defun normalize-model (node)
  "Return NODE with adjacent text children merged, as a parser coalesces them."
  (if (eq (first node) :element)
      (destructuring-bind (name attributes children) (rest node)
        (let ((merged nil))
          (dolist (child (mapcar #'normalize-model children))
            (if (and (eq (first child) :text) (eq (first (first merged)) :text))
                (setf (first merged)
                      (list :text (concatenate 'string (second (first merged)) (second child))))
                (push child merged)))
          (list :element name attributes (nreverse merged))))
      node))

(defun dom-model (node)
  "Return the model of the parsed NODE, in the shape TREE-MODEL-GENERATOR makes."
  (etypecase node
    (xml-element
     (list :element (xml-name-local-name (xml-element-name node))
           (mapcar (lambda (attribute)
                     (list (xml-name-local-name (xml-attribute-name attribute))
                           (xml-attribute-value attribute)))
                   (xml-element-attributes node))
           (mapcar #'dom-model (xml-element-children node))))
    (xml-text (list :text (xml-text-data node)))
    (xml-comment (list :comment (xml-comment-data node)))
    (xml-processing-instruction
     (list :pi (xml-processing-instruction-target node)
           (xml-processing-instruction-data node)))))

(defun model-events (node)
  "Return the event summaries a parser must produce for the normalized model NODE."
  (ecase (first node)
    (:element (append (list (list :start-element (second node)))
                      (mapcan #'model-events (fourth node))
                      (list (list :end-element (second node)))))
    (:text (list (list :characters (second node))))
    (:comment (list (list :comment (second node))))
    (:pi (list (list :processing-instruction (second node) (third node))))))

(defun event-content (event)
  "Return the type of EVENT with its name, text, or target and data."
  (let ((type (xml-event-type event)))
    (case type
      ((:start-element :end-element)
       (list type (xml-name-local-name (xml-event-name event))))
      ((:characters :comment) (list type (xml-event-text event)))
      (:processing-instruction (list type (xml-event-target event) (xml-event-text event)))
      (otherwise (list type)))))

(defun merge-characters (summaries)
  "Merge adjacent :CHARACTERS summaries, since one text node may arrive in several events."
  (let ((merged nil))
    (dolist (summary summaries (nreverse merged))
      (if (and (eq (first summary) :characters) (eq (first (first merged)) :characters))
          (setf (first merged)
                (list :characters (concatenate 'string (second (first merged)) (second summary))))
          (push summary merged)))))

(it-property "reads a written tree back identically through PARSE-XML, pull events, and mapped events"
    ((model (tree-model-generator)))
  (let* ((input (with-output-to-string (stream) (write-model model stream)))
         (expected (normalize-model model))
         (pulled (mapcar #'event-content (pull-events input))))
    (expect (dom-model (document-element input)) :to-equal expected)
    (expect (merge-characters pulled)
            :to-equal (append '((:start-document)) (model-events expected) '((:end-document))))
    (expect (mapcar #'event-content (mapped-events input)) :to-equal pulled)))
