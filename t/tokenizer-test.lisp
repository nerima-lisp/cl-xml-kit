(in-package #:cl-xml-kit/test)

(defun tokenizer-tokens (input &key source limits)
  "Collect the token contract, including the one-shot EOF token."
  (let ((tokenizer (cl-xml-kit::make-xml-tokenizer input
                                                   :source source
                                                   :limits (or limits
                                                               cl-xml-kit:*default-xml-resource-limits*)))
        (tokens nil))
    (loop repeat 256
          for token = (cl-xml-kit::next-xml-token tokenizer)
          while token
          do (push (list (cl-xml-kit::xml-token-kind token)
                         (cl-xml-kit::xml-token-value token)
                         (cl-xml-kit:xml-position-offset
                          (cl-xml-kit::xml-token-position token))
                         (cl-xml-kit:xml-position-line
                          (cl-xml-kit::xml-token-position token))
                         (cl-xml-kit:xml-position-column
                          (cl-xml-kit::xml-token-position token)))
                   tokens))
    (nreverse tokens)))

(defun tokenizer-condition (input)
  "Return the well-formedness condition signaled while tokenizing INPUT."
  (let ((condition nil)
        (tokenizer (cl-xml-kit::make-xml-tokenizer input)))
    (handler-case
        (loop repeat 256
              for token = (cl-xml-kit::next-xml-token tokenizer)
              while token)
      (cl-xml-kit:xml-well-formedness-error (caught)
        (setf condition caught)))
    condition))

(defun tokenizer-limit-kind (input limits)
  "Return the resource-limit kind signaled while tokenizing INPUT."
  (let ((condition nil)
        (tokenizer (cl-xml-kit::make-xml-tokenizer input :limits limits)))
    (handler-case
        (loop repeat 256
              for token = (cl-xml-kit::next-xml-token tokenizer)
              while token)
      (cl-xml-kit:xml-resource-limit-exceeded (caught)
        (setf condition caught)))
    (and condition
         (cl-xml-kit:xml-resource-limit-exceeded-kind condition))))

(it "tokenizes XML constructs with delimiter-free values and positions"
  (let ((tokens (tokenizer-tokens
                 "text&amp;<![CDATA[c<d]]><!--ok--><?pi data?><!DOCTYPE root [<!ELEMENT root ANY>]><root a=\"v&amp;x\"/>tail</root>"
                 :source "tokenizer-test")))
    (expect (mapcar #'first tokens)
            :to-equal '(:text :entity-reference :cdata :comment
                        :processing-instruction :doctype :start-tag-open :name
                        :name :equals :quoted-value :empty-element-close :text
                        :end-tag-open :name :tag-close :eof))
    (expect (mapcar #'second tokens)
            :to-equal '("text" "amp" "c<d" "ok" "pi data"
                        " root [<!ELEMENT root ANY>]" nil "root" "a" nil
                        "v&amp;x" nil "tail" nil "root" nil nil))
    (expect (mapcar #'third tokens)
            :to-equal '(0 4 9 24 33 44 81 82 87 88 89 98 100 104 106 110 111))
    (expect (cl-xml-kit::next-xml-token
             (let ((tokenizer (cl-xml-kit::make-xml-tokenizer "")))
               (cl-xml-kit::next-xml-token tokenizer)
               tokenizer))
            :to-equal nil)))

(it "tracks line and column at tag tokens"
  (let ((tokens (tokenizer-tokens (format nil "<a~% b='x'/>") )))
    (expect (mapcar (lambda (token) (list (first token) (third token)
                                           (fourth token) (fifth token)))
                    (subseq tokens 0 7))
            :to-equal '((:start-tag-open 0 1 1)
                        (:name 1 1 2)
                        (:name 4 2 2)
                        (:equals 5 2 3)
                        (:quoted-value 6 2 4)
                        (:empty-element-close 9 2 7)
                        (:eof 11 2 9)))))

(it "signals well-formedness errors for incomplete lexical constructs"
  (dolist (input '("<!--" "<![CDATA[x" "<?pi" "<!DOCTYPE root ["
                   "<a x=\"unterminated" "&" "<a x=y>" "</a/>"
                   "< a>" "</ a>" "<a x=\"1\"y=\"2\">") )
    (let ((condition nil))
      (handler-case
          (let ((tokenizer (cl-xml-kit::make-xml-tokenizer input)))
            (loop for token = (cl-xml-kit::next-xml-token tokenizer)
                while token))
        (cl-xml-kit:xml-well-formedness-error (caught)
          (setf condition caught)))
      (expect (typep condition 'cl-xml-kit:xml-well-formedness-error)
              :to-equal t))))

(it "applies name and text-node resource limits"
  (let ((name-condition nil)
        (text-condition nil)
        (name-limits (cl-xml-kit:make-xml-resource-limits :max-name-length 3))
        (text-limits (cl-xml-kit:make-xml-resource-limits :max-text-node-length 2)))
    (handler-case
        (let ((tokenizer (cl-xml-kit::make-xml-tokenizer "<long>"
                                                          :limits name-limits)))
          (loop for token = (cl-xml-kit::next-xml-token tokenizer)
                while token))
      (cl-xml-kit:xml-resource-limit-exceeded (caught)
        (setf name-condition caught)))
    (handler-case
        (let ((tokenizer (cl-xml-kit::make-xml-tokenizer "abc"
                                                          :limits text-limits)))
          (loop for token = (cl-xml-kit::next-xml-token tokenizer)
                while token))
      (cl-xml-kit:xml-resource-limit-exceeded (caught)
        (setf text-condition caught)))
    (expect (list (cl-xml-kit:xml-resource-limit-exceeded-kind name-condition)
                  (cl-xml-kit:xml-resource-limit-exceeded-limit name-condition)
                  (cl-xml-kit:xml-resource-limit-exceeded-kind text-condition)
                  (cl-xml-kit:xml-resource-limit-exceeded-limit text-condition))
            :to-equal '(:max-name-length 3 :max-text-node-length 2))))

(it "accepts exact name and text-node resource limits"
  (expect (mapcar #'first
                  (tokenizer-tokens
                   "<long>"
                   :limits (cl-xml-kit:make-xml-resource-limits
                            :max-name-length 4)))
          :to-equal '(:start-tag-open :name :tag-close :eof))
  (expect (mapcar #'first
                  (tokenizer-tokens
                   "abc"
                   :limits (cl-xml-kit:make-xml-resource-limits
                            :max-text-node-length 3)))
          :to-equal '(:text :eof)))

(it "tokenizes decimal hexadecimal and named references"
  (let ((tokens (tokenizer-tokens "&#65;&#x42;&name;")))
    (expect (mapcar #'first tokens)
            :to-equal '(:entity-reference :entity-reference :entity-reference :eof))
    (expect (mapcar #'second tokens)
            :to-equal '("#65" "#x42" "name" nil))))

(it "retains quoted doctype subset text including quoted delimiters"
  (let* ((input "<!DOCTYPE root [<!ENTITY gt '>'><!ELEMENT root (#PCDATA)>]>")
         (tokens (tokenizer-tokens input)))
    (expect (mapcar #'first tokens) :to-equal '(:doctype :eof))
    (expect (second (first tokens))
            :to-equal " root [<!ENTITY gt '>'><!ELEMENT root (#PCDATA)>]")))

(it "reports source and normalized line-column positions"
  (let* ((tokenizer (cl-xml-kit::make-xml-tokenizer
                     (format nil "x~%<root>")
                     :source "positions.xml"))
         (text (cl-xml-kit::next-xml-token tokenizer))
         (tag (progn
                (cl-xml-kit::next-xml-token tokenizer)
                (cl-xml-kit::next-xml-token tokenizer))))
    (expect (list (cl-xml-kit:xml-position-source
                   (cl-xml-kit::xml-token-position tag))
                  (cl-xml-kit:xml-position-offset
                   (cl-xml-kit::xml-token-position tag))
                  (cl-xml-kit:xml-position-line
                   (cl-xml-kit::xml-token-position tag))
                  (cl-xml-kit:xml-position-column
                   (cl-xml-kit::xml-token-position tag))
                  (cl-xml-kit::xml-token-value text))
            :to-equal (list "positions.xml" 3 2 2 (format nil "x~%")))))

(it "rejects invalid comment cdata and character-data terminators"
  (dolist (input '("<!--a--b-->" "<!--a--->" "<![CDATA[a]]>b]]>" "abc]]>"))
    (expect (typep (tokenizer-condition input)
                   'cl-xml-kit:xml-well-formedness-error)
            :to-equal t)))

(it "rejects malformed references names and markup separators"
  (dolist (input '("&#;" "&#0;" "&#x110000;" "&bad name;"
                   "<a!>" "<a 1b>" "<a b@c>" "<?pi=?>"))
    (expect (typep (tokenizer-condition input)
                   'cl-xml-kit:xml-well-formedness-error)
            :to-equal t)))

(it "rejects unmatched doctype brackets and unterminated constructs"
  (dolist (input '("<!DOCTYPE root ]>" "<!DOCTYPE root ["
                   "<!--" "<![CDATA[x" "<?pi" "<a x=\"unterminated"))
    (expect (typep (tokenizer-condition input)
                   'cl-xml-kit:xml-well-formedness-error)
            :to-equal t)))

(it "returns the expected limit kind only after the inclusive boundary"
  (expect (tokenizer-limit-kind
           "<long>"
           (cl-xml-kit:make-xml-resource-limits :max-name-length 4))
          :to-equal nil)
  (expect (tokenizer-limit-kind
           "<long>"
           (cl-xml-kit:make-xml-resource-limits :max-name-length 3))
          :to-equal :max-name-length)
  (expect (tokenizer-limit-kind
           "abc"
           (cl-xml-kit:make-xml-resource-limits :max-text-node-length 3))
          :to-equal nil)
  (expect (tokenizer-limit-kind
           "abcd"
           (cl-xml-kit:make-xml-resource-limits :max-text-node-length 3))
          :to-equal :max-text-node-length))

