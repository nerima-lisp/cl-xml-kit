(in-package #:cl-xml-kit/test)

(it "returns the EOF token once and NIL on every later call"
  (let ((tokenizer (cl-xml-kit::make-xml-tokenizer "")))
    (expect (list (cl-xml-kit::xml-token-kind (cl-xml-kit::next-xml-token tokenizer))
                  (cl-xml-kit::next-xml-token tokenizer)
                  (cl-xml-kit::next-xml-token tokenizer))
            :to-equal '(:eof nil nil))))

(it "places the EOF token at the end of the input"
  (expect (token-summary (car (last (drain-tokens "<a/>t"))))
          :to-equal '(:eof nil 5 1 6)))

(it "refuses to advance something that is not a tokenizer"
  (expect (lambda () (cl-xml-kit::next-xml-token "<a/>"))
          :to-signal-xml '(xml-input-error
                           (xml-condition-message "TOKENIZER must be an XML-TOKENIZER."))))

;; Each fragment is lexically complete on its own, so any concatenation
;; tokenizes; the whitespace fragments land inside text and between tags, and
;; no fragment ends in "]]" so that no concatenation forms "]]>" in text.
(defparameter *tokenizer-property-fragments*
  (list "<a>" "</a>" "<b x='1' y=\"2\"/>" "<c  z = 'v' >" "text" " " (string #\Newline)
        "&amp;" "&#x41;" "<!--c-->" "<?p d?>" "<![CDATA[<x>]]>"
        "<!DOCTYPE a [<!ENTITY e 'v'>]>"))

(it-property "covers every non-whitespace character of concatenated constructs with ordered tokens"
    ((fragments (gen-list (gen-member *tokenizer-property-fragments*) :max-length 10)))
  (let* ((input (apply #'concatenate 'string fragments))
         (tokens (drain-tokens input))
         (spans (mapcar (lambda (token)
                          (cons (xml-position-offset (cl-xml-kit::xml-token-position token))
                                (cl-xml-kit::xml-token-end token)))
                        tokens)))
    (expect (loop for (start . end) in spans
                  for previous-end = 0 then last-end
                  for last-end = end
                  always (<= previous-end start end))
            :to-be t)
    (expect (loop for index below (length input)
                  unless (or (cl-xml-kit::xml-space-char-p (char input index))
                             (find-if (lambda (span) (<= (car span) index (1- (cdr span))))
                                      spans))
                    collect index)
            :to-equal nil)
    (expect (first (token-summary (car (last tokens)))) :to-be :eof)))

;; The alphabet has no "<", "&", or "]", so every string over it is one
;; character-data run; the newline checks that line ends stay inside it.
(it-property "returns markup-free text as a single text token"
    ((text (gen-string :min-length 1 :max-length 24
                       :alphabet (coerce '(#\a #\b #\Space #\; #\# #\x #\> #\Newline)
                                         'string))))
  (expect text :to-tokenize-as `((:text ,text) (:eof nil))))
