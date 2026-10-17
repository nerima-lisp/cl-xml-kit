(in-package #:cl-xml-kit)

(defun next-xml-token (tokenizer)
  "Return the next XML token, then NIL after the one-shot EOF token.

The token kind/value/position meanings are fixed by `+XML-TOKEN-KINDS+`.
Malformed XML signals XML-WELL-FORMEDNESS-ERROR with the current source
position; configured name and text-node bounds signal
XML-RESOURCE-LIMIT-EXCEEDED. Every successful branch consumes input or
returns EOF, so malformed input cannot leave a zero-progress loop."
  (check-argument tokenizer xml-tokenizer-p xml-input-error :tokenizer-required)
  (let ((state (%xml-tokenizer-state tokenizer)))
    (cond
      ((%xml-tokenizer-state-eof-returned state) nil)
      ((= (%xml-tokenizer-state-index state) (length (%xml-tokenizer-input tokenizer)))
       (unless (eq (%xml-tokenizer-state-mode state) :content)
         (%xml-tokenizer-fail tokenizer (xml-message :unexpected-end-of-input)
                              :constraint :well-formedness))
       (setf (%xml-tokenizer-state-eof-returned state) t)
       (%xml-tokenizer-token tokenizer :eof nil (%xml-tokenizer-current-position tokenizer)))
      (t
       (ecase (%xml-tokenizer-state-mode state)
         (:content (%xml-tokenizer-next-content tokenizer))
         (:start-tag (%xml-tokenizer-name-token tokenizer :start-after-element-name))
         (:start-after-element-name (%xml-tokenizer-next-attribute-or-close tokenizer nil))
         (:start-after-attribute-name (%xml-tokenizer-next-equals tokenizer))
         (:start-after-equals (%xml-tokenizer-next-quoted-value tokenizer))
         (:start-after-quote (%xml-tokenizer-next-attribute-or-close tokenizer t))
         (:end-tag (%xml-tokenizer-name-token tokenizer :end-after-name))
         (:end-after-name (%xml-tokenizer-next-end-tag-close tokenizer)))))))
