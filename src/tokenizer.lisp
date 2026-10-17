(in-package #:cl-xml-kit)

(defun next-xml-token (tokenizer)
  "Return the next XML token, then NIL after the one-shot EOF token.

The token kind/value/position meanings are fixed by `+XML-TOKEN-KINDS+`.
Malformed XML signals XML-WELL-FORMEDNESS-ERROR with the current source
position; configured name and text-node bounds signal
XML-RESOURCE-LIMIT-EXCEEDED. Every successful branch consumes input or
returns EOF, so malformed input cannot leave a zero-progress loop."
  (check-argument tokenizer xml-tokenizer-p xml-input-error :tokenizer-required)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer)))
    (when (%xml-tokenizer-state-eof-returned state)
      (return-from next-xml-token nil))
    (loop
      (when (= (%xml-tokenizer-state-index state) (length input))
        (unless (eq (%xml-tokenizer-state-mode state) :content)
          (%xml-tokenizer-fail tokenizer (xml-message :unexpected-end-of-input)
                               :constraint :well-formedness))
        (setf (%xml-tokenizer-state-eof-returned state) t)
        (return (%xml-tokenizer-token tokenizer :eof nil
                                      (%xml-tokenizer-current-position tokenizer))))
      (return
        (case (%xml-tokenizer-state-mode state)
          (:content (%xml-tokenizer-next-content tokenizer))
          (:start-tag (%xml-tokenizer-next-start-tag tokenizer))
          (:start-after-element-name
           (%xml-tokenizer-next-start-after-element-name tokenizer))
          (:start-after-attribute-name
           (%xml-tokenizer-next-start-after-attribute-name tokenizer))
          (:start-after-equals (%xml-tokenizer-next-start-after-equals tokenizer))
          (:start-after-quote (%xml-tokenizer-next-start-after-quote tokenizer))
          (:end-tag (%xml-tokenizer-next-end-tag tokenizer))
          (:end-after-name (%xml-tokenizer-next-end-after-name tokenizer))
          (otherwise
           (%xml-tokenizer-fail tokenizer (xml-message :unknown-tokenizer-state)
                                :constraint :tokenizer-state)))))))

