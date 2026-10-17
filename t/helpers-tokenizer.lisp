(in-package #:cl-xml-kit/test)

(defconstant +tokenizer-token-bound+ 512
  "The most tokens DRAIN-TOKENS accepts from one input, EOF included.")

(defun xml-text-of (parts)
  "Return the string made of PARTS, where a string stands for itself and an
integer for the character with that code. Signals an error for a code the
implementation cannot represent, so a row never silently loses a character."
  (with-output-to-string (output)
    (dolist (part parts)
      (if (stringp part)
          (write-string part output)
          (write-char (or (code-char part)
                          (error "Unicode code point #X~X is unavailable." part))
                      output)))))

(defun drain-tokens (input &key source (limits *default-xml-resource-limits*))
  "Tokenize INPUT to the end and return its tokens, EOF included.
Signals whatever the tokenizer signals, and an error when the tokenizer
returns more than +TOKENIZER-TOKEN-BOUND+ tokens, so a tokenizer that stops
making progress fails the test instead of hanging it or being truncated."
  (let ((tokenizer (cl-xml-kit::make-xml-tokenizer input :source source
                                                         :limits limits)))
    (loop for count from 1
          for token = (cl-xml-kit::next-xml-token tokenizer)
          while token
          when (> count +tokenizer-token-bound+)
            do (error "Tokenizer returned more than ~D tokens for ~S."
                      +tokenizer-token-bound+ input)
          collect token)))

(defun token-summary (token)
  "Return (KIND VALUE OFFSET LINE COLUMN) for TOKEN."
  (let ((position (cl-xml-kit::xml-token-position token)))
    (list (cl-xml-kit::xml-token-kind token)
          (cl-xml-kit::xml-token-value token)
          (xml-position-offset position)
          (xml-position-line position)
          (xml-position-column position))))

(defun token-lexemes (input)
  "Return the substring of INPUT that each token spans, from its start offset to its end."
  (mapcar (lambda (token)
            (subseq input
                    (xml-position-offset (cl-xml-kit::xml-token-position token))
                    (cl-xml-kit::xml-token-end token)))
          (drain-tokens input)))

(defmatcher :to-tokenize-as (input expected)
  (:description "Passes when tokenizing the string INPUT with the default limits "
                "yields exactly the (KIND VALUE) pairs in EXPECTED, EOF included. "
                "A tokenizer condition propagates and fails the test.")
  (let ((actual (mapcar (lambda (token) (subseq (token-summary token) 0 2))
                        (drain-tokens input))))
    (values (equal actual (first expected)) actual (first expected))))
