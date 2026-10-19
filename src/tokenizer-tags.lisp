;;;; src/tokenizer-tags.lisp

(in-package #:cl-xml-kit)

(defun %xml-tokenizer-start-tag-close (tokenizer kind literal)
  (let ((position (%xml-tokenizer-state-index (%xml-tokenizer-state tokenizer)))
        (state (%xml-tokenizer-state tokenizer)))
    (%xml-tokenizer-consume-literal tokenizer literal)
    (setf (%xml-tokenizer-state-tag-open-p state) nil
          (%xml-tokenizer-state-space-seen-p state) nil)
    (%xml-tokenizer-token tokenizer kind nil position)))

(defun %xml-tokenizer-name-token (tokenizer)
  (multiple-value-bind (value position) (%xml-tokenizer-scan-name tokenizer)
    (%xml-tokenizer-token tokenizer :name value position)))

(defun %xml-tokenizer-next-attribute-name (tokenizer space-required-p)
  (let* ((state (%xml-tokenizer-state tokenizer))
         (character (%xml-tokenizer-peek tokenizer)))
    (unless (and character (xml-name-start-char-p character))
      (%xml-tokenizer-fail tokenizer
                           (if space-required-p
                               (xml-message :expected-another-attribute-or-close)
                               (xml-message :expected-attribute-or-close))
                           :constraint :start-tag))
    (when (and space-required-p
               (not (%xml-tokenizer-state-space-seen-p state)))
      (%xml-tokenizer-fail tokenizer (xml-message :attributes-need-whitespace)
                           :constraint :start-tag))
    (setf (%xml-tokenizer-state-space-seen-p state) nil)
    (%xml-tokenizer-name-token tokenizer)))

(defun %xml-tokenizer-next-equals (tokenizer)
  (let ((character (%xml-tokenizer-peek tokenizer)))
    (unless (and character (char= character #\=))
      (%xml-tokenizer-fail tokenizer (xml-message :expected-equals-after-attribute)
                           :constraint :attribute))
    (let ((position (%xml-tokenizer-state-index (%xml-tokenizer-state tokenizer))))
      (%xml-tokenizer-advance tokenizer)
      (%xml-tokenizer-token tokenizer :equals nil position))))

(defun %xml-tokenizer-next-quoted-value (tokenizer)
  (let ((character (%xml-tokenizer-peek tokenizer)))
    (unless (and character (or (char= character #\") (char= character #\')))
      (%xml-tokenizer-fail tokenizer (xml-message :expected-quoted-value-after-equals)
                           :constraint :attribute-value))
    (setf (%xml-tokenizer-state-space-seen-p
           (%xml-tokenizer-state tokenizer)) nil)
    (multiple-value-bind (value position) (%xml-tokenizer-scan-quoted-value tokenizer)
      (%xml-tokenizer-token tokenizer :quoted-value value position))))

(defun %xml-tokenizer-next-end-tag-close (tokenizer)
  (let ((character (%xml-tokenizer-peek tokenizer)))
    (unless (and character (char= character #\>))
      (%xml-tokenizer-fail tokenizer (xml-message :expected-close-after-end-tag-name)
                           :constraint :end-tag))
    (%xml-tokenizer-start-tag-close tokenizer :tag-close ">")))
