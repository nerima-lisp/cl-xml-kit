;;;; src/tokenizer-tags.lisp

(in-package #:cl-xml-kit)

(defun %xml-tokenizer-start-tag-close (tokenizer kind literal)
  (let ((position (%xml-tokenizer-current-position tokenizer)))
    (%xml-tokenizer-consume-literal tokenizer literal)
    (setf (%xml-tokenizer-state-mode (%xml-tokenizer-state tokenizer)) :content)
    (%xml-tokenizer-token tokenizer kind nil position)))

(defun %xml-tokenizer-name-token (tokenizer next-mode)
  (multiple-value-bind (value position) (%xml-tokenizer-scan-name tokenizer)
    (setf (%xml-tokenizer-state-mode (%xml-tokenizer-state tokenizer)) next-mode)
    (%xml-tokenizer-token tokenizer :name value position)))

(defun %xml-tokenizer-next-content (tokenizer)
  (let ((state (%xml-tokenizer-state tokenizer))
        (position (%xml-tokenizer-current-position tokenizer))
        (character (%xml-tokenizer-peek tokenizer)))
    (cond
      ((char= character #\&)
       (multiple-value-bind (value token-position) (%xml-tokenizer-scan-entity tokenizer)
         (%xml-tokenizer-token tokenizer :entity-reference value token-position)))
      ((char= character #\<)
       (cond
         ((%xml-tokenizer-starts-with-p tokenizer "<!--")
          (%xml-tokenizer-scan-comment tokenizer))
         ((%xml-tokenizer-starts-with-p tokenizer "<![CDATA[")
          (%xml-tokenizer-scan-cdata tokenizer))
         ((%xml-tokenizer-starts-with-p tokenizer "<!DOCTYPE")
          (%xml-tokenizer-scan-doctype tokenizer))
         ((%xml-tokenizer-starts-with-p tokenizer "<?")
          (%xml-tokenizer-scan-processing-instruction tokenizer))
         ((%xml-tokenizer-starts-with-p tokenizer "</")
          (%xml-tokenizer-consume-literal tokenizer "</")
          (setf (%xml-tokenizer-state-mode state) :end-tag)
          (%xml-tokenizer-token tokenizer :end-tag-open nil position))
         (t
          (%xml-tokenizer-advance tokenizer)
          (setf (%xml-tokenizer-state-mode state) :start-tag)
          (%xml-tokenizer-token tokenizer :start-tag-open nil position))))
      (t (%xml-tokenizer-scan-text tokenizer)))))

(defun %xml-tokenizer-next-attribute-or-close (tokenizer space-required-p)
  "Return the tag close or the next attribute name inside a start tag.

SPACE-REQUIRED-P is true after an attribute value, where XML requires
whitespace before another attribute."
  (let ((before (%xml-tokenizer-state-index (%xml-tokenizer-state tokenizer))))
    (%xml-tokenizer-skip-space tokenizer)
    (let ((character (%xml-tokenizer-peek tokenizer)))
      (cond
        ((%xml-tokenizer-starts-with-p tokenizer "/>")
         (%xml-tokenizer-start-tag-close tokenizer :empty-element-close "/>"))
        ((and character (char= character #\>))
         (%xml-tokenizer-start-tag-close tokenizer :tag-close ">"))
        ((not (and character (xml-name-start-char-p character)))
         (%xml-tokenizer-fail tokenizer
                              (if space-required-p
                                  (xml-message :expected-another-attribute-or-close)
                                  (xml-message :expected-attribute-or-close))
                              :constraint :start-tag))
        ((and space-required-p
              (= before (%xml-tokenizer-state-index (%xml-tokenizer-state tokenizer))))
         (%xml-tokenizer-fail tokenizer (xml-message :attributes-need-whitespace)
                              :constraint :start-tag))
        (t (%xml-tokenizer-name-token tokenizer :start-after-attribute-name))))))

(defun %xml-tokenizer-next-equals (tokenizer)
  (%xml-tokenizer-skip-space tokenizer)
  (let ((character (%xml-tokenizer-peek tokenizer)))
    (unless (and character (char= character #\=))
      (%xml-tokenizer-fail tokenizer (xml-message :expected-equals-after-attribute)
                           :constraint :attribute))
    (let ((position (%xml-tokenizer-current-position tokenizer)))
      (%xml-tokenizer-advance tokenizer)
      (setf (%xml-tokenizer-state-mode (%xml-tokenizer-state tokenizer)) :start-after-equals)
      (%xml-tokenizer-token tokenizer :equals nil position))))

(defun %xml-tokenizer-next-quoted-value (tokenizer)
  (%xml-tokenizer-skip-space tokenizer)
  (let ((character (%xml-tokenizer-peek tokenizer)))
    (unless (and character (or (char= character #\") (char= character #\')))
      (%xml-tokenizer-fail tokenizer (xml-message :expected-quoted-value-after-equals)
                           :constraint :attribute-value))
    (setf (%xml-tokenizer-state-mode (%xml-tokenizer-state tokenizer)) :start-after-quote)
    (multiple-value-bind (value position) (%xml-tokenizer-scan-quoted-value tokenizer)
      (%xml-tokenizer-token tokenizer :quoted-value value position))))

(defun %xml-tokenizer-next-end-tag-close (tokenizer)
  (%xml-tokenizer-skip-space tokenizer)
  (let ((character (%xml-tokenizer-peek tokenizer)))
    (unless (and character (char= character #\>))
      (%xml-tokenizer-fail tokenizer (xml-message :expected-close-after-end-tag-name)
                           :constraint :end-tag))
    (%xml-tokenizer-start-tag-close tokenizer :tag-close ">")))
