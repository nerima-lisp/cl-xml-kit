;;;; src/tokenizer-doctype.lisp

(in-package #:cl-xml-kit)

(defun %doctype-skip-space (body index)
  (loop while (and (< index (length body)) (xml-space-char-p (char body index)))
        do (incf index))
  index)

(defun %doctype-keyword-at-p (body index keyword)
  (let ((end (+ index (length keyword))))
    (and (<= end (length body))
         (string= keyword body :start2 index :end2 end))))

(defun %doctype-quoted-literal-end (body index)
  "Return the index after the quoted literal at INDEX in BODY, or NIL."
  (let ((quote (and (< index (length body)) (char body index))))
    (when (member quote '(#\" #\'))
      (let ((close (position quote body :start (1+ index))))
        (and close (1+ close))))))

(defun %xml-tokenizer-doctype-root-name (tokenizer body index)
  "Validate the root element Name at INDEX in BODY and return the index after it."
  (unless (and (< index (length body)) (xml-name-start-char-p (char body index)))
    (%xml-tokenizer-fail tokenizer (xml-message :doctype-needs-root-element)
                         :constraint :doctype))
  (let ((end (1+ index)))
    (loop while (and (< end (length body)) (xml-name-char-p (char body end)))
          do (incf end))
    (%xml-tokenizer-check-name-limit tokenizer (- end index))
    end))

(defun %xml-tokenizer-doctype-literal (tokenizer body index message-control)
  "Return the index after the space-led quoted literal at INDEX, or fail with MESSAGE-CONTROL."
  (or (%doctype-quoted-literal-end body (%doctype-skip-space body index))
      (%xml-tokenizer-fail tokenizer message-control :constraint :doctype)))

(defun %xml-tokenizer-doctype-external-id (tokenizer body index)
  "Validate an optional SYSTEM or PUBLIC ExternalID at INDEX and return the index after it."
  (cond
    ((%doctype-keyword-at-p body index "SYSTEM")
     (%xml-tokenizer-doctype-literal tokenizer body (+ index 6)
                                     (xml-message :system-needs-literal)))
    ((%doctype-keyword-at-p body index "PUBLIC")
     (%xml-tokenizer-doctype-literal
      tokenizer body
      (%xml-tokenizer-doctype-literal tokenizer body (+ index 6)
                                      (xml-message :public-needs-public-literal))
      (xml-message :public-needs-system-literal)))
    (t index)))

(defun %xml-tokenizer-doctype-subset-end (tokenizer body index)
  "Return the index after an internal subset that starts at INDEX, or INDEX when absent.

Brackets inside quoted literals do not count toward the subset nesting."
  (if (and (< index (length body)) (char= (char body index) #\[))
      (let ((depth 1)
            (quote nil)
            (length (length body)))
        (incf index)
        (loop while (and (< index length) (plusp depth))
              do (let ((character (char body index)))
                   (cond
                     (quote (when (char= character quote) (setf quote nil)))
                     ((or (char= character #\") (char= character #\')) (setf quote character))
                     ((char= character #\[) (incf depth))
                     ((char= character #\]) (decf depth)))
                   (incf index)))
        (when (plusp depth)
          (%xml-tokenizer-fail tokenizer (xml-message :unterminated-internal-subset)
                               :constraint :doctype))
        index)
      index))

(defun %xml-tokenizer-validate-doctype-body (tokenizer body)
  (let* ((index (%xml-tokenizer-doctype-root-name
                 tokenizer body (%doctype-skip-space body 0)))
         (index (%xml-tokenizer-doctype-external-id
                 tokenizer body (%doctype-skip-space body index)))
         (index (%xml-tokenizer-doctype-subset-end
                 tokenizer body (%doctype-skip-space body index))))
    (unless (= (%doctype-skip-space body index) (length body))
      (%xml-tokenizer-fail tokenizer (xml-message :unexpected-doctype-content)
                           :constraint :doctype))))

(defun %xml-tokenizer-scan-doctype (tokenizer)
  (let* ((input (%xml-tokenizer-input tokenizer))
         (state (%xml-tokenizer-state tokenizer))
         (position (%xml-tokenizer-current-position tokenizer)))
    (%xml-tokenizer-consume-literal tokenizer "<!DOCTYPE")
    (let ((start (%xml-tokenizer-state-index state))
          (subset-depth 0)
          (quote nil))
      (loop
        (when (= (%xml-tokenizer-state-index state) (length input))
          (%xml-tokenizer-fail tokenizer (xml-message :unterminated-doctype)
                               :constraint :doctype :expected #\>))
        (let ((character (char input (%xml-tokenizer-state-index state))))
          (cond
            (quote
             (%xml-tokenizer-advance tokenizer)
             (when (char= character quote) (setf quote nil)))
            ((or (char= character #\") (char= character #\'))
             (setf quote character)
             (%xml-tokenizer-advance tokenizer))
            ((char= character #\[)
             (incf subset-depth)
             (%xml-tokenizer-advance tokenizer))
            ((char= character #\])
             (when (zerop subset-depth)
               (%xml-tokenizer-fail tokenizer (xml-message :unmatched-bracket-in-doctype)
                                    :constraint :doctype))
             (decf subset-depth)
             (%xml-tokenizer-advance tokenizer))
            ((and (char= character #\>) (zerop subset-depth))
             (let ((end (%xml-tokenizer-state-index state)))
               (%xml-tokenizer-validate-doctype-body tokenizer
                                                      (subseq input start end))
               (%xml-tokenizer-advance tokenizer)
               (setf (%xml-tokenizer-state-mode state) :content)
               (return (%xml-tokenizer-token tokenizer :doctype
                                             (subseq input start end) position))))
            (t (%xml-tokenizer-advance tokenizer))))))))
