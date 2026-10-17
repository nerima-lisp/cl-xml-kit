;;;; src/spans-tag-node-scan.lisp

(in-package #:cl-xml-kit)

(declaim (inline %line-break-char-p))

(defun %line-break-char-p (character)
  (or (char= character #\Return) (char= character #\Linefeed)))

(defun %span-original (scanner index)
  (let ((offsets (span-scanner-offsets scanner)))
    (if offsets
        (aref offsets index)
        (+ index (span-scanner-shift scanner)))))

(defun %span-token-start (scanner token)
  (%span-original scanner (xml-position-offset (xml-token-position token))))

(defun %span-token-end (scanner token)
  (%span-original scanner (xml-token-end token)))

(defun %span-flush-run (scanner frame)
  (let ((run (span-frame-run frame)))
    (when run
      (push (%make-span-text (%span-original scanner (span-run-start run))
                             (%span-original scanner (span-run-end run))
                             (plusp (span-run-cdata-count run))
                             (and (= (span-run-token-count run) 1)
                                  (= (span-run-cdata-count run) 1))
                             (span-run-char-ref-p run)
                             (span-run-predefined-ref-p run)
                             (reverse (span-run-entity-names run)))
            (span-frame-children frame))
      (setf (span-frame-run frame) nil))))

(defun %span-add-run-piece (frame token)
  (let ((run (or (span-frame-run frame)
                 (setf (span-frame-run frame)
                       (%make-span-run
                        (xml-position-offset (xml-token-position token)))))))
    (setf (span-run-end run) (xml-token-end token))
    (incf (span-run-token-count run))
    (case (xml-token-kind token)
      (:cdata (incf (span-run-cdata-count run)))
      (:entity-reference
       (let ((body (%xml-token-value token)))
         (cond
           ((char= (char body 0) #\#)
            (setf (span-run-char-ref-p run) t))
           ((assoc body +xml-predefined-entities+ :test #'string=)
            (setf (span-run-predefined-ref-p run) t))
           (t
            (pushnew body (span-run-entity-names run) :test #'string=))))))))

(defun %span-add-node (scanner node)
  (let ((frame (car (span-scanner-stack scanner))))
    (cond
      (frame
       (%span-flush-run scanner frame)
       (push node (span-frame-children frame)))
      (t
       (push node (span-scanner-nodes scanner))
       (when (span-element-p node)
         (setf (span-scanner-root scanner) node))))))

(defun %span-attribute (scanner name-token value-token space-start)
  (let ((qname (%xml-token-value name-token))
        (value-offset (xml-position-offset (xml-token-position value-token))))
    (%make-span-attribute qname
                          (%namespace-declaration-name-p qname)
                          (%span-original scanner space-start)
                          (%span-token-start scanner name-token)
                          (%span-token-end scanner name-token)
                          (%span-token-start scanner value-token)
                          (%span-token-end scanner value-token)
                          (char (span-scanner-text scanner) value-offset))))

(defun %span-open-element (scanner open-token name-token attributes close-token)
  (let ((start (%span-token-start scanner open-token))
        (qname (%xml-token-value name-token))
        (name-end (%span-token-end scanner name-token))
        (start-tag-end (%span-token-end scanner close-token)))
    (if (eq (xml-token-kind close-token) :empty-element-close)
        (let ((slash (%span-token-start scanner close-token)))
          (%span-add-node scanner
                          (%make-span-element
                           :start start :end start-tag-end :qname qname
                           :name-end name-end :attributes attributes
                           :empty-p t :start-tag-end start-tag-end
                           :content-start slash :content-end slash)))
        (push (%make-span-frame qname start name-end attributes start-tag-end)
              (span-scanner-stack scanner)))))

(defun %span-leave-tag (scanner)
  "Return SCANNER to reading between tags."
  (setf (span-scanner-open-token scanner) nil
        (span-scanner-name-token scanner) nil
        (span-scanner-attribute-name scanner) nil
        (span-scanner-attributes scanner) nil
        (span-scanner-space-start scanner) nil))

(defun %span-start-tag-token (scanner token)
  "Take TOKEN, read inside the start tag that SCANNER has open."
  (case (xml-token-kind token)
    (:name
     (if (span-scanner-name-token scanner)
         (setf (span-scanner-attribute-name scanner) token)
         (setf (span-scanner-name-token scanner) token
               (span-scanner-space-start scanner) (xml-token-end token))))
    (:quoted-value
     (push (%span-attribute scanner (span-scanner-attribute-name scanner) token
                            (span-scanner-space-start scanner))
           (span-scanner-attributes scanner))
     (setf (span-scanner-space-start scanner) (xml-token-end token)))
    ((:tag-close :empty-element-close)
     (%span-open-element scanner (span-scanner-open-token scanner)
                         (span-scanner-name-token scanner)
                         (nreverse (span-scanner-attributes scanner)) token)
     (%span-leave-tag scanner))))

(defun %span-close-element (scanner open-token close-token)
  (let ((frame (pop (span-scanner-stack scanner))))
    (%span-flush-run scanner frame)
    (let ((end-tag-start (%span-token-start scanner open-token))
          (end (%span-token-end scanner close-token)))
      (%span-add-node scanner
                      (%make-span-element
                       :start (span-frame-start frame) :end end
                       :qname (span-frame-qname frame)
                       :name-end (span-frame-name-end frame)
                       :attributes (span-frame-attributes frame)
                       :empty-p nil
                       :start-tag-end (span-frame-start-tag-end frame)
                       :content-start (span-frame-start-tag-end frame)
                       :content-end end-tag-start
                       :end-tag-start end-tag-start :end-tag-end end
                       :children (nreverse (span-frame-children frame)))))))

(defun %span-end-tag-token (scanner token)
  "Take TOKEN, read inside the end tag that SCANNER has open."
  (case (xml-token-kind token)
    (:name (setf (span-scanner-name-token scanner) token))
    (:tag-close
     (%span-close-element scanner (span-scanner-open-token scanner) token)
     (%span-leave-tag scanner))))
