(in-package #:cl-xml-kit/test)

(it "keeps the documented condition inheritance"
  (expect (subtypep 'xml-unsupported-encoding 'xml-decode-error) :to-be-truthy)
  (expect (subtypep 'xml-decode-error 'xml-input-error) :to-be-truthy)
  (expect (subtypep 'xml-namespace-error 'xml-parse-error) :to-be-truthy))

(it "retains shared and specific condition readers"
  (let ((condition (make-condition 'xml-resource-limit-exceeded
                                   :message "too deep" :kind :max-depth
                                   :value 3 :limit 2)))
    (expect (xml-condition-message condition) :to-equal "too deep")
    (expect (xml-resource-limit-exceeded-kind condition) :to-equal :max-depth)
    (expect (xml-resource-limit-exceeded-value condition) :to-equal 3)
    (expect (xml-resource-limit-exceeded-limit condition) :to-equal 2)))

(it "retains every public condition payload"
  (let ((decode (make-condition 'xml-decode-error :message "decode"
                               :octet 255 :encoding :utf-8))
        (unsupported (make-condition 'xml-unsupported-encoding
                                     :message "unsupported" :encoding "UTF-16"
                                     :reason :declaration))
        (well-formed (make-condition 'xml-well-formedness-error
                                     :constraint :root-element :expected "root"))
        (namespace (make-condition 'xml-namespace-error
                                   :prefix "p" :namespace-uri "urn:p"))
        (external (make-condition 'xml-external-resource-disabled
                                  :identifier "x.dtd" :kind :doctype))
        (parser-state (make-condition 'xml-parser-state-error :parser :parser))
        (invalid (make-condition 'xml-invalid-resource-limit
                                 :kind :max-depth :value 0))
        (serialization (make-condition 'xml-serialization-error
                                       :object :object :path '(:element))))
    (expect (list (xml-decode-error-octet decode)
                  (xml-decode-error-encoding decode)
                  (xml-unsupported-encoding-encoding unsupported)
                  (xml-unsupported-encoding-reason unsupported)
                  (xml-well-formedness-error-constraint well-formed)
                  (xml-well-formedness-error-expected well-formed)
                  (xml-namespace-error-prefix namespace)
                  (xml-namespace-error-namespace-uri namespace)
                  (xml-external-resource-disabled-identifier external)
                  (xml-external-resource-disabled-kind external)
                  (xml-parser-state-error-parser parser-state)
                  (xml-invalid-resource-limit-kind invalid)
                  (xml-invalid-resource-limit-value invalid)
                  (xml-serialization-error-object serialization)
                  (xml-serialization-error-path serialization))
            :to-equal '(255 :utf-8 "UTF-16" :declaration :root-element "root"
                         "p" "urn:p" "x.dtd" :doctype :parser :max-depth 0
                         :object (:element)))))

(it "reports the shared condition metadata"
  (let* ((position (make-xml-position :source "memory.xml"
                                      :offset 4 :line 1 :column 5))
         (condition (make-condition 'xml-kit-error
                                    :message "bad XML"
                                    :position position
                                    :source "memory.xml")))
    (expect (xml-condition-message condition) :to-equal "bad XML")
    (expect (xml-condition-position condition) :to-equal position)
    (expect (xml-condition-source condition) :to-equal "memory.xml")
    (expect (with-output-to-string (stream)
              (princ condition stream))
            :to-equal "bad XML")))

(it "keeps the documented edit condition inheritance"
  (expect (subtypep 'xml-edit-error 'xml-kit-error) :to-be-truthy)
  (dolist (type '(xml-path-error xml-unsupported-edit xml-edit-value-error))
    (expect (subtypep type 'xml-edit-error) :to-be-truthy)))

(it "retains the edit condition payloads"
  (let ((path-error (make-condition 'xml-path-error :message "bad path" :path "/a/"
                                                    :reason :syntax :index 3))
        (unsupported (make-condition 'xml-unsupported-edit :message "refused"
                                                           :path "/a/@xmlns"
                                                           :reason :namespace-declaration))
        (value-error (make-condition 'xml-edit-value-error :message "bad value"
                                                           :path "/a" :reason :malformed-fragment
                                                           :cause :cause)))
    (expect (list (xml-edit-error-path path-error) (xml-edit-error-reason path-error)
                  (xml-path-error-index path-error) (xml-condition-message path-error)
                  (xml-edit-error-path unsupported) (xml-edit-error-reason unsupported)
                  (xml-edit-error-path value-error) (xml-edit-error-reason value-error)
                  (xml-edit-value-error-cause value-error))
            :to-equal '("/a/" :syntax 3 "bad path" "/a/@xmlns" :namespace-declaration
                        "/a" :malformed-fragment :cause))))

(it "defaults the optional edit condition payloads to nil"
  (let ((path-error (make-condition 'xml-path-error :message "no match" :path "/a"
                                                    :reason :no-match))
        (value-error (make-condition 'xml-edit-value-error :message "bad"
                                                           :reason :not-single-element)))
    (expect (list (xml-path-error-index path-error) (xml-edit-value-error-cause value-error)
                  (xml-edit-error-path value-error))
            :to-equal '(nil nil nil))))
