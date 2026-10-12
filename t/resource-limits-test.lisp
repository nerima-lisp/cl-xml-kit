(in-package #:cl-xml-kit/test)

(it "uses the documented default resource limits"
  (let ((limits *default-xml-resource-limits*))
    (expect (xml-resource-limits-max-input-octets limits) :to-equal 16777216)
    (expect (xml-resource-limits-max-depth limits) :to-equal 512)
    (expect (xml-resource-limits-max-elements limits) :to-equal 1000000)))

(it "constructs independent validated resource limits"
  (let ((limits (make-xml-resource-limits :max-depth 4 :max-name-length 8)))
    (expect (xml-resource-limits-p limits) :to-be-truthy)
    (expect (xml-resource-limits-max-depth limits) :to-equal 4)
    (expect (xml-resource-limits-max-name-length limits) :to-equal 8)))

(it "exposes every configured limit"
  (let ((limits (make-xml-resource-limits
                 :max-input-octets 1 :max-document-characters 2
                 :max-output-characters 3 :max-depth 4 :max-elements 5
                 :max-attributes-per-element 6 :max-name-length 7
                 :max-text-node-length 8 :max-namespace-bindings 9
                 :max-entity-expansions 10 :max-expanded-characters 11
                 :max-entity-depth 12)))
    (expect (list (xml-resource-limits-max-input-octets limits)
                  (xml-resource-limits-max-document-characters limits)
                  (xml-resource-limits-max-output-characters limits)
                  (xml-resource-limits-max-depth limits)
                  (xml-resource-limits-max-elements limits)
                  (xml-resource-limits-max-attributes-per-element limits)
                  (xml-resource-limits-max-name-length limits)
                  (xml-resource-limits-max-text-node-length limits)
                  (xml-resource-limits-max-namespace-bindings limits)
                  (xml-resource-limits-max-entity-expansions limits)
                  (xml-resource-limits-max-expanded-characters limits)
                  (xml-resource-limits-max-entity-depth limits))
            :to-equal '(1 2 3 4 5 6 7 8 9 10 11 12))))

(it "signals a package condition for a non-positive limit"
  (let ((condition nil))
    (handler-case (make-xml-resource-limits :max-depth 0)
      (xml-invalid-resource-limit (caught) (setf condition caught)))
    (expect (typep condition 'xml-invalid-resource-limit) :to-be-truthy)
    (expect (xml-invalid-resource-limit-kind condition) :to-equal :max-depth)))

(it "rejects every invalid limit value"
  (dolist (entry '((:max-input-octets . 0) (:max-document-characters . -1)
                   (:max-output-characters . nil) (:max-depth . 1.5)
                   (:max-elements . 0) (:max-attributes-per-element . -2)
                   (:max-name-length . nil) (:max-text-node-length . 0)
                   (:max-namespace-bindings . -1)
                   (:max-entity-expansions . nil)
                   (:max-expanded-characters . 0) (:max-entity-depth . -3)))
    (let ((condition nil))
      (handler-case
          (apply #'make-xml-resource-limits (list (car entry) (cdr entry)))
        (xml-invalid-resource-limit (caught) (setf condition caught)))
      (expect (xml-invalid-resource-limit-kind condition) :to-equal (car entry))
      (expect (xml-invalid-resource-limit-value condition) :to-equal (cdr entry)))))

