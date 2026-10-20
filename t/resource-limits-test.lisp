(in-package #:cl-xml-kit/test)

;; Defaults from the limit table in docs/src/architecture.md section 3.
(describe "resource limits"
  (it-each ((xml-resource-limits-max-input-octets 16777216)
            (xml-resource-limits-max-document-characters 16777216)
            (xml-resource-limits-max-output-characters 16777216)
            (xml-resource-limits-max-depth 512)
            (xml-resource-limits-max-elements 1000000)
            (xml-resource-limits-max-attributes-per-element 1024)
            (xml-resource-limits-max-name-length 1024)
            (xml-resource-limits-max-text-node-length 1048576)
            (xml-resource-limits-max-namespace-bindings 1024)
            (xml-resource-limits-max-entity-expansions 10000)
            (xml-resource-limits-max-expanded-characters 16777216)
            (xml-resource-limits-max-entity-depth 32))
      "defaults ~S to the documented ~D"
      (reader expected)
    (expect (list (funcall reader *default-xml-resource-limits*)
                  (funcall reader (make-xml-resource-limits)))
            :to-equal (list expected expected)))

  ;; Each row rebuilds the full fixture so every reader is checked with all limits configured.
  (it-each ((xml-resource-limits-max-input-octets 1)
            (xml-resource-limits-max-document-characters 2)
            (xml-resource-limits-max-output-characters 3)
            (xml-resource-limits-max-depth 4)
            (xml-resource-limits-max-elements 5)
            (xml-resource-limits-max-attributes-per-element 6)
            (xml-resource-limits-max-name-length 7)
            (xml-resource-limits-max-text-node-length 8)
            (xml-resource-limits-max-namespace-bindings 9)
            (xml-resource-limits-max-entity-expansions 10)
            (xml-resource-limits-max-expanded-characters 11)
            (xml-resource-limits-max-entity-depth 12))
      "returns configured ~S as ~D"
      (reader expected)
    (let ((limits (make-xml-resource-limits
                   :max-input-octets 1 :max-document-characters 2
                   :max-output-characters 3 :max-depth 4 :max-elements 5
                   :max-attributes-per-element 6 :max-name-length 7
                   :max-text-node-length 8 :max-namespace-bindings 9
                   :max-entity-expansions 10 :max-expanded-characters 11
                   :max-entity-depth 12)))
      (expect (funcall reader limits) :to-equal expected)))

  (it "keeps the unconfigured limits at their defaults"
    (let ((limits (make-xml-resource-limits :max-depth 4 :max-name-length 8)))
      (expect (list (xml-resource-limits-p limits) (xml-resource-limits-max-depth limits)
                    (xml-resource-limits-max-name-length limits)
                    (xml-resource-limits-max-elements limits))
              :to-equal '(t 4 8 1000000))))

  (it-each ((nil nil) (:max-depth nil) (42 nil))
      "does not take ~S for resource limits"
      (value expected)
    (expect (xml-resource-limits-p value) :to-be expected))

  (it-each ((:max-input-octets 0) (:max-document-characters -1) (:max-output-characters nil)
            (:max-depth 1.5) (:max-depth 0) (:max-elements 0)
            (:max-attributes-per-element -2) (:max-name-length nil)
            (:max-text-node-length 0) (:max-namespace-bindings -1)
            (:max-entity-expansions nil) (:max-expanded-characters 0)
            (:max-entity-depth -3) (:max-entity-depth "1") (:max-depth 2.0))
      "refuses ~S set to ~S"
      (kind value)
    (expect (lambda () (make-xml-resource-limits kind value))
            :to-signal-xml `(xml-invalid-resource-limit
                             (xml-invalid-resource-limit-kind ,kind)
                             (xml-invalid-resource-limit-value ,value)
                             (xml-condition-message
                              ,(format nil "Resource limit ~S must be a positive integer, not ~S."
                                       kind value)))))

  (it "names the invalid limit and value in the condition message"
    (expect (lambda () (make-xml-resource-limits :max-depth 0))
            :to-signal-xml '(xml-invalid-resource-limit
                             (xml-condition-message
                              "Resource limit :MAX-DEPTH must be a positive integer, not 0."))))

  (it "accepts the smallest positive limit"
    (expect (xml-resource-limits-max-depth (make-xml-resource-limits :max-depth 1))
            :to-equal 1)))

