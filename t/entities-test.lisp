;;;; t/entities-test.lisp

(in-package #:cl-xml-kit/test)

(it "expands predefined and character references without external access"
  (let ((context (cl-xml-kit::make-xml-entity-context)))
    (expect (cl-xml-kit::expand-xml-entity-references
             "&lt;a&gt;&amp;&#65;&#x42;" context)
            :to-equal "<a>&AB")))

(it "parses and expands internal general entities"
  (let ((context (cl-xml-kit::parse-xml-internal-subset
                  "<!ENTITY greeting \"hello &amp; world\">")))
    (expect (cl-xml-kit::expand-xml-entity-references
             "before &greeting; after" context)
            :to-equal "before hello & world after")))

(it "expands parameter entities while collecting declarations"
  (let ((context (cl-xml-kit::parse-xml-internal-subset
                  "<!ENTITY % declaration \"<!ENTITY answer '42'>\"> %declaration;")))
    (expect (cl-xml-kit::expand-xml-entity-references "&answer;" context)
            :to-equal "42")))

(it "rejects undeclared, illegal, and recursive references"
  (let ((context (cl-xml-kit::make-xml-entity-context)))
    (let ((condition nil))
      (handler-case
          (cl-xml-kit::expand-xml-entity-references "&missing;" context)
        (xml-well-formedness-error (caught) (setf condition caught)))
      (expect (xml-well-formedness-error-constraint condition)
              :to-equal :entity-declared))
    (let ((condition nil))
      (handler-case
          (cl-xml-kit::expand-xml-entity-references "&#x110000;" context)
        (xml-well-formedness-error (caught) (setf condition caught)))
      (expect (xml-well-formedness-error-constraint condition)
              :to-equal :legal-character))
    (let ((recursive (cl-xml-kit::make-xml-entity-context))
          (condition nil))
      (cl-xml-kit::xml-entity-context-declare
       recursive
       (cl-xml-kit::make-xml-entity-declaration
        :name "loop" :replacement-text "&loop;"))
      (handler-case
          (cl-xml-kit::expand-xml-entity-references "&loop;" recursive)
        (xml-well-formedness-error (caught) (setf condition caught)))
      (expect (xml-well-formedness-error-constraint condition)
              :to-equal :no-recursion))))

(it "enforces expansion count, size, and depth limits before appending"
  (let ((condition nil)
        (context (cl-xml-kit::parse-xml-internal-subset
                  "<!ENTITY one 'x'>"
                  :resource-limits
                  (make-xml-resource-limits :max-entity-expansions 1
                                             :max-expanded-characters 1
                                             :max-entity-depth 1))))
    (handler-case
        (cl-xml-kit::expand-xml-entity-references "&one;&one;" context)
      (xml-resource-limit-exceeded (caught) (setf condition caught)))
    (expect (xml-resource-limit-exceeded-kind condition)
            :to-equal :max-entity-expansions)
    (setf condition nil)
    (let ((small (cl-xml-kit::parse-xml-internal-subset
                  "<!ENTITY two 'xx'>"
                  :resource-limits
                  (make-xml-resource-limits :max-expanded-characters 1))))
      (handler-case
          (cl-xml-kit::expand-xml-entity-references "&two;" small)
        (xml-resource-limit-exceeded (caught) (setf condition caught))))
    (expect (xml-resource-limit-exceeded-kind condition)
            :to-equal :max-expanded-characters)
    (setf condition nil)
    (let ((deep (cl-xml-kit::parse-xml-internal-subset
                 "<!ENTITY one '&two;'><!ENTITY two 'x'>"
                 :resource-limits
                 (make-xml-resource-limits :max-entity-depth 1))))
      (handler-case
          (cl-xml-kit::expand-xml-entity-references "&one;" deep)
        (xml-resource-limit-exceeded (caught) (setf condition caught))))
    (expect (xml-resource-limit-exceeded-kind condition)
            :to-equal :max-entity-depth)))

(it "applies the XML name-length limit to entity references"
  (let ((condition nil)
        (context (cl-xml-kit::make-xml-entity-context
                  :resource-limits
                  (make-xml-resource-limits :max-name-length 3))))
    (handler-case
        (cl-xml-kit::expand-xml-entity-references "&long-name;" context)
      (xml-resource-limit-exceeded (caught) (setf condition caught)))
    (expect (xml-resource-limit-exceeded-kind condition)
            :to-equal :max-name-length)))

(it "rejects external declarations and skips them without replacement"
  (let ((reject-condition nil)
        (reject (cl-xml-kit::parse-xml-internal-subset
                 "<!ENTITY remote SYSTEM 'urn:example:remote'>")))
    (handler-case
        (cl-xml-kit::expand-xml-entity-references "A&remote;B" reject)
      (xml-external-resource-disabled (caught)
        (setf reject-condition caught)))
    (expect (xml-external-resource-disabled-kind reject-condition)
            :to-equal :general-entity)
    (let ((skip (cl-xml-kit::parse-xml-internal-subset
                 "<!ENTITY remote SYSTEM 'urn:example:remote'>"
                 :external-entity-policy :skip)))
      (expect (cl-xml-kit::expand-xml-entity-references "A&remote;B" skip)
              :to-equal "AB")
      (expect (member "remote"
                      (cl-xml-kit::xml-entity-context-skipped-external-entities skip)
                      :test #'string=)
              :to-be-truthy))))

(it "rejects less-than generated in an attribute value"
  (let ((context (cl-xml-kit::make-xml-entity-context))
        (condition nil))
    (handler-case
        (cl-xml-kit::expand-xml-entity-references "&lt;" context :attribute-p t)
      (xml-well-formedness-error (caught) (setf condition caught)))
    (expect (xml-well-formedness-error-constraint condition)
            :to-equal :no-lt-in-attribute-values)))

(it "rejects ampersand generated in an attribute value"
  (let ((context (cl-xml-kit::parse-xml-internal-subset
                  "<!ENTITY e \"&#38;\">"))
        (condition nil))
    (handler-case
        (cl-xml-kit::expand-xml-entity-references "&e;" context
                                                   :attribute-p t)
      (xml-well-formedness-error (caught) (setf condition caught)))
    (expect (xml-well-formedness-error-constraint condition)
            :to-equal :no-amp-in-attribute-values)))

(it "expands decimal and hexadecimal references in either letter case"
  (expect (cl-xml-kit::expand-xml-entity-references
           "&#65;&#x41;&#xAf;&#xaF;" (cl-xml-kit::make-xml-entity-context))
          :to-equal (format nil "AA~C~C" (code-char #xAF) (code-char #xAF))))

(it "rejects non-ASCII digits in character references"
  (dolist (reference (list (format nil "&#~C~C;" (code-char #x666) (code-char #x665))
                           (format nil "&#x~C~C;" (code-char #xFF14) (code-char #xFF11))
                           (format nil "&#x~C;" (code-char #xFF21))))
    (let ((condition nil))
      (handler-case
          (cl-xml-kit::expand-xml-entity-references
           reference (cl-xml-kit::make-xml-entity-context))
        (xml-well-formedness-error (caught) (setf condition caught)))
      (expect (xml-well-formedness-error-constraint condition)
              :to-equal :character-reference))))

(it "rejects non-ASCII digits in a character reference inside an entity value"
  (let ((condition nil))
    (handler-case
        (cl-xml-kit::parse-xml-internal-subset
         (format nil "<!ENTITY e '&#~C~C;'>" (code-char #x666) (code-char #x665)))
      (xml-well-formedness-error (caught) (setf condition caught)))
    (expect (xml-well-formedness-error-constraint condition)
            :to-equal :character-reference)))
