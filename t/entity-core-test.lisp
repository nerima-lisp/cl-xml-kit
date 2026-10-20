(in-package #:cl-xml-kit/test)

(describe "entity context construction"
  (it-each ((:resource-limits :bad "RESOURCE-LIMITS must be XML resource limits.")
            (:external-entity-policy :load "EXTERNAL-ENTITY-POLICY must be :REJECT or :SKIP.")
            (:external-entity-policy nil "EXTERNAL-ENTITY-POLICY must be :REJECT or :SKIP.")
            (:source 42 "SOURCE must be a string or NIL.")
            (:source :resource-limits "SOURCE must be a string or NIL.")
            (:source :external-entity-policy "SOURCE must be a string or NIL.")
            (:position "p" "POSITION must be an XML-POSITION."))
      "refuses ~S ~S with the message ~S"
      (key value message)
    (expect (lambda () (cl-xml-kit::make-xml-entity-context key value))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message))))

  (it-each ((:load) (nil) (:resource-limits))
      "refuses external-entity policy ~S when parsing a subset"
      (policy)
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset "" :external-entity-policy policy))
            :to-signal-xml '(xml-input-error
                             (xml-condition-message
                              "EXTERNAL-ENTITY-POLICY must be :REJECT or :SKIP."))))

  (it "keeps a private copy of the source name"
    (let* ((source (copy-seq "doc.xml"))
           (context (cl-xml-kit::make-xml-entity-context :source source)))
      (setf (char source 0) #\X)
      (expect (cl-xml-kit::xml-entity-context-source context) :to-equal "doc.xml")))

  (it "uses omitted limits and rejects external entities by default"
    (let* ((*default-xml-resource-limits* (make-xml-resource-limits :max-elements 3))
           (context (cl-xml-kit::make-xml-entity-context))
           (subset-context
             (cl-xml-kit::parse-xml-internal-subset
              "<!ENTITY remote SYSTEM 'urn:example:remote'>")))
      (expect (list (cl-xml-kit::xml-entity-context-limits context)
                    (cl-xml-kit::xml-entity-context-limits subset-context))
              :to-equal (list *default-xml-resource-limits*
                              *default-xml-resource-limits*))
      (expect (list (cl-xml-kit::xml-entity-context-external-entity-policy context)
                    (cl-xml-kit::xml-entity-context-external-entity-policy subset-context))
              :to-equal '(:reject :reject))
      (expect (lambda ()
                (cl-xml-kit::expand-xml-entity-references "&remote;" subset-context))
              :to-signal-xml
              '(xml-external-resource-disabled
                (xml-external-resource-disabled-kind :general-entity)
                (xml-external-resource-disabled-identifier "urn:example:remote"))))))

(describe "entity declaration construction"
  (it-each (("" "x" nil "ENTITY NAME must be a non-empty string.")
            (nil "x" nil "ENTITY NAME must be a non-empty string.")
            ("a" nil nil "An internal ENTITY declaration needs replacement text.")
            ("a" "x" t "An external ENTITY declaration cannot have replacement text."))
      "refuses name ~S, replacement ~S, external ~S with the message ~S"
      (name replacement-text external-p message)
    (expect (lambda ()
              (cl-xml-kit::make-xml-entity-declaration
               :name name :replacement-text replacement-text :external-p external-p))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message))))

  (it "preserves declaration defaults and external identifiers"
    (let ((declaration (cl-xml-kit::make-xml-entity-declaration
                        :name "e" :external-p t :public-id "-//P" :system-id "s"))
          (defaults (cl-xml-kit::%make-xml-entity-declaration-defaults)))
      (expect (list (cl-xml-kit::xml-entity-declaration-public-id declaration)
                    (cl-xml-kit::xml-entity-declaration-system-id declaration)
                    (cl-xml-kit::xml-entity-declaration-external-p declaration))
              :to-equal (quote ("-//P" "s" t)))
      (expect (list (cl-xml-kit::xml-entity-declaration-name defaults)
                    (cl-xml-kit::xml-entity-declaration-replacement-text defaults)
                    (cl-xml-kit::xml-entity-declaration-parameter-p defaults)
                    (cl-xml-kit::xml-entity-declaration-external-p defaults)
                    (cl-xml-kit::xml-entity-declaration-public-id defaults)
                    (cl-xml-kit::xml-entity-declaration-system-id defaults))
              :to-equal (quote ("" nil nil nil nil nil))))))

(describe "entity registration"
  (it "refuses to register into something other than an entity context"
    (expect (lambda ()
              (cl-xml-kit::xml-entity-context-declare
               :nope (cl-xml-kit::make-xml-entity-declaration :name "a" :replacement-text "")))
            :to-signal-xml '(xml-input-error
                             (xml-condition-message "CONTEXT must be an XML entity context."))))

  (it "refuses to register something other than an entity declaration"
    (expect (lambda ()
              (cl-xml-kit::xml-entity-context-declare
               (cl-xml-kit::make-xml-entity-context) :nope))
            :to-signal-xml '(xml-input-error
                             (xml-condition-message
                              "DECLARATION must be an XML entity declaration."))))

  (it "returns the registered declaration and expands references to it"
    (let ((context (cl-xml-kit::make-xml-entity-context))
          (declaration (cl-xml-kit::make-xml-entity-declaration
                        :name "who" :replacement-text "world")))
      (expect (cl-xml-kit::xml-entity-context-declare context declaration)
              :to-be declaration)
      (expect (cl-xml-kit::expand-xml-entity-references "hello &who;" context)
              :to-equal "hello world")))
  (it "rejects a duplicate declaration without replacing the original"
    (let ((context (cl-xml-kit::make-xml-entity-context))
          (original (cl-xml-kit::make-xml-entity-declaration
                     :name "who" :replacement-text "original"))
          (duplicate (cl-xml-kit::make-xml-entity-declaration
                      :name "who" :replacement-text "replacement")))
      (cl-xml-kit::xml-entity-context-declare context original)
      (expect (lambda ()
                (cl-xml-kit::xml-entity-context-declare context duplicate))
              :to-signal-xml
              (quote (xml-well-formedness-error
                      (xml-well-formedness-error-constraint :duplicate-entity-declaration)
                      (xml-well-formedness-error-expected "who")
                      (xml-condition-message "Duplicate XML entity declaration."))))
      (expect (cl-xml-kit::expand-xml-entity-references "&who;" context)
              :to-equal "original"))))

;; Expected characters follow XML 1.0 Fifth Edition [2] Char and [66] CharRef:
;; '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';', naming a Char.
(describe "character references"
  (it-each (("&#65;" 65) ("&#x41;" 65) ("&#0065;" 65) ("&#xAf;" #xAF) ("&#xaF;" #xAF)
            ("&#9;" 9) ("&#xA;" 10) ("&#xD;" 13) ("&#x20;" #x20) ("&#xD7FF;" #xD7FF)
            ("&#xE000;" #xE000) ("&#xFFFD;" #xFFFD) ("&#x10000;" #x10000)
            ("&#x10FFFF;" #x10FFFF))
      "expands ~S to the character with code ~S"
      (reference code)
    (expect (cl-xml-kit::expand-xml-entity-references
             reference (cl-xml-kit::make-xml-entity-context))
            :to-equal (string (code-char code))))

  (it-each (("&#0;") ("&#x8;") ("&#x1F;") ("&#xD800;") ("&#xDFFF;") ("&#xFFFE;")
            ("&#xFFFF;") ("&#x110000;") ("&#99999999999999999999;"))
      "rejects ~S because it names no XML character"
      (reference)
    (expect (lambda ()
              (cl-xml-kit::expand-xml-entity-references
               reference (cl-xml-kit::make-xml-entity-context)))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :legal-character)
                             (xml-well-formedness-error-expected nil))))

  (it-each (("&#;") ("&#x;") ("&#xG;") ("&#12a;") ("&#X41;") ("&#-1;") ("&# 65;"))
      "rejects the malformed character reference ~S"
      (reference)
    (expect (lambda ()
              (cl-xml-kit::expand-xml-entity-references
               reference (cl-xml-kit::make-xml-entity-context)))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :character-reference)
                             (xml-well-formedness-error-expected nil))))

  ;; [66] admits only ASCII digits; these are Arabic-Indic and fullwidth forms.
  (it-each (("&#~C~C;" #x666 #x665) ("&#x~C~C;" #xFF14 #xFF11) ("&#x~C;" #xFF21))
      "rejects the non-ASCII digits in ~S ~S"
      (control &rest codes)
    (expect (lambda ()
              (cl-xml-kit::expand-xml-entity-references
               (apply #'format nil control (mapcar #'code-char codes))
               (cl-xml-kit::make-xml-entity-context)))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :character-reference)))))

(describe "references inside entity values"
  (it-each (("&1x;" :entity-reference nil)
            ("&;" :entity-reference nil)
            ("&ab" :entity-reference ";")
            ("&#;" :character-reference nil)
            ("&#xD800;" :legal-character nil))
      "rejects the entity value ~S with constraint ~S and expected ~S"
      (value constraint expected)
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset (format nil "<!ENTITY e '~A'>" value)))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint ,constraint)
                             (xml-well-formedness-error-expected ,expected))))

  (it "rejects non-ASCII digits in a character reference inside an entity value"
    (expect (lambda ()
              (cl-xml-kit::parse-xml-internal-subset
               (format nil "<!ENTITY e '&#~C~C;'>" (code-char #x666) (code-char #x665))))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :character-reference))))

  (it "accepts character, predefined, and not yet declared references in a value"
    (expect (cl-xml-kit::xml-entity-declaration-name
             (subset-declaration "<!ENTITY e '&#x41;&amp;&later;'>" "e"))
            :to-equal "e")))

(describe "general entity references"
  ;; XML 1.0 section 4.6 predefines exactly these five entities.
  (it "expands the five predefined entities"
    (expect (cl-xml-kit::expand-xml-entity-references
             "&lt;&gt;&amp;&apos;&quot;" (cl-xml-kit::make-xml-entity-context))
            :to-equal "<>&'\""))

  (it "rejects a reference without its terminating semicolon"
    (expect (lambda ()
              (cl-xml-kit::expand-xml-entity-references
               "&amp" (cl-xml-kit::make-xml-entity-context)))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-reference)
                             (xml-well-formedness-error-expected ";")
                             (xml-condition-message
                              "An entity reference must end with a semicolon."))))

  (it-each (("<r>&missing;</r>" "missing" "Undeclared XML entity missing.")
            ("<!DOCTYPE r [%missing;]><r/>" "missing" "Undeclared parameter entity missing.")
            ("<!DOCTYPE r [<!ENTITY b '[&zz;]'>]><r>&b;</r>" "zz" "Undeclared XML entity zz."))
      "rejects ~S because entity ~S is undeclared, saying ~S"
      (document name message)
    (expect (lambda () (parse-xml document))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-declared)
                             (xml-well-formedness-error-expected ,name)
                             (xml-condition-message ,message)))))

(describe "external entity references"
  (it-each (("<!ENTITY remote SYSTEM 'urn:example:remote'>" "A&remote;B"
             :general-entity "urn:example:remote")
            ("<!ENTITY remote PUBLIC '-//P' 'urn:p'>" "&remote;"
             :general-entity "urn:p"))
      "refuses to expand the external entity in ~S referenced by ~S as ~S ~S"
      (subset text kind identifier)
    (expect (lambda ()
              (cl-xml-kit::expand-xml-entity-references
               text (cl-xml-kit::parse-xml-internal-subset subset)))
            :to-signal-xml `(xml-external-resource-disabled
                             (xml-external-resource-disabled-kind ,kind)
                             (xml-external-resource-disabled-identifier ,identifier))))

  (it "names the entity when an external declaration has no system identifier"
    (let ((context (cl-xml-kit::make-xml-entity-context)))
      (cl-xml-kit::xml-entity-context-declare
       context (cl-xml-kit::make-xml-entity-declaration :name "bare" :external-p t))
      (expect (lambda () (cl-xml-kit::expand-xml-entity-references "&bare;" context))
              :to-signal-xml '(xml-external-resource-disabled
                               (xml-external-resource-disabled-identifier "bare")
                               (xml-condition-message
                                "External general entity bare is disabled.")))))

  (it "drops skipped external references and records each entity once"
    (let ((context (cl-xml-kit::parse-xml-internal-subset
                    "<!ENTITY remote SYSTEM 'urn:example:remote'>"
                    :external-entity-policy :skip)))
      (expect (cl-xml-kit::expand-xml-entity-references "A&remote;&remote;B" context)
              :to-equal "AB")
      (expect (cl-xml-kit::xml-entity-context-skipped-external-entities context)
              :to-equal '("remote"))))
  (it "treats an empty reference body as a named reference"
    (expect (cl-xml-kit::%reference-character "" (cl-xml-kit::make-xml-entity-context))
            :to-be nil)))
