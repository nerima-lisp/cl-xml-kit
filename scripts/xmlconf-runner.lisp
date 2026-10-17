;;;; Command-line entry point of the XMLConf runner. Run with --help for usage.
;;;; The runner modules are loaded explicitly; loading them executes nothing.

(require :asdf)

(load (merge-pathnames "script-support.lisp" *load-truename*))

(cl-xml-kit/script-support:load-xmlconf-modules)

(cl-xml-kit/script-support:run-script
 "xmlconf-runner"
 cl-xml-kit/xmlconf:*runner-usage*
 #'cl-xml-kit/xmlconf:command-line-main)
