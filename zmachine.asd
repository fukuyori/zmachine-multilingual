;;;; Z-machine Interpreter in Common Lisp
;;;; zmachine.asd - ASDF system definition

(asdf:defsystem :zmachine
  :name "Z-machine Interpreter"
  :description "A Z-machine interpreter for running Infocom text adventures"
  :author "Claude"
  :version "1.0.0"
  :license "MIT"
  :serial t
  :components ((:file "packages")
               (:file "memory")
               (:file "text")
               (:file "objects")
               (:file "dictionary")
               (:file "decode")
               (:file "opcodes")
               (:file "opcodes-var")
               (:file "execute")
               (:file "translate")
               (:file "languages")))
