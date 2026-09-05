;;;; Z-machine Interpreter in Common Lisp
;;;; zmachine.asd - ASDF system definition

(asdf:defsystem :zmachine
  :name "Z-machine Interpreter"
  :description "A Z-machine interpreter for running Infocom text adventures"
  :author "Claude"
  :version "0.5.4"
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
               (:file "settings")
               (:file "glossary")
               (:file "translate")
               (:file "ollama")
               (:file "blorb")
               (:file "graphics")
               (:file "languages")))
