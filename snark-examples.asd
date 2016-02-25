(in-package :common-lisp-user)

(asdf:defsystem #:snark-examples
  :serial t
  :description "Examples for Snark"
  :version "20120808r022"
  :author "Mark E. Stickel, SRI International"
  :author "Matthias Hölzl, LMU"
  :license "MPL 1.1, see file LICENSE"
  :depends-on (#:snark)
  :pathname "examples/"
  :components ((:file "overbeek-test")
               (:file "front-last-example")
               (:file "steamroller-example")
               (:file "reverse-example")
               (:file "hot-drink-example")
               (:file "coder-examples")
               (:file "latin-squares")))
