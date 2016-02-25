(in-package :common-lisp-user)

(asdf:defsystem #:snark-numbering
  :serial t
  :description "The Snark Theorem Prover"
  :version "20120808r022"
  :author "Mark E. Stickel, SRI International"
  :license "MPL 1.1, see file LICENSE"
  :depends-on (#:snark-auxiliary-packages #:snark-lisp #:snark-sparse-array)
  :pathname "src/"
  :components ((:file "numbering")))
