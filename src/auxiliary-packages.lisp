(in-package :common-lisp-user)

#+asdf
(defparameter *snark-system-pathname*
  (let ((system (asdf:find-system :snark)))
    (asdf:system-source-directory system)))
    
#+asdf
(defvar *compile-me* nil)

#+asdf
(defun make-snark-system (&optional (*compile-me* *compile-me*))
  (when (eql *compile-me* :optimize)
    (proclaim (print '(optimize (safety 1) (space 1) (speed 3) (debug 1)))))
  (with-compilation-unit ()
    (if *compile-me*
	(progn
	  (asdf:compile-system :snark :force t)
	  (asdf:load-system :snark))
	(asdf:load-system :snark))))

(defpackage :snark-lisp
  (:use :common-lisp)
  (:export

   ;; defined in mvlet.lisp
   #:mvlet #:mvlet*

   ;; defined in progc.lisp
   #:prog->
   #:*prog->-function-second-forms*
   #:*prog->-special-forms*

   ;; defined in lisp.lisp
   #:none
   #:true #:false
   #:definline
   #:neq #:neql #:nequal #:nequalp
   #:if-let #:when-let
   #:iff #:implies
   #:kwote #:unquote
   #:rrest #:rrrest #:rrrrest
   #:mklist #:firstn #:consn #:leafp
   #:naturalp #:ratiop
   #:carc #:cdrc #:caarcc #:cadrcc #:cdarcc #:cddrcc
   #:lcons
   #:cons-unless-nil #:push-unless-nil #:pushnew-unless-nil
   #:dotails #:dopairs
   #:choose
   #:integers-between #:ints
   #:length= #:length< #:length<= #:length> #:length>=
   #:acons+ #:alist-notany-plusp #:alist-notany-minusp
   #:cons-count
   #:char-invert-case
   #:to-string
   #:find-or-make-package
   #:percentage
   #:print-current-time
   #:leap-year-p #:days-per-month #:month-number
   #:print-args
   #:define-plist-slot-accessor
   #:*print-pretty2*
   #:with-standard-io-syntax2
   #:quit

   ;; defined in collectors.lisp
   #:make-collector #:collector-value #:collect-item #:collect-list
   #:make-queue #:queue-empty-p #:enqueue #:dequeue
   #:collect #:ncollect

   ;; defined in map-file.lisp
   #:mapnconc-stream-forms #:mapnconc-stream-lines
   #:mapnconc-file-forms #:mapnconc-file-lines
   #:read-file #:read-file-lines #:read-file-to-string

   ;; defined in clocks.lisp
   #:initialize-clocks #:print-clocks
   #:with-clock-on #:with-clock-off
   #:total-run-time
   #:print-incremental-time-used

   ;; defined in counters.lisp
   #:make-counter
   #:increment-counter #:decrement-counter
   #:counter-value #:counter-values
   #:princf

   ;; defined in pattern-match.lisp
   #:pattern-match

   ;; defined in topological-sort.lisp
   #:topological-sort* #:topological-sort

   ;; undefined symbols used by snark
   #:implied-by #:xor #:nand #:nor
   #:forall #:exists
   #:$$cons #:$$list #:$$list*
   ))

(defpackage :snark-deque
  (:use :common-lisp :snark-lisp)
  (:export
   #:make-deque
   #:deque?
   #:deque-empty?
   #:deque-first #:deque-rest #:deque-pop-first #:deque-add-first #:deque-push-first
   #:deque-last #:deque-butlast #:deque-pop-last #:deque-add-last #:deque-push-last
   #:deque-length
   #:deque-delete
   #:deque-delete-if
   #:mapnconc-deque
   ))

(defpackage :snark-sparse-array
  (:use :common-lisp :snark-lisp)
  (:export
   #:sparef
   #:sparse-vector #:make-sparse-vector #:sparse-vector-p
   #:sparse-vector-boolean #:sparse-vector-default-value
   #:sparse-vector-count
   #:map-sparse-vector #:map-sparse-vector-with-indexes #:map-sparse-vector-indexes-only
   #:with-sparse-vector-iterator
   #:first-sparef #:last-sparef #:pop-first-sparef #:pop-last-sparef
   #:copy-sparse-vector #:spacons
   #:sparse-matrix #:make-sparse-matrix #:sparse-matrix-p
   #:sparse-matrix-boolean #:sparse-matrix-default-value
   #:sparse-matrix-count
   #:sparse-matrix-row #:sparse-matrix-column #:sparse-matrix-rows #:sparse-matrix-columns
   #:map-sparse-matrix #:map-sparse-matrix-with-indexes #:map-sparse-matrix-indexes-only

   #:sparse-vector-expression-p
   #:map-sparse-vector-expression
   #:map-sparse-vector-expression-with-indexes
   #:map-sparse-vector-expression-indexes-only
   #:optimize-sparse-vector-expression
   #:uniond
   ))

(defpackage :snark-numbering
  (:use :common-lisp :snark-lisp :snark-sparse-array)
  (:export
   #:nonce
   #:initialize-numberings #:make-numbering
   #:*standard-eql-numbering* #:*standard-equal-numbering*
   ))

(defpackage :snark-agenda
  (:use :common-lisp :snark-lisp :snark-deque :snark-sparse-array)
  (:export
   #:make-agenda
   #:agenda-name #:agenda-length
   #:agenda-insert #:agenda-delete
   #:agenda-first #:pop-agenda #:mapnconc-agenda #:agenda-delete-if
   #:limit-agenda-length
   #:print-agenda
   #:*agenda*
   ))

(defpackage :snark-infix-reader
  (:use :common-lisp :snark-lisp)
  (:export
   #:initialize-operator-syntax #:declare-operator-syntax
   #:tokenize #:read-infix-term
   #:--))

(defpackage :snark-feature
  (:use :common-lisp :snark-lisp)
  (:export
   #:initialize-features
   #:make-feature #:declare-feature
   #:declare-features-incompatible
   #:feature? #:feature-parent
   #:the-feature
   #:delete-feature #:feature-live?
   #:feature-union #:feature-subsumes?
   #:print-feature-tree
   ))

(defpackage :snark-dpll
  (:use :common-lisp :snark-lisp)
  (:export
   #:dp-prover #:dp-version
   #:dp-tracing #:dp-tracing-state #:dp-tracing-models #:dp-tracing-choices
   #:dp-satisfiable-p #:dp-satisfiable-file-p #:make-dp-clause-set
   #:dp-insert #:dp-insert-sorted #:dp-insert-wff #:dp-insert-file
   #:dp-count #:dp-clauses #:dp-output-clauses-to-file #:wff-clauses
   #:dp-horn-clause-set-p
   #:checkpoint-dp-clause-set #:restore-dp-clause-set #:uncheckpoint-dp-clause-set
   #:choose-an-atom-of-a-shortest-clause
   #:choose-an-atom-of-a-shortest-clause-randomly
   #:choose-an-atom-of-a-shortest-clause-with-most-occurrences
   #:choose-an-atom-of-a-shortest-clause-with-most-occurrences-randomly
   #:choose-an-atom-of-a-shortest-positive-clause
   #:choose-an-atom-of-a-shortest-positive-clause-randomly
   #:choose-an-atom-of-a-shortest-positive-clause-with-most-occurrences
   #:choose-an-atom-of-a-shortest-positive-clause-with-most-occurrences-randomly
   #:lookahead-true #:lookahead-false
   #:lookahead-true-false #:lookahead-false-true
   ))
