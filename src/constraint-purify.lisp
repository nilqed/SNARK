;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: constraint-purify.lisp
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations
;;; under the License.
;;;
;;; The Original Code is SNARK.
;;; The Initial Developer of the Original Code is SRI International.
;;; Portions created by the Initial Developer are Copyright (C) 1981-2011.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(defun constraint-purify-wff (wff)
  (let ((ucp (use-constraint-purification?)))
    ;; if ucp = 1, add equality constraints to wff instead of constraint-alist
    ;; if ucp = 2, add all constraints to wff instead of constraint-alist
    (let ((vars (nontheory-variables wff))
          (constraint-alist-additions nil)
          (cache nil))
      (labels
        ((constraint-purify-atom (atom polarity)
           (dereference
            atom nil
            :if-constant atom
            :if-variable (not-wff-error atom)
            :if-compound-cons (not-wff-error atom)
            :if-compound-appl (let* ((head (heada atom))
                                     (args (argsa atom))
                                     (theory2 (function-constraint-theory head))
                                     (args* (constraint-purify-terms args theory2))
                                     (atom* (if (eq args args*) atom (make-compound* head args*))))
                                (if (null theory2)
                                    atom*
                                    (ecase polarity
                                      (:pos (add-constraint atom* theory2) false)
                                      (:neg (add-constraint (negate atom*) theory2) true))))))
         
         (constraint-purify-term (term theory1)
           (let ((theory2 nil) (dont-abstract nil))
             (dereference
              term nil
              :if-variable (setf dont-abstract (not (member term vars)))
              :if-constant (setf dont-abstract (constant-constructor term))
              :if-compound (let* ((head (head term))
                                  (args (args term))
                                  (args* (constraint-purify-terms
                                          args
                                          (if (setf dont-abstract (function-constructor head))
                                              theory1		;constructor symbols are transparent wrt theory
                                              (setf theory2 (function-constraint-theory head))))))
                             (unless (eq args args*)
                               (setf term (make-compound* head args*)))))
             (cond
              ((or dont-abstract (eq theory1 theory2))
               term)
              (theory1
               (variable-for term (or theory2 'equality)))
              (t
               (variable-for (variable-for term (or theory2 'equality)) 'equality)))))
         
         (constraint-purify-terms (terms theory)
           (lcons (constraint-purify-term (first terms) theory)
                  (constraint-purify-terms (rest terms) theory)
                  terms))
         
         (add-constraint (lit theory)
           (setf constraint-alist-additions (disjoin-alist1 theory lit constraint-alist-additions)))
         
         (variable-for (term theory)
           ;; create a variable to use in place of term and store ($$eq var term) in theory constraint
           (or (cdr (assoc term cache :test #'equal-p))
               (let ((eq (input-relation-symbol (intern (to-string :$$eq_ theory) :snark) 2))
                     (var (make-variable (term-sort term))))
                 (add-constraint (make-compound *not* (make-compound eq var term)) theory)
                 (setf cache (acons term var cache))
                 var))))
        
        (let ((wff* (map-atoms-in-wff-and-compose-result #'constraint-purify-atom wff)))
          (if constraint-alist-additions
              (values
               (disjoin* (cons wff* (mapcan #'(lambda (p) (if (or (eql 2 ucp) (and (eql 1 ucp) (eq 'equality (car p)))) (list (cdr p)) nil)) constraint-alist-additions)))
               (mapcan #'(lambda (p) (if (or (eql 2 ucp) (and (eql 1 ucp) (eq 'equality (car p)))) nil (list p))) constraint-alist-additions))
              wff))))))

(defun constraint-purified-p (x &optional subst)
  (let ((variable-theory-alist nil))
    (labels
      ((cpp (x theory1)
         (dereference
          x subst
          :if-variable (let ((p (assoc x variable-theory-alist)))
                         (cond
                          ((null p)
                           (unless (eq none theory1)
                             (setf variable-theory-alist (acons x theory1 variable-theory-alist)))
                           t)
                          (t
                           (or (eq none theory1) (eq theory1 (cdr p))))))                           
          :if-constant (or (eq none theory1) (null theory1) (constant-constructor x))
          :if-compound (let* ((head (head x))
                              (theory2 (cond
                                        ((function-logical-symbol-p head)
                                         none)
                                        ((function-constructor head)
                                         theory1)			;constructor symbols are transparent wrt theory
                                        ((eq 'equality (function-constraint-theory head))
                                         none)
                                        (t
                                         (function-constraint-theory head)))))
                         (and (or (eq none theory1) (eq theory1 theory2))
                              (dolist (arg (args x) t)
                                (unless (cpp arg theory2)
                                  (return nil))))))))
      (cpp x none))))

(defun constraint-purified-row-p (row)
  ;; ignore answer and ordering-constraint
  (constraint-purified-p (cons (row-wff row) (remove-if #'(lambda (x) (eq (car x) 'ordering)) (row-constraints row)))))

(defun variable-occurs-purely-p (var x &optional subst atom)
  (let ((theory none))
    (labels
      ((vop (x th)
         (dereference
          x subst
          :if-variable (when (eq var x)
                         (unless (eq theory th)
                           (if (eq none theory)
                               (setf theory th)
                               (return-from variable-occurs-purely-p nil))))
          :if-compound-cons (progn (vop (carc x) th) (vop (cdrc x) th))
          :if-compound-appl (unless (eq atom x)			;ignore atom being resolved away
                              (let ((th (if (function-constructor (heada x)) th (function-constraint-theory (heada x)))))
                                (dolist (arg (args x))
                                  (vop arg th)))))))
      (vop x nil))
    t))

(defun variable-occurs-purely-in-row-p (var row &optional atom)
  (variable-occurs-purely-p var (list* (row-wff row) (row-answer row) (remove-if #'(lambda (x) (eq (car x) 'ordering)) (row-constraints row))) nil atom))

;;; constraint-purify.lisp EOF
