;;; Thu 20 Feb 23:40:30 CET 2025 -- kfp@omega:~$ SBCL 2.2.9.debian

(ql:quickload :snark)
(in-package :snark-user)
(initialize)
(use-resolution t)
;(use-resolution nil)
;(use-hyperresolution t)


;; we use quantifiers here (instead of ?-prefixed variables).

;;; Axioms
(defvar ax-grp-1 '(forall x (= (* e x) x)))
(defvar ax-grp-2 '(forall x (= (* (inv x) x) e)))
(defvar ax-grp-3 '(forall (x y z) (= (* x (* y z)) (* (* x y) z))))

(assert ax-grp-1 :name 'left-identity)
(assert ax-grp-2 :name 'left-inverse)
(assert ax-grp-3 :name 'associative)

;;; Hypotheses
(defvar left-cancel '(forall (x y z) (implies (= (* x y) (* x z)) (= y z)))) 
(defvar right-id '(forall x (= (* x e) x)))
(defvar right-inv-unique '(forall (x y) (implies (= (* x y) e) (= y (inv x)))))
(defvar inv-involution '(forall x (= (inv (inv x)) x)))
(defvar inv-prod '(forall (x y) (= (inv (* x y)) (* (inv y) (inv x)))))


;;; Proves (requiring: paramodulation t)
;;; Then no axioms for equality need be provided by the user.
(use-paramodulation t)
(defvar p1 (prove left-cancel         :name 'p-left-cancel))
(defvar p2 (prove right-id            :name 'p-right-id))
(defvar p3 (prove right-inv-unique    :name 'p-right-inverse-unique))
(defvar p4 (prove inv-involution      :name 'p-inverse-involution))
(defvar p5 (prove inv-prod            :name 'p-inverse-product))

(format t "~%RESULTS: ~A~%" (list p1 p2 p3 p4 p5))

;;;;
;;;; https://github.com/nilqed/SNARK/blob/master/src/infix-reader.lisp
;;;;
(snark-infix-reader::read-infix-term "p(x,y,z)")
;;; (|p| |x| |y| |z|)

(snark-infix-reader::read-infix-term "r(x,?,?1,X,?X,??X)")
;;; (|r| |x| ? ?1 ?X ??X ???X)

(snark-infix-reader::declare-operator-syntax "<=>" :xfy 505)
;#S(SNARK-INFIX-READER::OPERATOR
;   :INPUT-STRING "<=>"
;   :TYPE :XFY
;   :PRECEDENCE 505
;   :OUTPUT-SYMBOL <=>)

(snark-infix-reader::read-infix-term "x <=> y")
; (<=> |x| |y|)






