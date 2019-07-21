;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the predicate utils functions.
;;;;;
;;;;; Created: 3/10/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rtp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: resolve
;;; Arguments:
;;; 	- resolve-axiom: the axiom from the knowledge base
;;; 	- axiom: the leftover/current axiom
;;; Returns: whether the two axioms resolve and the disjunction 
;;; Description:
;;; 	Resolves two axioms.
;;; 	Results in new axiom, a list of predicates from the two input axioms
;;; 	without the predicates that resolve.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve (resolve-axiom axiom)
	(let* ((resolve-terms (to-terms (un-disjunct resolve-axiom)))
			(terms (to-terms (un-disjunct axiom)))
			(new-terms (set-exclusive-or resolve-terms terms :test #'resolve-p))
			(p (< (length new-terms) (length (append resolve-terms terms)))))
		(values p (apply #'disjunct new-terms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: resolve-p
;;; Arguments:
;;; 	- resolve-term: the term from the knowledge base
;;; 	- term: the term from the leftover/current axiom
;;; Returns: whether the two terms resolve
;;; Description:
;;; 	Checks if the two terms resolve together.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-p (resolve-term term)
	(equal (negate term) resolve-term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: disjunct
;;; Arguments:
;;; 	- terms (rest): a list of terms/predicates
;;; Returns: an axiom, a list of predicates, with "or" at the front if there is
;;; 	more than one predicate
;;; Description:
;;; 	Disjuncts a list of predicates.
;;; 	If there is only one predicate, the resulting list is just that predicate.
;;; 	Otherwise, it is a list of the predicates with "or" at the front.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun disjunct (&rest terms)
	(let ((terms (un-idempotent terms)))
		(if (> (length terms) 1)
			`(or ,@terms)
			(first terms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: un-disjunct
;;; Arguments:
;;; 	- axiom: an axiom
;;; Returns: an axiom, a list of predicates, without "or" at the front
;;; Description:
;;; 	Removes "or" from an axiom if it is present.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun un-disjunct (axiom)
	(if (eq (first axiom) 'or)
		(rest axiom)
		axiom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: negate
;;; Arguments:
;;; 	- axiom: an axiom
;;; Returns: an axiom, a list of predicates, with "not" at the front
;;; Description:
;;; 	Negates an axiom.
;;; 	Ensures that double negations are removed.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun negate (axiom)
	(un-double-negate `(not ,axiom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: un-double-negate
;;; Arguments:
;;; 	- axiom: an axiom
;;; Returns: an axiom, a list of predicates, without two nested "not"s
;;; Description:
;;; 	Removes double negation from an axiom.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun un-double-negate (axiom)
	(if (and (typep (second axiom) 'list)
				(eq (first axiom) 'not)
				(eq (first (second axiom)) 'not))
		(first (rest (second axiom)))
		axiom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: un-idempotent
;;; Arguments:
;;; 	- axiom: an axiom
;;; Returns: an axiom, a list of predicates, without any duplicate predicates
;;; Description:
;;; 	Removes duplicate predicates from an axiom.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun un-idempotent (axiom)
	(remove-duplicates axiom :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: to-terms
;;; Arguments:
;;; 	- terms: a list of terms/predicates
;;; Returns: a nested list of terms
;;; Description:
;;; 	Creates nested list of terms if the first element of terms is a list.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-terms (terms)
	(if (typep (first terms) 'list)
		terms
		(list terms)))
