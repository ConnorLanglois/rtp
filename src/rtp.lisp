;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the rtp main file.
;;;;;
;;;;; Created: 3/10/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rtp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: run-simulator
;;; Arguments: none
;;; Returns: nil
;;; Description:
;;; 	Runs the simulations.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-simulator ()
	(let ((base (parse (make-instance 'parser :path (format nil "../bases/~a" (prompt-line "Knowledge base: ")))))
			(theorem (prompt "Theorem: ")))
		(format t "~%Knowledge base:~%~{    ~a~%~}~%" base)
		(run-simulation :base base :theorem theorem :show-world-p t)
		(run-simulation :base base :theorem theorem :agent-program 'a*-agent-program :show-world-p t)))
