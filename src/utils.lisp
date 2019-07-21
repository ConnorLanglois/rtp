;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the utils functions.
;;;;;
;;;;; Created: 3/10/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rtp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: prompt
;;; Arguments:
;;; 	- prompt: the text to display
;;; Returns: the input from the user
;;; Description:
;;; 	Displays prompt and reads input from the user.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prompt (prompt)
	(format t prompt)
	(finish-output)
	(read))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: prompt-line
;;; Arguments:
;;; 	- prompt: the text to display
;;; Returns: the input from the user
;;; Description:
;;; 	Displays prompt and reads input as text from the user.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prompt-line (prompt)
	(format t prompt)
	(finish-output)
	(read-line))
