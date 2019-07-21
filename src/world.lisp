;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the world class.
;;;;;
;;;;; Created: 3/10/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rtp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: world
;;; Parent class: agent:world
;;; Slots: none
;;; Description:
;;; 	Represents the grid-based world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass world (agent:world)
	((base :reader base :initarg :base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: act
;;; Arguments:
;;; 	- world: the world
;;; 	- resolve: a resolve-action
;;; Returns: the current state of agent
;;; Description:
;;; 	Resolves current axiom of agent state according to resolve action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent:act ((world world) (resolve-action resolve-action) &key &allow-other-keys)
	(with-accessors ((resolve-axiom resolve-action-axiom)) resolve-action
		(with-accessors ((agent-state agent:agent-meta-state)) (agent:agent-meta world)
			(with-accessors ((axiom agent-state-axiom)) agent-state
				(format t "Resolved: ~a~%~%" resolve-axiom)
				(multiple-value-bind (p new-agent-state) (resolve-state resolve-axiom axiom)
					(setf agent-state new-agent-state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: show
;;; Arguments:
;;; 	- world: the world
;;; Returns: nil
;;; Description:
;;; 	Shows the world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent:show ((world world) &rest rest &key &allow-other-keys)
	(with-accessors ((axiom agent-state-axiom)) (agent:agent-meta-state (agent:agent-meta world))
		(format t "Axiom:    ~a~%~%" axiom)))
