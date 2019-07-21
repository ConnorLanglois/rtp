;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the utility-based-agent-program class.
;;;;;
;;;;; Created: 3/10/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rtp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: utility-based-agent-program
;;; Parent class: agent:utility-based-agent-program
;;; Slots:
;;; 	- test: the function that tests for goal state
;;; 	- adder: the function that gives step cost
;;; 	- heuristic: the function that gives heuristic cost
;;; 	- generator: the function that generates new states
;;; Description:
;;; 	Represents a Utility-Based Agent Program.
;;; 	Performs graph search to find path to goal, i.e. where there is nil
;;; 	as the left over axiom.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass utility-based-agent-program (agent:utility-based-agent-program)
	((agent:test :initform #'nil-test)
	(agent:adder :initform #'1+)
	(agent:heuristic :initform #'unit-preference)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: initialize-instance
;;; Arguments:
;;; 	- agent-program: a utility-based-agent-program
;;; 	- base: the knowledge base
;;; Returns: the utility-based-agent-program
;;; Description:
;;; 	Initializes the utility-based-agent-program.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/11/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((agent-program utility-based-agent-program) &rest rest &key base)
	(apply #'call-next-method agent-program :generator (agent:curry #'resolves base) rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action--
;;; Arguments:
;;; 	- agent-program: a utility-based-agent-program
;;; 	- agent-state: the agent-state
;;; Returns: an action
;;; Description:
;;; 	Makes an action from the state.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent:action-- ((agent-program utility-based-agent-program) agent-state)
	(make-resolve-action :axiom (agent-state-resolve-axiom agent-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: nil-test
;;; Arguments:
;;; 	- agent-state: the agent-state
;;; Returns: whether there is ni as the left over axiom
;;; Description:
;;; 	Checks if there is nil as the left over axiom.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nil-test (agent-state)
	(null (agent-state-axiom agent-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: resolves
;;; Arguments:
;;; 	- base: the knowledge base
;;; 	- agent-state: the agent-state
;;; Returns: the new states where an axiom was resolved using Set of Support
;;; Description:
;;; 	Gets the next states where the axiom can be resolved in the knowledge
;;; 	base of the state using the Set of Support strategy.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolves (base agent-state)
	(with-accessors ((axiom agent-state-axiom)) agent-state
		(loop for resolve-axiom in base
				for resolve-state = (multiple-value-list (resolve-state resolve-axiom axiom))
				for p = (first resolve-state)
				for agent-state = (second resolve-state)
				if p
			collect agent-state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: resolve-state
;;; Arguments:
;;; 	- resolve-axiom: the axiom from knowledge base to resolve with
;;; 	- axiom: the leftover/current axiom to resolve
;;; Returns: whether the two axioms resolve and agent state resulting 
;;; Description:
;;; 	Attempts to resolve the axiom with the resolve-axiom, resulting in a new
;;; 	agent state.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-state (resolve-axiom axiom)
	(multiple-value-bind (unify-p bindings) (unify-axioms resolve-axiom axiom)
		(multiple-value-bind (p new-axiom) (resolve (instantiate resolve-axiom bindings) (instantiate axiom bindings :if-unbound :last))
			(values p (make-agent-state :resolve-axiom resolve-axiom :axiom new-axiom)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: unit-preference
;;; Arguments:
;;; 	- agent-state: the state of the agent
;;; Returns: length of an axiom, disregarding "or"
;;; Description:
;;; 	Represents the Unit Preference heuristic strategy.
;;; 	Gives the length of the agent state's axiom, disregarding "or" if present.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unit-preference (agent-state)
	(length (un-disjunct (agent-state-axiom agent-state))))
