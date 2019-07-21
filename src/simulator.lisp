;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the simulator class.
;;;;;
;;;;; Created: 3/10/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rtp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: simulator
;;; Parent class: simulator
;;; Slots: none
;;; Description:
;;; 	Represents the main simulator.
;;; 	Contains main loop of program, runs agents, world, and performance
;;; 	measures.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simulator (agent:simulator) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: initialize-instance
;;; Arguments:
;;; 	- simulator: a simulator
;;; 	- base: a knowledge base
;;; 	- agent (key agent): an agent symbol
;;; 	- agent-program (key): an agent-program symbol
;;; 	- performance-measure (key search-performance-measure): a performance measure symbol
;;; 	- show-world-p (key t): whether to show the world
;;; Returns: the simulator
;;; Description:
;;; 	Initializes the simulator.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent:initialize-instance :around ((simulator simulator) &key
																base
																theorem
																(agent 'agent:agent)
																(agent-program 'dfs-agent-program)
																(performance-measure 'agent:search-performance-measure)
																(show-world-p t))
	(let* ((agent-state (make-agent-state :axiom (negate theorem)))
			(agent-meta (agent:make-agent-meta :state agent-state))
			(agent-program (make-instance agent-program :agent-meta agent-meta :base base))
			(agent (make-instance agent :agent-program agent-program))
			(world (make-instance 'world :base base :agent agent :agent-meta agent-meta))
			(performance-measure (make-instance performance-measure))
			(agent-sim (agent:make-agent-sim :test #'nil-test :performance-measures (list performance-measure) :path (list agent-state))))
		(call-next-method simulator :world world :agent agent :agent-sim agent-sim :show-world-p show-world-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: simulate-
;;; Arguments:
;;; 	- simulator: a simulator
;;; 	-  the agent
;;; 	- agent-sim: the agent-sim
;;; 	- agent-meta: the agent-meta
;;; Returns: nil
;;; Description:
;;; 	Shows world, gets senses from world, gives agent senses,
;;; 	gets action from agent, and gives action to world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent:simulate- ((simulator simulator) agent agent-sim agent-meta)
	(with-accessors ((world agent:world)) simulator
		(agent:sensors agent)
		(agent:act world (agent:action agent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: run-simulation
;;; Arguments:
;;; 	- rest (rest): the arguments of the simulator
;;; Returns: nil
;;; Description:
;;; 	Makes the simulator with the arguments and simulates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/10/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-simulation (&rest rest)
	(apply #'agent:run-simulation- 'simulator rest))
