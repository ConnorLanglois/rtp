;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the agent package.
;;;;;
;;;;; Created: 3/10/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage agent
	(:use :cl)
	(:export
		:act
		:action
		:action--
		:adder
		:agent
		:agent-meta
		:agent-meta-state
		:agent-sim
		:a*-agent-program
		:curry
		:dfs-agent-program
		:generator
		:heuristic
		:initialize-instance
		:make-agent-meta
		:make-agent-sim
		:make-generator
		:run-simulation-
		:search-performance-measure
		:sensors
		:show
		:simulate-
		:simulator
		:utility-based-agent-program
		:test
		:world))
