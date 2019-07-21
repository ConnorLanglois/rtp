#|
	This file is a part of agent project.
	Copyright (c) 2019 Connor Langlois (connor.langlois@maine.edu)
|#

#|
	Author: Connor Langlois (connor.langlois@maine.edu)
|#

(defsystem "agent"
	:version "0.1.0"
	:author "Connor Langlois"
	:license ""
	:depends-on ()
	:components ((:module "src"
					:components
					((:file "package")
						(:file "action")
						(:file "agent-meta")
						(:file "agent-program")
						(:file "agent-sim")
						(:file "agent")
						(:file "astar-agent-program")
						(:file "bfs-agent-program")
						(:file "coordinate")
						(:file "dfs-agent-program")
						(:file "goal-based-agent-program")
						(:file "graph")
						(:file "grid-world")
						(:file "model-based-agent-program")
						(:file "node")
						(:file "nop-action")
						(:file "performance-measure")
						(:file "search-performance-measure")
						(:file "senses")
						(:file "simple-reflex-agent-program")
						(:file "simulator")
						(:file "utility-based-agent-program")
						(:file "utils")
						(:file "world"))))
	:description ""
	:long-description
		#.(read-file-string
				(subpathname *load-pathname* "../README.md"))
	:in-order-to ((test-op (test-op "agent-test"))))
