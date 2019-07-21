#|
	This file is a part of rtp project.
	Copyright (c) 2019 Connor Langlois (connor.langlois@maine.edu)
|#

#|
	Author: Connor Langlois (connor.langlois@maine.edu)
|#

(defsystem "rtp"
	:version "0.1.0"
	:author "Connor Langlois"
	:license ""
	:depends-on ("agent")
	:components ((:module "src"
					:components
						((:file "package")
						(:file "agent-state")
						(:file "astar-agent-program")
						(:file "dfs-agent-program")
						(:file "parser")
						(:file "unify")
						(:file "predicate")
						(:file "resolve-action")
						(:file "rtp")
						(:file "simulator")
						(:file "utility-based-agent-program")
						(:file "utils")
						(:file "world")))
				(:module "bases"
					:components
						((:static-file "bases/marcus"))))
	:description ""
	:long-description
		#.(read-file-string
			(subpathname *load-pathname* "../README.md"))
	:in-order-to ((test-op (test-op "rtp-test"))))
