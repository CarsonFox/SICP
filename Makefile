SCM_SRC := *.scm

run:
	rlwrap mit-scheme --load $(SCM_SRC)
