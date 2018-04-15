SCM_SRC := *.scm

.Phony: run edit

run:
	rlwrap mit-scheme --load $(SCM_SRC)

edit:
	nvim -p $(SCM_SRC)
