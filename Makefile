SCM_SRC := $(sort $(wildcard *.scm))
LOAD_PATT := (load "filename")
LOAD_STR := $(foreach src,$(SCM_SRC),$(subst filename,$(src),$(LOAD_PATT)))

.Phony: run edit

run: run.h
	@clear
	racket -i -p neil/sicp -l xrepl -f run.h

run.h: $(SCM_SRC)
	@echo '$(LOAD_STR)' > run.h
	@echo '(display "Load Complete")' >> run.h

edit:
	@nvim -p $(SCM_SRC)
