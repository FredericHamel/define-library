
all: define-library.o1

clean:
	rm -f define-library.o1

.SUFFIXES:
.SUFFIXES: .o1 .scm .sld

define-library.o1: define-library.scm

.scm.o1:
	@echo "Compiling $< => $@"
	@gsc-script -prelude '(declare(block))' -o $@ $<

.PHONY:
.PHONY: all clean

