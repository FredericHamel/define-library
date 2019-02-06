
all: define-library.o1

clean:
	rm -f define-library.o1

.SUFFIXES:
.SUFFIXES: .o1 .scm .sld

.scm.o1:
	@echo "Compiling $< => $@"
	@gsc-script -:d- \
		-module-name _dl \
		define-library.scm

.PHONY:
.PHONY: all clean

