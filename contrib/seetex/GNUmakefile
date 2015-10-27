# $Id: GNUmakefile 19708 2010-10-29 18:04:21Z d3y133 $

OBJ = seetex.o

LIBRARY = libseetex.a
LIB_TARGETS = seetex $(OBJ)

include ../../src/config/makefile.h

target:
	$(MAKE) seetex
	rm -f sample.tex
	seetex sample.th sample.F sample.tex

include ../../src/config/makelib.h

install:
	$(MAKE) seetex
	mv seetex $(BINDIR)/seetex
