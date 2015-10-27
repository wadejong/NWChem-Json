#
# $Id: GNUmakefile 27164 2015-05-26 21:04:04Z d3y133 $
#

LIBRARY = libnwpython.a

OBJ = python_input.o task_python.o nwchem_wrap.o nw_inp_from_string.o 

ifndef PYTHONHOME
error:
	@echo Must define PYTHONHOME and PYTHONVERSION to build with Python
	@exit 1
endif

# Under WIN32 only need to define PYTHONHOME
ifndef PYTHONVERSION
error1:
	@echo Must define PYTHONHOME and PYTHONVERSION to build with Python
	@exit 1
endif


LIB_INCLUDES = -I$(PYTHONHOME)/include/python$(PYTHONVERSION)$(PYTHONABIFLAGS) -I$(PYTHONHOME)/include -I$(PYTHONHOME)/Include -I$(PYTHONHOME)


include ../config/makefile.h
include ../config/makelib.h


