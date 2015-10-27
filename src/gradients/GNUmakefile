# $Id: GNUmakefile 26854 2015-02-19 06:23:09Z jhammond $

 OBJ = gradients.o grad_force.o grad1.o scf_gradient.o  \
       grad_dens.o grad_inp.o ga_reorder.o
 OBJ_OPTIMIZE = grad2.o grad_getdens.o

   USES_BLAS = grad2.F ga_reorder.F grad_dens.F

   LIBRARY = libgradients.a

include ../config/makefile.h
include ../config/makelib.h

