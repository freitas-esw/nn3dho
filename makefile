SHELL = /bin/sh

# Path to SPRNG library
# SPRDIR = /home/des01/vitiello/viniciuz/sw/sprng5/install                # masternode 
# SPRDIR = ${HOME}/Documents/Programs/my_lib_f90/gitlab_sprng/lib/sprng5  # aslan
# SPRDIR = ${HOME}/Documents/git/tutorial_sprng/lib/sprng5/               # lucy 
#                                                                         #
SPRDIR = ${HOME}/Documents/git/tutorial_sprng/lib/sprng5/

# Path to current directory
#
CURDIR = $(shell pwd)

# Compiler variable
#
FC = gfortran

# Compiler flags
#
FMOD      = -J
SPRNGFLAG = -DPOINTER_SIZE=8 -DLONG64=long -DINTEGER_STAR_8
#
#
## Debugging
##
## Extra flagging
F1     = -ffree-line-length-0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow 
F2     = -finit-real=nan -Warray-temporaries -Wconversion -Wconversion-extra 
FEXTRA = $(F1) $(F2) 
COARR  = -fcoarray=single 
GPROF  = -pg 
FMAIN  = -g -cpp -Wall -Wextra -fimplicit-none -fbacktrace -fbounds-check
## FMAIN  = -g -cpp -fimplicit-none -fbacktrace -fbounds-check
##
FFLAG = $(FMAIN) $(COARR) $(GPROF) 
## FFLAG = $(FMAIN) $(COARR) $(GPROF) $(FEXTRA)
##
## Optimization flags
 FFLAG = -cpp -O3 -fcoarray=single
##
#
# Directories and libraries flags
DIRFLAG = -I$(SPRDIR)/include -L$(SPRDIR)/lib
LIBS    = -lsprng -lstdc++
#

# Program subdirectories
EXDIR  = examples
SRCDIR = src
OBJDIR = include
BINDIR = bin

# Target and objects
TARGET = $(BINDIR)/libqmc.a
OBJFIL = mod_cte.o mod_ios.o mod_global.o mod_rng.o \
	 mod_kinds.o mod_activation.o mod_parallel.o mod_layer.o mod_network.o \
	 3dho_nn.o

VPATH  = $(SRCDIR):$(OBJDIR)

.SUFFIXES: .f90 .o
.f90.o: ; @mkdir -p $(BINDIR) $(OBJDIR) 
	$(FC) -c $(SPRNGFLAG) $(DIRFLAG) $(FFLAG) $(FMOD) $(OBJDIR) -o $(OBJDIR)/$@ $< $(LIBS) 

%.o: %.mod

$(TARGET): $(OBJFIL)
	ar rcs $(TARGET) $(addprefix $(OBJDIR)/, $(OBJFIL))
	cd examples; make

.PHONEY: clean
clean:
	@rm -rf *~ $(BINDIR) $(OBJDIR)  $(SRCDIR)/*~ *.out *.err *.log *.ipo
	rm -rf $(EXDIR)/*.run $(EXDIR)/*.out

.PHONEY: tidy
tidy:
	rm -rf $(EXDIR)/*.run $(EXDIR)/*.out

.PHONY: examples
examples:
	cd examples; make
