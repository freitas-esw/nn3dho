SHELL = /bin/sh

# Path to SPRNG library
# SPRDIR = /home/des01/vitiello/viniciuz/sw/sprng5/install                # masternode 
# SPRDIR = ${HOME}/Documents/Programs/my_lib_f90/gitlab_sprng/lib/sprng5  # aslan
# SPRDIR = ${HOME}/Documents/git/tutorial_sprng/lib/sprng5/               # lucy 
#                                                                         #
SPRDIR = ${HOME}/Documents/git/tutorial_sprng/lib/sprng5/
#

# Path to current directory
CURDIR = $(shell pwd)
#

# Compiler variable
FC = gfortran
#

# Compiler flags
FMOD      = -J
SPRNGFLAG = -DPOINTER_SIZE=8 -DLONG64=long -DINTEGER_STAR_8
#

## Flags
#
F1 = -ffree-line-length-0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow 
F2 = -finit-real=nan -Warray-temporaries -Wconversion -Wconversion-extra 
FG = -pg  # gprof flag

FM = -g -cpp -fimplicit-none -fbacktrace -fbounds-check -fcoarray=single
FW = $(FM) -Wall -Wextra 
FO = -cpp -O3 -fcoarray=single

#
# Use simple flags
  FFLAG = $(FM)
#
# Use warning flags
# FFLAG = $(FW)
#
# Use warning and gprof flags
# FFLAG = $(FG) $(FW) 
#
# Use extra warning, degub and gprof flags
# FFLAG = $(FG) $(FM) $(F1) $(F2)
#
# Use optimization flags only
# FFLAG = $(FO)
#
#

# Directories and libraries flags
DIRFLAG = -I$(SPRDIR)/include -L$(SPRDIR)/lib
LIBS    = -lsprng -lstdc++
#

# Program subdirectories
SRCDIR = src
OBJDIR = include
BINDIR = bin

# Target and objects
TARGET = $(BINDIR)/nn3dho.run

OBJFIL = mod_cte.o mod_ios.o mod_global.o mod_rng.o \
	 mod_kinds.o mod_activation.o mod_parallel.o mod_layer.o mod_network.o \
	 3dho_nn.o

VPATH  = $(SRCDIR):$(OBJDIR)

$(TARGET): $(SRCDIR)/main.f90 $(OBJFIL)
	$(FC) $(SPRNGFLAG) $(DIRFLAG) $(FFLAG) $(FMOD)$(OBJDIR) -o $@ $< $(patsubst %.o, $(OBJDIR)/%.o, $(OBJFIL)) $(LIBS)

%.o : %.f90 ; @mkdir -p $(BINDIR) $(OBJDIR) 
	$(FC) -c $(SPRNGFLAG) $(DIRFLAG) $(FFLAG) $(FMOD) $(OBJDIR) -o $(OBJDIR)/$@ $< $(LIBS) 

.PHONEY: clean
clean:
	@rm -rf *~ $(BINDIR) $(OBJDIR)  $(SRCDIR)/*~ *.out *.err *.log *.ipo

