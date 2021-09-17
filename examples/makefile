
FC = gfortran

# Path to SPRNG library
# SPRDIR = /home/des01/vitiello/viniciuz/sw/sprng5/install                # masternode 
# SPRDIR = ${HOME}/Documents/Programs/my_lib_f90/gitlab_sprng/lib/sprng5  # aslan
# SPRDIR = ${HOME}/Documents/git/tutorial_sprng/lib/sprng5/               # lucy 
#                                                                         #
SPRDIR = ${HOME}/Documents/git/tutorial_sprng/lib/sprng5/

# Path to current directory
#
CURDIR = $(shell pwd)

# Compiler flags
SPRNGFLAG = -DPOINTER_SIZE=8 -DLONG64=long -DINTEGER_STAR_8
FMOD  = -J
# Debugging flags
FFLAG = -pg -g -cpp -Wall -Wextra -fimplicit-none -fbacktrace -fbounds-check
# Optimization flags
 FFLAG = -cpp -O3

DIRRNG = -I$(SPRDIR)/include
DIRQMC = -I$(CURDIR)/../include

LIBRNG = $(DIRRNG) -L$(SPRDIR)/lib
LIBQMC = $(DIRQMC) -L$(CURDIR)/../bin

LIBS = $(LIBQMC) -lqmc $(LIBRNG) -lsprng -lstdc++

FILES = $(wildcard *.f90)
TARGET = $(patsubst %.f90,%.run,$(FILES))

# OBJFIL = $(wildcard ../include/*.o)

# .PHONY: examples
#  $(TARGET): $(FILES)

all: $(TARGET)

%.run : %.f90
	$(FC) $(SPRNGFLAG) $(FFLAG) -o $@ $< $(LIBS)
