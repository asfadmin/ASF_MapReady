# rules.mk
# Universal rules for makefiles one directory deeper

include ../../../make_support/system_rules

BINDIR = ../../../bin

FC     = gfortran -ff2c
FFLAGS = $(OPTFLAG) $(DEBUGFLAGS) $(LARGEFILE_FLAGS) -ffixed-line-length-132
#FFLAGS = $(OPTFLAG) $(LARGEFILE_FLAGS) \
#         -fno-globals -Wno-globals -ffixed-line-length-132

CC     = gcc
CFLAGS = $(OPTFLAG) $(DEBUGFLAGS) $(LARGEFILE_FLAGS)
X_LIBS   = -L/usr/X11R6/lib -lXm -lXt -lX11 -lm
