# Makefile for statistical-cost network algorithm for phase unwrapping 

CC		=	cc
CFLAGS		=	-O3
DEFPTHREAD	=	-D USE_PTHREADS
PTHREADLIB	=	-lpthread
LIBS		=	-lm
BINDIR		=	../../bin/irix/
SNAPHU		=	$(BINDIR)/snaphu
TILESNAPHU	=	$(BINDIR)/tilesnaphu
OBJS		=	snaphu_solver.o \
			snaphu_io.o \
			snaphu_util.o \
			snaphu_cost.o \
			snaphu_cs2.o 


all:		$(SNAPHU) $(TILESNAPHU) clean

$(SNAPHU):	snaphu.c \
		$(OBJS) \
		snaphu.h Makefile 
		$(CC) $(CFLAGS) \
		snaphu.c \
		$(OBJS) \
		-o $(SNAPHU) $(LIBS)

$(TILESNAPHU):	tilesnaphu.c \
		$(OBJS) \
		snaphu.h tilesnaphu.h Makefile 
		$(CC) $(DEFPTHREAD) $(CFLAGS) \
		tilesnaphu.c \
		$(OBJS) \
		-o $(TILESNAPHU) $(PTHREADLIB) $(LIBS)
		cp ak_demIFM $(BINDIR)
		cp tilesnaphu_catDEMSeeds $(BINDIR)
clean:
		rm -f core *.o
		

clobber:
		@rm -f $(SNAPHU) $(TILESNAPHU)

