# Alaska SAR Processor (ASP) %W% %E% %U%
#   Makefile for ASP operational software

EXEC_ASP= ../../bin/alpha/asp.exe 
EXEC_CLEANUP= ../../bin/alpha/cleanup.exe 
EXEC_CHECKSPACE= ../../bin/alpha/checkspace.exe 
EXEC_STOPIT= ../../bin/alpha/stopit 

INCLDIR = -I. -I../../include/local -I../../include/global
ASPDIR = ../../lib/alpha/local
LIBS = $(ASPDIR)/libxasp.a $(ASPDIR)/libx334.a $(ASPDIR)/libxrtns.a $(ASF)/lib/libceos.a
LINKLIBS = $(LIBS) -lfor -lots -ltermcap -lm -lcurses

CFLAGS = -g $(INCLDIR) 

CLEANUP_OBJS = 	op_uts.o \
		cleanup_uts.o \
		mbatermsize.o	\
		mbatokens.o	

CHECKSPACE_OBJS = cleanup_uts.o \
		  mbatermsize.o	\
		  mbatokens.o	

all: $(EXEC_ASP) $(EXEC_CLEANUP) $(EXEC_CHECKSPACE) $(EXEC_STOPIT)

$(EXEC_ASP):	asp.o $(LIBS)
	 cc $(CFLAGS) asp.o -o $@ ${LINKLIBS}

$(EXEC_CLEANUP): cleanup.o $(CLEANUP_OBJS)
	cc $(CFLAGS) -o $@ cleanup.o $(CLEANUP_OBJS) -ltermcap 

$(EXEC_CHECKSPACE): checkspace.o $(CHECKSPACE_OBJS)
	cc $(CFLAGS) checkspace.o -o $@ $(CHECKSPACE_OBJS) -ltermcap

$(EXEC_STOPIT): stopit.o
	cc $(CFLAGS) stopit.o -o $@

depend:; makedepend $(INCLDIR) *.c 

#============================= make depend
# DO NOT DELETE THIS LINE -- make depend depends on it.

asp.o: /usr/include/sys/types.h /usr/include/standards.h /usr/include/stdio.h
asp.o: /usr/include/stdarg.h /usr/include/string.h /usr/include/time.h
asp.o: /usr/include/signal.h /usr/include/fcntl.h /usr/include/sys/fcntl.h
asp.o: /usr/include/errno.h /usr/include/unistd.h /usr/include/sys/access.h
asp.o: /usr/include/syslog.h 
asp.o: ../../include/local/procfil.h
asp.o: ../../include/local/scene_file.h
asp.o: ../../include/local/ndir.h
asp.o: /usr/include/sys/param.h /usr/include/sys/limits.h
asp.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
asp.o: /usr/include/machine/machlimits.h /usr/include/dirent.h
asp.o: ../../include/local/aspdecl.h
asp.o: ../../include/local/procdec.h
asp.o: ../../include/local/procpar.h
asp.o: ../../include/global/version.h
asp.o: ../../include/local/asp_msg.h
checkspace.o: /usr/include/stdio.h /usr/include/standards.h
checkspace.o: /usr/include/unistd.h /usr/include/sys/types.h
checkspace.o: /usr/include/sys/access.h /usr/include/time.h
checkspace.o: /usr/include/dirent.h
cleanup.o: /usr/include/sys/types.h /usr/include/standards.h
cleanup.o: /usr/include/stdio.h /usr/include/string.h /usr/include/unistd.h
cleanup.o: /usr/include/sys/access.h /usr/include/time.h
cleanup.o: /usr/include/dirent.h
stopit.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/string.h
stopit.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
stopit.o: /usr/include/sys/types.h
