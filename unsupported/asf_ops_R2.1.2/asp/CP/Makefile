# Alaska SAR Processor (ASP) %W% %E% %U%
#   Makefile for ASP-CP operational software

EXEC_ASP_CP = ../../bin/alpha/asp_cp.exe 

ASPLIB = ../../lib/alpha/local
LIBS = $(ASPLIB)/libxasp.a $(ASPLIB)/libxrtns.a $(ASPLIB)/libx334.a $(ASF)/lib/libsps.a $(ASF)/lib/libceos.a
LINKLIBS = $(LIBS) -lfor -lots -ltermcap -lm -lmp -lpthreads -lmach -lc_r 

CC=cc -ansi
INCLDIR = -I. -I../../include/local -I../../include/global
CFLAGS = -g $(INCLDIR) -DCP_SW

CP_INCS = 	asf.h odl.h Msg.h

CSOURCES=	cp_uts.c

$(EXEC_ASP_CP):	asp.o cp_uts.o $(LIBS) 
	 $(CC) $(CFLAGS) asp.o -o $@ cp_uts.o ${LINKLIBS}

#copy_CP_source:
#	-cp -p /ASF/R1BP/lib/libsps.a ../../lib/alpha/global/.
#	-for f in $(CP_INCS); do \
#		cp -p /ASF/R1BP/include/sps/$$f .;\
#	done

depend:; makedepend $(INCLDIR) -DCP_SW *.c ../main/asp.c

asp.o: ../main/asp.c
	$(CC) -c $(CFLAGS) ../main/asp.c 
#============================= make depend
# DO NOT DELETE THIS LINE -- make depend depends on it.

cp_uts.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/string.h
cp_uts.o: /usr/include/stdlib.h /usr/include/ctype.h /usr/include/errno.h
cp_uts.o: /usr/include/syslog.h /usr/include/math.h /usr/include/sys/types.h
cp_uts.o: /usr/include/sys/stat.h /usr/include/sys/mode.h /usr/include/time.h
cp_uts.o: ../../include/global/asf.h /usr/include/stdarg.h
cp_uts.o: ../../include/global/odl.h
cp_uts.o: ../../include/global/Msg.h
cp_uts.o: ../../include/global/pthread_wrapper.h
cp_uts.o: ../../include/local/cp_uts.h
cp_uts.o: ../../include/local/procfil.h
cp_uts.o: ../../include/global/version.h
cp_uts.o: ../../include/local/asp_msg.h
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
asp.o: ../../include/local/asp_msg.h
asp.o: ../../include/global/version.h
