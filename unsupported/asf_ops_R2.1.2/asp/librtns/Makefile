# Alaska SAR Processor (ASP) %W% %E% %U%
#   Makefile for ASPDEV library routines software

INCLDIR = -I. -I../../include/local

CFLAGS = -g $(INCLDIR)

LIB = ../../lib/alpha/local/libxrtns.a
OBJDIR = ../main

LFILES = 	$(LIB)(aspio.o) \
		$(LIB)(camio.o) \
		$(LIB)(gettime.o) \
		$(LIB)(mbaDatToIEEE.o) \
		$(LIB)(mbabob.o) \
		$(LIB)(mbac.o) \
		$(LIB)(mbacbob.o) \
		$(LIB)(mbacmdscan.o) \
		$(LIB)(mbadreg.o) \
		$(LIB)(mbaexec.o) \
		$(LIB)(mbafc.o) \
		$(LIB)(mbafr.o) \
		$(LIB)(mbagetdata.o) \
		$(LIB)(mbagetrdata.o) \
		$(LIB)(mbahelp.o) \
		$(LIB)(mbahist.o) \
		$(LIB)(mbaibob.o) \
		$(LIB)(mbaigraph.o) \
		$(LIB)(mbaim.o) \
		$(LIB)(mbaimgdata.o) \
		$(LIB)(mbaimsubs.o) \
		$(LIB)(mbakey.o) \
		$(LIB)(mbaloopon.o) \
		$(LIB)(mbam.o) \
		$(LIB)(mbaplot.o) \
		$(LIB)(mbapup.o) \
		$(LIB)(mbaputdata.o) \
		$(LIB)(mbarandstim.o) \
		$(LIB)(mbarel.o) \
		$(LIB)(mbarun_shell.o) \
		$(LIB)(mbarun_vi.o) \
		$(LIB)(mbascan.o) \
		$(LIB)(mbat.o) \
		$(LIB)(mbatermsize.o) \
		$(LIB)(mbatokens.o) \
		$(LIB)(mbawd.o) \
		$(LIB)(mbawdr.o) \
		$(LIB)(setup_avg.o) \
		$(LIB)(setup_az.o) \
		$(LIB)(setup_ctc.o) \
		$(LIB)(setup_ctm.o) \
		$(LIB)(setup_fft.o) \
		$(LIB)(setup_iif.o) \
		$(LIB)(setup_jdfm.o) \
		$(LIB)(setup_mlc.o) \
		$(LIB)(setup_mlm.o) \
		$(LIB)(setup_nexec.o) \
		$(LIB)(setup_out.o) \
		$(LIB)(setup_rdfm.o) \
		$(LIB)(setup_rm.o) \
		$(LIB)(setup_rpi.o) \
		$(LIB)(setup_umr.o) \
		$(LIB)(setup_xfer.o) \
		$(LIB)(simaadd16.o) \
		$(LIB)(simampy.o) \
		$(LIB)(wait_io.o) 

OBJS = 		$(OBJDIR)/mbatermsize.o \
		$(OBJDIR)/mbatokens.o 

all: $(LIB) $(OBJS)

$(LIB):	$(LFILES) 

$(LFILES): 
	cc -c $(CFLAGS) $(@:.o=.c)
	ar rv $(LIB) $@
	ranlib $(LIB)	 
	rm $@

$(OBJS):
	ar xov $(LIB) $@

.PRECIOUS: $(LIB)

.c.a:;

depend:; makedepend $(INCLDIR) *.c
#============================= make depend
# DO NOT DELETE THIS LINE -- make depend depends on it.

aspio.o: /usr/include/sys/file.h /usr/include/sys/types.h
aspio.o: /usr/include/standards.h /usr/include/sys/param.h
aspio.o: /usr/include/sys/limits.h /usr/include/machine/machparam.h
aspio.o: /usr/include/machine/endian.h /usr/include/machine/machlimits.h
aspio.o: /usr/include/sys/access.h /usr/include/sys/fcntl.h
aspio.o: /usr/include/sys/stat.h /usr/include/sys/mode.h /usr/include/stdio.h
aspio.o: /usr/include/unistd.h /usr/include/time.h /usr/include/errno.h
aspio.o: ../../include/local/btio.h
camio.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/unistd.h
camio.o: /usr/include/sys/types.h /usr/include/sys/access.h
camio.o: /usr/include/time.h /usr/include/sys/file.h /usr/include/sys/param.h
camio.o: /usr/include/sys/limits.h /usr/include/machine/machparam.h
camio.o: /usr/include/machine/endian.h /usr/include/machine/machlimits.h
camio.o: /usr/include/sys/fcntl.h /usr/include/sys/ioctl.h
camio.o: /usr/include/sys/secdefines.h /usr/include/sys/uio.h
camio.o: /usr/include/strings.h /usr/include/string.h /usr/include/ctype.h
camio.o: /usr/include/math.h ../../include/local/cam_defs.h
camio.o: /usr/include/io/common/iotypes.h /usr/include/io/cam/cam.h
camio.o: /usr/include/io/cam/dec_cam.h /usr/include/io/cam/uagt.h
camio.o: /usr/include/io/cam/scsi_all.h
gettime.o: /usr/include/sys/time.h /usr/include/sys/limits.h
gettime.o: /usr/include/standards.h /usr/include/sys/types.h
gettime.o: /usr/include/sys/signal.h /usr/include/time.h
mbaDatToIEEE.o: ../../include/local/aspdecl.h
mbabob.o: ../../include/local/aspdecl.h 
mbabob.o: ../../include/local/aspimg.h /usr/include/math.h
mbabob.o: /usr/include/standards.h /usr/include/stdio.h /usr/include/fcntl.h
mbabob.o: /usr/include/sys/fcntl.h /usr/include/sys/types.h
mbabob.o: /usr/include/time.h
mbac.o: ../../include/local/aspdecl.h
mbacmdscan.o: ../../include/local/aspdecl.h
mbadreg.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/ctype.h
mbadreg.o: ../../include/local/aspdecl.h 
mbadreg.o: ../../include/local/ibobdefs.h
mbaexec.o: /usr/include/sys/types.h /usr/include/standards.h
mbaexec.o: ../../include/local/aspdecl.h 
mbafc.o: ../../include/local/aspdecl.h
mbafr.o: /usr/include/math.h /usr/include/standards.h 
mbafr.o: ../../include/local/aspdecl.h
mbagetdata.o: ../../include/local/aspdecl.h
mbagetrdata.o: ../../include/local/aspdecl.h
mbahelp.o: /usr/include/stdio.h /usr/include/standards.h
mbahist.o: ../../include/local/aspdecl.h
mbahist.o: /usr/include/math.h /usr/include/standards.h
mbaibob.o: ../../include/local/ibobdefs.h /usr/include/stdio.h
mbaibob.o: ../../include/local/aspdecl.h
mbaibob.o: /usr/include/standards.h /usr/include/signal.h
mbaigraph.o: /usr/include/stdio.h /usr/include/standards.h
mbaigraph.o: /usr/include/ctype.h ../../include/local/ibobdefs.h
mbaigraph.o: ../../include/local/aspdecl.h
mbaim.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/fcntl.h
mbaim.o: /usr/include/sys/fcntl.h /usr/include/sys/types.h 
mbaim.o: ../../include/local/aspdecl.h
mbaim.o: ../../include/local/aspimg.h /usr/include/math.h
mbaimsubs.o: ../../include/local/aspimg.h 
mbaimsubs.o: /usr/include/math.h /usr/include/standards.h
mbakey.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
mbakey.o: /usr/include/standards.h /usr/include/sys/types.h
mbakey.o: /usr/include/sys/ioctl.h /usr/include/sys/secdefines.h
mbakey.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/termio.h
mbakey.o: /usr/include/sys/ttmap.h /usr/include/sys/termios.h
mbakey.o: /usr/include/sys/ttydefaults.h /usr/include/ctype.h
mbaloopon.o: ../../include/local/aspdecl.h
mbam.o: ../../include/local/aspdecl.h
mbaplot.o: ../../include/local/aspdecl.h
mbapup.o: ../../include/local/aspdecl.h
mbaputdata.o: ../../include/local/aspdecl.h
mbarandstim.o: /usr/include/math.h /usr/include/standards.h
mbarandstim.o: ../../include/local/aspdecl.h
mbarel.o: ../../include/local/aspdecl.h
mbarun_shell.o: /usr/include/stdio.h /usr/include/standards.h
mbarun_vi.o: /usr/include/stdio.h /usr/include/standards.h
mbascan.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/ctype.h
mbat.o: /usr/include/stdio.h /usr/include/standards.h 
mbat.o: ../../include/local/aspdecl.h
mbatermsize.o: /usr/include/curses.h /usr/include/stdio.h
mbatermsize.o: /usr/include/standards.h /usr/include/unctrl.h
mbatermsize.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
mbatermsize.o: /usr/include/sys/types.h /usr/include/sys/secdefines.h
mbatokens.o: /usr/include/stdio.h /usr/include/standards.h
mbatokens.o: /usr/include/unistd.h /usr/include/sys/types.h
mbatokens.o: /usr/include/sys/access.h /usr/include/time.h
mbawd.o: ../../include/local/aspdecl.h 
mbawd.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
mbawd.o: /usr/include/standards.h /usr/include/sys/types.h
mbawd.o: /usr/include/stdio.h
mbawdr.o: ../../include/local/aspdecl.h 
mbawdr.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
mbawdr.o: /usr/include/standards.h /usr/include/sys/types.h
mbawdr.o: /usr/include/stdio.h
setup_avg.o: ../../include/local/aspdecl.h
setup_az.o: ../../include/local/aspdecl.h
setup_ctc.o: ../../include/local/aspdecl.h
setup_ctm.o: ../../include/local/aspdecl.h
setup_fft.o: ../../include/local/aspdecl.h
setup_iif.o: ../../include/local/aspdecl.h
setup_jdfm.o: /usr/include/stdio.h /usr/include/standards.h
setup_jdfm.o: ../../include/local/aspdecl.h
setup_mlc.o: ../../include/local/aspdecl.h
setup_mlm.o: ../../include/local/aspdecl.h
setup_nexec.o: ../../include/local/aspdecl.h
setup_out.o: ../../include/local/aspdecl.h
setup_rdfm.o: /usr/include/stdio.h /usr/include/standards.h 
setup_rdfm.o: ../../include/local/aspdecl.h
setup_rm.o: ../../include/local/aspdecl.h
setup_rpi.o: /usr/include/stdio.h /usr/include/standards.h
setup_rpi.o: ../../include/local/aspdecl.h
setup_rpi.o: ../../include/local/btio.h
setup_rpi.o: /usr/include/sys/types.h
setup_umr.o: ../../include/local/aspdecl.h
setup_xfer.o: ../../include/local/aspdecl.h
wait_io.o: /usr/include/sys/time.h /usr/include/sys/limits.h
wait_io.o: /usr/include/standards.h /usr/include/sys/types.h
wait_io.o: /usr/include/sys/signal.h /usr/include/time.h /usr/include/stdio.h
wait_io.o: /usr/include/signal.h /usr/include/ctype.h
