#
# @(#)Makefile	1.7 96/10/10 11:03:58
#
# Makefile for libsarproc.a
#
# (New version of libx334.a)
#

AR	= /bin/ar
ARFLAGS	= rv
CC	= /usr/bin/cc
CHMOD	= /usr/bin/chmod
ECHO	= /bin/echo
F77	= /usr/bin/f77
RM	= /bin/rm

#INCLUDEDIR	= ../include
INCLUDEDIR	= .
#INSTALLDIR	= $(ASF)/lib
INSTALLDIR	= ../../lib/$(MACH)/global
CFLAGS = -g -DDEBUG=1 -I$(INCLUDEDIR)
FFLAGS = -g -I$(INCLUDEDIR)

LIB	= libsarproc.a

COBJ =	\
	az1l.o \
	az4l.o \
	az4lpre.o \
	ceos_cmp.o \
	corn_loc.o \
	dtohex_.o \
	froff.o \
	i_fdr_cmp.o \
	itohex_.o \
	l_adr_cmp.o \
	l_dhr_cmp.o \
	l_dqs_cmp.o \
	l_dss_cmp.o \
	l_fdr_cmp.o \
	l_ppd_cmp.o \
	l_rcd_cmp.o \
	l_rdr_cmp.o \
	l_rsr_cmp.o \
	least_sq.o \
	least_sq2.o \
	libsarproc_version.o \
	linear_lsq.o \
	lsq.o \
	minpath.o \
	noise.o \
	np_delay.o \
	pathval.o \
	predict.o \
	pregp4l.o \
	printers.o \
	rate_chk.o \
	rawchk.o \
	rm1l.o \
	rm4l.o \
	rm4lslr.o \
	sitohex_.o \
	sl2gr_intg.o \
	sltogr.o \
	sv_attd.o \
	t_cdr_cmp.o \
	t_dpp_cmp.o \
	t_fdr_cmp.o \
	t_fdt_cmp.o \
	table.o \
	time_uts.o \
	vecsr2gr.o \
	weight.o \
	write_ceos.o

FOBJ =	\
	angles.o \
	anomly.o \
	asap.o \
	bigfft.o \
	calcr.o \
	cart2sph.o \
	clock.o \
	cnvrt13.o \
	cnvrt15.o \
	coord.o \
	dcube2.o \
	debug4.o \
	delm.o \
	delpsd1.o \
	dens76.o \
	depsln.o \
	der.o \
	deuler2.o \
	dframe11.o \
	ditsct2.o \
	ditsct3.o \
	dmtrv.o \
	dprecis1.o \
	dquad.o \
	dquart1.o \
	drotm.o \
	drotv.o \
	dscalar.o \
	dscale1.o \
	dsumv.o \
	dsumv2.o \
	dvectp.o \
	dvnr1.o \
	ephem.o \
	eqnox.o \
	fnd1st.o \
	get3db.o \
	getdop2.o \
	getloc.o \
	getloc_C.o \
	getlocc.o \
	getlook.o \
	getstat.o \
	gha.o \
	i2twos.o \
	initdops.o \
	intrp81.o \
	julian.o \
	kepler.o \
	kozsak.o \
	legend.o \
	makesxchi.o \
	math.o \
	max5.o \
	mkchirp.o \
	mkcl_table.o \
	mksl2gr.o \
	mkwvec.o \
	newearth.o \
	newtprop.o \
	pout.o \
	rangetf.o \
	rdmtric.o \
	repmeas.o \
	rk78.o \
	rk78cn.o \
	setsun.o \
	setthd.o \
	simpwrap.o \
	snrmeas.o \
	twos2fp.o \
	userop.o \
	utc2gha.o \
	xthird.o

.c.o:
	$(CC) $(CFLAGS) -c $<
	-$(CHMOD) g+w $@

.f.o:
	$(F77) $(FFLAGS) -c $*.f
	-$(CHMOD) g+w $@

glib:	all
all:	$(INSTALLDIR)/$(LIB)

$(INSTALLDIR)/$(LIB):  $(LIB)
	-$(RM) $(INSTALLDIR)/$(LIB)
	install -v -m 644 -F $(INSTALLDIR) $(LIB)

$(LIB):  $(COBJ) $(FOBJ)
	-$(RM) $(LIB)
	 $(AR) $(ARFLAGS) $(LIB) $(COBJ) $(FOBJ)

glib_inst:
	@ $(ECHO) No global files needed from `pwd` -- skipping installation

clean:
	-$(RM) $(COBJ) $(FOBJ) $(LIB)

# FORTRAN dependencies (not handled by makedepend)
rangetf.o: $(INCLUDEDIR)/libmp.h
repmeas.o: $(INCLUDEDIR)/libmp.h
snrmeas.o: $(INCLUDEDIR)/libmp.h

# DO NOT DELETE THIS LINE -- make depend depends on it.

i_fdr_cmp.o: $(INCLUDEDIR)/i_fdr.h $(INCLUDEDIR)/procdec.h $(INCLUDEDIR)/procfil.h
i_fdr_cmp.o: $(INCLUDEDIR)/scene_file.h
l_adr_cmp.o: $(INCLUDEDIR)/l_adr.h $(INCLUDEDIR)/procfil.h $(INCLUDEDIR)/scene_file.h
l_adr_cmp.o: $(INCLUDEDIR)/procdec.h
l_dhr_cmp.o: $(INCLUDEDIR)/l_dhr.h $(INCLUDEDIR)/procfil.h $(INCLUDEDIR)/scene_file.h
l_dhr_cmp.o: $(INCLUDEDIR)/procdec.h
l_dqs_cmp.o: $(INCLUDEDIR)/l_dqs.h $(INCLUDEDIR)/procfil.h $(INCLUDEDIR)/scene_file.h
l_dqs_cmp.o: $(INCLUDEDIR)/procdec.h
l_dss_cmp.o: $(INCLUDEDIR)/l_dss.h $(INCLUDEDIR)/scene_file.h $(INCLUDEDIR)/procdec.h
l_dss_cmp.o: $(INCLUDEDIR)/procfil.h $(INCLUDEDIR)/procpar.h
l_fdr_cmp.o: $(INCLUDEDIR)/l_fdr.h $(INCLUDEDIR)/procdec.h $(INCLUDEDIR)/procfil.h
l_fdr_cmp.o: $(INCLUDEDIR)/scene_file.h
l_ppd_cmp.o: $(INCLUDEDIR)/l_ppd.h $(INCLUDEDIR)/scene_file.h $(INCLUDEDIR)/procdec.h
l_ppd_cmp.o: $(INCLUDEDIR)/procfil.h
l_rcd_cmp.o: $(INCLUDEDIR)/l_rcd.h $(INCLUDEDIR)/scene_file.h $(INCLUDEDIR)/procfil.h
l_rcd_cmp.o: $(INCLUDEDIR)/procdec.h
l_rdr_cmp.o: $(INCLUDEDIR)/l_rdr.h $(INCLUDEDIR)/scene_file.h $(INCLUDEDIR)/procfil.h
l_rdr_cmp.o: $(INCLUDEDIR)/procdec.h $(INCLUDEDIR)/procpar.h
l_rsr_cmp.o: $(INCLUDEDIR)/l_rsr.h $(INCLUDEDIR)/scene_file.h $(INCLUDEDIR)/procfil.h
l_rsr_cmp.o: $(INCLUDEDIR)/procdec.h
predict.o: $(INCLUDEDIR)/procfil.h
printers.o: $(INCLUDEDIR)/flagerror.h
rm1l.o: $(INCLUDEDIR)/flagerror.h
rm4l.o: $(INCLUDEDIR)/flagerror.h
rm4lslr.o: $(INCLUDEDIR)/flagerror.h
t_cdr_cmp.o: $(INCLUDEDIR)/t_cdr.h $(INCLUDEDIR)/scene_file.h $(INCLUDEDIR)/procfil.h
t_cdr_cmp.o: $(INCLUDEDIR)/procdec.h
t_dpp_cmp.o: $(INCLUDEDIR)/t_dpp.h $(INCLUDEDIR)/scene_file.h $(INCLUDEDIR)/procfil.h
t_dpp_cmp.o: $(INCLUDEDIR)/procdec.h
t_fdr_cmp.o: $(INCLUDEDIR)/t_fdr.h $(INCLUDEDIR)/scene_file.h $(INCLUDEDIR)/procdec.h
t_fdr_cmp.o: $(INCLUDEDIR)/procfil.h $(INCLUDEDIR)/procpar.h
t_fdt_cmp.o: $(INCLUDEDIR)/procdec.h $(INCLUDEDIR)/procfil.h $(INCLUDEDIR)/t_fdt.h
t_fdt_cmp.o: $(INCLUDEDIR)/scene_file.h
time_uts.o: $(INCLUDEDIR)/procfil.h
write_ceos.o: $(INCLUDEDIR)/scene_file.h $(INCLUDEDIR)/procdec.h
write_ceos.o: $(INCLUDEDIR)/procfil.h $(INCLUDEDIR)/procpar.h
