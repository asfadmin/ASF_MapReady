# Alaska SAR Processor (ASP) %W% %E% %U%
#   Makefile for ASPDEV 334 source files on ALPHA

INCLDIR = -I. -I../../include/local
LIBPATH = ../../lib/alpha/local

LIB = $(LIBPATH)/libx334.a

CFLAGS = -g $(INCLDIR)

FFILES = $(LIB)(angles.o) \
	 $(LIB)(anomly.o) \
	 $(LIB)(asap.o) \
	 $(LIB)(bigfft.o) \
	 $(LIB)(calcr.o) \
	 $(LIB)(cart2sph.o) \
	 $(LIB)(clock.o) \
	 $(LIB)(cnvrt13.o) \
	 $(LIB)(cnvrt15.o) \
	 $(LIB)(coord.o) \
	 $(LIB)(dcube2.o) \
	 $(LIB)(debug4.o) \
	 $(LIB)(delm.o) \
	 $(LIB)(delpsd1.o) \
	 $(LIB)(dens76.o) \
	 $(LIB)(depsln.o) \
	 $(LIB)(der.o) \
	 $(LIB)(deuler2.o) \
	 $(LIB)(dframe11.o) \
	 $(LIB)(ditsct2.o) \
	 $(LIB)(ditsct3.o) \
	 $(LIB)(dmtrv.o) \
	 $(LIB)(dprecis1.o) \
	 $(LIB)(dquad.o) \
	 $(LIB)(dquart1.o) \
	 $(LIB)(drotm.o) \
	 $(LIB)(drotv.o) \
	 $(LIB)(dscalar.o) \
	 $(LIB)(dscale1.o) \
	 $(LIB)(dsumv.o) \
	 $(LIB)(dsumv2.o) \
	 $(LIB)(dvectp.o) \
	 $(LIB)(dvnr1.o) \
	 $(LIB)(ephem.o) \
	 $(LIB)(eqnox.o) \
	 $(LIB)(fnd1st.o) \
	 $(LIB)(get3db.o) \
	 $(LIB)(getdop2.o) \
	 $(LIB)(getloc.o) \
	 $(LIB)(getloc_C.o) \
	 $(LIB)(getlocc.o) \
	 $(LIB)(getlook.o) \
	 $(LIB)(getstat.o) \
	 $(LIB)(gha.o) \
	 $(LIB)(i2twos.o) \
	 $(LIB)(initdops.o) \
	 $(LIB)(intrp81.o) \
	 $(LIB)(julian.o) \
	 $(LIB)(kepler.o) \
	 $(LIB)(kozsak.o) \
	 $(LIB)(legend.o) \
	 $(LIB)(makesxchi.o) \
	 $(LIB)(math.o) \
	 $(LIB)(max5.o) \
	 $(LIB)(mkchirp.o) \
	 $(LIB)(mkcl_table.o) \
	 $(LIB)(mksl2gr.o) \
	 $(LIB)(mkwvec.o) \
	 $(LIB)(newearth.o) \
	 $(LIB)(newtprop.o) \
	 $(LIB)(pout.o) \
	 $(LIB)(rangetf.o) \
	 $(LIB)(rdmtric.o) \
	 $(LIB)(repmeas.o) \
	 $(LIB)(rk78.o) \
	 $(LIB)(rk78cn.o) \
	 $(LIB)(setsun.o) \
	 $(LIB)(setthd.o) \
	 $(LIB)(simpwrap.o) \
	 $(LIB)(snrmeas.o) \
	 $(LIB)(twos2fp.o) \
	 $(LIB)(userop.o) \
	 $(LIB)(utc2gha.o) \
	 $(LIB)(xthird.o)

CFILES = \
	$(LIB)(az1l.o)\
	$(LIB)(az4l.o)\
	$(LIB)(az4lpre.o)\
	$(LIB)(corn_loc.o)\
	$(LIB)(dtohex_.o)\
	$(LIB)(froff.o)\
	$(LIB)(itohex_.o)\
	$(LIB)(lsq.o)\
	$(LIB)(minpath.o)\
	$(LIB)(np_delay.o)\
	$(LIB)(pathval.o)\
	$(LIB)(predict.o)\
	$(LIB)(pregp4l.o)\
	$(LIB)(rawchk.o)\
	$(LIB)(rm1l.o)\
	$(LIB)(rm4l.o)\
	$(LIB)(rm4lslr.o)\
	$(LIB)(sitohex_.o)\
	$(LIB)(sltogr.o)\
	$(LIB)(sv_attd.o)\
	$(LIB)(table.o)\
	$(LIB)(vecsr2gr.o)\
	$(LIB)(weight.o)

$(LIB): $(CFILES) $(FFILES)

$(CFILES):
	 cc -c $(CFLAGS) $(@:.o=.c)
	 ar rv $(LIB) $@
	 rm $@
	 ranlib $(LIB)

$(FFILES):
	 f77 -c $(@:.o=.f)
	 ar rv $(LIB) $@
	 rm $@
	 ranlib $(LIB)

.PRECIOUS: $(LIB)

.f.a:;
.c.a:;
depend:; makedepend $(INCLDIR) *.c
#============================= make depend
# DO NOT DELETE THIS LINE -- make depend depends on it.

az1l.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
az4l.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
az4l.o: ../../include/local/procfil.h
az4lpre.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
corn_loc.o: /usr/include/math.h /usr/include/standards.h /usr/include/stdio.h
froff.o: /usr/include/math.h /usr/include/standards.h
lsq.o: /usr/include/stdio.h /usr/include/standards.h
minpath.o: /usr/include/math.h /usr/include/standards.h
np_delay.o: /usr/include/math.h /usr/include/standards.h
pathval.o: /usr/include/math.h /usr/include/standards.h
predict.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
predict.o: /usr/include/ctype.h /usr/include/string.h 
predict.o: ../../include/local/procfil.h
pregp4l.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
rawchk.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
rm1l.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
rm1l.o: ../../include/local/flagerror.h
rm4l.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
rm4l.o: ../../include/local/flagerror.h
rm4lslr.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
rm4lslr.o: ../../include/local/flagerror.h
sltogr.o: /usr/include/stdio.h /usr/include/standards.h
sv_attd.o: /usr/include/math.h /usr/include/standards.h /usr/include/stdio.h
table.o: /usr/include/math.h /usr/include/standards.h
vecsr2gr.o: /usr/include/math.h /usr/include/standards.h
weight.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
