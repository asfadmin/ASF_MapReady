# Makefile for          :  geolib.a 
# Module Author         :  Tom Logan   

include ../../make_support/system_rules

SUBS = \
		alberfor.o \
		alberinv.o \
		alconfor.o \
		alconinv.o \
		azimfor.o \
		aziminv.o \
		c_ckptid.o \
		c_decdeg.o \
		c_degdms.o \
		c_eval.o \
		c_getgrd.o \
		c_getrwt.o \
		c_grderr.o \
		c_grdred.o \
		c_mapedg.o \
		c_proj.o \
		c_projon.o \
		c_prostr.o \
		c_ptclse.o \
		c_putgrd.o \
		c_steplr.o \
		check_dms.o \
		cproj.o \
		eqconfor.o \
		eqconinv.o \
		equifor.o \
		equiinv.o \
		for_init.o \
		gctp.o \
		geograph.o \
		gnomfor.o \
		gnominv.o \
		goodfor.o \
		goodinv.o \
		gvnspfor.o \
		gvnspinv.o \
		hamfor.o \
		haminv.o \
		imolwfor.o \
		imolwinv.o \
		inv_init.o \
		lamazfor.o \
		lamazinv.o \
		lamccfor.o \
		lamccinv.o \
		merfor.o \
		merinv.o \
		millfor.o \
		millinv.o \
		molwfor.o \
		molwinv.o \
		obleqfor.o \
		obleqinv.o \
		omerfor.o \
		omerinv.o \
		orthfor.o \
		orthinv.o \
		paksz.o \
		polyfor.o \
		polyinv.o \
		proj_err.o \
		proj_print.o \
		proj_report.o \
		prt_spcs_zone.o \
		psfor.o \
		psinv.o \
		psupport.o \
		report.o \
		robfor.o \
		robinv.o \
		sinfor.o \
		sininv.o \
		somfor.o \
		sominv.o \
		spcs_zone.o \
		sphdz.o \
		spheroid.o \
		sterfor.o \
		sterinv.o \
		stplnfor.o \
		stplninv.o \
		tmfor.o \
		tminv.o \
		untfz.o \
		vandgfor.o \
		vandginv.o \
		wivfor.o \
		wivinv.o \
		wviifor.o \
		wviiinv.o

all: asf_geolib.a
	rm *.o

asf_geolib.a:	$(SUBS) 
		ar rv asf_geolib.a $(SUBS)
		$(RANLIB) asf_geolib.a
		mv asf_geolib.a $(LIBDIR)
#		cp asf_geolib.man ../../man/cat1/asf_geolib.1

