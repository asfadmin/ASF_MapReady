# @(#)makefile	2.41 3/24/98
MPP_DIRECTIVE =  -Pv -O3 -bmaxdata:0x20000000 -D -qarch=pwr2 -qtune=pwr2
#MPP_DIRECTIVE = -C -qfixed=132 -g -bmaxdata:0x20000000
#MPP_DIRECTIVE = -C

#FCOMP = xlf
FCOMP = mpxlf
#CCOMP = xlc
CCOMP = mpcc

EXE_DIR       = $(CM_HOME)/bin/$(MACH)

LOCAL_INCLUDE = $(CM_HOME)/include/local
GLOBAL_INCLUDE = /ASF/$(ASFVER)/include/sps
INCLUDE       = -I$(LOCAL_INCLUDE)  -I$(GLOBAL_INCLUDE)

EPHEM_LIB     = $(CM_HOME)/lib/$(MACH)/local/libephem.a



.SUFFIXES: .c .f .h

FORTRAN_OBJS = 	asap.o \
		az_resample.o \
		azfft.o \
		azfft_clutr.o \
		azfft_intp.o \
		btor.o \
		c_turn.o \
		cfft.o \
		chirp_lk.o \
		circ_shft.o \
		cluter_focus.o \
		cross.o \
		cvmul.o \
		decode.o \
		decode2.o \
		prf_amb.o \
		dop_parameter.o \
		dop_ref.o \
		doppler_cnt.o \
		ebf_to_ll.o \
		eme_to_ebf.o \
		dop_fit.o \
		expand.o \
		fdr_reg.o \
		frame_pp.o \
		frate_parameter.o \
		get_az_line.o \
		get_peak.o \
		get_tv.o \
		gr2sr.o \
		gttarg1.o \
		gttarg2.o \
		inc_ang.o \
		init_fft.o \
		interp.o \
		interp2.o \
		interpolation.o \
		kaiser.o \
		lk_ang.o \
		look_1_4_xfunc.o \
		lfit.o \
		mulply_azimuth_reference.o \
		mulply_phase1_air.o \
		mulply_phase1_spc.o \
		mulply_phase2_air.o \
		mulply_phase2_spc.o \
		mulply_phase3_air.o \
		mulply_phase3_spc.o \
		mulply_range_reference.o \
		n_pt_intp.o \
		overlay.o \
		overlay_16.o \
		pdf.o \
		peak_pos.o \
		pointing.o \
		propa_simp.o \
	        propag_init.o \
		propagation.o \
		pt_echo.o \
		put_az_line.o \
		r_fd_fr_init.o \
		r_resample.o \
		r_resample_16.o \
		reduce.o \
		rm_doppler.o \
		rngdop.o \
		rngfft.o \
		rngfft_intp.o \
		seetarg.o \
		sel_band.o \
		set_parameters.o \
		sinc.o \
		spat_avg.o \
		spat_avg_16.o \
		sr2gr.o \
		truncate.o \
		weight.o \
		weights.o \
		xcorrel.o \
		zero_crsng.o 

OTHER_OBJS = average_8.o \
	cmplx_2_short.o \
	echo_data_histgram_4.o \
	get_frame_rqst.o \
	get_auxiliary.o \
	get_ephemeris.o \
	get_calibration.o \
	gmt2sec.o \
	gmt2double.o \
	info_handler.o \
	io.o \
	line_hdng.o \
	merge_image_8.o \
	merge_image_16.o \
	merge_image_32.o \
	setup.o \
	sec2gmt.o \
	timer.o \
	write_info.o \
	main.o \
	radio_metric_interp.o \
	radio_metric_vector.o \
	overlay_32.o \
	select_fdd.o \
	select_fdd_amb.o \
	seconds_to_time.o \
	range_spectrum.o
	

FILE_SCRIPTS = run.pp_correlator.F run.pp_correlator.S
EXE_FILES = pp

app: $(EXE_FILES)
	/bin/mv $(EXE_FILES) $(EXE_DIR)
	/bin/cp $(FILE_SCRIPTS) $(EXE_DIR)

clean:
	/bin/rm -f *.o *.lst
	/bin/rm -f $(EXE_FILES)

pp: $(FORTRAN_OBJS) $(OTHER_OBJS)
	$(FCOMP) $(MPP_DIRECTIVE) $(FORTRAN_OBJS) $(OTHER_OBJS) \
	-lesslp2 -lm /ASF/$(ASFVER)/lib/libsps.a \
	$(CM_HOME)/lib/$(MACH)/local/libephem.a -o pp

.c.o: $(CM_HOME)/include/local/pp.h $(CM_HOME)/include/local/variables.h
	$(CCOMP) -langlvl=ansi $(INCLUDE) -O3 -c $<

.f.o:
	$(FCOMP) $(MPP_DIRECTIVE) -c $<
