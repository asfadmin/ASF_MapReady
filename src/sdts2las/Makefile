# Makefile for: sdts2las
# Makefile author: Orion Lawlor
# Makefile date: 5/13/99

include ../../make_support/system_rules

OBJS = \
        a_toe.o\
        bak_fld.o\
        bak_rec.o\
        bak_sfld.o\
        beg_ddre.o\
        beg_file.o\
        beg_rec.o\
        bld_fmt.o\
        bld_lab.o\
        bldadscr.o\
        c_dddir.o\
        c_ddlead.o\
        c_drdir.o\
        c_drlead.o\
        ch_size.o\
        chk_fld.o\
        chk_nfld.o\
        chk_nrec.o\
        chk_nsfl.o\
        chk_rec.o\
        chk_sfld.o\
        cki_nfld.o\
        conv_cat.o\
        del_dim.o\
        del_fmt.o\
        del_labs.o\
        deldrval.o\
        e_toa.o\
        end_ddre.o\
        end_file.o\
        end_rec.o\
        er_ddfld.o\
        er_ddrec.o\
        er_ddsfl.o\
        free_lab.o\
        g_order.o\
        gbstr.o\
        gdstr.o\
        get_dim.o\
        get_dval.o\
        get_fmt.o\
        get_lvl.o\
        gfstr.o\
        gint.o\
        gsstr.o\
        gstr.o\
        gt_adscr.o\
        i_toa.o\
        incr_lab.o\
        is_adscr.o\
        is_intgr.o\
        l_tos.o\
        ld_ddrec.o\
        ld_rec.o\
        ld_tagp.o\
        load_fld.o\
        load_fmt.o\
        load_lab.o\
        out_fmt.o\
        pop_rs.o\
        push_rs.o\
        r_dddir.o\
        r_ddlead.o\
        r_drdir.o\
        r_drlead.o\
        rd_ddfld.o\
        rd_ddrec.o\
        rd_ddsfl.o\
        rd_fld.o\
        rd_rec.o\
        rd_sfld.o\
        ret_dv.o\
        ret_fv.o\
        ret_matc.o\
        ret_pdm.o\
        rt_pvfld.o\
        rt_pvsfl.o\
        s_tol.o\
        set_stat.o\
        setup_lb.o\
        stc_empt.o\
        stor_dv.o\
        str_tok.o\
        uld_ddre.o\
        uld_rec.o\
        v_ddtag.o\
        v_drtag.o\
        wint.o\
        wr_ddfld.o\
        wr_ddrec.o\
        wr_ddsfl.o\
        wr_fld.o\
        wr_rec.o\
        wr_sfld.o\
        sdts2las.o

all: sdts2las sdts_ingest clean

sdts_ingest:
	cp sdts_ingest.sh $(BINDIR)/sdts_ingest
	-chmod 775 $(BINDIR)/sdts_ingest
	cp sdts_ingest.1 ../../man/cat1

sdts2las: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o sdts2las -lm
	mv sdts2las$(BIN_POSTFIX) $(BINDIR)
	cp sdts2las.1 ../../man/cat1

clean:
	-rm -f *.o
