# Alaska SAR Processor (ASP) %W% %E% %U%
#   Makefile for ASP library routines software

INCLDIR = -I. -I../../include/local -I../../include/global

CFLAGS = -g $(INCLDIR)

LIB = ../../lib/alpha/local/libxasp.a
OBJDIR = ../main

LFILES = $(LIB)(asp_msg.o) \
	$(LIB)(asp_shm.o) \
	$(LIB)(best_val.o) \
	$(LIB)(cleanup_uts.o) \
	$(LIB)(clear_pipe.o) \
	$(LIB)(dc_low.o) \
	$(LIB)(dc_usr.o) \
	$(LIB)(dcrs_uts.o) \
	$(LIB)(e1_ppr.o) \
	$(LIB)(e1_scan.o) \
	$(LIB)(e1_uts.o) \
	$(LIB)(get_2bits.o) \
	$(LIB)(get_bits.o) \
	$(LIB)(get_outb.o) \
	$(LIB)(i_fdr_cmp.o) \
	$(LIB)(j1_hdr.o) \
	$(LIB)(j1_make_ppr.o) \
	$(LIB)(j1_ppr.o) \
	$(LIB)(j1_scan.o) \
	$(LIB)(j1_sync_pcm.o) \
	$(LIB)(j1_uts.o) \
	$(LIB)(j1_write_hk.o) \
	$(LIB)(j1_write_pcm.o) \
	$(LIB)(make_fix_ctr.o) \
	$(LIB)(op_futs.o) \
	$(LIB)(op_uts.o) \
	$(LIB)(p_get_df.o) \
	$(LIB)(p_get_m_file.o) \
	$(LIB)(p_uts.o) \
	$(LIB)(pp_init.o) \
	$(LIB)(ppr.o) \
	$(LIB)(preproc.o) \
	$(LIB)(proc.o) \
	$(LIB)(r1_cal_samp.o) \
	$(LIB)(r1_ppr.o) \
	$(LIB)(r1_scan.o) \
	$(LIB)(r1_uts.o) \
	$(LIB)(rc_uts.o) \
	$(LIB)(run_image.o) \
	$(LIB)(scan.o) \
	$(LIB)(sel_seg.o) \
	$(LIB)(sl_to_gl.o) \
	$(LIB)(st_play.o) \
	$(LIB)(tape_mnt.o) \
	$(LIB)(time_uts.o) \
	$(LIB)(uts.o) \
	$(LIB)(write_ceos.o)

OBJS = 	$(OBJDIR)/cleanup_uts.o \
	$(OBJDIR)/op_uts.o

all: $(LIB) $(OBJS)

$(LIB):  $(LFILES)

$(LFILES):
	cc -c $(CFLAGS) $(@:.o=.c) 
	ar rv $(LIB) $@
	rm $@

$(OBJS):
	ar xov $(LIB) $@

.PRECIOUS: $(LIB)

.c.a:;

depend:; makedepend $(INCLDIR) *.c
#============================= make depend
# DO NOT DELETE THIS LINE -- make depend depends on it.

asp_msg.o: /usr/include/stdio.h /usr/include/standards.h
asp_msg.o: /usr/include/stdarg.h /usr/include/string.h /usr/include/syslog.h
asp_msg.o: ../../include/local/asp_msg.h
asp_shm.o: /usr/include/sys/types.h /usr/include/standards.h
asp_shm.o: /usr/include/sys/ipc.h /usr/include/sys/shm.h
asp_shm.o: /usr/include/mach/machine/vm_types.h /usr/include/errno.h
asp_shm.o: ../../include/local/asp_msg.h
best_val.o: /usr/include/stdio.h /usr/include/standards.h
best_val.o: ../../include/local/procfil.h 
best_val.o: ../../include/local/procfil_temp.h 
best_val.o: ../../include/local/procdec.h
cleanup_uts.o: /usr/include/unistd.h /usr/include/standards.h
cleanup_uts.o: /usr/include/sys/types.h /usr/include/sys/access.h
cleanup_uts.o: /usr/include/time.h /usr/include/dirent.h
clear_pipe.o: /usr/include/stdio.h /usr/include/standards.h
clear_pipe.o: ../../include/local/aspdecl.h
clear_pipe.o: ../../include/local/procfil.h 
clear_pipe.o: ../../include/local/procfil_temp.h
clear_pipe.o: ../../include/local/procdec.h 
clear_pipe.o: ../../include/local/procpar.h
dc_low.o: /usr/include/ctype.h /usr/include/standards.h /usr/include/fcntl.h
dc_low.o: /usr/include/sys/fcntl.h /usr/include/sys/types.h
dc_low.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/termio.h
dc_low.o: /usr/include/sys/ioctl.h /usr/include/sys/secdefines.h
dc_low.o: /usr/include/sys/ttmap.h /usr/include/sys/termios.h
dc_low.o: /usr/include/sys/ttydefaults.h /usr/include/sys/time.h
dc_low.o: /usr/include/sys/limits.h /usr/include/sys/signal.h
dc_low.o: ../../include/local/procfil.h 
dc_low.o: ../../include/local/procfil_temp.h
dc_low.o: ../../include/local/procdec.h 
dc_low.o: /usr/include/time.h
dc_usr.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/string.h
dc_usr.o: ../../include/local/procfil.h 
dc_usr.o: ../../include/local/procfil_temp.h
dc_usr.o: ../../include/local/procdec.h 
dcrs_uts.o: /usr/include/sys/ioctl.h /usr/include/sys/types.h
dcrs_uts.o: /usr/include/standards.h /usr/include/sys/secdefines.h
dcrs_uts.o: /usr/include/termio.h /usr/include/sys/ttmap.h
dcrs_uts.o: /usr/include/sys/termios.h /usr/include/sys/ttydefaults.h
dcrs_uts.o: /usr/include/stdio.h /usr/include/string.h /usr/include/fcntl.h
dcrs_uts.o: /usr/include/sys/fcntl.h
dcrs_uts.o: ../../include/local/procfil.h
dcrs_uts.o: ../../include/local/procfil_temp.h
dcrs_uts.o: ../../include/local/procdec.h 
e1_ppr.o: ../../include/local/aspdecl.h
e1_ppr.o: ../../include/local/procpar.h
e1_ppr.o: ../../include/local/scene_file.h 
e1_ppr.o: /usr/include/stdio.h
e1_ppr.o: /usr/include/standards.h /usr/include/math.h
e1_scan.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
e1_scan.o: /usr/include/malloc.h /usr/include/stdlib.h 
e1_scan.o: ../../include/local/aspdecl.h
e1_scan.o: ../../include/local/procfil.h
e1_scan.o: ../../include/local/procfil_temp.h
e1_scan.o: ../../include/local/procdec.h 
e1_uts.o: ../../include/local/procfil.h
e1_uts.o: ../../include/local/procfil_temp.h
e1_uts.o: ../../include/local/procdec.h 
e1_uts.o: /usr/include/stdio.h /usr/include/standards.h
e1_uts.o: /usr/include/sys/types.h
e1_uts.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
get_outb.o: /usr/include/sys/types.h /usr/include/standards.h
get_outb.o: /usr/include/stdio.h /usr/include/stdlib.h /usr/include/string.h
get_outb.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
get_outb.o: /usr/include/netinet/in.h /usr/include/errno.h 
get_outb.o: ../../include/local/i_fdr.h
get_outb.o: ../../include/local/asp_shm.h
i_fdr_cmp.o: /usr/include/stdio.h /usr/include/standards.h
i_fdr_cmp.o: /usr/include/netinet/in.h /usr/include/sys/types.h
i_fdr_cmp.o: ../../include/local/procfil.h
i_fdr_cmp.o: ../../include/local/procfil_temp.h
i_fdr_cmp.o: ../../include/local/procdec.h 
i_fdr_cmp.o: ../../include/local/i_fdr.h
i_fdr_cmp.o: ../../include/local/scene_file.h
i_fdr_cmp.o: /usr/include/string.h
j1_hdr.o: ../../include/local/procfil.h
j1_hdr.o: ../../include/local/procfil_temp.h
j1_hdr.o: ../../include/local/aspdecl.h 
j1_hdr.o: /usr/include/stdio.h
j1_hdr.o: /usr/include/standards.h
j1_make_ppr.o: /usr/include/stdio.h /usr/include/standards.h
j1_make_ppr.o: /usr/include/math.h 
j1_make_ppr.o: ../../include/local/aspdecl.h
j1_make_ppr.o: ../../include/local/procfil.h
j1_make_ppr.o: ../../include/local/procfil_temp.h
j1_ppr.o: ../../include/local/aspdecl.h
j1_ppr.o: ../../include/local/procfil.h
j1_ppr.o: ../../include/local/procfil_temp.h
j1_ppr.o: ../../include/local/procdec.h
j1_ppr.o: ../../include/local/procpar.h
j1_ppr.o: /usr/include/stdio.h
j1_ppr.o: ../../include/local/scene_file.h
j1_ppr.o: /usr/include/standards.h /usr/include/math.h
j1_scan.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
j1_scan.o: /usr/include/malloc.h /usr/include/stdlib.h 
j1_scan.o: ../../include/local/aspdecl.h
j1_scan.o: ../../include/local/procfil.h
j1_scan.o: ../../include/local/procfil_temp.h
j1_scan.o: ../../include/local/procdec.h
j1_uts.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/math.h
j1_uts.o: ../../include/local/procfil.h
j1_uts.o: ../../include/local/procfil_temp.h
j1_uts.o: ../../include/local/procdec.h
j1_write_hk.o: /usr/include/stdio.h /usr/include/standards.h
j1_write_pcm.o: /usr/include/stdio.h /usr/include/standards.h
make_fix_ctr.o: /usr/include/stdio.h /usr/include/standards.h
make_fix_ctr.o: /usr/include/math.h 
make_fix_ctr.o: ../../include/local/procfil.h
make_fix_ctr.o: ../../include/local/procfil_temp.h
make_fix_ctr.o: ../../include/local/aspdecl.h
make_fix_ctr.o: ../../include/local/fix_ctr_tbl.h 
make_fix_ctr.o: ../../include/local/E1_fix_00212.h 
make_fix_ctr.o: ../../include/local/E1_fix_02354.h 
make_fix_ctr.o: ../../include/local/E1_fix_03735.h 
make_fix_ctr.o: ../../include/local/E1_fix_12754.h 
make_fix_ctr.o: ../../include/local/E1_fix_14302.h 
make_fix_ctr.o: ../../include/local/E1_fix_16747.h 
make_fix_ctr.o: ../../include/local/E1_fix_19248.h 
make_fix_ctr.o: ../../include/local/E2_fix_00001.h 
make_fix_ctr.o: ../../include/local/J1_fix_00464.h 
make_fix_ctr.o: ../../include/local/R1_STD1_fix_ctr.h 
make_fix_ctr.o: ../../include/local/R1_STD2_fix_ctr.h 
make_fix_ctr.o: ../../include/local/R1_STD3_fix_ctr.h 
make_fix_ctr.o: ../../include/local/R1_STD4_fix_ctr.h 
make_fix_ctr.o: ../../include/local/R1_STD5_fix_ctr.h 
make_fix_ctr.o: ../../include/local/R1_STD6_fix_ctr.h 
make_fix_ctr.o: ../../include/local/R1_STD7_fix_ctr.h 
op_futs.o: ../../include/local/procfil.h 
op_futs.o: ../../include/local/procfil_temp.h 
op_futs.o: ../../include/local/procdec.h 
op_futs.o: /usr/include/stdio.h
op_futs.o: /usr/include/standards.h /usr/include/errno.h
op_futs.o: /usr/include/unistd.h /usr/include/sys/types.h
op_futs.o: /usr/include/sys/access.h /usr/include/time.h
op_futs.o: /usr/include/string.h
op_uts.o: ../../include/local/procfil.h 
op_uts.o: ../../include/local/procfil_temp.h 
op_uts.o: ../../include/local/procdec.h 
op_uts.o: /usr/include/stdio.h
op_uts.o: /usr/include/ctype.h /usr/include/standards.h
p_get_df.o: ../../include/local/procfil.h 
p_get_df.o: ../../include/local/procfil_temp.h 
p_get_df.o: ../../include/local/procdec.h 
p_get_df.o: /usr/include/stdio.h /usr/include/standards.h
p_get_df.o: ../../include/local/scene_file.h
p_get_m_file.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
p_get_m_file.o: /usr/include/standards.h /usr/include/sys/types.h
p_get_m_file.o: /usr/include/stdio.h /usr/include/unistd.h
p_get_m_file.o: /usr/include/sys/access.h /usr/include/time.h
p_get_m_file.o: /usr/include/string.h
p_get_m_file.o: ../../include/local/procfil.h 
p_get_m_file.o: ../../include/local/procfil_temp.h 
p_get_m_file.o: ../../include/local/procdec.h 
p_uts.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/string.h
p_uts.o: /usr/include/syslog.h /usr/include/math.h
p_uts.o: ../../include/local/procfil.h 
p_uts.o: ../../include/local/procfil_temp.h 
p_uts.o: ../../include/local/procdec.h 
p_uts.o: ../../include/local/procpar.h 
p_uts.o: ../../include/local/asp_msg.h 
pp_init.o: ../../include/local/procfil.h 
pp_init.o: ../../include/local/procfil_temp.h 
pp_init.o: ../../include/local/procdec.h 
pp_init.o: /usr/include/stdio.h
pp_init.o: /usr/include/standards.h /usr/include/syslog.h
pp_init.o: /usr/include/string.h /usr/include/errno.h 
pp_init.o: ../../include/local/asp_msg.h
ppr.o: ../../include/local/procfil.h 
ppr.o: ../../include/local/procfil_temp.h 
ppr.o: ../../include/local/procdec.h 
ppr.o: /usr/include/stdio.h
ppr.o: /usr/include/standards.h /usr/include/math.h
preproc.o: /usr/include/stdio.h /usr/include/standards.h
preproc.o: /usr/include/syslog.h /usr/include/string.h 
preproc.o: ../../include/local/aspdecl.h
preproc.o: ../../include/local/procfil.h 
preproc.o: ../../include/local/procfil_temp.h 
preproc.o: ../../include/local/procdec.h 
proc.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/syslog.h
proc.o: /usr/include/unistd.h /usr/include/sys/types.h
proc.o: /usr/include/sys/access.h /usr/include/time.h /usr/include/malloc.h
proc.o: /usr/include/stdlib.h /usr/include/netinet/in.h /usr/include/fcntl.h
proc.o: /usr/include/sys/fcntl.h /usr/include/sys/stat.h
proc.o: /usr/include/sys/mode.h
proc.o: ../../include/local/aspdecl.h 
proc.o: ../../include/local/procfil.h 
proc.o: ../../include/local/procfil_temp.h 
proc.o: ../../include/local/procdec.h 
proc.o: ../../include/local/scene_file.h
proc.o: ../../include/local/i_fdr.h 
proc.o: ../../include/local/asp_msg.h
proc.o: ../../include/local/asp_shm.h
r1_cal_samp.o: /usr/include/stdio.h /usr/include/standards.h
r1_cal_samp.o: ../../include/local/procfil.h 
r1_cal_samp.o: ../../include/local/procfil_temp.h 
r1_cal_samp.o: ../../include/local/procdec.h 
r1_cal_samp.o: /usr/include/math.h /usr/include/ctype.h
r1_ppr.o: ../../include/local/procfil.h 
r1_ppr.o: ../../include/local/procfil_temp.h 
r1_ppr.o: ../../include/local/procdec.h 
r1_ppr.o: ../../include/local/aspdecl.h 
r1_ppr.o: ../../include/local/procpar.h 
r1_ppr.o: ../../include/local/scene_file.h 
r1_ppr.o: ../../include/local/ephem.h 
r1_ppr.o: /usr/include/stdio.h
r1_ppr.o: /usr/include/standards.h /usr/include/math.h
r1_ppr.o: /usr/include/netinet/in.h /usr/include/sys/types.h
r1_scan.o: /usr/include/stdio.h /usr/include/standards.h 
r1_scan.o: /usr/include/math.h
r1_scan.o: ../../include/local/ephem.h 
r1_scan.o: ../../include/local/procfil.h 
r1_scan.o: ../../include/local/procfil_temp.h 
r1_scan.o: ../../include/local/procdec.h 
r1_scan.o: ../../include/local/aspdecl.h 
r1_uts.o: /usr/include/stdio.h /usr/include/standards.h
r1_uts.o: ../../include/local/procfil.h 
r1_uts.o: ../../include/local/procfil_temp.h 
r1_uts.o: ../../include/local/procdec.h 
r1_uts.o: /usr/include/math.h
r1_uts.o: /usr/include/netinet/in.h /usr/include/sys/types.h
rc_uts.o: ../../include/local/procfil.h 
rc_uts.o: ../../include/local/procfil_temp.h 
rc_uts.o: ../../include/local/procdec.h 
rc_uts.o: /usr/include/ctype.h
rc_uts.o: /usr/include/standards.h /usr/include/stdio.h /usr/include/string.h
rc_uts.o: /usr/include/math.h
run_image.o: /usr/include/stdio.h /usr/include/standards.h
run_image.o: /usr/include/unistd.h /usr/include/sys/types.h
run_image.o: /usr/include/sys/access.h /usr/include/time.h
run_image.o: ../../include/local/procfil.h 
run_image.o: ../../include/local/procfil_temp.h 
run_image.o: ../../include/local/procdec.h 
run_image.o: ../../include/local/aspdecl.h 
run_image.o: ../../include/local/procpar.h 
run_image.o: ../../include/local/scene_file.h 
run_image.o: ../../include/local/asp_shm.h 
scan.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/syslog.h
scan.o: ../../include/local/procfil.h 
scan.o: ../../include/local/procfil_temp.h 
scan.o: ../../include/local/aspdecl.h 
scan.o: ../../include/local/asp_msg.h 
sel_seg.o: /usr/include/stdio.h /usr/include/standards.h
sel_seg.o: ../../include/local/procfil.h 
sel_seg.o: ../../include/local/procfil_temp.h 
sel_seg.o: ../../include/local/procdec.h 
sel_seg.o: ../../include/local/aspdecl.h 
sel_seg.o: /usr/include/string.h
sl_to_gl.o: /usr/include/stdio.h /usr/include/standards.h
sl_to_gl.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
sl_to_gl.o: /usr/include/sys/types.h /usr/include/sys/stat.h
sl_to_gl.o: /usr/include/sys/mode.h /usr/include/math.h
st_play.o: /usr/include/stdio.h /usr/include/standards.h
st_play.o: ../../include/local/procfil.h 
st_play.o: ../../include/local/procfil_temp.h 
st_play.o: ../../include/local/procdec.h 
tape_mnt.o: /usr/include/stdio.h /usr/include/standards.h
tape_mnt.o: ../../include/local/procfil.h 
tape_mnt.o: ../../include/local/procfil_temp.h 
tape_mnt.o: ../../include/local/procdec.h 
tape_mnt.o: /usr/include/string.h 
time_uts.o: ../../include/local/procfil.h 
time_uts.o: ../../include/local/procfil_temp.h 
uts.o: /usr/include/stdio.h /usr/include/standards.h /usr/include/stdlib.h
uts.o: ../../include/local/procfil.h 
uts.o: ../../include/local/procfil_temp.h 
uts.o: /usr/include/string.h /usr/include/math.h
write_ceos.o: /usr/include/stdio.h /usr/include/standards.h
write_ceos.o: /usr/include/stdarg.h /usr/include/math.h
write_ceos.o: /usr/include/strings.h /usr/include/string.h
write_ceos.o: /usr/include/ctype.h /usr/include/time.h
write_ceos.o: /usr/include/sys/types.h 
write_ceos.o: ../../include/global/defs.h
write_ceos.o: ../../include/global/oceos.h
write_ceos.o: ../../include/global/extern.h
write_ceos.o: ../../include/local/procfil.h 
write_ceos.o: ../../include/local/procfil_temp.h 
write_ceos.o: ../../include/local/scene_file.h
