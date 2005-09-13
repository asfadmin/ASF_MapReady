/*Header file for metadata
display (print) routines.*/

 void usage (char *name); 
 void write_to_file (char *exe, char *rectypes, char *infile); 
 
 void prn_atdr (struct att_data_rec *a); 
 void prn_dhr (struct data_hist_rec* h);
 void prn_dqsr (struct qual_sum_rec *q, int era); 
 void prn_dssr (struct dataset_sum_rec *ds, int era); 
 void prn_facdr (struct VFDRECV *ofdr, int era); 
 void prn_ifiledr (struct IOF_VFDR *ifdr); 
 void prn_mpdr (struct VMPDREC *vmpdr); 
 void prn_ppdr (struct pos_data_rec *p); 
 void prn_raddr (struct VRADDR *dr); 
 void prn_rsr (struct rng_spec_rec *r);
 void prn_fdr (struct FDR *fdr);
