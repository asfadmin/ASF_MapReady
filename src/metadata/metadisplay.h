/*Header file for metadata
display (print) routines.*/

void usage (char *name); 
void write_to_file (char *exe, char *rectypes, char *infile); 

void prn_atdr(FILE *fp, struct att_data_rec *a); 
void prn_dhr(FILE *fp, struct data_hist_rec* h);
void prn_dqsr(FILE *fp, struct qual_sum_rec *q, int era); 
void prn_dssr(FILE *fp, struct dataset_sum_rec *ds, int era); 
void prn_facdr(FILE *fp, struct VFDRECV *ofdr, int era); 
void prn_ifiledr(FILE *fp, struct IOF_VFDR *ifdr); 
void prn_mpdr(FILE *fp, struct VMPDREC *vmpdr); 
void prn_ppdr(FILE *fp, struct pos_data_rec *p); 
void prn_raddr(FILE *fp, struct VRADDR *dr); 
void prn_rsr(FILE *fp, struct rng_spec_rec *r);
void prn_fdr(FILE *fp, struct FDR *fdr);
void prn_esa_facdr(FILE *fp, struct ESA_FACDR *f);
void prn_rcdr(FILE *fp, struct radio_comp_data_rec *rc);
void prn_ppr(FILE *fp, struct PPREC *p);
void prn_shr(FILE *fp, struct scene_header_rec *sh);
void prn_ampr(FILE *fp, struct alos_map_proj_rec *mp);

