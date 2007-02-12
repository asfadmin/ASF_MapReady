/*Header file for metadata
display (print) routines.*/

int check_record(char *fileName, int reqrec);
void print_record(FILE *fp, char *fileName, int reqrec);
void output_record(char *fileName, char *extension, int rec, int save);
char *get_record_as_string(char *fileName, int reqrec);

void add(char **s, const char *format, ...);
void write_to_file (char *exe, char *rectypes, char *infile); 

char *sprn_shr(struct scene_header_rec *sh);
void prn_shr(FILE *fp, struct scene_header_rec *sh);

char *sprn_atdr(struct att_data_rec* a);
void prn_atdr(FILE *fp, struct att_data_rec *a); 

char *sprn_dhr(struct data_hist_rec* h);
void prn_dhr(FILE *fp, struct data_hist_rec* h);

char *sprn_dqsr(struct qual_sum_rec* q, int era);
void prn_dqsr(FILE *fp, struct qual_sum_rec *q, int era); 

char *sprn_dssr(struct dataset_sum_rec *ds, int era);
void prn_dssr(FILE *fp, struct dataset_sum_rec *ds, int era); 

char *sprn_facdr(struct VFDRECV *ofdr, int era);
void prn_facdr(FILE *fp, struct VFDRECV *ofdr, int era); 

char *sprn_ifiledr(struct IOF_VFDR *ifdr);
void prn_ifiledr(FILE *fp, struct IOF_VFDR *ifdr); 

char *sprn_mpdr(struct VMPDREC *vmpdr);
void prn_mpdr(FILE *fp, struct VMPDREC *vmpdr); 

char *sprn_ppdr(struct pos_data_rec *p);
void prn_ppdr(FILE *fp, struct pos_data_rec *p); 

char *sprn_raddr(struct VRADDR *dr);
void prn_raddr(FILE *fp, struct VRADDR *dr); 

char *sprn_rsr(struct rng_spec_rec *r);
void prn_rsr(FILE *fp, struct rng_spec_rec *r);

char *sprn_fdr(struct FDR *fdr);
void prn_fdr(FILE *fp, struct FDR *fdr);

char *sprn_esa_facdr(struct ESA_FACDR *f);
void prn_esa_facdr(FILE *fp, struct ESA_FACDR *f);

char *sprn_rcdr(struct radio_comp_data_rec *rc);
void prn_rcdr(FILE *fp, struct radio_comp_data_rec *rc);

char *sprn_ppr(struct PPREC *p);
void prn_ppr(FILE *fp, struct PPREC *p);

char *sprn_ampr(struct alos_map_proj_rec *mp);
void prn_ampr(FILE *fp, struct alos_map_proj_rec *mp);

char *sprn_ardr(struct alos_rad_data_rec *dr);
void prn_ardr(FILE *fp, struct alos_rad_data_rec *dr);
