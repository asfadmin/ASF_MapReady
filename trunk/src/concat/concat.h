

void averopt (int *laver, int (*out_window)[4], int *limage); 

void  cal_lin_smp (char **hostin, int *total_cnt, int *in_count, int *numimg, 
 	int (*window)[4], float projtol, int *ulcoors_flag, int *outsize_flag, 
 	double *ulcoors, double *outsize, int *sl, int *ss, int *nl, int *ns, 
 	int (*out_window)[4]); 

void overlay_img (char *hostin, int nmrimg, int (*wind)[4], int (*bands)[MAXBND+1], 
 	int *nbands, int *sl, int *ss, float *mask, char *hostout, int *dtype, 
 	int *totbnd, int *nl, int *ns, int replace, int leave, int aver, 
 	int *update, int *laver); 

void pad (char *hostout, int *nl, int *ns, int *totbnd, int *dtype, float *mask); 

void set_dtype (char **hostin, int image_count, int *out_dtype/*, float maskval, int *in_count*/); 

extern  struct FDESC **fdesc;	 /* image file descriptors	              */
extern  struct GDESC *gdesc; /* group descriptor 	                      */

