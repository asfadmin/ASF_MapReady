void *init_offset(char *ceos,meta_parameters *i,struct DDR *ddr);

void add_offset(void *l,
	double x,double y,double lat,double lon,double elev);

void refine_offset(void *l,double *out_t,double *out_x);

