#include "ceos.h"
#include "ddr.h"

typedef struct {
	struct HEADER           hdr;
	struct RHEADER          rhdr;
	struct FDR              fdr;
	struct IOF_VFDR		vfdr;
	struct dataset_sum_rec  dssr;
	struct VMPDREC 		mpdr;
	struct pos_data_rec     ppdr;
	struct att_data_rec     atdr;
	struct VRADDR           raddr;
	struct qual_sum_rec     dqsr;
	struct data_hist_rec    sdhr;
	struct data_hist_rec    pdhr;
	struct rng_spec_rec     rsr;
	struct VFDRECV          facdr;
} ceosLeader;

#define DMIN(A, B) ((A) < (B) ? (A) : (B))
#define DMAX(A, B) ((A) > (B) ? (A) : (B))  
#define GILLION     999999999
#define CEOS_LDR	20000
int radio_fill(struct VRADDR *radiometricDataRecord, const char *inName);


