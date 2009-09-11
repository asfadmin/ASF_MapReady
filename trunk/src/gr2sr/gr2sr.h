#ifndef _SR2GR_H_
#define _SR2GR_H_

#define MAX_IMG_SIZE 100000

#include <asf.h>
#include <asf_meta.h>

/**
 Compute a resampling vector gr2sr such that:
     ground pixel = gr2sr [ slant pixel ];
 for the given slant range pixel increment.
*/
void gr2sr_vec(meta_parameters *meta,float srinc, float *gr2sr);

#endif
