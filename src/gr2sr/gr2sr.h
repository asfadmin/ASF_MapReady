#ifndef _SR2GR_H_
#define _SR2GR_H_

#define MAX_IMG_SIZE 10000

#include <asf.h>
#include <asf_meta.h>
#include <asf_reporting.h>

void gr2sr_vec(meta_parameters *meta, float *gr2sr);
void gr2ml_vec(meta_parameters *meta, float *gr2ml);

#endif
