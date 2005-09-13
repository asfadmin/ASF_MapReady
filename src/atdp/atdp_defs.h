/****************************************************************************
*								            *
*   aisp_def.h - Global type, function, and constants definitions            *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
/* Global Definitions for ASP software - T. Logan 8/96*/

#ifndef __ASPMATH_H     /* include only once */
#define __ASPMATH_H

/*-----------------------------------------------------*/
/* define complex variable type if not already defined */
/*-----------------------------------------------------*/
#include "asf_complex.h"
#include "read_signal.h"
#include "geolocate.h"
#include "asf_meta.h"

/*-----------------------------*/
/* Simple function definitions */
/*-----------------------------*/
#define   NINT(a)       ((a)>=0.0 ? (int)((a)+0.5):(int)((a)-0.5)) /* nearest int */
#define   MIN(a,b)      (((a) < (b)) ? (a) : (b))                    /* minimum     */
#define   MAX(a,b)      (((a) > (b)) ? (a) : (b))                    /* maximum     */

/*-----------------------*/
/* Constants Definitions */
/*-----------------------*/
#define   VERSION       3.1
#define   default_n_az 4096

#define   speedOfLight 299792458.0 /*Speed of light in vacuum, m/s */
#define   pi      	3.14159265358979323
#define   pi2     	(2*pi)
/* Flags for calibration */
#define SIGMA_0 2
#define GAMMA_0 3
#define BETA_0 4
/* Flags for debugging */
#define AZ_X_T 2
#define AZ_X_F 4
#define AZ_REF_F 8
#define AZ_REF_T 16
#define AZ_MIG_F 32
#define AZ_RAW_F 64
#define AZ_RAW_T 128
#define RANGE_REF_MAP 256
#define RANGE_X_F 512
#define RANGE_REF_F 1024
#define RANGE_REF_T 2048
#define RANGE_RAW_F 4096
#define RANGE_RAW_T 8192
#define NO_RANGE 16384
#define NO_RCM 32768
#define NO_AZIMUTH 65536


/*-------------Structures:---------------
file: parameters describing output file.
*/
typedef struct {
  char in[255]; /*Input file.*/
  char out_cpx[255],out_amp[255]; /*Complex and Amplitude output names.*/
  char out_pwr[255], out_sig[255], out_gam[255], out_bet[255];  /*Power or RCS (dB) 
								  output names.*/
  float azpix,rngpix; /*Azimuth and range pixel size.*/
  int firstLineToProcess,firstOutputLine,skipFile,skipSamp;
  int n_az_valid,nlooks;
  int nPatches;
} file;


/*-------------Initialization:---------------*/
#include "aisp_params.h"
int parse_cla(int argc,char *argv[],struct AISP_PARAMS *g,meta_parameters **meta_out);
void aisp_setup(struct AISP_PARAMS *g,meta_parameters *meta,file **f,
		getRec **signalGetRec);

#endif
