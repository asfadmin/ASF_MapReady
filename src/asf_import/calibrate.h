/*Calibration header file.

*/
#ifndef _calibrate_h_
#define _calibrate_h_

#include "asf_meta.h"

typedef enum {
	by_pixel=0, /*Noise indexed by pixel number.   */
	by_slant=1, /*Noise indexed by slant range.    */
	by_look=2,  /*Noise indexed by look angle.     */
	by_geo=3    /*Noise indexed by geocoded pixel. */
} noise_index_type;

typedef enum {
	sigma_naught=0, /* Sigma-0 output desired */
	gamma_naught=1,  /* Gamma-0 output desired */
	beta_naught=2  /* Beta-0 output desired */
} output_correction_type;

typedef struct {

	double a0;		/*Noise scaling coefficient.*/
	double a1;
	double a2;

	double Dmax,Dmin;	/*Max and minumum output sigma-0 values.*/

	int noise_len;
	double noise[1024];	/*Noise values.*/

	noise_index_type noise_type;

	int ns; 		/*Number of input samples.*/
				/*Used if noise_index==by_pixel
				    of if noise_index==by_geo  */

	double minSlant,slantPer;/*Slant range to first pixel/per noise bin.*/
				 /*Used iff noise_index==by_slant*/

	meta_parameters *meta;   /*Used to extract look and incidence angles.*/
			         /*Used if noise_index==by_look  or
					if noise_index==by_geo or
					if  output_type==gamma_naught or
						output_type==beta_naught */

	output_correction_type output_type;

} cal_params;


int slantRange2groundPixel(meta_parameters *sar,double slant);

/* cal_params.c */
double get_noise(cal_params *p,int x,int y);
double get_invCosIncAngle(cal_params *p,int x, int y);
double get_invSinIncAngle(cal_params *p,int x, int y);
cal_params *create_cal_params(const char *inSAR);
float get_cal_dn(cal_params*,double,double,int);
float sprocket_get_cal_dn(cal_params*,double,double,int);
int check_cal(char*);

#endif
