/***************************************************************************
  NAME: asf_sar.h

  DESCRIPTION: Header file for asf_sar functions. 

               Any constants required for asf_sar.a functions should also
               be defined here.

  AUTHOR:  Mike Shindle, August 21, 1995.
	   Tom Logan, May 1996.
	   Mike Shindle, May 1996.
***************************************************************************/

#ifndef __ASF_SAR_H

/* include this header file only once */
#define __ASF_SAR_H

#ifndef __CEOS_H
/* must include ceos.h */
#include "ceos.h"
#endif

#ifndef M_PI
	#define M_PI 3.14159265358979323
#endif

#define d_r (M_PI/180.0)
#define r_d (180.0/M_PI)

/* all system definitions */
#define asfPRE_RADARSAT       0
#define asfRADARSAT           1
#define asfRADARSAT_COLHDR    192
#define asfOLD_COLHDR         12
#define asfRADARSAT_ROWHDR    1
#define asfOLD_ROWHDR         1

/* set era constants. These are passed as opflag. see set_era(). */
#define asfSE_UNKNOWN         -1
#define asfSE_DATFILE         0
#define asfSE_LDRFILE         1
#define asfSE_TRLFILE         2


/* function declarations */

char **alloc2d_1(int,int,int);
void ***alloc3d_1(int,int,int);
int check_cal(char *filename);
int create_ddr(char *,int,int,double,int);

int    get_I4(char *, int);
double get_F4(char *, int);

void   nullify(char *, int);

double make_east_deg(double);
double *midpoint(double *,double *);
char   *trim(char *);
double packed_deg(double);
int set_era(char *,char *,int);
void StartWatch(void);
void StopWatch(void);
void ex_corners(
	char	*filename,
	double	*ul_lat,double	*ul_lon,double	*lr_lat,double	*lr_lon);
void make_valid_DDR(
	struct VMPDREC *mpdr,     /* Map Projection Data Record */
	char *hosout,             /* HOST name */
	int odtype,              /* data type */
	int nbands              /* number of bands */);

void prn_dhr(struct data_hist_rec* h);

#endif  /* End include file */

