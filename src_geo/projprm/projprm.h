/* Header file for ProjPrm.
   Contains function declarations as well as version information.
*/

#ifndef __PROJPRM_H__
#define __PROJPRM_H__
#include "cproj.h"
#include "proj.h"

/* constant declarations */
extern float VERSION;

/* function declarations */

void geograph (int nargs, char **args, int *prjzone, int *prjtype, int *prjsph, double *prjparms); 
void plstereo (int nargs, char **args, int *prjzone, int *prjtype, int *prjsph, double *prjparms); 
void print_plstereo_usg (void);  
void utm (int nargs, char **args, int *prjzone, int *prjtype, int *prjsph, double *prjparms); 
void print_utm_usg (void); 
void lamazeqa (int nargs, char **args, int *prjzone, int *prjtype, int *prjsph, double *prjparms);
void print_lamazeqa_usg (void);
void albers (int nargs, char **args, int *prjzone, int *prjtype, int *prjsph, double *prjparms);
void print_albers_usg(void);
void wrlabtab (char *hosout, char *prjkey, int *prjunits, int *prjzone, int *prjtype, int *prjsph, double *prjparms); 


#endif

