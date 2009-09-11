#ifndef _CPROJ_H_
#define _CPROJ_H_

#include "asf.h"

#define HALF_PI (PI*0.5)
#define TWO_PI 	(PI*2.0)
#define EPSLN	1.0e-10
#define S2R	4.848136811095359e-6

#define OK	0
#define ERROR  -1
#define IN_BREAK -2

/* Misc macros
  -----------*/
#define SQUARE(x)       x * x   /* x**2 */
#define CUBE(x)     x * x * x   /* x**3 */
#define QUAD(x) x * x * x * x   /* x**4 */

#define GMAX(A, B)      ((A) > (B) ? (A) : (B)) /* assign maximum of a and b */
#define GMIN(A, B)      ((A) < (B) ? (A) : (B)) /* assign minimum of a and b */

#define IMOD(A, B)      (A) - (((A) / (B)) * (B)) /* Integer mod function */

/*External Interfaces are in proj.h*/

/* Projection support routines.*/
/*cproj.c interface:*/
void sincos(double val,double *sin_val,double *cos_val);
double asinz (double con);
double msfnz (double eccent,double sinphi,double cosphi);
double qsfnz (double eccent,double sinphi,double cosphi);

double phi1z (double eccent,double qs,int  *flag);
double phi2z(double eccent,double ts,int *flag);
double phi3z(double ml,double e0,double e1,double e2,double e3,int *flag);
double phi4z (double eccent,double e0,double e1,
	double e2,double e3,double a,double b,double *c,double *phi);

double pakcz(double pak);	/* Angle in alternate packed DMS format	*/
double pakr2dm(double pak);	/* Angle in radians			*/
double tsfnz(double eccent,double phi,double sinphi);
double adjust_lon(double x);		/* Angle in radians			*/
double e0fn(double x);
double e1fn(double x);
double e2fn(double x);
double e3fn(double x);
double e4fn(double x);
double mlfn(double e0,double e1,double e2,double e3,double phi);
int calc_utm_zone(double lon); /*lon in degrees.*/

/*report.c Interface:*/
int init(
int ipr,		/* flag for printing errors (0,1,or 2)		*/
int jpr,		/* flag for printing parameters (0,1,or 2)	*/
char *efile,		/* name of error file				*/
char *pfile)		/* name of parameter file			*/
;
void close_file(void );
void ptitle(char *A) ;
void radius(double A);
void radius2(double A,double B);
void cenlon(double A);
void cenlonmer(double A);
void cenlat(double A);
void origin(double A);
void stanparl(double A,double B);
void stparl1(double A);
void offsetp(double A,double B);
void genrpt(double A, char *S);
void genrpt_long(int A,char *S);
void pblank(void ) ;
void p_error(char *what,char *where);

 int c_ckptid (char *pt_id); 
 char *rem_blanks (register char *str, register int *len); 
 int c_decdeg (double *angle, char *coform, char *type); 
 int c_degdms (double *deg, double *dms, char *code, char *check); 
 double c_eval (int *degree, double *a, double *x, double *y); 
 int grid_coef (double tl, double tr, double bl, double br, double *c0, double *c1, double *c2, double *c3, int dx, int dy); 
 void c_prostr (int *proj_code, char *string); 
 int check_dms (double angle); 

 int sign (double x); 

 void gctp (double *incoor, int *insys, int *inzone, double *inparm, int *inunit, int *indatum, int *ipr, char *efile, int *jpr, char *pfile, double *outcoor, int *outsys, int *outzone, double *outparm, int *outunit, char *fn27, char *fn83, int *iflg); 

 double paksz (double ang, int *iflg); 
 
 
 int proj_err (int err); 
 void proj_print (int proj, int zone, double *par, char *fname, char *mess); 
 void prt_proj_type (char *text, FILE *fp); 
 void prt_proj_gen (double val, char *text, FILE *fp); 
 void prt_proj_dms (double angle, char *text, FILE *fp); 
 void prt_proj_spheroid (double major, double e2, FILE *fp); 
 void prt_proj_rad (double radius, FILE *fp); 
 void prt_false_ne (double east, double north, FILE *fp); 
 void prt_proj_zone (int zone, FILE *fp); 
 void prt_proj_nad (double val, char *text, FILE *fp); 
 void proj_report (int proj, int zone, double *par, char *mess); 
 void proj_type (char *text); 
 void proj_gen (double val, char *text); 
 void proj_dms (double angle, char *text); 
 void proj_spheroid (double major, double e2); 
 void proj_rad (double radius); 
 void false_ne (double east, double north); 
 void proj_zone (int zone); 
 void spcs_zone(int zone);
 void proj_nad (double val); 
 void prt_spcs_zone (int zone, FILE *fp); 
 void report_type (char *text); 
 void report_gen (double val, char *text); 
 void report_deg (double angle, char *text); 
 void report_spheroid (double major, double e2); 
 void report_rad (double radius); 
 void report_zone (int zone); 
 void report_lf (void); 

 int sphdz (int isph, double *parm, double *r_major, double *r_minor, double *radius); 
 int spheroid (int proj, int datum, double *projparms); 
 
 /*Various Map Projection prototypes:*/
 /*A note about these projections:
	They expect latitude and longitude in radians
(not the usual degrees), and the projection coordinates are
(usually) meters.
	To use them more easily, use cproj.h's for_init and inv_init.
*/
 int alberforint (double r_maj, double r_min, double lat1, double lat2, double lon0, double lat0, double false_east, double false_north); 
 int alberfor (double lon, double lat, double *x, double *y); 
 int alberinvint (double r_maj, double r_min, double lat1, double lat2, double lon0, double lat0, double false_east, double false_north); 
 int alberinv (double x, double y, double *lon, double *lat); 
 int alconforint (double r_maj, double r_min, double false_east, double false_north); 
 int alconfor (double lon, double lat, double *x, double *y); 
 int alconinvint (double r_maj, double r_min, double false_east, double false_north); 
 int alconinv (double x, double y, double *lon, double *lat); 
 int azimforint (double r_maj, double center_lon, double center_lat, double false_east, double false_north); 
 int azimfor (double lon, double lat, double *x, double *y); 
 int aziminvint (double r_maj, double center_lon, double center_lat, double false_east, double false_north); 
 int aziminv (double x, double y, double *lon, double *lat); 
 int eqconforint (double r_maj, double r_min, double lat1, double lat2, double center_lon, double center_lat, double false_east, double false_north, int mode); 
 int eqconfor (double lon, double lat, double *x, double *y); 
 int eqconinvint (double r_maj, double r_min, double lat1, double lat2, double center_lon, double center_lat, double false_east, double false_north, int mode); 
 int eqconinv (double x, double y, double *lon, double *lat); 
 int equiforint (double r_maj, double center_lon, double lat1, double false_east, double false_north); 
 int equifor (double lon, double lat, double *x, double *y); 
 int equiinvint (double r_maj, double center_lon, double lat1, double false_east, double false_north); 
 int equiinv (double x, double y, double *lon, double *lat); 
 int geographfor (double lon, double lat, double *x, double *y); 
 int geographinv (double x, double y, double *lon, double *lat); 
 int gnomforint (double r, double center_long, double center_lat, double false_east, double false_north); 
 int gnomfor (double lon, double lat, double *x, double *y); 
 int gnominvint (double r, double center_long, double center_lat, double false_east, double false_north); 
 int gnominv (double x, double y, double *lon, double *lat); 
 int goodforint (double r); 
 int goodfor (double lon, double lat, double *x, double *y); 
 int goodinvint (double r); 
 int goodinv (double x, double y, double *lon, double *lat); 
 int gvnspforint (double r, double h, double center_long, double center_lat, double false_east, double false_north); 
 int gvnspfor (double lon, double lat, double *x, double *y); 
 int gvnspinvint (double r, double h, double center_long, double center_lat, double false_east, double false_north); 
 int gvnspinv (double x, double y, double *lon, double *lat); 
 int hamforint (double r, double center_long, double false_east, double false_north); 
 int hamfor (double lon, double lat, double *x, double *y); 
 int haminvint (double r, double center_long, double false_east, double false_north); 
 int haminv (double x, double y, double *lon, double *lat); 
 int imolwforint (double r); 
 int imolwfor (double lon, double lat, double *x, double *y); 
 int imolwinvint (double r); 
 int imolwinv (double x, double y, double *lon, double *lat); 
 int lamazforint (double r, double center_long, double center_lat, double false_east, double false_north); 
 int lamazfor (double lon, double lat, double *x, double *y); 
 int lamazinvint (double r, double center_long, double center_lat, double false_east, double false_north); 
 int lamazinv (double x, double y, double *lon, double *lat); 
 int lamccforint (double r_maj, double r_min, double lat1, double lat2, double c_lon, double c_lat, double false_east, double false_north); 
 int lamccfor (double lon, double lat, double *x, double *y); 
 int lamccinvint (double r_maj, double r_min, double lat1, double lat2, double c_lon, double c_lat, double false_east, double false_north); 
 int lamccinv (double x, double y, double *lon, double *lat); 
 int merforint (double r_maj, double r_min, double center_lon, double center_lat, double false_east, double false_north); 
 int merfor (double lon, double lat, double *x, double *y); 
 int merinvint (double r_maj, double r_min, double center_lon, double center_lat, double false_east, double false_north); 
 int merinv (double x, double y, double *lon, double *lat); 
 int millforint (double r, double center_long, double false_east, double false_north); 
 int millfor (double lon, double lat, double *x, double *y); 
 int millinvint (double r, double center_long, double false_east, double false_north); 
 int millinv (double x, double y, double *lon, double *lat); 
 int molwforint (double r, double center_long, double false_east, double false_north); 
 int molwfor (double lon, double lat, double *x, double *y); 
 int molwinvint (double r, double center_long, double false_east, double false_north); 
 int molwinv (double x, double y, double *lon, double *lat); 
 int obleqforint (double r, double center_long, double center_lat, double shape_m, double shape_n, double angle, double false_east, double false_north); 
 int obleqfor (double lon, double lat, double *x, double *y); 
 int obleqinvint (double r, double center_long, double center_lat, double shape_m, double shape_n, double angle, double false_east, double false_north); 
 int obleqinv (double x, double y, double *lon, double *lat); 
 int omerforint (double r_maj, double r_min, double scale_fact, double azimuth, double lon_orig, double lat_orig, double false_east, double false_north, double lon1, double lat1, double lon2, double lat2, int mode); 
 int omerfor (double lon, double lat, double *x, double *y); 
 int omerinvint (double r_maj, double r_min, double scale_fact, double azimuth, double lon_orig, double lat_orig, double false_east, double false_north, double lon1, double lat1, double lon2, double lat2, int mode); 
 int omerinv (double x, double y, double *lon, double *lat); 
 int orthforint (double r_maj, double center_lon, double center_lat, double false_east, double false_north); 
 int orthfor (double lon, double lat, double *x, double *y); 
 int orthinvint (double r_maj, double center_lon, double center_lat, double false_east, double false_north); 
 int orthinv (double x, double y, double *lon, double *lat); 
 int polyforint (double r_maj, double r_min, double center_lon, double center_lat, double false_east, double false_north); 
 int polyfor (double lon, double lat, double *x, double *y); 
 int polyinvint (double r_maj, double r_min, double center_lon, double center_lat, double false_east, double false_north); 
 int polyinv (double x, double y, double *lon, double *lat); 
 int psforint (double r_maj, double r_min, double c_lon, double c_lat, double false_east, double false_north); 
 int psfor (double lon, double lat, double *x, double *y); 
 int psinvint (double r_maj, double r_min, double c_lon, double c_lat, double false_east, double false_north); 
 int psinv (double x, double y, double *lon, double *lat); 
 int robforint (double r, double center_long, double false_east, double false_north); 
 int robfor (double lon, double lat, double *x, double *y); 
 int robinvint (double r, double center_long, double false_east, double false_north); 
 int robinv (double x, double y, double *lon, double *lat); 
 int sinforint (double r, double center_long, double false_east, double false_north); 
 int sinfor (double lon, double lat, double *x, double *y); 
 int sininvint (double r, double center_long, double false_east, double false_north); 
 int sininv (double x, double y, double *lon, double *lat); 
 int somforint (double r_major, double r_minor, int satnum, int path, double alf_in, double lon, double false_east, double false_north, double time, int start1, int flag); 
 int somfor (double lon, double lat, double *y, double *x); 
 int sominvint (double r_major, double r_minor, int satnum, int path, double alf_in, double lon, double false_east, double false_north, double time, int start1, int flag); 
 int sominv (double y, double x, double *lon, double *lat); 
 int sterforint (double r_maj, double center_lon, double center_lat, double false_east, double false_north); 
 int sterfor (double lon, double lat, double *x, double *y); 
 int sterinvint (double r_maj, double center_lon, double center_lat, double false_east, double false_north); 
 int sterinv (double x, double y, double *lon, double *lat); 
 int stplnforint (int zone, int sphere, char *fn27, char *fn83); 
 int stplnfor (double lon, double lat, double *x, double *y); 
 int stplninvint (int zone, int sphere, char *fn27, char *fn83); 
 int stplninv (double x, double y, double *lon, double *lat); 
 int utmforint (double r_maj, double r_min, double scale_fact, int zone); 
 int tmforint (double r_maj, double r_min, double scale_fact, double center_lon, double center_lat, double false_east, double false_north); 
 int utmfor (double lon, double lat, double *x, double *y); 
 int tmfor (double lon, double lat, double *x, double *y); 
 int utminvint (double r_maj, double r_min, double scale_fact, int zone); 
 int tminvint (double r_maj, double r_min, double scale_fact, double center_lon, double center_lat, double false_east, double false_north); 
 int utminv (double x, double y, double *lon, double *lat); 
 int tminv (double x, double y, double *lon, double *lat); 
 int untfz (int inunit, int outunit, double *factor); 
 int vandgforint (double r, double center_long, double false_east, double false_north); 
 int vandgfor (double lon, double lat, double *x, double *y); 
 int vandginvint (double r, double center_long, double false_east, double false_north); 
 int vandginv (double x, double y, double *lon, double *lat); 
 int wivforint (double r, double center_long, double false_east, double false_north); 
 int wivfor (double lon, double lat, double *x, double *y); 
 int wivinvint (double r, double center_long, double false_east, double false_north); 
 int wivinv (double x, double y, double *lon, double *lat); 
 int wviiforint (double r, double center_long, double false_east, double false_north); 
 int wviifor (double lon, double lat, double *x, double *y); 
 int wviiinvint (double r, double center_long, double false_east, double false_north); 
 int wviiinv (double x, double y, double *lon, double *lat); 

#endif
