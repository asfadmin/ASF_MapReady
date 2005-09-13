#include "asf.h"
#include "asf_meta.h"

/* DEFINITIONS:
 * Ground Range: Distance measured along curve of earth, at sea level
 *                 (0 elevation).
 * Phi:          Smallest angle made between line connecting point on earth's
 *                 surface with earth's center and line connecting satellite and
 * 	        earth's center.
 * Slant Range:  Straight-line distance between satellite and point on earth's
 *                 surface.
 */

#define phi2grX(phi) (((phi)-minPhi)*phiMul)
#define grX2phi(gr) (minPhi+(gr)/phiMul)

extern float badDEMht;
extern int maxBreakLen;

extern double grPixelSize;
extern int gr_ns,sr_ns;

/*Array[gr_ns] indexed by ground range pixel.*/
extern double *slantGR;/*Slant range pixel #*/
extern double *heightShiftGR;

/*Array[sr_ns] indexed by slant range pixel.*/
extern double *heightShiftSR;
extern double *groundSR;/*Ground range pixel #*/
extern double *slantRangeSqr,*slantRange,*heightShift;
extern double *incidAng,*sinIncidAng,*cosIncidAng;

double calc_ranges(meta_parameters *meta);

void geo_compensate(float *srDEM,float *in,float *out,int ns);
void radio_compensate(float *grDEM, float *grDEMprev,float *inout,int ns);
void dem_sr2gr(float *inBuf,float *outBuf);
void dem_gr2sr(float *grDEM, float *srDEM,float *amp);
void dem_interp_col(float *buf,int ns,int nl);

float sr2gr(float srX,float height);
float gr2sr(float grX,float height);
