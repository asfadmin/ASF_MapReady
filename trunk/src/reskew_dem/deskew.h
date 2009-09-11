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
extern int gr_ns, sr_ns;

void geo_compensate(float *srDEM,float *in,float *out,int ns);
void radio_compensate(float *grDEM, float *grDEMprev,float *inout,int ns);
void dem_sr2gr(float *inBuf,float *outBuf);
void dem_gr2sr(float *grDEM, float *srDEM,float *amp);
void dem_interp_col(float *buf,int ns,int nl);

/*
 Turn an x pixel coordinate srX in the source DEM,
    measured in slant range to earth ellipsoid (height==0)
    [ slant range Ellipsoid]
 Into an x pixel in the SAR image, measured in 
    slant range to earth ellipsoid at this height
    [ slant range at terrain height ]
*/
float srE2srH(float srE,float height);

