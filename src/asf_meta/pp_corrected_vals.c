#include "asf.h"
#include "ceos.h"
#include "asf_reporting.h"

#include <stdio.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_roots.h>

struct pp_erfin_params {
    int npixels,nlines;
    double satellite_height; /* earth center to satellite height (PP "rsc") */
    double slant_first; /* slant range to first pixel */
    double slant_last; /* slant range to last pixel */
    double nominal_pixsize_range;        
};

static double getObjective(double R, void *params)
{
    struct pp_erfin_params *p =
        (struct pp_erfin_params *)params;

    double H=p->satellite_height;
    double sf=p->slant_first;
    double HHRR=H*H + R*R;
    /* Convert slant_first to ground range */
    double g=R*acos( (HHRR - sf*sf) / (2*H*R)); 
    /* Add width of CEOS image in ground range */
    g += (p->npixels-1) * p->nominal_pixsize_range;
    /* Compute slant range to last pixel */
    double slant_last_R=sqrt(HHRR - 2*H*R*cos(g/R));
    /* Compare with slant_last from CEOS */
    return slant_last_R-p->slant_last;   
}

void pp_get_corrected_vals(char *sarName, double *corrected_earth_radius,
                           double *corrected_azimuth_time_per_pixel)
{
    int status;
    int iter = 0, max_iter = 100;
    const gsl_root_fsolver_type *T;
    gsl_root_fsolver *s;
    gsl_function F;
    struct pp_erfin_params params;

    double nominal_pixsize_azimuth;
    double nadir_radius; /* earth radius at nadir */
    double pp_earth_radius; /* f'd up PP Earth radius from start of swath */
    double seconds_per_azimuth_line; /* PP's real azimuth resolution */

    struct VFDRECV facdr;

    get_asf_facdr(sarName, &facdr);

    /* Find the PP's earth radius with an iterative search */
    F.function = &getObjective;
    F.params = &params;

    params.npixels=facdr.npixels;
    params.nlines=facdr.nlines;
    params.nominal_pixsize_range=facdr.rapixspc;
    nominal_pixsize_azimuth=facdr.azpixspc;
    nadir_radius=1.0e3*facdr.eradnadr;
    params.satellite_height=1.0e3*(facdr.scalt+facdr.eradnadr);
    params.slant_first=1.0e3*facdr.sltrngfp;
    params.slant_last=1.0e3*facdr.sltrnglp;
    
    T = gsl_root_fsolver_brent;
    s = gsl_root_fsolver_alloc (T);
    gsl_root_fsolver_set (s, &F, nadir_radius-1000, nadir_radius+1000.0);

    do {
        ++iter;
        status = gsl_root_fsolver_iterate(s);
        pp_earth_radius = gsl_root_fsolver_root(s);
        status = gsl_root_test_residual(
            getObjective(pp_earth_radius, (void*)&params), 1.0e-4);
    } while (status == GSL_CONTINUE && iter < max_iter);

    if (status == GSL_SUCCESS) {
        //printf("Converged after %d iterations.\n", iter);
        //printf("PP Earth Radius: %.3f m\n",pp_earth_radius);
        //printf("   (for comparison) Nadir Earth Radius: %.3f m\n",nadir_radius);
        *corrected_earth_radius = pp_earth_radius;
    } else {
        asfPrintWarning("Failed to determine PP earth radius!\n"
                        "iter: %d, pp_earth_radius=%.3f, res=%.5f\n"
                        "Proceeding using the nadir radius: %.3f m\n",
                        iter, pp_earth_radius, 
                        getObjective(pp_earth_radius, (void*)&params),
                        nadir_radius);
        *corrected_earth_radius = nadir_radius;
    }

    // Find the PP's per-second azimuth pixel spacing
    seconds_per_azimuth_line=nominal_pixsize_azimuth/facdr.swathvel;
    //printf("PP seconds per azimuth line: %.9f s/line\n",seconds_per_azimuth_line);
    //printf("   (for comparison) PP interpolated lines per second: %.3f lines/s\n",1.0/seconds_per_azimuth_line);
    //printf("   (for comparison) FACDR swath velocity: %.3f m/s\n",facdr.swathvel);
    
    double R=pp_earth_radius;
    double H=params.satellite_height;
    double HHRR=H*H + R*R;
    double slant=0.5*(params.slant_first+params.slant_last);
    double rg_center=R*acos( (HHRR - slant*slant) / (2*H*R)); 
    double vs=sqrt(facdr.scxvel*facdr.scxvel + facdr.scyvel*facdr.scyvel + facdr.sczvel*facdr.sczvel);
    double vsg = vs * pp_earth_radius/params.satellite_height*cos(rg_center/pp_earth_radius);
    
    //printf("   (for comparison) PP-style recalc velocity: %.3f m/s\n",vsg);

    *corrected_azimuth_time_per_pixel = seconds_per_azimuth_line;  
}
