/******************************************************************************
NAME:  sr2gr - converts slant range images to ground range images

DESCRIPTION:
	This program converts slant range imagery into ground range imagery. 
	The algorithm calculates the ground range to the first pixel in the
	image and from the spacecraft ephemeris, earth ellipsoid, and slant
	range to first pixel given in the image's metadata.  It then uses the
	slant range spacing interval to determine appropriate ground range
	positions.  The remapping is performed using bi-linear interpolation.
*/

#include "asf.h"
#include "asf_meta.h"
#include "asf_sar.h"
#include "asf_raster.h"

#include <gsl/gsl_multifit.h>

#define FUDGE_FACTOR 2

/*Create vector for multilooking.*/
static void ml_vec(float oldSize, float newSize,float *ml)
{
	float  gr=0;
	int    ii;
	
	for (ii=0; ii<MAX_IMG_SIZE; ii++)
	{
		ml[ii]=gr/oldSize;
		gr+=newSize;
	}
}

/*
    This subroutine constructs the slantrange to groundrange interpolation
    vector.
 
        Input:      ht = distance from Earth center to SAR in meters  (real*4)
                    sr = slant range distance to first interpolation point.
                         This may be a point different from the slant range
                         to the first sampled point due to range migration.
                         Units = meters. (real*4)
                    re = radius of Earth at center swath (mid range) in meters.
                         (real*4)
                 srinc = slant range increment in meters (real*4). This
                         determined by the SAR's A/D sampling rate.
                 grinc = ground range increment in meters (real*4)
                         For ASF = 12.5meters
 
        Output:    sr2gr = 8k (real*4) vector that contains the interpolation
                          points for slant range to ground range conversion.
                          The first element is always 0, which means the first
                          interpolation point is at r_close.  This vector
                          is in units of slant range bins.
 
        Algorithm:  The local Earth surface (within image) is approximated
                    by a sphere of radius r_earth.  This radius may be the
                    local Earth radius as determined by a higher order model,
                    such as an ellipsoid.  (Deviations between the elliptical
                    surface and the sphere, for the purpose of this resampling,
                    are small.)  The following equations are used to determine
                    the resampling vector sr2gr and are inverses of each other:
 
    slant range (sr) as a function of ground range (rg):
 
        sr=sqrt(ht*ht+re*re-2*ht*re*cos(rg/re));
        
    ground range (rg) as a function of slant range (sr):
 
	    rg=re*acos((ht*ht+re*re-sr*sr)/(2*ht*re));
 
    First the ground range at the close slant range is determined from the
    first equation above.  This ground range, plus the enumerated increment
    (grinc) is used to calculate the slant range to the interpolation points.
    Finally the interpolation points are normalized to slant range bins by
    dividing by the slant range bin size, rsinc.
*/

static void sr2gr_vec(meta_parameters *meta, float srinc, float newSize,
                      float *sr2gr)
{
    double rg,rg0;/*Ground range distances from nadir, along curve of earth.*/
    double ht,re,sr;/*S/C height, earth radius, slant range [m]*/
    int    ii;
    
    ht = meta_get_sat_height(meta, 0, 0);
    re = meta_get_earth_radius(meta, 0, 0);
    sr = meta_get_slant(meta,0,0);

    /* calculate ground range to first point */
    rg0 = re * acos((ht*ht+re*re-sr*sr) / (2*ht*re));
    
    /* begin loop */
    rg = rg0;
    for (ii = 0; ii<MAX_IMG_SIZE; ii++)
    {
        double this_slant = sqrt(ht*ht+re*re-2.0*ht*re*cos(rg/re));
        sr2gr[ii] = (this_slant - sr) / srinc;
        rg += newSize;
    }
}

int sr2gr(const char *infile, const char *outfile)
{
    return sr2gr_pixsiz(infile, outfile, -1);
}

static void update_doppler(int in_np, int out_np, float *sr2gr, meta_parameters *meta)
{
          // Update the doppler
          // In going from slant to ground, and potentially changing the width,
          // we will likely change the shape of the doppler polynomial.  So, solve
          // for the coefficients of the new polynomial.  Ignore the constant term
          //   v1 = d1*x1 + d2*x1*x1   <-- d1,d2 doppler coefs
          //   v2 = d1*x2 + d2*x2*x2   <-- x1,x2 are image center and right
          // New poly:
          //   v1 = a*y1 + b*y1*y1     <-- a,b new doppler coefs
          //   v2 = a*y2 + b*y2*y2     <-- y1,y2 are image center and right in new geom
          // Solving for b:
          //   b = (v1*y2 - v2*y1)/(y2*y1^2 - y1*y2^2)
          // Then a:
          //   a = (v1 - b*y1*y1)/y1
          double is2 = in_np-1;         // Input sample 2  ( x2 in the above )
          double os1 = out_np/2;        // Output sample 1 ( y1 in the above )
          double is1 = sr2gr[(int)os1]; // Input sample 1  ( x1 in the above )
          double os2 = out_np-1;        // Output sample 2 ( y2 in the above )

          double d1 = meta->sar->range_doppler_coefficients[1];
          double d2 = meta->sar->range_doppler_coefficients[2];

          double v1 = d1*is1 + d2*is1*is1;
          double v2 = d1*is2 + d2*is2*is2;

          double b = (v1*os2 - v2*os1)/(os1*os1*os2 - os2*os2*os1);
          double a = (v1 - b*os1*os1)/os1;

          // Debug code: print out the doppler across the image
          // a and b will be the starting points for a least squares fit
          const int N=1000;
          double chisq, xi[N], yi[N];
          int ii;
          for (ii=0; ii<N; ++ii) {
             xi[ii] = (double)ii/(double)N * (double)(out_np-1);
             // the sr2gr array maps a ground range index to a slant range index
             double s = sr2gr[(int)xi[ii]];
             yi[ii] = d1*s + d2*s*s;
          }

          gsl_matrix *X, *cov;
          gsl_vector *y, *w, *c;

          X = gsl_matrix_alloc(N,3);
          y = gsl_vector_alloc(N);
          w = gsl_vector_alloc(N);

          c = gsl_vector_alloc(3);
          cov = gsl_matrix_alloc(3, 3);

          for (ii=0; ii<N; ++ii) {
            gsl_matrix_set(X, ii, 0, 1.0);
            gsl_matrix_set(X, ii, 1, xi[ii]);
            gsl_matrix_set(X, ii, 2, xi[ii]*xi[ii]);

            gsl_vector_set(y, ii, yi[ii]);
            gsl_vector_set(w, ii, 1.0);
          }

          gsl_multifit_linear_workspace *work = gsl_multifit_linear_alloc(N, 3);
          gsl_multifit_wlinear(X, w, y, c, cov, &chisq, work);
          gsl_multifit_linear_free(work);

          double c0 = gsl_vector_get(c, 0);
          double c1 = gsl_vector_get(c, 1);
          double c2 = gsl_vector_get(c, 2);

          gsl_matrix_free(X);
          gsl_vector_free(y);
          gsl_vector_free(w);
          gsl_vector_free(c);
          gsl_matrix_free(cov);
        
          // now the x and y vectors are the desired doppler polynomial

          double ee1=0, ee2=0;
          for (ii=0; ii<out_np; ii+=100) {

            // ii: index in ground range, s: index in slant range 
            double s = sr2gr[ii];

            // dop1: doppler in slant, dop2: doppler in ground (should agree)
            double dop1 = d1*s + d2*s*s;
            double dop2 = a*ii + b*ii*ii;
            double dop3 = c0 + c1*ii + c2*ii*ii;

            double e1 = fabs(dop1-dop2);
            double e2 = fabs(dop1-dop3);

            ee1 += e1;
            ee2 += e2;

            if (ii % 1000 == 0)
              printf("%5d -> %8.3f %8.3f %8.3f   %5d -> %5d   %7.4f %7.4f\n",
                     ii, dop1, dop2, dop3, (int)s, ii, e1, e2);

          }

          printf("Original: %14.9f %14.9f %14.9f\n", 0., d1, d2);
          printf("First   : %14.9f %14.9f %14.9f\n", 0., a, b);
          printf("Second  : %14.9f %14.9f %14.9f\n\n", c0, c1, c2);

          printf("Overall errors: %8.4f %8.4f\n", ee1, ee2);

          meta->sar->range_doppler_coefficients[0] += c0;
          meta->sar->range_doppler_coefficients[1] = c1;
          meta->sar->range_doppler_coefficients[2] = c2;
}

int sr2gr_pixsiz(const char *infile, const char *outfile, float grPixSize)
{
	int    in_np,  in_nl;               /* input number of pixels,lines  */
	int    out_np, out_nl;              /* output number of pixels,lines */
	int    ii,line,band;
	float  oldX,oldY;
	float  sr2gr[MAX_IMG_SIZE];
	float  ml2gr[MAX_IMG_SIZE];
	int    a_lower[MAX_IMG_SIZE];
	int    lower[MAX_IMG_SIZE], upper[MAX_IMG_SIZE];
	float  a_ufrac[MAX_IMG_SIZE], a_lfrac[MAX_IMG_SIZE];
	float  ufrac[MAX_IMG_SIZE], lfrac[MAX_IMG_SIZE];
	float *ibuf1,*ibuf2,*obuf;
	char   infile_name[512],inmeta_name[512];
	char   outfile_name[512],outmeta_name[512];
	FILE  *fpi, *fpo;
	meta_parameters *in_meta;
	meta_parameters *out_meta;

        create_name (infile_name, infile, ".img");
        create_name (outfile_name, outfile, ".img");

        create_name (inmeta_name, infile, ".meta");
        create_name (outmeta_name, outfile, ".meta");

	in_meta  = meta_read(inmeta_name);
	out_meta = meta_copy(in_meta);
	in_nl = in_meta->general->line_count;
	in_np = in_meta->general->sample_count;
        float ss = in_meta->sar->slant_shift;
        in_meta->sar->slant_shift = out_meta->sar->slant_shift =  0;

      	oldX = in_meta->general->x_pixel_size * in_meta->sar->sample_increment;
	oldY = in_meta->general->y_pixel_size * in_meta->sar->line_increment;

        /* If user didn't specify a pixel size, make the pixels square & leave
           the y pixel size unchanged */
        if (grPixSize < 0)
            grPixSize = oldY;

        printf("Entering sr2gr_pixsiz\n");
        printf("\tinfile %s\n",infile);
        printf("\toutfile %s\n",outfile);
        printf("\tgrPixSize %f\n",grPixSize);

	printf("Creating sr2gr with pixsize %f\n",grPixSize);

	if (in_meta->sar->image_type != 'S')
	{
            asfPrintError("sr2gr only works with slant range images!\n");
	}

	/*Update metadata for new pixel size*/
	out_meta->sar->time_shift  += ((in_meta->general->start_line)
				* in_meta->sar->azimuth_time_per_pixel);
	out_meta->sar->slant_shift -= ((in_meta->general->start_sample)
				* in_meta->general->x_pixel_size);
	out_meta->general->start_line   = 0.0;
	out_meta->general->start_sample = 0.0;
	out_meta->sar->azimuth_time_per_pixel *= grPixSize
					/ in_meta->general->y_pixel_size;
	out_meta->sar->line_increment   = 1.0;
        out_meta->sar->sample_increment = 1.0;
        if (out_meta->transform)
          out_meta->transform->target_pixel_size = grPixSize;	
	/*Create ground/slant and azimuth conversion vectors*/
	out_meta->sar->image_type       = 'G'; 
	out_meta->general->x_pixel_size = grPixSize;
	out_meta->general->y_pixel_size = grPixSize;
	sr2gr_vec(out_meta,oldX,grPixSize,sr2gr);
	ml_vec(oldY,grPixSize,ml2gr);

	out_np = MAX_IMG_SIZE;
	out_nl = MAX_IMG_SIZE;
	for (ii=MAX_IMG_SIZE-1; ii>0; ii--)
		if ((int)sr2gr[ii] > in_np)
			out_np = ii;
	for (ii=MAX_IMG_SIZE-1; ii>0; ii--)
		if ((int)ml2gr[ii] > in_nl)
			out_nl = ii;

	out_meta->general->line_count   = out_nl;
        out_meta->general->line_scaling *= (double)in_nl/(double)out_nl;
        out_meta->general->sample_scaling = 1;
	out_meta->general->sample_count = out_np;
	if (out_meta->projection) {
		out_meta->projection->perX = grPixSize;
		out_meta->projection->perY = grPixSize;
	}

        if (out_meta->sar){
          update_doppler(in_np, out_np, sr2gr, out_meta);
        }

        out_meta->sar->slant_shift = ss;
	meta_write(out_meta,outmeta_name);
	
	fpi = fopenImage(infile_name,"rb");
	fpo = fopenImage(outfile_name,"wb");
	
	for (ii=0; ii<MAX_IMG_SIZE; ii++)
	{
		lower[ii] = (int) sr2gr[ii];
		upper[ii] = lower[ii] + 1;
		ufrac[ii] = sr2gr[ii] - (float) lower[ii];
		lfrac[ii] = 1.0 - ufrac[ii]; 
		
		a_lower[ii] = (int) ml2gr[ii];
		a_ufrac[ii] = ml2gr[ii] - (float) a_lower[ii];
		a_lfrac[ii] = 1.0 - a_ufrac[ii]; 
	}

	ibuf1 = (float *) MALLOC ((in_np+FUDGE_FACTOR)*sizeof(float));
	ibuf2 = (float *) MALLOC ((in_np+FUDGE_FACTOR)*sizeof(float));
	obuf = (float *) MALLOC (out_np*sizeof(float));

	/* Initialize input arrays to 0 */
	for (ii=0;ii<in_np+FUDGE_FACTOR;ii++) {
		ibuf1[ii]=ibuf2[ii]=0.0;
	}

        /* Get the band info */
        int bc = in_meta->general->band_count;
        char **band_name = extract_band_names(in_meta->general->bands, bc);

	/* Work dat magic! */
        for (band=0; band<bc; ++band) {
          asfPrintStatus("Working on band: %s\n", band_name[band]);
          for (line=0; line<out_nl; line++)
          {
            if (a_lower[line]+1 < in_nl)
            {
              get_band_float_line(fpi,in_meta,band,a_lower[line],  ibuf1);
              get_band_float_line(fpi,in_meta,band,a_lower[line]+1,ibuf2);
            }
            
            for (ii=0; ii<out_np; ii++)
            {
              float val00,val01,val10,val11,tmp1,tmp2;
              val00 = ibuf1[lower[ii]];
              val01 = ibuf1[upper[ii]];
              val10 = ibuf2[lower[ii]];
              val11 = ibuf2[upper[ii]];
              
              tmp1 = val00*lfrac[ii] + val01*ufrac[ii];
              tmp2 = val10*lfrac[ii] + val11*ufrac[ii];
              
              obuf[ii] = tmp1*a_lfrac[line] + tmp2*a_ufrac[line];
            }
            put_band_float_line(fpo,out_meta,band,line,obuf);
            asfLineMeter(line, out_nl);
          }
        }
        for (band=0; band<bc; ++band)
          FREE(band_name[band]);
        FREE(band_name);
        meta_free(in_meta);
        meta_free(out_meta);
	FCLOSE(fpi);
	FCLOSE(fpo);
	
        return TRUE;
}

