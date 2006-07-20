/* Cal_params:
	Fetches noise and scaling factors from
input CEOS image, and returns them to
calibrate.*/


#include "asf.h"
#include "ceos.h"
#include "calibrate.h"

/**Harcodings to fix calibration of ASF data***
 --------------------------------------------
   Currently contains:

        double sspswb010_noise_vec[256];
        double sspswb011_noise_vec[256];
        double sspswb013_noise_vec[256];
        double sspswb014_noise_vec[256];
        double sspswb015_noise_vec[256];
	double sspswb016_noise_vec[256]; (identical to 15)

*********************************************/
#include "noise_vectors.h"
#define LOVAL 0.0

/**************************************************************************
get_noise

Returns the radiometric noise value at
the given image location.

**************************************************************************/

double get_noise(cal_params *p,int x,int y)
{
	float noise_index=0,frac;
	int index;

/*Switch on the indexing scheme of the noise table.*/
	if (p->noise_type==by_pixel)
		noise_index=(float)x*p->noise_len/p->ns;
	else if (p->noise_type==by_slant)
		noise_index=(meta_get_slant(p->meta,(float)y,(float)x)
			-p->minSlant)/p->slantPer;
	else if (p->noise_type==by_look)
		noise_index=(meta_look(p->meta,
			(float)y,(float)x)*180.0/PI-16.3)*10.0;
	else if (p->noise_type==by_geo)
	  {
	    double time,slant,dop;
	    int    pixel;

	    meta_get_timeSlantDop(p->meta,(float)y,(float)x,
			    &time,&slant,&dop);
	    pixel = slantRange2groundPixel(p->meta,slant);
	    noise_index = (float) pixel*p->noise_len/p->ns;
	  }

/*Clamp noise_index to within the noise table.*/
	if (noise_index<=0)
		return p->noise[0];
	if (noise_index>=p->noise_len-1)
		return p->noise[p->noise_len-1];

/*Use linear interpolation on noise array.*/
	index=(int)noise_index;
	frac=noise_index-index;
	return p->noise[index]+frac*(p->noise[index+1]-p->noise[index]);
}

double get_invCosIncAngle(cal_params *p,int x, int y)
  { return( 1.0 / cos(meta_incid(p->meta,(float)y,(float)x))); }

double get_invSinIncAngle(cal_params *p,int x, int y)
  { return( 1.0 / sin(meta_incid(p->meta,(float)y,(float)x))); }

/**************************************************************************
create_cal_params

Constructs a cal_params record, by reading the
given inSAR CEOS file.

**************************************************************************/

#define ERROR_IMAGEID   7000

cal_params *create_cal_params(const char *inSAR)
{
        int i;
        cal_params *p=(cal_params *)MALLOC(sizeof(cal_params));
        struct VFDRECV         facdr;   /* Facility-related data record.*/
        struct VRADDR          rdr;     /* Radiometric data record      */
        struct dataset_sum_rec dssr;    /* Data set summary record.     */
        struct VMPDREC         mpdr;    /* Map projection data record.  */
        double  *noise_vector;          /* Noise vector pointer         */
        char sarName[512];

        strcpy (sarName,inSAR);

        /* Check for FOCUS data and directly return because we cannot handle
           them yet */
        get_dssr(sarName, &dssr);
        if (strcmp(dssr.product_type, "FOCUS")==0) return NULL;

        p->meta=meta_init(sarName);

        /* Get values for calibration coefficients and LUT */
        get_raddr(sarName, &rdr);

        /* hardcodings for not-yet-calibrated fields */
        if (rdr.a[0] == -99.0 || rdr.a[1]==0.0 ) {
                p->a0 = 1.1E4;
                p->a1 = 2.2E-5;
                p->a2 = 0.0;
        } else {
                p->a0 = rdr.a[0];
                p->a1 = rdr.a[1];
                p->a2 = rdr.a[2];
        }

        /* Set Default values
         --------------------*/
        p->Dmax = 0.0;
        p->Dmin = -30.0;
        p->noise_len=256;
        p->output_type=sigma_naught;

        get_dssr(sarName, &dssr);

        /* Set the Noise Correction Vector to correct version
         -------------------------------------0--------------*/
        if (strncmp(dssr.cal_params_file,"SSPSWB010.CALPARMS",18)==0) {
          asfPrintStatus("\n   Substituting hardcoded noise vector sspswb010\n");
          noise_vector = sspswb010_noise_vec;
        } else if (strncmp(dssr.cal_params_file,"SSPSWB011.CALPARMS",18)==0) {
          asfPrintStatus("\n   Substituting hardcoded noise vector sspswb011\n");
          noise_vector = sspswb011_noise_vec;
        } else if (strncmp(dssr.cal_params_file,"SSPSWB013.CALPARMS",18)==0) {
          asfPrintStatus("\n   Substituting hardcoded noise vector sspswb013\n");
          noise_vector = sspswb013_noise_vec;
        } else if (strncmp(dssr.cal_params_file,"SSPSWB014.CALPARMS",18)==0) {
          asfPrintStatus("\n   Substituting hardcoded noise vector sspswb014\n");
          noise_vector = sspswb014_noise_vec;
        } else if (strncmp(dssr.cal_params_file,"SSPSWB015.CALPARMS",18)==0) {
          asfPrintStatus("\n   Substituting hardcoded noise vector sspswb015\n");
          noise_vector = sspswb015_noise_vec;
        } else if (strncmp(dssr.cal_params_file,"SSPSWB016.CALPARMS",18)==0) {
          asfPrintStatus("\n   Substituting hardcoded noise vector sspswb016\n");
          noise_vector = sspswb015_noise_vec;
        /* 16 and 15 were identical antenna patterns, only metadata fields were changed, so the noise vector for 16 is the same and that for 15. JBN */
        } else noise_vector = rdr.noise;

        for (i=0;i<p->noise_len;i++) p->noise[i]=noise_vector[i];

        /* ASF SCANSAR images are indexed by look angle so Check for SCANSAR */
        if (strncmp(dssr.product_type,"SCANSAR",7)==0)
                p->noise_type=by_look;

        /* Next, check for old-style geocoded images */
        else if (get_mpdr(sarName,&mpdr)>=0)
          {
            get_asf_facdr(sarName,&facdr);
            p->noise_type=by_geo;
            p->ns = slantRange2groundPixel(p->meta,facdr.sltrnglp*1000.0);
            asfPrintStatus("   Recognize old geocoded image; Max ns = %i\n",
                           p->ns);
          }

        /* Otherwise, use a straight-forward by_pixel approach */
        else if (1)
          {
                p->noise_type=by_pixel;
                p->ns=p->meta->ifm->orig_nSamples;
          }

        /* For Future Use:: Most other images are indexed by slant range.*/
        else
          {
                double totalSlant;

                get_asf_facdr(sarName,&facdr);
                p->noise_type=by_slant;
                p->minSlant=facdr.sltrngfp;
                totalSlant=facdr.sltrnglp-facdr.sltrngfp;
                p->slantPer=totalSlant/p->noise_len;
          }
        return p;
}

/*----------------------------------------------------------------------
  Get_cal_dn:
        Convert amplitude image data number into calibrated image data
        number (in power scale), given the current noise value.
----------------------------------------------------------------------*/
float get_cal_dn(cal_params *p,double noiseValue,double invIncAngle,int inDn)
{
        double scaledPower;

        /* Convert (amplitude) data number to scaled, noise-removed power */
        scaledPower=(p->a1*((float)inDn*inDn-p->a0*noiseValue) + p->a2)*invIncAngle;

        /* We don't want to convert the scaled power image into dB values
          since it messes up the statistics */
        if (scaledPower > 0.0 && inDn > 0)
          return scaledPower;
        else
          return LOVAL;
}

//----------------------------------------------------------------------
//Get_cal_dn_in_db:
//      Convert amplitude image data number into calibrated image data
//      number (in decibles), given the current noise value.
//--------------------------------------------------------------------
float get_cal_dn_in_db(cal_params *p,double noiseValue,double invIncAngle,int inDn)
{
  double scaledPower,db;

  // Convert (amplitude) data number to scaled, noise-removed power
  scaledPower=(p->a1*((float)inDn*inDn-p->a0*noiseValue) + p->a2)*invIncAngle;

  // Convert power to decibles
  if (scaledPower > 0.0)
  {
    db=10.0*log10(scaledPower);
//    if (db > p->Dmin)
      return db;
  }
  // Otherwise, set dB value to a floor
  return p->Dmin;
}

/*----------------------------------------------------------------------
  sprocket_get_cal_dn:
        Convert amplitude image data number into calibrated image data
        number (NOT in dB), given the current noise value.
----------------------------------------------------------------------*/
float sprocket_get_cal_dn(cal_params *p,double noiseValue,double invIncAngle,
                          int inDn)
{
        /*Convert (amplitude) data number to scaled, noise-removed power*/
        return (p->a1*((float)inDn*inDn-p->a0*noiseValue) + p->a2)*invIncAngle;
}

/*----------------------------------------------------------------------
check_cal
  Checks calibration status field of the data quality summary record.
  If the data is not calibrated, a message is displayed and 0 is returned.
  If the data has inferred calibration, a message is displayed and 1 is
  returned.  Otherwise 1 is returned.
----------------------------------------------------------------------*/
#include "ceos.h"
int check_cal(char *filename)
{
        struct qual_sum_rec    *dqsr;

        dqsr=(struct qual_sum_rec*)MALLOC(sizeof(struct qual_sum_rec));
        if (get_dqsr(filename,dqsr) == -1) return(1);

        if (strncmp(dqsr->cal_status,"UNCALIB",7)==0)
        {
          asfPrintStatus("   **********  UNCALIBRATED DATA  **********  \n");
          asfPrintStatus("   Calibration Comments: %s\n",dqsr->cal_comment);
          FREE(dqsr);
          return(1);
        }
        else if (strncmp(dqsr->cal_status,"INFERRE",7)==0)
        {
          asfPrintStatus("   INFERRED CALIBRATION DATA\n");
          asfPrintStatus("   Calibration Comments: %s\n",dqsr->cal_comment);
          FREE(dqsr);
          return(1);
        }
        else if (strncmp(dqsr->cal_status,"CALIBRA",7)==0)
        {
          asfPrintStatus("   Calibration Comments: %s\n",dqsr->cal_comment);
          FREE(dqsr);
          return(1);
        }
        else
        {
          asfPrintStatus("   ****** UNABLE TO DETERMINE CALIBRATION OF DATA ******\n");
          asfPrintStatus("   Calibration Comments: %s\n",dqsr->cal_comment);
          FREE(dqsr);
          return(0);
        }
}

