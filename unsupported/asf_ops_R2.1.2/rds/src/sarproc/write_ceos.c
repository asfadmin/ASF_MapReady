static char sccsid_write_ceos_c[] =
    "@(#)write_ceos.c	1.2 96/04/09 19:13:36";

/* write_ceos(sp,pp) ---------------------------------

   write_ceos.c -- write the ceos leader and trailer files 
 
   This routine has changed drastically due to changes called
   for by IOM#3340-92-003.  This included changes to the order
   in which the leader and trailer files are written (reverse
   from before), moving of code to calculate values for the
   trailer facility data record to up front (trailer_info) for
   the fields that are needed in the leader.
*/
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include "scene_file.h"
#include "procdec.h"
#include "procfil.h"
#include "procpar.h"

extern DPP_FILE 	dpf;
extern SP_FILE 		spf;
extern RTF_FILE 	rtf;
extern EM_FILE 		emf;
extern SCENE_FILE	sf;

extern RQST_PTR 	Cur_Rqst;
extern SV		sv1,sv2;
extern TC_FILE 		tcf;

extern int	vbose;


write_ceos(sp,pp)
    TAPE_SEG_PTR sp;
    PP_BLOCK_PTR pp;
{
FILE *fp;
char ibuf[8192];
CEOS_INFO cip;

/**********************************************************************/
/* Leader File Update Routines 				              */
/**********************************************************************/
/* open ceos_leader file */
if((fp = fopen("ceos_leader","w"))==NULL){
  printf("Cannot open ceos_leader\n");
  return (FAIL);
  }

/* call to compute some last minute stuff to be put into CEOS */
if (leader_info(sp,pp,&cip) == FAIL) {
  printf("...Failed in making leader info.\n");
  return (FAIL);
}
if (trailer_info(sp,pp) == FAIL) {
  printf("...Failed in making trailer info.\n");
  return (FAIL);
}

/* file descriptor record */
if (l_fdr_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making leader (fdr) info.\n");
  return (FAIL);
}

/* data set summary */
if (l_dss_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making leader (dss) info.\n");
  return (FAIL);
}

/* platform position data */
if (l_ppd_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making leader (ppd) info.\n");
  return (FAIL);
}

/* attitude data record */
if (l_adr_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making leader (adr) info.\n");
  return (FAIL);
}

/* radiometric data record */
if (l_rdr_cmp(sp,pp,fp,&cip) == FAIL) {
  printf("...Failed in making leader (rdr) info.\n");
  return (FAIL);
}

/* radiometric compensation data */
if (l_rcd_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making leader (rcd) info.\n");
  return (FAIL);
}

/* data quality summary */
if (l_dqs_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making leader (dqs) info.\n");
  return (FAIL);
}

/* data histogram record */
if (l_dhr_cmp(sp,pp,fp,&cip) == FAIL) {
  printf("...Failed in making leader (dhr) info.\n");
  return (FAIL);
}

/* range spectra record */
if (l_rsr_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making leader (rsr) info.\n");
  return (FAIL);
}

/* close ceos_leader file */
fclose(fp);

/**********************************************************************/
/* Trailer File Update Routines				              */
/**********************************************************************/
/* open ceos_trailer file */
if((fp = fopen("ceos_trailer","w"))==NULL){
  printf("Cannot open ceos_trailer\n");
  return (FAIL);
  }

/* file descriptor record */
if (t_fdt_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making trailer (fdt) info.\n");
  return (FAIL);
}

/* detailed processing record */
if (t_dpp_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making trailer (dpp) info.\n");
  return (FAIL);
}

/* calibration data record */
if (t_cdr_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making trailer (cdr) info.\n");
  return (FAIL);
}

/* facility data record */
if (t_fdr_cmp(sp,pp,fp) == FAIL) {
  printf("...Failed in making trailer (fdr) info.\n");
  return (FAIL);
}

/* data file descriptor record (not really part of trailer) */
i_fdr_cmp(ibuf,8192);
fwrite(ibuf,1,1024,fp);

/* close ceos_trailer file */
fclose(fp);

return (PASS);

}

/* leader_info(sp,pp,cp) ----------------------------------

********************************************************************
* leader_info computes some auxilary info for the CEOS format      *
********************************************************************

*/


leader_info(sp,pp,cp)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    CEOS_INFO_PTR	cp;
{

int	i,k,grand_total,expected;
float	peak;
float	value[256],meansq;
float	meansamp,stdvsamp,meanfreq,stdvfreq,minfreq,maxfreq;


/* determine the type of product, and set up the histogram   */
/* and the corresponding values for the histogram.           */

   if (strcmp(Cur_Rqst->type,"CSD") == 0) {
	cp->nbins = 32;     /* 32 is for 5 bits on E-ERS1 */
	for (i=0; i<cp->nbins; i++) {
	   cp->hstgrm[i] = pp->ihist_cor[i];
	   value[i]  = (float)i;
   	}
   }
   if (strcmp(Cur_Rqst->type,"CPX") == 0) {
	/* rearrange the histogram table to account for two-s */
	/* compliment to go from -128 to +127.                */
	cp->nbins = 256;
	k = 0;
	for (i=128; i<256; i++) cp->hstgrm[i] = sf.i_hstgrm[k++];
	for (i=0;   i<128; i++) cp->hstgrm[i] = sf.i_hstgrm[k++];
	for (i=0; i<cp->nbins; i++) value[i]  = (float)(i-128)*256.;
   }
   if ((strcmp(Cur_Rqst->type,"STD") == 0) ||
       (strcmp(Cur_Rqst->type,"QLK") == 0) ||
       (strcmp(Cur_Rqst->type,"RPR") == 0))  {
	cp->nbins = 256;
	grand_total = 0;
	for (i=0; i<cp->nbins; i++) {
	   cp->hstgrm[i] = sf.i_hstgrm[i];
	   value[i]  = (float)i;
	   grand_total += cp->hstgrm[i];
	}
	expected = grand_total - sf.nl * ap_rmlines;
	if (expected < 0) 
	  printf("...histogram total count is lower than expected: %d\n",grand_total);
	if (expected > 0) 
	   cp->hstgrm[0] -= expected;
   	if (cp->hstgrm[0] < 0) cp->hstgrm[0] = 0;
   } 

	hist_stat(cp->nbins,cp->hstgrm,value,&minfreq,&maxfreq,
		&meansamp,&stdvsamp,&meanfreq,&stdvfreq);
	     cp->minfreq = minfreq;
	     cp->maxfreq = maxfreq;
	     cp->meansamp = meansamp;
	     cp->stdvsamp = stdvsamp;
	     cp->meanfreq = meanfreq;
	     cp->stdvfreq = stdvfreq;
	     meansq = meansamp * meansamp;


/* For field 'q' -- process only CSD data */

  if (strcmp(Cur_Rqst->type,"CSD") == 0) {

	cp->q_nbins = 32;     /* 32 is for 5 bits on E-ERS1 */
	for (i=0; i<cp->q_nbins; i++) {
	   cp->q_hstgrm[i] = pp->qhist_cor[i];
	   value[i]  = (float)i;
	}

	hist_stat(cp->q_nbins,cp->q_hstgrm,value,&minfreq,&maxfreq,
		&meansamp,&stdvsamp,&meanfreq,&stdvfreq);
	     cp->q_minfreq = minfreq;
	     cp->q_maxfreq = maxfreq;
	     cp->q_meansamp = meansamp;
	     cp->q_stdvsamp = stdvsamp;
	     cp->q_meanfreq = meanfreq;
	     cp->q_stdvfreq = stdvfreq;
	     meansq += meansamp * meansamp;

  } /* end if CSD -- process "q" for CSD only */


/* compute the three coefficients for the radiometric data record */

  sf.noise_scl_fac = dpf.noise_fctr * pp->noise *
		 pow(10.,((pp->pro_gain + Cur_Rqst->proc_gain)/10.0));

/* -- The linear term no longer is to use the peak power as per
      IOM#3340-92-003  but is left here in comments for record   
  peak = pp->peak;
  if (peak ==  0.0) peak = dpf.peak_ref;
  sf.lnr_conv_fac  = (dpf.peak_ref/peak) * dpf.linear_fctr * 
		 pow(10.,(- (pp->pro_gain + Cur_Rqst->proc_gain)/10.0));
-- */

  sf.lnr_conv_fac  = dpf.linear_fctr * 
		 pow(10.,(- (pp->pro_gain + Cur_Rqst->proc_gain)/10.0));

  sf.off_conv_fac  = dpf.offset_fctr;


  return (PASS);

}


/* hist_stat(nbins,hstgrm,value,minfreq,maxfreq,meansamp, -------
	     stdvsamp,meanfreq,stdvfreq)

 compute some statics for the histogram.

 inputs:	nbins 	-- number of bins in the histogram
		hstgrm 	-- the histogram 
		value	-- the values of each bin 
 output:	minfreq	-- the minimum histogram table value
		maxfreq	-- the maximum histogram table value
		meansamp - the mean sample value
		stdvsamp - the std dev. of sample value
		meanfreq - the mean histogram table value
		stdvfreq - the std dev of histogram table value

*/


hist_stat(nbins,hstgrm,value,minfreq,maxfreq,meansamp,
	     stdvsamp,meanfreq,stdvfreq)

int nbins,hstgrm[];
float	value[],*meansamp,*stdvsamp,*meanfreq,*stdvfreq,*minfreq,*maxfreq;
{

int	i,temp,mintemp,maxtemp;
float	ftemp,total,total2,sum,sum2,val;

if (nbins <= 0) return;

mintemp = 9999999;
maxtemp = 0;

for (total=0, total2=0., sum=0., sum2=0., i=0; i < nbins; i++) {
	temp   = hstgrm[i];
	ftemp  = (float)temp;
	val    = value[i];
	total  += ftemp;
	total2 += ftemp*ftemp;
	sum    += ftemp*val;
	sum2   += ftemp*val*val;
	if ( mintemp > temp) mintemp = temp;
	if ( maxtemp < temp) maxtemp = temp;
 }

if (total != 0.) {
  *meansamp = sum/total;
  *stdvsamp = sqrt((sum2/total) - ((*meansamp) * (*meansamp)));
 }
*meanfreq = total/nbins;
*stdvfreq = sqrt ((total2/nbins) - (*meanfreq) * (*meanfreq));
*minfreq  = (float)mintemp;
*maxfreq  = (float)maxtemp;
if (vbose) {
	printf("mean and stdv (sample) = %g,%g\n",*meansamp,*stdvsamp);
	printf("mean and stdv (freque) = %g,%g\n",*meanfreq,*stdvfreq);
 }
return;

}


/* trailer_info(sp,pp) -----------------------------------------

**********************************************************************
* computes some last minute stuff for trailer facility data record   *
**********************************************************************

*/

trailer_info(sp,pp)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
{

double  ff,xx,yy,zz,dd1,dd2,len,trk_angle;
double  r,gclat_sc,gdlat_sc,dlat;
double  lk_angle,spacing,delta;
double  pi = 3.141592653589793;
double  c = 2.998E8;


/*extract ap_rmlines */
sf.np_nflr = ap_rmlines;

/*extract sf.nl*/
sf.nl_nflr = sf.nl;

/*Range gate delay. */
/*calculate from (data_window_pos + (8/PRF)x10**6)*/
sf.rnge_gate = sf.wndw_pos + ((8.0/sp->pre->prf)*1000000.0); 

/*calculate track angle*/
ff = 1.0 / emf.f;
xx = sf.x_begin * sf.x_begin;
yy = sf.y_begin * sf.y_begin;
zz = sf.z_begin * sf.z_begin;
dd1 = (xx + yy) / (xx + yy + zz);
dd1 = acos(sqrt(dd1));
xx = sf.x_end * sf.x_end;
yy = sf.y_end * sf.y_end;
zz = sf.z_end * sf.z_end;
dd2 = (xx + yy) / (xx + yy + zz);
dd2 = acos(sqrt(dd2));
len = (strcmp(Cur_Rqst->type,"CPX") == 0) ? 51.2 : 102.4;
trk_angle = (180.0 / pi)
	    * asin(sf.re_nadir * fabs(dd2 - dd1) / len);
if (dd2 > dd1 || (dd2 == dd1 && sf.z_mid > 0.0)) {
    trk_angle = 270.0 + trk_angle; 
    sf.asc_dsc[0] = 'A';
}
else {
    trk_angle = 270.0 - trk_angle;
    sf.asc_dsc[0] = 'D';
}
sf.trk_ang = trk_angle;

/* calculate spacecraft altitude at image center*/
xx = sf.x_mid * sf.x_mid;
yy = sf.y_mid * sf.y_mid;
zz = sf.z_mid * sf.z_mid;
r = sqrt(xx + yy + zz);
gclat_sc = acos(sqrt(xx + yy) / r);
gdlat_sc = atan(tan(gclat_sc) / ((1.0 - ff) * (1.0 - ff)));
dlat = gdlat_sc - gclat_sc;
sf.altitude = (r - sf.re_nadir) * cos(dlat);

/* calculate antenna look angle */
delta = (sf.r_mid * sf.r_mid - sf.altitude * sf.altitude)
	/ (2.0 * r);
lk_angle = (180.0 / pi) * (acos((sf.altitude + delta) / sf.r_mid));
sf.lk_angle = lk_angle;
/* need to save lk_angle in sf.??? */

/*calculate incidence angle*/
sf.inc_image_ctr = lk_angle + (180.0 / pi) * acos(1.0 - (delta / emf.r_e));

/*Squint angle sf.squint =pitch cos 20 - yaw sin 20*/
lk_angle = (pi / 180.0) * lk_angle;
sf.squint = pp->att.pitch * cos(lk_angle) - pp->att.yaw * sin(lk_angle);

return (PASS);

}
