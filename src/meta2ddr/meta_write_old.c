#include <stdio.h>

#include "asf_meta.h"
#include "caplib.h"

/* Prototypes from asf_tools/src_lib/asf_meta */
void meta_put_string(FILE *meta_file,char *name,char  *value,char *comment);
void meta_put_double(FILE *meta_file,char *name,double value,char *comment);
void meta_put_int   (FILE *meta_file,char *name,int    value,char *comment);
void meta_put_char  (FILE *meta_file,char *name,char   value,char *comment);

/****************************************************************
 * meta_write_old:
 * Given a meta_parameters structure pointer and a file name,
 * write an old style metadata file for that structure.        */
void meta_write_old(meta_parameters *meta, const char *file_name)
{
  /* Maximum file name length, including trailing null.  */
#define FILE_NAME_MAX 1000
  char *file_name_with_extension = appendExt(file_name, ".meta");
  FILE *fp = FOPEN(file_name_with_extension, "w");
  geo_parameters *geo=meta->geo;
  ifm_parameters *ifm=meta->ifm;

  FREE(file_name_with_extension);

  /* Write an 'about meta file' comment  */
  fprintf(fp,
  	"# This file contains the metadata for satellite capture file of the same base name.\n"
	"# It was created by meta2ddr, most likely from post version 0.9 data.\n\n");

  /* Meta version 0.9 since we're formatting it that way.  */
  meta_put_double(fp,"meta_version:",0.90,"ASF APD Metadata File.\n");

/*Geolocation parameters.*/
  meta_put_string(fp,"geo {","","begin parameters used in geolocating the image.");
  meta_put_char(fp,"type:",geo->type,"Image type: [S=slant range; G=ground range; P=map projected]");
  if (geo->type=='P')
  {/*Projection Parameters.*/
    proj_parameters *proj=meta->geo->proj;
    meta_put_string(fp,"proj {","","Map Projection parameters");
    meta_put_char(fp,"type:",proj->type,"Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
    meta_put_double(fp,"startX:",proj->startX,"Projection Coordinate at top-left, X direction");
    meta_put_double(fp,"startY:",proj->startY,"Projection Coordinate at top-left, Y direction");
    meta_put_double(fp,"perX:",proj->perX,"Projection Coordinate per pixel, X direction");
    meta_put_double(fp,"perY:",proj->perY,"Projection Coordinate per pixel, X direction");
    meta_put_char(fp,"hem:",proj->hem,"Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
    meta_put_double(fp,"re_major:",proj->re_major,"Major (equator) Axis of earth (meters)");
    meta_put_double(fp,"re_minor:",proj->re_minor,"Minor (polar) Axis of earth (meters)");
    switch(proj->type)
    {
      case 'A':/*Along-track/cross-track projection.*/
	meta_put_double(fp,"rlocal:",proj->param.atct.rlocal,"Local earth radius [m]");
	meta_put_double(fp,"atct_alpha1:",proj->param.atct.alpha1,"at/ct projection parameter");
	meta_put_double(fp,"atct_alpha2:",proj->param.atct.alpha2,"at/ct projection parameter");
	meta_put_double(fp,"atct_alpha3:",proj->param.atct.alpha3,"at/ct projection parameter");
	break;
      case 'L':/*Lambert Conformal Conic projection.*/
	meta_put_double(fp,"lam_plat1:",proj->param.lamcc.plat1,"Lambert first standard parallel");
	meta_put_double(fp,"lam_plat2:",proj->param.lamcc.plat2,"Lambert second standard parallel");
	meta_put_double(fp,"lam_lat:",proj->param.lamcc.lat0,"Lambert original latitude");
	meta_put_double(fp,"lam_lon:",proj->param.lamcc.lon0,"Lambert original longitude");
	break;
      case 'P':/*Polar Stereographic Projection.*/
	meta_put_double(fp,"ps_lat:",proj->param.ps.slat,"Polar Stereographic reference Latitude");
	meta_put_double(fp,"ps_lon:",proj->param.ps.slon,"Polar Stereographic reference Longitude");
	break;
      case 'U':/*Universal Trasnverse Mercator Projection.*/
	meta_put_int(fp,"utm_zone:",proj->param.utm.zone,"UTM Zone Code");
	break;
      default:
	printf("ERROR! Unrecognized map projection code '%c' in function '%s'; program exitting.\n",
		proj->type, __func__);
	exit(EXIT_FAILURE);
    }
    meta_put_string(fp,"}","","end proj");
  }
  meta_put_char(fp,"lookDir:",geo->lookDir,"SAR Satellite look direction (normally R) [R=right; L=left]");
  meta_put_int(fp,"deskew:",geo->deskew,"Image moved to zero doppler? [1=yes; 0=no]");
  meta_put_double(fp,"xPix:",geo->xPix,"Pixel size in X direction [m]");
  meta_put_double(fp,"yPix:",geo->yPix,"Pixel size in Y direction [m]");
  meta_put_double(fp,"rngPixTime:",geo->rngPixTime,"Time/pixel, range (xPix/(c/2.0), or 1/fs) [s]");
  meta_put_double(fp,"azPixTime:",geo->azPixTime,"Time/pixel, azimuth (yPix/swathVel, or 1/prf) [s]");
  meta_put_double(fp,"slantShift:",geo->slantShift,"Error correction factor, in slant range [m]");
  meta_put_double(fp,"timeShift:",geo->timeShift,"Error correction factor, in time [s]");
  meta_put_double(fp,"slantFirst:",geo->slantFirst,"Slant range to first image pixel [m]");
  meta_put_double(fp,"wavelength:",geo->wavelen,"SAR Carrier Wavelength [m]");
  meta_put_double(fp,"dopRangeCen:",geo->dopRange[0],"Doppler centroid [Hz]");
  meta_put_double(fp,"dopRangeLin:",geo->dopRange[1],"Doppler per range pixel [Hz/pixel]");
  meta_put_double(fp,"dopRangeQuad:",geo->dopRange[2],"Doppler per range pixel sq. [Hz/(pixel^2)]");
  meta_put_double(fp,"dopAzCen:",geo->dopAz[0],"Doppler centroid [Hz]");
  meta_put_double(fp,"dopAzLin:",geo->dopAz[1],"Doppler per azimuth pixel [Hz/pixel]");
  meta_put_double(fp,"dopAzQuad:",geo->dopAz[2],"Doppler per azimuth pixel sq. [Hz/(pixel^2)]");
  meta_put_string(fp,"}","","end geo");

/*Interferometry parameters:*/
  meta_put_string(fp,"ifm {","","begin interferometry-related parameters");
  meta_put_double(fp,"er:",ifm->er,"Local earth radius [m]");
  meta_put_double(fp,"ht:",ifm->ht,"Satellite height, from center of earth [m]");
  meta_put_int(fp,"nLooks:",ifm->nLooks,"Number of looks to take from SLC");
  meta_put_int(fp,"orig_lines:",ifm->orig_nLines,"Number of lines in original image");
  meta_put_int(fp,"orig_samples:",ifm->orig_nSamples,"Number of samples in original image");
  meta_put_string(fp,"}","","end ifm");

/*State Vectors:*/
  if (meta->stVec!=NULL) {
    meta_put_string(fp,"state {","","begin list of state vectors for satellite, over image");
    meta_put_int   (fp,"year:",meta->state_vectors->year,"Year of image start");
    meta_put_int   (fp,"day:",meta->state_vectors->julDay,"Julian day of the year for image start");
    meta_put_double(fp,"second:",meta->state_vectors->second,"Second of the day for image start");
    meta_put_int   (fp,"number:",meta->state_vectors->vector_count,"Number of state vectors below");
    { 
      int ii;
      for (ii = 0; ii < meta->state_vectors->vector_count; ii++ ) {
	meta_put_string(fp,"vector {","","begin a single state vector");
	meta_put_double(fp,"time:",meta->state_vectors->vecs[ii].time,"Time, relative to image start [s]");
	meta_put_double(fp,"x:",meta->state_vectors->vecs[ii].vec.pos.x,"X Coordinate, earth-fixed [m]");
	meta_put_double(fp,"y:",meta->state_vectors->vecs[ii].vec.pos.y,"Y Coordinate, earth-fixed [m]");
	meta_put_double(fp,"z:",meta->state_vectors->vecs[ii].vec.pos.z,"Z Coordinate, earth-fixed [m]");
	meta_put_double(fp,"vx:",meta->state_vectors->vecs[ii].vec.vel.x,"X Velocity, earth-fixed [m/s]");
	meta_put_double(fp,"vy:",meta->state_vectors->vecs[ii].vec.vel.y,"Y Velocity, earth-fixed [m/s]");
	meta_put_double(fp,"vz:",meta->state_vectors->vecs[ii].vec.vel.z,"Z Velocity, earth-fixed [m/s]");
	meta_put_string(fp,"}","","end vector");
      }
    }
    meta_put_string(fp,"}","","end of list of state vectors");
  }

/*Extra Info:*/
  meta_put_string(fp,"extra {","","begin extra sensor information");
  meta_put_string(fp,"sensor:",meta->info->sensor,"Imaging sensor");
  meta_put_string(fp,"mode:",meta->info->mode,"Imaging mode");
  meta_put_string(fp,"processor:",meta->info->processor,"Name & Version of SAR Processor");
  meta_put_int(fp,"orbit:",meta->info->orbit,"Orbit Number for this datatake");
  meta_put_double(fp,"bitErrorRate:",meta->info->bitErrorRate,"Bit Error Rate");
  meta_put_string(fp,"satBinTime:",meta->info->satBinTime,"Satellite Binary Time");
  meta_put_string(fp,"satClkTime:",meta->info->satClkTime,"Satellite Clock Time (UTC)");
  meta_put_double(fp,"prf:",meta->info->prf,"Pulse Repition Frequency");
  meta_put_string(fp,"}","","end extra");

  FCLOSE(fp);

  return;
}
