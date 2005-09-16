#include "libasf_meta.h"

#define FILE_NAME_MAX 1000

void asf_meta_write09(meta_parameters *meta, char *file_name)
{
  FILE *fp;

  fp = FOPEN(file_name, "w");

  /* Write an 'about meta file' comment  */
  fprintf(fp, "meta_version: %.1f\n\n", meta->meta_version);

  /*Geometry parameters.*/
  meta_put_string(fp, "geo {", "",
		  "begin parameters used in geolocating the image.");
  meta_put_char(fp, "type:", meta->geo->type,
		"Image type: [S=slant range; G=ground range; P=map projected]");
  if (meta->geo->type == 'P' &&
      (meta->geo->proj->type == SCANSAR_PROJECTION ||
       meta->geo->proj->type == LAMBERT_CONFORMAL_CONIC ||
       meta->geo->proj->type == POLAR_STEREOGRAPHIC ||
       meta->geo->proj->type == UNIVERSAL_TRANSVERSE_MERCATOR))
    {/*Projection Parameters.*/
      meta_put_string(fp, "proj {", "",
		      "Map Projection parameters");
      switch(meta->geo->proj->type)
	{
	case SCANSAR_PROJECTION:
	  meta_put_char(fp, "type:", 'A',
			"Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
	  break;
	case LAMBERT_CONFORMAL_CONIC:
	  meta_put_char(fp, "type:", 'L',
			"Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
	  break;
	case POLAR_STEREOGRAPHIC:
	  meta_put_char(fp, "type:", 'P',
			"Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
	  break;
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	  meta_put_char(fp, "type:", 'U',
			"Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
	  break;
	case ALBERS_EQUAL_AREA:
	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	case STATE_PLANE:
	  break;
	}
      meta_put_double(fp, "startX:", meta->geo->proj->startX,
		      "Projection Coordinate at top-left, X direction");
      meta_put_double(fp, "startY:", meta->geo->proj->startY,
		      "Projection Coordinate at top-left, Y direction");
      meta_put_double(fp, "perX:", meta->geo->proj->perX,
		      "Projection Coordinate per pixel, X direction");
      meta_put_double(fp, "perY:", meta->geo->proj->perY,
		      "Projection Coordinate per pixel, X direction");
      meta_put_char(fp, "hem:", meta->geo->proj->hem,
		    "Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
      meta_put_double(fp, "re_major:", meta->geo->proj->re_major,
		      "Major (equator) Axis of earth (meters)");
      meta_put_double(fp, "re_minor:", meta->geo->proj->re_minor,
		      "Minor (polar) Axis of earth (meters)");
      switch(meta->geo->proj->type)
	{
	case SCANSAR_PROJECTION:/*Along-track/cross-track projection.*/
	  meta_put_double(fp, "rlocal:", meta->geo->proj->param.atct.rlocal,
			  "Local earth radius [m]");
	  meta_put_double(fp, "atct_alpha1:", meta->geo->proj->param.atct.alpha1,
			  "at/ct projection parameter");
	  meta_put_double(fp, "atct_alpha2:", meta->geo->proj->param.atct.alpha2,
			  "at/ct projection parameter");
	  meta_put_double(fp, "atct_alpha3:", meta->geo->proj->param.atct.alpha3,
			  "at/ct projection parameter");
	  break;
	case LAMBERT_CONFORMAL_CONIC:/*Lambert Conformal Conic projection.*/
	  meta_put_double(fp, "lam_plat1:", meta->geo->proj->param.lamcc.plat1,
			  "Lambert first standard parallel");
	  meta_put_double(fp, "lam_plat2:", meta->geo->proj->param.lamcc.plat2,
			  "Lambert second standard parallel");
	  meta_put_double(fp, "lam_lat:", meta->geo->proj->param.lamcc.lat0,
			  "Lambert original latitude");
	  meta_put_double(fp, "lam_lon:", meta->geo->proj->param.lamcc.lon0,
			  "Lambert original longitude");
	  break;
	case POLAR_STEREOGRAPHIC:/*Polar Stereographic Projection.*/
	  meta_put_double(fp, "ps_lat:", meta->geo->proj->param.ps.slat,
			  "Polar Stereographic reference Latitude");
	  meta_put_double(fp, "ps_lon:", meta->geo->proj->param.ps.slon,
			  "Polar Stereographic reference Longitude");
	  break;
	case UNIVERSAL_TRANSVERSE_MERCATOR:/*Universal Trasnverse Mercator Projection.*/
	  meta_put_int(fp, "utm_zone:", meta->geo->proj->param.utm.zone,"UTM Zone Code");
	  break;
	case ALBERS_EQUAL_AREA:
	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	case STATE_PLANE:
	  break;
	}
      meta_put_string(fp, "}", "", "end proj");
    }
  meta_put_char(fp, "lookDir:", meta->geo->lookDir,
		"SAR Satellite look direction (normally R) [R=right; L=left]");
  meta_put_int(fp, "deskew:", meta->geo->deskew,
	       "Image moved to zero doppler? [1=yes; 0=no]");
  meta_put_double(fp, "xPix:", meta->geo->xPix, "Pixel size in X direction [m]");
  meta_put_double(fp, "yPix:", meta->geo->yPix, "Pixel size in Y direction [m]");
  meta_put_double(fp, "rngPixTime:", meta->geo->rngPixTime,
		  "Time/pixel, range (xPix/(c/2.0), or 1/fs) [s]");
  meta_put_double(fp, "azPixTime:", meta->geo->azPixTime,
		  "Time/pixel, azimuth (yPix/swathVel, or 1/prf) [s]");
  meta_put_double(fp, "slantShift:", meta->geo->slantShift,
		  "Error correction factor, in slant range [m]");
  meta_put_double(fp, "timeShift:", meta->geo->timeShift,
		  "Error correction factor, in time [s]");
  meta_put_double(fp, "slantFirst:", meta->geo->slantFirst,
		  "Slant range to first image pixel [m]");
  meta_put_double(fp, "wavelength:", meta->geo->wavelen, "SAR Carrier Wavelength [m]");
  meta_put_double(fp, "dopRangeCen:", meta->geo->dopRange[0], "Doppler centroid [Hz]");
  meta_put_double(fp, "dopRangeLin:", meta->geo->dopRange[1],
		  "Doppler per range pixel [Hz/pixel]");
  meta_put_double(fp, "dopRangeQuad:", meta->geo->dopRange[2],
		  "Doppler per range pixel sq. [Hz/(pixel^2)]");
  meta_put_double(fp, "dopAzCen:", meta->geo->dopAz[0], "Doppler centroid [Hz]");
  meta_put_double(fp, "dopAzLin:", meta->geo->dopAz[1],
		  "Doppler per azimuth pixel [Hz/pixel]");
  meta_put_double(fp, "dopAzQuad:", meta->geo->dopAz[2],
		  "Doppler per azimuth pixel sq. [Hz/(pixel^2)]");
  meta_put_string(fp, "}", "", "end geo");
  
  /*Interferometry parameters:*/
  meta_put_string(fp, "ifm {", "", "begin interferometry-related parameters");
  meta_put_double(fp, "er:", meta->ifm->er, "Local earth radius [m]");
  meta_put_double(fp, "ht:", meta->ifm->ht, 
		  "Satellite height, from center of earth [m]");
  meta_put_int(fp, "nLooks:", meta->ifm->nLooks, "Number of looks to take from SLC");
  meta_put_int(fp, "orig_lines:", meta->ifm->orig_nLines,
	       "Number of lines in original image");
  meta_put_int(fp, "orig_samples:", meta->ifm->orig_nSamples,
	       "Number of samples in original image");
  meta_put_string(fp, "}", "", "end ifm");
  
  /*State Vectors:*/
  if (meta->stVec) {
    meta_put_string(fp, "state {", "",
		    "begin list of state vectors for satellite, over image");
    meta_put_int(fp, "year:", meta->stVec->year, "Year of image start");
    meta_put_int(fp, "day:", meta->stVec->julDay,
		 "Julian day of the year for image start");
    meta_put_double(fp, "second:", meta->stVec->second, 
		    "Second of the day for image start");
    meta_put_int (fp, "number:", meta->stVec->num,
		  "Number of state vectors below");
    {
      int ii;
      for (ii = 0; ii < meta->stVec->num; ii++ ) {
	meta_put_string(fp, "vector {", "", "begin a single state vector");
	meta_put_double(fp, "time:", meta->stVec->vecs[ii].time,
			"Time, relative to image start [s]");
	meta_put_double(fp, "x:", meta->stVec->vecs[ii].vec.pos.x,
			"X Coordinate, earth-fixed [m]");
	meta_put_double(fp, "y:", meta->stVec->vecs[ii].vec.pos.y,
			"Y Coordinate, earth-fixed [m]");
	meta_put_double(fp, "z:", meta->stVec->vecs[ii].vec.pos.z,
			"Z Coordinate, earth-fixed [m]");
	meta_put_double(fp, "vx:", meta->stVec->vecs[ii].vec.vel.x,
			"X Velocity, earth-fixed [m/s]");
	meta_put_double(fp, "vy:", meta->stVec->vecs[ii].vec.vel.y,
			"Y Velocity, earth-fixed [m/s]");
	meta_put_double(fp, "vz:", meta->stVec->vecs[ii].vec.vel.z,
			"Z Velocity, earth-fixed [m/s]");
	meta_put_string(fp, "}", "", "end vector");
      }
    }
    meta_put_string(fp, "}", "", "end of list of state vectors");
  }
  
  /*Extra Info:*/
  meta_put_string(fp, "extra {", "", "begin extra sensor information");
  meta_put_string(fp, "sensor:", meta->info->sensor, "Imaging sensor");
  meta_put_string(fp, "mode:", meta->info->mode, "Imaging mode");
  meta_put_string(fp, "processor:", meta->info->processor,
		  "Name & Version of SAR Processor");
  meta_put_int(fp, "orbit:", meta->info->orbit, "Orbit Number for this datatake");
  meta_put_double(fp, "bitErrorRate:", meta->info->bitErrorRate, 
		  "Bit Error Rate");
  meta_put_string(fp, "satBinTime:", meta->info->satBinTime,
		  "Satellite Binary Time");
  meta_put_string(fp, "satClkTime:", meta->info->satClkTime,
		  "Satellite Clock Time (UTC)");
  meta_put_double(fp, "prf:", meta->info->prf, "Pulse Repition Frequency");
  meta_put_string(fp, "}", "", "end extra");
  
  FCLOSE(fp);

  return;
}
