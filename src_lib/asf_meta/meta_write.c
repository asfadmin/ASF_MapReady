#include <stdio.h>

#include "asf_meta.h"
#include "caplib.h"
#include "err_die.h"

void meta_put_string(FILE *meta_file,char *name,char  *value,char *comment);
void meta_put_double(FILE *meta_file,char *name,double value,char *comment);
void meta_put_int   (FILE *meta_file,char *name,int    value,char *comment);
void meta_put_char  (FILE *meta_file,char *name,char   value,char *comment);

/* Given a meta_parameters structure pointer and a file name, write a
   metadata file for that structure.  */
void meta_write(meta_parameters *meta, const char *file_name)
{
  /* Maximum file name length, including trailing null.  */
#define FILE_NAME_MAX 1000
  char *file_name_with_extension = appendExt(file_name, ".meta");
  FILE *fp = FOPEN(file_name_with_extension, "w");

  FREE(file_name_with_extension);

  /* Write an 'about meta file' comment  */
  fprintf(fp,
  	"# This file contains the metadata for satellite capture file of the same base name.\n"
	"#      '?' is likely an unknown single character value.\n"
	"#      '??\?' is likely an unknown string of characters.\n"
	"#      '-2147283648' is likely an unknown integer value.\n"
	"#      'NaN' is likely an unknown Real value.\n\n");

  /* We always write out files corresponding to the latest meta version.  */
  fprintf(fp, "meta_version: %.2f\n\n", META_VERSION);

/* General block.  */
  meta_put_string(fp,"general {", "","Begin parameters generally used in remote sensing");
  meta_put_string(fp,"sensor:", meta->general->sensor, "Imaging sensor");
  meta_put_string(fp,"mode:",meta->general->mode,"Imaging mode");
  meta_put_string(fp,"processor:", meta->general->processor,"Name and Version of Processor");
  meta_put_string(fp,"data_type:", meta->general->data_type,"Type of samples (e.g. REAL*4)");
  meta_put_string(fp,"system:", meta->general->system,"System of samples (e.g. big_ieee)");
  meta_put_int   (fp,"orbit:", meta->general->orbit,"Orbit Number for this datatake");
  meta_put_char  (fp,"orbit_direction:", meta->general->orbit_direction,"Ascending 'A', or descending 'D'");
  meta_put_int   (fp,"frame:", meta->general->frame,"Frame for this image [-1 if n/a]");
  meta_put_int   (fp,"band_number:", meta->general->band_number,"Band number; first band is 0");
  meta_put_int   (fp,"line_count:", meta->general->line_count,"Number of lines in image");
  meta_put_int   (fp,"sample_count:", meta->general->sample_count,"Number of samples in image");
  meta_put_int   (fp,"start_line:", meta->general->start_line,"First line relative to original image");
  meta_put_int   (fp,"start_sample:", meta->general->start_sample,"First sample relative to original image");
  meta_put_double(fp,"x_pixel_size:", meta->general->x_pixel_size,"Range pixel size [m]");
  meta_put_double(fp,"y_pixel_size:", meta->general->y_pixel_size,"Azimuth pixel size [m]");
  meta_put_double(fp,"center_latitude:", meta->general->center_latitude,"Approximate image center latitude");
  meta_put_double(fp,"center_longitude:", meta->general->center_longitude,"Approximate image center longitude");
  meta_put_double(fp,"re_major:", meta->general->re_major,"Major (equator) Axis of earth [m]");
  meta_put_double(fp,"re_minor:", meta->general->re_minor,"Minor (polar) Axis of earth [m]");
  meta_put_double(fp,"bit_error_rate:", meta->general->bit_error_rate,"Fraction of bits which are in error");
  meta_put_int   (fp,"missing_lines:", meta->general->missing_lines,"Number of missing lines in data take");
  meta_put_string(fp,"}", "","End general");

/* SAR block.  */
  meta_put_string(fp,"sar {","","Begin parameters used specifically in SAR imaging");
  meta_put_char  (fp,"image_type:", meta->sar->image_type,"[S=slant range; G=ground range; P=map projected]");
  meta_put_char  (fp,"look_direction:",meta->sar->look_direction,"SAR Satellite look direction [R=right; L=left]");
  meta_put_int   (fp,"look_count:",meta->sar->look_count,"Number of looks to take from SLC");
  meta_put_int   (fp,"deskewed:",meta->sar->deskewed,"Image moved to zero doppler? [1=yes; 0=no]");
  meta_put_int   (fp,"original_line_count:",meta->sar->original_line_count,"Number of lines in original image");
  meta_put_int   (fp,"original_sample_count:",meta->sar->original_sample_count,"Number of samples in original image");
  meta_put_double(fp,"line_increment:",meta->sar->line_increment,"Line increment for sampling");
  meta_put_double(fp,"sample_increment:",meta->sar->sample_increment,"Sample increment for sampling");
  meta_put_double(fp,"range_time_per_pixel:",meta->sar->range_time_per_pixel,"Time per pixel in range [s]");
  meta_put_double(fp,"azimuth_time_per_pixel:",meta->sar->azimuth_time_per_pixel,"Time per pixel in azimuth [s]");
  meta_put_double(fp,"slant_range_first_pixel:",meta->sar->slant_range_first_pixel,"Slant range to first pixel [m]");
  meta_put_double(fp,"slant_shift:",meta->sar->slant_shift,"Error correction factor, in slant range [m]");
  meta_put_double(fp,"time_shift:",meta->sar->time_shift,"Error correction factor, in time [s]");
  meta_put_double(fp,"wavelength:",meta->sar->wavelength,"SAR carrier wavelength [m]");
  meta_put_double(fp,"prf:",meta->sar->prf,"Pulse Repition Frequency");
  meta_put_string(fp,"satellite_binary_time:",meta->sar->satellite_binary_time,"Satellite Binary Time");
  meta_put_string(fp,"satellite_clock_time:",meta->sar->satellite_clock_time,"Satellite Clock Time (UTC)");
  meta_put_double(fp,"dopRangeCen:",meta->sar->range_doppler_coefficients[0],"Range doppler centroid [Hz]");
  meta_put_double(fp,"dopRangeLin:",meta->sar->range_doppler_coefficients[1],"Range doppler per range pixel [Hz/pixel]");
  meta_put_double(fp,"dopRangeQuad:",meta->sar->range_doppler_coefficients[2],"Range doppler per range pixel sq. [Hz/(pixel^2)]");
  meta_put_double(fp,"dopAzCen:",meta->sar->azimuth_doppler_coefficients[0],"Azimuth doppler centroid [Hz]");
  meta_put_double(fp,"dopAzLin:",meta->sar->azimuth_doppler_coefficients[1],"Azimuth doppler per azimuth pixel [Hz/pixel]");
  meta_put_double(fp,"dopAzQuad:",meta->sar->azimuth_doppler_coefficients[2],"Azimuth doppler per azimuth pixel sq. [Hz/(pixel^2)]");
  meta_put_string(fp,"}","","End sar");

/* State block.  */
  meta_put_string(fp,"state {","","Begin list of state vectors for satellite, over image");
  meta_put_int   (fp,"year:",meta->state_vectors->year,"Year of image start");
  meta_put_int   (fp,"julDay:",meta->state_vectors->julDay,"Julian day of the year for image start");
  meta_put_double(fp,"second:",meta->state_vectors->second,"Second of the day for image start");
  meta_put_int   (fp,"vector_count:",meta->state_vectors->vector_count,"Number of state vectors below");
  { 
    int ii;
    for (ii = 0; ii < meta->state_vectors->vector_count; ii++ ) {
      meta_put_string(fp,"vector {","","Begin a single state vector");
      meta_put_double(fp,"time:",meta->state_vectors->vecs[ii].time,"Time, relative to image start [s]");
      meta_put_double(fp,"x:",meta->state_vectors->vecs[ii].vec.pos.x,"X Coordinate, earth-fixed [m]");
      meta_put_double(fp,"y:",meta->state_vectors->vecs[ii].vec.pos.y,"Y Coordinate, earth-fixed [m]");
      meta_put_double(fp,"z:",meta->state_vectors->vecs[ii].vec.pos.z,"Z Coordinate, earth-fixed [m]");
      meta_put_double(fp,"vx:",meta->state_vectors->vecs[ii].vec.vel.x,"X Velocity, earth-fixed [m/s]");
      meta_put_double(fp,"vy:",meta->state_vectors->vecs[ii].vec.vel.y,"Y Velocity, earth-fixed [m/s]");
      meta_put_double(fp,"vz:",meta->state_vectors->vecs[ii].vec.vel.z,"Z Velocity, earth-fixed [m/s]");
      meta_put_string(fp,"}","","End a single state vector");
    }
  }
  meta_put_string(fp,"}","","End the list of state vectors");

/* Projection parameters block, if appropriate.  */
  if ( meta->sar->image_type == 'P' ) {
    meta_put_string(fp,"projection {","","Map Projection parameters");
    meta_put_char  (fp,"type:",meta->projection->type,"Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
    meta_put_double(fp,"startX:",meta->projection->startX,"Projection Coordinate at top-left, X direction");
    meta_put_double(fp,"startY:",meta->projection->startY,"Projection Coordinate at top-left, Y direction");
    meta_put_double(fp,"perX:",meta->projection->perX,"Projection Coordinate per pixel, X direction");
    meta_put_double(fp,"perY:",meta->projection->perY,"Projection Coordinate per pixel, Y direction");
    meta_put_string(fp,"units:",meta->projection->units,"Units of projection [meters, seconds]");
    meta_put_char  (fp,"hem:",meta->projection->hem,"Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
    meta_put_double(fp,"re_major:",meta->projection->re_major,"Major Axis (equator) of earth [m]");
    meta_put_double(fp,"re_minor:",meta->projection->re_minor,"Minor Axis (polar) of earth [m]");
    meta_put_string(fp,"param {","","Projection specific parameters");
    switch ( meta->projection->type ) {
    case 'A': /* Along-track/cross-track projection.  */
      meta_put_string(fp,"atct {","","Begin along-track/cross-track projection");
      meta_put_double(fp,"rlocal:",meta->projection->param.atct.rlocal,"Local earth radius [m]");
      meta_put_double(fp,"alpha1:",meta->projection->param.atct.alpha1,"First rotation angle [degrees]");
      meta_put_double(fp,"alpha2:",meta->projection->param.atct.alpha2,"Second rotation angle [degrees]");
      meta_put_double(fp,"alpha3:",meta->projection->param.atct.alpha3,"Third rotation angle [degrees]");
      meta_put_string(fp,"}","","End long-track/cross-track projection");
      break;
    case'B':/*Lambert conformal conic projection.*/
      meta_put_string(fp,"lambert {","","Begin Lambert Conformal Conic projection");
      meta_put_double(fp,"plat1:",meta->projection->param.lambert.plat1,"First standard parallel");
      meta_put_double(fp,"plat2:",meta->projection->param.lambert.plat2,"Second standard parallel");
      meta_put_double(fp,"lat0:",meta->projection->param.lambert.lat0,"Original latitude");
      meta_put_double(fp,"lon0:",meta->projection->param.lambert.lon0,"Original longitude");
      meta_put_string(fp,"}","","End Lambert Conformal Conic projection");
      break;
    case'P':/*Polarstereographicprojection.*/
      meta_put_string(fp,"ps {","","Begin Polar Stereographic Projection");
      meta_put_double(fp,"lat:",meta->projection->param.ps.slat,"Reference Latitude");
      meta_put_double(fp,"lon:",meta->projection->param.ps.slon,"Reference Longitude");
      meta_put_string(fp,"}","","End Polar Stereographic Projection");
      break;
    case'U':/*Universaltransversemercatorprojection.*/
      meta_put_string(fp,"utm {","","Begin Universal Transverse Mercator Projection");
      meta_put_int   (fp,"zone:",meta->projection->param.utm.zone,"Zone Code");
      meta_put_string(fp,"}","","End Universal Transverse Mercator Projection");
      break;
   default: 
      err_die("unknown projection type seen in function '%s'", __func__);
    }
    meta_put_string(fp,"}","","End param");
    meta_put_string(fp,"}","","End projection");
  }

  
  FCLOSE(fp);
  
  return;
}

void meta_put_string(FILE *meta_file,char *name,char *value,char *comment)
{
	int ii;
	char line[255];/*The line to be written to the file.*/
	static int depth=0;
	strcpy(line,"");
	
/*Deal with indentation.*/
	if (strchr(name,'}'))/*If the string has a closing brace, indent less.*/
		depth--;
	if (depth<0)
		{printf("ERROR!  Too many '}' in meta file!\n"); exit(1);}
	
	for (ii=0; ii<depth; ii++)
		strcat(line,"    ");/*Indent the appropriate number of spaces.*/

/*Append parameter and value.*/
	strcat(line,name);/*Append parameter name*/
	strcat(line," ");
	strcat(line,value);/*Append parameter value.*/

	if (comment!=NULL)
	{
	/*Space over to the comment section.*/
		ii=strlen(line);
		while (ii < 42+depth*4) /*Fill spaces out to about column 50.*/
			line[ii++]=' ';
		line[ii++]='\0';        /*Append trailing NULL.*/
	
	/*Add the comment.*/
		strcat(line," # ");     /*Signal beginning of comment.*/
		strcat(line,comment);   /*Append comment.*/
	}

	/*If the string has a closing brace, append newline*/
	if (strchr(name,'}') && (depth==0))
		strcat(line,"\n");

/*More indentation.*/	
	if (strchr(name,'{'))/*If the string has an opening brace, indent more.*/
		depth++;

/*Finally, write the line to the file.*/
	fprintf(meta_file,"%s\n",line);
}

void meta_put_double(FILE *meta_file,char *name,double value,char *comment)
{
	char param[64];
	sprintf(param,"%-16.11g",value);
	strtok(param," ");/*remove all trailing spaces */
	meta_put_string(meta_file,name,param,comment);
}

	
void meta_put_int(FILE *meta_file,char *name,int value,char *comment)
{
	char param[64];
	sprintf(param,"%i",value);
	meta_put_string(meta_file,name,param,comment);
}

void meta_put_char(FILE *meta_file,char *name,char value,char *comment)
{
	char param[64];
	sprintf(param,"%c",value);
	meta_put_string(meta_file,name,param,comment);
}





