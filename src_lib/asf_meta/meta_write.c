#include <stdio.h>

#include "asf_meta.h"
#include "caplib.h"
#include "err_die.h"

/* Given a meta_parameters structure pointer and a file name, write a
   metadata file for that structure.  */
void meta_write(meta_parameters *meta, const char *file_name)
{
  FILE *fp = FOPEN(file_name, "w");

  fprintf(fp, "meta_version: %f\n", meta->meta_version);

  /* General block.  */
  fprintf(fp, "general {\n");
  fprintf(fp, "    sensor: %s\n", meta->general->sensor);
  fprintf(fp, "    mode: %s\n", meta->general->sensor);
  fprintf(fp, "    processor: %s\n", meta->general->processor);
  fprintf(fp, "    data_type: %s\n", meta->general->data_type);
  fprintf(fp, "    system: %s\n", meta->general->system);
  fprintf(fp, "    orbit: %d\n", meta->general->orbit);
  fprintf(fp, "    frame: %d\n", meta->general->frame);
  fprintf(fp, "    line_count: %d\n", meta->general->line_count);
  fprintf(fp, "    sample_count: %d\n", meta->general->sample_count);
  fprintf(fp, "    start_line: %d\n", meta->general->start_line);
  fprintf(fp, "    start_sample: %d\n", meta->general->start_sample);
  fprintf(fp, "    x_pixel_size: %f\n", meta->general->x_pixel_size);
  fprintf(fp, "    y_pixel_size: %f\n", meta->general->y_pixel_size);
  fprintf(fp, "    center_latitude: %f\n", meta->general->center_latitude);
  fprintf(fp, "    center_longitude: %f\n", meta->general->center_longitude);
  fprintf(fp, "    re_major: %f\n", meta->general->re_major);
  fprintf(fp, "    re_minor: %f\n", meta->general->re_minor);
  fprintf(fp, "    bit_error_rate: %f\n", meta->general->bit_error_rate);
  fprintf(fp, "    missing_lines: %d\n", meta->general->missing_lines);
  fprintf(fp, "}\n");

  /* SAR block.  */
  fprintf(fp, "sar {\n");
  fprintf(fp, "    image_type: %c  # [S=slant range; G=ground range; P=map projected]\n", meta->sar->image_type);
  fprintf(fp, "    look_direction: %c  # (normally R) [R=right; L=left]\n", 
	  meta->sar->look_direction);
  fprintf(fp, "    look_count: %d  # Number of looks from SLC\n", 
	  meta->sar->look_count);
  fprintf(fp, "    look_angle: %f\n", meta->sar->look_angle);
  fprintf(fp, "    deskewed: %d  # [1=yes, 0=no]\n", meta->sar->deskewed);
  fprintf(fp, "    range_time_per_pixel: %f\n", 
	  meta->sar->range_time_per_pixel);
  fprintf(fp, "    azimuth_time_per_pixel: %f\n", 
	  meta->sar->azimuth_time_per_pixel);
  fprintf(fp, "    slant_range_first_pixel: %f\n", 
	  meta->sar->slant_range_first_pixel);
  fprintf(fp, "    slant_shift: %f\n", meta->sar->slant_shift);
  fprintf(fp, "    time_shift: %f\n", meta->sar->time_shift);
  fprintf(fp, "    wavelength: %f\n", meta->sar->wavelength);
  fprintf(fp, "    prf: %f\n", meta->sar->prf);
  fprintf(fp, "    doppler range center frequency: %f\n", 
	  meta->sar->range_doppler_coefficients[0]);
  fprintf(fp, "    doppler range linear coefficient: %f\n", 
	  meta->sar->range_doppler_coefficients[1]);
  fprintf(fp, "    doppler range quadratic coefficient: %f\n", 
	  meta->sar->range_doppler_coefficients[2]);
  fprintf(fp, "    doppler azimuth center frequency: %f\n", 
	  meta->sar->azimuth_doppler_coefficients[0]);
  fprintf(fp, "    doppler azimuth linear coefficient: %f\n", 
	  meta->sar->azimuth_doppler_coefficients[1]);
  fprintf(fp, "    doppler azimuth quadratic coefficient: %f\n", 
	  meta->sar->azimuth_doppler_coefficients[2]);
  fprintf(fp, "    satellite_binary_time: %s\n", 
	  meta->sar->satellite_binary_time);
  fprintf(fp, "    satellite_clock_time: %s  # (UTC)\n", 
	  meta->sar->satellite_clock_time);
  fprintf(fp, "}\n");

  /* State block.  */
  fprintf(fp, "state {\n");
  fprintf(fp, "    year: %d\n", meta->state_vectors->year);
  fprintf(fp, "    julDay: %d\n", meta->state_vectors->julDay);
  fprintf(fp, "    second: %f\n", meta->state_vectors->second);
  fprintf(fp, "    vector_count: %d\n", meta->state_vectors->vector_count);
  { 
    int i;
    for ( i = 0 ; i < meta->state_vectors->vector_count ; i++ ) {
      fprintf(fp, "    vector {\n");
      fprintf(fp, "        time: %f  # From start of image\n", 
	      meta->state_vectors->vecs[i].time);
      fprintf(fp, "        X coordinate, earth-fixed [m]: %f\n", 
	      meta->state_vectors->vecs[i].vec.pos.x);
      fprintf(fp, "        Y coordinate, earth-fixed [m]: %f\n", 
	      meta->state_vectors->vecs[i].vec.pos.y);
      fprintf(fp, "        Z coordinate, earth-fixed [m]: %f\n", 
	      meta->state_vectors->vecs[i].vec.pos.z);
      fprintf(fp, "        X velocity, earth-fixed [m/s]: %f\n", 
	      meta->state_vectors->vecs[i].vec.vel.x);
      fprintf(fp, "        Y velocity, earth-fixed [m/s]: %f\n", 
	      meta->state_vectors->vecs[i].vec.vel.y);
      fprintf(fp, "        Z velocity, earth-fixed [m/s]: %f\n", 
	      meta->state_vectors->vecs[i].vec.vel.z);
      fprintf(fp, "    }\n");
    }
  }
  fprintf(fp, "}\n");

  /* Projection parameters block, if appropriate.  */
  if ( meta->sar->image_type == 'P' ) {
    fprintf(fp, "projection {\n");
    fprintf(fp, "    type: %c\n", meta->projection->type);
    fprintf(fp, "    startX: %f\n", meta->projection->startX);
    fprintf(fp, "    startY: %f\n", meta->projection->startY);
    fprintf(fp, "    perX: %f\n", meta->projection->perX);
    fprintf(fp, "    perY: %f\n", meta->projection->perY);
    fprintf(fp, "    units: %s\n", meta->projection->units);
    fprintf(fp, "    hem: %c  # S=>southern, other=>northern\n", 
	    meta->projection->hem);
    fprintf(fp, "    re_major: %f\n", meta->projection->re_major);
    fprintf(fp, "    re_minor: %f\n", meta->projection->re_minor);
    fprintf(fp, "    param {\n");
    switch ( meta->projection->type ) {
    case 'A': /* Along-track/cross-track projection.  */
      fprintf(fp, "        atct {\n");
      fprintf(fp, "            rlocal: %f  # Earth radius at scene center\n", 
	      meta->projection->param.atct.rlocal);
      fprintf(fp, "            alpha1: %f  # Rotation angle 1\n", 
	      meta->projection->param.atct.alpha1);
      fprintf(fp, "            alpha2: %f  # Rotation angle 2\n", 
	      meta->projection->param.atct.alpha2);
      fprintf(fp, "            alpha3: %f  # Rotation angle 3\n", 
	      meta->projection->param.atct.alpha3);
      break;
    case 'B': /* Lambert conformal conic projection.  */
      fprintf(fp, "        lambert {\n");
      fprintf(fp, "            plat1: %f  # First standard parallel\n", 
	      meta->projection->param.lambert.plat1);
      fprintf(fp, "            plat2: %f  # Second standard parallel\n", 
	      meta->projection->param.lambert.plat2);
      fprintf(fp, "            lat0: %f  # Original latitude\n", 
	      meta->projection->param.lambert.lat0);
      fprintf(fp, "            lon0: %f  # Original longitude\n", 
	      meta->projection->param.lambert.lon0);
      break;
    case 'P': /* Polar stereographic projection.  */
      fprintf(fp, "        polar stereographic {\n");
      fprintf(fp, "            lat: %f  # Reference latitude\n",
	      meta->projection->param.ps.slat);
      fprintf(fp, "            lon: %f  # Regerence longitude\n", 
	      meta->projection->param.ps.slon);
      break;
    case 'U': /* Universal transverse mercator projection.  */
      fprintf(fp, "        utm {\n");
      fprintf(fp, "            zone: %d\n", meta->projection->param.utm.zone);
      break;
    default: 
      err_die("unknown projection type seen in function '%s'\n", __func__);
    }
  }

  fprintf(fp, "        }\n");
  fprintf(fp, "    }\n");
  fprintf(fp, "}\n");
  
  FCLOSE(fp);
  
  return;
}
