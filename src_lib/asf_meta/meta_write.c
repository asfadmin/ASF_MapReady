#include <stdio.h>

#include "asf_meta.h"
#include "caplib.h"

/* Given a meta_parameters structure pointer and a file name, write a
   metadata file for that structure.  */
void meta_write(meta_parameters *meta, char *file_name)
{
  FILE *fp = FOPEN(file_name, "w");
  
  fprintf(fp, "Meta version: %f\n", meta->meta_version);

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
  fprintf(fp, "    range_pixel_size: %f\n", meta->general->range_pixel_size);
  fprintf(fp, "    azimuth_pixel_size: %f\n", 
	  meta->general->azimuth_pixel_size);
  fprintf(fp, "    center_latitude: %f\n", meta->general->center_latitude);
  fprintf(fp, "    center_longitude: %f\n", meta->general->center_longitude);
  fprintf(fp, "    re_major: %f\n", meta->general->re_major);
  fprintf(fp, "    re_minor: %f\n", meta->general->re_minor);
  fprintf(fp, "    bit_error_rate: %f\n", meta->general->bit_error_rate);
  fprintf(fp, "    missing_lines: %d\n", meta->general->missing_lines);
  fprintf(fp, "}\n");

  /* SAR block.  */
  fprintf(fp, "sar {\n");
  fprintf(fp, "    proj_type: %c\n", meta->sar->proj_type);
  fprintf(fp, "    look_direction: %c\n", meta->sar->look_direction);
  fprintf(fp, "    look_count: %d\n", meta->sar->look_count);
  fprintf(fp, "    look_angle: %f\n", meta->sar->look_angle);
  fprintf(fp, "    deskewed: %d\n", meta->sar->deskewed);
  fprintf(fp, "    range_time_per_pixel: %f\n", 
	  meta->sar->range_time_per_pixel);
  fprintf(fp, "    azimuth_time_per_pixel: %f\n", 
	  meta->sar->azimuth_time_per_pixel);
  fprintf(fp, "    slant_range_first_pixel: %f\n", 
	  meta->sar->slant_range_first_pixel);
  fprintf(fp, "    slant_shift: %d\n", meta->sar->slant_shift);
  fprintf(fp, "    time_shift: %d\n", meta->sar->time_shift);
  fprintf(fp, "    wavelength: %d\n", meta->sar->wavelength);
  fprintf(fp, "    pfr: %d\n", meta->sar->prf);
  fprintf(fp, "    doppler range center frequency: %d\n", 
	  meta->sar->range_doppler_coefficients[0]);
  fprintf(fp, "    doppler range linear coefficient: %d\n", 
	  meta->sar->range_doppler_coefficients[1]);
  fprintf(fp, "    doppler range quadratic coefficient: %d\n", 
	  meta->sar->range_doppler_coefficients[2]);
  fprintf(fp, "    doppler azimuth center frequency: %d\n", 
	  meta->sar->azimuth_doppler_coefficients[0]);
  fprintf(fp, "    doppler azimuth linear coefficient: %d\n", 
	  meta->sar->azimuth_doppler_coefficients[1]);
  fprintf(fp, "    doppler azimuth quadratic coefficient: %d\n", 
	  meta->sar->azimuth_doppler_coefficients[2]);
  fprintf(fp, "    satellite_binary_time: %s\n", 
	  meta->sar->satellite_binary_time);
  fprintf(fp, "    satellite_clock_time: %s\n", 
	  meta->sar->satellite_clock_time);
  fprintf(fp, "}\n");

  /* Projection parameters block.  */
  fprintf(fp, "projection {\n");
  fprintf(fp, "    type: %c\n", meta->projection->type);
  fprintf(fp, "    startX: %f\n", meta->projection->startX);
  fprintf(fp, "    startY: %f\n", meta->projection->startY);
  fprintf(fp, "    perX: %f\n", meta->projection->perX);
  fprintf(fp, "    perY: %f\n", meta->projection->perY);
  fprintf(fp, "    units: %s\n", meta->projection->units);
  fprintf(fp, "    hem: %c\n", meta->projection->hem);
  fprintf(fp, "    re_major: %d\n", meta->projection->re_major);
  fprintf(fp, "    re_minor: %d\n", meta->projection->re_minor);
  fprintf(fp, "    param {\n");
  switch ( meta->projection->type ) {
  case 'A': /* Along-track/cross-track projection.  */
    fprintf(fp, "        atct {\n");
    fprintf(fp, "            rlocal: %d\n", 
	    meta->projection->param.atct.rlocal);
    fprintf(fp, "            alpha1: %d\n", 
	    meta->projection->param.atct.alpha1);
    fprintf(fp, "            alpha2: %d\n", 
	    meta->projection->param.atct.alpha2);
    fprintf(fp, "            alpha3: %d\n", 
	    meta->projection->param.atct.alpha3);
    break;
  case 'B': /* Lambert conformal conic projection.  */
    fprintf(fp, "        lambert {\n"
    fprintf(fp, "            plat1: %d\n", 
	    meta->projection->param.lambert.plat1);
    fprintf(fp, "            plat2: %d\n", 
	    meta->projection->param.lambert.plat2);
    fprintf(fp, "            lat0: %d\n", 
	    meta->projection->param.lambert.lat0);
    fprintf(fp, "            lon0: %d\n", 
	    meta->projection->param.lambert.lon0);
    break;
  case 'P': /* Polar stereographic projection.  */
    fprintf(fp, "        polar stereographic {\n");
    fprintf(fp, "            lat: %d\n", meta->projection->param.ps.slat);
    fprintf(fp, "            lon: %d\n", meta->projection->param.ps.slon);
    break;
  case



