/********************************************************************
NAME:   prn_dssr.c

PURPOSE:   Reads the input (.ldr) file given and prints all of the
           fields of the dataset summary record.

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           4/96   T. Logan (ASF)
*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

void prn_shr(FILE *fp, struct scene_header_rec *sh)
{
  fprintf(fp, "\n*********** begin of Scene Header Record  *******************\n");
  fprintf(fp, "\n Product ID\t\t\t\t\t\t%16s", sh->product_id);
  fprintf(fp, "\n Uncorrected scene ID\t\t\t%16s", sh->uncorr_sc_id);
  fprintf(fp, "\n Level 1A and 1B1 scene latiude\t\t\t%16.7f", sh->sc_lat);
  fprintf(fp, "\n Level 1A and 1B1 scene longitude\t\t%16.7f", sh->sc_lon);
  fprintf(fp, "\n Line number of level 1A and 1B1 scene center\t%16.7f", 
	  sh->sc_line);
  fprintf(fp, "\n Sample number of level 1A and 1B1 scene center\t%16.7f", 
	  sh->sc_sample);
  fprintf(fp, "\n Scene center time\t\t%16s", sh->sc_time);
  fprintf(fp, "\n Time offset from nominal RSP center\t\t%16d", sh->time_off);
  fprintf(fp, "\n RSP ID\t\t\t\t\t\t\t%16s", sh->sc_shift);
  fprintf(fp, "\n Orbits per cycle\t\t\t\t%16d", sh->orbit_cycle);
  fprintf(fp, "\n Level 1B2 scene ID\t\t\t\t%16s", sh->sc_id);
  fprintf(fp, "\n Level 1B2 scene center latitude\t\t%16.7f", sh->sc_lat2);
  fprintf(fp, "\n Level 1B2 scene center longitude\t\t%16.7f", sh->sc_lon2);
  fprintf(fp, "\n Line number for level 1B2 scene center\t\t%16.7f", sh->sc_line2);
  fprintf(fp, "\n Sample number for level 1B2 scene center\t%16.7f", 
	  sh->sc_sample2);
  fprintf(fp, "\n Orientation angle\t\t\t\t%16s", sh->orient_angle);
  fprintf(fp, "\n Incidence angle\t\t\t\t\t%16s", sh->inc_angle);
  fprintf(fp, "\n Mission ID\t\t\t\t\t\t%16s", sh->mission_id);
  fprintf(fp, "\n Sensor ID\t\t\t\t\t\t%16s", sh->sensor_id);
  fprintf(fp, "\n Calculated orbit number\t\t\t%16d", sh->orbit);
  fprintf(fp, "\n Orbit direction\t\t\t\t\t%16s", sh->orbit_dir);
  //fprintf(fp, "\n Off-nadir mirror pointing angle\t%16.7f", sh->off_nadir_angle);
  fprintf(fp, "\n Acquisition date\t\t\t\t%16s", sh->acq_date);
  fprintf(fp, "\n Latitude and longitude of scene center\t\t%16s", sh->center_loc);
  fprintf(fp, "\n Type of sensor and spectrum identification\t%16s", 
	  sh->sensor_type);
  fprintf(fp, "\n Sun angle at product scene center\t\t%16s", sh->sun_angle);
  fprintf(fp, "\n Processing code\t\t\t\t%16s", sh->proc_code);
  fprintf(fp, "\n Identification of component agent and project\t%16s", 
	  sh->project);
  fprintf(fp, "\n Scene ID of work order\t\t\t\t%16s", sh->work_scene_id);
  fprintf(fp, "\n Number of effective bands in image\t\t%16d", sh->no_bands);
  fprintf(fp, "\n Number of pixels per line in image\t\t%16d", sh->samples);
  fprintf(fp, "\n Number of scene lines in image\t\t\t%16d", sh->lines);
  fprintf(fp, "\n Radiometric resolution\t\t\t\t%16d", sh->radio_res);
  fprintf(fp, "\n Level 1B2 option\t\t\t\t\t%16s", sh->level_opt);
  fprintf(fp, "\n Resampling method\t\t\t\t\t%16s", sh->resample);
  fprintf(fp, "\n Map projection\t\t\t\t\t\t%16s", sh->map_proj);
  fprintf(fp, "\n Correction level\t\t\t\t\t%16s", sh->corr_level);
  fprintf(fp, "\n Number of map projection ancillary records\t%16d", sh->proj_recs);
  fprintf(fp, "\n Number of radiometric ancillary records\t%16d", sh->radio_recs);
  fprintf(fp, "\n Effective bands (CCD)\t\t\t\t%16d", sh->eff_bands);
  fprintf(fp, "\n Image format\t\t\t\t\t\t%16s", sh->img_format);
  fprintf(fp, "\n Latitude of scene left upper corner\t\t%16.7f", sh->lat_ul);
  fprintf(fp, "\n Longitude of scene left upper corner\t\t%16.7f", sh->lon_ul);
  fprintf(fp, "\n Latitude of scene right upper corner\t\t%16.7f", sh->lat_ur);
  fprintf(fp, "\n Longitude of scene right upper corner\t\t%16.7f", sh->lon_ur);
  fprintf(fp, "\n Latitude of scene left lower corner\t\t%16.7f", sh->lat_ll);
  fprintf(fp, "\n Longitude of scene left lower corner\t\t%16.7f", sh->lon_ll);
  fprintf(fp, "\n Latitude of scene right lower corner\t\t%16.7f", sh->lat_lr);
  fprintf(fp, "\n Longitude of scene right lower corner\t\t%16.7f", sh->lon_lr);
  fprintf(fp, "\n Time system status\t\t\t\t%16s", sh->time_sys_status);
  fprintf(fp, "\n Absolute navigation status\t\t\t%16s", sh->abs_nav_status);
  fprintf(fp, "\n Attitude determination flag\t\t\t%16s", sh->att_det_flag);
  fprintf(fp, "\n Accuracy of used orbit data\t\t\t%16s", sh->acc_orbit);
  fprintf(fp, "\n Accuracy of used attitude data\t\t\t%16s", sh->acc_att);
  fprintf(fp, "\n Yaw steering flag\t\t\t\t%16s", sh->yaw_flag);
  fprintf(fp, "\n*********** end of Scene Header record ********************\n");
  return;
}
