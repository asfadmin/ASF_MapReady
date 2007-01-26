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

char *sprn_shr(struct scene_header_rec *sh)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n*********** begin of Scene Header Record  *******************\n");
  add(&ret, "\n Product ID\t\t\t\t\t\t%16s", sh->product_id);
  add(&ret, "\n Uncorrected scene ID\t\t\t%16s", sh->uncorr_sc_id);
  add(&ret, "\n Level 1A and 1B1 scene latiude\t\t\t%16.7f", sh->sc_lat);
  add(&ret, "\n Level 1A and 1B1 scene longitude\t\t%16.7f", sh->sc_lon);
  add(&ret, "\n Line number of level 1A and 1B1 scene center\t%16.7f", 
	  sh->sc_line);
  add(&ret, "\n Sample number of level 1A and 1B1 scene center\t%16.7f", 
	  sh->sc_sample);
  add(&ret, "\n Scene center time\t\t%16s", sh->sc_time);
  add(&ret, "\n Time offset from nominal RSP center\t\t%16d", sh->time_off);
  add(&ret, "\n RSP ID\t\t\t\t\t\t\t%16s", sh->sc_shift);
  add(&ret, "\n Orbits per cycle\t\t\t\t%16d", sh->orbit_cycle);
  add(&ret, "\n Level 1B2 scene ID\t\t\t\t%16s", sh->sc_id);
  add(&ret, "\n Level 1B2 scene center latitude\t\t%16.7f", sh->sc_lat2);
  add(&ret, "\n Level 1B2 scene center longitude\t\t%16.7f", sh->sc_lon2);
  add(&ret, "\n Line number for level 1B2 scene center\t\t%16.7f", sh->sc_line2);
  add(&ret, "\n Sample number for level 1B2 scene center\t%16.7f", 
	  sh->sc_sample2);
  add(&ret, "\n Orientation angle\t\t\t\t%16s", sh->orient_angle);
  add(&ret, "\n Incidence angle\t\t\t\t\t%16s", sh->inc_angle);
  add(&ret, "\n Mission ID\t\t\t\t\t\t%16s", sh->mission_id);
  add(&ret, "\n Sensor ID\t\t\t\t\t\t%16s", sh->sensor_id);
  add(&ret, "\n Calculated orbit number\t\t\t%16d", sh->orbit);
  add(&ret, "\n Orbit direction\t\t\t\t\t%16s", sh->orbit_dir);
  add(&ret, "\n Off-nadir mirror pointing angle\t\t%16s", sh->off_nadir_angle);
  add(&ret, "\n Acquisition date\t\t\t\t%16s", sh->acq_date);
  add(&ret, "\n Latitude and longitude of scene center\t\t%16s", sh->center_loc);
  add(&ret, "\n Type of sensor and spectrum identification\t%16s", 
	  sh->sensor_type);
  add(&ret, "\n Sun angle at product scene center\t\t%16s", sh->sun_angle);
  add(&ret, "\n Processing code\t\t\t\t%16s", sh->proc_code);
  add(&ret, "\n Identification of component agent and project\t%16s", 
	  sh->project);
  add(&ret, "\n Scene ID of work order\t\t\t\t%16s", sh->work_scene_id);
  add(&ret, "\n Number of effective bands in image\t\t%16d", sh->no_bands);
  add(&ret, "\n Number of pixels per line in image\t\t%16d", sh->samples);
  add(&ret, "\n Number of scene lines in image\t\t\t%16d", sh->lines);
  add(&ret, "\n Radiometric resolution\t\t\t\t%16d", sh->radio_res);
  add(&ret, "\n Level 1B2 option\t\t\t\t\t%16s", sh->level_opt);
  add(&ret, "\n Resampling method\t\t\t\t\t%16s", sh->resample);
  add(&ret, "\n Map projection\t\t\t\t\t\t%16s", sh->map_proj);
  add(&ret, "\n Correction level\t\t\t\t\t%16s", sh->corr_level);
  add(&ret, "\n Number of map projection ancillary records\t%16d", sh->proj_recs);
  add(&ret, "\n Number of radiometric ancillary records\t%16d", sh->radio_recs);
  add(&ret, "\n Effective bands (CCD)\t\t\t\t%16d", sh->eff_bands);
  add(&ret, "\n Image format\t\t\t\t\t\t%16s", sh->img_format);
  add(&ret, "\n Latitude of scene left upper corner\t\t%16.7f", sh->lat_ul);
  add(&ret, "\n Longitude of scene left upper corner\t\t%16.7f", sh->lon_ul);
  add(&ret, "\n Latitude of scene right upper corner\t\t%16.7f", sh->lat_ur);
  add(&ret, "\n Longitude of scene right upper corner\t\t%16.7f", sh->lon_ur);
  add(&ret, "\n Latitude of scene left lower corner\t\t%16.7f", sh->lat_ll);
  add(&ret, "\n Longitude of scene left lower corner\t\t%16.7f", sh->lon_ll);
  add(&ret, "\n Latitude of scene right lower corner\t\t%16.7f", sh->lat_lr);
  add(&ret, "\n Longitude of scene right lower corner\t\t%16.7f", sh->lon_lr);
  add(&ret, "\n Time system status\t\t\t\t%16s", sh->time_sys_status);
  add(&ret, "\n Absolute navigation status\t\t\t%16s", sh->abs_nav_status);
  add(&ret, "\n Attitude determination flag\t\t\t%16s", sh->att_det_flag);
  add(&ret, "\n Accuracy of used orbit data\t\t\t%16s", sh->acc_orbit);
  add(&ret, "\n Accuracy of used attitude data\t\t\t%16s", sh->acc_att);
  add(&ret, "\n Yaw steering flag\t\t\t\t%16s", sh->yaw_flag);
  add(&ret, "\n*********** end of Scene Header record ********************\n");
  return ret;
}

void prn_shr(FILE *fp, struct scene_header_rec *sh)
{
    char *rec = sprn_shr(sh);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
