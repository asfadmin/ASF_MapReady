#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_ampr(struct alos_map_proj_rec *mp)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  int ii;

  add(&ret, "\n*********** begin of Map Projection (ALOS) record ************\n");
  add(&ret, "\n Number of nominal pixels per line (1A, 1B1)\t\t%24d", 
	  mp->sample_count);
  add(&ret, "\n Number of nominal lines per scene (1A, 1B1)\t\t%24d", 
	  mp->line_count);
  add(&ret, "\n Nominal inter-pixel distance at scene center\t\t%24.7f", 
	  mp->x_pixel_size);
  add(&ret, "\n Nominal inter-line distance at scene center\t\t%24.7f", 
	  mp->y_pixel_size);
  add(&ret, "\n Image skew (milliradian) at scene center\t\t%24.7f", 
	  mp->image_skew);
  add(&ret, "\n Hemisphere - 0 is North, 1 is South\t\t\t%24d",mp->hem);
  add(&ret, "\n UTM zone number (1 to 60)\t\t\t\t%24d", mp->utm_zone);
  add(&ret, "\n Scene center position (northing - km)\t\t\t%24.7f", 
	  mp->sc_cen_northing);
  add(&ret, "\n Scene center position (easting - km)\t\t\t%24.7f", 
	  mp->sc_cen_easting);
  add(&ret, "\n Angle map projection and true north\t\t\t%24.7f", 
	  mp->angle_true_north);
  add(&ret, "\n Latitude of map projection origin (1B2)\t\t%24.7f", 
	  mp->lat_map_origin);
  add(&ret, "\n Longitude of map projection origin (1B2)\t\t%24.7f", 
	  mp->lon_map_origin);
  add(&ret, "\n Reference latitude (1B2)\t\t\t\t%24.7f", mp->ref_lat);
  add(&ret, "\n Reference longitude (1B2)\t\t\t\t%24.7f", mp->ref_lon);
  add(&ret, "\n X coordinates of the scene center (1B2)\t\t%24.7f", 
	  mp->sc_center_x);
  add(&ret, "\n Y coordinates of the scene center (1B2)\t\t%24.7f", 
	  mp->sc_center_y);
  add(&ret, "\n Angle map projection and true north\t\t\t%24.7f", 
	  mp->angle_true_north2);
  add(&ret, "\n Number of nominal pixels per line (1B2)\t\t%24.7f", 
	  mp->sample_count2);
  add(&ret, "\n Number of nominal lines per scene (1B2)\t\t%24.7f", 
	  mp->line_count2);
  add(&ret, "\n Nominal inter-pixel distance at scene center (1B2)\t%24.7f",
	  mp->x_pixel_size2);
  add(&ret, "\n Nominal inter-pixel distance at scene center (1B2)\t%24.7f",
	  mp->y_pixel_size2);
  add(&ret, "\n Angle map projection and true north\t\t\t%24.7f", 
	  mp->angle_true_north3);
  add(&ret, "\n Nominal satellite orbit inclination (degree)\t\t%24.7f", 
	  mp->orbit_inc);
  add(&ret, "\n Longitude of nominal ascending node (radian)\t\t%24.7f", 
	  mp->lon_asc_node);
  add(&ret, "\n Nominal satellite altitude (km)\t\t\t%24.7f", mp->sat_height);
  add(&ret, "\n Nominal ground speed (km/s)\t\t\t\t%24.7f", mp->gr_speed);
  add(&ret, "\n Satellite heading angle (radian)\t\t\t%24.7f", mp->head_angle);
  add(&ret, "\n Swath angle (nominal - degree)\t\t\t\t%24.7f", mp->swath_angle);
  add(&ret, "\n Nominal scan rate (scan/s)\t\t\t\t%24.7f", mp->scan_rate);
  add(&ret, "\n Name of reference ellipsoid\t\t\t\t%24s", mp->ref_ellipsoid);
  add(&ret, "\n Semimajor axis of reference ellipsoid (m)\t\t%24.7f", 
	  mp->ref_major_axis);
  add(&ret, "\n Semiminor axis of reference ellipsoid (m)\t\t%24.7f",
	  mp->ref_minor_axis);
  add(&ret, "\n Geodetic coordinates name\t\t\t\t\t%24s", mp->geod_coord_name);
  for (ii=0; ii<10; ii++)
    add(&ret, "\n latitude transformation coefficients - phi(%d)\t\t%24.16f", 
	   ii, mp->phi[ii]);
  for (ii=0; ii<10; ii++)
    add(&ret, "\n longitude transformaiton coefficients - lambda (%d)\t%24.16f",
	   ii, mp->lambda[ii]);
  for (ii=0; ii<10; ii++)
    add(&ret, "\n pixel transformation coefficients - i(%d)\t\t%24.16f", 
	   ii, mp->i[ii]);
  for (ii=0; ii<10; ii++)
    add(&ret, "\n line transformation coefficients - j(%d)\t\t%24.16f", 
	    ii, mp->j[ii]);
  /*
    int a,b,c,d,e,f;            // map coefficients
    int phi_ccd[10][8];         // latitude transformation coefficients (CCDs)
    int lambda_ccd[10][8];      // longitude transformaiton coefficients (CCDs)
    int i_ccd[10][8];           // pixel transformation coefficients (CCDs)
    int j_ccd[10][8];           // line transformation coefficients (CCDs)  
  */
  add(&ret, "\n*********** end of Map Projection (ALOS) record **************\n");
  return ret;
}

void prn_ampr(FILE *fp, struct alos_map_proj_rec *mp)
{
    char *rec = sprn_ampr(mp);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
