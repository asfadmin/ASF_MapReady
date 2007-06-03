#include "asf_vector.h"
#include "asf.h"
#include "asf_nan.h"
#include "dateUtil.h"
#include "float_image.h"
#include "ceos_io.h"
#include "libasf_proj.h"
#include <stdio.h>
#include <math.h>

/* 
   When invoking google earth from the command line, you can put a kml
   file as a command line argument, but in that case it wants the kml
   files' location to be fully specified, e.g.:
    googleearth /export/home/user/files/the/dir/here/the_file.kml
*/
static void swap(double *x, double *y)
{
    double tmp = *x;
    *x = *y;
    *y = tmp;
}

void kml_header(FILE *kml_file)
{
    fprintf(kml_file, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    fprintf(kml_file,
            "<kml xmlns=\"http://earth.google.com/kml/2.0\">\n");
    fprintf(kml_file, "<Document>\n");
}

static void
update_latlon_maxes(double lat, double lon, double *max_lat, double *min_lat,
                    double *max_lon, double *min_lon)
{
    if (lat>*max_lat) *max_lat = lat;
    if (lat<*min_lat) *min_lat = lat;

    if (lon>*max_lon) *max_lon = lon;
    if (lon<*min_lon) *min_lon = lon;
}

static void rotate(double x_in, double y_in, double x0, double y0, double ang,
                   double *xr, double *yr)
{
    double x = x_in - x0;
    double y = y_in - y0;

    *xr = x0 + sin(ang)*x + cos(ang)*y;
    *yr = y0 + cos(ang)*x - sin(ang)*y;
}

static void kml_entry_impl(FILE *kml_file, meta_parameters *meta, 
                           char *name, char *png_filename, char *dir)
{
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;
    double lat_UL, lon_UL;
    double lat_UR, lon_UR;
    double lat_LL, lon_LL;
    double lat_LR, lon_LR;

    if (meta->location) {
        lat_UL = meta->location->lat_start_near_range;
        lon_UL = meta->location->lon_start_near_range;
        lat_UR = meta->location->lat_start_far_range;
        lon_UR = meta->location->lon_start_far_range;
        lat_LR = meta->location->lat_end_far_range;
        lon_LR = meta->location->lon_end_far_range;
        lat_LL = meta->location->lat_end_near_range;
        lon_LL = meta->location->lon_end_near_range;
    } else {
        meta_get_latLon(meta, 0, 0, 0, &lat_UL, &lon_UL);
        meta_get_latLon(meta, nl, 0, 0, &lat_LL, &lon_LL);
        meta_get_latLon(meta, nl, ns, 0, &lat_LR, &lon_LR);
        meta_get_latLon(meta, 0, ns, 0, &lat_UR, &lon_UR);
    }

    fprintf(kml_file, "<Placemark>\n");
    fprintf(kml_file, "  <description><![CDATA[\n");
    fprintf(kml_file, "<strong>Sensor</strong>: %s<br>\n", meta->general->sensor);
    fprintf(kml_file, "<strong>Sensor name</strong>: %s<br>\n", 
	    meta->general->sensor_name);
    fprintf(kml_file, "<strong>Beam mode</strong>: %s<br>\n", meta->general->mode);
    fprintf(kml_file, "<strong>Orbit</strong>: %d<br>\n", meta->general->orbit);
    fprintf(kml_file, "<strong>Frame</strong>: %d<br>\n", meta->general->frame);
    fprintf(kml_file, "<strong>Acquisition date</strong>: %s<br>\n", 
	    meta->general->acquisition_date);
    if (meta->general->orbit_direction == 'D')
      fprintf(kml_file, "<strong>Orbit direction</strong>: Descending<br>\n");
    else if (meta->general->orbit_direction == 'A')
      fprintf(kml_file, "<strong>Orbit direction</strong>: Ascending<br>\n");
    fprintf(kml_file, "<strong>Center - Lat</strong>: %9.4lf, ", 
	    meta->general->center_latitude);
    fprintf(kml_file, "<strong>Lon</strong>: %9.4lf<br>\n", 
	    meta->general->center_longitude);
    fprintf(kml_file, "<strong>Corner 1 - Lat</strong>: %9.4lf, ", lat_UL);
    fprintf(kml_file, "<strong>Lon</strong>: %9.4lf<br>\n", lon_UL);
    fprintf(kml_file, "<strong>Corner 2 - Lat</strong>: %9.4lf, ", lat_UR);
    fprintf(kml_file, "<strong>Lon</strong>: %9.4lf<br>\n", lon_UR);
    fprintf(kml_file, "<strong>Corner 3 - Lat</strong>: %9.4lf, ", lat_LR);
    fprintf(kml_file, "<strong>Lon</strong>: %9.4lf<br>\n", lon_LR);
    fprintf(kml_file, "<strong>Corner 4 - Lat</strong>: %9.4lf, ", lat_LL);
    fprintf(kml_file, "<strong>Lon</strong>: %9.4lf<br>\n", lon_LL);
    fprintf(kml_file, "  ]]></description>\n");
    fprintf(kml_file, "  <name>%s</name>\n", name);
    fprintf(kml_file, "  <LookAt>\n");
    fprintf(kml_file, "    <longitude>%.10f</longitude>\n",
            meta->general->center_longitude);
    fprintf(kml_file, "    <latitude>%.10f</latitude>\n",
            meta->general->center_latitude);
    fprintf(kml_file, "    <range>400000</range>\n");
    fprintf(kml_file, "    <tilt>30</tilt>\n");
    //fprintf(kml_file, "    <heading>50</heading>\n");
    fprintf(kml_file, "  </LookAt>\n");
    fprintf(kml_file, "  <visibility>1</visibility>\n");
    fprintf(kml_file, "  <open>1</open>\n");
    write_kml_style_keys(kml_file);
    fprintf(kml_file, "  <Polygon>\n");
    // different behavior if we have an overlay - no extrude, and draw on
    // the ground instead of at an absolute height above the terrain
	fprintf(kml_file, "    <extrude>%d</extrude>\n",
        png_filename ? 0 : 1);
	fprintf(kml_file, "    <altitudeMode>%s</altitudeMode>\n",
        png_filename ? "relativeToGround" : "absolute");
    fprintf(kml_file, "    <outerBoundaryIs>\n");
    fprintf(kml_file, "      <LinearRing>\n");
    fprintf(kml_file, "        <coordinates>\n");

    // have the outline on the ground, if we are doing an overlay
    // otherwise, draw it up in the air a little
    int alt = png_filename ? 500 : 7000;
    fprintf(kml_file, "          %.12f,%.12f,%d\n", lon_UL, lat_UL, alt);
    fprintf(kml_file, "          %.12f,%.12f,%d\n", lon_LL, lat_LL, alt);
    fprintf(kml_file, "          %.12f,%.12f,%d\n", lon_LR, lat_LR, alt);
    fprintf(kml_file, "          %.12f,%.12f,%d\n", lon_UR, lat_UR, alt);
    fprintf(kml_file, "          %.12f,%.12f,%d\n", lon_UL, lat_UL, alt);
    
    fprintf(kml_file, "        </coordinates>\n");
    fprintf(kml_file, "      </LinearRing>\n");
    fprintf(kml_file, "    </outerBoundaryIs>\n");
    fprintf(kml_file, "  </Polygon>\n");
    fprintf(kml_file, "</Placemark>\n");

    if (!png_filename)
        printf("No overlay: Unprocessed data.\n");
    if (!meta->projection)
        printf("No overlay: Unprojected data.\n");
    else if (meta->projection->type != UNIVERSAL_TRANSVERSE_MERCATOR)
        printf("No overlay: Not UTM.\n");
    else
    {
        printf("png filename: %s\n", png_filename);

        if (!dir)
            asfPrintError("Must pass in a directory for the overlay files!\n");
        else
        {
            double h = 0.0;

            if (meta->location) {
                // override what we found earlier, when drawing the box, here
                // we want the actual bounding box, not the satellite swath
                meta_get_latLon(meta, 0, 0, 0, &lat_UL, &lon_UL);
                meta_get_latLon(meta, nl, 0, 0, &lat_LL, &lon_LL);
                meta_get_latLon(meta, nl, ns, 0, &lat_LR, &lon_LR);
                meta_get_latLon(meta, 0, ns, 0, &lat_UR, &lon_UR);
            }

            double clat = meta->general->center_latitude;
            double clon = meta->general->center_longitude;
            printf("1) Estimated center lat, lon:  %lf, %lf\n", clat, clon);
            meta_get_latLon(meta, nl/2, ns/2, h, &clat, &clon);
            printf("2) Calculated center lat, lon: %lf, %lf\n", clat, clon);

            double ul_x, ul_y, ur_x, ur_y, ll_x, ll_y, lr_x, lr_y,
                ctr_x, ctr_y;
            double ul_x_rot, ul_y_rot, ur_x_rot, ur_y_rot,
                ll_x_rot, ll_y_rot, lr_x_rot, lr_y_rot;
            double lat_UL_rot, lon_UL_rot;
            double lat_UR_rot, lon_UR_rot;
            double lat_LR_rot, lon_LR_rot;
            double lat_LL_rot, lon_LL_rot;

            int zone = utm_zone(clon);
            
            latLon2UTM_zone(lat_UL, lon_UL, h, zone, &ul_x, &ul_y);
            latLon2UTM_zone(lat_UR, lon_UR, h, zone, &ur_x, &ur_y);
            latLon2UTM_zone(lat_LR, lon_LR, h, zone, &lr_x, &lr_y);
            latLon2UTM_zone(lat_LL, lon_LL, h, zone, &ll_x, &ll_y);
            latLon2UTM_zone(clat, clon, h, zone, &ctr_x, &ctr_y);

            double ang1 = atan2(ul_y-ur_y, ur_x-ul_x);
            double ang2 = atan2(ll_y-lr_y, lr_x-ll_x);
            double ang = (ang1+ang2)/2;

            printf("UL: %lf,%lf\n", ul_x,ul_y);
            printf("UR: %lf,%lf\n", ur_x,ur_y);
            printf("LL: %lf,%lf\n", ll_x,ll_y);
            printf("LR: %lf,%lf\n", lr_x,lr_y);
            printf("angle= %lf\n", ang*R2D);

            rotate(ul_x, ul_y, ctr_x, ctr_y, ang, &ul_x_rot, &ul_y_rot);
            rotate(ur_x, ur_y, ctr_x, ctr_y, ang, &ur_x_rot, &ur_y_rot);
            rotate(ll_x, ll_y, ctr_x, ctr_y, ang, &ll_x_rot, &ll_y_rot);
            rotate(lr_x, lr_y, ctr_x, ctr_y, ang, &lr_x_rot, &lr_y_rot);

            printf("Rotated UL: %lf,%lf\n", ul_x_rot,ul_y_rot);
            printf("Rotated UR: %lf,%lf\n", ur_x_rot,ur_y_rot);
            printf("Rotated LL: %lf,%lf\n", ll_x_rot,ll_y_rot);
            printf("Rotated LR: %lf,%lf\n", lr_x_rot,lr_y_rot);

            if (clat < 0) {
                // account for the false northing in the southern hemisphere
                ul_y_rot -= 10000000;
                ur_y_rot -= 10000000;
                ll_y_rot -= 10000000;
                lr_y_rot -= 10000000;
            }

            UTM2latLon(ul_x_rot, ul_y_rot, h, zone, &lat_UL_rot, &lon_UL_rot);
            UTM2latLon(ur_x_rot, ur_y_rot, h, zone, &lat_UR_rot, &lon_UR_rot);
            UTM2latLon(ll_x_rot, ll_y_rot, h, zone, &lat_LL_rot, &lon_LL_rot);
            UTM2latLon(lr_x_rot, lr_y_rot, h, zone, &lat_LR_rot, &lon_LR_rot);
            //double box_north_lat = (lat_UL_rot + lat_UR_rot)/2;
            //double box_south_lat = (lat_LL_rot + lat_LR_rot)/2;
            //double box_east_lon = (lon_UL_rot + lon_LL_rot)/2;
            //double box_west_lon = (lon_UR_rot + lon_LR_rot)/2;

            double box_north_lat = lat_UL_rot;
            double box_south_lat = lat_LR_rot;
            double box_east_lon = lon_UL_rot;
            double box_west_lon = lon_LR_rot;
            
            if (box_south_lat > box_north_lat)
                swap(&box_south_lat, &box_north_lat);
            if (box_east_lon < box_west_lon)
                swap(&box_east_lon, &box_west_lon);

            //if (meta->general->orbit_direction == 'D')
            //    swap(&box_south_lat, &box_north_lat);

            fprintf(kml_file, "<GroundOverlay>\n");
            //fprintf(kml_file, "  <description>\n");
            //fprintf(kml_file, "    sensor/mode: %s/%s\n",
            //        meta->general->sensor, meta->general->mode);
            //fprintf(kml_file, "    orbit/frame: %d/%d\n",
            //        meta->general->orbit, meta->general->frame);
            //fprintf(kml_file, "  </description>\n");
            fprintf(kml_file, "  <name>%s</name>\n", name);
            fprintf(kml_file, "  <LookAt>\n");
            fprintf(kml_file, "    <longitude>%.10f</longitude>\n", clon);
            fprintf(kml_file, "    <latitude>%.10f</latitude>\n", clat);
            fprintf(kml_file, "    <range>400000</range>\n");
            fprintf(kml_file, "    <tilt>45</tilt>\n");
            fprintf(kml_file, "    <heading>50</heading>\n");
            fprintf(kml_file, "  </LookAt>\n");
            fprintf(kml_file, "  <color>ffffffff</color>\n");
            fprintf(kml_file, "  <Icon>\n");
            fprintf(kml_file, "      <href>%s</href>\n", png_filename);
            fprintf(kml_file, "  </Icon>\n");
            fprintf(kml_file, "  <LatLonBox>\n");
            fprintf(kml_file, "      <north>%.12f</north>\n", box_north_lat);
            fprintf(kml_file, "      <south>%.12f</south>\n", box_south_lat);
            fprintf(kml_file, "      <east>%.12f</east>\n", box_east_lon);
            fprintf(kml_file, "      <west>%.12f</west>\n", box_west_lon);
            fprintf(kml_file, "      <rotation>%.12f</rotation>\n", ang*R2D);
            fprintf(kml_file, "  </LatLonBox>\n");

            fprintf(kml_file, "</GroundOverlay>\n");
        }
    }
}

static
void kml_entry_overlay(FILE *kml_file, char *name)
{
  julian_date jdate;
  ymd_date ymd;
  meta_parameters *meta;
  FloatImage *image;
  char input_image[512], output_image[512];
  int nl, ns;
  double lat_UL, lon_UL;
  double lat_UR, lon_UR;
  double lat_LL, lon_LL;
  double lat_LR, lon_LR;
  double max_lat = -90, max_lon = -180, min_lat = 90, min_lon = 180;
  char *mon[13]={"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
		 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
  double ul_x, ul_y, ur_x, ur_y, ang;
  
  meta = meta_read(name);
  nl = meta->general->line_count;
  ns = meta->general->sample_count;
  
  jdate.year = meta->state_vectors->year;
  jdate.jd = meta->state_vectors->julDay;
  date_jd2ymd(&jdate, &ymd);
  
  // Get corner coordinates from metadata
  meta_get_latLon(meta, 0, 0, 0, &lat_UL, &lon_UL);
  meta_get_latLon(meta, nl, 0, 0, &lat_LL, &lon_LL);
  meta_get_latLon(meta, nl, ns, 0, &lat_LR, &lon_LR);
  meta_get_latLon(meta, 0, ns, 0, &lat_UR, &lon_UR);
  
  // Figure out rotation and box coordinates
  latLon2UTM(lat_UL, lon_UL, 0.0, &ul_x, &ul_y);
  latLon2UTM(lat_UR, lon_UR, 0.0, &ur_x, &ur_y);
  ang = atan2(ur_y-ul_y, ur_x-ul_x);

  // Need to figure out the correct way of applying the affine transformation
  // that Kirk came up with. Should be able to get everything line up.
  update_latlon_maxes(lat_UL, lon_UL, &max_lat, &min_lat, &max_lon, &min_lon);
  update_latlon_maxes(lat_LL, lon_LL, &max_lat, &min_lat, &max_lon, &min_lon);
  update_latlon_maxes(lat_LR, lon_LR, &max_lat, &min_lat, &max_lon, &min_lon);
  update_latlon_maxes(lat_UR, lon_UR, &max_lat, &min_lat, &max_lon, &min_lon);

  // Read the image and generate a JPEG from it
  create_name(input_image, name, ".img");
  create_name(output_image, name, ".png");
  //img2png(meta, input_image, 512, output_image);
  
  // Write everything into kml file
  fprintf(kml_file, "<Placemark>\n");
  fprintf(kml_file, "  <description><![CDATA[\n");
  fprintf(kml_file, "<strong>Sensor</strong>: %s<br>\n", meta->general->sensor);
  fprintf(kml_file, "<strong>Beam mode</strong>: %s<br>\n", meta->general->mode);
  fprintf(kml_file, "<strong>Orbit</strong>: %d<br>\n", meta->general->orbit);
  fprintf(kml_file, "<strong>Frame</strong>: %d<br>\n", meta->general->frame);
  fprintf(kml_file, "<strong>Acquisition date</strong>: %d-%s-%d<br>\n", 
	  ymd.day, mon[ymd.month], ymd.year);
  if (meta->general->orbit_direction == 'D')
    fprintf(kml_file, "<strong>Orbit direction</strong>: Descending<br>\n");
  else if (meta->general->orbit_direction == 'A')
    fprintf(kml_file, "<strong>Orbit direction</strong>: Ascending<br>\n");
  fprintf(kml_file, "<strong>Center latitude</strong>: %9.4lf<br>\n", 
	  meta->general->center_latitude);
  fprintf(kml_file, "<strong>Center longitude</strong>: %9.4lf<br>\n", 
	  meta->general->center_longitude);
  fprintf(kml_file, "  ]]></description>\n");
  fprintf(kml_file, "  <name>%s</name>\n", name);
  fprintf(kml_file, "  <LookAt>\n");
  fprintf(kml_file, "    <longitude>%.10f</longitude>\n",
	  meta->general->center_longitude);
  fprintf(kml_file, "    <latitude>%.10f</latitude>\n",
	  meta->general->center_latitude);
  fprintf(kml_file, "    <range>400000</range>\n");
  fprintf(kml_file, "    <tilt>45</tilt>\n");
  fprintf(kml_file, "    <heading>50</heading>\n");
  fprintf(kml_file, "  </LookAt>\n");
  fprintf(kml_file, "  <visibility>1</visibility>\n");
  fprintf(kml_file, "  <open>1</open>\n");
  write_kml_style_keys(kml_file);
  fprintf(kml_file, "  <LineString>\n");
  fprintf(kml_file, "    <extrude>1</extrude>\n");
  fprintf(kml_file, "    <tessellate>1</tessellate>\n");
  fprintf(kml_file, "    <altitudeMode>absolute</altitudeMode>\n");
  fprintf(kml_file, "    <coordinates>\n");
  
  fprintf(kml_file, "      %.12f,%.12f,4000\n", lon_UL, lat_UL);
  fprintf(kml_file, "      %.12f,%.12f,4000\n", lon_LL, lat_LL);
  fprintf(kml_file, "      %.12f,%.12f,4000\n", lon_LR, lat_LR);
  fprintf(kml_file, "      %.12f,%.12f,4000\n", lon_UR, lat_UR);
  fprintf(kml_file, "      %.12f,%.12f,4000\n", lon_UL, lat_UL);
  
  fprintf(kml_file, "    </coordinates>\n");
  fprintf(kml_file, "  </LineString>\n");
  fprintf(kml_file, "</Placemark>\n");
  
  fprintf(kml_file, "<GroundOverlay>\n");
  fprintf(kml_file, "  <name>%s</name>\n", name);
  fprintf(kml_file, "  <LookAt>\n");
  fprintf(kml_file, "    <longitude>%.10f</longitude>\n", 
	  meta->general->center_longitude);
  fprintf(kml_file, "    <latitude>%.10f</latitude>\n", 
	  meta->general->center_latitude);
  fprintf(kml_file, "    <range>400000</range>\n");
  fprintf(kml_file, "    <tilt>45</tilt>\n");
  fprintf(kml_file, "    <heading>50</heading>\n");
  fprintf(kml_file, "  </LookAt>\n");
  fprintf(kml_file, "  <color>9effffff</color>\n");
  fprintf(kml_file, "  <Icon>\n");
  fprintf(kml_file, "      <href>%s</href>\n", output_image);
  fprintf(kml_file, "  </Icon>\n");
  fprintf(kml_file, "  <LatLonBox>\n");
  fprintf(kml_file, "      <north>%.12f</north>\n", max_lat);
  fprintf(kml_file, "      <south>%.12f</south>\n", min_lat);
  fprintf(kml_file, "      <east>%.12f</east>\n", max_lon);
  fprintf(kml_file, "      <west>%.12f</west>\n", min_lon);
  fprintf(kml_file, "      <rotation>%.12f</rotation>\n", -ang*R2D);
  fprintf(kml_file, "  </LatLonBox>\n");
  fprintf(kml_file, "</GroundOverlay>\n");
  
}

void kml_entry_with_overlay(FILE *kml_file, meta_parameters *meta, char *name,
                            char *png_filename, char *dir)
{
   kml_entry_impl(kml_file, meta, name, png_filename, dir);
}

void kml_entry(FILE *kml_file, meta_parameters *meta, char *name)
{
  kml_entry_impl(kml_file, meta, name, NULL, NULL);
}

void kml_footer(FILE *kml_file)
{
    fprintf(kml_file, "</Document>\n");
    fprintf(kml_file, "</kml>\n");
}

void write_kml_overlay(char *filename)
{
  char *kml_filename = appendExt(filename, ".kml");
  char *basename = get_basename(filename);
  
  FILE *kml_file = FOPEN(kml_filename, "w");
  
  kml_header(kml_file);
  kml_entry_overlay(kml_file, basename);
  kml_footer(kml_file);
  
  FCLOSE(kml_file);
  FREE(basename);
  FREE(kml_filename);

}

void convert2kml(char *line, FILE *fp, char *name, format_type_t format)
{
  switch (format)
    {
    case META:
      meta2kml(line, fp);
      break;
    case POINT:
      point2kml(line, fp);
      break;
    case POLYGON:
      polygon2kml(line, fp, name);
      break;
    case RGPS:
      rgps2kml(line, fp, name);
      break;
    case TEXT:
    case URSA:
    case KMLFILE:
    case SHAPEFILE:
      break;
    }

  return;
}

void write_kml(char *inFile, char *basename, format_type_t format, int list)
{
  FILE *fpIn, *fpOut;
  char *line, *outFile;
  int n=0;

  // Write kml header
  outFile = (char *) MALLOC(sizeof(char)*255);
  sprintf(outFile, "%s.kml", basename);
  fpOut = FOPEN(outFile, "w");
  kml_header(fpOut);

  // Convert to kml
  if (list) {
    line = (char *) MALLOC(sizeof(char)*1024);
    fpIn = FOPEN(inFile, "r");
    while (fgets(line, 1024, fpIn)) {
      // strip whitespace from the end of the line
      while (isspace(line[strlen(line)-1])) line[strlen(line)-1] = '\0';

      convert2kml(line, fpOut, basename, format);
      n++;
    }
    FCLOSE(fpIn);
    FREE(line);
  }
  else
    convert2kml(inFile, fpOut, basename, format);
  
  // Close business
  kml_footer(fpOut);
  FCLOSE(fpOut);
  FREE(outFile);

  return;
}

void write_kml_style_keys(FILE *kml_file)
{
    // so all of our methods use the same "look" for the
    // boxes/lines.
    fprintf(kml_file, "  <Style>\n");
    fprintf(kml_file, "    <LineStyle>\n");
    fprintf(kml_file, "      <color>ffff9900</color>\n");
    fprintf(kml_file, "      <width>3</width>\n");
    fprintf(kml_file, "    </LineStyle>\n");
    fprintf(kml_file, "    <PolyStyle>\n");
    fprintf(kml_file, "      <color>1fff5500</color>\n");
    fprintf(kml_file, "    </PolyStyle>\n");
    fprintf(kml_file, "  </Style>\n");
}
