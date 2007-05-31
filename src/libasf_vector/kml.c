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

static int
make_input_image_thumbnail (meta_parameters *imd, const char *input_data,
                            size_t max_thumbnail_dimension,
                            const char *output_jpeg)
{
    /* Make a copy of one of the arguments so the compilers doesn't
    complain about us ignoring the const qualifier when we pass it fo
    fopenCeos().  */

    if (imd->general->data_type != BYTE &&
        imd->general->data_type != INTEGER16 &&
        imd->general->data_type != INTEGER32 &&
        imd->general->data_type != REAL32 &&
        imd->general->data_type != REAL64)
    {
        /* don't know how to make a thumbnail for this type ... */
        return FALSE;
    }

    CEOS_FILE *id = fopenCeos ((char*)input_data); // Input data file.

    if (!id->f_in) {
        // failed for some reason, just quit without thumbnailing
        return FALSE;
    }

    // Vertical and horizontal scale factors required to meet the
    // max_thumbnail_dimension part of the interface contract.
    int vsf = ceil (id->ddr.nl / max_thumbnail_dimension);
    int hsf = ceil (id->ddr.ns / max_thumbnail_dimension);
    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : vsf);

    // Thumbnail image sizes.
    size_t tsx = id->ddr.ns / sf;
    size_t tsy = id->ddr.nl / sf;

    // Thumbnail image.
    FloatImage *ti = float_image_new (tsx, tsy);

    // Form the thumbnail image by grabbing individual pixels.  FIXME:
    // Might be better to do some averaging or interpolating.
    size_t ii;
    int *line = MALLOC (sizeof(int) * id->ddr.ns);
    for ( ii = 0 ; ii < tsy ; ii++ ) {
        readCeosLine (line, ii * sf, id);
        size_t jj;
        for ( jj = 0 ; jj < tsx ; jj++ ) {
            // Current sampled value.  We will average a couple pixels together.
            int csv;
            if ( jj * sf < id->ddr.nl - 1 ) {
                csv = (line[jj * sf] + line[jj * sf + 1]) / 2;
            }
            else {
                csv = (line[jj * sf] + line[jj * sf - 1]) / 2;
            }
            float_image_set_pixel (ti, jj, ii, (float)csv);
        }
    }
    free (line);

    // Export as jpeg image as promised.  We don't want to reduce the
    // image resolution anymore, so we use the largest dimension
    // currently in the image as the max dimension for the image to
    // generate.
    float_image_export_as_jpeg (ti, output_jpeg, (ti->size_x > ti->size_y ?
            ti->size_x : ti->size_y), NAN);

    float_image_free (ti);
    FCLOSE(id->f_in);
    free(id);

    return TRUE;
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
                           char *name, char *ceos_filename, char *dir)
{
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;
    double lat_UL, lon_UL;
    double lat_UR, lon_UR;
    double lat_LL, lon_LL;
    double lat_LR, lon_LR;
    double max_lat = -90, max_lon = -180, min_lat = 90, min_lon = 180;

    if (meta->location) {
      lat_UL = meta->location->lat_start_near_range;
      lon_UL = meta->location->lon_start_near_range;
      lat_UR = meta->location->lat_start_far_range;
      lon_UR = meta->location->lon_start_far_range;
      lat_LL = meta->location->lat_end_near_range;
      lon_LL = meta->location->lon_end_near_range;
      lat_LR = meta->location->lat_end_far_range;
      lon_LR = meta->location->lon_end_far_range;
    }
    else {
      meta_get_latLon(meta, 0, 0, 0, &lat_UL, &lon_UL);
      lat_UL *= R2D;
      lon_UL *= R2D;
      meta_get_latLon(meta, nl, 0, 0, &lat_LL, &lon_LL);
      lat_LL *= R2D;
      lon_LL *= R2D;
      meta_get_latLon(meta, nl, ns, 0, &lat_LR, &lon_LR);
      lat_LR *= R2D;
      lon_LR *= R2D;
      meta_get_latLon(meta, 0, ns, 0, &lat_UR, &lon_UR);
      lat_UR *= R2D;
      lon_UR *= R2D;
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
    fprintf(kml_file, "  <Style>\n");
    fprintf(kml_file, "    <LineStyle>\n");
    fprintf(kml_file, "      <color>ffff9900</color>\n");
    fprintf(kml_file, "      <width>3</width>\n");
    fprintf(kml_file, "    </LineStyle>\n");
    fprintf(kml_file, "    <PolyStyle>\n");
    fprintf(kml_file, "      <color>1fff5500</color>\n");
    fprintf(kml_file, "    </PolyStyle>\n");
    fprintf(kml_file, "  </Style>\n");
    fprintf(kml_file, "  <Polygon>\n");
	fprintf(kml_file, "    <extrude>1</extrude>\n");
	fprintf(kml_file, "    <altitudeMode>absolute</altitudeMode>\n");
    fprintf(kml_file, "    <outerBoundaryIs>\n");
    fprintf(kml_file, "      <LinearRing>\n");
    fprintf(kml_file, "        <coordinates>\n");
    
    update_latlon_maxes(lat_UL, lon_UL, &max_lat, &min_lat, &max_lon, &min_lon);
    update_latlon_maxes(lat_LL, lon_LL, &max_lat, &min_lat, &max_lon, &min_lon);
    update_latlon_maxes(lat_LR, lon_LR, &max_lat, &min_lat, &max_lon, &min_lon);
    update_latlon_maxes(lat_UR, lon_UR, &max_lat, &min_lat, &max_lon, &min_lon);

    fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UL, lat_UL);
    fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_LL, lat_LL);
    fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_LR, lat_LR);
    fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UR, lat_UR);
    fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UL, lat_UL);
    
    fprintf(kml_file, "        </coordinates>\n");
    fprintf(kml_file, "      </LinearRing>\n");
    fprintf(kml_file, "    </outerBoundaryIs>\n");
    fprintf(kml_file, "  </Polygon>\n");
    fprintf(kml_file, "</Placemark>\n");

    if (ceos_filename)
    {
        printf("ceos filename: %s\n", ceos_filename);

        if (!dir)
            asfPrintError("Must pass in a directory for the overlay files!\n");

        char *jpeg_basename = get_basename(ceos_filename);
        char *output_jpeg = MALLOC(sizeof(char)*(strlen(jpeg_basename)+
						strlen(dir)+10));
        sprintf(output_jpeg, "%s%s.jpg", dir, jpeg_basename);
        if (make_input_image_thumbnail(meta, ceos_filename, 512, output_jpeg))
        {
            double h = 0.0;

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

            double ang1 = atan2(ul_y-ur_y, ul_x-ur_x);
            double ang2 = atan2(ll_y-lr_y, ll_x-lr_x);
            double ang = -(ang1+ang2)/2;

            printf("UL: %lf,%lf\n", ul_x,ul_y);
            printf("UR: %lf,%lf\n", ur_x,ur_y);
            printf("LL: %lf,%lf\n", ll_x,ll_y);
            printf("LR: %lf,%lf\n", lr_x,lr_y);
            printf("angle= %lf %lf %lf\n", ang1*R2D, ang2*R2D,
                   ang*R2D);

            rotate(ul_x, ul_y, ctr_x, ctr_y, ang, &ul_x_rot, &ul_y_rot);
            rotate(ur_x, ur_y, ctr_x, ctr_y, ang, &ur_x_rot, &ur_y_rot);
            rotate(ll_x, ll_y, ctr_x, ctr_y, ang, &ll_x_rot, &ll_y_rot);
            rotate(lr_x, lr_y, ctr_x, ctr_y, ang, &lr_x_rot, &lr_y_rot);

            printf("Rotated UL: %lf,%lf\n", ul_x_rot,ul_y_rot);
            printf("Rotated UR: %lf,%lf\n", ur_x_rot,ur_y_rot);
            printf("Rotated LL: %lf,%lf\n", ll_x_rot,ll_y_rot);
            printf("Rotated LR: %lf,%lf\n", lr_x_rot,lr_y_rot);

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
            if (box_east_lon > box_west_lon)
                swap(&box_east_lon, &box_west_lon);

            if (meta->general->orbit_direction == 'D')
                swap(&box_south_lat, &box_north_lat);

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
            fprintf(kml_file, "  <color>9effffff</color>\n");
            fprintf(kml_file, "  <Icon>\n");
            fprintf(kml_file, "      <href>%s</href>\n", output_jpeg);
            fprintf(kml_file, "  </Icon>\n");
            fprintf(kml_file, "  <LatLonBox>\n");
            fprintf(kml_file, "      <north>%.12f</north>\n", box_north_lat);
            fprintf(kml_file, "      <south>%.12f</south>\n", box_south_lat);
            fprintf(kml_file, "      <east>%.12f</east>\n", box_east_lon);
            fprintf(kml_file, "      <west>%.12f</west>\n", box_west_lon);
            fprintf(kml_file, "      <rotation>%.12f</rotation>\n", -ang*R2D);
            fprintf(kml_file, "  </LatLonBox>\n");

            fprintf(kml_file, "</GroundOverlay>\n");
        } else {
            printf("Failed to generate overlay for: %s\n", ceos_filename);
        }

        free(output_jpeg);
        free(jpeg_basename);
    }

}

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
  create_name(output_image, name, ".jpg");
  image = float_image_new_from_file(ns, nl, input_image, 0, 
				    FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  float_image_export_as_jpeg(image, output_image, MAX(nl, ns), NAN);
  
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
  fprintf(kml_file, "  <Style>\n");
  fprintf(kml_file, "    <LineStyle>\n");
  fprintf(kml_file, "      <color>ff00ffff</color>\n");
  fprintf(kml_file, "    </LineStyle>\n");
  fprintf(kml_file, "    <PolyStyle>\n");
  fprintf(kml_file, "      <color>7f00ff00</color>\n");
  fprintf(kml_file, "    </PolyStyle>\n");
  fprintf(kml_file, "  </Style>\n");
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
                            char *ceos_filename, char *jpeg_dir)
{
   kml_entry_impl(kml_file, meta, name, ceos_filename, jpeg_dir);
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
  
  FILE *kml_file = FOPEN(kml_filename, "wt");
  
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
  fpOut = FOPEN(outFile, "wt");
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

