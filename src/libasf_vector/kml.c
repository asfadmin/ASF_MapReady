#include "asf_vector.h"
#include "asf.h"
#include <stdio.h>

/* 
   When invoking google earth from the command line, you can put a kml
   file as a command line argument, but in that case it wants the kml
   files' location to be fully specified, e.g.:
    googleearth /export/home/user/files/the/dir/here/the_file.kml
*/

void kml_header(FILE *kml_file)
{
    fprintf(kml_file, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    fprintf(kml_file,
            "<kml xmlns=\"http://earth.google.com/kml/2.0\">\n");
    fprintf(kml_file, "<Document>\n");
}

void kml_entry(FILE *kml_file, meta_parameters *meta, char *name)
{
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;
    double lat, lon;

    fprintf(kml_file, "<Placemark>\n");
    fprintf(kml_file, "  <description>\n");
    fprintf(kml_file, "    sensor/mode: %s/%s\n",
            meta->general->sensor, meta->general->mode);
    fprintf(kml_file, "    orbit/frame: %d/%d\n",
            meta->general->orbit, meta->general->frame);
    fprintf(kml_file, "  </description>\n");
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
    
    meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
    fprintf(kml_file, "      %.12f,%.12f,4000\n", lon, lat);
    meta_get_latLon(meta, nl, 0, 0, &lat, &lon);
    fprintf(kml_file, "      %.12f,%.12f,4000\n", lon, lat);
    meta_get_latLon(meta, nl, ns, 0, &lat, &lon);
    fprintf(kml_file, "      %.12f,%.12f,4000\n", lon, lat);
    meta_get_latLon(meta, 0, ns, 0, &lat, &lon);
    fprintf(kml_file, "      %.12f,%.12f,4000\n", lon, lat);
    meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
    fprintf(kml_file, "      %.12f,%.12f,4000\n", lon, lat);
    
    fprintf(kml_file, "    </coordinates>\n");
    fprintf(kml_file, "  </LineString>\n");
    fprintf(kml_file, "</Placemark>\n");
}

void kml_footer(FILE *kml_file)
{
    fprintf(kml_file, "</Document>\n");
    fprintf(kml_file, "</kml>\n");
}

void write_kml(char *filename)
{
    meta2kml(filename, NULL);
}

void meta2kml(char *filename, meta_parameters *meta)
{
    if (!meta)
        meta = meta_read(filename);

    char *basename = get_basename(filename);
    FILE *kml_file = FOPEN(filename, "wt");
    kml_header(kml_file);
    kml_entry(kml_file, meta, basename);
    kml_footer(kml_file);
    FREE(basename);
}
