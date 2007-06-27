#include "asf.h"
#include "asf_meta.h"
#include "asf_import.h"

#include <string.h>
#include <stdio.h>
#include <ctype.h>

static int matches(const char *line, const char *key)
{
    return strncmp_case(line, key, strlen(key)) == 0;
}

static void get_value(const char *line, char *value)
{
    // scan ahead to the first whitespace, then eat up the whitespace
    const char *p = line;
    while (!isspace(*p) && *p!='\0') ++p;
    while (isspace(*p) && *p!='\0') ++p;

    strcpy(value, p);

    // strip whitespace at the end of the value
    while (isspace(value[strlen(value)-1]))
        value[strlen(value)-1]='\0';
}

void import_bil(char *inBaseName, char *outBaseName)
{
    int i,j;

    // all four of these must be present
    char *blw_file = appendExt(inBaseName, ".blw");
    char *hdr_file = appendExt(inBaseName, ".hdr");
    char *prj_file = appendExt(inBaseName, ".prj");
    char *bil_file = appendExt(inBaseName, ".bil");

    if (!fileExists(blw_file) || !fileExists(hdr_file) || !fileExists(bil_file))
    {
        asfPrintError(
            "The BIL and/or associated metadata files were not found:\n"
            "  BIL File: %s %s\n"
            "  BLW File: %s %s\n"
            "  PRJ File: %s %s\n"
            "  HDR File: %s %s\n",
          bil_file, fileExists(bil_file) ? "Found" : "NOT FOUND",
          blw_file, fileExists(blw_file) ? "Found" : "NOT FOUND",
          prj_file, fileExists(prj_file) ? "Found" : "NOT FOUND",
          hdr_file, fileExists(hdr_file) ? "Found" : "NOT FOUND");
    }
    
    asfPrintStatus("Parsing HDR file...\n");

    // Info we'll get from the header file
    int nrows=-1;
    int ncols=-1;

    // Read header file
    char line[1024], value[1024];
    FILE *fp = FOPEN(hdr_file, "r");
    while (NULL!=fgets(line, 1024, fp)) {
        if (matches(line, "BYTEORDER")) {
            get_value(line, value);
            if (strcmp(value, "I") != 0)
                asfPrintError("Unsupported byte order (should be 'I'): %s\n",
                    value);
        } else if (matches(line, "LAYOUT")) {
            get_value(line, value);
            if (strcmp(value, "BIL") != 0)
                asfPrintError("Unsupported layout (should be BIL): %s\n",
                    value);
        } else if (matches(line, "NROWS")) {
            get_value(line, value);
            nrows = atoi(value);
        } else if (matches(line, "NCOLS")) {
            get_value(line, value);
            ncols = atoi(value);
        } else if (matches(line, "NBANDS")) {
            get_value(line, value);
            if (strcmp(value, "1") != 0)
                asfPrintError("Unsupported number of bands (should be 1): %s\n",
                    value);
        } else if (matches(line, "NBITS")) {
            get_value(line, value);
            if (strcmp(value, "16") != 0)
                asfPrintError("Unsupported number of bands (should be 16): %s\n",
                    value);
        }
    }
    fclose(fp);

    if (nrows < 0 || ncols < 0) {
        asfPrintError(
            "Header file did not contain Row/Column infomation.\n"
            "It is a valid .HDR file?\n");
    }

    // items we expect in the BLW file
    asfPrintStatus("Parsing BLW file...\n");

    double cell_size_horiz = 0;
    double cell_size_vert = 0;
    double lat_ul = -999;
    double lon_ul = -999;

    // BLW file is weirder... items are not labelled.  But, here is
    // the order of what's in there:
    //   Line 1: Cell size (horizontal)
    //   Line 2: <ignore>
    //   Line 3: <ignore>
    //   Line 4: Cell size (vertical)
    //   Line 5: Upper left latitude (degrees)
    //   Line 6: Upper left longitude (degrees)
    fp = FOPEN(blw_file, "r");
    fscanf(fp, "%lf\n", &cell_size_horiz); // Line 1
    fgets(line, 1024, fp);                //      2
    fgets(line, 1024, fp);                //      3
    fscanf(fp, "%lf\n", &cell_size_vert);  //      4
    fscanf(fp, "%lf\n", &lat_ul);          //      5
    fscanf(fp, "%lf\n", &lon_ul);          //      6
    fclose(fp);

    // PRJ File
    asfPrintStatus("Parsing PRJ file...\n");

    datum_type_t datum;
    spheroid_type_t spheroid;

    fp = FOPEN(prj_file, "r");
    while (NULL!=fgets(line, 1024, fp)) {
        if (matches(line, "Projection")) {
            get_value(line, value);
            if (strcmp(value, "GEOGRAPHIC") != 0)
                asfPrintError("Unsupported byte order (should be GEOGRAPHIC): %s\n",
                    value);
        } else if (matches(line, "Units")) {
            get_value(line, value);
            if (strcmp(value, "DD") != 0)
                asfPrintError("Unsupported Units (should be DD): %s\n",
                    value);
        } else if (matches(line, "Zunits")) {
            get_value(line, value);
            if (strcmp(value, "METERS") != 0)
                asfPrintError("Unsupported Zunits (should be METERS): %s\n",
                    value);
        } else if (matches(line, "Datum")) {
            get_value(line, value);
            if (strcmp_case(value, "NAD27") == 0)
                datum = NAD27_DATUM;
            else if (strcmp_case(value, "NAD83") == 0)
                datum = NAD83_DATUM;
            else if (strcmp_case(value, "WGS83") == 0)
                datum = WGS84_DATUM;
            else
                asfPrintError(
                    "Unsupported Datum (should be NAD27, NAD83, or WGS84): %s\n",
                    value);
        } else if (matches(line, "Spheroid")) {
            get_value(line, value);
            if (strcmp_case(value, "CLARKE1866") == 0)
                spheroid = CLARKE1866_SPHEROID;
            else if (strcmp_case(value, "BESSEL") == 0)
                spheroid = BESSEL_SPHEROID;
            else if (strcmp_case(value, "CLARKE1880") == 0)
                spheroid = CLARKE1880_SPHEROID;
            else if (strcmp_case(value, "GEM6") == 0)
                spheroid = GEM6_SPHEROID;
            else if (strcmp_case(value, "GEM10C") == 0)
                spheroid = GEM10C_SPHEROID;
            else if (strcmp_case(value, "GRS1980") == 0)
                spheroid = GRS1980_SPHEROID;
            else if (strcmp_case(value, "WGS72") == 0)
                spheroid = WGS72_SPHEROID;
            else if (strcmp_case(value, "WGS84") == 0)
                spheroid = WGS84_SPHEROID;
            else if (strcmp_case(value, "INTERNATIONAL1924") == 0)
                spheroid = INTERNATIONAL1924_SPHEROID;
            else if (strcmp_case(value, "INTERNATIONAL1967") == 0)
                spheroid = INTERNATIONAL1967_SPHEROID;
            else
                asfPrintError("Unsupported Spheroid: %s\n", value);
        }
    }
    fclose(fp);

    // create the metadata
    char *meta_filename = appendExt(outBaseName, ".meta");
    meta_parameters *meta = raw_init();

    meta->projection = meta_projection_init();
    meta->location = meta_location_init();

    // aliases
    meta_general *mg = meta->general;
    meta_projection *mp = meta->projection;
    meta_location *ml = meta->location;

    // populate general block info
    mg->data_type = INTEGER16;
    mg->image_data_type = DEM; //presumably!?

    mg->x_pixel_size = cell_size_horiz;
    mg->y_pixel_size = cell_size_vert;

    mg->line_count = nrows;
    mg->sample_count = ncols;
    mg->band_count = 1;
    mg->start_line = 0;
    mg->start_sample = 0;

    char *basename = get_basename(inBaseName);
    strcpy(mg->basename, basename);
    free(basename);

    strcpy(mg->system, "big_ieee");
    strcpy(mg->sensor, "USGS Seamless data (e.g., NED, STRM)");
    strcpy(mg->mode, "N/A");
    strcpy(mg->processor, "Unknown");

    mg->center_latitude =
        lat_ul + cell_size_horiz * ncols / 2.0;
    mg->center_longitude =
        lat_ul + cell_size_horiz * ncols / 2.0;

    // populate projection block info

    mp->type = LAT_LONG_PSEUDO_PROJECTION;
    mp->startX = lon_ul;
    mp->startY = lat_ul;
    mp->perX = mg->x_pixel_size;
    mp->perY = -mg->y_pixel_size;
    strcpy(mp->units, "degrees");
    mp->hem = mg->center_latitude > 0 ? 'N' : 'S';

    // the datum for NED is NAD83, for SRTM it is WGS84
    mp->datum = datum;
    mp->spheroid = spheroid;

    spheroid_axes_lengths(spheroid,
        &mg->re_major, &mg->re_minor);

    mp->re_major = mg->re_major;
    mp->re_minor = mg->re_minor;

    // location block
    ml->lat_start_near_range = mp->startX;
    ml->lon_start_near_range = mp->startY;
    ml->lat_start_far_range = mp->startX + mp->perX * ncols;
    ml->lon_start_far_range = mp->startY;
    ml->lat_end_near_range = mp->startX;
    ml->lon_end_near_range = mp->startY + mp->perY * nrows;
    ml->lat_end_far_range = mp->startX + mp->perX * ncols;
    ml->lon_end_far_range = mp->startY + mp->perY * nrows;

    meta_write(meta, meta_filename);

    // Now read/write the actual data.  Read as INT16, cast to
    // floats so we can use put_float_line (which will write
    // INT16 data, since we asked for it in the metadata)
    char *data_filename = appendExt(outBaseName, ".img");
    unsigned short *shorts = MALLOC(sizeof(unsigned short)*ncols);
    float *floats = MALLOC(sizeof(float)*ncols);

    fp = FOPEN(bil_file, "rb");
    FILE *out = FOPEN(data_filename, "wb");
    for (i=0; i<nrows; ++i) {
        FREAD(shorts, sizeof(unsigned short), ncols, fp);
        for (j=0; j<ncols; ++j)
            floats[j] = (float)shorts[j];
        put_float_line(out, meta, i, floats);
    }

    fclose(fp);
    fclose(out);

    free(shorts);
    free(data_filename);
    free(floats);

    free(blw_file);
    free(hdr_file);
    free(prj_file);
    free(bil_file);
}
