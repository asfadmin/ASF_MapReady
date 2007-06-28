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

void import_gridfloat(char *inBaseName, char *outBaseName)
{
    int i;

    // all three of these must be present
    char *hdr_file = appendExt(inBaseName, ".hdr");
    char *prj_file = appendExt(inBaseName, ".prj");
    char *flt_file = appendExt(inBaseName, ".flt");

    if (!fileExists(flt_file) || !fileExists(hdr_file) || !fileExists(prj_file))
    {
        asfPrintError(
            "The BIL and/or associated metadata files were not found:\n"
            "  FLT File: %s %s\n"
            "  PRJ File: %s %s\n"
            "  HDR File: %s %s\n",
          flt_file, fileExists(flt_file) ? "Found" : "NOT FOUND",
          prj_file, fileExists(prj_file) ? "Found" : "NOT FOUND",
          hdr_file, fileExists(hdr_file) ? "Found" : "NOT FOUND");
    }
    
    asfPrintStatus("Parsing %s ...\n", hdr_file);

    // Info we'll get from the header file
    int nrows=-1;
    int ncols=-1;
    double cell_size = 0;
    double lat_ll = -999;
    double lon_ll = -999;
    double nodata = -9999;

    // Read header file
    char line[1024], value[1024];
    FILE *fp = FOPEN(hdr_file, "r");
    while (NULL!=fgets(line, 1024, fp)) {
        if (matches(line, "ncols")) {
            get_value(line, value);
            ncols = atoi(value);
        } else if (matches(line, "nrows")) {
            get_value(line, value);
            nrows = atoi(value);
        } else if (matches(line, "xllcorner")) {
            get_value(line, value);
            lon_ll = atof(value);
        } else if (matches(line, "yllcorner")) {
            get_value(line, value);
            lat_ll = atof(value);
        } else if (matches(line, "cellsize")) {
            get_value(line, value);
            cell_size = atof(value);
        } else if (matches(line, "NODATA_value")) {
            get_value(line, value);
            nodata = atof(value);
        } else if (matches(line, "byteorder")) {
            get_value(line, value);
            if (strcmp(value, "LSBFIRST") != 0)
                asfPrintError("Unsupported byte order (should be 'LSBFIRST'): %s\n",
                    value);
        }
    }
    fclose(fp);

    if (nrows < 0 || ncols < 0) {
        asfPrintError(
            "Header file did not contain Row/Column infomation.\n"
            "It is a valid .HDR file?\n");
    }

    // PRJ File
    asfPrintStatus("Parsing %s ...\n", prj_file);

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

    asfPrintStatus("Building %s ...\n", meta_filename);

    meta_parameters *meta = raw_init();
    meta->projection = meta_projection_init();
    meta->location = meta_location_init();

    // aliases
    meta_general *mg = meta->general;
    meta_projection *mp = meta->projection;
    meta_location *ml = meta->location;

    // populate general block info
    mg->data_type = REAL32;
    mg->image_data_type = DEM; //presumably!?

    mg->no_data = nodata;

    // these are supposed to be in meters
    // currently, cell_size_* is in arcsecs... so here is a kludgey
    // calculation, to round to the nearest 10m. usgs data is either
    // 30, 60, or 90 meter.
    // Note that the projection block will have the actual pixel size
    int cell_size_m = 10 * (int)(11131.95 * cell_size + .5);
    if (cell_size_m != 30 && cell_size_m != 60 && cell_size_m != 90)
    {
        asfPrintWarning("Unexpected pixel size of %dm (%.10f degree) detected.\n"
            "USGS Seamless data should be 30, 60, or 90 meter.\n",
            cell_size_m, cell_size);
    }
    mg->x_pixel_size = cell_size_m;
    mg->y_pixel_size = cell_size_m;

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

    mg->center_latitude = lat_ll + cell_size * ncols / 2.0;
    mg->center_longitude = lon_ll + cell_size * nrows / 2.0;

    // populate projection block info

    mp->type = LAT_LONG_PSEUDO_PROJECTION;
    mp->startX = lon_ll;
    mp->startY = lat_ll + cell_size * nrows;
    mp->perX = cell_size;
    mp->perY = -cell_size;
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
    ml->lon_start_near_range = mp->startX;
    ml->lat_start_near_range = mp->startY;
    ml->lon_start_far_range = mp->startX + mp->perX * ncols;
    ml->lat_start_far_range = mp->startY;
    ml->lon_end_near_range = mp->startX;
    ml->lat_end_near_range = mp->startY + mp->perY * nrows;
    ml->lon_end_far_range = mp->startX + mp->perX * ncols;
    ml->lat_end_far_range = mp->startY + mp->perY * nrows;

    meta_write(meta, meta_filename);

    // Now read/write the actual data.  Read as INT16, cast to
    // floats so we can use put_float_line (which will write
    // INT16 data, since we asked for it in the metadata)
    char *data_filename = appendExt(outBaseName, ".img");

    asfPrintStatus("Reading %s, writing %s ...\n", flt_file, data_filename);

    float *floats = MALLOC(sizeof(float)*ncols);
    fp = FOPEN(flt_file, "rb");
    FILE *out = FOPEN(data_filename, "wb");

    for (i=0; i<nrows; ++i) {
        FREAD(floats, sizeof(float), ncols, fp);
        put_float_line(out, meta, i, floats);
        asfLineMeter(i,nrows);
    }

    fclose(fp);
    fclose(out);

    free(data_filename);
    free(floats);

    free(hdr_file);
    free(prj_file);
    free(flt_file);
}
