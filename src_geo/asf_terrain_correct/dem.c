#include "dem.h"
#include "asf_meta.h"
#include "float_image.h"

#include <string.h>
#include <glib.h>
#include <gsl/gsl_math.h>

#ifndef linux
#ifndef win32
static double
round (double arg)
{
  return floor (arg + 0.5);
}
#endif // #ifndef win32
#endif // #ifndef linux

/* Forward declarations for static methods */

static void 
parse_gridfloat_header(char * headername, ssize_t * ncols, ssize_t * nrows,
		       double * xllcorner, double * yllcorner, 
		       double * cellsize, double * nodata_value,
		       float_image_byte_order_t * byte_order);

static const char * 
get_extension (const char *file);

static Dem *
dem_new_from_file_gridfloat(const char * file);

static Dem *
dem_new();

/* Public Methods */

Dem * 
dem_new_from_file(const char *file)
{
    // assure the file is there... better way to handle errors?
    if (!g_file_test (file, G_FILE_TEST_EXISTS))
    {
	g_error("DEM file not found: %s\n", file);
	return NULL;
    }

    // the extension tells us what type of data we are dealing with
    const char * ext = get_extension (file);

    if (!ext)
    {
	g_error("DEM file lacks extension, unable to determine type.\n");
	return NULL;
    }

    if (strcmp(ext, "flt") == 0)
    {
	// GRIDFLOAT
	return dem_new_from_file_gridfloat (file);
    }
    else
    {
	// extension not recognized
	g_error("DEM file has unrecognized format, unknown extension: %s\n",
		ext);
	return NULL;
    }
}

double
dem_get_height(Dem * dem, double latitude, double longitude)
{
    if (!dem_contains_coordinate(dem, latitude, longitude))
    {
	// should be error ?
	return dem->nodata_value;
    }

    ssize_t x = floor ((longitude - dem->xllcorner) / dem->cellsize);

    ssize_t y = dem->nrows - 
	(1 + floor((latitude - dem->yllcorner) / dem->cellsize));

    return 3.281 * (double) float_image_get_pixel (dem->float_image, x, y);
}

int
dem_contains_coordinate(Dem * dem, double latitude, double longitude)
{
    g_assert (longitude > -180);
    g_assert (longitude < 180);
    g_assert (latitude > -90);
    g_assert (latitude < 90);
    
    double xurcorner = dem->xllcorner + dem->cellsize * (dem->ncols + 1);
    double yurcorner = dem->yllcorner + dem->cellsize * (dem->nrows + 1);

    return
	latitude >= dem->yllcorner &&
	latitude <= yurcorner &&
	longitude >= dem->xllcorner &&
	longitude <= xurcorner;
}

void 
dem_free(Dem * dem)
{
    if (dem)
    {
	if (dem->float_image)
	    float_image_free(dem->float_image);

	g_free (dem);
    }
    
}

/* Private Methods */

static const char * 
get_extension (const char *file)
{
    char * p = strrchr(file, '.');
    if (p)
	++p;

    return p;
}

static char *
replace_extension(const char *filename, const char *old_extension,
		  const char *new_extension)
{
    // assume given file has the required extension
    g_assert( strcmp(get_extension(filename), old_extension) == 0 );

    char * new_name =
	g_malloc(sizeof(char) * (1 + strlen(filename) + 
				 strlen(new_extension)));

    strcpy(new_name, filename);
    
    // wipe out old extension, add new
    char * p = strrchr (new_name, '.');
    g_assert (p);

    ++p;
    *p = '\0';

    strcat(new_name, new_extension);

    return new_name;
}

static Dem *
dem_new()
{
    Dem * ret = g_new (Dem, 1);

    ret->float_image = NULL;
    ret->ncols = 0;
    ret->nrows = 0;
    ret->xllcorner = 0.0;
    ret->yllcorner = 0.0;
    ret->cellsize = 0.0;
    ret->nodata_value = -9999;

    return ret;
}

static Dem *
dem_new_from_file_gridfloat(const char * file)
{
    Dem * ret = dem_new();

    // Build the header's file name based on the data file's name
    char * header_file = replace_extension (file, "flt", "hdr");

    if (!g_file_test (header_file, G_FILE_TEST_EXISTS))
    {
	g_error("DEM header file for '%s' not found: %s\n", file, header_file);
	return NULL;
    }

    parse_gridfloat_header(header_file, &ret->ncols, &ret->nrows,
			   &ret->xllcorner, &ret->yllcorner,
			   &ret->cellsize, &ret->nodata_value,
			   &ret->byte_order);

    g_assert (ret->ncols > 0);
    g_assert (ret->nrows > 0);

    g_assert (ret->xllcorner > -180);
    g_assert (ret->xllcorner < 180);
    g_assert (ret->yllcorner > -90);
    g_assert (ret->yllcorner < 90);

    g_assert (ret->cellsize > 0.0);

    ret->float_image = float_image_new_from_file(ret->ncols, ret->nrows,
						 file, 0, ret->byte_order);

    g_assert (ret->float_image);

    return ret;
}

static void 
parse_gridfloat_header(char * headername, ssize_t * ncols, ssize_t * nrows,
		       double * xllcorner, double * yllcorner, 
		       double * cellsize, double * nodata_value,
		       float_image_byte_order_t * byte_order)
{
    FILE * fp = fopen(headername, "rt");
    if (fp)
    {
	char c_byte_order[256];

	fscanf(fp, "ncols %d\n", ncols);
	fscanf(fp, "nrows %d\n", nrows);
	fscanf(fp, "xllcorner %lf\n", xllcorner);
	fscanf(fp, "yllcorner %lf\n", yllcorner);
	fscanf(fp, "cellsize %lf\n", cellsize);
	fscanf(fp, "NODATA_value %lf\n", nodata_value);

	fscanf(fp, "byteorder %s\n", c_byte_order);
	if (strcmp(c_byte_order, "LSBFIRST") == 0)
	    *byte_order = FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN;
	else
	    *byte_order = FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;
    }
    else
    {
	g_error ("Couldn't open DEM header file: %s\n", headername);
    }

    fclose(fp);
    return;
}

