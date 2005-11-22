
#include <glib.h>

#include <asf.h>
#include <libasf_proj.h>

#include "gridFloat.h"
#include "parse_options.h"
#include "geocode_options.h"
#include "asf_import_dem.h"

static double VERSION=0.1;

void usage (char *name);
void help_page(void);

int main(int argc, char **argv)
{
  char inDataName[256];
  char outDataName[256], outMetaName[256];

  // Get the projection parameters from the command line.
  projection_type_t projection_type;
  // Terrain height to assume.  Defaults to 0.
  double average_height;
  // Pixel size to use for output image, in projection coordinate units.
  // This variable corresponds to a "private"
  // (i.e. undocumented, so users don't fiddle with it) option.
  double pixel_size;
  // Datum to use in the target projection (default to wgs84)
  datum_type_t datum = WGS84_DATUM;
  // Method to use to resample images.
  resample_method_t resample_method;

  // Detect & Process logging arguments
  if ((logflag = detect_string_options(argc, argv, logFile,
                                      "-log", "--log", NULL))) {
      fLog = fopen (logFile, "w");
      if ( fLog == NULL ) {
        // Couldn't open the log file, so just don't do logging.
        logflag = FALSE;
      }
  }
  quietflag = detect_flag_options(argc, argv, "-quiet", "--quiet", NULL);

  // If help was requested, display it.
  if (detect_flag_options(argc, argv, "-help", "--help", NULL)) {
    help_page ();
  }

  project_parameters_t *outProjPrms
    = get_geocode_options (&argc, &argv, &projection_type, &average_height,
                           &pixel_size, &datum, &resample_method);
  if (outProjPrms == NULL) {
    asfPrintError("Failure getting projection parameters!\n");
  }

  if ( argc != 3 ) {
    usage (argv[0]);
  }

  // Input name
  create_name(inDataName,argv[1],".flt");

  // Output Names
  create_name(outDataName,argv[2],".img");
  create_name(outMetaName,argv[2],".meta");

  // Assign our transformation function pointers to point to the
  // appropriate functions.
  PROJECT_T     project     = NULL;
  PROJECT_ARR_T project_arr = NULL;
  UNPROJECT_T   unproject   = NULL;
  switch ( projection_type ) {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
    project = project_utm;
    project_arr = project_utm_arr;
    unproject = project_utm_inv;
    break;
  case POLAR_STEREOGRAPHIC:
    project = project_ps;
    project_arr = project_ps_arr;
    unproject = project_ps_inv;
    break;
  case ALBERS_EQUAL_AREA:
    project = project_albers;
    project_arr = project_albers_arr;
    unproject = project_albers_inv;
    break;
  case LAMBERT_CONFORMAL_CONIC:
    project = project_lamcc;
    project_arr = project_lamcc_arr;
    unproject = project_lamcc_inv;
    break;
  case LAMBERT_AZIMUTHAL_EQUAL_AREA:
    project = project_lamaz;
    project_arr = project_lamaz_arr;
    unproject = project_lamaz_inv;
    break;
  default:
    g_assert_not_reached ();
    break;
  }

  // Load the metadata
  seamless_meta_t *smeta = gridFloat_metadata_to_seamless_meta(argv[1]);

  // Fill out the input meta using what we've gotten so far
  fill_proj_meta(projection_type, outProjPrms, smeta, project, &average_height,
                 &pixel_size);
  // Convert all angle measures in the project_parameters to radians.
  to_radians (projection_type, outProjPrms);

  // Set Projection Datum & Average Height
  datum = WGS84_DATUM;
  project_set_datum (datum);
  project_set_avg_height (average_height);

  double min_x=DBL_MAX, max_x=DBL_MIN, min_y=DBL_MAX, max_y=DBL_MIN;
  find_extents (smeta, outProjPrms, project_arr,
                &min_x, &max_x, &min_y, &max_y);

  const int grid_size = 131;
  const int sparse_grid_stride = 2;
  data_to_fit_t *dtf = data_to_fit_new (grid_size, sparse_grid_stride);
  create_grid (smeta, outProjPrms, unproject, grid_size, sparse_grid_stride,
               min_x, max_x, min_y, max_y, dtf);

  check_splines (dtf, grid_size);

  write_asf_dem(smeta, dtf, pixel_size, outProjPrms, projection_type,
                resample_method, inDataName, outDataName, outMetaName,
                min_x, max_x, min_y, max_y);

  exit(EXIT_SUCCESS);
}

void usage (char *name)
{
  printf("USAGE: asf_import_dem <inBaseName> <outBaseName>\n");
  printf("ASF SAR Tools version %.2f\n", VERSION);
  exit(EXIT_FAILURE);
}

void help_page(void)
{
  usage("asf_import_dem");
}
