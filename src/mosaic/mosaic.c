#include <stdio.h>
#include <stdlib.h>

#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_nan.h"
#include "float_image.h"

#include "asf_contact.h"
#include "asf_license.h"
#include "asf_version.h"
#include "asf_geocode.h"

#define ASF_NAME_STRING "mosaic"

static const float_image_byte_order_t fibo_be =
    FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;

void help()
{
    printf(
"Tool name:\n"
"    %s\n\n"
"Usage:\n"
"     %s -p <projection name> <<projection parameters>>\n"\
"            [-force] [-resample-method <method>] [-height <height>]\n"\
"            [-datum <datum>] [-pixel-size <pixel size>] [-band <band_id | all>]\n"\
"            [-log <file>] [-write-proj-file <file>] [-read-proj-file <file>]\n"\
"            [-background <val>] [-quiet] [-license] [-version] [-help]\n"\
"            <outfile> <infile1> <infile2> ... \n\n"
"Description:\n"
"     This program mosaics the input files together, producing an output image\n"
"     that is the union of all listed input images.  Where the input images\n"
"     overlap, the pixel in the image listed first on the command-line is\n"
"     chosen.\n\n"
"Input:\n"
"     At least 2 input files are required.  You may list as many input files\n"
"     as desired, however extremely large output files will take some time to\n"
"     process.\n\n"
"Output:\n"
"     The output file, listed first, is created by the program, and, depending\n"
"     on the locations of the input files relative to each other, can range\n"
"     in size from equal to the largest input image, to much larger than the\n"
"     total size of all input images.\n\n"
"Options:\n"
"%s"
"\n"
"     -log <log file>\n"
"          Output will be written to a specified log file.\n"
"\n"
"     -quiet\n"
"          Supresses all non-essential output.\n"
"\n"
"     -help\n"
"          Print this help information and exit.\n"
"\n"
"     -license\n"
"          Print copyright and license for this software and exit.\n"
"\n"
"     -version\n"
"          Print version and copyright and exit.\n\n"
"Examples:\n"
"    %s -p utm --pixel-size 100 out in1 in2 in3 in4 in5 in6\n\n"
"Limitations:\n"
"     Theoretically, any size output image will work.  The output image will\n"
"     be cached on disk if it is too large to fit in memory, however in this\n"
"     situation the process will be quite slow.\n\n"
"See also:\n"
"     asf_geocode\n\n"
"Contact:\n"
"%s\n\n"
"Version:\n"
"   %s\n",
ASF_NAME_STRING, ASF_NAME_STRING, geocode_projection_options_help(),
ASF_NAME_STRING, ASF_CONTACT_STRING, CONVERT_PACKAGE_VERSION_STRING
);
    exit(1);
}

void usage()
{
    printf("Usage:\n"
           "    %s <outfile> <infile1> <infile2> ... \n\n"
           "At least 2 input files are required.\n", ASF_NAME_STRING);
    exit(1);
}

int main(int argc, char *argv[])
{
    handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
    if (argc>1 && (strcmp(argv[1], "-help")==0 || strcmp(argv[1],"--help")==0))
        help();
    if (argc<3) usage();

    projection_type_t projection_type;
    datum_type_t datum;
    double pixel_size, average_height, background_val;
    resample_method_t resample_method;
    int force_flag;
    char band_id[256]="";

    project_parameters_t *pp
      = get_geocode_options (&argc, &argv, &projection_type, &average_height,
      &pixel_size, &datum, &resample_method,
      &force_flag, band_id);

    if (!pp)
      usage();

    extract_double_options(&argc, &argv, &background_val, "--background",
                          "-background", NULL);
    if (ISNAN(background_val)) background_val = DEFAULT_NO_DATA_VALUE;

    char *outfile = argv[1];
    int i, n_inputs = argc - 2;

    asfSplashScreen(argc, argv);

    asfPrintStatus("Output file: %s\n", outfile);

    //char **infiles = &argv[2];
    //asfPrintStatus("Input files:\n");
    //for (i = 0; i < n_inputs; ++i)
    //    asfPrintStatus("   %d: %s\n", i+1, infiles[i]);

    // form array for asf_mosaic
    char **files = MALLOC(sizeof(char*)*(n_inputs+1));
    for (i = 0; i < n_inputs; ++i) {
      int n = argc-i-1;
      files[i] = MALLOC(sizeof(char)*(strlen(argv[n])+2));
      //printf("File #%d: %s\n", i+1, argv[n]);
      strcpy(files[i], argv[n]);
    }
    files[n_inputs] = NULL;

    char *outfile_full = appendExt(outfile, ".img");

    double lat_min = -999, lon_min = -999;
    double lat_max = 999, lon_max = 999;

    int multiband = 1;
    int band_num = 0;

    asf_mosaic(pp, projection_type, force_flag, resample_method,
              average_height, datum, pixel_size, multiband,  band_num, files,
              outfile, background_val, lat_min, lat_max, lon_min, lon_max);

    free(outfile_full);
    for (i=0; i<n_inputs; ++i)
      FREE(files[i]);
    FREE(files);

    asfPrintStatus("Done.\n");
    return 0;
}
