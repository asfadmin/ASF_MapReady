#define ASF_NAME_STRING "dem_trim"

#include <stdio.h>
#include <asf.h>
#include <asf_sar.h>
#include <asf_terrcorr.h>
#include <asf_license.h>
#include <asf_contact.h>

void usage()
{
    printf("dem_trim <input DEM> <input SAR> <output DEM>\n"
        "     input DEM:  input DEM image basename\n"
        "     input SAR:  input SAR image basename\n"
        "     output DEM: output DEM file\n\n"
        "Clips out the portion of the DEM that covers the given SAR image,\n"
        "with a healthy amount of padding along the edges to allow for\n"
        "geolocation and/or height corrections during terrain correction.\n\n");
    exit(1);
}

// Main program body.
int
main (int argc, char *argv[])
{
  handle_common_asf_args(&argc, &argv, "dem_trim");
  asfSplashScreen(argc, argv);

  if (argc != 4) usage();

  char *demFile = argv[1];
  char *sarFile = argv[2];
  char *outFile = argv[3];

  meta_parameters *metaDEM = meta_read(demFile);
  meta_parameters *metaSAR = meta_read(sarFile);

  cut_dem(metaSAR, metaDEM, demFile, outFile);

  meta_free(metaDEM);
  meta_free(metaSAR);
}
