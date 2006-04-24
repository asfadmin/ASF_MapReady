// Implementation of the interface described in geotiff_flavors.h.

#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <asf_reporting.h>
#include "asf_import.h"
#include "geotiff_flavors.h"

geotiff_importer
detect_geotiff_flavor (const char *file)
{
  // Open the Geotiff.
  TIFF *tiff = XTIFFOpen (file, "r");
  asfRequire (tiff != NULL, "Error opening input TIFF file.\n");
  GTIF *gtif = GTIFNew (tiff);
  asfRequire (gtif != NULL, 
	      "Error reading GeoTIFF keys from input TIFF file.\n");
  
  // Grab the citation.
  size_t max_citation_length = 10000;
  char *citation = MALLOC ((max_citation_length + 1) * sizeof (char));
  GTIFKeyGet (gtif, GTCitationGeoKey, citation, 0, max_citation_length);
  // Ensure the citation is at least eventually terminated somewhere.
  citation[max_citation_length] = '\0';

  // Test for a particular flavor.
  const char *tmp = "IMAGINE GeoTIFF Support";
  if ( strncmp (citation, tmp, strlen (tmp)) == 0 ) {
    short model_type;
    int read_count
      = GTIFKeyGet (gtif, GTModelTypeGeoKey, &model_type, 0, 1);
    asfRequire (read_count == 1, "GTIFKeyGet failed.\n");  
    if ( model_type == ModelTypeGeographic ) {
      return import_srtm_seamless;
    }
  }

  // Couldn't determine any flavor we know.
  return NULL;
}
