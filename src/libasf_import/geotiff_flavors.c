// Implementation of the interface described in geotiff_flavors.h.

#include <assert.h>
#include <regex.h>

#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <proj.h>
#include "asf_import.h"
#include "geotiff_flavors.h"
#include "import_arcgis_geotiff.h"
#include "find_arcgis_geotiff_aux_name.h"

geotiff_importer
detect_geotiff_flavor (const char *file)
{
  GString *inGeotiffAuxName;
  
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
    short proj_type;
    int read_count
      = GTIFKeyGet (gtif, GTModelTypeGeoKey, &model_type, 0, 1);
    asfRequire (read_count == 1, "GTIFKeyGet failed.\n");  
    if ( model_type == ModelTypeGeographic ) {
      return import_usgs_seamless;
    }
    // If the TIFF claims that a projection exists OR if the projection type
    // is unknown, then check for an ArcGIS metadata (.aux) file since it may
    // contain the needed info.
    //
    // If the tiff turns out to be an ArcGIS GeoTIFF and the file(s) contain
    // a supported projection type, then return the arcgis importer...
    else if ( model_type == ModelTypeProjected ||
              (model_type != ModelTypeGeographic && model_type != ModelTypeGeocentric)
            ) {
      inGeotiffAuxName = find_arcgis_geotiff_aux_name(file);
      if ( inGeotiffAuxName != NULL ) {
        proj_type = getArcgisProjType (inGeotiffAuxName->str);
        switch (proj_type) {
          case UTM:     // Universal Transverse Mercator (UTM)
          case ALBERS:  // Albers Equal Area Conic (aka Albers Conical Equal Area)
          case LAMCC:   // Lambert Conformal Conic
          case PS:      // Polar Stereographic
          case LAMAZ:   // Lambert Azimuthal Equal Area
            return import_arcgis_geotiff;
            break;
          case DHFA_UNKNOWN_PROJECTION:
          default:      // Else cont...
            break;
        }
      }
      g_string_free(inGeotiffAuxName, TRUE);
    }
  } // strncmp on citation, tmp
  
  // Test for a particular flavor.
  GTIFKeyGet (gtif, PCSCitationGeoKey, citation, 0, max_citation_length);
  // Ensure the citation is at least eventually terminated somewhere.
  citation[max_citation_length] = '\0';
  regex_t asf_utm_citation_regex;
  /*
  int return_code 
    = regcomp (&asf_utm_citation_regex, 
	       "UTM zone ? projected GeoTIFF on WGS84 ellipsoid datum written "
	       "by Alaska Satellite Facility tools.", REG_NOSUB);
  */
  int return_code 
    = regcomp (&asf_utm_citation_regex, 
	       "UTM zone [[:digit:]]+ [NS]", REG_EXTENDED | REG_NOSUB);
  //[[:digit:]]+ [NS]
  assert (return_code == 0);
  return_code = regexec (&asf_utm_citation_regex, citation, 0, NULL, 0);
  if ( return_code == 0 ) {
    return import_asf_utm_geotiff;
  }

  // Couldn't determine any flavor we know.
  return NULL;
}
