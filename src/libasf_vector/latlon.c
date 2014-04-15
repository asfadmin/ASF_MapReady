#include "asf_vector.h"
#include "shapefil.h"
#include "asf_nan.h"
#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include "ursa.h"
#include "dateUtil.h"

int latlon2shape(char *inFile, char *outFile)
{
  DBFHandle dbase;
  SHPHandle shape;
  dbf_header_t *dbf;
  char line[8192], id[255], shape_type[25], **coords;
  int ii, idx, n = 0, start = 0, nCols, nCoords;

  // Read CSV file
  FILE *ifp = FOPEN(inFile, "r");

  // Initialize the database file
  shapefile_init(outFile, "LATLON", NULL);
  open_shape(outFile, &dbase, &shape);
  read_header_config("LATLON", &dbf, &nCols, shape_type);

  while (fgets(line, 8192, ifp) != NULL) {
    chomp(line);
    split_into_array(line, ',', &nCols, &coords);
    nCoords = nCols/2 + 1;
    if (nCols % 2 != 0) {
      start = 1;
      strcpy(id, coords[0]);
    }
    else
      sprintf(id, "%d", n);
    printf("Found %d coordinate pairs\n", nCoords-1);
    double *lat = (double *) MALLOC(sizeof(double)*nCoords);
    double *lon = (double *) MALLOC(sizeof(double)*nCoords);
    for (ii=start; ii<nCols; ii+=2) {
      idx = ii/2;
      lat[idx] = atof(coords[ii]);
      lon[idx] = atof(coords[ii+1]);
    }
    lat[idx+1] = lat[0];
    lon[idx+1] = lon[0];
    dbf[ii].sValue = STRDUP(id);
    write_shape_attributes(dbase, nCoords, n, dbf);
    write_shape_object(shape, nCoords, lat, lon);
    FREE(lat);
    FREE(lon);
    FREE(coords);
    n++; 
  }

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  FCLOSE(ifp);

  return 1;
}
