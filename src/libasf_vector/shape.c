#include "shapefil.h"
#include "spheroids.h"
#include "asf_vector.h"
#include "libasf_proj.h"
#include "asf.h"

// Initialize internal format such as RGPS and MULTIMATCH
void shape_init(char *inFile, format_type_t format)
{
  char *dbaseFile;
  DBFHandle dbase;
  SHPHandle shape;

  // Open database for initialization
  dbaseFile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+5));
  sprintf(dbaseFile, "%s.dbf", inFile);
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  // Add fields to database
  if (format == RGPS) {
    if (DBFAddField(dbase, "Cell_ID", FTInteger, 6, 0) == -1)
      asfPrintError("Could not add 'Cell_ID' field to database file\n");
    if (DBFAddField(dbase, "Vertices", FTInteger, 2, 0) == -1)
      asfPrintError("Could not add 'Vertices' field to database file\n");
    if (DBFAddField(dbase, "Date", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'Date' field to database file\n");
    if (DBFAddField(dbase, "SrcImage", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'SrcImage' field to database file\n");
    if (DBFAddField(dbase, "TrgImage", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'TrgImage' field to database file\n");
    if (DBFAddField(dbase, "Stream", FTString, 3, 0) == -1)
      asfPrintError("Could not add 'Stream' field to database file\n");
    if (DBFAddField(dbase, "Area", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Area' field to database file\n");
    if (DBFAddField(dbase, "MY_ice", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'MY_ice' field to database file\n");
    if (DBFAddField(dbase, "OpenWater", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'OpenWater' field to database file\n");
    if (DBFAddField(dbase, "IncidAngle", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'IncidAngle' field to database file\n");
    if (DBFAddField(dbase, "Cell_x", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Cell_x' field to database file\n");
    if (DBFAddField(dbase, "Cell_y", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Cell_y' field to database file\n");
    if (DBFAddField(dbase, "dudx", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dudx' field to database file\n");
    if (DBFAddField(dbase, "dudy", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dudy' field to database file\n");
    if (DBFAddField(dbase, "dvdx", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dvdx' field to database file\n");
    if (DBFAddField(dbase, "dvdy", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dvdy' field to database file\n");
    if (DBFAddField(dbase, "dtp", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dtp' field to database file\n");
    if (DBFAddField(dbase, "Temp", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Temp' field to database file\n");
    if (DBFAddField(dbase, "u_wind", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'u_wind' field to database file\n");
    if (DBFAddField(dbase, "v_wind", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'v_wind' field to database file\n");
  }
  else if (format == RGPS_GRID) {
    if (DBFAddField(dbase, "Grid_ID", FTInteger, 6, 0) == -1)
      asfPrintError("Could not add 'Grid_ID' field to database file\n");
    if (DBFAddField(dbase, "Date", FTString, 12, 0) == -1)
      asfPrintError("Could not add 'Date' field to database file\n");
    if (DBFAddField(dbase, "Day", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Grid_x' field to database file\n");
    if (DBFAddField(dbase, "Grid_x", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Grid_x' field to database file\n");
    if (DBFAddField(dbase, "Grid_y", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Grid_y' field to database file\n");
    if (DBFAddField(dbase, "SrcImage", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'SrcImage' field to database file\n");
    if (DBFAddField(dbase, "TrgImage", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'TrgImage' field to database file\n");
    if (DBFAddField(dbase, "Stream", FTString, 3, 0) == -1)
      asfPrintError("Could not add 'Stream' field to database file\n");
    if (DBFAddField(dbase, "Quality", FTInteger, 3, 0) == -1)
      asfPrintError("Could not add 'Quality' field to database file\n");
  }
  else if (format == RGPS_WEATHER) {
    if (DBFAddField(dbase, "Date", FTString, 12, 0) == -1)
      asfPrintError("Could not add 'Date' field to database file\n");
    if (DBFAddField(dbase, "Lat", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Lat' field to database file\n");
    if (DBFAddField(dbase, "Lon", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Lon' field to database file\n");
    if (DBFAddField(dbase, "Direction", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Direction' field to database file\n");
    if (DBFAddField(dbase, "Speed", FTDouble, 5, 1) == -1)
      asfPrintError("Could not add 'Speed' field to database file\n");
    if (DBFAddField(dbase, "Temp", FTDouble, 5, 1) == -1)
      asfPrintError("Could not add 'Temp' field to database file\n");
    if (DBFAddField(dbase, "Pressure", FTDouble, 6, 1) == -1)
      asfPrintError("Could not add 'Pressure' field to database file\n");
  }
  else if (format == MULTIMATCH) {
    if (DBFAddField(dbase, "Ref_x", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_x' field to database file\n");
    if (DBFAddField(dbase, "Ref_y", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_y' field to database file\n");
    if (DBFAddField(dbase, "Ref_z", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_z' field to database file\n");
    if (DBFAddField(dbase, "Search_x", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_x' field to database file\n");
    if (DBFAddField(dbase, "Search_y", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_y' field to database file\n");
    if (DBFAddField(dbase, "Search_z", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_z' field to database file\n");
    if (DBFAddField(dbase, "dx", FTDouble, 7, 2) == -1)
      asfPrintError("Could not add 'dx' field to database file\n");
    if (DBFAddField(dbase, "dy", FTDouble, 7, 2) == -1)
      asfPrintError("Could not add 'dy' field to database file\n");
    if (DBFAddField(dbase, "dh", FTDouble, 7, 3) == -1)
      asfPrintError("Could not add 'dh' field to database file\n");
    if (DBFAddField(dbase, "Direction", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Direction' field to database file\n");
    if (DBFAddField(dbase, "Speed", FTDouble, 6, 1) == -1)
      asfPrintError("Could not add 'Speed' field to database file\n");
  }
  else if (format == GRANULE_COUNT) {
    if (DBFAddField(dbase, "FRAMES", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAMES' field to database file\n");
  }
  else if (format == GRANULE_LIST) {
    if (DBFAddField(dbase, "STACK_ID", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'STACK_ID' field to database file\n");
    if (DBFAddField(dbase, "GRANULE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'GRANULE' field to database file\n");
    if (DBFAddField(dbase, "FRAMES", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAMES' field to database file\n");
  }
  else if (format == GRANULE_DETAILS_A3) {
    if (DBFAddField(dbase, "STACK_ID", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'STACK_ID' field to database file\n");
    if (DBFAddField(dbase, "GRANULE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'GRANULE' field to database file\n");
    if (DBFAddField(dbase, "SATELLITE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'SATELLITE' field to database file\n");
    if (DBFAddField(dbase, "BEAM_MODE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'BEAM_MODE' field to database file\n");
    if (DBFAddField(dbase, "OFF_NADIR", FTDouble, 5, 1) == -1)
      asfPrintError("Could not add 'OFF_NADIR' field to database file\n");
    if (DBFAddField(dbase, "ORBIT", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'ORBIT' field to database file\n");
    if (DBFAddField(dbase, "FRAME", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAME' field to database file\n");
    if (DBFAddField(dbase, "DATE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'DATE' field to database file\n");
    if (DBFAddField(dbase, "NSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "NSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LON field to database file\n");
    if (DBFAddField(dbase, "FSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "FSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LON field to database file\n");
    if (DBFAddField(dbase, "N_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LAT field to database file\n");
    if (DBFAddField(dbase, "N_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LON field to database file\n");
    if (DBFAddField(dbase, "F_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LAT field to database file\n");
    if (DBFAddField(dbase, "F_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LON field to database file\n");
  }
  else if (format == GRANULE_DETAILS) {
    if (DBFAddField(dbase, "STACK_ID", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'STACK_ID' field to database file\n");
    if (DBFAddField(dbase, "GRANULE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'GRANULE' field to database file\n");
    if (DBFAddField(dbase, "SATELLITE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'SATELLITE' field to database file\n");
    if (DBFAddField(dbase, "BEAM_MODE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'BEAM_MODE' field to database file\n");
    if (DBFAddField(dbase, "ORBIT", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'ORBIT' field to database file\n");
    if (DBFAddField(dbase, "FRAME", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAME' field to database file\n");
    if (DBFAddField(dbase, "DATE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'DATE' field to database file\n");
    if (DBFAddField(dbase, "NSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "NSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LON field to database file\n");
    if (DBFAddField(dbase, "FSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "FSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LON field to database file\n");
    if (DBFAddField(dbase, "N_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LAT field to database file\n");
    if (DBFAddField(dbase, "N_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LON field to database file\n");
    if (DBFAddField(dbase, "F_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LAT field to database file\n");
    if (DBFAddField(dbase, "F_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LON field to database file\n");
  }
  else if (format == FOOT_PRINT) {
    if (DBFAddField(dbase, "STACK_ID", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'STACK_ID' field to database file\n");
    if (DBFAddField(dbase, "FRAME_CNT", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'FRAME_CNT' field to database file\n");
    if (DBFAddField(dbase, "GRANULE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'GRANULE' field to database file\n");
    if (DBFAddField(dbase, "SATELLITE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'SATELLITE' field to database file\n");
    if (DBFAddField(dbase, "BEAM_MODE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'BEAM_MODE' field to database file\n");
    if (DBFAddField(dbase, "OFF_NADIR", FTDouble, 5, 1) == -1)
      asfPrintError("Could not add 'OFF_NADIR' field to database file\n");
    if (DBFAddField(dbase, "ORBIT", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'ORBIT' field to database file\n");
    if (DBFAddField(dbase, "FRAME", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAME' field to database file\n");
    if (DBFAddField(dbase, "DATE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'DATE' field to database file\n");
    if (DBFAddField(dbase, "ORBIT_DIR", FTString, 15, 0) == -1)
      asfPrintError("Could not add 'ORBIT_DIR' field to database file\n");
    if (DBFAddField(dbase, "PATH", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'PATH' field to database file\n");
    if (DBFAddField(dbase, "CENTER_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add 'CENTER_LAT' field to database file\n");
    if (DBFAddField(dbase, "CENTER_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add 'CENTER_LON' field to database file\n");
    if (DBFAddField(dbase, "NSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "NSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LON field to database file\n");
    if (DBFAddField(dbase, "FSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "FSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LON field to database file\n");
    if (DBFAddField(dbase, "N_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LAT field to database file\n");
    if (DBFAddField(dbase, "N_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LON field to database file\n");
    if (DBFAddField(dbase, "F_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LAT field to database file\n");
    if (DBFAddField(dbase, "F_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LON field to database file\n");
  }

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  if (format == POINT || format == RGPS_GRID || format == RGPS_WEATHER ||
      format == MULTIMATCH)
    shape = SHPCreate(inFile, SHPT_POINT);
  else
    shape = SHPCreate(inFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", inFile);

  // Close shapefile for initialization
  SHPClose(shape);

  FREE(dbaseFile);

  return;
}

void open_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape)
{
  char *dbaseFile;

  if (dbase) {
    // Open database for adding values
    dbaseFile = appendExt(inFile, ".dbf");
    *dbase = DBFOpen(dbaseFile, "r+b");
    if (*dbase == NULL)
      asfPrintError("Could not open database file '%s'\n", dbaseFile);
    FREE(dbaseFile);
  }

  if (shape) {
    // Create a copy of the name to use for opening
    char tmpInFile[1024];
    strcpy(tmpInFile, inFile);
    char *ext = findExt(inFile);
    if (!ext) {
      // KLUDGE ALERT!  SHPOpen() below replaces the file extension in inFile
      // by searching from the end of the filename in reverse for a '.'
      // character, then appends .shx and .shp to the two filenames that it
      // produces ...Unfortunately, this results in truncated ALOS basenames
      // in the output files AND we don't own the shape library.
      
      // So, since SHPOpen() always wants to strip off an extension, we
      // add an extension for it to strip in cases where one isn't already
      // there.  This will solve the ALOS naming problem.
      sprintf(tmpInFile, "%s.dummy", inFile);
    }

    // Open shapefile for adding values
    *shape = SHPOpen(tmpInFile, "r+b");
    if (*shape == NULL)
      asfPrintError("Could not open shapefile '%s'\n", inFile);
  }
}

void close_shape(DBFHandle dbase, SHPHandle shape)
{
  // Close database
  DBFClose(dbase);

  // Close shapefile
  SHPClose(shape);

  return;
}

void write_esri_proj_file(char *inFile)
{
  FILE *fp;
  char esri_prj_file_name[255];

  create_name (esri_prj_file_name, inFile, ".prj");

  fp = FOPEN(esri_prj_file_name, "w");
  fprintf(fp,
      "GEOGCS[\"GCS_WGS_1984\","
      "DATUM[\"D_WGS_1984\","
      "SPHEROID[\"WGS_1984\",6378137,298.257223563]],"
      "PRIMEM[\"Greenwich\",0],"
      "UNIT[\"Degree\",0.017453292519943295]]");
  FCLOSE(fp);
}

void write_asf2esri_proj(meta_parameters *meta, char *projFile, char *outFile)
{
  FILE *fpIn, *fpOut;
  char projcsStr[100], geogcsStr[200], projStr[250], datumStr[150];
  char spheroidStr[100], esri_prj_file_name[255];
  char **error;

  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum;
  spheroid_type_t spheroid;
  double semimajor;
  double inv_flattening ;

  create_name (esri_prj_file_name, outFile, ".prj");

  // Get projection information
  if (meta &&
      meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR ||
      meta->projection->type == POLAR_STEREOGRAPHIC ||
      meta->projection->type == ALBERS_EQUAL_AREA ||
      meta->projection->type == LAMBERT_CONFORMAL_CONIC ||
      meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
    
    pps = meta->projection->param;
    proj_type = meta->projection->type;
    datum = meta->projection->datum;
    spheroid = meta->projection->spheroid;
    semimajor = meta->projection->re_major;
    inv_flattening = 
      semimajor / (semimajor - meta->projection->re_minor);
  }
  else if (projFile) {
    parse_proj_args_file(projFile, &pps, &proj_type, &datum, &spheroid, error);
    fpIn = FOPEN(projFile, "r");
    //spheroid = get_spheroid(fpIn);
    FCLOSE(fpIn);

    switch (spheroid)
      {
      case BESSEL_SPHEROID:
	semimajor = BESSEL_SEMIMAJOR;
	inv_flattening = BESSEL_INV_FLATTENING;
	break;
      case CLARKE1866_SPHEROID:
	semimajor = CLARKE1866_SEMIMAJOR;
	inv_flattening = CLARKE1866_INV_FLATTENING;
	break;
      case GEM6_SPHEROID:
	semimajor = GEM6_SEMIMAJOR;
	inv_flattening = GEM6_INV_FLATTENING;
	break;
      case GEM10C_SPHEROID:
	semimajor = GEM10C_SEMIMAJOR;
	inv_flattening = GEM10C_INV_FLATTENING;			\
	break;
      case GRS1980_SPHEROID:
	semimajor = GRS1980_SEMIMAJOR;
	inv_flattening = GRS1980_INV_FLATTENING;
	break;
      case INTERNATIONAL1924_SPHEROID:
	semimajor = INTERNATIONAL1924_SEMIMAJOR;
	inv_flattening = INTERNATIONAL1924_INV_FLATTENING;
	break;
      case INTERNATIONAL1967_SPHEROID:
	semimajor = INTERNATIONAL1967_SEMIMAJOR;
	inv_flattening = INTERNATIONAL1967_INV_FLATTENING;
	break;
      case WGS72_SPHEROID:
	semimajor = WGS72_SEMIMAJOR;
	inv_flattening = WGS72_INV_FLATTENING;
	break;
      case WGS84_SPHEROID:
	semimajor = WGS84_SEMIMAJOR;
	inv_flattening = WGS84_INV_FLATTENING;
	break;
      case HUGHES_SPHEROID:
	semimajor = HUGHES_SEMIMAJOR;
	inv_flattening = HUGHES_INV_FLATTENING;
	break;
      }
  }

  // Convert the projection information into ESRI projection format
  if ((meta &&
       meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR ||
       meta->projection->type == POLAR_STEREOGRAPHIC ||
       meta->projection->type == ALBERS_EQUAL_AREA ||
       meta->projection->type == LAMBERT_CONFORMAL_CONIC ||
       meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) || projFile) {

    // Construct geographic coordinate system string
    sprintf(geogcsStr, "GEOGCS[");
    sprintf(datumStr, "DATUM[");
    switch (datum)
      {
      case EGM96_DATUM:
	break;
      case ED50_DATUM:
	strcat(geogcsStr, "\"GCS_European_1950\",");
	strcat(datumStr, "\"D_European_1950\",");
	break;
      case ETRF89_DATUM:
	strcat(geogcsStr, "\"GCS_ETRS_1989\",");
	strcat(datumStr, "\"D_ETRS_1989\",");
	break;
      case ITRF97_DATUM:
	break;
      case NAD27_DATUM:
	strcat(geogcsStr, "\"GCS_North_American_1927\",");
	strcat(datumStr, "\"D_North_American_1927\",");
	break;
      case NAD83_DATUM:
	strcat(geogcsStr, "\"GCS_North_American_1983\",");
	strcat(datumStr, "\"D_North_American_1983\",");
	break;
      case WGS72_DATUM:
	strcat(geogcsStr, "\"GCS_WGS_1972\",");
	strcat(datumStr, "\"D_WGS_1972\",");
	break;
      case WGS84_DATUM:
	strcat(geogcsStr, "\"GCS_WGS_1984\",");
	strcat(datumStr, "\"D_WGS_1984\",");
	break;
      case HUGHES_DATUM:
	strcat(geogcsStr, "\"GCS_HUGHES\",");
	strcat(datumStr, "\"D_HUGHES\",");
	break;
      }
    strcat(geogcsStr, datumStr);
    switch (spheroid)
      {
      case BESSEL_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"BESSEL\",%.0lf,%.9lf]]",
		semimajor, inv_flattening);
	break;
      case CLARKE1866_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"CLARKE_1866\",%.0lf,%.9lf]]",
		semimajor, inv_flattening);
	break;
      case GEM6_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"GEM6\",%.0lf,%.9lf]]",
		semimajor, inv_flattening);
	break;
      case GEM10C_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"GEM10C\",%.0lf,%.9lf]]",
		semimajor, inv_flattening);	\
	break;
      case GRS1980_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"GRS_1980\",%.0lf,%.9lf]]", 
		semimajor, inv_flattening);
	break;
      case INTERNATIONAL1924_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"International_1924\",%.0lf,%.9lf]]",
		semimajor, inv_flattening);
	break;
      case INTERNATIONAL1967_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"International_1967\",%.0lf,%.9lf]]",
		semimajor, inv_flattening);
	break;
      case WGS72_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"WGS_1972\",%.0lf,%.9lf]]",
		semimajor, inv_flattening);
	break;
      case WGS84_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"WGS_1984\",%.0lf,%.9lf]]",
		semimajor, inv_flattening);
	break;
      case HUGHES_SPHEROID:
	sprintf(spheroidStr, "SPHEROID[\"HUGHES\",%.0lf,%9lf]]",
		semimajor, inv_flattening);
	break;
      }
    strcat(geogcsStr, spheroidStr);  
    
    // Construct projection string
    switch (proj_type)
      {
      case LAT_LONG_PSEUDO_PROJECTION:
	break;
      case UNIVERSAL_TRANSVERSE_MERCATOR:
	sprintf(projcsStr, "PROJCS[\"Universal_Transverse_Mercator\"");
	sprintf(projStr, "PROJECTION[\"Transverse_Mercator\"],PARAMETER[\""
		"False_Easting\",%.1lf],PARAMETER[\"False_Northing\",%.1lf],"
		"PARAMETER[\"Central_Meridian\",%.1lf],PARAMETER["
		"\"Scale_Factor\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.1lf],"
		"UNIT[\"Meter\",1.0]",
		pps.utm.false_easting, pps.utm.false_northing, 
		pps.utm.lon0, pps.utm.scale_factor, pps.utm.lat0);
	break;
      case POLAR_STEREOGRAPHIC:
	if (!isfinite(pps.ps.false_easting))
	  pps.ps.false_easting = 0.0;
	if (!isfinite(pps.ps.false_northing))
	  pps.ps.false_northing = 0.0;
	sprintf(projcsStr, "PROJCS[\"Polar_Stereographic\"");
	sprintf(projStr, "PROJECTION[\"Stereographic\"],PARAMETER["
		"\"False_Easting\",%.1lf],PARAMETER[\"False_Northing\",%.1lf],"
		"PARAMETER[\"Central_Meridian\",%.1lf],PARAMETER["
		"\"Scale_Factor\",1.0],PARAMETER[\"Latitude_Of_Origin\",%.1lf],"
		"UNIT[\"Meter\",1.0]",
		pps.ps.false_easting, pps.ps.false_northing, pps.ps.slat, 
		pps.ps.slon);
	break;
      case ALBERS_EQUAL_AREA:
	sprintf(projcsStr, "PROJCS[\"Albers_Equal_Area_Conic\"");
	sprintf(projStr, "PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\","
		"%.1lf],PARAMETER[\"False_Northing\",%.1lf],PARAMETER["
		"\"Central_Meridian\",%.1lf],PARAMETER[\"Standard_Parallel_1\","
		"%.1lf],PARAMETER[\"Standard_Parallel_2\",%.1lf],PARAMETER["
		"\"Latitude_Of_Origin\",%.1lf],UNIT[\"Meter\",1.0]",
		pps.albers.false_easting, pps.albers.false_northing, 
		pps.albers.center_meridian, pps.albers.std_parallel1, 
		pps.albers.std_parallel2, pps.albers.orig_latitude);
	break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	sprintf(projcsStr, "PROJCS[\"Lambert_Azimuthal_Equal_Area\"");
	sprintf(projStr, "PROJECTION[\"\"],PARAMETER[\"False_Easting\","
		"%.1lf],PARAMETER[\"False_Northing\",%.1lf],PARAMETER["
		"\"Central_Meridian\",%.1lf],PARAMETER["
		"\"Latitude_Of_Origin\",%.1lf],UNIT[\"Meter\",1.0]",
		pps.lamaz.false_easting, pps.lamaz.false_northing, 
		pps.lamaz.center_lat, pps.lamaz.center_lon);
	break;
      case LAMBERT_CONFORMAL_CONIC:
	sprintf(projcsStr, "PROJCS[\"Lambert_Conformal_Conic\"");
	sprintf(projStr, "PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER["
		"\"False_Easting\",%.1lf],PARAMETER[\"False_Northing\",%.1lf],"
		"PARAMETER[\"Central_Meridian\",%.1lf],PARAMETER["
		"\"Standard_Parallel_1\",%.1lf],PARAMETER["
		"\"Standard_Parallel_2\",%.1lf],PARAMETER["
		"\"Latitude_Of_Origin\",%.1lf],UNIT[\"Meter\",1.0]",
		pps.lamcc.false_easting, pps.lamcc.false_northing, 
		pps.lamcc.lon0, pps.lamcc.plat1, pps.lamcc.plat2, 
		pps.lamcc.lat0);
	break;
      case MERCATOR:
	break;
      case EQUI_RECTANGULAR:
	break;
      }
    
    fpOut = FOPEN(esri_prj_file_name, "w");
    fprintf(fpOut, "%s,%s,PRIMEM[\"Greenwich\",0],UNIT[\"Degree\","
	    "0.0174532925199432955]],%s]\n", projcsStr, geogcsStr, projStr);
    FCLOSE(fpOut);
  }
  else {
    fpOut = FOPEN(esri_prj_file_name, "w");
    fprintf(fpOut,
	    "GEOGCS[\"GCS_WGS_1984\","
	    "DATUM[\"D_WGS_1984\","
	    "SPHEROID[\"WGS_1984\",6378137,298.257223563]],"
	    "PRIMEM[\"Greenwich\",0],"
	    "UNIT[\"Degree\",0.017453292519943295]]");
    FCLOSE(fpOut);
  }
}

// Convert shape to point file
int shape2point(char *inFile, char *outFile, int listFlag)
{
  FILE *fp = NULL;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  int ii, kk, nEntities, nVertices, pointType;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Extract the vital information out of the shapefile
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
  switch (pointType) {
  case SHPT_POLYGON:
    asfPrintError("Shape file contains point information!\n"
                  "Use output format 'polygon'\n");
    break;
  case SHPT_POINT:
    break;
  case SHPT_ARC:
    asfPrintError("Shape file data type 'Arc' not supported\n");
    break;
  case SHPT_MULTIPOINT:
    asfPrintError("Shape file data type 'Multipoint' not supported\n");
    break;
  default:
    asfPrintError("Unexpected or unrecognized shape file data format\n");
    break;
  }

  // Write file
  fp = FOPEN(outFile, "w");
  fprintf(fp, "# Format: POINT (generated by convert2vector "
      "(version %s))\n", SVN_REV);
  fprintf(fp, "#\n");
  fprintf(fp, "# ID,Latitude,Longitude\n");

  for (ii=0; ii<nEntities; ii++) {

    // Read object for the number of vertices
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;

    for (kk=0; kk<nVertices; kk++) {
      fprintf(fp, "%d,%.4f,%.4f\n",
              ii+1, shapeObject->padfY[kk], shapeObject->padfX[kk]);
    }

    SHPDestroyObject(shapeObject);
  }
  FCLOSE(fp);

  // Close shapefile
  close_shape(dbase, shape);

  return 1;
}

// Convert shape to polygon file
int shape2polygon(char *inFile, char *outfile, int listFlag)
{
  FILE *fp = NULL;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  int ii, kk, nEntities, nVertices, pointType;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Extract the vital information out of the shapefile
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
  switch (pointType) {
  case SHPT_POLYGON:
    break;
  case SHPT_POINT:
    asfPrintError("Shape file contains point information!\n"
                  "Use output format 'point'\n");
    break;
  case SHPT_ARC:
    asfPrintError("Shape file data type 'Arc' not supported\n");
    break;
  case SHPT_MULTIPOINT:
    asfPrintError("Shape file data type 'Multipoint' not supported\n");
    break;
  default:
    asfPrintError("Unexpected or unrecognized shape file data format\n");
    break;
  }

  // Open file
  fp = FOPEN(outfile, "w");
  fprintf(fp, "# Format: POLYGON (generated by convert2vector "
  	  "(version %s))\n", SVN_REV);
  fprintf(fp, "#\n");
  fprintf(fp, "# Polygon,ID,Latitude,Longitude\n");

  // Write the text file
  for (ii=0; ii<nEntities; ii++) {

    // Read object for the number of vertices
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;

    for (kk=0; kk<nVertices-1; kk++) {
      fprintf(fp, "%d,%d,%.4f,%.4f\n",
              ii+1, kk+1, shapeObject->padfY[kk], shapeObject->padfX[kk]);
    }

    SHPDestroyObject(shapeObject);
  }

  // Close shapefile
  close_shape(dbase, shape);
  FCLOSE(fp);

  return 1;
}

static void write_dbase_field_to_csv(DBFHandle dbase, int record,
                                     int field, char *line)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals, nValue;
  double fValue;
  const char *sValue;

  dbaseType = DBFGetFieldInfo(dbase, field, fieldName,
                              &nWidth, &nDecimals);
  switch (dbaseType)
    {
    case FTString:
      sValue = DBFReadStringAttribute(dbase, record, field);
      sprintf(str, "\"%s\",", sValue);
      break;
    case FTInteger:
      nValue = DBFReadIntegerAttribute(dbase, record, field);
      sprintf(str, "%d,", nValue);
      break;
    case FTDouble:
      fValue = DBFReadDoubleAttribute(dbase, record, field);
      sprintf(str, "%s,", lf(fValue));
      break;
    case FTLogical:
    case FTInvalid:
      break;
    }
  strcat(line, str);

}

static void write_name_field_to_csv(DBFHandle dbase, int record, char *header)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals;

  dbaseType = DBFGetFieldInfo(dbase, record, fieldName, &nWidth, &nDecimals);
  sprintf(str, "%s,", fieldName);
  strcat(header, str);
}

// Convert shape to generic csv file
int shape2csv(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  char *line = (char *) MALLOC(sizeof(char)*4096);
  char *header = (char *) MALLOC(sizeof(char)*4096);
  int ii, kk, ll, nEntities, nVertices, nParts, *part;
  int nFields, pointType;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Extract the vital information out of the shapefile
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
  switch (pointType)
    {
    case SHPT_POLYGON:
    case SHPT_POINT:
      break;
    case SHPT_ARC:
      asfPrintError
        ("Conversion does not support shape type 'Arc'\n");
      break;
    case SHPT_MULTIPOINT:
      asfPrintError
        ("Conversion does not support shape type 'Multipoint'\n");
      break;
    }

  // Determine the number of fields in the database file
  nFields = DBFGetFieldCount(dbase);

  // Open csv file
  fp = FOPEN(outFile, "w");
  strcpy(header, "");
  for (ii=0; ii<nFields; ii++)
    write_name_field_to_csv(dbase, ii, header);
  header[strlen(header)-1] = '\0';
  fprintf(fp, "%s\n", header);

  for (ii=0; ii<nEntities; ii++) {

    // Read lat/lon from shape object
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;
    nParts = shapeObject->nParts;
    part = (int *) MALLOC(sizeof(int)*(nParts+1));
    for (kk=0; kk<nParts; kk++)
      part[kk] = shapeObject->panPartStart[kk];
    part[nParts] = nVertices;
    SHPDestroyObject(shapeObject);
    if (nParts == 0)
      nParts++;

    strcpy(line, "");
    for (ll=0; ll<nParts; ll++) {
      for (kk=0; kk<nFields; kk++)
        write_dbase_field_to_csv(dbase, ii, kk, line);
    }
    line[strlen(line)-1] = '\0';
    fprintf(fp, "%s\n", line);
  }

  // Close shapefile
  close_shape(dbase, shape);

  // Clean up
  FREE(line);
  FREE(header);
  FCLOSE(fp);

  return 1;
}

static void write_dbase_field_to_kml(DBFHandle dbase, int record,
                     int field, FILE *fp)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals, nValue;
  double fValue;
  const char *sValue;

  dbaseType = DBFGetFieldInfo(dbase, field, fieldName,
                  &nWidth, &nDecimals);
  switch (dbaseType)
    {
    case FTString:
      sValue = DBFReadStringAttribute(dbase, record, field);
      fprintf(fp, "<strong>%s</strong>: %s <br>\n",
          fieldName, sValue);
      break;
    case FTInteger:
      nValue = DBFReadIntegerAttribute(dbase, record, field);
      fprintf(fp, "<strong>%s</strong>: %d <br>\n",
          fieldName, nValue);
      break;
    case FTDouble:
      fValue = DBFReadDoubleAttribute(dbase, record, field);
      sprintf(str, "<strong>%%s</strong>: %%%d.%dlf <br>\n",
          nWidth, nDecimals);
      fprintf(fp, str, fieldName, fValue);
      break;
    case FTLogical:
    case FTInvalid:
      break;
    }
}

static void write_name_field_to_kml(DBFHandle dbase, int record, FILE *fp)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals, nValue;
  double fValue;
  const char *sValue;

  dbaseType = DBFGetFieldInfo(dbase, 0, fieldName, &nWidth, &nDecimals);
  switch (dbaseType)
    {
    case FTString:
      sValue = DBFReadStringAttribute(dbase, record, 0);
      fprintf(fp, "<name>%s</name>\n", sValue);
      break;
    case FTInteger:
      nValue = DBFReadIntegerAttribute(dbase, record, 0);
      fprintf(fp, "<name>%d</name>\n", nValue);
      break;
    case FTDouble:
      fValue = DBFReadDoubleAttribute(dbase, record, 0);
      sprintf(str, "<name>%%%d.%dlf</name>\n", nWidth, nDecimals);
      fprintf(fp, str, fValue);
      break;
    case FTLogical:
    case FTInvalid:
      break;
    }
}

// Convert shape to kml file
int shape2kml(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  int ii, kk, ll, nEntities, nVertices, nParts, *part;
  int nFields, pointType;
  double *lat, *lon, *height, *min, *max, clat, clon;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Open kmlfile
  fp = FOPEN(outFile, "w");
  kml_header(fp);

  // Extract the vital information out of the shapefile
  min = (double *) MALLOC(sizeof(double)*4);
  max = (double *) MALLOC(sizeof(double)*4);
  SHPGetInfo(shape, &nEntities, &pointType, min, max);
  switch (pointType)
    {
    // 2D shape types
    case SHPT_POLYGON:
    case SHPT_ARC:
    case SHPT_POINT:
      break;
    case SHPT_MULTIPOINT:
      asfPrintError("Conversion does not support shape type 'Multipoint'\n");
      break;
    // 3D shape types
    case SHPT_POLYGONZ:
    case SHPT_ARCZ:
    case SHPT_POINTZ:
      break;
    case SHPT_MULTIPOINTZ:
      asfPrintError("Conversion does not support shape type 'Multipoint'\n");
      break;
    // 2D + measure types
    case SHPT_POINTM:
      asfPrintError("Conversion does not support shape type 'PointM'\n");
      break;
    case SHPT_ARCM:
      asfPrintError("Conversion does not support shape type 'ArcM'\n");
      break;
    case SHPT_POLYGONM:
      asfPrintError("Conversion does not support shape type 'PolygonM'\n");
      break;
    case SHPT_MULTIPOINTM:
      asfPrintError("Conversion does not support shape type 'MultipointM'\n");
      break;
    case SHPT_MULTIPATCH:
      asfPrintError("Conversion does not support shape type 'Multipatch'\n");
    }
  // Ball park center for <LookAt> position  - does not have to be accurate
  clat = min[1] + (max[1]-min[1])/2;
  clon = min[0] + (max[0]-min[0])/2;

  for (ii=0; ii<nEntities; ii++) {

    // Read lat/lon from shape object
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;
    lat = (double *) MALLOC(sizeof(double)*(nVertices+1));
    lon = (double *) MALLOC(sizeof(double)*(nVertices+1));
    height = (double *) MALLOC(sizeof(double)*(nVertices+1));
    for (kk=0; kk<nVertices; kk++) {
      lat[kk] = shapeObject->padfY[kk];
      lon[kk] = shapeObject->padfX[kk];
      if (shapeObject->padfZ)
	height[kk] = shapeObject->padfZ[kk];
    }
    lat[nVertices] = lat[0];
    lon[nVertices] = lon[0];
    height[nVertices] = height[0];
    nParts = shapeObject->nParts;
    part = (int *) MALLOC(sizeof(int)*(nParts+1));
    for (kk=0; kk<nParts; kk++)
      part[kk] = shapeObject->panPartStart[kk];
    part[nParts] = nVertices;
    SHPDestroyObject(shapeObject);
    if (nParts == 0)
      nParts++;

    // Extract the attributes out of the database file
    nFields = DBFGetFieldCount(dbase);

    for (ll=0; ll<nParts; ll++) {

      // Write information in kml file
      fprintf(fp, "<Placemark>\n");
      fprintf(fp, "<description><![CDATA[\n");
      fprintf(fp, "<!-- Format: SHAPE (generated by convert2vector "
          "(version %s)) -->\n", SVN_REV);
      for (kk=1; kk<nFields; kk++)
	write_dbase_field_to_kml(dbase, ii, kk, fp);
      fprintf(fp, "]]></description>\n");
      write_name_field_to_kml(dbase, ii, fp);
      fprintf(fp, "<LookAt>\n");
      fprintf(fp, "<longitude>%9.4f</longitude>\n", clon);
      fprintf(fp, "<latitude>%9.4f</latitude>\n", clat);
      fprintf(fp, "<range>400000</range>\n");
      fprintf(fp, "</LookAt>\n");
      if (pointType == SHPT_POINT) {
	fprintf(fp, "<Point>\n");
	fprintf(fp, "<coordinates>%.12lf,%.12lf,4000</coordinates>",
		lon[0], lat[0]);
	fprintf(fp, "</Point>\n");
      }
      else if (pointType == SHPT_POLYGON) {
	write_kml_style_keys(fp);
	fprintf(fp, "<Polygon>\n");
	fprintf(fp, "<outerBoundaryIs>\n");
	fprintf(fp, "<LinearRing>\n");
	fprintf(fp, "<coordinates>\n");
	for (kk=part[ll]; kk<part[ll+1]; kk++)
	  fprintf(fp, "%.6f,%.6f,4000\n", lon[kk], lat[kk]);
	fprintf(fp, "</coordinates>\n");
	fprintf(fp, "</LinearRing>\n");
	fprintf(fp, "</outerBoundaryIs>\n");
	fprintf(fp, "</Polygon>\n");
      }
      else if (pointType == SHPT_POINTZ) {
	fprintf(fp, "<Point>\n");
	fprintf(fp, "<coordinates>%.6lf,%.6lf,%.3lf</coordinates>",
		lon[0], lat[0], height[0]);
	fprintf(fp, "</Point>\n");
      }
      else if (pointType == SHPT_ARCZ) {
	fprintf(fp, "<styleUrl>#yellowLineGreenPoly</styleUrl>\n");
	fprintf(fp, "<LineString>\n");
        fprintf(fp, "<extrude>1</extrude>\n");
        fprintf(fp, "<tessellate>1</tessellate>\n");
        fprintf(fp, "<altitudeMode>%s</altitudeMode>\n", altitude_mode());
        fprintf(fp, "<coordinates>\n");
 	for (kk=part[ll]; kk<part[ll+1]; kk++)
	  fprintf(fp, "%.6f,%.6f,%.3f\n", lon[kk], lat[kk], height[kk]);
	fprintf(fp, "</coordinates>\n");
	fprintf(fp, "</LineString>\n");
      }
      else if (pointType == SHPT_POLYGONZ) {
	write_kml_style_keys(fp);
	fprintf(fp, "<Polygon>\n");
	fprintf(fp, "<outerBoundaryIs>\n");
	fprintf(fp, "<LinearRing>\n");
	fprintf(fp, "<coordinates>\n");
	for (kk=part[ll]; kk<part[ll+1]; kk++)
	  fprintf(fp, "%.6f,%.6f,%.3f\n", lon[kk], lat[kk], height[kk]);
	fprintf(fp, "</coordinates>\n");
	fprintf(fp, "</LinearRing>\n");
	fprintf(fp, "</outerBoundaryIs>\n");
	fprintf(fp, "</Polygon>\n");
      }
      fprintf(fp, "</Placemark>\n");
    }
  }

  // Close shapefile
  close_shape(dbase, shape);

  // Clean up
  kml_footer(fp);
  FCLOSE(fp);

  return 1;
}
