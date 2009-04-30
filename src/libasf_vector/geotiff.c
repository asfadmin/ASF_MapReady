#include "asf_tiff.h"
#include "asf.h"
#include "asf_nan.h"
#include "dateUtil.h"
#include "shapefil.h"
#include "asf_vector.h"
#include "geotiff_support.h"

typedef struct {
  char id[50];
  char format[20];
  int bits_per_sample;
  char planes[25];
  int band_count;
  double lat[5];
  double lon[5];
} geotiff_type_t;

static void geotiff_init(geotiff_type_t *geotiff)
{
  int ii;

  strcpy(geotiff->id, MAGIC_UNSET_STRING);
  geotiff->bits_per_sample = MAGIC_UNSET_INT;
  strcpy(geotiff->format, MAGIC_UNSET_STRING);
  strcpy(geotiff->planes, MAGIC_UNSET_STRING);
  geotiff->band_count = MAGIC_UNSET_INT;
  for (ii=0; ii<5; ii++) {
    geotiff->lat[ii] = MAGIC_UNSET_DOUBLE;
    geotiff->lon[ii] = MAGIC_UNSET_DOUBLE;
  }
}

static void read_geotiff(char *inFile, geotiff_type_t **g)
{
  geotiff_type_t *geo = (geotiff_type_t *) MALLOC(sizeof(geotiff_type_t));;
  meta_parameters *meta;
  TIFF *input_tiff;
  data_type_t data_type;
  short num_bands;
  short int sample_format, bits_per_sample, planar_config;
  int ignore[MAX_BANDS], is_scanline_format, is_palette_color_tiff;

  // Initialize structure
  geotiff_init(geo);
  *g = geo;

  // Read parameter out of TIFF structure
  input_tiff = XTIFFOpen(inFile, "r");
  get_tiff_data_config(input_tiff, &sample_format, &bits_per_sample,
                       &planar_config, &data_type, &num_bands,
                       &is_scanline_format, &is_palette_color_tiff, REPORT_LEVEL_NONE);
  XTIFFClose(input_tiff);
  strcpy(geo->id, inFile);
  if (sample_format == SAMPLEFORMAT_UINT)
    strcpy(geo->format, "unsigned integer");
  else if (sample_format == SAMPLEFORMAT_INT)
    strcpy(geo->format, "signed integer");
  else if (sample_format == SAMPLEFORMAT_IEEEFP)
    strcpy(geo->format, "floating point");
  geo->bits_per_sample = (int) bits_per_sample;
  if (planar_config == PLANARCONFIG_CONTIG)
    strcpy(geo->planes, "contiguous interlaced");
  else if (planar_config == PLANARCONFIG_SEPARATE)
    strcpy(geo->planes, "separate planes");
  geo->band_count = (int) num_bands;

  // Generate metadata structure
  int i;
  for (i=0; i<MAX_BANDS; i++) ignore[i] = 0; // Default to ignoring no bands
  meta = read_generic_geotiff_metadata(inFile, ignore, NULL);
  if (meta && meta->location) {
    meta_location *ml = meta->location; // Convenience pointer
    geo->lon[0] = ml->lon_start_near_range;
    geo->lat[0] = ml->lat_start_near_range;
    geo->lon[1] = ml->lon_start_far_range;
    geo->lat[1] = ml->lat_start_far_range;
    geo->lon[2] = ml->lon_end_far_range;
    geo->lat[2] = ml->lat_end_far_range;
    geo->lon[3] = ml->lon_end_near_range;
    geo->lat[3] = ml->lat_end_near_range;
    geo->lon[4] = geo->lon[0];
    geo->lat[4] = geo->lat[0];
  }
  else {
    meta_free(meta);
    asfPrintError("GeoTIFF %s contains no location information\n", inFile);
  }
}

// Convert GeoTIFF to generic csv file
int geotiff2csv(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  dbf_header_t *dbf;
  geotiff_type_t *geo;
  int ii, kk, nCols;
  char str[50];
  char *line = (char *) MALLOC(sizeof(char)*4096);
  char *header = (char *) MALLOC(sizeof(char)*4096);

  // Read configuration file
  strcpy(header, "");
  read_header_config("GEOTIFF", &dbf, &nCols);
  for (ii=0; ii<nCols; ii++)
    if (dbf[ii].visible) {
      if (strcmp(dbf[ii].header, "LAT") == 0 ||
	  strcmp(dbf[ii].header, "LON") == 0) {
	for (kk=0; kk<4; kk++) {
	  sprintf(str, "%s%d,", dbf[ii].header, kk+1);
	  strcat(header, str);
	}
      }
      else {
	sprintf(str, "%s,", dbf[ii].header);
	strcat(header, str);
      }
    }

  // Read GeoTIFF information
  read_geotiff(inFile, &geo);

  // Write generic csv file
  strcpy(line, "");
  fp = FOPEN(outFile, "w");
  header[strlen(header)-1] = '\0';
  fprintf(fp, "%s\n", header);
  header[strlen(header)-1] = ',';
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].visible) {
      if (strcmp(dbf[ii].header, "ID") == 0) {
	sprintf(str, "\"%s\",", geo->id);
	strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "FORMAT") == 0) {
	sprintf(str, "\"%s\",", geo->format);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "BITS_SAMPLES") == 0) {
	sprintf(str, "%d,", geo->bits_per_sample);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "PLANES") == 0) {
	sprintf(str, "\"%s\",", geo->planes);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "NUM_BANDS") == 0) {
	sprintf(str, "%d,", geo->band_count);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "LAT") == 0) {
	for (kk=0; kk<4; kk++) {
	  sprintf(str, "%s,", lf(geo->lat[kk]));
	  strcat(line, str);
	}
      }
      else if (strcmp(dbf[ii].header, "LON") == 0) {
	for (kk=0; kk<4; kk++) {
	  sprintf(str, "%s,", lf(geo->lon[kk]));
	  strcat(line, str);
	}
      }
    }
  }
  line[strlen(line)-1] = '\0';
  fprintf(fp, "%s\n", line);
  FREE(header);
  FREE(line);
  FCLOSE(fp);

  return 1;
}

static void add_placemark(FILE *fp, geotiff_type_t *geo, dbf_header_t *dbf,
			  int nCols)
{
  int ii, kk;
  char begin[10], end[10];

  // Print out according to configuration
  fprintf(fp, "<Placemark>\n");
  fprintf(fp, "  <description><![CDATA[\n");
  fprintf(fp, "<table width=\"350\"><tr><td>\n");
  fprintf(fp, "<!-- Format: GEOTIFF (generated by convert2vector "
          "(version %s)) -->\n", SVN_REV);
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].visible == 0) {
      strcpy(begin, "<!--");
      strcpy(end, "-->\n");
    }
    else {
      strcpy(begin, "");
      strcpy(end, "\n");
    }
    if (strcmp(dbf[ii].header, "ID") == 0)
      fprintf(fp, "%s<strong>ID</strong>: %s <br>%s", begin, geo->id, end);
    else if (strcmp(dbf[ii].header, "FORMAT") == 0)
      fprintf(fp, "%s<strong>Format</strong>: %s <br>%s",
	      begin, geo->format, end);
    else if (strcmp(dbf[ii].header, "BITS_SAMPLES") == 0)
      fprintf(fp, "%s<strong>Bits per sample</strong>: %d <br>%s",
	      begin, geo->bits_per_sample, end);
    else if (strcmp(dbf[ii].header, "PLANES") == 0)
      fprintf(fp, "%s<strong>Planes configuration</strong>: %s <br>%s",
	      begin, geo->planes, end);
    else if (strcmp(dbf[ii].header, "NUM_BANDS") == 0)
      fprintf(fp, "%s<strong>Number of bands</strong>: %d <br>%s",
	     begin, geo->band_count, end);
    else if (strcmp(dbf[ii].header, "LAT") == 0 &&
	     strcmp(dbf[ii].header, "LON") == 0) {
      for (kk=0; kk<4; kk++) {
	fprintf(fp, "%s<strong>Lat [%d]</strong>: %s <br>%s",
		begin, kk+1, lf(geo->lat[kk]), end);
	fprintf(fp, "%s<strong>Lon [%d]</strong>: %s <br>%s",
		begin, kk+1, lf(geo->lon[kk]), end);
      }
    }
  }
  fprintf(fp, "  ]]></description>\n");
  fprintf(fp, "  <name>%s</name>\n", geo->id);
  fprintf(fp, "  <LookAt>\n");
  fprintf(fp, "    <longitude>%.10f</longitude>\n", geo->lon[0]);
  fprintf(fp, "    <latitude>%.10f</latitude>\n", geo->lat[0]);
  fprintf(fp, "    <range>400000</range>\n");
  //fprintf(fp, "    <tilt>30</tilt>\n");
  fprintf(fp, "  </LookAt>\n");
  fprintf(fp, "  <visibility>1</visibility>\n");
  fprintf(fp, "  <open>1</open>\n");

  write_kml_style_keys(fp);

  fprintf(fp, "  <Polygon>\n");
  fprintf(fp, "    <extrude>1</extrude>\n");
  fprintf(fp, "    <altitudeMode>%s</altitudeMode>\n", altitude_mode());
  fprintf(fp, "    <outerBoundaryIs>\n");
  fprintf(fp, "     <LinearRing>\n");
  fprintf(fp, "      <coordinates>\n");
  fprintf(fp, "       %.12f,%.12f,7000\n", geo->lon[0], geo->lat[0]);
  fprintf(fp, "       %.12f,%.12f,7000\n", geo->lon[1], geo->lat[1]);
  fprintf(fp, "       %.12f,%.12f,7000\n", geo->lon[2], geo->lat[2]);
  fprintf(fp, "       %.12f,%.12f,7000\n", geo->lon[3], geo->lat[3]);
  fprintf(fp, "       %.12f,%.12f,7000\n", geo->lon[4], geo->lat[4]);
  fprintf(fp, "      </coordinates>\n");
  fprintf(fp, "     </LinearRing>\n");
  fprintf(fp, "    </outerBoundaryIs>\n");
  fprintf(fp, "  </Polygon>\n");
  fprintf(fp, "</Placemark>\n");

}

// Convert GeoTIFF to kml file
int geotiff2kml(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  dbf_header_t *dbf;
  geotiff_type_t *geo;
  int nCols;

  // Read configuration file
  read_header_config("GEOTIFF", &dbf, &nCols);

  // Read GeoTIFF information
  read_geotiff(inFile, &geo);

  // Write kml file
  fp = FOPEN(outFile, "w");
  kml_header(fp);
  add_placemark(fp, geo, dbf, nCols);
  kml_footer(fp);
  FCLOSE(fp);

  return 1;
}

void shape_geotiff_init(char *inFile)
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
  if (DBFAddField(dbase, "ID", FTString, 50, 0) == -1)
    asfPrintError("Could not add ID field to database file\n");
  if (DBFAddField(dbase, "Format", FTString, 20, 0) == -1)
    asfPrintError("Could not add format field to database file\n");
  if (DBFAddField(dbase, "Bits_samples", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add bits per sample  field to database file\n");
  if (DBFAddField(dbase, "Planes", FTString, 25, 0) == -1)
    asfPrintError("Could not add planes field to database file\n");
  if (DBFAddField(dbase, "Num_bands", FTInteger, 5, 0) == -1)
    asfPrintError("Could not add number of bands field to database file\n");
  if (DBFAddField(dbase, "Lat1", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
  if (DBFAddField(dbase, "Lon1", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add longitude field to database file\n");
  if (DBFAddField(dbase, "Lat2", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
  if (DBFAddField(dbase, "Lon2", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add longitude field to database file\n");
  if (DBFAddField(dbase, "Lat3", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
  if (DBFAddField(dbase, "Lon3", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add longitude field to database file\n");
  if (DBFAddField(dbase, "Lat4", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add latitude field to database file\n");
  if (DBFAddField(dbase, "Lon4", FTDouble, 9, 4) == -1)
    asfPrintError("Could not add longitude field to database file\n");

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  shape = SHPCreate(inFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", inFile);

  // Close shapefile for initialization
  SHPClose(shape);

  FREE(dbaseFile);

  return;
}

// Write GeoTIFF file to shape file
static void geotiff2shape_line(char *inFile, char *outFile, int n)
{
  geotiff_type_t *geo;
  DBFHandle dbase;
  SHPHandle shape;
  meta_parameters *meta = NULL;

  // Read GeoTIFF information
  read_geotiff(inFile, &geo);

  // Initialize output
  shape_geotiff_init(outFile);
  open_shape(outFile, &dbase, &shape);

  // Write information into database file
  DBFWriteStringAttribute(dbase, n, 0, geo->id);
  DBFWriteStringAttribute(dbase, n, 1, geo->format);
  DBFWriteIntegerAttribute(dbase, n, 2, geo->bits_per_sample);
  DBFWriteStringAttribute(dbase, n, 3, geo->planes);
  DBFWriteIntegerAttribute(dbase, n, 4, geo->band_count);
  DBFWriteDoubleAttribute(dbase, n, 5, geo->lat[0]);
  DBFWriteDoubleAttribute(dbase, n, 6, geo->lon[0]);
  DBFWriteDoubleAttribute(dbase, n, 7, geo->lat[1]);
  DBFWriteDoubleAttribute(dbase, n, 8, geo->lon[1]);
  DBFWriteDoubleAttribute(dbase, n, 9, geo->lat[2]);
  DBFWriteDoubleAttribute(dbase, n, 10, geo->lon[2]);
  DBFWriteDoubleAttribute(dbase, n, 11, geo->lat[3]);
  DBFWriteDoubleAttribute(dbase, n, 12, geo->lon[3]);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, 5,
				      geo->lon, geo->lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);
  meta_free(meta);

  return;
}

// Convert GeoTIFF to shapefile
int geotiff2shape(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  char *line = (char *) MALLOC(sizeof(char)*1024);
  int n=0;

  if (listFlag) {
    fp = FOPEN(inFile, "r");
    while (fgets(line, 1024, fp)) {
      line[strlen(line)-1] = '\0';
      geotiff2shape_line(inFile, outFile, n);
      n++;
    }
  }
  else
    geotiff2shape_line(inFile, outFile, n);

  return 1;
}
