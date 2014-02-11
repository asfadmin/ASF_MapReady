#include "asf_raster.h"
#include "asf_import.h"
#include "asf_export.h"
#include "asf_geocode.h"
#include "asf.h"
#include "asf_meta.h"
#include "libasf_proj.h"

#define VERSION 1.0

void usage()
{
  printf("\n"
   "USAGE:\n"
   "   asf2geobrowse <inFile> <outFile\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   inFile    Name of an ASF internal file\n"
   "   outFile   Name of the output browse image\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program generates browse images with associated world and auxiliary"
   " files for ease of use in a GIS environment.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  FILE *fp;
  char *baseName, inFile[1024], outFile[1024], tmpDir[1024];
  char browseFile[1024], geoFile[1024];
  char browse[][10]= 
  	{"browseA","browseB","browseC","browseD","browseE","browseF","browseG",
  	 "browseH","browseI","browseJ","browseK","browseL","browseM","browseN",
  	 "browseO","browseP","browseQ","browseR","browseS","browseT","browseU",
  	 "browseV","browseW","browseX","browseY","browseZ"};
  float zero = 0.0;
  int ii;

  // Parse command line
  if ((argc-currArg)<1) {
    printf("Insufficient arguments.\n"); 
    usage();
  }
  strcpy(inFile, argv[currArg]);
  strcpy(outFile, argv[currArg+1]);
  asfSplashScreen(argc, argv);

  // Create temporary directory, if needed
  meta_parameters *meta = meta_read(inFile);
  baseName = get_basename(outFile);
  if (!meta->projection) {
    strcpy(tmpDir, baseName);
    strcat(tmpDir, "-");
    strcat(tmpDir, time_stamp_dir());
    create_clean_dir(tmpDir);
  }

  // Generate browse image
  int size, lines, nBrowse;
  size = meta->general->sample_count;
  lines = meta->general->line_count;
  if (strcmp_case(meta->general->sensor, "SMAP") == 0)
    nBrowse = lines / size;
  else
    nBrowse = 1;
  for (ii=0; ii<nBrowse; ii++) {
  
    // Generate subsets if we deal with SMAP data and geocode
    if (strcmp_case(meta->general->sensor, "SMAP") == 0) {
      sprintf(browseFile, "%s%csmap_%s", tmpDir, DIR_SEPARATOR, browse[ii]);
      if (ii == nBrowse-1)
        trim(inFile, browseFile, 0, size*ii, size, lines-size*ii);
      else
        trim(inFile, browseFile, 0, size*ii, size, size);
      sprintf(geoFile, "%s%csmap_%s_geo", tmpDir, DIR_SEPARATOR, browse[ii]);
      asf_geocode_from_proj_file("geographic", TRUE, RESAMPLE_BILINEAR, 0.0, 
        WGS84_DATUM, -99, "HH_fore", browseFile, geoFile, -9999);
      sprintf(outFile, "%s_%s", baseName, browse[ii]);
    	asf_export(JPEG, SIGMA, geoFile, outFile);
    }
    else if (!meta->projection) {
      sprintf(geoFile, "%s%cbrowse_geo", tmpDir, DIR_SEPARATOR);
      asf_geocode_utm(RESAMPLE_BILINEAR, 0.0, WGS84_DATUM, -99, NULL, inFile, 
        geoFile, 0.0);
    	asf_export(JPEG, SIGMA, geoFile, outFile);      
    }
    else
      asf_export(JPEG, SIGMA, inFile, outFile);
  	
  	// Generation of world file
  	meta_parameters *geo;
  	if (strcmp_case(meta->general->sensor, "SMAP") == 0)
    	sprintf(outFile, "%s_%s.wld", baseName, browse[ii]);
    else
      sprintf(outFile, "%s.wld", baseName);
    if (!meta->projection)
    	geo = meta_read(geoFile);
    else
      geo = meta_read(inFile);
  	fp = FOPEN(outFile, "wt");
  	double perX = geo->projection->perX;
  	double perY = geo->projection->perY;
  	double startX = geo->projection->startX + (perX/2.0);
  	double startY = geo->projection->startY + (perY/2.0);
  	fprintf(fp, "%.10f\n%.10f\n%.10f\n%.10f\n", perX, zero, zero, perY);
  	fprintf(fp, "%.10f\n", startX);
  	fprintf(fp, "%.10f\n", startY);
  	FCLOSE(fp);
  	
  	// Generation of auxiliary file
  	char zone[5];
  	int epsg;
  	project_parameters_t *pps = &geo->projection->param;
  	if (strcmp_case(meta->general->sensor, "SMAP") == 0)
    	sprintf(outFile, "%s_%s.jpg.aux.xml", baseName, browse[ii]);
    else
      sprintf(outFile, "%s.jpg.aux.xml", baseName);
  	fp = FOPEN(outFile, "wt");
		fprintf(fp, "<PAMDataset>\n");
    if (geo->projection->type == LAT_LONG_PSEUDO_PROJECTION)
		  fprintf(fp, "  <SRS>GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\""
		    "WGS 84\",6378137,298.2572201434283,AUTHORITY[\"EPSG\",\"7030\"]],"
		    "AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\","
			  "0.0174532925199433],AUTHORITY[\"EPSG\",\"4326\"]]</SRS>\n");
		else if (geo->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
		  if (geo->projection->hem == 'N') {
		    sprintf(zone, "%dN", pps->utm.zone);
		    epsg = 32600 + pps->utm.zone;
		  }
		  else {
		    sprintf(zone, "%dS", pps->utm.zone);
		    epsg = 32700 + pps->utm.zone;
		  }
		  fprintf(fp, "  <SRS>PROJCS[\"WGS 84 / UTM zone %s\",GEOGCS[\"WGS 84\""
		    ",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.2572201434283,"
		    "AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\""
		    "Greenwich\",0],UNIT[\"degree\",0.0174532925199433],AUTHORITY[\"EPSG\","
		    "\"4326\"]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\""
		    "latitude_of_origin\",%g],PARAMETER[\"central_meridian\",%g],"
		    "PARAMETER[\"scale_factor\",%g],PARAMETER[\"false_easting\",%g],"
		    "PARAMETER[\"false_northing\",%g],UNIT[\"metre\",1,AUTHORITY[\"EPSG\","
		    "\"9001\"]],AUTHORITY[\"EPSG\",\"%d\"]]</SRS>\n", zone, pps->utm.lat0,
		    pps->utm.lon0, pps->utm.scale_factor, pps->utm.false_easting,
		    pps->utm.false_northing, epsg);
    }
		fprintf(fp, "  <Metadata>\n");
		fprintf(fp, "    <MDI key=\"AREA_OR_POINT\">Area</MDI>\n");
		fprintf(fp, "    <MDI key=\"TIFFTAG_RESOLUTIONUNIT\">1 (unitless)</MDI>\n");
		fprintf(fp, "    <MDI key=\"TIFFTAG_XRESOLUTION\">1</MDI>\n");
		fprintf(fp, "    <MDI key=\"TIFFTAG_YRESOLUTION\">1</MDI>\n");
		fprintf(fp, "  </Metadata>\n");
		fprintf(fp, "  <PAMRasterBand band=\"1\">\n");
		fprintf(fp, "    <NoDataValue>%.14E</NoDataValue>\n", geo->general->no_data);
		fprintf(fp, "    <Metadata domain=\"IMAGE_STRUCTURE\">\n");
		fprintf(fp, "      <MDI key=\"COMPRESSION\">JPEG</MDI>\n");
		fprintf(fp, "    </Metadata>\n");
		fprintf(fp, "  </PAMRasterBand>\n");
		fprintf(fp, "</PAMDataset>\n");
  	FCLOSE(fp);
  	meta_free(geo);
  }
  remove_dir(tmpDir);
	meta_free(meta);
  FREE(baseName);

  return(0);
}
