#include "asf_raster.h"
#include "asf_import.h"
#include "asf_export.h"
#include "asf_geocode.h"
#include "asf.h"
#include "asf_meta.h"

#define VERSION 1.0

void usage()
{
  printf("\n"
   "USAGE:\n"
   "   smap2browse <inFile>\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   inFile		Name of the SMAP HDF5 file\n");
  printf("\n"
   "OPTIONAL ARGUMENTS:\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program generates a number of browse images for a SMAP HDF5 file\n"
   "   with associated world and auxiliary files for ease of use in a GIS\n"
   "   environment.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  FILE *fp;
  char *baseName, inFile[1024], outFile[1024], tmpDataFile[1024], tmpDir[1024];
  char browseFile[1024], geoFile[1024];
  char browse[][10]= 
  	{"browseA","browseB","browseC","browseD","browseE","browseF","browseG",
  	 "browseH","browseI","browseJ","browseK","browseL","browseM","browseN",
  	 "browseO","browseP","browseQ","browseR","browseS","browseT","browseU",
  	 "browseV","browseW","browseX","browseY","browseZ"};
  double nanValue = MAGIC_UNSET_DOUBLE;
  float zero = 0.0;
  int ii;

  // Parse command line
  if ((argc-currArg)<1) {
    printf("Insufficient arguments.\n"); 
    usage();
  }
  strcpy(inFile, argv[currArg]);
  asfSplashScreen(argc, argv);

  // Create temporary directory
  baseName = get_basename(inFile);
  strcpy(tmpDir, baseName);
  strcat(tmpDir, "-");
  strcat(tmpDir, time_stamp_dir());
  create_clean_dir(tmpDir);

  // Ingest SMAP file into internal format
  sprintf(tmpDataFile, "%s%csmap", tmpDir, DIR_SEPARATOR);
  import_smap(inFile, tmpDataFile, nanValue, nanValue, nanValue, nanValue);

  // Cycle through SMAP image and generate browse images
  meta_parameters *meta = meta_read(tmpDataFile);
  int size = meta->general->sample_count;
  int lines = meta->general->line_count;
  int nBrowse = lines / size;
  for (ii=0; ii<nBrowse; ii++) {
  
  	// Generation of JPEG file
  	sprintf(browseFile, "%s%csmap_%s", tmpDir, DIR_SEPARATOR, browse[ii]);
  	if  (ii == nBrowse-1)
	  	trim(tmpDataFile, browseFile, 0, size*ii, size, lines-size*ii);
	  else
  		trim(tmpDataFile, browseFile, 0, size*ii, size, size);
  	sprintf(geoFile, "%s%csmap_%s_geo", tmpDir, DIR_SEPARATOR, browse[ii]);
    asf_geocode_from_proj_file("geographic", TRUE, RESAMPLE_BILINEAR, 0.0, 
    	WGS84_DATUM, -99, "HH_fore", browseFile, geoFile, -9999);
    sprintf(outFile, "%s_%s", baseName, browse[ii]);
  	asf_export(JPEG, SIGMA, geoFile, outFile);
  	
  	// Generation of world file
  	sprintf(outFile, "%s_%s.wld", baseName, browse[ii]);
  	meta_parameters *geo = meta_read(geoFile);
  	fp = FOPEN(outFile, "wt");
  	double perX = geo->projection->perX;
  	double perY = geo->projection->perY;
  	double startX = geo->projection->startX + (perX/2.0);
  	double startY = geo->projection->startY + (perY/2.0);
  	fprintf(fp, "%.10f\n%.10f\n%.10f\n%.10f\n", perX, zero, zero, perY);
  	fprintf(fp, "%.10f\n", startX);
  	fprintf(fp, "%.10f\n", startY);
  	FCLOSE(fp);
  	meta_free(geo);
  	
  	// Generation of auxiliary file
  	sprintf(outFile, "%s_%s.jpg.aux.xml", baseName, browse[ii]);
  	fp = FOPEN(outFile, "wt");
		fprintf(fp, "<PAMDataset>\n");
		fprintf(fp, "  <SRS>GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84"
			"\",6378137,298.2572201434283,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\""
			"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\","
			"0.0174532925199433],AUTHORITY[\"EPSG\",\"4326\"]]</SRS>\n");
		fprintf(fp, "  <Metadata>\n");
		fprintf(fp, "    <MDI key=\"AREA_OR_POINT\">Area</MDI>\n");
		fprintf(fp, "    <MDI key=\"TIFFTAG_RESOLUTIONUNIT\">1 (unitless)</MDI>\n");
		fprintf(fp, "    <MDI key=\"TIFFTAG_XRESOLUTION\">1</MDI>\n");
		fprintf(fp, "    <MDI key=\"TIFFTAG_YRESOLUTION\">1</MDI>\n");
		fprintf(fp, "  </Metadata>\n");
		fprintf(fp, "  <PAMRasterBand band=\"1\">\n");
		fprintf(fp, "    <NoDataValue>-9.99900000000000E+03</NoDataValue>\n");
		fprintf(fp, "    <Metadata domain=\"IMAGE_STRUCTURE\">\n");
		fprintf(fp, "      <MDI key=\"COMPRESSION\">JPEG</MDI>\n");
		fprintf(fp, "    </Metadata>\n");
		fprintf(fp, "  </PAMRasterBand>\n");
		fprintf(fp, "</PAMDataset>\n");
  	FCLOSE(fp);
  }
  remove_dir(tmpDir);
	meta_free(meta);

  return(0);
}
