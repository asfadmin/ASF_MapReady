#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "asf_contact.h"
#include "asf_license.h"
#include "netcdf.h"
#include "gdal.h"

#define ASF_NAME_STRING \
"convert2roipac"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" <inFile> <outFile>\n"

#define ASF_DESCRIPTION_STRING \
"   This tool ingests files required for the InSAR processing with ROI_PAC:\n"\
"   DEMs from OpenTOPO and ZPDDM data from OSCAR.\n"\
"   The output is a generic binary file and the related .rsc file.\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   inFile\n"\
"        The file that needs to be converted into ROI_PAC format.\n"\
"   outFile\n"\
"        The basename of the ROI_PAC file.\n"

// Print the help info & exit
static void print_help(void)
{
  asfPrintStatus(
      "\n"
      "Tool name:\n   " ASF_NAME_STRING "\n\n"
      "Usage:\n" ASF_USAGE_STRING "\n"
      "Description:\n" ASF_DESCRIPTION_STRING "\n"
      "Required Arguments:\n" ASF_REQUIRED_ARGUMENTS_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " TOOL_SUITE_NAME " " TOOL_SUITE_VERSION_STRING "\n\n");
  exit(EXIT_FAILURE);
}

static void read_gdal(char *inFile, char *outFile)
{
  GDALAllRegister();

	// Open input file
  GDALDatasetH hGdal = GDALOpen(inFile, GA_ReadOnly);
  if (!hGdal)
    asfPrintError("Failed to open file (%s)\n", inFile);
	
	// Determine data type
  GDALRasterBandH hBand = GDALGetRasterBand(hGdal, 1);
  int dataType = GDALGetRasterDataType(hBand);
       
  // Check image dimensions
  int column_count = GDALGetRasterXSize(hGdal);
  int row_count = GDALGetRasterYSize(hGdal);

  // Extract general map projection parameters
  double adfGeoTransform[6];
  GDALGetGeoTransform(hGdal, adfGeoTransform);

	// Write generic binary file
	FILE *fpOut = FOPEN(outFile, "wb");
  if (dataType == GDT_Int16) {
    short *nData = (short *) MALLOC(sizeof(short)*column_count*row_count);
		GDALRasterIO(hBand, GF_Read, 0, 0, column_count, row_count, nData, 
			column_count, row_count, GDT_Int16, 0, 0);
		FWRITE(nData, sizeof(short)*column_count*row_count, 1, fpOut);
		FREE(nData);    
  }
  else 
  	asfPrintError("Wrong data type!\n");
  FCLOSE(fpOut);

	// Write ROI_PAC .rsc file
	char rscFile[512];
	sprintf(rscFile, "%s.rsc", outFile);
	fpOut = FOPEN(rscFile, "w");
	fprintf(fpOut, "WIDTH          %d\n", column_count);
	fprintf(fpOut, "FILE_LENGTH    %d\n", row_count);
	fprintf(fpOut, "X_FIRST        %.12f\n", adfGeoTransform[0]);
	fprintf(fpOut, "Y_FIRST        %.12f\n", adfGeoTransform[3]);
	fprintf(fpOut, "X_STEP         %.12f\n", adfGeoTransform[1]);
	fprintf(fpOut, "Y_STEP         %.12f\n", adfGeoTransform[5]);
	fprintf(fpOut, "Z_SCALE        1\n");
	fprintf(fpOut, "Z_OFFSET       0\n");
	fprintf(fpOut, "X_UNIT         degrees\n");
	fprintf(fpOut, "Y_UNIT         degrees\n");
	fprintf(fpOut, "PROJECTION     LATLON");
	FCLOSE(fpOut);

  GDALClose(hGdal);
  GDALDestroyDriverManager();
}

int is_netcdf(char *inFile) 
{
	int ncid, ret = FALSE;
	int status = nc_open(inFile, NC_NOWRITE, &ncid);
	if (status == NC_NOERR)
		ret = TRUE;
	
	return ret;
}

static void read_netcdf(char *inFile, char *outFile)
{
	int ncid;
	int status = nc_open(inFile, NC_NOWRITE, &ncid);
	if (status != NC_NOERR)
		asfPrintError("Could not open file (%s)!\n", inFile);
	
	// Check out dimensions
	int xDim, yDim;
	size_t xLength, yLength;
	nc_inq_dimid(ncid, "x", &xDim);
	nc_inq_dimlen(ncid, xDim, &xLength);
	nc_inq_dimid(ncid, "y", &yDim);
	nc_inq_dimlen(ncid, yDim, &yLength);
	int column_count = (int) xLength;
	int row_count = (int) yLength;
	
	// Reading longitude
	int varid;
	nc_inq_varid(ncid, "x", &varid);
	float *lon = (float *) MALLOC(sizeof(float)*column_count);
	nc_get_var_float(ncid, varid, &lon[0]);
	float x_first = lon[0];
	
	// Reading latitude
	nc_inq_varid(ncid, "y", &varid);
	float *lat = (float *) MALLOC(sizeof(float)*row_count);
	nc_get_var_float(ncid, varid, &lat[0]);
	float y_first = lat[0];

	// Reading z value
	nc_inq_varid(ncid, "z", &varid);
	float *z = (float *) MALLOC(sizeof(float)*column_count*row_count);
	nc_get_var_float(ncid, varid, &z[0]);
	nc_close(ncid);
	
	// Write generic binary file
	FILE *fpOut = FOPEN(outFile, "wb");
	FWRITE(z, sizeof(float)*column_count*row_count, 1, fpOut);
  FCLOSE(fpOut);
	
	// Calculate rest of metadata
	float x_step = (lon[column_count-1] - lon[0])/(column_count - 1);
	float y_step = (lat[row_count-1] - lat[0])/(row_count - 1);

	// Clean up
	FREE(lat);
	FREE(lon);
	FREE(z);
	
	// Write ROI_PAC .rsc file
	char rscFile[512];
	sprintf(rscFile, "%s.rsc", outFile);
	fpOut = FOPEN(rscFile, "w");
	fprintf(fpOut, "WIDTH          %d\n", column_count);
	fprintf(fpOut, "FILE_LENGTH    %d\n", row_count);
	fprintf(fpOut, "X_FIRST        %.12f\n", x_first);
	fprintf(fpOut, "Y_FIRST        %.12f\n", y_first);
	fprintf(fpOut, "X_STEP         %.12f\n", x_step);
	fprintf(fpOut, "Y_STEP         %.12f\n", y_step);
	fprintf(fpOut, "Z_SCALE        1\n");
	fprintf(fpOut, "Z_OFFSET       0\n");
	fprintf(fpOut, "X_UNIT         degrees\n");
	fprintf(fpOut, "Y_UNIT         degrees\n");
	fprintf(fpOut, "PROJECTION     LATLON");
	FCLOSE(fpOut);
}

int main(int argc, char **argv)
{
  char inFile[512], outFile[512];
  int needed_args=3;

  // Make sure we have the right number of args
  if(argc != needed_args) {
    print_help();
  }

  // Fetch required arguments
  strcpy(inFile, argv[argc-2]);
  strcpy(outFile, argv[argc-1]);

  asfSplashScreen (argc, argv);

	if (is_netcdf(inFile))
		read_netcdf(inFile, outFile);
	else
		read_gdal(inFile, outFile);

  asfPrintStatus("Done.\n\n");

  return(0);
}