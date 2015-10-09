#include "asf.h"
#include "asf_meta.h"
#include "terrasar.h"
#include "radarsat2.h"
#include "airsar.h"
#include "envi.h"
#include "xml_util.h"
#include <math.h>
#include <gsl/gsl_spline.h>

int isCEOS(const char *dataFile, char **error)
{
  char **inBandName = NULL, **inMetaName = NULL, tmp[1024], *message = NULL;
  char baseName[512];
  int ret = TRUE, nBands, trailer;
  ceos_metadata_ext_t metadata_ext;
  ceos_data_ext_t     data_ext;

  if (strlen(dataFile) <= 0)
    return FALSE;

  char *dirName = (char *) MALLOC(sizeof(char)*1024);
  char *fileName = (char *) MALLOC(sizeof(char)*1024);
  split_dir_and_file(dataFile, dirName, fileName);
  metadata_ext = get_ceos_metadata_name(dataFile, &inMetaName, &trailer);
  data_ext = get_ceos_data_name(dataFile, baseName, &inBandName, &nBands);
  if (data_ext == NO_CEOS_DATA) {
    message = (char *) MALLOC(sizeof(char)*1024);
    strcpy(message, "");
    sprintf(tmp, "Data file%s of original data (%s) missing.\n",
	    (nBands>1) ? "s" : "", fileName);
    strcat(message, tmp);
    ret = FALSE;
  }    
  if (metadata_ext == NO_CEOS_METADATA) {
    if (!message) {
      message = (char *) MALLOC(sizeof(char)*1024);
      strcpy(message, "");
    }
    sprintf(tmp, "Metadata file of original data (%s) missing.\n", fileName);
    strcat(message, tmp);
    ret = FALSE;
  }

  *error = message;
  FREE(dirName);
  FREE(fileName);
  free_ceos_names(inBandName, inMetaName);
  return ret;
}

static char *get_airsar(char *buf, char *str)
{
  char *p, *q;

  static char value[51];
  memset(value,0,51);

  q = (char *) CALLOC(51,sizeof(char));
  p = strstr(buf, str);
  if (p) {
    strncpy(q, p, 50);
    strcpy(value, q+strlen(str));
  }
  else
    strcpy(value, "");

  FREE(q);

  return value;
}

int isAIRSAR(char *dataFile)
{
  airsar_header *header = NULL;
  FILE *fp;
  int L_airsar = 0;
  int C_airsar = 0;
  int P_airsar = 0;
  char buf[4400], *value, *band_data, *s;
  double version;
  if (is_dir(dataFile))
    return FALSE;
  char *inFile = STRDUP(dataFile);

  // Allocate memory and file handling
  value = (char *) MALLOC(sizeof(char)*25);
  header = (airsar_header *) CALLOC(1, sizeof(airsar_header));
  band_data = (char *) MALLOC(sizeof(char)*(strlen(inFile)+32));
  strcpy(band_data, inFile);

  // Try L-band
  s = strrchr(band_data, '_');
  if (s) {
    *s = '\0';
    strcat(s, "_l.dat");
  }
  //if (fileExists(band_data)) {
    strcpy(buf, "");
    fp = fopen(band_data, "r");
    if (fp != NULL && fgets(buf, 4400, fp) == NULL)
      asfPrintError("Could not read general header\n");
    FCLOSE(fp);
    version = atof(get_airsar(buf, "JPL AIRCRAFT SAR PROCESSOR VERSION"));
    L_airsar = (version > 0.0) ? 1 : 0;
    /*}
  else
  L_airsar = FALSE;*/

  // Try C-band
  s = strrchr(band_data, '_');
  if (s) {
    *s = '\0';
    strcat(s, "_c.dat");
  }
  //if (fileExists(band_data)) {
    strcpy(buf, "");
    fp = fopen(band_data, "r");
    if (fp != NULL && fgets(buf, 4400, fp) == NULL)
      asfPrintError("Could not read general header\n");
    FCLOSE(fp);
    version = atof(get_airsar(buf, "JPL AIRCRAFT SAR PROCESSOR VERSION"));
    C_airsar = (version > 0.0) ? 1 : 0;
    /*}
  else
  C_airsar = FALSE;*/

    // Try P-band
  s = strrchr(band_data, '_');
  if (s) {
    *s = '\0';
    strcat(s, "_p.dat");
  }
  //if (fileExists(band_data)) {
    strcpy(buf, "");
    fp = fopen(band_data, "r");
    if (fp != NULL && fgets(buf, 4400, fp) == NULL)
      asfPrintError("Could not read general header\n");
    FCLOSE(fp);
    version = atof(get_airsar(buf, "JPL AIRCRAFT SAR PROCESSOR VERSION"));
    P_airsar = (version > 0.0) ? 1 : 0;
    /*}
  else
  P_airsar = FALSE;*/

  FREE(value);
  FREE(header);
  FREE(band_data);
  FREE(inFile);

  return (L_airsar || C_airsar || P_airsar);
}

int isTerrasar_ext(char *dataFile, int checkPolarimetry, char **error)
{
  int found = FALSE;
  char *inFile = STRDUP(dataFile);
  // Let's first check for an .xml extension
  char *ext = findExt(inFile);

  // If it has the correct extension, investigate it further
  // Might sound a little harsh but avoids some XML parser warning otherwise.
  if (ext && strcmp_case(ext, ".xml") == 0) {
    char *satellite = NULL;
    char tmp[256], *message = NULL, *path=NULL, *inDataName=NULL;
    char imageDataType[25];
    int ii, numberOfLayers;
    satellite = (char *) MALLOC(sizeof(char)*25);
    FILE *fp;
    fp = fopen(inFile, "r");
    xmlDoc *doc = xmlReadFile(inFile, NULL, 0);
    if (doc) {
      strcpy(satellite, xml_get_string_value(doc, 
        "level1Product.productInfo.missionInfo.mission"));

      // only care about TerraSAR-X data
      if (satellite && 
	  (strncmp_case(satellite, "TSX", 3) == 0 ||
	   strncmp_case(satellite, "TDX", 3) == 0)) {

	found = TRUE;
	if (checkPolarimetry) {
	  strcpy(imageDataType, xml_get_string_value(doc, 
	     "level1Product.productInfo.imageDataInfo.imageDataType"));
	  if (strcmp_case(imageDataType, "COMPLEX") != 0) {
	    if (!message)
	      message = (char *) MALLOC(sizeof(char)*1024);
	    sprintf(tmp, "Wrong data type!\n"
		    "Polarimetric processing requires SLC data!\n");
	    strcat(message, tmp);
	    found = FALSE;
	  }
	}

	// path from the xml (metadata) file
	path = get_dirname(inFile);
	inDataName = (char *) MALLOC(sizeof(char)*(strlen(path)+100));
	numberOfLayers = xml_get_int_value(doc, 
	  "level1Product.productInfo.imageDataInfo.numberOfLayers");

	for (ii=0; ii<numberOfLayers; ii++) {
	  if (strlen(path)>0) {
	    strcpy(inDataName, path);
	    if (inDataName[strlen(inDataName)-1] != '/')
	      strcat(inDataName, "/");
	  }
	  else
	    strcpy(inDataName, "");
	  
	  // check whether data file exists
	  strcat(inDataName, xml_get_string_value(doc, 
	    "level1Product.productComponents.imageData[%d].file.location.path",
	    ii));
	  strcat(inDataName, "/");
	  strcat(inDataName, xml_get_string_value(doc, 
	    "level1Product.productComponents.imageData[%d].file.location."
	    "filename", ii));
	  if (!fileExists(inDataName)) {
	    if (!message)
	      message = (char *) MALLOC(sizeof(char)*1024);
	    sprintf(tmp, "Data file (%s) does not exist!\n", inDataName);
	    strcat(message, tmp);
	    found = FALSE;
	  }	
	}
      }
    }
    fclose(fp);
    xmlFreeDoc(doc);
    xmlCleanupParser();
    if (path)
      FREE(path);
    if (inDataName)
      FREE(inDataName);
    FREE(inFile);
  }

  return found;
}

int isTerrasar(char *dataFile, char **error)
{
  return isTerrasar_ext(dataFile, FALSE, error);
}

int isRadarsat2(char *dataFile, char **error) {
  return isRadarsat2_ext(dataFile, TRUE, error);
}

int isRadarsat2_ext(char *dataFile, int check_data, char **error)
{
  char dataType[25];
  char *message = NULL;
  int found = FALSE;
  char *inFile = MALLOC(sizeof(char)*(strlen(dataFile)+16));
  strcpy(inFile, dataFile);
  // Let's first check for an .xml extension
  char *ext = findExt(inFile);

  // Append extension in case we don't find it
  if (!fileExists(inFile) && ext == NULL) {
    strcat(inFile, ".xml");
    ext = findExt(inFile);
  }

  // If it has the correct extension, investigate it further
  // Might sound a little harsh but avoids some XML parser warning otherwise.
  if (fileExists(inFile) && ext && strcmp_case(ext, ".xml") == 0) {
    int ii, band_count = 0;
    char tmp[256], *path = NULL, *inDataName = NULL;
    char polarizations[25], satellite[25];
    FILE *fp;
    fp = fopen(inFile, "r");
    xmlDoc *doc = xmlReadFile(inFile, NULL, 0);
    if (doc) {
      strncpy_safe(satellite, 
	           xml_get_string_value(doc, "product.sourceAttributes.satellite"), 20);
      
      // only care about Radarsat-2 data
      if (strcmp_case(satellite, "RADARSAT-2") == 0) {
	
	found = TRUE;
	strncpy_safe(dataType, xml_get_string_value(doc, 
          "product.imageAttributes.rasterAttributes.dataType"), 20);
	if (strcmp_case(dataType, "COMPLEX") != 0) {
	  if (!message)
	    message = (char *) MALLOC(sizeof(char)*1024);
	  sprintf(tmp, "Wrong data type!\n"
		  "Polarimetric processing requires SLC data!\n");
	  strcat(message, tmp);
	  if (check_data)
  	  found = FALSE;
	}
	
	// path from the xml (metadata) file
	path = get_dirname(inFile);
	inDataName = (char *) MALLOC(sizeof(char)*(strlen(path)+512));
	strncpy_safe(polarizations, xml_get_string_value(doc, 
	  "product.sourceAttributes.radarParameters.polarizations"),20);
	for (ii=0; ii<strlen(polarizations)-1; ii++)
	  if (polarizations[ii] == ' ')
	  polarizations[ii] = ',';
	if (strstr(polarizations, "HH"))
	  band_count++;
	if (strstr(polarizations, "VV"))
	  band_count++;
	if (strstr(polarizations, "HV"))
	  band_count++;
	if (strstr(polarizations, "VH"))
	  band_count++;
	
	for (ii=0; ii<band_count; ii++) {
	  if (strlen(path)>0) {
	    strcpy(inDataName, path);
	    if (inDataName[strlen(inDataName)-1] != '/')
	      strcat(inDataName, "/");
	  }
	  else
	    strcpy(inDataName, "");
	  strcat(inDataName, xml_get_string_value(doc, 
	    "product.imageAttributes.fullResolutionImageData[%d]", ii));
	  if (!fileExists(inDataName)) {
	    if (!message)
	      message = (char *) MALLOC(sizeof(char)*1024);
	    sprintf(tmp, "Data file (%s) does not exist!\n", inDataName);
	    strcat(message, tmp);
	    if (check_data)
  	    found = FALSE;
	  }
	}
      }
    }
    if (fp) {
      fclose(fp);
      xmlFreeDoc(doc);
      xmlCleanupParser();
    }
  }    
  FREE(inFile);

  if (!found && message)
    *error = message;

  return found;
}

int isUAVSAR(char *dataFile, char **error)
{
  int found = FALSE;
  char *inFile = STRDUP(dataFile);
  // Let's first check for an .ann extension
  char *ext = findExt(inFile);

  // If it has the correct extension, investigate it further
  if (ext && strcmp_case(ext, ".ann") == 0) {

    // We have the most generic annotation file ever, with no indication what
    // kind of data we are supposed to have. It just list everything that was
    // originally produced.
    // The only identifier for even UAVSAR, I could find, was the URL.
    char line[512];
    FILE *fp = fopen(inFile, "r");;
    if (fp) {
      while (fgets(line, 512, fp)) {
        if (strstr(line, "uavsar.jpl.nasa.gov"))
   	  found = TRUE;
      }
      fclose(fp);
    }
  }
  FREE(inFile);

  return found;
}


static char *t3_matrix[9] = {"T11.bin","T12_real.bin","T12_imag.bin",
                             "T13_real.bin","T13_imag.bin","T22.bin",
                             "T23_real.bin","T23_imag.bin","T33.bin"};
static char *t4_matrix[16] = {"T11.bin","T12_real.bin","T12_imag.bin",
                              "T13_real.bin","T13_imag.bin","T14_real.bin",
                              "T14_imag.bin","T22.bin","T23_real.bin",
                              "T23_imag.bin","T24_real.bin","T24_imag.bin",
                              "T33.bin","T34_real.bin","T34_imag.bin",
                              "T44.bin"};
static char *c2_matrix[4] = {"C11.bin","C12_real.bin","C12_imag.bin",
                             "C22.bin"};
static char *c3_matrix[9] = {"C11.bin","C12_real.bin","C12_imag.bin",
                             "C13_real.bin","C13_imag.bin","C22.bin",
                             "C23_real.bin","C23_imag.bin","C33.bin"};
static char *c4_matrix[16] = {"C11.bin","C12_real.bin","C12_imag.bin",
                              "C13_real.bin","C13_imag.bin","C14_real.bin",
                              "C14_imag.bin","C22.bin","C23_real.bin",
                              "C23_imag.bin","C24_real.bin","C24_imag.bin",
                              "C33.bin","C34_real.bin","C34_imag.bin",
                              "C44.bin"};
static char *t3_tif_matrix[9] = {"T11.tif","T12_real.tif","T12_imag.tif",
                             "T13_real.tif","T13_imag.tif","T22.tif",
                             "T23_real.tif","T23_imag.tif","T33.tif"};
static char *t4_tif_matrix[16] = {"T11.tif","T12_real.tif","T12_imag.tif",
                              "T13_real.tif","T13_imag.tif","T14_real.tif",
                              "T14_imag.tif","T22.tif","T23_real.tif",
                              "T23_imag.tif","T24_real.tif","T24_imag.tif",
                              "T33.tif","T34_real.tif","T34_imag.tif",
                              "T44.tif"};
static char *c2_tif_matrix[4] = {"C11.tif","C12_real.tif","C12_imag.tif",
                             "C22.tif"};
static char *c3_tif_matrix[9] = {"C11.tif","C12_real.tif","C12_imag.tif",
                             "C13_real.tif","C13_imag.tif","C22.tif",
                             "C23_real.tif","C23_imag.tif","C33.tif"};
static char *c4_tif_matrix[16] = {"C11.tif","C12_real.tif","C12_imag.tif",
                              "C13_real.tif","C13_imag.tif","C14_real.tif",
                              "C14_imag.tif","C22.tif","C23_real.tif",
                              "C23_imag.tif","C24_real.tif","C24_imag.tif",
                              "C33.tif","C34_real.tif","C34_imag.tif",
                              "C44.tif"};

static char *freeman2_decomposition[2] =
  {"Freeman2_Ground.bin","Freeman2_Vol.bin"};
static char *freeman3_decomposition[3] =
  {"Freeman_Dbl.bin","Freeman_Odd.bin","Freeman_Vol.bin"};
static char *vanZyl3_decomposition[3] =
  {"VanZyl3_Dbl.bin","VanZyl3_Odd.bin","VanZyl3_Vol.bin"};
static char *yamaguchi3_decomposition[3] =
  {"Yamaguchi3_Dbl.bin","Yamaguchi3_Odd.bin","Yamaguchi3_Vol.bin"};
static char *yamaguchi4_decomposition[4] =
  {"Yamaguchi4_Dbl.bin","Yamaguchi4_Hlx.bin","Yamaguchi4_Odd.bin",
   "Yamaguchi4_Vol.bin"};
static char *krogager_decomposition[3] =
  {"Krogager_Kd.bin","Krogager_Kh.bin","Krogager_Ks.bin"};
static char *touzi1_decomposition[3] =
  {"TSVM_alpha_s1.bin","TSVM_alpha_s2.bin","TSVM_alpha_s3.bin"};
static char *touzi2_decomposition[3] =
  {"TSVM_phi_s1.bin","TSVM_phi_s2.bin","TSVM_phi_s3.bin"};
static char *touzi3_decomposition[3] =
  {"TSVM_tau_m1.bin","TSVM_tau_m2.bin","TSVM_tau_m3.bin"};
static char *touzi4_decomposition[3] =
  {"TSVM_psi1.bin","TSVM_psi2.bin","TSVM_psi3.bin"};
static char *touzi5_decomposition[4] =
  {"TSVM_alpha_s.bin","TSVM_phi_s.bin","TSVM_tau_m.bin","TSVM_psi.bin"};


int isGeocoded(const char *dataFile)
{
  char enviName[1024];
  envi_header *envi;
  int ret = FALSE;
  sprintf(enviName, "%s.hdr", dataFile);
  envi = read_envi(enviName);
  if (strcmp_case(envi->projection, "not map projected") != 0)
    ret = TRUE;
  FREE(envi);
  
  return ret;
}

static char *getFileName(char *path, char *file)
{
  char *file_name = (char *) MALLOC(sizeof(char)*1024);
  strcpy(file_name, "");
  if (strlen(path)>0) {
    if (path[strlen(path)-1] != DIR_SEPARATOR)
      sprintf(file_name, "%s%c", path, DIR_SEPARATOR);
    else
      sprintf(file_name, "%s", path);
  }
  else
    sprintf(file_name, "%s", path);
  strcat(file_name, file);
 
  return file_name;
}

static int checkMatrixFile(char *type, char *path, char *file, char *reference,
			   char *matrix, char **error)
{
  char *message = NULL, dirName[1024], fileName[1024];
  int ret = TRUE;
  split_dir_and_file(file, dirName, fileName);
  char *file_name = getFileName(path, reference);
  if (!fileExists(file_name)) {
    message = (char *) MALLOC(sizeof(char)*1024);
    sprintf(message, "%s file (%s) of %s matrix element missing.",
	    type, reference, matrix);
    ret = FALSE;
  }
  FREE(file_name);
  *error = message;
  return ret;
}

int isPolsarproMatrix(char *dataFile, char **matrixType, char **error)
{
  int matrix = FALSE;
  char *path;
  if (!dataFile || strlen(dataFile)<=0)
    return FALSE;
  if (is_dir(dataFile))
    path = STRDUP(dataFile);
  else
    path = get_dirname(dataFile);
  if (strlen(path)<=0) 
    path = g_get_current_dir();
  if (path[strlen(path)-1] == DIR_SEPARATOR)
    path[strlen(path)-1] = '\0';
  char *p = path, *q = path;
  while (q) {
    if ((q = strchr(p, DIR_SEPARATOR)) != NULL)
      p = q+1;
  }
  int ii;
  char *directory = STRDUP(p);
  char *binError = (char *) MALLOC(sizeof(char)*255);
  char *hdrError = (char *) MALLOC(sizeof(char)*255);
  char *tifError = (char *) MALLOC(sizeof(char)*255);
  char headerFile[1024], metaFile[1024];
  // check files for coherency matrix T3
  if (strcmp_case(directory, "T3") == 0) {
    for (ii=0; ii<9; ii++) {
      checkMatrixFile("Data", path, dataFile, t3_tif_matrix[ii], directory, 
                           &tifError);
      checkMatrixFile("Data", path, dataFile, t3_matrix[ii], directory, 
                           &binError);
      sprintf(headerFile, "%s.hdr", t3_matrix[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      checkMatrixFile("Header", path, metaFile, headerFile, directory, 
                           &hdrError);
      if (binError && tifError) {
        *error = binError;
        return FALSE;
      }
      else if (hdrError && tifError) {
        *error = hdrError;
        return FALSE;
      }
    }
    matrix = TRUE;
  }
  // check files for coherency matrix T4
  else if (strcmp_case(directory, "T4") == 0) {
    for (ii=0; ii<16; ii++) {
      if (!checkMatrixFile("Data", path, dataFile, t4_matrix[ii], directory, 
                           error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", t4_matrix[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkMatrixFile("Header", path, metaFile, headerFile, directory, 
                           error))
        return FALSE;
    }
    matrix = TRUE;
  }
  // check files for covariance matrix C2
  else if (strcmp_case(directory, "C2") == 0) {
    for (ii=0; ii<4; ii++) {
      if (!checkMatrixFile("Data", path, dataFile, c2_matrix[ii], directory, 
                           error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", c2_matrix[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkMatrixFile("Header", path, metaFile, headerFile, directory,
                                    error))
        return FALSE;
    }
    matrix = TRUE;
  }
  // check files for covariance matrix C3
  else if (strcmp_case(directory, "C3") == 0) {
    for (ii=0; ii<9; ii++) {
      checkMatrixFile("Data", path, dataFile, c3_tif_matrix[ii], directory, 
                           &tifError);
      checkMatrixFile("Data", path, dataFile, c3_matrix[ii], directory, 
                           &binError);
      sprintf(headerFile, "%s.hdr", c3_matrix[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      checkMatrixFile("Header", path, metaFile, headerFile, directory, 
                           &hdrError);
      if (binError && tifError) {
        *error = binError;
        return FALSE;
      }
      else if (hdrError && tifError) {
        *error = hdrError;
        return FALSE;
      }
    }
    matrix = TRUE;
  }
  // check files for covariance matrix C4
  else if (strcmp_case(directory, "C4") == 0) {
    for (ii=0; ii<16; ii++) {
      if (!checkMatrixFile("Data", path, dataFile, c4_matrix[ii], directory, 
                                    error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", c4_matrix[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkMatrixFile("Header", path, dataFile, headerFile, directory, 
                                    error))
        return FALSE;
    }
    matrix = TRUE;
  }
  else {
    *error = STRDUP("Matrix does not exist.");
  }
  *matrixType = directory;
  if (binError)
    *error = STRDUP(binError);
  else if (hdrError)
    *error = STRDUP(hdrError);
  else if (tifError)
    *error = STRDUP(tifError);
  FREE(binError);
  FREE(hdrError);
  FREE(tifError);

  return matrix;
}

static int checkDecompositionFile(char *type, char *path, char *file, 
            				                         char *reference, char *decomposition, 
             				  				         char **error)
{
  char *message = NULL, dirName[1024], fileName[1024];
  int ret = TRUE;
  split_dir_and_file(file, dirName, fileName);
  char *file_name = getFileName(path, reference);
  if (!fileExists(file) || is_dir(file)) {
    message = (char *) MALLOC(sizeof(char)*1024);
    sprintf(message, "Decomposition file (%s) does not exist.",
      fileName);
    ret = FALSE;
  }
  else if (!fileExists(file_name)) {
    message = (char *) MALLOC(sizeof(char)*1024);
    sprintf(message, "%s file (%s) of %s decomposition missing.",
      type, reference, decomposition);
    ret = FALSE;
  }
  *error = message;

  return ret;
}

int isPolsarproDecomposition(char *dataFile, char **decompositionType,
                                              char **error)
{
  char path[1024], fileName[1024], headerFile[1024], metaFile[1024];
  int decomposition = FALSE;
  if (!dataFile || strlen(dataFile)<=0)
    return FALSE;
  int ii;
  split_dir_and_file(dataFile, path, fileName);
  char *decompositionStr = (char *) MALLOC(sizeof(char)*strlen(dataFile));
  strcpy(decompositionStr, "");
  // check files for Freeman 2 components decomposition
  if (strstr(fileName, "Freeman2_")) {
    strcpy(decompositionStr, "Freeman 2");
    for (ii=0; ii<2; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        freeman2_decomposition[ii],
        decompositionStr,error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", freeman2_decomposition[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkDecompositionFile("Header", path, metaFile, headerFile,
        decompositionStr, error))
        return FALSE;
    }
    decomposition = TRUE;
  }
  // check files for Freeman 3 components decomposition
  else if (strstr(fileName, "Freeman_")) {
    strcpy(decompositionStr, "Freeman 3");
    for (ii=0; ii<3; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        freeman3_decomposition[ii],
        decompositionStr, error))
        return FALSE;
        sprintf(headerFile, "%s.hdr", freeman3_decomposition[ii]);
        sprintf(metaFile, "%s.hdr", dataFile);
        if (!checkDecompositionFile("Header", path, metaFile, headerFile, 
          decompositionStr, error))
          return FALSE;
    }
    decomposition = TRUE;
  }
  // check files for Van Zyl 3 components decomposition
  else if (strstr(fileName, "VanZyl3_")) {
    strcpy(decompositionStr, "Van Zyl 3");
    for (ii=0; ii<3; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
		vanZyl3_decomposition[ii], 
		decompositionStr, error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", vanZyl3_decomposition[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkDecompositionFile("Header", path, metaFile, headerFile,
        decompositionStr, error))
        return FALSE;
    }
    decomposition = TRUE;
  }
  // check files for Yamaguchi 3 components decomposition
  else if (strstr(fileName, "Yamaguchi3_")) {
    strcpy(decompositionStr, "Yamaguchi 3");
    for (ii=0; ii<3; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        yamaguchi3_decomposition[ii],
        decompositionStr, error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", yamaguchi3_decomposition[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkDecompositionFile("Header", path, metaFile, headerFile,
        decompositionStr, error))
        return FALSE;
    }
    decomposition = TRUE;
  }
  // check files for Yamaguchi 4 components decomposition
  else if (strstr(fileName, "Yamaguchi4_")) {
    strcpy(decompositionStr, "Yamaguchi 4");
    for (ii=0; ii<4; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        yamaguchi4_decomposition[ii],
        decompositionStr, error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", yamaguchi4_decomposition[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkDecompositionFile("Header", path, metaFile, headerFile,
        decompositionStr, error))
        return FALSE;
    }
    decomposition = TRUE;
  }
  // check files for Krogager components decomposition
  else if (strstr(fileName, "Krogager_")) {
    strcpy(decompositionStr, "Krogager");
    for (ii=0; ii<3; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        krogager_decomposition[ii], 
        decompositionStr, error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", krogager_decomposition[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkDecompositionFile("Header", path, metaFile, headerFile, 
		decompositionStr, error))
		return FALSE;
	}
	decomposition = TRUE;
  }
  // check files for Touzi Alpha_s decomposition
  else if (strcmp_case(fileName, "TSVM_alpha_s1.bin") == 0 ||
    strcmp_case(fileName, "TSVM_alpha_s2.bin") == 0 ||
    strcmp_case(fileName, "TSVM_alpha_s3.bin") == 0) {
    strcpy(decompositionStr, "Touzi alpha_s");
    for (ii=0; ii<3; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        touzi1_decomposition[ii],
        decompositionStr, error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", touzi1_decomposition[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkDecompositionFile("Header", path, metaFile, headerFile,
        decompositionStr, error))
        return FALSE;
    }
    decomposition = TRUE;
  }
  // check files for Touzi Phi_s decomposition
  else if (strcmp_case(fileName, "TSVM_phi_s1.bin") == 0 ||
    strcmp_case(fileName, "TSVM_phi_s2.bin") == 0 ||
    strcmp_case(fileName, "TSVM_phi_s3.bin") == 0) {
    strcpy(decompositionStr, "Touzi phi_s");
    for (ii=0; ii<3; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        touzi2_decomposition[ii],
        decompositionStr, error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", touzi2_decomposition[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkDecompositionFile("Header", path, metaFile, headerFile,
        decompositionStr, error))
        return FALSE;
    }
    decomposition = TRUE;
  }
  // check files for Touzi Tau_m decomposition
  else if (strcmp_case(fileName, "TSVM_tau_m1.bin") == 0 ||
    strcmp_case(fileName, "TSVM_tau_m2.bin") == 0 ||
    strcmp_case(fileName, "TSVM_tau_m3.bin") == 0) {
    strcpy(decompositionStr, "Touzi tau_m");
    for (ii=0; ii<3; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        touzi3_decomposition[ii],
        decompositionStr, error))
        return FALSE;
        sprintf(headerFile, "%s.hdr", touzi3_decomposition[ii]);
        sprintf(metaFile, "%s.hdr", dataFile);
        if (!checkDecompositionFile("Header", path, metaFile, headerFile,
          decompositionStr, error))
          return FALSE;
    }
    decomposition = TRUE;
  }
  // check files for Touzi Psi decomposition
  else if (strcmp_case(fileName, "TSVM_psi1.bin") == 0 ||
    strcmp_case(fileName, "TSVM_psi2.bin") == 0 ||
	strcmp_case(fileName, "TSVM_psi3.bin") == 0) {
	strcpy(decompositionStr, "Touzi psi");
    for (ii=0; ii<3; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        touzi4_decomposition[ii],
        decompositionStr, error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", touzi4_decomposition[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkDecompositionFile("Header", path, metaFile, headerFile,
        decompositionStr, error))
        return FALSE;
    }
    decomposition = TRUE;
  }
  // check files for Touzi 4 components decomposition
  else if (strstr(fileName, "TSVM_")) {
    strcpy(decompositionStr, "Touzi 4 components");
    for (ii=0; ii<4; ii++) {
      if (!checkDecompositionFile("Data", path, dataFile,
        touzi5_decomposition[ii],
        decompositionStr, error))
        return FALSE;
      sprintf(headerFile, "%s.hdr", touzi5_decomposition[ii]);
      sprintf(metaFile, "%s.hdr", dataFile);
      if (!checkDecompositionFile("Header", path, metaFile, headerFile,
        decompositionStr, error))
        return FALSE;
    }
    decomposition = TRUE;
  }
  else {
    *error = STRDUP("Decomposition file does not exist.");
  }
  *decompositionType = decompositionStr;
  
  return decomposition;
}

int isPolsarproSegmentation(const char *dataFile, char **error)
{
  int segmentation = TRUE;
  char *message = NULL, dirName[1024], fileName[1024], headerFile[1024];
  if (!dataFile || strlen(dataFile)<=0)
    return FALSE;
  split_dir_and_file(dataFile, dirName, fileName);
  sprintf(headerFile, "%s", dataFile);
  if (!fileExists(dataFile)) {
    message = (char *) MALLOC(sizeof(char)*1024);
    sprintf(message, "PolSARPro data file for segmentation missing.");
    segmentation = FALSE;
  }
  else if (!fileExists(headerFile)) {
    message = (char *) MALLOC(sizeof(char)*1024);
    sprintf(message,
      "PolSARPro metadata file (%s.hdr) for segmentation missing.",
      fileName);
    segmentation = FALSE;
  }
  *error = message;
  
  return segmentation;
}

int isPolsarproParameter(char *dataFile, char **error)
{
  int parameter = TRUE;
  char *message = NULL, dirName[1024], fileName[1024], headerFile[1024];
  if (!dataFile || strlen(dataFile)<=0)
    return FALSE;
  split_dir_and_file(dataFile, dirName, fileName);
  sprintf(headerFile, "%s", dataFile);
  if (!fileExists(dataFile)) {
    message = (char *) MALLOC(sizeof(char)*1024);
    sprintf(message, "PolSARPro ata file for parameter missing.");
    parameter = FALSE;
  }
  else if (!fileExists(headerFile)) {
    message = (char *) MALLOC(sizeof(char)*1024);
    sprintf(message, "PolSARPro metadata file (%s.hdr) for parameter missing.",
      fileName);
    parameter = FALSE;
  }
  *error = message;
    
  return parameter;
}
