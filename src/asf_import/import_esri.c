#include "asf.h"
#include "asf_import.h"
#include "esri.h"


/******************************************************************************
 * Import the ESRI format into our ASF Tools file format */
void import_esri(char *inDataName, char *inMetaName, char *outBaseName,
                 flag_indices_t flags[])
{
  char line[255]="", key[25]="", value[25]="";
  char outDataName[256], outMetaName[256];
  FILE *fp;
  meta_parameters *meta=NULL;
  esri_header *esri=NULL;

  /* Handle output file name */
  strcpy(outDataName,outBaseName);
  strcat(outDataName,TOOLS_IMAGE_EXT);
  strcpy(outMetaName,outBaseName);
  strcat(outMetaName,TOOLS_IMAGE_EXT);

  /* Allocate memory for ESRI header structure */
  esri = (esri_header *)MALLOC(sizeof(esri_header));

  /* Read .hdr and fill meta structures */
  fp = FOPEN(inMetaName, "r");
  while (NULL != fgets(line, 255, fp)) {
    sscanf(line, "%s %s", key, value);
    if (strncmp(key, "NROWS", 5)==0) esri->nrows = atoi(value);
    else if (strncmp(key, "NCOLS", 5)==0) esri->ncols = atoi(value);
    else if (strncmp(key, "NBITS", 5)==0) {
      esri->nbits = atoi(value);
      if (esri->nbits < 8) {
        sprintf(errbuf, "metadata do not support data less than 8 bit");
        print_error(errbuf);
      }
    }
    else if (strncmp(key, "NBANDS", 6)==0) {
      esri->nbands = atoi(value);
      if (esri->nbands > 1) {
        sprintf(errbuf, "metadata do not support multi-band data");
        print_error(errbuf);
      }
    }
    else if (strncmp(key, "BYTEORDER", 9)==0) esri->byteorder = value[0];
    else if (strncmp(key, "LAYOUT", 6)==0) {
      sprintf(esri->layout, "%s", value);
      if (strncmp(uc(esri->layout), "BIL", 3)!=0) {
        sprintf(errbuf, "metadata do not support data other than BIL format");
        print_error(errbuf);
      }
   }
    else if (strncmp(key, "SKIPBYTES", 9)==0) {
      esri->skipbytes = atoi(value);
      if (esri->skipbytes > 0) {
        sprintf(errbuf, "metadata only support generic binary data");
        print_error(errbuf);
      }
    }
    else if (strncmp(key, "ULXMAP", 6)==0) esri->ulxmap = atof(value);
    else if (strncmp(key, "ULYMAP", 6)==0) esri->ulymap = atof(value);
    else if (strncmp(key, "XDIM", 4)==0) esri->xdim = atof(value);
    else if (strncmp(key, "YDIM", 4)==0) esri->ydim = atof(value);
    /* bandrowbytes, totalrowbytes, bandgapdata and nodata currently not used */
  }
  FCLOSE(fp);

  /* Fill metadata structure with valid data */
  meta = esri2meta(esri);

  /* Write metadata file */
  meta_write(meta,outMetaName);

  /* Write data file - currently no header, so just copying generic binary */
  fileCopy(inDataName, outDataName);

  /* Clean and report */
  meta_free(meta);
  sprintf(logbuf, "   Converted ESRI file (%s) to ASF internal file (%s)\n\n",
          inDataName, outDataName);
  if(flags[f_QUIET] == FLAG_NOT_SET) printf(logbuf);
  fLog = FOPEN(logFile, "a");
  printLog(logbuf);
  FCLOSE(fLog);
}
