/*
  get_ceos_names:

  Functions that match CEOS files with each other primarily using their
  extensions.
*/

#include "asf.h"
#include "get_ceos_names.h"


const char ceos_metadata_extensions[][8] = {"",".L",".LDR","lea."};
const char ceos_data_extensions[][8]     = {"",".D",".RAW","dat."};

/*****************************************************************************
 * has_ceos_metadata_extension:
 * Returns TRUE if the file exists and has an accepted ASF CEOS metadata file
 * extension  */
int has_ceos_metadata_extension(const char *ceosName)
{
  char dirName[256], fileName[256];
  char metaTemp[1024];
  char baseName[256];
  char ext[256];
  FILE *metaFP;
  int begin=NO_CEOS_METADATA+1, end=NUM_CEOS_METADATA_EXTS;
  int ii;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(ceosName, dirName, fileName);

  for (ii=begin; ii<end; ii++) {
    int strEnd = strlen(ceos_metadata_extensions[ii]) - 1;

    /* First check for suffix style extensions */
    if (ceos_metadata_extensions[ii][0] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, APPENDED_EXTENSION, baseName, ext);
      sprintf(metaTemp,"%s%s%s",dirName,baseName,ceos_metadata_extensions[ii]);
      if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
        fclose(metaFP);
        return TRUE;
      }
    }
    /* Second look for prefix style extensions (you can thank RSI) */
    else if (ceos_metadata_extensions[ii][strEnd] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, PREPENDED_EXTENSION, baseName, ext);
      sprintf(metaTemp,"%s%s%s",dirName,ceos_metadata_extensions[ii],baseName);
      if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
        fclose(metaFP);
        return TRUE;
      }
    }
  }
  /* If we haven't returned yet there ain't no metadata file */
  return FALSE;
}


/*****************************************************************************
 * has_ceos_data_extension:
 * Returns TRUE if the file exists and has an accepted ASF CEOS data file
 * extension  */
int has_ceos_data_extension(const char *ceosName)
{
  char dirName[256], fileName[256];
  char dataTemp[1024];
  char baseName[256];
  char ext[256];
  FILE *dataFP;
  int begin=NO_CEOS_DATA+1, end=NUM_CEOS_DATA_EXTS;
  int ii;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(ceosName, dirName, fileName);

  for (ii=begin; ii<end; ii++) {
    int strEnd = strlen(ceos_data_extensions[ii]) - 1;

    /* First check for suffix style extensions */
    if (ceos_data_extensions[ii][0] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, APPENDED_EXTENSION, baseName, ext);
      sprintf(dataTemp,"%s%s%s",dirName,baseName,ceos_data_extensions[ii]);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        return TRUE;
      }
    }
    /* Second look for prefix style extensions (you can thank RSI) */
    else if (ceos_data_extensions[ii][strEnd] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, PREPENDED_EXTENSION, baseName, ext);
      sprintf(dataTemp,"%s%s%s",dirName,ceos_data_extensions[ii],baseName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        return TRUE;
      }
    }
  }
  /* If we haven't returned yet there ain't no data file */
  return FALSE;
}


/******************************************************************************
 * get_ceos_metadata_name:
 * Given the name of a file (potentially with path in front of it), determine
 * if it is a CEOS metadata file (depending on our accepted CEOS extensions).
 * If so populate metaName with the appropriate name and return the appropriate
 * ENUM ceos_metadata_ext_t value.  */
ceos_metadata_ext_t get_ceos_metadata_name(const char *ceosName, char *metaName)
{
  char dirName[256], fileName[256];
  char metaTemp[1024];
  char baseName[256];
  char ext[256];
  FILE *metaFP;
  int begin=NO_CEOS_METADATA+1, end=NUM_CEOS_METADATA_EXTS;
  int ii;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(ceosName, dirName, fileName);

  for (ii=begin; ii<end; ii++) {
    int strEnd = strlen(ceos_metadata_extensions[ii]) - 1;

    /* First check for suffix style extensions */
    if (ceos_metadata_extensions[ii][0] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, APPENDED_EXTENSION, baseName, ext);
      sprintf(metaTemp,"%s%s%s",dirName,baseName,ceos_metadata_extensions[ii]);
      if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
        fclose(metaFP);
        strcpy(metaName,metaTemp);
        return ii;
      }
    }
    /* Second look for prefix style extensions (you can thank RSI) */
    else if (ceos_metadata_extensions[ii][strEnd] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, PREPENDED_EXTENSION, baseName, ext);
      sprintf(metaTemp,"%s%s%s",dirName,ceos_metadata_extensions[ii],baseName);
      if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
        fclose(metaFP);
        strcpy(metaName,metaTemp);
        return ii;
      }
    }
  }
  /* If we haven't returned yet there ain't no metadata file */
  return NO_CEOS_METADATA;
}

/******************************************************************************
 * require_ceos_metadata:
 * If get_ceos_metadata_name fails to find a file, then exit the program.
 * Otherwise act like get_ceos_metadata_name  */
ceos_metadata_ext_t require_ceos_metadata(const char *ceosName, char *metaName)
{
  ceos_metadata_ext_t ret = get_ceos_metadata_name(ceosName, metaName);

  /* If we didn't find anything, report & leave */
  if (ret == NO_CEOS_METADATA) {
    char extensionList[128];
    int andFlag=TRUE;
    int begin=NO_CEOS_METADATA+1, end=NUM_CEOS_METADATA_EXTS;
    int ii;

    /* Prepare a very readable list of possible extensions */
    sprintf(extensionList,"%s",ceos_metadata_extensions[begin++]);
    if (end-begin == 1)
      andFlag=FALSE;
    else
      end--;
    for (ii=begin; ii<end; ii++) {
      sprintf(extensionList,"%s, %s",extensionList,
                                     ceos_metadata_extensions[ii]);
    }
    if (andFlag)
      sprintf(extensionList,"%s, and %s",extensionList,
                                         ceos_metadata_extensions[ii]);

    /* Report to user & exit */
    printf("**************************** ERROR! ****************************\n"
           "*   This program was looking for the CEOS style metadata file,\n"
           "*   %s\n"
           "*   That file either does not exist or cannot be read.\n"
           "*   Expected metadata file extensions are:\n"
           "*   %s\n"
           "****************************************************************\n",
           ceosName, extensionList);
    exit(EXIT_FAILURE);
  }

  /* If we found a file, return its extension type! */
  else
    return ret;
}


/******************************************************************************
 * get_ceos_data_name:
 * Given the name of a file (potentially with path in front of it), determine
 * if it is a CEOS data file (depending on our accepted CEOS extensions). If
 * so populate dataName with the appropriate name and return the appropriate
 * ENUM ceos_data_ext_t value.  */
ceos_data_ext_t get_ceos_data_name(const char *ceosName, char *dataName)
{
  char dirName[256], fileName[256];
  char dataTemp[1024];
  char baseName[256];
  char ext[256];
  FILE *dataFP;
  int begin=NO_CEOS_DATA+1, end=NUM_CEOS_DATA_EXTS;
  int ii;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(ceosName, dirName, fileName);

  for (ii=begin; ii<end; ii++) {
    int strEnd = strlen(ceos_data_extensions[ii]) - 1;

    /* First check for suffix style extensions */
    if (ceos_data_extensions[ii][0] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, APPENDED_EXTENSION, baseName, ext);
      sprintf(dataTemp,"%s%s%s",dirName,baseName,ceos_data_extensions[ii]);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName,dataTemp);
        return ii;
      }
    }
    /* Second look for prefix style extensions (you can thank RSI) */
    else if (ceos_data_extensions[ii][strEnd] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, PREPENDED_EXTENSION, baseName, ext);
      sprintf(dataTemp,"%s%s%s",dirName,ceos_data_extensions[ii],baseName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName,dataTemp);
        return ii;
      }
    }
  }
  /* If we haven't returned yet there ain't no data file */
  return NO_CEOS_DATA;
}

/******************************************************************************
 * require_ceos_data:
 * If get_ceos_data_name fails to find a file, then exit the program.
 * Otherwise act like get_ceos_data_name  */
ceos_data_ext_t require_ceos_data(const char *ceosName, char *dataName)
{
  ceos_data_ext_t ret = get_ceos_data_name(ceosName, dataName);

  /* If we didn't find anything, report & leave */
  if (ret == NO_CEOS_DATA) {
    char extensionList[128];
    int andFlag=TRUE;
    int begin=NO_CEOS_DATA+1, end=NUM_CEOS_DATA_EXTS;
    int ii;

    /* Prepare a very readable list of possible extensions */
    sprintf(extensionList,"%s",ceos_data_extensions[begin++]);
    if (end-begin == 1)
      andFlag=FALSE;
    else
      end--;
    for (ii=begin; ii<end; ii++) {
      sprintf(extensionList,"%s, %s",extensionList,
                                     ceos_data_extensions[ii]);
    }
    if (andFlag)
      sprintf(extensionList,"%s, and %s",extensionList,
                                         ceos_data_extensions[ii]);

    /* Report to user & exit */
    printf("**************************** ERROR! ****************************\n"
           "*   This program was looking for the CEOS style data file,\n"
           "*   %s\n"
           "*   That file either does not exist or cannot be read.\n"
           "*   Expected data file extensions are:\n"
           "*   %s\n"
           "****************************************************************\n",
           ceosName, extensionList);
    exit(EXIT_FAILURE);
  }

  /* If we found a file, return its extension type! */
  else
    return ret;
}


/******************************************************************************
 * get_ceos_names:
 * Given the name of a file (potentially with path in front of it), determine
 * if it is one of a CEOS file pair (depending on our accepted CEOS file
 * extensions). If so populate dataName & metaName with the appropriate names
 * and return the appropriate ENUM ceos_file_pairs_t value.*/
ceos_file_pairs_t get_ceos_names(const char *ceosName, char *dataName,
                                 char *metaName)
{

  if (   get_ceos_data_name(ceosName, dataName)     == CEOS_D
      && get_ceos_metadata_name(ceosName, metaName) == CEOS_L)
    return CEOS_D_L_PAIR;

  if (   get_ceos_data_name(ceosName, dataName)     == CEOS_RAW
      && get_ceos_metadata_name(ceosName, metaName) == CEOS_LDR)
    return CEOS_RAW_LDR_PAIR;

  if (   get_ceos_data_name(ceosName, dataName)     == CEOS_dat
      && get_ceos_metadata_name(ceosName, metaName) == CEOS_lea)
    return CEOS_dat_lea_PAIR;

  return NO_CEOS_FILE_PAIR;
}

/******************************************************************************
 * require_ceos_pair:
 * Do as get_ceos_names would unless there is no pair in which case exit the
 * program with a failure.  */
ceos_file_pairs_t require_ceos_pair(const char *ceosName, char *dataName,
                                    char *metaName)
{
  ceos_file_pairs_t ret = require_ceos_pair(ceosName, dataName, metaName);

  /* If we didn't find anything, report & leave */
  if (ret == NO_CEOS_FILE_PAIR) {
    char extensionList[128];
    int andFlag=TRUE;
    int begin=NO_CEOS_FILE_PAIR+1, end=NUM_CEOS_FILE_PAIRS;
    int ii;

    /* Prepare a very readable list of possible extension pairs */
    sprintf(extensionList,"(%s %s)",ceos_data_extensions[begin],
                                    ceos_metadata_extensions[begin]);
    begin++;
    if (end-begin == 1)
      andFlag=FALSE;
    else
      end--;
    for (ii=begin; ii<end; ii++) {
      sprintf(extensionList,"%s, (%s %s)",extensionList,
                                          ceos_data_extensions[ii],
                                          ceos_metadata_extensions[ii]);
    }
    if (andFlag)
      sprintf(extensionList,"%s, and (%s %s)",extensionList,
                                              ceos_data_extensions[ii],
                                              ceos_metadata_extensions[ii]);

    /* Report to user & exit */
    printf("**************************** ERROR! ****************************\n"
           "*   This program was looking for the CEOS style SAR files,\n"
           "*   %s and its associated file.\n"
           "*   One or both files either do not exist or cannot be read.\n"
           "*   Expected fileset extensions are:\n"
           "*   %s\n"
           "****************************************************************\n",
           ceosName, extensionList);
    exit(EXIT_FAILURE);
  }

  /* If we found a file, return its extension type! */
  else
    return ret;
}
