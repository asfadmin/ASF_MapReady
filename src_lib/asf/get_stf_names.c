/*
  get_stf_names:

  Functions that match STF files with each other primarily using their
  extensions.
*/

#include "asf.h"
#include "get_stf_names.h"


const char stf_metadata_extensions[][8] = {"",".PAR",".par"};
const char stf_data_extensions[][8]     = {"",    "",    ""};

/*****************************************************************************
 * has_stf_metadata_extension:
 * Returns TRUE if the file exists and has an accepted ASF STF metadata file
 * extension  */
int has_stf_metadata_extension(const char *stfName)
{
  char dirName[256], fileName[256];
  char metaTemp[1024];
  char baseName[256];
  char ext[256];
  FILE *metaFP;
  int begin=NO_STF_METADATA+1, end=NUM_STF_METADATA_EXTS;
  int ii;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(stfName, dirName, fileName);

  for (ii=begin; ii<end; ii++) {
    int strEnd = strlen(stf_metadata_extensions[ii]) - 1;

    /* First check for suffix style extensions */
    if (stf_metadata_extensions[ii][0] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, APPENDED_EXTENSION, baseName, ext);
      sprintf(metaTemp,"%s%s%s",dirName,baseName,stf_metadata_extensions[ii]);
      if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
        fclose(metaFP);
        return TRUE;
      }
    }
    /* Second look for prefix style extensions */
    else if (stf_metadata_extensions[ii][strEnd] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, PREPENDED_EXTENSION, baseName, ext);
      sprintf(metaTemp,"%s%s%s",dirName,stf_metadata_extensions[ii],baseName);
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
 * has_stf_data_extension:
 * Returns TRUE if the file exists and has an accepted ASF STF data file
 * extension
 * **Can't really have this function since the stf data file IS the base name**
 * int has_stf_data_extension(const char *stfName)
 */

/******************************************************************************
 * get_stf_metadata_name:
 * Given the name of a file (potentially with path in front of it), determine
 * if it is a STF metadata file (depending on our accepted STF extensions).
 * If so populate metaName with the appropriate name and return the appropriate
 * ENUM stf_metadata_ext_t value.  */
stf_metadata_ext_t get_stf_metadata_name(const char *stfName, char *metaName)
{
  char dirName[256], fileName[256];
  char metaTemp[1024];
  char baseName[256];
  char ext[256];
  FILE *metaFP;
  int begin=NO_STF_METADATA+1, end=NUM_STF_METADATA_EXTS;
  int ii;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(stfName, dirName, fileName);

  for (ii=begin; ii<end; ii++) {
    /* First check for suffix style extensions */
    if (stf_metadata_extensions[ii][0] == EXTENSION_SEPARATOR) {
      split_base_and_ext(fileName, APPENDED_EXTENSION, baseName, ext);
      sprintf(metaTemp,"%s%s%s",dirName,baseName,stf_metadata_extensions[ii]);
      if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
        fclose(metaFP);
        strcpy(metaName,metaTemp);
        return ii;
      }
    }
/* Keep this in case STF naming pulls prefix extensions on us **
 *  ** Second look for prefix style extensions (you can thank RSI) **
 *  int strEnd = strlen(stf_metadata_extensions[ii]) - 1;
 *  else if (stf_metadata_extensions[ii][strEnd] == EXTENSION_SEPARATOR) {
 *    split_base_and_ext(fileName, PREPENDED_EXTENSION, baseName, ext);
 *    sprintf(metaTemp,"%s%s%s",dirName,stf_metadata_extensions[ii],baseName);
 *    if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
 *      fclose(metaFP);
 *      strcpy(metaName,metaTemp);
 *      return ii;
 *    }
 *  }
 */
  }
  /* If we haven't returned yet there ain't no metadata file */
  return NO_STF_METADATA;
}

/******************************************************************************
 * require_stf_metadata:
 * If get_stf_metadata_name fails to find a file, then exit the program.
 * Otherwise act like get_stf_metadata_name  */
stf_metadata_ext_t require_stf_metadata(const char *stfName, char *metaName)
{
  stf_metadata_ext_t ret = get_stf_metadata_name(stfName, metaName);

  /* If we didn't find anything, report & leave */
  if (ret == NO_STF_METADATA) {
    char extensionList[128];
    int andFlag=TRUE;
    int begin=NO_STF_METADATA+1, end=NUM_STF_METADATA_EXTS;
    int ii;

    /* Prepare a very readable list of possible extensions */
    sprintf(extensionList,"%s",stf_metadata_extensions[begin++]);
    if (end-begin == 1)
      andFlag=FALSE;
    else
      end--;
    for (ii=begin; ii<end; ii++) {
      sprintf(extensionList,"%s, %s",extensionList,
                                     stf_metadata_extensions[ii]);
    }
    if (andFlag)
      sprintf(extensionList,"%s, and %s",extensionList,
                                         stf_metadata_extensions[ii]);

    /* Report to user & exit */
    printf("**************************** ERROR! ****************************\n"
           "*   This program was looking for the STF style metadata file,\n"
           "*   %s\n"
           "*   That file either does not exist or cannot be read.\n"
           "*   Expected metadata file extensions are:\n"
           "*   %s\n"
           "****************************************************************\n",
           stfName, extensionList);
    exit(EXIT_FAILURE);
  }

  /* If we found a file, return its extension type! */
  else
    return ret;
}


/******************************************************************************
 * get_stf_data_name:
 * This one sort of cheats, since currently the STF data file represents the
 * base name. We just copy stfName into dataName if fopen is succesful opening
 * the given STF name. */
stf_data_ext_t get_stf_data_name(const char *stfName, char *dataName)
{
  FILE *dataFP;

  if ((dataFP=fopen(stfName,"r"))!=NULL) {
    fclose(dataFP);
    strcpy(dataName,stfName);
    return NO_STF_DATA+1;
  }
  /* If we haven't returned yet there ain't no data file */
  return NO_STF_DATA;
}

/******************************************************************************
 * require_stf_data:
 * If get_stf_data_name fails to find a file, then exit the program.
 * Otherwise act like get_stf_data_name  */
stf_data_ext_t require_stf_data(const char *stfName, char *dataName)
{
  stf_data_ext_t ret = get_stf_data_name(stfName, dataName);

  /* If we didn't find anything, report & leave */
  if (ret == NO_STF_DATA) {
    char extensionList[128];
    int andFlag=TRUE;
    int begin=NO_STF_DATA+1, end=NUM_STF_DATA_EXTS;
    int ii;

    /* Prepare a very readable list of possible extensions */
    sprintf(extensionList,"%s",stf_data_extensions[begin++]);
    if (end-begin == 1)
      andFlag=FALSE;
    else
      end--;
    for (ii=begin; ii<end; ii++) {
      sprintf(extensionList,"%s, %s",extensionList,
                                     stf_data_extensions[ii]);
    }
    if (andFlag)
      sprintf(extensionList,"%s, and %s",extensionList,
                                         stf_data_extensions[ii]);

    /* Report to user & exit */
    printf("**************************** ERROR! ****************************\n"
           "*   This program was looking for the STF style data file,\n"
           "*   %s\n"
           "*   That file either does not exist or cannot be read.\n"
           "*   Expected data file extensions are:\n"
           "*   %s\n"
           "****************************************************************\n",
           stfName, extensionList);
    exit(EXIT_FAILURE);
  }

  /* If we found a file, return its extension type! */
  else
    return ret;
}


/******************************************************************************
 * get_stf_names:
 * Given the name of a file (potentially with path in front of it), determine
 * if it is one of a STF file pair (depending on our accepted STF file
 * extensions). If so populate dataName & metaName with the appropriate names
 * and return the appropriate ENUM stf_file_pairs_t value.*/
stf_file_pairs_t get_stf_names(const char *stfName, char *dataName,
                                 char *metaName)
{
  if (   get_stf_data_name(stfName, dataName)     == STF_BLANK
      && get_stf_metadata_name(stfName, metaName) == STF_PAR)
    return STF_PAR_PAIR;

  if (   get_stf_data_name(stfName, dataName)     == STF_blank
      && get_stf_metadata_name(stfName, metaName) == STF_par)
    return STF_par_PAIR;

  return NO_STF_FILE_PAIR;
}

/******************************************************************************
 * require_stf_pair:
 * Do as get_stf_names would unless there is no pair in which case exit the
 * program with a failure.  */
stf_file_pairs_t require_stf_pair(const char *stfName, char *dataName,
                                    char *metaName)
{
  stf_file_pairs_t ret = require_stf_pair(stfName, dataName, metaName);

  /* If we didn't find anything, report & leave */
  if (ret == NO_STF_FILE_PAIR) {
    char extensionList[128];
    int andFlag=TRUE;
    int begin=NO_STF_FILE_PAIR+1, end=NUM_STF_FILE_PAIRS;
    int ii;

    /* Prepare a very readable list of possible extension pairs */
    sprintf(extensionList,"('%s' '%s')",stf_data_extensions[begin],
                                        stf_metadata_extensions[begin]);
    begin++;
    if (end-begin == 1)
      andFlag=FALSE;
    else
      end--;
    for (ii=begin; ii<end; ii++) {
      sprintf(extensionList,"%s, ('%s' '%s')",extensionList,
                                              stf_data_extensions[ii],
                                              stf_metadata_extensions[ii]);
    }
    if (andFlag)
      sprintf(extensionList,"%s, and ('%s' '%s')",extensionList,
                                                  stf_data_extensions[ii],
                                                  stf_metadata_extensions[ii]);

    /* Report to user & exit */
    printf("**************************** ERROR! ****************************\n"
           "*   This program was looking for the STF style SAR files,\n"
           "*   %s and its associated file.\n"
           "*   One or both files either do not exist or cannot be read.\n"
           "*   Expected fileset extensions are:\n"
           "*   %s\n"
           "****************************************************************\n",
           stfName, extensionList);
    exit(EXIT_FAILURE);
  }

  /* If we found a file, return its extension type! */
  else
    return ret;
}
