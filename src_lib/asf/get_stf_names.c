/*
  get_stf_names:

  Functions that match STF files with each other primarily using their
  extensions.
*/

#include "asf.h"
#include "get_stf_names.h"


const char stf_metadata_extensions[][8] = {"",".PAR",".par"};
const char stf_data_extensions[][8]     = {"",    "",    ""};


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
  stf_metadata_ext_t ii;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(stfName, dirName, fileName);

  for (ii=begin; ii<end; ii++) {
    /* Check for suffix style extensions */
    if (stf_metadata_extensions[ii][0] == EXTENSION_SEPARATOR) {
      /* Assume stfName came to the function as a base name */
      sprintf(metaTemp,"%s%s%s",dirName,fileName,stf_metadata_extensions[ii]);
      if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
        fclose(metaFP);
        strcpy(metaName,metaTemp);
        return ii;
      }
      /* Hmmm, didn't work, maybe it's got an extension on there already,
       * nix it and try again */
      split_base_and_ext(fileName, APPENDED_EXTENSION, baseName, ext);
      sprintf(metaTemp,"%s%s%s",dirName,baseName,stf_metadata_extensions[ii]);
      if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
        fclose(metaFP);
        strcpy(metaName,metaTemp);
        return ii;
      }
    }
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
    if (end-begin == 0)
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
   sprintf(logbuf,
           "**************************** ERROR! ****************************\n"
           "*   This program was looking for the STF style metadata file,\n"
           "*   %s\n"
           "*   That file either does not exist or cannot be read.\n"
           "*   Expected metadata file extensions are:\n"
           "*   %s\n"
           "****************************************************************\n",
           stfName, extensionList);
    if (logflag)   {printLog(logbuf);}
    printf(logbuf);
    exit(EXIT_FAILURE);
  }

  /* If we found a file, return its extension type! */
  else
    return ret;
}


/******************************************************************************
 * get_stf_data_name:
 * This one sort of cheats, since currently the STF data file represents the
 * base name. The stf_data_ext_t enum is sort of buggered up since its only two
 * extensions are the same, always return STF_BLANK on success.  */
stf_data_ext_t get_stf_data_name(const char *stfName, char *dataName)
{
  char dirName[256], fileName[256];
  char dataTemp[1024];
  char baseName[256];
  char ext[256];
  FILE *dataFP;
  stf_data_ext_t ii=STF_BLANK;
  stf_metadata_ext_t jj;
  stf_metadata_ext_t metaBegin=NO_STF_METADATA+1, metaEnd=NUM_STF_METADATA_EXTS;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(stfName, dirName, fileName);

  /* First & foremost make sure the stfName isn't a metafile, if it is, prune
   * the extension (so we don't mistakenly open it, and think its a datafile */
  split_base_and_ext(fileName, APPENDED_EXTENSION, baseName, ext);
  for (jj=metaBegin; jj<metaEnd; jj++) {
    if (strcmp(ext,stf_metadata_extensions[jj])==0) {
      strcpy(fileName,baseName);
      break;
    }
  }

  /* Assume the FILENAME is the base name (so just tack the extension on) */
  sprintf(dataTemp,"%s%s%s",dirName,fileName,stf_data_extensions[ii]);
  if ((dataFP=fopen(stfName,"r"))!=NULL) {
    fclose(dataFP);
    strcpy(dataName,dataTemp);
    return ii;
  }
  /* Mokay, the given filename didn't work. Lets try the filename minus any
   * extention it may have had (ie BASENAME).  */
  sprintf(dataTemp,"%s%s%s",dirName,baseName,stf_data_extensions[ii]);
  if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
    fclose(dataFP);
    strcpy(dataName,dataTemp);
    return ii;
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
    sprintf(extensionList,"'%s'",stf_data_extensions[begin++]);
    if (end-begin == 0)
      andFlag=FALSE;
    else
      end--;
    for (ii=begin; ii<end; ii++) {
      sprintf(extensionList,"%s, '%s'",extensionList,
                                     stf_data_extensions[ii]);
    }
    if (andFlag)
      sprintf(extensionList,"%s, and '%s'",extensionList,
                                         stf_data_extensions[ii]);

    /* Report to user & exit */
   sprintf(logbuf,
           "**************************** ERROR! ****************************\n"
           "*   This program was looking for the STF style data file,\n"
           "*   %s\n"
           "*   That file either does not exist or cannot be read.\n"
           "*   Expected data file extensions are:\n"
           "*   %s\n"
           "****************************************************************\n",
           stfName, extensionList);
    if (logflag)   {printLog(logbuf);}
    printf(logbuf);
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

  if (   get_stf_data_name(stfName, dataName)     == STF_BLANK
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
  char extensionList[128];
  int andFlag=TRUE;
  int begin=NO_STF_FILE_PAIR+1, end=NUM_STF_FILE_PAIRS;
  int ii;

  if (   require_stf_data(stfName, dataName)     == STF_BLANK
      && require_stf_metadata(stfName, metaName) == STF_PAR)
    return STF_PAR_PAIR;

  if (   require_stf_data(stfName, dataName)     == STF_BLANK
      && require_stf_metadata(stfName, metaName) == STF_par)
    return STF_par_PAIR;

/****** We should never actually get here. The above code should either ******
 ****** return or exit with an error message, BUT... just in case       ******/

  /* Prepare a very readable list of possible extension pairs */
  sprintf(extensionList,"('%s' '%s')",stf_data_extensions[begin],
                                      stf_metadata_extensions[begin]);
  begin++;
  if (end-begin == 0)
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
  sprintf(logbuf,
          "**************************** ERROR! ****************************\n"
          "*   This program was looking for the STF style SAR files,\n"
          "*   %s and its associated file.\n"
          "*   One or both files either do not exist or cannot be read.\n"
          "*   Expected fileset extensions are:\n"
          "*   %s\n"
          "****************************************************************\n",
          stfName, extensionList);
  if (logflag)   {printLog(logbuf);}
  printf(logbuf);
  exit(EXIT_FAILURE);
}
