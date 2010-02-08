/*
  get_ceos_names:

  Functions that match CEOS files with each other primarily using their
  extensions.
*/

#include "asf.h"
#include "asf_meta.h"
#include "get_ceos_names.h"
#include "meta_init.h"

const char ceos_metadata_extensions[][12] =
{
    "",
    "LEA_ TRA_",
    "lea_ tra_",
    ".sarl .sart",
    ".L",
    ".LDR",
    ".ldr",
    "LEA.",
    "lea.",
    "lea_",
    "LED-",
    "LEA_"
};
const char ceos_data_extensions[][12] =
{
    "",
    "DAT_",
    "dat_",
    ".sard",
    ".D",
    ".RAW",
    ".raw",
    "DAT.",
    "dat.",
    "dat_",
    "IMG-",
    "IMG_",
    ".dat"
};

static ceos_metadata_ext_t free_and_return(char *leaderExt,
                                           char *trailerExt,
                                           int ret)
{
  FREE (leaderExt);
  FREE (trailerExt);
  return ret;
}

/******************************************************************************
 * get_ceos_metadata_name:
 * Given the name of a file (potentially with path in front of it), determine
 * if it is a CEOS metadata file (depending on our accepted CEOS extensions).
 * If so populate metaName with the appropriate name and return the appropriate
 * ENUM ceos_metadata_ext_t value.  */
ceos_metadata_ext_t get_ceos_metadata_name(const char *ceosName,
                                           char ***pMetaName,
                                           int *trailerFlag)
{
  char dirName[1024], fileName[1024], leaderTemp[1024], trailerTemp[1024];
  char baseName[1024], ext[256], *leaderExt, *trailerExt, **metaName;
  FILE *leaderFP, *trailerFP;
  int begin=NO_CEOS_METADATA+1, end=NUM_CEOS_METADATA_EXTS;
  int ii, index;

  leaderExt = (char *) MALLOC(sizeof(char)*25);
  trailerExt = (char *) MALLOC(sizeof(char)*25);
  metaName = (char **) MALLOC(2*sizeof(char *));
  for (ii=0; ii<2; ii++)
    metaName[ii] = (char *) MALLOC(sizeof(char)*512);

  *pMetaName = metaName;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(ceosName, dirName, fileName);

  for (ii=begin; ii<end; ii++) {

    char *trailerPtr = strchr(ceos_metadata_extensions[ii], ' ');

    if (trailerPtr) {
      index = trailerPtr - ceos_metadata_extensions[ii];
      strncpy(leaderExt, ceos_metadata_extensions[ii], index);
      leaderExt[index] = '\0';
      trailerPtr++;
      strcpy(trailerExt, trailerPtr);
      *trailerFlag = 1;
    }
    else {
      strcpy(leaderExt, ceos_metadata_extensions[ii]);
      strcpy(trailerExt, "");
      *trailerFlag = 0;
    }

    int strEnd = strlen(leaderExt) - 1;

    /* First, check for suffix style extensions */
    if (leaderExt[0] == EXTENSION_SEPARATOR) {
      /* Assume ceosName came to the function as a base name */
      if (*trailerFlag) {
        sprintf(leaderTemp, "%s%s%s", dirName, fileName, leaderExt);
        sprintf(trailerTemp, "%s%s%s", dirName, fileName, trailerExt);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL &&
            (trailerFP = fopen(trailerTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          fclose(trailerFP);
          strcpy(metaName[1], trailerTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      else {
        sprintf(leaderTemp, "%s%s%s", dirName, fileName, leaderExt);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      /* Hmmm, didn't work, maybe it's got an extension on there already,
       * nix it and try again */
      split_base_and_ext(fileName, APPENDED_EXTENSION, '.', baseName, ext);
      if (*trailerFlag) {
        sprintf(leaderTemp, "%s%s%s", dirName, baseName, leaderExt);
        sprintf(trailerTemp, "%s%s%s", dirName, baseName, trailerExt);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL &&
            (trailerFP = fopen(trailerTemp, "r"))!= NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          fclose(trailerFP);
          strcpy(metaName[1], trailerTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      else {
        sprintf(leaderTemp, "%s%s%s", dirName, baseName, leaderExt);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
    }

    /* Second, look for prefix style extensions '.' */
    else if (leaderExt[strEnd] == EXTENSION_SEPARATOR) {
      /* Assume ceosName came to the function as a base name */
      if (*trailerFlag) {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, fileName);
        sprintf(trailerTemp, "%s%s%s", dirName, trailerExt, fileName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL &&
            (trailerFP = fopen(trailerTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          fclose(trailerFP);
          strcpy(metaName[1], trailerTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      else {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, fileName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      /* Hmmm, didn't work, maybe it's got an extension on there already,
       * nix it and try again */
      split_base_and_ext(fileName, PREPENDED_EXTENSION, '.', baseName, ext);
      if (*trailerFlag) {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, baseName);
        sprintf(trailerTemp, "%s%s%s", dirName, trailerExt, baseName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL &&
            (trailerFP = fopen(trailerTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          fclose(trailerFP);
          strcpy(metaName[1], trailerTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      else {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, baseName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
    }

    /* Third, look for ALOS prefix style extensions '-' */
    else if (leaderExt[strEnd] == ALOS_EXTENSION_SEPARATOR) {
      /* Assume ceosName came to the function as a base name */
      if (*trailerFlag) {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, fileName);
        sprintf(trailerTemp, "%s%s%s", dirName, trailerExt, fileName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL &&
            (trailerFP = fopen(trailerTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          fclose(trailerFP);
          strcpy(metaName[1], trailerTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      else {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, fileName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      /* Hmmm, didn't work, maybe it's got an extension on there already,
       * nix it and try again */
      split_base_and_ext(fileName, PREPENDED_EXTENSION, '-', baseName, ext);
      if (baseName[2] == '-') {
        char tmp[1024];
        strcpy(tmp, baseName);
        split_base_and_ext(tmp, PREPENDED_EXTENSION, '-', baseName, ext);
      }
      if (*trailerFlag) {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, baseName);
        sprintf(trailerTemp, "%s%s%s", dirName, trailerExt, baseName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL &&
            (trailerFP = fopen(trailerTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          fclose(trailerFP);
          strcpy(metaName[0], trailerTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      else {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, baseName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
    }

    /* Fourth, look for Canadian prefix style extensions '_' */
    else if (leaderExt[strEnd] == CAN_EXTENSION_SEPARATOR) {
      /* Assume ceosName came to the function as a base name */
      if (trailerFlag) {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, fileName);
        sprintf(trailerTemp, "%s%s%s", dirName, trailerExt, fileName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL &&
            (trailerFP = fopen(trailerTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          fclose(trailerFP);
          strcpy(metaName[1], trailerTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      else {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, fileName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      /* Hmmm, didn't work, maybe it's got an extension on there already,
       * nix it and try again */
      split_base_and_ext(fileName, PREPENDED_EXTENSION, '_', baseName, ext);
      if (*trailerFlag) {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, baseName);
        sprintf(trailerTemp, "%s%s%s", dirName, trailerExt, baseName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL &&
            (trailerFP = fopen(trailerTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          fclose(trailerFP);
          strcpy(metaName[1], trailerTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
      else {
        sprintf(leaderTemp, "%s%s%s", dirName, leaderExt, baseName);
        if ((leaderFP = fopen(leaderTemp, "r"))!=NULL) {
          fclose(leaderFP);
          strcpy(metaName[0], leaderTemp);
          return free_and_return (leaderExt, trailerExt, ii);
        }
      }
    }
  }
  /* If we haven't returned yet there ain't no metadata file */
  return free_and_return (leaderExt, trailerExt, NO_CEOS_METADATA);
}

/******************************************************************************
 * require_ceos_metadata:
 * If get_ceos_metadata_name fails to find a file, then exit the program.
 * Otherwise act like get_ceos_metadata_name  */
ceos_metadata_ext_t require_ceos_metadata(const char *ceosName, char ***metaName,
            int *trailerFlag)
{
  ceos_metadata_ext_t ret =
    get_ceos_metadata_name(ceosName, metaName, trailerFlag);

  /* If we didn't find anything, report & leave */
  if (ret == NO_CEOS_METADATA) {
    char extensionList[128];
    int andFlag=TRUE;
    int begin=NO_CEOS_METADATA+1, end=NUM_CEOS_METADATA_EXTS;
    int ii;

    /* Prepare a very readable list of possible extensions */
    sprintf(extensionList,"%s",ceos_metadata_extensions[begin++]);
    if (end-begin == 0)
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
    sprintf(logbuf,
            "**************************** ERROR! ****************************\n"
            "*   This program was looking for the CEOS style metadata file,\n"
            "*   %s\n"
            "*   That file either does not exist or cannot be read.\n"
            "*   Expected metadata file extensions are:\n"
            "*   %s\n"
            "****************************************************************\n",
            ceosName, extensionList);
    if (logflag)   {printLog(logbuf);}
    printf(logbuf);
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
ceos_data_ext_t get_ceos_data_name(const char *ceosName, char *baseName,
           char ***pDataName, int *nBands)
{
  char dirName[1024], fileName[1024], dataTemp[1024], ext[256];
  char **dataName;
  FILE *dataFP;
  int begin=NO_CEOS_DATA+1, end=NUM_CEOS_DATA_EXTS;
  int ii, kk;

  dataName = (char **) MALLOC(MAX_BANDS*sizeof(char *));
  for (ii=0; ii<MAX_BANDS; ii++)
    dataName[ii] = (char *) MALLOC(sizeof(char)*512);

  *pDataName = dataName;

  *nBands = 0;

  /* Separate the filename from the path (if there's a path there) */
  split_dir_and_file(ceosName, dirName, fileName);

  for (ii=begin; ii<end; ii++) {
    int strEnd = strlen(ceos_data_extensions[ii]) - 1;

    /* First check for suffix style extensions */
    if (ceos_data_extensions[ii][0] == EXTENSION_SEPARATOR) {
      /* Assume ceosName came to the function as a base name */
      sprintf(dataTemp,"%s%s%s",dirName,fileName,ceos_data_extensions[ii]);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[0],dataTemp);
  strcpy(baseName, fileName);
  *nBands = 1;
        return ii;
      }
      /* Hmmm, didn't work, maybe it's got an extension on there already,
       * nix it and try again */
      split_base_and_ext(fileName, APPENDED_EXTENSION, '.', baseName, ext);
      sprintf(dataTemp,"%s%s%s",dirName,baseName,ceos_data_extensions[ii]);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[0],dataTemp);
  *nBands = 1;
        return ii;
      }
    }
    /* Second look for prefix style extensions '.' */
    else if (ceos_data_extensions[ii][strEnd] == EXTENSION_SEPARATOR) {
      /* Assume ceosName came to the function as a base name */
      sprintf(dataTemp,"%s%s%s",dirName,ceos_data_extensions[ii],fileName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[0],dataTemp);
  strcpy(baseName, fileName);
  *nBands = 1;
        return ii;
      }
      /* Hmmm, didn't work, maybe it's got an extension on there already,
       * nix it and try again */
      split_base_and_ext(fileName, PREPENDED_EXTENSION, '.', baseName, ext);
      sprintf(dataTemp,"%s%s%s",dirName,ceos_data_extensions[ii],baseName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[0],dataTemp);
  *nBands = 1;
        return ii;
      }
    }
    /* Third look for prefix style extensions '-' */
    else if (ceos_data_extensions[ii][strEnd] == ALOS_EXTENSION_SEPARATOR) {
      /* Assume ceosName came to the function as a base name */
      sprintf(dataTemp,"%s%sHH-%s",dirName,ceos_data_extensions[ii],fileName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
  strcpy(baseName, fileName);
        (*nBands)++;
      }
      sprintf(dataTemp,"%s%sHV-%s",dirName,ceos_data_extensions[ii],fileName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
  strcpy(baseName, fileName);
        (*nBands)++;
      }
      sprintf(dataTemp,"%s%sVH-%s",dirName,ceos_data_extensions[ii],fileName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
  strcpy(baseName, fileName);
        (*nBands)++;
      }
      sprintf(dataTemp,"%s%sVV-%s",dirName,ceos_data_extensions[ii],fileName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
  strcpy(baseName, fileName);
        (*nBands)++;
      }
      for (kk=1; kk<10; kk++) {
  sprintf(dataTemp,"%s%s0%d-%s",dirName,ceos_data_extensions[ii],kk,fileName);
  if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
    fclose(dataFP);
    strcpy(dataName[*nBands],dataTemp);
    strcpy(baseName, fileName);
    (*nBands)++;
  }
      }
      sprintf(dataTemp,"%s%s%s",dirName,ceos_data_extensions[ii],fileName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
  strcpy(baseName, fileName);
        (*nBands)++;
      }
      if (*nBands)
  return ii;
      /* Hmmm, didn't work, maybe it's got an extension on there already,
       * nix it and try again */
      split_base_and_ext(fileName, PREPENDED_EXTENSION, '-', baseName, ext);
      if (baseName[2] == '-') {
  char tmp[1024];
  strcpy(tmp, baseName);
  split_base_and_ext(tmp, PREPENDED_EXTENSION, '-', baseName, ext);
      }
      sprintf(dataTemp,"%s%sHH-%s",dirName,ceos_data_extensions[ii],baseName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
        (*nBands)++;
      }
      sprintf(dataTemp,"%s%sHV-%s",dirName,ceos_data_extensions[ii],baseName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
        (*nBands)++;
      }
      sprintf(dataTemp,"%s%sVH-%s",dirName,ceos_data_extensions[ii],baseName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
        (*nBands)++;
      }
      sprintf(dataTemp,"%s%sVV-%s",dirName,ceos_data_extensions[ii],baseName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
        (*nBands)++;
      }
      for (kk=1; kk<10; kk++) {
  sprintf(dataTemp,"%s%s0%d-%s",dirName,ceos_data_extensions[ii],kk,baseName);
  if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
    fclose(dataFP);
    strcpy(dataName[*nBands],dataTemp);
    (*nBands)++;
  }
      }
      sprintf(dataTemp,"%s%s%s",dirName,ceos_data_extensions[ii],baseName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[*nBands],dataTemp);
        (*nBands)++;
      }
      if (*nBands)
  return ii;
    }
    /* Fourth look for prefix style extensions '_' */
    else if (ceos_data_extensions[ii][strEnd] == CAN_EXTENSION_SEPARATOR) {
      /* Assume ceosName came to the function as a base name */
      sprintf(dataTemp,"%s%s%s",dirName,ceos_data_extensions[ii],fileName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[0],dataTemp);
  strcpy(baseName, fileName);
        *nBands = 1;
        return ii;
      }
      /* Hmmm, didn't work, maybe it's got an extension on there already,
       * nix it and try again */
      split_base_and_ext(fileName, PREPENDED_EXTENSION, '_', baseName, ext);
      sprintf(dataTemp,"%s%s%s",dirName,ceos_data_extensions[ii],baseName);
      if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
        fclose(dataFP);
        strcpy(dataName[0],dataTemp);
        *nBands = 1;
        return ii;
      }
    }
 }
  /* If we haven't returned yet there ain't no data file */
  for (ii=0; ii<MAX_BANDS; ii++)
    FREE(dataName[ii]);
  FREE(dataName);
  dataName = NULL;
  *pDataName = NULL;
  return NO_CEOS_DATA;
}

/******************************************************************************
 * require_ceos_data:
 * If get_ceos_data_name fails to find a file, then exit the program.
 * Otherwise act like get_ceos_data_name  */
ceos_data_ext_t require_ceos_data(const char *ceosName,char ***dataName,
          int *nBands)
{
  char baseName[1024];

  ceos_data_ext_t ret =
    get_ceos_data_name(ceosName, baseName, dataName, nBands);

  /* If we didn't find anything, report & leave */
  if (ret == NO_CEOS_DATA) {
    char extensionList[128];
    int andFlag=TRUE;
    int begin=NO_CEOS_DATA+1, end=NUM_CEOS_DATA_EXTS;
    int ii;

    /* Prepare a very readable list of possible extensions */
    sprintf(extensionList,"%s",ceos_data_extensions[begin++]);
    if (end-begin == 0)
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
    sprintf(logbuf,
           "**************************** ERROR! ****************************\n"
           "*   This program was looking for the CEOS style data file,\n"
           "*   %s\n"
           "*   That file either does not exist or cannot be read.\n"
           "*   Expected data file extensions are:\n"
           "*   %s\n"
           "****************************************************************\n",
           ceosName, extensionList);
    if (logflag)   {printLog(logbuf);}
    printf(logbuf);
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
ceos_file_pairs_t get_ceos_names(const char *ceosName, char *baseName,
         char ***dataName, char ***metaName,
         int *nBands, int *trailerFlag)
{
  ceos_metadata_ext_t metadata_ext;
  ceos_data_ext_t     data_ext;

  metadata_ext = get_ceos_metadata_name(ceosName, metaName, trailerFlag);
  data_ext = get_ceos_data_name(ceosName, baseName, dataName, nBands);

  if ((data_ext == CEOS_DAT || data_ext == CEOS_DAT_) && metadata_ext == CEOS_LEA_TRA)
    return CEOS_DAT_LEA_TRA_TRIPLE;

  if (data_ext == CEOS_dat_ && metadata_ext == CEOS_lea_tra)
    return CEOS_dat_lea_tra_TRIPLE;

  if (data_ext == CEOS_dat_t && metadata_ext == CEOS_lea_tra)
    return CEOS_dat_lea_tra_TRIPLE;

  if (data_ext == CEOS_sard && metadata_ext == CEOS_sarl_sart)
    return CEOS_sard_sarl_sart_TRIPLE;

  if (data_ext == CEOS_D && metadata_ext == CEOS_L)
    return CEOS_D_L_PAIR;

  if (data_ext == CEOS_RAW && metadata_ext == CEOS_LDR)
    return CEOS_RAW_LDR_PAIR;

  if (data_ext == CEOS_dat && metadata_ext == CEOS_lea)
    return CEOS_dat_lea_PAIR;

  if (data_ext == CEOS_raw && metadata_ext == CEOS_ldr)
    return CEOS_raw_ldr_PAIR;

  if (data_ext == CEOS_DAT && metadata_ext == CEOS_LEA)
    return CEOS_DAT_LEA_PAIR;

  if (data_ext == CEOS_dat_ && metadata_ext == CEOS_lea_)
    return CEOS_dat_lea_PAIR;

  if (data_ext == CEOS_dat_t && metadata_ext == CEOS_lea_)
    return CEOS_dat_lea_PAIR;

  if (data_ext == CEOS_DAT_ && metadata_ext == CEOS_LEA_)
    return CEOS_DAT_LEA_PAIR;

  if (data_ext == CEOS_IMG && metadata_ext == CEOS_LED)
    return CEOS_IMG_LED_PAIR;

  if (data_ext == CEOS_dat2 && metadata_ext == CEOS_ldr)
    return CEOS_dat_ldr_PAIR;

  return NO_CEOS_FILE_PAIR;
}

/******************************************************************************
 * require_ceos_pair:
 * Do as get_ceos_names would unless there is no pair in which case exit the
 * program with a failure.  */
ceos_file_pairs_t require_ceos_pair(const char *ceosName, char ***dataName,
            char ***metaName, int *nBands, int *trailerFlag)
{
  char extensionList[512], baseName[512];
  int andFlag=TRUE;
  int begin=NO_CEOS_FILE_PAIR+1, end=NUM_CEOS_FILE_PAIRS;
  int ii;
  ceos_file_pairs_t pair;

  pair = get_ceos_names(ceosName, baseName, dataName, metaName, nBands, trailerFlag);

  if (pair != NO_CEOS_FILE_PAIR)
    return pair;
  else {

    /* Prepare a very readable list of possible extension pairs */
    sprintf(extensionList,"(%s %s)",ceos_data_extensions[begin],
      ceos_metadata_extensions[begin]);
    begin++;
    if (end-begin == 0)
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
    sprintf(logbuf,
      "**************************** ERROR! ****************************\n"
      "*   This program was looking for the CEOS style SAR files,\n"
      "*   %s and its associated file.\n"
      "*   One or both files either do not exist or cannot be read.\n"
      "*   Expected fileset extensions are:\n"
      "*   %s\n"
      "****************************************************************\n",
      ceosName, extensionList);
    if (logflag)   {printLog(logbuf);}
    printf(logbuf);
    exit(EXIT_FAILURE);
  }
}

void free_ceos_names(char **dataName, char **metaName)
{
  int ii;

  if (dataName != NULL) {
    for (ii=0; ii<MAX_BANDS; ii++)
      FREE(dataName[ii]);
    FREE(dataName);
  }
  if (metaName != NULL) {
    for (ii=0; ii<2; ii++)
      FREE(metaName[ii]);
    FREE(metaName);
  }
}
