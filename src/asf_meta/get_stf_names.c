/*
  get_stf_names:

  Functions that match STF files with each other primarily using their
  extensions.
*/

#include "asf.h"
#include "get_stf_names.h"

/* Lists of STF data and metadata extensions */
// NOTE: Must match 1:1 with the enums in stf_metadata_ext_t,
// stf_data_ext_t, and stf_file_pairs_t (see get_stf_names.h)
const char stf_metadata_extensions[][NUM_STF_METADATA_EXTS] = {
    "",
    ".PAR",     ".par",
    ".000.PAR", ".000.par",
    ".001.PAR", ".001.par",
    ".002.PAR", ".002.par",
    ".003.PAR", ".003.par",
    ".004.PAR", ".004.par",
    ".005.PAR", ".005.par",
    ".006.PAR", ".006.par",
    ".007.PAR", ".007.par",
    ".008.PAR", ".008.par",
    ".009.PAR", ".009.par",
    ".010.PAR", ".010.par",
    ".011.PAR", ".011.par",
    ".012.PAR", ".012.par",
    ".013.PAR", ".013.par",
    ".014.PAR", ".014.par",
    ".015.PAR", ".015.par",
    ".016.PAR", ".016.par",
    ".017.PAR", ".017.par",
    ".018.PAR", ".018.par",
    ".019.PAR", ".019.par",
    "_000.PAR", "_000.par",
    "_001.PAR", "_001.par",
    "_002.PAR", "_002.par",
    "_003.PAR", "_003.par",
    "_004.PAR", "_004.par",
    "_005.PAR", "_005.par",
    "_006.PAR", "_006.par",
    "_007.PAR", "_007.par",
    "_008.PAR", "_008.par",
    "_009.PAR", "_009.par",
    "_010.PAR", "_010.par",
    "_011.PAR", "_011.par",
    "_012.PAR", "_012.par",
    "_013.PAR", "_013.par",
    "_014.PAR", "_014.par",
    "_015.PAR", "_015.par",
    "_016.PAR", "_016.par",
    "_017.PAR", "_017.par",
    "_018.PAR", "_018.par",
    "_019.PAR", "_019.par",
};

const char stf_data_extensions[][NUM_STF_DATA_EXTS] = {
    "",
    "",     "",
    ".000", ".000",
    ".001", ".001",
    ".002", ".002",
    ".003", ".003",
    ".004", ".004",
    ".005", ".005",
    ".006", ".006",
    ".007", ".007",
    ".008", ".008",
    ".009", ".009",
    ".010", ".010",
    ".011", ".011",
    ".012", ".012",
    ".013", ".013",
    ".014", ".014",
    ".015", ".015",
    ".016", ".016",
    ".017", ".017",
    ".018", ".018",
    ".019", ".019",
    ".000", ".000",
    ".001", ".001",
    ".002", ".002",
    ".003", ".003",
    ".004", ".004",
    ".005", ".005",
    ".006", ".006",
    ".007", ".007",
    ".008", ".008",
    ".009", ".009",
    ".010", ".010",
    ".011", ".011",
    ".012", ".012",
    ".013", ".013",
    ".014", ".014",
    ".015", ".015",
    ".016", ".016",
    ".017", ".017",
    ".018", ".018",
    ".019", ".019",
};

/******************************************************************************
 * get_stf_metadata_name:
 * Given the name of a file (potentially with path in front of it), determine
 * if it is a STF metadata file (depending on our accepted STF extensions).
 * If so populate metaName with the appropriate name and return the appropriate
 * ENUM stf_metadata_ext_t value.  */
stf_metadata_ext_t get_stf_metadata_name(const char *stfName, char **pMetaName)
{
    stf_metadata_ext_t ret = NO_STF_METADATA;
    char *pDataName = NULL, MetaName[1024];
    int begin=NO_STF_METADATA+1, end=NUM_STF_METADATA_EXTS;

    asfRequire(NUM_STF_METADATA_EXTS == NUM_STF_DATA_EXTS,
               "Programming error: get_stf_names.h has a different number of extensions\n"
               "for metadata and data files for STF data.\n");

    stf_data_ext_t dret = get_stf_data_name(stfName, &pDataName);
    if (dret != NO_STF_DATA) {
        // A data basename exists, so look for a metadata name based on the data
        // name
        char dir[1024], dummy[1024];
        split_dir_and_file(stfName, dir, dummy);
        stf_metadata_ext_t i;
        for (i=begin; i<end; i++) {
            sprintf(MetaName, "%s%s", pDataName, stf_metadata_extensions[i]);
            if (fileExists(MetaName)) {
                *pMetaName = (char *)MALLOC(sizeof(char)*(strlen(MetaName) + 64));
                strcpy(*pMetaName, MetaName);
                ret = i;
                break;
            }
        }
        if (ret == NO_STF_METADATA) *pMetaName = NULL;
    }

    FREE(pDataName);
    return ret;
}

/******************************************************************************
 * require_stf_metadata:
 * If get_stf_metadata_name fails to find a file, then exit the program.
 * Otherwise act like get_stf_metadata_name  */
stf_metadata_ext_t require_stf_metadata(const char *stfName, char **metaName)
{
  stf_metadata_ext_t ret = get_stf_metadata_name(stfName, metaName);

  /* If we didn't find anything, report & leave */
  if (ret == NO_STF_METADATA) {
    char extensionList[2048];
    int andFlag=TRUE;
    int begin=NO_STF_METADATA+1, end=NUM_STF_METADATA_EXTS;
    int ii;

    asfRequire(NUM_STF_METADATA_EXTS == NUM_STF_DATA_EXTS,
               "Programming error: get_stf_names.h has a different number of extensions\n"
                       "for metadata and data files for STF data.\n");

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
    if (logflag) printLog(logbuf);
    asfPrintError(logbuf);
  }

  /* If we found a file, return its extension type! */
  return ret;
}

/******************************************************************************
 * get_stf_data_name:
 */
stf_data_ext_t get_stf_data_name(const char *stfName, char **pDataName)
{
  char dataName[1024];
  stf_data_ext_t ret = NO_STF_DATA;
  int begin=NO_STF_DATA+1, end=NUM_STF_DATA_EXTS;

  stf_data_ext_t i;
  for (i=begin; i<end; i++) {
    sprintf(dataName, "%s%s", stfName, stf_data_extensions[i]);
    if (fileExists(dataName)) {
      *pDataName = (char *)MALLOC(sizeof(char)*(strlen(dataName) + 64\
						));
      strcpy(*pDataName, dataName);
      ret = i;
      break;
    }
  }
  if (ret == NO_STF_DATA) *pDataName = NULL;
  
  return ret;
}

/******************************************************************************
 * require_stf_data:
 * If get_stf_data_name fails to find a file, then exit the program.
 * Otherwise act like get_stf_data_name  */
stf_data_ext_t require_stf_data(const char *stfName, char **dataName)
{
  stf_data_ext_t ret = get_stf_data_name(stfName, dataName);

  /* If we didn't find anything, report & leave */
  if (ret == NO_STF_DATA) {
    char extensionList[2048];
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
stf_file_pairs_t get_stf_names(const char *stfName, char **dataName,
                                 char **metaName)
{
    stf_data_ext_t     stf_data_type     = get_stf_data_name(stfName, dataName);
    stf_metadata_ext_t stf_metadata_type = get_stf_metadata_name(stfName, metaName);

  if ((stf_data_type     == STF_BLANK || stf_data_type == STF_blank)
      && stf_metadata_type == STF_PAR)
    return STF_PAR_PAIR;

  else if ((stf_data_type     == STF_BLANK || stf_data_type == STF_blank)
      && stf_metadata_type == STF_par)
    return STF_par_PAIR;

  else if ((stf_data_type     == STF_000_BLANK || stf_data_type == STF_000_blank)
      && (stf_metadata_type == STF_000_PAR || stf_metadata_type == STF_PAR))
    return STF_000_PAR_PAIR;

  else if ((stf_data_type     == STF_000_BLANK || stf_data_type == STF_000_blank)
      && (stf_metadata_type == STF_000_par || stf_metadata_type == STF_par))
    return STF_000_par_PAIR;

  else if ((stf_data_type     == STF_001_BLANK || stf_data_type == STF_001_blank)
         && (stf_metadata_type == STF_001_PAR || stf_metadata_type == STF_PAR))
      return STF_001_PAR_PAIR;

  else if ((stf_data_type     == STF_001_BLANK || stf_data_type == STF_001_blank)
         && (stf_metadata_type == STF_001_par || stf_metadata_type == STF_par))
      return STF_001_par_PAIR;

  else if ((stf_data_type     == STF_002_BLANK || stf_data_type == STF_002_blank)
         && (stf_metadata_type == STF_002_PAR || stf_metadata_type == STF_PAR))
      return STF_002_PAR_PAIR;

  else if ((stf_data_type     == STF_002_BLANK || stf_data_type == STF_002_blank)
         && (stf_metadata_type == STF_002_par || stf_metadata_type == STF_par))
      return STF_002_par_PAIR;

  else if ((stf_data_type     == STF_003_BLANK || stf_data_type == STF_003_blank)
         && (stf_metadata_type == STF_003_PAR || stf_metadata_type == STF_PAR))
      return STF_003_PAR_PAIR;

  else if ((stf_data_type     == STF_003_BLANK || stf_data_type == STF_003_blank)
         && (stf_metadata_type == STF_003_par || stf_metadata_type == STF_par))
      return STF_003_par_PAIR;

  else if ((stf_data_type     == STF_004_BLANK || stf_data_type == STF_004_blank)
         && (stf_metadata_type == STF_004_PAR || stf_metadata_type == STF_PAR))
      return STF_004_PAR_PAIR;

  else if ((stf_data_type     == STF_004_BLANK || stf_data_type == STF_004_blank)
         && (stf_metadata_type == STF_004_par || stf_metadata_type == STF_par))
      return STF_004_par_PAIR;

  else if ((stf_data_type     == STF_005_BLANK || stf_data_type == STF_005_blank)
         && (stf_metadata_type == STF_005_PAR || stf_metadata_type == STF_PAR))
      return STF_005_PAR_PAIR;

  else if ((stf_data_type     == STF_005_BLANK || stf_data_type == STF_005_blank)
         && (stf_metadata_type == STF_005_par || stf_metadata_type == STF_par))
      return STF_005_par_PAIR;

  else if ((stf_data_type     == STF_006_BLANK || stf_data_type == STF_006_blank)
         && (stf_metadata_type == STF_006_PAR || stf_metadata_type == STF_PAR))
      return STF_006_PAR_PAIR;

  else if ((stf_data_type     == STF_006_BLANK || stf_data_type == STF_006_blank)
         && (stf_metadata_type == STF_006_par || stf_metadata_type == STF_par))
      return STF_006_par_PAIR;

  else if ((stf_data_type     == STF_007_BLANK || stf_data_type == STF_007_blank)
         && (stf_metadata_type == STF_007_PAR || stf_metadata_type == STF_PAR))
      return STF_007_PAR_PAIR;

  else if ((stf_data_type     == STF_007_BLANK || stf_data_type == STF_007_blank)
         && (stf_metadata_type == STF_007_par || stf_metadata_type == STF_par))
      return STF_007_par_PAIR;

  else if ((stf_data_type     == STF_008_BLANK || stf_data_type == STF_008_blank)
         && (stf_metadata_type == STF_008_PAR || stf_metadata_type == STF_PAR))
      return STF_008_PAR_PAIR;

  else if ((stf_data_type     == STF_008_BLANK || stf_data_type == STF_008_blank)
         && (stf_metadata_type == STF_008_par || stf_metadata_type == STF_par))
      return STF_008_par_PAIR;

  else if ((stf_data_type     == STF_009_BLANK || stf_data_type == STF_009_blank)
         && (stf_metadata_type == STF_009_PAR || stf_metadata_type == STF_PAR))
      return STF_009_PAR_PAIR;

  else if ((stf_data_type     == STF_009_BLANK || stf_data_type == STF_009_blank)
         && (stf_metadata_type == STF_009_par || stf_metadata_type == STF_par))
      return STF_009_par_PAIR;

  else if ((stf_data_type     == STF_010_BLANK || stf_data_type == STF_010_blank)
         && (stf_metadata_type == STF_010_PAR || stf_metadata_type == STF_PAR))
      return STF_010_PAR_PAIR;

  else if ((stf_data_type     == STF_010_BLANK || stf_data_type == STF_010_blank)
         && (stf_metadata_type == STF_010_par || stf_metadata_type == STF_par))
      return STF_010_par_PAIR;

  else if ((stf_data_type     == STF_011_BLANK || stf_data_type == STF_011_blank)
         && (stf_metadata_type == STF_011_PAR || stf_metadata_type == STF_PAR))
      return STF_011_PAR_PAIR;

  else if ((stf_data_type     == STF_011_BLANK || stf_data_type == STF_011_blank)
         && (stf_metadata_type == STF_011_par || stf_metadata_type == STF_par))
      return STF_011_par_PAIR;

  else if ((stf_data_type     == STF_012_BLANK || stf_data_type == STF_012_blank)
         && (stf_metadata_type == STF_012_PAR || stf_metadata_type == STF_PAR))
      return STF_012_PAR_PAIR;

  else if ((stf_data_type     == STF_012_BLANK || stf_data_type == STF_012_blank)
         && (stf_metadata_type == STF_012_par || stf_metadata_type == STF_par))
      return STF_012_par_PAIR;

  else if ((stf_data_type     == STF_013_BLANK || stf_data_type == STF_013_blank)
         && (stf_metadata_type == STF_013_PAR || stf_metadata_type == STF_PAR))
      return STF_013_PAR_PAIR;

  else if ((stf_data_type     == STF_013_BLANK || stf_data_type == STF_013_blank)
         && (stf_metadata_type == STF_013_par || stf_metadata_type == STF_par))
      return STF_013_par_PAIR;

  else if ((stf_data_type     == STF_014_BLANK || stf_data_type == STF_014_blank)
         && (stf_metadata_type == STF_014_PAR || stf_metadata_type == STF_PAR))
      return STF_014_PAR_PAIR;

  else if ((stf_data_type     == STF_014_BLANK || stf_data_type == STF_014_blank)
         && (stf_metadata_type == STF_014_par || stf_metadata_type == STF_par))
      return STF_014_par_PAIR;

  else if ((stf_data_type     == STF_015_BLANK || stf_data_type == STF_015_blank)
         && (stf_metadata_type == STF_015_PAR || stf_metadata_type == STF_PAR))
      return STF_015_PAR_PAIR;

  else if ((stf_data_type     == STF_015_BLANK || stf_data_type == STF_015_blank)
         && (stf_metadata_type == STF_015_par || stf_metadata_type == STF_par))
      return STF_015_par_PAIR;

  else if ((stf_data_type     == STF_016_BLANK || stf_data_type == STF_016_blank)
         && (stf_metadata_type == STF_016_PAR || stf_metadata_type == STF_PAR))
      return STF_016_PAR_PAIR;

  else if ((stf_data_type     == STF_016_BLANK || stf_data_type == STF_016_blank)
         && (stf_metadata_type == STF_016_par || stf_metadata_type == STF_par))
      return STF_016_par_PAIR;

  else if ((stf_data_type     == STF_017_BLANK || stf_data_type == STF_017_blank)
         && (stf_metadata_type == STF_017_PAR || stf_metadata_type == STF_PAR))
      return STF_017_PAR_PAIR;

  else if ((stf_data_type     == STF_017_BLANK || stf_data_type == STF_017_blank)
         && (stf_metadata_type == STF_017_par || stf_metadata_type == STF_par))
      return STF_017_par_PAIR;

  else if ((stf_data_type     == STF_018_BLANK || stf_data_type == STF_018_blank)
         && (stf_metadata_type == STF_018_PAR || stf_metadata_type == STF_PAR))
      return STF_018_PAR_PAIR;

  else if ((stf_data_type     == STF_018_BLANK || stf_data_type == STF_018_blank)
         && (stf_metadata_type == STF_018_par || stf_metadata_type == STF_par))
      return STF_018_par_PAIR;

  else if ((stf_data_type     == STF_019_BLANK || stf_data_type == STF_019_blank)
         && (stf_metadata_type == STF_019_PAR || stf_metadata_type == STF_PAR))
      return STF_019_PAR_PAIR;

  else if ((stf_data_type     == STF_019_BLANK || stf_data_type == STF_019_blank)
         && (stf_metadata_type == STF_019_par || stf_metadata_type == STF_par))
      return STF_019_par_PAIR;

  else if ((stf_data_type     == STF_000_BLANK || stf_data_type == STF_000_blank)
      && (stf_metadata_type == STF_U_000_PAR || stf_metadata_type == STF_PAR))
    return STF_U_000_PAR_PAIR;

  else if ((stf_data_type     == STF_000_BLANK || stf_data_type == STF_000_blank)
      && (stf_metadata_type == STF_U_000_par || stf_metadata_type == STF_par))
    return STF_U_000_par_PAIR;

  else if ((stf_data_type     == STF_001_BLANK || stf_data_type == STF_001_blank)
      && (stf_metadata_type == STF_U_001_PAR || stf_metadata_type == STF_PAR))
    return STF_U_001_PAR_PAIR;

  else if ((stf_data_type     == STF_001_BLANK || stf_data_type == STF_001_blank)
      && (stf_metadata_type == STF_U_001_par || stf_metadata_type == STF_par))
    return STF_U_001_par_PAIR;

  else if ((stf_data_type     == STF_002_BLANK || stf_data_type == STF_002_blank)
         && (stf_metadata_type == STF_U_002_PAR || stf_metadata_type == STF_PAR))
      return STF_U_002_PAR_PAIR;

  else if ((stf_data_type     == STF_002_BLANK || stf_data_type == STF_002_blank)
         && (stf_metadata_type == STF_U_002_par || stf_metadata_type == STF_par))
      return STF_U_002_par_PAIR;

  else if ((stf_data_type     == STF_003_BLANK || stf_data_type == STF_003_blank)
         && (stf_metadata_type == STF_U_003_PAR || stf_metadata_type == STF_PAR))
      return STF_U_003_PAR_PAIR;

  else if ((stf_data_type     == STF_003_BLANK || stf_data_type == STF_003_blank)
         && (stf_metadata_type == STF_U_003_par || stf_metadata_type == STF_par))
      return STF_U_003_par_PAIR;

  else if ((stf_data_type     == STF_004_BLANK || stf_data_type == STF_004_blank)
         && (stf_metadata_type == STF_U_004_PAR || stf_metadata_type == STF_PAR))
      return STF_U_004_PAR_PAIR;

  else if ((stf_data_type     == STF_004_BLANK || stf_data_type == STF_004_blank)
         && (stf_metadata_type == STF_U_004_par || stf_metadata_type == STF_par))
      return STF_U_004_par_PAIR;

  else if ((stf_data_type     == STF_005_BLANK || stf_data_type == STF_005_blank)
         && (stf_metadata_type == STF_U_005_PAR || stf_metadata_type == STF_PAR))
      return STF_U_005_PAR_PAIR;

  else if ((stf_data_type     == STF_005_BLANK || stf_data_type == STF_005_blank)
         && (stf_metadata_type == STF_U_005_par || stf_metadata_type == STF_par))
      return STF_U_005_par_PAIR;

  else if ((stf_data_type     == STF_006_BLANK || stf_data_type == STF_006_blank)
         && (stf_metadata_type == STF_U_006_PAR || stf_metadata_type == STF_PAR))
      return STF_U_006_PAR_PAIR;

  else if ((stf_data_type     == STF_006_BLANK || stf_data_type == STF_006_blank)
         && (stf_metadata_type == STF_U_006_par || stf_metadata_type == STF_par))
      return STF_U_006_par_PAIR;

  else if ((stf_data_type     == STF_007_BLANK || stf_data_type == STF_007_blank)
         && (stf_metadata_type == STF_U_007_PAR || stf_metadata_type == STF_PAR))
      return STF_U_007_PAR_PAIR;

  else if ((stf_data_type     == STF_007_BLANK || stf_data_type == STF_007_blank)
         && (stf_metadata_type == STF_U_007_par || stf_metadata_type == STF_par))
      return STF_U_007_par_PAIR;

  else if ((stf_data_type     == STF_008_BLANK || stf_data_type == STF_008_blank)
         && (stf_metadata_type == STF_U_008_PAR || stf_metadata_type == STF_PAR))
      return STF_U_008_PAR_PAIR;

  else if ((stf_data_type     == STF_008_BLANK || stf_data_type == STF_008_blank)
         && (stf_metadata_type == STF_U_008_par || stf_metadata_type == STF_par))
      return STF_U_008_par_PAIR;

  else if ((stf_data_type     == STF_009_BLANK || stf_data_type == STF_009_blank)
         && (stf_metadata_type == STF_U_009_PAR || stf_metadata_type == STF_PAR))
      return STF_U_009_PAR_PAIR;

  else if ((stf_data_type     == STF_009_BLANK || stf_data_type == STF_009_blank)
         && (stf_metadata_type == STF_U_009_par || stf_metadata_type == STF_par))
      return STF_U_009_par_PAIR;

  else if ((stf_data_type     == STF_010_BLANK || stf_data_type == STF_010_blank)
         && (stf_metadata_type == STF_U_010_PAR || stf_metadata_type == STF_PAR))
      return STF_U_010_PAR_PAIR;

  else if ((stf_data_type     == STF_010_BLANK || stf_data_type == STF_010_blank)
         && (stf_metadata_type == STF_U_010_par || stf_metadata_type == STF_par))
      return STF_U_010_par_PAIR;

  else if ((stf_data_type     == STF_011_BLANK || stf_data_type == STF_011_blank)
         && (stf_metadata_type == STF_U_011_PAR || stf_metadata_type == STF_PAR))
      return STF_U_011_PAR_PAIR;

  else if ((stf_data_type     == STF_011_BLANK || stf_data_type == STF_011_blank)
         && (stf_metadata_type == STF_U_011_par || stf_metadata_type == STF_par))
      return STF_U_011_par_PAIR;

  else if ((stf_data_type     == STF_012_BLANK || stf_data_type == STF_012_blank)
         && (stf_metadata_type == STF_U_012_PAR || stf_metadata_type == STF_PAR))
      return STF_U_012_PAR_PAIR;

  else if ((stf_data_type     == STF_012_BLANK || stf_data_type == STF_012_blank)
         && (stf_metadata_type == STF_U_012_par || stf_metadata_type == STF_par))
      return STF_U_012_par_PAIR;

  else if ((stf_data_type     == STF_013_BLANK || stf_data_type == STF_013_blank)
         && (stf_metadata_type == STF_U_013_PAR || stf_metadata_type == STF_PAR))
      return STF_U_013_PAR_PAIR;

  else if ((stf_data_type     == STF_013_BLANK || stf_data_type == STF_013_blank)
         && (stf_metadata_type == STF_U_013_par || stf_metadata_type == STF_par))
      return STF_U_013_par_PAIR;

  else if ((stf_data_type     == STF_014_BLANK || stf_data_type == STF_014_blank)
         && (stf_metadata_type == STF_U_014_PAR || stf_metadata_type == STF_PAR))
      return STF_U_014_PAR_PAIR;

  else if ((stf_data_type     == STF_014_BLANK || stf_data_type == STF_014_blank)
         && (stf_metadata_type == STF_U_014_par || stf_metadata_type == STF_par))
      return STF_U_014_par_PAIR;

  else if ((stf_data_type     == STF_015_BLANK || stf_data_type == STF_015_blank)
         && (stf_metadata_type == STF_U_015_PAR || stf_metadata_type == STF_PAR))
      return STF_U_015_PAR_PAIR;

  else if ((stf_data_type     == STF_015_BLANK || stf_data_type == STF_015_blank)
         && (stf_metadata_type == STF_U_015_par || stf_metadata_type == STF_par))
      return STF_U_015_par_PAIR;

  else if ((stf_data_type     == STF_016_BLANK || stf_data_type == STF_016_blank)
         && (stf_metadata_type == STF_U_016_PAR || stf_metadata_type == STF_PAR))
      return STF_U_016_PAR_PAIR;

  else if ((stf_data_type     == STF_016_BLANK || stf_data_type == STF_016_blank)
         && (stf_metadata_type == STF_U_016_par || stf_metadata_type == STF_par))
      return STF_U_016_par_PAIR;

  else if ((stf_data_type     == STF_017_BLANK || stf_data_type == STF_017_blank)
         && (stf_metadata_type == STF_U_017_PAR || stf_metadata_type == STF_PAR))
      return STF_U_017_PAR_PAIR;

  else if ((stf_data_type     == STF_017_BLANK || stf_data_type == STF_017_blank)
         && (stf_metadata_type == STF_U_017_par || stf_metadata_type == STF_par))
      return STF_U_017_par_PAIR;

  else if ((stf_data_type     == STF_018_BLANK || stf_data_type == STF_018_blank)
         && (stf_metadata_type == STF_U_018_PAR || stf_metadata_type == STF_PAR))
      return STF_U_018_PAR_PAIR;

  else if ((stf_data_type     == STF_018_BLANK || stf_data_type == STF_018_blank)
         && (stf_metadata_type == STF_U_018_par || stf_metadata_type == STF_par))
      return STF_U_018_par_PAIR;

  else if ((stf_data_type     == STF_019_BLANK || stf_data_type == STF_019_blank)
         && (stf_metadata_type == STF_U_019_PAR || stf_metadata_type == STF_PAR))
      return STF_U_019_PAR_PAIR;

  else if ((stf_data_type     == STF_019_BLANK || stf_data_type == STF_019_blank)
         && (stf_metadata_type == STF_U_019_par || stf_metadata_type == STF_par))
      return STF_U_019_par_PAIR;

  return NO_STF_FILE_PAIR;
}

/******************************************************************************
 * require_stf_pair:
 * Do as get_stf_names would unless there is no pair in which case exit the
 * program with a failure.  */
stf_file_pairs_t require_stf_pair(const char *stfName, char **dataName,
                                    char **metaName)
{
  char extensionList[2048];
  int andFlag=TRUE;
  int begin=NO_STF_FILE_PAIR+1, end=NUM_STF_FILE_PAIRS;
  int ii;

  stf_file_pairs_t ret = get_stf_names(stfName, dataName, metaName);

  if (ret != NO_STF_FILE_PAIR) return ret;

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
  asfPrintError(logbuf);

  // Should never get here, but this will zip d'lips of the compiler
  return ret;
}

void free_stf_names(char *dataName, char *metaName)
{
  FREE(dataName);
  FREE(metaName);
}

char *get_stf_basename(const char *file, stf_data_ext_t *idx)
{
    char *ext = NULL;
    char *basename = STRDUP(file);
    stf_data_ext_t i, begin=NO_STF_DATA+1, end=NUM_STF_DATA_EXTS;

    // Look for standard extensions of the form ".xxx" first...
    for (i = begin, ext = NULL; i < end && ext == NULL; i++) {
        if (strlen(stf_data_extensions[i])) {
            ext = strstr(basename, stf_data_extensions[i]);
        }
    }
    if (ext && strlen(ext)) {
        // Whack the extension from the base name
        *ext = '\0';
        strcat(basename, stf_data_extensions[i]);
        *idx = i;
    }
    else {
        *idx = NO_STF_DATA;
    }

    return basename;
}

// Accessor functions
const char *get_stf_data_extension(stf_data_ext_t idx) {
    if (idx > NO_STF_DATA && idx < NUM_STF_DATA_EXTS)
        return stf_data_extensions[idx];
    else
        return (char*)NULL;
}

const char *get_stf_metadata_extension(stf_metadata_ext_t idx) {
    if (idx > NO_STF_METADATA && idx < NUM_STF_METADATA_EXTS)
        return stf_metadata_extensions[idx];
    else
        return (char*)NULL;
}

