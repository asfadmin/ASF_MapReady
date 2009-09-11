/*
get_stf_names.h

Functions that match STF files with each other using their extensions and
testing to see if the files actually exist
*/

#ifndef _GET_STF_NAMES_H_
#define _GET_STF_NAMES_H_

/* Enum & strings for STF metadata file extensions */
typedef enum {
  NO_STF_METADATA=0,
  STF_PAR,        STF_par,
  STF_000_PAR,    STF_000_par,
  STF_001_PAR,    STF_001_par,
  STF_002_PAR,    STF_002_par,
  STF_003_PAR,    STF_003_par,
  STF_004_PAR,    STF_004_par,
  STF_005_PAR,    STF_005_par,
  STF_006_PAR,    STF_006_par,
  STF_007_PAR,    STF_007_par,
  STF_008_PAR,    STF_008_par,
  STF_009_PAR,    STF_009_par,
  STF_010_PAR,    STF_010_par,
  STF_011_PAR,    STF_011_par,
  STF_012_PAR,    STF_012_par,
  STF_013_PAR,    STF_013_par,
  STF_014_PAR,    STF_014_par,
  STF_015_PAR,    STF_015_par,
  STF_016_PAR,    STF_016_par,
  STF_017_PAR,    STF_017_par,
  STF_018_PAR,    STF_018_par,
  STF_019_PAR,    STF_019_par,
  STF_U_000_PAR,  STF_U_000_par,
  STF_U_001_PAR,  STF_U_001_par,
  STF_U_002_PAR,  STF_U_002_par,
  STF_U_003_PAR,  STF_U_003_par,
  STF_U_004_PAR,  STF_U_004_par,
  STF_U_005_PAR,  STF_U_005_par,
  STF_U_006_PAR,  STF_U_006_par,
  STF_U_007_PAR,  STF_U_007_par,
  STF_U_008_PAR,  STF_U_008_par,
  STF_U_009_PAR,  STF_U_009_par,
  STF_U_010_PAR,  STF_U_010_par,
  STF_U_011_PAR,  STF_U_011_par,
  STF_U_012_PAR,  STF_U_012_par,
  STF_U_013_PAR,  STF_U_013_par,
  STF_U_014_PAR,  STF_U_014_par,
  STF_U_015_PAR,  STF_U_015_par,
  STF_U_016_PAR,  STF_U_016_par,
  STF_U_017_PAR,  STF_U_017_par,
  STF_U_018_PAR,  STF_U_018_par,
  STF_U_019_PAR,  STF_U_019_par,
  NUM_STF_METADATA_EXTS
} stf_metadata_ext_t;

/* Enum for & strings for STF data file extensions (stick with *_BLANK; *_blank
   is simply for matching data array indices with metadata array indices */
typedef enum {
  NO_STF_DATA=0,
  STF_BLANK,      STF_blank,
  STF_000_BLANK,  STF_000_blank,
  STF_001_BLANK,  STF_001_blank,
  STF_002_BLANK,  STF_002_blank,
  STF_003_BLANK,  STF_003_blank,
  STF_004_BLANK,  STF_004_blank,
  STF_005_BLANK,  STF_005_blank,
  STF_006_BLANK,  STF_006_blank,
  STF_007_BLANK,  STF_007_blank,
  STF_008_BLANK,  STF_008_blank,
  STF_009_BLANK,  STF_009_blank,
  STF_010_BLANK,  STF_010_blank,
  STF_011_BLANK,  STF_011_blank,
  STF_012_BLANK,  STF_012_blank,
  STF_013_BLANK,  STF_013_blank,
  STF_014_BLANK,  STF_014_blank,
  STF_015_BLANK,  STF_015_blank,
  STF_016_BLANK,  STF_016_blank,
  STF_017_BLANK,  STF_017_blank,
  STF_018_BLANK,  STF_018_blank,
  STF_019_BLANK,  STF_019_blank,
  STF_000_BLANK_,  STF_000_blank_, /* Technically, these elements (from here down) are not needed except to make */
  STF_001_BLANK_,  STF_001_blank_, /* the stf_data_ext_t enum list contain the same number of elements as the    */
  STF_002_BLANK_,  STF_002_blank_, /* stf_metadata_ext_t enum list, i.e. for loops                               */
  STF_003_BLANK_,  STF_003_blank_,
  STF_004_BLANK_,  STF_004_blank_,
  STF_005_BLANK_,  STF_005_blank_,
  STF_006_BLANK_,  STF_006_blank_,
  STF_007_BLANK_,  STF_007_blank_,
  STF_008_BLANK_,  STF_008_blank_,
  STF_009_BLANK_,  STF_009_blank_,
  STF_010_BLANK_,  STF_010_blank_,
  STF_011_BLANK_,  STF_011_blank_,
  STF_012_BLANK_,  STF_012_blank_,
  STF_013_BLANK_,  STF_013_blank_,
  STF_014_BLANK_,  STF_014_blank_,
  STF_015_BLANK_,  STF_015_blank_,
  STF_016_BLANK_,  STF_016_blank_,
  STF_017_BLANK_,  STF_017_blank_,
  STF_018_BLANK_,  STF_018_blank_,
  STF_019_BLANK_,  STF_019_blank_,
  NUM_STF_DATA_EXTS
} stf_data_ext_t;

/* Enum for pairs of stf extensions */
typedef enum {
  NO_STF_FILE_PAIR=0,     /* Maybe an individual match, but not a pair */
  STF_PAR_PAIR,        STF_par_PAIR,
  STF_000_PAR_PAIR,    STF_000_par_PAIR,
  STF_001_PAR_PAIR,    STF_001_par_PAIR,
  STF_002_PAR_PAIR,    STF_002_par_PAIR,
  STF_003_PAR_PAIR,    STF_003_par_PAIR,
  STF_004_PAR_PAIR,    STF_004_par_PAIR,
  STF_005_PAR_PAIR,    STF_005_par_PAIR,
  STF_006_PAR_PAIR,    STF_006_par_PAIR,
  STF_007_PAR_PAIR,    STF_007_par_PAIR,
  STF_008_PAR_PAIR,    STF_008_par_PAIR,
  STF_009_PAR_PAIR,    STF_009_par_PAIR,
  STF_010_PAR_PAIR,    STF_010_par_PAIR,
  STF_011_PAR_PAIR,    STF_011_par_PAIR,
  STF_012_PAR_PAIR,    STF_012_par_PAIR,
  STF_013_PAR_PAIR,    STF_013_par_PAIR,
  STF_014_PAR_PAIR,    STF_014_par_PAIR,
  STF_015_PAR_PAIR,    STF_015_par_PAIR,
  STF_016_PAR_PAIR,    STF_016_par_PAIR,
  STF_017_PAR_PAIR,    STF_017_par_PAIR,
  STF_018_PAR_PAIR,    STF_018_par_PAIR,
  STF_019_PAR_PAIR,    STF_019_par_PAIR,
  STF_U_000_PAR_PAIR,  STF_U_000_par_PAIR,
  STF_U_001_PAR_PAIR,  STF_U_001_par_PAIR,
  STF_U_002_PAR_PAIR,  STF_U_002_par_PAIR,
  STF_U_003_PAR_PAIR,  STF_U_003_par_PAIR,
  STF_U_004_PAR_PAIR,  STF_U_004_par_PAIR,
  STF_U_005_PAR_PAIR,  STF_U_005_par_PAIR,
  STF_U_006_PAR_PAIR,  STF_U_006_par_PAIR,
  STF_U_007_PAR_PAIR,  STF_U_007_par_PAIR,
  STF_U_008_PAR_PAIR,  STF_U_008_par_PAIR,
  STF_U_009_PAR_PAIR,  STF_U_009_par_PAIR,
  STF_U_010_PAR_PAIR,  STF_U_010_par_PAIR,
  STF_U_011_PAR_PAIR,  STF_U_011_par_PAIR,
  STF_U_012_PAR_PAIR,  STF_U_012_par_PAIR,
  STF_U_013_PAR_PAIR,  STF_U_013_par_PAIR,
  STF_U_014_PAR_PAIR,  STF_U_014_par_PAIR,
  STF_U_015_PAR_PAIR,  STF_U_015_par_PAIR,
  STF_U_016_PAR_PAIR,  STF_U_016_par_PAIR,
  STF_U_017_PAR_PAIR,  STF_U_017_par_PAIR,
  STF_U_018_PAR_PAIR,  STF_U_018_par_PAIR,
  STF_U_019_PAR_PAIR,  STF_U_019_par_PAIR,
  NUM_STF_FILE_PAIRS
} stf_file_pairs_t;

/* Given the name of a file (potentially with path in front of it), determine
   if it is a STF metadata file (depending on our accepted STF extensions). If
   so populate metaName with the appropriate name and return the appropriate
   ENUM stf_metadata_ext_t value.  */
stf_metadata_ext_t get_stf_metadata_name(const char *stfName, char **pMetaName);

/* If get_stf_metadata_name fails to find a file, then exit the program.  */
stf_metadata_ext_t require_stf_metadata(const char *stfName, char **metaName);

/* Given the name of a file (potentially with path in front of it), determine
   if it is a STF data file (depending on our accepted STF extensions). If
   so, populate dataName with the appropriate name and return the appropriate
   ENUM stf_data_ext_t value.  */
stf_data_ext_t get_stf_data_name(const char *stfName, char **pDataName);

/* If get_stf_data_name fails to find a file, then exit the program.  */
stf_data_ext_t require_stf_data(const char *stfName, char **dataName);

/* Given the name of a file (potentially with path in front of it), determine if
   it is one of a STF file pair (depending on our accepted STF file
   extensions). If so populate dataName & metaName with the appropriate names
   and return the appropriate ENUM stf_file_pairs_t value.  */
stf_file_pairs_t get_stf_names(const char *stfName, char **dataName,
                   char **metaName);

/* Do as get_stf_names would unless there is no pair in which case exit the
   program with a failure.  */
stf_file_pairs_t require_stf_pair(const char *ceosName, char **dataName,
                  char **metaName);

void free_stf_names(char *dataName, char *metaName);
char *get_stf_basename(const char *file, stf_data_ext_t *idx);
const char *get_stf_data_extension(stf_data_ext_t idx);
const char *get_stf_metadata_extension(stf_metadata_ext_t idx);

#endif
