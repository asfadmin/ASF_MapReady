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
  STF_PAR,
  STF_par,
  NUM_STF_METADATA_EXTS
} stf_metadata_ext_t;
extern const char stf_metadata_extensions[][8];

/* Enum for & strings for STF data file extensions */
typedef enum {
  NO_STF_DATA=0,
  STF_BLANK,           /* Dummy value to match up with stf metadata STF_PAR */
  STF_blank,           /* Dummy value to match up with stf metadata STF_par */
  NUM_STF_DATA_EXTS
} stf_data_ext_t;
extern const char stf_data_extensions[][8];

/* Enum for pairs of stf extensions */
typedef enum {
   NO_STF_FILE_PAIR=0,     /* Maybe an individual match, but not a pair */
   STF_PAR_PAIR,
   STF_par_PAIR,
   NUM_STF_FILE_PAIRS
} stf_file_pairs_t;

/* Given the name of a file (potentially with path in front of it), determine
   if it is a STF metadata file (depending on our accepted STF extensions). If
   so populate metaName with the appropriate name and return the appropriate
   ENUM stf_metadata_ext_t value.  */
stf_metadata_ext_t get_stf_metadata_name(const char *stfName, char *metaName);

/* If get_stf_metadata_name fails to find a file, then exit the program.  */
stf_metadata_ext_t require_stf_metadata(const char *stfName, char *metaName);

/* Given the name of a file (potentially with path in front of it), determine
   if it is a STF data file (depending on our accepted STF extensions). If
   so, populate dataName with the appropriate name and return the appropriate
   ENUM stf_data_ext_t value.  */
stf_data_ext_t get_stf_data_name(const char *stfName, char *dataName);

/* If get_stf_data_name fails to find a file, then exit the program.  */
stf_data_ext_t require_stf_data(const char *stfName, char *dataName);

/* Given the name of a file (potentially with path in front of it), determine if
   it is one of a STF file pair (depending on our accepted STF file
   extensions). If so populate dataName & metaName with the appropriate names
   and return the appropriate ENUM stf_file_pairs_t value.  */
stf_file_pairs_t get_stf_names(const char *stfName, char *dataName,
                                 char *metaName);

/* Do as get_stf_names would unless there is no pair in which case exit the
   program with a failure.  */
stf_file_pairs_t require_stf_pair(const char *ceosName, char *dataName,
                                   char *metaName);


#endif
