/*
  get_ceos_names:

  Functions that match CEOS files with eachother primarily using their
  extensions.
*/

#include "asf.h"


/*****************************************************************************
 * has_ceos_leader_extension:
 * Returns TRUE if the file exists and has an accepted ASF CEOS leader file
 * extension  */
int has_ceos_leader_extension(const char *ceosName)
{
   char dirName[256], fileName[256];
   char metaTemp[1024];
   char base[256];
   char ext[256];
   FILE *metaFP;

   /* Separate the filename from the path (if there's a path there) */
   split_dir_and_file(ceosName, dirName, fileName);

   /* First we'll check for ASF style ceos extensions (which are suffixes) */
   split_base_and_ext(fileName, APPENDED_EXTENSION, base, ext);

   /* .L -- ASF level 1 (and sometimes level 0) naming scheme */
   sprintf(metaTemp,"%s%s.L",dirName,base);
   if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
      fclose(metaFP);
      return TRUE;
   }

   /* .LDR -- ASF level 0 naming scheme */
   sprintf(metaTemp,"%s%s.LDR",dirName,base);
   if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
      fclose(metaFP);
      return TRUE;
   }

   /* Second we'll look for FOCUS extensions (prefixes - you can thank RSI) */
   split_base_and_ext(fileName, PREPENDED_EXTENSION, base, ext);

   /* lea. -- FOCUS (taken from RSI) naming scheme */
   sprintf(metaTemp,"%slea.%s",dirName,base);
   if ((metaFP=fopen(metaTemp,"r"))!=NULL) {
      fclose(metaFP);
      return TRUE;
   }

   /* If we haven't returned yet there ain't no leader file */
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
   char base[256];
   char ext[256];
   FILE *dataFP;

   /* Separate the filename from the path (if there's a path there) */
   split_dir_and_file(ceosName, dirName, fileName);

   /* First we'll check for ASF style ceos extensions (which are suffixes) */
   split_base_and_ext(fileName, APPENDED_EXTENSION, base, ext);

   /* .D -- ASF level 1 (and sometimes level 0) naming scheme */
   sprintf(dataTemp,"%s%s.D",dirName,base);
   if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
      fclose(dataFP);
      return TRUE;
   }

   /* .RAW -- ASF level 0 naming scheme */
   sprintf(dataTemp,"%s%s.RAW",dirName,base);
   if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
      fclose(dataFP);
      return TRUE;
   }

   /* Second we'll look for FOCUS extensions (prefixes - you can thank RSI) */
   split_base_and_ext(fileName, PREPENDED_EXTENSION, base, ext);

   /* dat. -- FOCUS (taken from RSI) naming scheme */
   sprintf(dataTemp,"%sdat.%s",dirName,base);
   if ((dataFP=fopen(dataTemp,"r"))!=NULL) {
      fclose(dataFP);
      return TRUE;
   }

   /* If we haven't returned yet there ain't no leader file */
   return FALSE;
}


/******************************************************************************
 * has_legit_ceos_extensions:
 * Checks to see if the file extension of each ceos file name is an accepted
 * one, and if it has the correct pairing (ie a .D has a matching .L) */
int has_legit_ceos_extensions(const char *dataName, const char *metaName)
{
   char dataDirName[1024], metaDirName[1024];
   char tempDataName[1024], tempMetaName[1024];
   char dataBase[1024], metaBase[1024];
   char dataExt[1024], metaExt[1024];
   int ii;

   /* Separate the filename from the path (if there's a path there) */
   split_dir_and_file(dataName, dataDirName, tempDataName);
   split_dir_and_file(metaName, metaDirName, tempMetaName);

   /* Get the files' appended extensions. If there's no ext, return false */
   if (!split_base_and_ext(tempDataName, APPENDED_EXTENSION, dataBase, dataExt))
      return FALSE;
   if (!split_base_and_ext(tempMetaName, APPENDED_EXTENSION, metaBase, metaExt))
       return FALSE;
   /* Convert extension to caps so as to ignore case */
   for (ii=0; ii<strlen(dataExt); ii++) { dataExt[ii] = toupper(dataExt[ii]); }
   for (ii=0; ii<strlen(metaExt); ii++) { metaExt[ii] = toupper(metaExt[ii]); }
   /* See if the extensions are a legit set of ceos appended exts */
   if (strcmp(dataExt,".RAW")==0 && strcmp(dataExt,".LDR")==0)  return TRUE;
   if (strcmp(dataExt,".D")==0 && strcmp(dataExt,".L")==0) return TRUE;

   /* Get the files' prepended extensions. If there's no ext, return false */
   if (!split_base_and_ext(tempDataName, PREPENDED_EXTENSION, dataBase, dataExt))
      return FALSE;
   if (!split_base_and_ext(tempMetaName, PREPENDED_EXTENSION, metaBase, metaExt))
      return FALSE;
   /* Convert extension to caps so as to ignore case */
   for (ii=0; ii<strlen(dataExt); ii++) { dataExt[ii] = toupper(dataExt[ii]); }
   for (ii=0; ii<strlen(metaExt); ii++) { metaExt[ii] = toupper(metaExt[ii]); }
   /* See if the extensions are a legit set of ceos prepended exts */
   if (strcmp(dataExt,"DAT.")==0 && strcmp(dataExt,"LEA.")==0)  return TRUE;

   /* If we haven't returned by now, we haven't got a correctly labelled ceos
    * pair */
   return FALSE;
}


/******************************************************************************
 * get_ceos_names:
 * Given the name of a file (potentially with path in front of it), determine if
 * it is one of a CEOS file pair (depending on our accepted CEOS file
 * extensions). If so populate dataName & metaName with the appropriate names
 * and return the appropriate ENUM ceos_file_pairs_t value. Otherwise exit the
 * program with a failure.  */
ceos_file_pairs_t get_ceos_names(const char *ceosName, char *dataName,
                                 char *metaName)
{
   char dirName[256], fileName[256];
   char dataTemp[1024], metaTemp[1024];
   char base[256];
   char ext[256];
   FILE *dataFP, *metaFP;

   /* Separate the filename from the path (if there's a path there) */
   split_dir_and_file(ceosName, dirName, fileName);


   /* First we'll check for ASF style ceos extensions (which are suffixes) */
   split_base_and_ext(fileName, APPENDED_EXTENSION, base, ext);

   /* .L & .D -- ASF level 1 (and sometimes level 0) naming scheme */
   sprintf(metaTemp,"%s%s.L",dirName,base);
   sprintf(dataTemp,"%s%s.D",dirName,base);
   if (   ((metaFP=fopen(metaTemp,"r"))!=NULL)
       && ((dataFP=fopen(dataTemp,"r"))!=NULL)) {
         fclose(metaFP);
         fclose(dataFP);
         strcpy(dataName,dataTemp);
         strcpy(metaName,metaTemp);
         return ASF_CEOS_D_L_PAIR;
   }

   /* .LDR & .RAW -- ASF level 0 naming scheme */
   sprintf(metaTemp,"%s%s.LDR",dirName,base);
   sprintf(dataTemp,"%s%s.RAW",dirName,base);
   if (   ((metaFP=fopen(metaTemp,"r"))!=NULL)
       && ((dataFP=fopen(dataTemp,"r"))!=NULL)) {
         fclose(metaFP);
         fclose(dataFP);
         strcpy(dataName,dataTemp);
         strcpy(metaName,metaTemp);
         return ASF_CEOS_RAW_LDR_PAIR;
   }

   /* Second we'll look for FOCUS extensions (prefixes - you can thank RSI) */
   split_base_and_ext(fileName, PREPENDED_EXTENSION, base, ext);

   /* lea. & dat. -- FOCUS (taken from RSI) naming scheme */
   sprintf(metaTemp,"%slea.%s",dirName,base);
   sprintf(dataTemp,"%sdat.%s",dirName,base);
   if (   ((metaFP=fopen(metaTemp,"r"))!=NULL)
       && ((dataFP=fopen(dataTemp,"r"))!=NULL)) {
         fclose(metaFP);
         fclose(dataFP);
         strcpy(dataName,dataTemp);
         strcpy(metaName,metaTemp);
         return FOCUS_CEOS_DAT_LEA_PAIR;
   }

   /* If we didn't find anything, report & leave */
   printf("**************************** ERROR! ****************************\n"
          "*   This program was looking for the CEOS style SAR file,\n"
          "*   %s and its associated file.\n"
          "*   It was unable to read one or both of them.\n"
          "*   File sets are: (*.D *.L), (*.RAW *.LDR), and (dat.* lea.*)\n"
          "****************************************************************\n",
          ceosName);
   exit(EXIT_FAILURE);
}
