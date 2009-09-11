/****************************************************************
FUNCTION NAME: set_era

SYNTAX: set_era(inname, onm, opflag)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    inname      char *          base file name 
    onm         char *          file name with extension
    opflag      int             -1 = Unknown File Type
                                 0 = data file
                                 1 = leader file
                                 2 = trailer file

DESCRIPTION:
        Given an input string, set_era strips all characters after
  the first "." that occurs in the string to create a base name.
  From this, it creates strings corresponding to all possible 
  ASF standard product types by appending file extensions.  Using
  the option flag passed, it then looks for files of the requested 
  type.  The program looks for post RADARSAT type files first. 

RETURN VALUE:   0 - Found PRE RADARSAT era data
                1 - Found RADARSAT era data

SPECIAL CONSIDERATIONS:  Exits with an error message if no file is found

PROGRAM HISTORY:
Vers    Date    Author          Description
----    ----    --------        ------------------------
 1.0    9/96    T. Logan        Initial Creation
 1.1    7/97    D. Corbett      extend n_ variables from 40 chars to 256
 1.2    1/99    O. Lawlor       Use create_name for better-behaved extensions.

****************************************************************/
#include "asf.h"

#define BUFFER          1024

int set_era(const char *inname, char *onm, int opflag)
{
  char n_L[BUFFER], n_D[BUFFER];
  char n_trl[BUFFER], n_tlr[BUFFER], n_ldr[BUFFER], n_dat[BUFFER];
  char fileType[128];
  FILE *fp;

  strcpy(fileType,"bad opflag");
  
  create_name(n_L,inname,".L");
  create_name(n_D,inname,".D");
  create_name(n_trl,inname,".trl");
  create_name(n_tlr,inname,".tlr");
  create_name(n_ldr,inname,".ldr");
  create_name(n_dat,inname,".dat");

  switch(opflag) 
  {
   case -1:  /*** Unknown File Type Request ***/
    if ((fp=fopen(n_L  ,"r"))!=NULL) {fclose(fp);strcpy(onm,n_L  );return(1); } 
    if ((fp=fopen(n_D  ,"r"))!=NULL) {fclose(fp);strcpy(onm,n_D  );return(1); } 
    if ((fp=fopen(n_ldr,"r"))!=NULL) {fclose(fp);strcpy(onm,n_ldr);return(0); } 
    if ((fp=fopen(n_trl,"r"))!=NULL) {fclose(fp);strcpy(onm,n_trl);return(0); } 
    if ((fp=fopen(n_tlr,"r"))!=NULL) {fclose(fp);strcpy(onm,n_tlr);return(0); } 
    if ((fp=fopen(n_dat,"r"))!=NULL) {fclose(fp);strcpy(onm,n_dat);return(0); } 
    strcpy(fileType,"unspecified-- any extension would do.");
    break;

   case  0: /*** Data File Requested ***/
    if ((fp=fopen(n_D  ,"r"))!=NULL) {fclose(fp);strcpy(onm,n_D  );return(1); } 
    if ((fp=fopen(n_dat,"r"))!=NULL) {fclose(fp);strcpy(onm,n_dat);return(0); } 
    strcpy(fileType,"the data file-- '.dat' or '.D'");
    break;

   case  1: /*** Leader File Requested ***/
    if ((fp=fopen(n_L  ,"r"))!=NULL) {fclose(fp);strcpy(onm,n_L  );return(1); } 
    if ((fp=fopen(n_ldr,"r"))!=NULL) {fclose(fp);strcpy(onm,n_ldr);return(0); } 
    strcpy(fileType,"the leader file-- '.ldr' or '.L'");
    break;

   case  2: /*** Trailer File Requested ***/
    if ((fp=fopen(n_L,  "r"))!=NULL) {fclose(fp);strcpy(onm,n_L  );return(1); } 
    if ((fp=fopen(n_trl,"r"))!=NULL) {fclose(fp);strcpy(onm,n_trl);return(0); } 
    if ((fp=fopen(n_tlr,"r"))!=NULL) {fclose(fp);strcpy(onm,n_tlr);return(0); } 
    strcpy(fileType,"the trailer file-- '.trl', '.tlr', or '.L'");
  }

 printf("****************** ERROR! ******************\n"
        "*  This program was looking for a SAR file, base name\n"
        "*  %s,\n"
        "*  which it could not find. The file it was looking for\n"
        "*  was %s.\n",inname,fileType);
 exit(EXIT_FAILURE);
}
