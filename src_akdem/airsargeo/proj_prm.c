/****************************************************************************
NAME:                           get_proj_prm

PURPOSE:  Retrieves the projection parameters from the projection parameter file.
Returns zero on error.

PROGRAM HISTORY:
VERSION  DATE   AUTHOR     CODE/CONT   REASON
-------  ----   ------     ---------   -----------------------------
  5.0    2/89   D. Steinwand  CSB      LAS 5.0 Original development

ALGORITHM DESCRIPTION:
        Open the label table file containing the projection parameters
        Get the label
        Determine the position of the fields within the file
        Scan the file for the projection parameters specified by the input
           and output keys  (If the keys are null, the first record in the
           file is assigned to the output projection; the second is the
           input projection)
        Close the table
        Return
*****************************************************************************/
#include "asf.h"
#include "worgen.h"
#include "ltable.h"
#include "projectGeo.h"

int get_proj_prm(char *proj_file, char *proj_key, proj_prm *proj)
{
char fileName[1000];
struct TAB_DEF tab;     /* Tabular structure of label table */
struct VECTOR *keyptr;  /* Pointer to key field */
struct VECTOR *codeptr; /* Pointer to projection code field */
struct VECTOR *zoneptr; /* Pointer to zone code field */
struct VECTOR *unitptr; /* Pointer to unit of measure field */
struct VECTOR *sphptr;  /* Pointer to datum (spheroid) code field */
struct VECTOR *parmptr; /* Pointer to projection coefficients field */
int outproj_found;     /* Boolean--output projection found? */
int status;            /* Function return status */
int i;                 /* Loop counter */
int len = 20;          /*Length of buffer to upper-case.*/
char outkey[20];       /*Copy of key*/

/* Open the labeled table file & retrieve the label
  ------------------------------------------------*/
if (fileExists(proj_file))
	strcpy(fileName,proj_file);
else
{
	if (extExists(proj_file,".proj"))
		create_name(fileName,proj_file,".proj");
	else
	{
	  sprintf(errbuf, "   ERROR: Couldn't find projection file named '%s'!!\n",proj_file);
	  printErr(errbuf);
	}
}
open_tab(&tab, fileName, IREAD);
get_vector(&tab);

/* Determine pointers to fields in the file
  ----------------------------------------*/
if (NULL==(keyptr  = get_field_ptr(&tab, "PROJKEY"))) return(0);
if (NULL==(codeptr = get_field_ptr(&tab, "PROJTYPE"))) return(0);
if (NULL==(zoneptr = get_field_ptr(&tab, "PROJZONE"))) return(0);
if (NULL==(unitptr = get_field_ptr(&tab, "PROJUNITS"))) return(0);
if (NULL==(sphptr  = get_field_ptr(&tab, "PROJSPH"))) return(0);
if (NULL==(parmptr = get_field_ptr(&tab, "PROJPARMS"))) return (0);

/* Initialize BOOLEAN variables
  ----------------------------*/
outproj_found = 0;
strcpy(outkey,proj_key);
c_low2up(outkey,&len);
printf("   Output projection key: %s\n",outkey);
if (logflag) {
  sprintf(logbuf, "   Output projection key: %s\n",outkey);
  printLog(logbuf);
}

/* Loop until key is found or EOF
 -------------------------------*/
for (;;)
{
   status = get_record(&tab);
   if (status == E_FAIL) return(0);
   if (status == E_EOF) break;
   c_low2up(keyptr->ptr.c,&len);

   if(strcmp(keyptr->ptr.c, outkey) == 0)
     if(!outproj_found)
     {
        proj->proj = *codeptr->ptr.i4;
        proj->zone = *zoneptr->ptr.i4;
        proj->units = *unitptr->ptr.i4;
        proj->datum = *sphptr->ptr.i4;
        for (i = 0; i < 15; i++)
           proj->parms[i] = parmptr->ptr.r8[i];
        outproj_found = 1;
        break;
      }
}

/* Close the file & return
  -----------------------*/
close_tab(&tab);
if (!outproj_found)
{
    printf("   Unable to find output projection key '%s' in \n"
           "   projection file '%s'!\n",outkey,fileName);
    return(0);
}
return(1);
}







