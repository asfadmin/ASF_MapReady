/****************************************************************
FUNCTION NAME: c_getdatum

SYNTAX: c_getdatum(dtm_info)  

PARAMETERS:
    NAME:	TYPE:		   PURPOSE:
    --------------------------------------------------------
    dtm_info  struct DATUMDEF*     Datum code

DESCRIPTION:
    Retrieve datum information from datum and spheroid tables and fill
    the DATUMDEF variable

RETURN VALUE: Integer (1 or o for sucess of failure)

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.2    02/98   J. Willems   Initial development for datum conversi
    0.4    05/99   D. Lloyd     Free malloc'd memory
    0.6    10/99   T. Ratliff	Redesigned the routine to fix an
				error and be more efficient
    0.8	   12/99   D. Hames	Previous redesign discontinued the file
				check to spheroid.txt after a datum code
				was assigned. Copy and modify c_getdatum
				version 09/01/99. Add additional checks
				to while statements to fix infinite 
				looping and fatal error if the datum is
				not found. SRF#3127.1.0     8/01   S.
    1.0     8/01    S. Watts    Removed TAE dependencies.

*******************************************************************************/

#include <stdlib.h>
#include <math.h>

#include "asf.h"
#include "datum.h"


#define MINDTM 99
#define E_SUCC 0
#define E_FAIL -1
#define TRUE 1
#define FALSE 0


int c_getdatum
(
    struct DATUMDEF *dtm_info		/* Datum code */
)
{
FILE *file_ptr;         	/* Pointer to current table */
long dtm_num = -1;		/* Datum code */
long found = FALSE;		/* Flag set if line contains correct code */
long dtmflag = FALSE;		/* Flag set if datum code represents datum */
/*long pass = E_SUCC;*/  	/* Non-fatal code for errmsg routine */
long i = 0;			/* increment for loop */
long numb = -1;			/* Number retrieved form table */

char *temp;			/* temporary storage of line in table */
/*char *table_ptr;*/		/* Location of the LASTABLES */
char line[BUFSIZ];		/* Line scanned in from datum table */
char *dtmary[11] = {0};		/* Array of dtm_num fields */
char *spherary[4] = {0};	/* Array of spher fields */
/*char dtmfile[BUFSIZ];*/          	/* Pointer to datum table */
/*char spherfile[BUFSIZ];*/		/* Pointer to spheroid table */

double flat = 0.0;              /* Flattening of the spheroid */

dtm_num = dtm_info->datumnum;

/* Check for a valid datum code
  ----------------------------*/
if(dtm_num < 0)
   {
   printf("\nDatum value out of valid range\n");
   return(E_FAIL);
   }
else if(dtm_num > MINDTM)
   dtmflag = TRUE;

/* Get the directory of the tables
  ------------------------------*/


/*********************************************************
*****Took this part out so it is no so LAS dependent****
**********************************************************
table_ptr = getenv("LASTABLES");
if (table_ptr == NULL)
   {
   printf("Global variable LASTABLES not defined\n");
   return(E_FAIL);
   } 

* Add the file names
  ------------------ *
sprintf(dtmfile,"%s/datum.txt",table_ptr);
sprintf(spherfile,"%s/spheroid.txt",table_ptr); 
****************************************************************
********moved these two files into the program directory********
************************JUST OPEN******************************/
/* /3dsar/swatts/mytools/src/las2geotiff/ */


/* Open the datum file.  Retrieve the necessary data
  -------------------------------------------------*/
file_ptr = FOPEN("./datum.txt", "r");
    
/* If datum is present, retrieve information from datum table
  ----------------------------------------------------------*/
if (dtmflag)
   {
   while ((fgets(line,BUFSIZ,file_ptr) != NULL) && (found != TRUE))
         {
         sscanf(line, "%d", &numb);
         if (numb == dtm_num)
            {
            for (i = 0; i < 11; i++)
                {
                temp = strtok(i ? NULL: line, ":");
                dtmary[i] = malloc(strlen(temp) + 1);
                strcpy(dtmary[i], temp);
                }
            strcpy(dtm_info->datumname, dtmary[1]);
            strcpy(dtm_info->area, dtmary[2]);
            strcpy(dtm_info->category, dtmary[3]);
            dtm_info->spherenum = atol(dtmary[4]);
            dtm_info->xshift = atol(dtmary[5]);
            dtm_info->yshift = atol(dtmary[6]);
            dtm_info->zshift = atol(dtmary[7]);
            dtm_info->xdelta = atol(dtmary[8]);
            dtm_info->ydelta = atol(dtmary[9]);
            dtm_info->zdelta = atol(dtmary[10]);
            found = TRUE;
            }
         }
   if (found == FALSE)
      {
      printf("\nCould not read datum values\n");
      return (E_FAIL);
      }
   }
/* Since there is no datum, assign sphere numb and set everything else to zero
  ---------------------------------------------------------------------------*/
else
   {
   dtm_info->spherenum = dtm_num;
   dtm_info->datumname[0] = '\0';
   dtm_info->area[0] = '\0';
   dtm_info->category[0] = '\0';
   dtm_info->xshift = 0;
   dtm_info->yshift = 0;
   dtm_info->zshift = 0;
   dtm_info->xdelta = 0;
   dtm_info->ydelta = 0;
   dtm_info->zdelta = 0;
   }

fclose(file_ptr);

/* Open the spheroid file.  Retrieve the necessary data
  ----------------------------------------------------*/
file_ptr=FOPEN("./spheroid.txt", "r");

found = FALSE;
while ((fgets(line,BUFSIZ,file_ptr) != NULL) && (found != TRUE))
      {
      sscanf(line, "%d", &numb);
      if (numb == dtm_info->spherenum)
         {
         for (i = 0; i < 4; i++)
             {
             temp = strtok(i ? NULL: line, ":");
             spherary[i] = malloc(strlen(temp) + 1);
             strcpy(spherary[i], temp);
             }
         strcpy(dtm_info->spherename, spherary[1]);
         dtm_info->smajor = atof(spherary[2]);
         dtm_info->recip_flat = atof(spherary[3]);

         /* If the recip_flat is zero it is a sphere, otherwise compute minor
          ------------------------------------------------------------------*/
         if (dtm_info->recip_flat != 0)
            {
            flat = (1/dtm_info->recip_flat);
            dtm_info->sminor = (dtm_info->smajor * (1 - flat));
            }
         else
            dtm_info->sminor = atof(spherary[2]);
         found = TRUE;
         }
      }
if (found == FALSE)
   {
   printf("\nProblem reading Spheroid.txt\n");
   return (E_FAIL);
   }
fclose(file_ptr);

/* Free malloc'd memory
-----------------------*/
for ( i = 0; i < 10; i++ )
    if ( dtmary[i] != NULL )
	free( dtmary[i] );
for ( i = 0; i < 4; i++ )
    if ( spherary[i] != NULL )
	free( spherary[i] );

return(E_SUCC);
}
