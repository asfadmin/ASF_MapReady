/*******************************************************************************
NAME			       SET_CAPTURE

PURPOSE	     Compare the fields related to the capture process of the band 
	     dependent records of two or more input DDR files

EXTERNAL ASSOCIATIONS
	This routine is called by C_UPDDR routine

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replace newlas.h with las.h
					Changed the calling sequence -- only
					  one structure
					Combined the set______ routines that
					pertain to the capture fields of the
					DDR into one routine
					
PROJECT       LAS

ALGORITHM 
   Compare the date field of each of the input BDDR structures
   If the input fields are not equal
      Place a blank in the output date field
   Else
      Copy the input date field to the output date field
   Endif

   Compare the direction field of each of the input BDDR structures
   If the input fields are not equal
      Place a blank in the output direction field
   Else
      Copy the input direction field to the output direction field
   Endif

   Compare the instrument field of each of the input BDDR structures
   If the input fields are not equal
      Place a blank in the output instrument field
   Else
      Copy the input instrument field to the output instrument field
   Endif

   Compare the source field of each of the input BDDR structures
   If the input fields are not equal
      Place a blank in the output source field
   Else
      Copy the input source field to the output source field
   Endif

   Compare the time field of each of the input BDDR structures
   If the input fields are not equal
      Place a blank in the output time field
   Else
      Copy the input time field to the output time field
   Endif

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "las.h"

void FUNCTION set_capture(const struct BDDR in_bddr[],struct BDDR *out_bddr,int nimg)
{ /* set_capture */

int i;				/* counter -- current image		*/
int invalid;			/* invalid flag				*/

/* Compare the input date fields with one another -- if they are not the 
   same invalididate the output field, otherwise copy it
--------------------------------------------------------------------------*/
invalid = FALSE;
for (i = 0; (i < nimg - 1) && (invalid != TRUE); i++)
   if (strcmp((in_bddr + i)->date,(in_bddr + i + 1)->date) != 0)
      invalid = TRUE;

if (invalid)
   strcpy (out_bddr->date," ");
else
   strcpy (out_bddr->date,in_bddr->date);

/* Compare the input directions fields with one another -- if they are not the 
   same invalididate the output field, otherwise copy it
--------------------------------------------------------------------------*/
invalid = FALSE;
for (i = 0; (i < nimg - 1) && (invalid != TRUE); i++)
   if (strcmp((in_bddr + i)->direction,(in_bddr + i + 1)->direction) != 0)
      invalid = TRUE;

if (invalid)
   strcpy (out_bddr->direction," ");
else
   strcpy (out_bddr->direction,in_bddr->direction);

/* Compare the input instrument fields with one another -- if they are not the 
   same invalididate the output field, otherwise copy it
--------------------------------------------------------------------------*/
invalid = FALSE;
for (i = 0; (i < nimg - 1) && (invalid != TRUE); i++)
   if (strcmp((in_bddr + i)->instrument,(in_bddr + i + 1)->instrument) != 0)
      invalid = TRUE;

if (invalid)
   strcpy (out_bddr->instrument," ");
else
   strcpy (out_bddr->instrument,in_bddr->instrument);

/* Compare the input source fields with one another -- if they are not the 
   same invalididate the output field, otherwise copy it
--------------------------------------------------------------------------*/
invalid = FALSE;
for (i = 0; (i < nimg - 1) && (invalid != TRUE); i++)
   if (strcmp((in_bddr + i)->source,(in_bddr + i + 1)->source) != 0)
      invalid = TRUE;

if (invalid)
   strcpy (out_bddr->source," ");
else
   strcpy (out_bddr->source,in_bddr->source);

/* Compare the input time fields with one another -- if they are not the 
   same invalididate the output field, otherwise copy it
--------------------------------------------------------------------------*/
invalid = FALSE;
for (i = 0; (i < nimg - 1) && (invalid != TRUE); i++)
   if (strcmp((in_bddr + i)->time,(in_bddr + i + 1)->time) != 0)
      invalid = TRUE;

if (invalid)
   strcpy (out_bddr->time," ");
else
   strcpy (out_bddr->time,in_bddr->time);

return;
} /* set_capture */
