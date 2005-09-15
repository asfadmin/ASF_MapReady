/****************************************************************
FUNCTION NAME: colortable

SYNTAX: colortable(table);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    table	RGBDATA *	pointer to structure containing colortable 
				RGB values

DESCRIPTION:
	Create a colortable containg the RGB values for a specific byte
	value. This function will fill the first MAXENTRIES of that table.

RETURN VALUE:
	A colortable pointed to by table.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
1.0 - Mike Shindle - Original Development
****************************************************************/
#include "ifm.h"
#include "pml.h"

void 
colortable(RGBDATA *table) {
   int i, n, p;

   for (i=0; i<MAXENTRIES; i++) {
      p = i / 42;     /* this is integer division */
      n = i - 42*p;   /* multiply result by 42 */
      
      /* go through sections */
      if (i < 42) {
	 table[i].red = (Uchar)(n * 6);
	 table[i].green = 0;
	 table[i].blue = 0;
      } else if (i < 84) {
	 table[i].red = (Uchar)(n * 6);
	 table[i].green = (Uchar)(n * 6);
	 table[i].blue = 0;
      } else if (i < 126) {
	 table[i].red = 0;
	 table[i].green = (Uchar)(n * 6);
	 table[i].blue = 0;
      } else if (i < 168) {
	 table[i].red = 0;
	 table[i].green = (Uchar)(n * 6);
	 table[i].blue = (Uchar)(n * 6);
      } else if (i < 210) {
	 table[i].red = 0;
	 table[i].green = 0;
	 table[i].blue = (Uchar)(n * 6);
      } else if (i < 252) {
	 table[i].red = (Uchar)(n * 6);
	 table[i].green = 0;
	 table[i].blue = (Uchar)(n* 6);
      } else {
	 table[i].red = 0;
	 table[i].green = 0;
	 table[i].blue = 0;
      }
   }

   return;
}

