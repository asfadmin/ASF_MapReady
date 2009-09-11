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

AUTHOR:
   Michael Shindle

PROGRAM HISTORY:
1.0 - Mike Shindle - Original Development, December 11, 1996.
****************************************************************/
#include "ifm.h"
#include "ifm2ppm.h"

/* local constants */
#define MAXENTRIES    256
#define LINE_BUFFER   128

/* local function declarations */
void interp(RGBDATA *,int,int);

void igram_colortable(RGBDATA *table) {
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

void grey_colortable(RGBDATA *table) {
   int i;

   for (i=0; i<MAXENTRIES; i++) {
	 table[i].red = (Uchar)(i);
	 table[i].green = (Uchar)(i);
	 table[i].blue = (Uchar)(i);
   }

   return;
}

void 
mask_colortable(RGBDATA *table)
{
  int i;
  int ntable = 256;

  /* initialize everything to zero
   * if its not specified below, it has no meaning
   */
  for (i = 0; i < ntable; i++) {
     table[i].red = 0;
     table[i].green = 0;
     table[i].blue = 0;
  }

  /* anything with the 'integrated' 4 bit set is yellow */
  for (i = 16; i < 64; i++) {
    table[i].red = 255;
    table[i].green = 255;
    table[i].blue = 100;
  }

  /* anything 'in tree' with the 5 bit set is green */
  for (i = 32; i < 64; i++) {
    table[i].red = 0;
    table[i].green = 255;
    table[i].blue = 0;
  }

  /* anything with the 'grounded' 3 bit set is black         */
  /*   (don't need to specify, as this is the default color) */

  /* anything which is untouched ('zero') is white */
  table[0].red =  255;
  table[0].green = 255;
  table[0].blue = 255;

  /* + charge, uncut = 1; color = red + 1/4 green */
  table[1].red = 255;
  table[1].green = 64;
  table[1].blue = 0;

  /* + charge, uncut = 1; color = red + 1/4 green */
  table[1].red =  255;
  table[1].green = 64;
  table[1].blue = 0;

  /* + charge, cut = 5; color = red */
  table[5].red = 255;
  table[5].green = 0;
  table[5].blue = 0;

  /* - charge, uncut = 2; color = blue + 1/4 green*/
  table[2].red = 0;
  table[2].green = 64;
  table[2].blue = 255;

  /* - charge, cut = 6; color = blue */
  table[6].red = 0;
  table[6].green = 0;
  table[6].blue = 255;

  /* uncharged cut pixels are purple */
  table[4].red = 255;
  table[4].green = 0;
  table[4].blue = 255;

  return;
}

void 
user_colortable(RGBDATA *table, char *fpal)
{
  FILE *fp;
  int r0, g0, b0;
  int r, g, b;
  int j, j0;
  char txt[LINE_BUFFER];

  /* open file & get number of lines */
  fp = FOPEN(fpal,"r");

  /* init. variables */
  r0 = g0 = b0 = 0;
  r = g = b = 0;
  j = j0 = 0;

  while (fgets(txt,LINE_BUFFER,fp) != NULL) {  
     sscanf(txt,"%d %d %d %d", &j,&r,&g,&b);
     if (j >= MAXENTRIES) break;
     if (j0 <= j) {
	table[j0].red = (Uchar)r0;
	table[j0].green = (Uchar)g0;
	table[j0].blue = (Uchar)b0;

	table[j].red = (Uchar)r;
	table[j].green = (Uchar)g;
	table[j].blue = (Uchar)b;

	if (abs(j-j0) > 1) interp(table,j0,j);
     }
     j0=j; r0=r; g0=g; b0=b;
  }
  fclose(fp);
}

/*
   Interp() takes a pointer to an array of RGBDATA values and two indicies
   to this array. It leaves the RGB specification at each index intact and
   fills the RGB values between them by linear interpolation.
*/
void
interp(RGBDATA *table, int start, int end)
{
  int i;
  float r, g, b;

  r =  (float)(table[end].red - table[start].red) / (float)(end - start);
  g =  (float)(table[end].green - table[start].green) / (float)(end - start);
  b =  (float)(table[end].blue - table[start].blue) / (float)(end - start);

  for (i=start+1; i < end; i++) {
    table[i].red = (Uchar)((r * (i-start) + 0.5) + table[start].red);
    table[i].green = (Uchar)((g * (i-start) + 0.5) + table[start].green);
    table[i].blue = (Uchar)((b * (i-start) + 0.5) + table[start].blue);
  }
}

/*
  Writes out the colortable
*/
void
write_table(RGBDATA *table,char *fname)
{
  int i;
  FILE *fp;

  fp = FOPEN(fname,"w");

  for (i=0;i<MAXENTRIES;i++) {
    fprintf(fp,"%3d %3d %3d %3d\n",
	    i,table[i].red,table[i].green,table[i].blue);
  }
  fclose(fp);
}


