/****************************************************************
FUNCTION NAME: find_corners - search an image for corner locations

SYNTAX:
   find_corners(unsigned char *buf, int nl, int np,
		int *tlc_line, int *tlc_samp,
		int *trc_line, int *trc_samp,
		int *blc_line, int *blc_samp,
		int *brc_line, int *brc_samp)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    buf		unsigned char * Contains input image to search
    nl		int		Number of lines in image
    np		int		Number of pixels per line in image
    tlc_line    int *		Returned top left corner line
    tlc_samp    int *		Returned top left corner samp
    trc_line    int *		Returned top right corner line
    trc_samp    int *		Returned top right corner samp
    blc_line    int *		Returned bottom left corner line
    blc_samp    int *		Returned bottom left corner samp
    brc_line    int *		Returned bottom right corner line
    brc_samp    int *		Returned bottom right corner samp

DESCRIPTION:

	The algorithm is based on a search of the left and right edges
    of the input image.  By placing a kernel at each edge pixel and 
    calculating the percent of the kernel that is filled with valid 
    image data, we can quickly determine corner locations.  This comes
    from the fact that a kernel placed at an image edge will be, on average,
    half filled with the image.  While, a kernel placed at an image corner
    will be, on average, about a quarter filled with image data:

    -------------------			-------------------
    |                *|			|		  |
    |               **|			|		  |
    |             ****|			|		  |
    |           ******|                 |                 |
    |         ********|                 |                 |
    |        *********|                 |        *********|
    |      ***********|                 |        *********|
    |    *************|                 |        *********|
    |   **************|                 |        *********|
    | ****************|                 |        *********|
    |*****************|                 |        *********|
    -------------------			-------------------

         Edge					Corner

    The algorithm flow is:
	1) find left and right edge vectors for the image
	2) scan left edge for possible corner locations
		- place a kernel centered at the edge location
		- calculate the percentage of non-zero pixels in the kernel
		- if % < MAX_CORNER_PERCENT, location is possible corner
        3) scan right edge for possible corner locations (same as step 2)
	4) sort possible corner locations by area filled
	5) select 4 distinct corner locations with minimum area filled
	6) sort the four corners by quadrants, top & bottom and left & right
	7) return selected corners

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "caplib.h"

#define SQR(a)			((a)*(a))
#define	MAX_CORNER_PERCENT	0.34


void bounds(unsigned char *buf, int nl, int np, int *topLine, int *bottomLine,
            int *leftSamp, int *rightSamp);

find_corners(unsigned char *buf, int nl, int np,
	int *tlc_line, int *tlc_samp, int *trc_line, int *trc_samp,
	int *blc_line, int *blc_samp, int *brc_line, int *brc_samp)
{
   int top_line, bottom_line, left_samp, right_samp;
   int *left_col, *right_col;
   int width = 8;
   int kernel = 17;
   int cnt;
   int line,i,j,total; 
   int save_line, save_samp, save_spot;
   int save = 0;
   int lines[100],samps[100];
   float areas[100];
   float area, min_area = 1.0;
   int cornerLine[4], cornerSamp[4];
   int repeat;
   int min_line;
 
   /* Get bounding box for valid image data
    --------------------------------------*/
   bounds(buf,nl,np,&top_line,&bottom_line,&left_samp,&right_samp);

   /* Determine the left and right edge vectors of the image 
    -------------------------------------------------------*/
   left_col  = (int *) MALLOC (nl*sizeof(int));
   right_col = (int *) MALLOC (nl*sizeof(int));

   /* fill left_col */
   for (j=0; j<nl; j++) {
      if (j<top_line || j>bottom_line) left_col[j] = -99;
      else { i = left_samp; while (buf[j*np+i] == 0) i++; left_col[j] = i; }
   }

   /* fill right_col */
   for (j = 0; j<nl; j++) {
      if (j<top_line || j>bottom_line) right_col[j] = -99;
      else { i = right_samp; while (buf[j*np+i] == 0) i--; right_col[j] = i; }
   }

   /* Look down the left side of the image for possible corners 
    ----------------------------------------------------------*/
   for (line=top_line; line<=bottom_line; line++)
    {
     total = 0;
     for (i=line-width; i<=line+width; i++)
      for (j=left_col[line]-width; j<=left_col[line]+width; j++)
        if (j<np && i<nl && j>=0 && i>= 0)
         {
          if (buf[i*np+j]) total++;
         }
     area = (float) total / (float) (kernel*kernel);
     if (area < MAX_CORNER_PERCENT)  
     {
      lines[save] = line; samps[save] = left_col[line];
      areas[save] = area;
      save++;
     }
    }

   /* Look down the right side of the image for possible corners 
    -----------------------------------------------------------*/
   for (line=top_line; line<=bottom_line; line++)
    {
     total = 0;
     for (i=line-width; i<=line+width; i++)
      for (j=right_col[line]-width; j<=right_col[line]+width; j++)
        if (j<np && i<nl && j>=0 && i>= 0)
         {
          if (buf[i*np+j]) total++;
         }
     area = (float) total/ (float) (kernel*kernel);
     if (area < MAX_CORNER_PERCENT)  
     {
      lines[save] = line; samps[save] = right_col[line];
      areas[save] = area;
      save++;
     }
    }

   /* sort possible corners by area */
   for (i=0; i<save; i++)
    {
       min_area = 1.0;
       for (j=i; j<save; j++)
         if (areas[j] < min_area) { save_spot = j; min_area = areas[j]; }
       save_line = lines[save_spot]; save_samp = samps[save_spot];
       lines[save_spot]=lines[i]; samps[save_spot]=samps[i];
       areas[save_spot]=areas[i];
       lines[i] = save_line; samps[i] = save_samp;
       areas[i] = min_area;
    }

   /* save the first corner in the list */
   cornerLine[0] = lines[0]; cornerSamp[0] = samps[0];

   /* select the other 3 corners from the list */
   cnt = 1;
   i = 1;
   while (cnt<4 && i < save)
     {
      repeat = 0;
      for (j=0; j<cnt; j++)
       {
        if(sqrt(SQR(lines[i]-cornerLine[j])+SQR(samps[i]-cornerSamp[j]))<375.0)
	   repeat = 1;
       }
      if (!repeat) { cornerLine[cnt]=lines[i]; cornerSamp[cnt]=samps[i]; cnt++;}
      i++;
     }

   if (cnt != 4)
    {
     printf("\nFIND_CORNERS: Unable determine 4 distinct corner locations\n");
     printf("FATAL ERROR - PROGRAM ABORTING\n\n");
     exit(1);
    }

   /* Sort selected corners by line to determine top & bottom */
   for (i=0; i<4; i++)
     {
        min_line = 1000;
        for (j=i; j<4; j++)
          if (cornerLine[j]< min_line) { min_line = cornerLine[j]; save_spot=j;}
        save_line = cornerLine[save_spot]; save_samp = cornerSamp[save_spot];
        cornerLine[save_spot]=cornerLine[i];cornerSamp[save_spot]=cornerSamp[i];
        cornerLine[i] = save_line; cornerSamp[i] = save_samp;
     }

   /* Determine left and right for top corners */
   if (cornerSamp[0] > cornerSamp[1]) 
     {
	save_line = cornerLine[0]; save_samp = cornerSamp[0];
	cornerLine[0] = cornerLine[1]; cornerSamp[0] = cornerSamp[1];
	cornerLine[1] = save_line; cornerSamp[1] = save_samp;
     }

   /* Determine left and right for bottom corners */
   if (cornerSamp[2] > cornerSamp[3]) 
     {
	save_line = cornerLine[2]; save_samp = cornerSamp[2];
	cornerLine[2] = cornerLine[3]; cornerSamp[2] = cornerSamp[3];
	cornerLine[3] = save_line; cornerSamp[3] = save_samp;
     }

   *tlc_line = cornerLine[0]; *tlc_samp = cornerSamp[0];
   *trc_line = cornerLine[1]; *trc_samp = cornerSamp[1];
   *blc_line = cornerLine[2]; *blc_samp = cornerSamp[2];
   *brc_line = cornerLine[3]; *brc_samp = cornerSamp[3];

   FREE(left_col);
   FREE(right_col);

   return (0);
}
