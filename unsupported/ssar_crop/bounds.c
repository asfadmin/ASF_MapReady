/****************************************************************
FUNCTION NAME: bounds - determines image bounding box

SYNTAX:	bounds(buf, nl, np, &topLine, &bottomLine, &leftSamp, &rightSamp) 

PARAMETERS:
    NAME:	TYPE:		  PURPOSE:
    --------------------------------------------------------
    buf		unsigned char *	  Image buffer
    nl		int		  number of lines in image
    np		int		  number of samples in image
    topLine	int *		  return top line
    bottomLine  int *		  return bottom line
    leftSamp	int *		  return left sample
    rightSamp   int * 		  return right sample

DESCRIPTION:

    Scans from outside edges of image buffer looking for first non-zero
    pixel.   

RETURN VALUE:

    topLine, bottomLine, leftSamp, rightSamp creating bounding box for
    valid (non-zero) image data.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY: Cretaed by T. Logan 12/98

****************************************************************/
void bounds(unsigned char *buf,
	    int nl, int np,
	    int *topLine,
            int *bottomLine,
            int *leftSamp,
            int *rightSamp)  
{
   int  line, samp;
   int  all_zeros, offset, i, j;
   int  top_line, bottom_line, right_samp, left_samp;

   /* search for top edge */
   /* printf("Scanning for top edge "); */
   line = -1;
   all_zeros = 1;
   while (all_zeros)
     {
        line++;
        offset = line * np;
        for (j = 0; j<np; j++) if (buf[j+offset]) all_zeros = 0;
     }
   top_line = line;
   /* printf("- found at line %i\n",top_line); */

   /* search for bottom edge */
   /* printf("Scanning for bottom edge "); */
   line = nl;
   all_zeros = 1;
   while (all_zeros)
     {
        line--;
        offset = line * np;
        for (j = 0; j<np; j++) if (buf[j+offset]) all_zeros = 0;
     }
   bottom_line = line;
   /* printf("- found at line %i\n",bottom_line); */

   /* search for left edge */
   /* printf("Scanning for left edge "); */
   samp = -1;
   all_zeros = 1;
   while (all_zeros)
     {
        samp++;
        for (i = 0; i<nl; i++) if (buf[i*np+samp]) all_zeros = 0;
     }
   left_samp = samp;
   /* printf("- found at sample %i\n",left_samp); */

   /* search for right edge */
   /* printf("Scanning for right edge "); */
   samp = np;
   all_zeros = 1;
   while (all_zeros)
     {
        samp--;
        for (i = 0; i<nl; i++) if (buf[i*np+samp]) all_zeros = 0;
     }
   right_samp = samp;
   /* printf("- found at sample %i\n",right_samp); */

   *topLine = top_line;
   *bottomLine = bottom_line;
   *leftSamp = left_samp;
   *rightSamp = right_samp;
}
