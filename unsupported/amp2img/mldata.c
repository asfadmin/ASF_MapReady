/****************************************************************
FUNCTION NAME: mldata

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"


#include "ifm.h"
#include "asf_meta.h"
#include "amp2img.h"

void mldata(FILE *fpin, FILE *fpout,
            int extraArea,
            int stepArea,
	    int ll, int ls,           /* lookline and looksample */
	    int sl, int ss,           /* stepline and stepsample */
            float *ibuf,
	    unsigned char *obuf,
	    int len, int wid,
	    int obuflen,
	    float mult)
{
   int col;
   float tmp;
   int i, j;
   int row_off;
   int lines;
   int count=0;
   int olines=0;
   int nPercent=5;
   float *ftmp;
   int lookArea = ll * ls;
   ftmp = (float *) MALLOC (obuflen*sizeof(float));  
   lines = fread(ibuf,sizeof(float)*wid,ll,fpin);
   while (lines>0)
    {
     /* Zero out temp buffer */
     for (count=0; count < obuflen; ftmp[count++]=0.0);

     /* Loop over lines, then samples */
     for (i=0, row_off=0 ; i<lines; i++, row_off+=wid)
      for (j=0, count=0; j<wid; j+=ss, count++)
       /* Sum the squares for this kernel */
       for (col=j;col<j+ss&&col<wid;col++) ftmp[count]+=SQR(ibuf[row_off+col]);

     /* convert sums of squares to output bytes */
     for (count=0; count < obuflen; count++)
      {
        tmp = sqrt(ftmp[count]/lookArea)*mult+0.5;
        if (tmp<255.0) obuf[count]=(unsigned char) tmp; else obuf[count]=255;
      }
     fwrite(obuf,sizeof(unsigned char),obuflen,fpout);

     olines++;
     if (((olines*100/(len/sl))==nPercent) && (!quietflag)){ 
	printf("   Completed %3d percent\n", nPercent);
	nPercent+=5;
     }

     for (i=stepArea, j=0; j<extraArea; i++,j++) ibuf[j]=ibuf[i];
  
     lines = fread(&ibuf[j],sizeof(float)*wid,sl,fpin);
     if (lines) lines += (ll-sl); /* if more data was read */ 
     else /* if no more data is being read */
       { lines = ll-sl; ll -= sl; extraArea = (ll-sl)*wid; }
   }

   printf("\n   Wrote %d lines of data\n\n",olines);
   if (logflag) {
     sprintf(logbuf,"   Wrote %d lines of data\n\n",olines);
     printLog(logbuf);
   }
   return;
}

