/****************************************************************
FUNCTION NAMES:

	find_tlc - find top left corner in image
	find_trc - find top right corner in image
	find_blc - find bottom left corner in image
	find_brc - find bottom right corner in image

SYNTAX:	

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

    Each of these routines work on the same principles:

	Calculate a weighting chip based on the corner type and rotation
	Extract an image chip at the estimated corner location
	Overlay the image chip on the weighting chip
		zero out weights that correspond to blank image pixels
		keep weights that correspond to valid image pixels
	Highest weight value remaining is the true image corner location

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:  Version 1.0 	12/98  T. Logan

****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "caplib.h"

void display_chip(int *chip, int size, char *name);
int *tbuf, *cbuf, *sbuf;
int x,y,high,ind;
int width;
int i,j;

void find_tlc(unsigned char *buf, int nl, int np, int kernel,
        int tlc_line, int tlc_samp, int *ret_line, int *ret_samp,
	double rotation)
 {
   tbuf = (int *) MALLOC (kernel*kernel*sizeof(int));
   cbuf = (int *) MALLOC (kernel*kernel*sizeof(int));
   sbuf = (int *) MALLOC (kernel*kernel*sizeof(int));

   /* create the scan buffer */
   for (i=0; i<kernel; i++)
    for (j=0; j<kernel; j++)
      {
        sbuf[i*kernel+j] = (kernel-i) + (kernel-j);
        if (rotation > 45.0 && rotation < 70.0) sbuf[i*kernel+j]+=(kernel-j);
      }	
   /* display_chip(sbuf,kernel,"Scan Buffer:");  */
   width = (kernel-1)/2;

   /* extract top left corner chip */
   for (y=0,i=tlc_line-width; i<=tlc_line+width; i++,y++)
     for (x=0,j=tlc_samp-width; j<=tlc_samp+width; j++,x++)
	if (j<np && i<nl && j>=0 && i>= 0)
          cbuf[y*kernel+x] = (int) buf[i*np+j];
        else
	  cbuf[y*kernel+x] = 0;
   /* display_chip(cbuf,kernel,"Chip Buffer:"); */

   /* multiply chip buff and scan buff */
   for (y=0; y<kernel*kernel; y++)
     if (cbuf[y]) tbuf[y] = sbuf[y]; else tbuf[y] = 0;
   /* display_chip(tbuf,kernel,"Multiplied Buffers:"); */

   /* search the multiplied buffer for corner */
   high = 0; ind = -99;
   for (y=0; y<kernel*kernel; y++) if (tbuf[y]>high){high=tbuf[y];ind=y;}
   if (ind==-99)
    { 
      printf("\n\n\n**********************************************\n");
      printf("The chip extracted is all zeros\n");
      printf("Cannot continue under these circumstances\n");
      printf("**********************************************\n\n");
      exit(1);
    }

   *ret_line = (int) (ind/kernel) + (tlc_line-width);
   *ret_samp = (ind % kernel) + (tlc_samp-width);
}

void find_trc(unsigned char *buf, int nl, int np, int kernel,
              int trc_line, int trc_samp, int *ret_line, int *ret_samp,
	double rotation)
 {
   for (i=0; i<kernel; i++)
    for (j=0; j<kernel; j++)
     {
      sbuf[i*kernel+j] = (kernel-i) + (j+1);
      if (rotation > 45.0 && rotation < 70.0) sbuf[i*kernel+j]+=(kernel-i);
     }
   /* display_chip(sbuf,kernel,"Scan Buffer:"); */

   /* extract top right corner chip */
   for (y=0,i=trc_line-width; i<=trc_line+width; i++,y++)
     for (x=0,j=trc_samp-width; j<=trc_samp+width; j++,x++)
	if (j<np && i<nl && j>=0 && i>= 0)
          cbuf[y*kernel+x] = (int) buf[i*np+j];
        else
	  cbuf[y*kernel+x] = 0;
   /* display_chip(cbuf,kernel,"Chip Buffer:"); */

   /* multiply chip buff and scan buff
    ---------------------------------*/
   for (y=0; y<kernel*kernel; y++)
     if (cbuf[y]) tbuf[y] = sbuf[y]; else tbuf[y] = 0;
   /* display_chip(tbuf,kernel,"Multiplied Buffers:"); */

   high = 0; ind = -99;
   for (y=0; y<kernel*kernel; y++)
      if (tbuf[y] > high) { high = tbuf[y]; ind = y; }
   if (ind==-99)
    { 
      printf("\n\n\n**********************************************\n");
      printf("The chip extracted is all zeros\n");
      printf("Cannot continue under these circumstances\n");
      printf("**********************************************\n\n");
      exit(1);
    }

   *ret_line = (int) (ind/kernel) + (trc_line-width);
   *ret_samp = (ind % kernel) + (trc_samp-width);
}


void find_blc(unsigned char *buf, int nl, int np, int kernel,
              int blc_line, int blc_samp, int *ret_line, int *ret_samp,
	double rotation)
 {
   for (i=0; i<kernel; i++)
    for (j=0; j<kernel; j++)
     {
      sbuf[i*kernel+j] = (i+1) + (kernel-j); 
      if (rotation > 45.0 && rotation < 70.0) sbuf[i*kernel+j]+=i;
     }

   /* display_chip(sbuf,kernel,"Scan Buffer:"); */

   /* extract bottom left corner chip */
   for (y=0,i=blc_line-width; i<=blc_line+width; i++,y++)
     for (x=0,j=blc_samp-width; j<=blc_samp+width; j++,x++)
	if (j<np && i<nl && j>=0 && i>= 0)
          cbuf[y*kernel+x] = (int) buf[i*np+j];
        else
	  cbuf[y*kernel+x] = 0;
   /* display_chip(cbuf,kernel,"Chip Buffer:"); */

   /* multiply chip buff and scan buff
    ---------------------------------*/
   for (y=0; y<kernel*kernel; y++)
     if (cbuf[y]) tbuf[y] = sbuf[y]; else tbuf[y] = 0;
   /* display_chip(tbuf,kernel,"Multiplied Buffers:"); */

   high = 0; ind = -99;
   for (y=0; y<kernel*kernel; y++)
      if (tbuf[y] >= high) { high = tbuf[y]; ind = y; }
   if (ind==-99)
    { 
      printf("\n\n\n**********************************************\n");
      printf("The chip extracted is all zeros\n");
      printf("Cannot continue under these circumstances\n");
      printf("**********************************************\n\n");
      exit(1);
    }

   *ret_line = (int) (ind/kernel) + (blc_line-width);
   *ret_samp = (ind % kernel) + (blc_samp-width);
}

void find_brc(unsigned char *buf, int nl, int np, int kernel,
              int brc_line, int brc_samp, int *ret_line, int *ret_samp,
	double rotation)
 {
   for (i=0; i<kernel; i++)
    for (j=0; j<kernel; j++)
     {
      sbuf[i*kernel+j] = (i+1) + (j+1);
      if (rotation > 45.0 && rotation < 70.0) sbuf[i*kernel+j] += j;
     }
   /* display_chip(sbuf,kernel,"Scan Buffer:"); */

   /* extract bottom right corner chip */
   for (y=0,i=brc_line-width; i<=brc_line+width; i++,y++)
     for (x=0,j=brc_samp-width; j<=brc_samp+width; j++,x++)
	if (j<np && i<nl && j>=0 && i>= 0)
          cbuf[y*kernel+x] = (int) buf[i*np+j];
        else
	  cbuf[y*kernel+x] = 0;
   /* display_chip(cbuf,kernel,"Chip Buffer:"); */

   /* multiply chip buff and scan buff
    ---------------------------------*/
   for (y=0; y<kernel*kernel; y++)
     if (cbuf[y]) tbuf[y] = sbuf[y]; else tbuf[y] = 0;
   /* display_chip(tbuf,kernel,"Multiplied Buffers:"); */

   high = 0; ind = -99;
   for (y=0; y<kernel*kernel; y++)
      if (tbuf[y] > high) { high = tbuf[y]; ind = y; }
   if (ind==-99)
    { 
      printf("\n\n\n**********************************************\n");
      printf("The chip extracted is all zeros\n");
      printf("Cannot continue under these circumstances\n");
      printf("**********************************************\n\n");
      exit(1);
    }

   *ret_line = (int) (ind/kernel) + (brc_line-width);
   *ret_samp = (ind % kernel) + (brc_samp-width);

   free(tbuf); free(sbuf); free(cbuf);
}

void display_chip(int *chip, int size, char *name)
 {
   printf("%s\n",name);
   for (i=0; i<size; i++) {
     for (j=0; j<size; j++) printf("%4i",chip[i*size+j]);
     printf("\n");
   }
 }
