/****************************************************************
NAME:  hist -- counts values for mask images and DEMs

SYNOPSIS:  hist DEMfile [1 2]
		1   byte file	     (mask image)
		2   integer*2 file   (DEM)
DESCRIPTION:

******************************************************************/


#include "asf.h"
#include "asf_meta.h"

int main(int argc, char **argv)
{
	char inDEM[255];        /* Input DEM file name              */
	char inDDR[255];        /* Input DDR file name              */
	FILE *fpi;              /* Input DEM file pointer           */
	struct DDR ddr;         /* Data Descriptor Record structure */
	int  nl,ns,dType=0;     /* Number lines, samples in DEM     */
	int  index, hist[3]; 
	unsigned char  *obuf;   /* I/O buffer for transfer data     */

	StartWatch();

	if (argc != 3) {
	    printf("\nUsage: %s inDEM [1 2]\n\n",argv[0]);
	    printf("  inDEM is the DEM file or mask image.\n");
	    printf("  If 1 is specified then %s expects a mask image.\n",argv[0]);
	    printf("  If 2 is specified then %s expects a DEM image.\n\n",argv[0]);
            exit(1);
	}

	strcat(strcpy(inDEM,argv[1]),".img");
	strcat(strcpy(inDDR,argv[1]),".ddr");
	switch (argv[2][0]) {
	    case '1': dType=1; break;
	    case '2': dType=2; break;
	    default : printf("ERROR: incorrect option, %s\n",argv[2]);
	}

	/*  Read input DEM DDR file */
	if (c_getddr(inDEM,&ddr) != 0) {
		printf("Error returned from c_getddr:  unable to read file %s\n",inDEM);
		exit(1);
	}

	nl = ddr.nl;
	ns = ddr.ns;

	printf("Image size, nl x ns:  %i x %i\n",nl,ns);
	printf("Total number of pixels:  %d\n",nl*ns);

	obuf = (unsigned char *) MALLOC (nl*ns*dType);
	fpi = fopenImage(inDEM,"rb");

	for (index=0; index < nl*ns*dType; index++) obuf[index] = 0;

	if (fread(&obuf[0],nl*ns*dType,1,fpi) != 1) {
		printf("Unable to read the input DEM file\n");
		exit(1);
	}

	for (index=0; index<3; index++) hist[index]=0;

	if (dType==1) {
		for (index=0; index < nl*ns; index++) {
			if (obuf[index]==0)   hist[0]++;
			if (obuf[index]==100) hist[1]++;
			if (obuf[index]==200) hist[2]++;
		}
		printf("  0:\t%d\n",hist[0]);
		printf("100:\t%d\n",hist[1]);
		printf("200:\t%d\n",hist[2]);
	}
	if (dType==2) {
		for (index=0; index < nl*ns*dType; index+=2) {
			if (obuf[index]==0 && obuf[index+1]==0) hist[0]++;
			else hist[1]++;
		}
		printf("    zero:\t%d\n",hist[0]);
		printf("non-zero:\t%d\n",hist[1]);
	}

	fclose(fpi);
	free(obuf);

	StopWatch();
	return(0);
}
