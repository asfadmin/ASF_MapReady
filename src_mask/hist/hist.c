/****************************************************************
NAME:  hist -- counts values for mask images and DEMs

SYNOPSIS:  hist DEMfile [1 2]
		1   byte file	     (mask image)
		2   integer*2 file   (DEM)
DESCRIPTION:

******************************************************************/


#include "asf.h"
#include "ddr.h"

int main(argc,argv)
    int argc; 
    char **argv;
  {
    char inDEM[255];		/* Input DEM file name 			*/
    char inDDR[255];		/* Input DDR file name 			*/
    FILE *fpi;			/* Input DEM file pointer 		*/
    struct DDR ddr;		/* Data Descriptor Record structure 	*/
    int  nl,ns,n=0;		/* Number lines, samples in DEM 	*/
    int  index, hist[3]; 
    char  *obuf;		/* I/O buffer for transfer data 	*/

    StartWatch();

    if (argc != 3)
      {
        printf("\n\nUsage: %s inDEM [1 2]\n\n",argv[0]);
        exit(1);
      }

    strcat(strcpy(inDEM,argv[1]),".img");
    strcat(strcpy(inDDR,argv[1]),".ddr");
    switch (argv[2][0]) {
    	case '1': n=1; break;
    	case '2': n=2; break;
    	default : printf("ERROR: incorrect option, %s\n",argv[2]);
    }
 
    
    /*  Read input DEM DDR file */
    if (c_getddr(inDEM,&ddr) != 0)
      {
        printf("Error returned from c_getddr:  unable to read file %s\n",inDEM);
        exit(1);
      }
	
    nl = ddr.nl;
    ns = ddr.ns;

    printf("Image size, nl x ns:  %i x %i\n",nl,ns);
	printf("Total number of pixels:  %d\n",nl*ns);
    
    obuf = (char *) MALLOC (nl*ns*n);
    fpi = fopenImage(inDEM,"rb");
    
    for (index=0; index < nl*ns*n; index++) obuf[index] = 0;
    
	if (fread(&obuf[0],nl*ns*n,1,fpi) != 1) 
    	{ printf("Unable to read the input DEM file\n"); exit(1); }
   
    for (index=0; index<3; index++) hist[index]=0;
	
	if (n==1) {
		for (index=0; index < nl*ns; index++) {
    		if (obuf[index]==0)   hist[0]++;
    		if (obuf[index]==100) hist[1]++;
    		if (obuf[index]==200) hist[2]++;
    	}
    	printf("  0:\t%d\n",hist[0]);
    	printf("100:\t%d\n",hist[1]);
    	printf("200:\t%d\n",hist[2]);
    }
    if (n==2) {
    	for (index=0; index < nl*ns*n; index+=2) {
    		if (obuf[index]==0 && obuf[index+1]==0) hist[0]++; else hist[1]++;
    	}
    	printf("    zero:\t%d\n",hist[0]);
    	printf("non-zero:\t%d\n",hist[1]);
    }
    
    
    fclose(fpi);
    free(obuf);

    StopWatch();
    return(0);
}
