/*
Name: intersect_las

Purpose: compute the intersection of two las images.

*/
#include "caplib.h"
#include <math.h>
#include "ddr.h"
void getFloatLine(FILE *f,const struct DDR *ddr,int yLine,float *dest); /*Read line from file.*/
void putFloatLine(FILE *f,const struct DDR *ddr,int yLine,const float *source); /*Write line to file.*/

main(int argc, char **argv)
{
	const float span=1.0;
	int x,y,size,npixels;
	float *line1,*line1before,*line1after;
	float *outline;
	char *fin,*fout;
	FILE *fpin,*fpout;
	struct DDR ddrin,ddrout;
	struct BDDR outbdr;
	if (argc!=4)
		{printf("Usage: cont_las input.ext  size output.ext\n\
\tComputes a difference at each point of a las image.\n\
\tThis enhances speckle, for detection of edges and\n\
\telimination of high-contrast regions.\n\
ASF STEP Tools verification, 1997.\n");exit(1);}
	fin=argv[1];
	size=atoi(argv[2]);
	fout=argv[3];
	if (0!=c_getddr(fin,&ddrin)) {printf("Couldn't open ddr file for %s.\n",fin);exit(1);}
	
	ddrout=ddrin;
	c_putddr(fout,&ddrout);
	c_intbdr(&outbdr);
	outbdr.bandno=1;
	c_putbdr(fout,&outbdr);
	fpout=FOPEN(fout,"wb");
	fpin=FOPEN(fin,"rb");
	line1=(float *)MALLOC(sizeof(float)*ddrin.ns);
	line1before=(float *)MALLOC(sizeof(float)*ddrin.ns);
	line1after=(float *)MALLOC(sizeof(float)*ddrin.ns);
	outline=(float *)MALLOC(sizeof(float)*ddrout.ns);
	for (x=0;x<ddrout.ns;x++)
		outline[x]=0;
	for (y=0;y<size;y++)
		putFloatLine(fpout,&ddrout,y,outline);
	for (y=size;y<ddrin.nl-size;y++)
	{
		getFloatLine(fpin,&ddrin,y,line1);
		getFloatLine(fpin,&ddrin,y-size,line1before);
		getFloatLine(fpin,&ddrin,y+size,line1after);
		for (x=size;x<ddrin.ns-size;x++)
		{
			register float cur=line1[x];
			outline[x]=(fabs(line1before[x-size]-cur)+fabs(line1before[x]-cur)+fabs(line1before[x+size]-cur)+
				   fabs(line1      [x-size]-cur)+                         fabs(line1      [x+size]-cur)+
				   fabs(line1after [x-size]-cur)+fabs(line1after [x]-cur)+fabs(line1after [x+size]-cur))*(4.0/8.0);
			if (ddrout.dtype==1) /*If data is of byte type*/
				if (outline[x]>255.0)
					outline[x]=255.0;
		}
		putFloatLine(fpout,&ddrout,y,outline);
		if (y%2==0)
			printf("\tComparing line %i...\r",y);
	}
	for (x=0;x<ddrout.ns;x++)
		outline[x]=0;
	for (y;y<ddrout.nl;y++)
		putFloatLine(fpout,&ddrout,y,outline);
	printf("\nCont_las complete!\n");
	return(0);
}
