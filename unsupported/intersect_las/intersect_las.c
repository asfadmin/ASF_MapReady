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
	float *line1,*line1before,*line1after,*line2,*line2before,*line2after;
	float *outline1,*outline2;
	char *f1,*f2,*f1o,*f2o;
	FILE *fp1,*fp2,*fp1o,*fp2o;
	struct DDR ddr1,ddr2,outddr;
	struct BDDR outbdr;
	if (argc!=6)
		{printf("Usage: intersect_las file1.ext file2.ext searchsize out1.ext out2.ext\n\
\tIntersect (find the common region of) two LAS 6.0 images.  The\n\
\tprogram will create an image with the same dimentions\n\
\tas the originals, whose pixels are only non-zero where\n\
\tboth original images have non-zero values for searchsize\n\
\tpixels around.\n\
ASF STEP Tools verification, 1997.\n");exit(1);}
	f1=argv[1];
	f2=argv[2];
	size=atoi(argv[3]);
	f1o=argv[4];
	f2o=argv[5];
	if (0!=c_getddr(f1,&ddr1)) {printf("Couldn't open ddr file for %s.\n",f1);exit(1);}
	if (0!=c_getddr(f2,&ddr2)) {printf("Couldn't open ddr file for %s.\n",f2);exit(1);}
	if (ddr1.nl!=ddr2.nl)
		{printf("The number of lines is different.\n");exit(1);}
	if (ddr1.ns!=ddr2.ns)
		{printf("The number of samples is different.\n");exit(1);}
	if (ddr1.dtype!=ddr2.dtype)
		{printf("The data types are different.\n");exit(1);}
	if (ddr1.nbands!=ddr2.nbands)
		{printf("The number of bands is different.\n");exit(1);}
	
	outddr=ddr1;
	c_putddr(f1o,&outddr);
	c_putddr(f2o,&outddr);
	c_intbdr(&outbdr);
	outbdr.bandno=1;
	c_putbdr(f1o,&outbdr);
	c_putbdr(f2o,&outbdr);
	fp1o=FOPEN(f1o,"wb");
	fp2o=FOPEN(f2o,"wb");
	fp1=FOPEN(f1,"rb");
	fp2=FOPEN(f2,"rb");
	line1=(float *)MALLOC(sizeof(float)*ddr1.ns);
	line2=(float *)MALLOC(sizeof(float)*ddr2.ns);
	line1before=(float *)MALLOC(sizeof(float)*ddr1.ns);
	line2before=(float *)MALLOC(sizeof(float)*ddr2.ns);
	line1after=(float *)MALLOC(sizeof(float)*ddr1.ns);
	line2after=(float *)MALLOC(sizeof(float)*ddr2.ns);
	outline1=(float *)MALLOC(sizeof(float)*ddr1.ns);
	outline2=(float *)MALLOC(sizeof(float)*ddr2.ns);
	npixels=0;
	for (x=0;x<ddr1.ns;x++)
		outline1[x]=outline2[x]=0;
	for (y=0;y<size;y++)
	{
		putFloatLine(fp1o,&outddr,y,outline1);
		putFloatLine(fp2o,&outddr,y,outline2);
	}
	for (y=size;y<ddr1.nl-size;y++)
	{
		getFloatLine(fp1,&ddr1,y,line1);
		getFloatLine(fp1,&ddr1,y-size,line1before);
		getFloatLine(fp1,&ddr1,y+size,line1after);
		getFloatLine(fp2,&ddr2,y,line2);
		getFloatLine(fp2,&ddr2,y-size,line2before);
		getFloatLine(fp2,&ddr2,y+size,line2after);
		for (x=size;x<ddr1.ns-size;x++)
			if ((line1before[x-size]!=0.0)&&(line1before[x+size]!=0.0)&&
			    (line1after [x-size]!=0.0)&&(line1after [x+size]!=0.0)&&
			    (line2before[x-size]!=0.0)&&(line2before[x+size]!=0.0)&&
			    (line2after [x-size]!=0.0)&&(line2after [x+size]!=0.0))
			{
				npixels++;
				outline1[x]=line1[x];
				outline2[x]=line2[x];
			} else outline1[x]=outline2[x]=0.0;
		putFloatLine(fp1o,&outddr,y,outline1);
		putFloatLine(fp2o,&outddr,y,outline2);
		if (y%2==0)
			printf("\tComparing line %i...\r",y);
	}
	for (x=0;x<ddr1.ns;x++)
		outline1[x]=outline2[x]=0;
	for (y;y<ddr1.nl;y++)
	{
		putFloatLine(fp1o,&outddr,y,outline1);
		putFloatLine(fp2o,&outddr,y,outline2);
	}
	fclose(fp1);
	fclose(fp2);
	fclose(fp1o);
	fclose(fp2o);
	printf("\n%i total shared pixels.\n",npixels);
	return(0);
}
