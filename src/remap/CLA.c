/*
	CLA.c: the user interface part of the remap image manipulation
program.  The primary entry point is process_CLAs, about halfway down.
This part of the program parses the all the Command Line Arguments (CLA's)
the user passes to the program, except the first two (the file names), which
main() handles itself.

1997, ASF SAR Tools.  By Orion Lawlor.
*/
#include "asf.h"
#include "ddr.h"
#include "Matrix2D.h"
#include "remap.h"

/*File formats used by the remap program:
	
	Standard LAS 6.0 DDR file format.
	
	<matrixFile> specifies a 2x3 image tranformation matrix.  It has the format:
		out.x=in.x*<num 1>+in.y*<num 2>+<num 3>
		out.y=in.x*<num 4>+in.y*<num 5>+<num 6>
		
		Where <num 1> ... <num 6> a floating-point numbers read sequentially 
		from the matrix file, separated by whitespace.
		
		e.g.:  A matrix which scales the X coordinates by 2, and then
			shifts the image 11.5 pixels to the right (+x) and 13 pixels 
			down (+y) is:
			
			2 0 11.5
			0 1 13
		
	<ppf_file> also specifies a 2x3 image transformation matrix as above, 
		but in a different way, compatible with 2dmap and greycorr:
		AZ1COEF        R 1 <num 5>
		AZ2COEF        R 1 <num 4>
		AZ3COEF        R 1 <num 6>
		GR1COEF        R 1 <num 2>
		GR2COEF        R 1 <num 1>
		GR3COEF        R 1 <num 3>
	
	<quadraticFile> specifies a quadratic 2D mapping, as
		out=A+Bx+Cy+Dxx+Exy+Fyy;
		The coefficients for each equation are stored as ASCII:
			<A> <B> <C> <D> <E> <F> 
		and are listed in the file as 4 lines:
		    convert_in_to_outX
		    convert_in_to_outY
		    convert_out_to_inX
		    convert_out_to_inY
		
	<deltaFile> (compatible with ASP -o delta files, and calc_deltas) specifies a 
		transformation defined by:
		
		in.x=out.x+out.x*<num 1>+<num 2>
		in.y=out.y+out.x*<num 3>+<num 4>
		
		Where <num 1> ... <num 4> are floating-point numbers read sequentially
		from the matrix file, separated by whitespace.
	
	
	<kernelFile> specifies an image convolution kernel.  This kernel is applied
		to pixels of the input image to produce pixels of the output image.
		Its format is:
		
		<size of kernel in X (sx) > <size of kernel in Y (sy) >
		 <kernel top left>    ...    <kernel top right>
		         ..           ...           ...
		<kernel bottom left>  ...   <kernel bottom right>
		
		In total, there are 2+sx*sy numbers in the kernel.
		The kernel is not normalized.  Its values can be negative.
*/
Matrix2D *readMatrixFile(char *matrixfilename);
Matrix2D *readPPFFile(char *ppffilename);
Matrix2D *readDeltaFile(char *deltafilename);


Matrix2D *readMatrixFile(char *matrixfilename)
{
	int i,j;
	FILE *f=FOPEN(matrixfilename,"r");
	Matrix2D *m=identityMatrix2D();
	
	/*******FOPEN TAKES CARE OF THIS NOW********************
	if (f==NULL) {
	  sprintf(errbuf, "   ERROR: Cannot open matrix file '%s'.\n",matrixfilename);
	  printErr(errbuf);
	}
	*******************************************************/
	for (j=0;j<2;j++)
		for (i=0;i<3;i++)
		{
			float inFloat;
			fscanf(f,"%f",&inFloat);
			m->e[i][j]=inFloat;
		}
	fclose(f);
	return m;
}
Matrix2D *readPPFFile(char *ppffilename)
{
	char garbage1[40],garbage2[40],garbage3[40];
	FILE *f=FOPEN(ppffilename,"r");
	Matrix2D *m=identityMatrix2D();

	/*******FOPEN TAKES CARE OF THIS NOW********************
	if (f==NULL) {
	  sprintf(errbuf, "   ERROR: Cannot open ppf file '%s' (remember, you need to include the\
extension).\n",ppffilename);
	  printErr(errbuf);
	}
	*******************************************************/
	fscanf(f,"%s %s %s %lf",garbage1,garbage2,garbage3,&m->e[1][1]);
	fscanf(f,"%s %s %s %lf",garbage1,garbage2,garbage3,&m->e[0][1]);
	fscanf(f,"%s %s %s %lf",garbage1,garbage2,garbage3,&m->e[2][1]);
	fscanf(f,"%s %s %s %lf",garbage1,garbage2,garbage3,&m->e[1][0]);
	fscanf(f,"%s %s %s %lf",garbage1,garbage2,garbage3,&m->e[0][0]);
	fscanf(f,"%s %s %s %lf",garbage1,garbage2,garbage3,&m->e[2][0]);
	m->e[0][0]+=1.0;
	m->e[1][1]+=1.0;
	fclose(f);
	return m;
}
Matrix2D *readDeltaFile(char *deltafilename)
{
	float inFloat;
	FILE *f=FOPEN(deltafilename,"r");
	Matrix2D *m=identityMatrix2D();
	
	/*******FOPEN TAKES CARE OF THIS NOW********************
	if (f==NULL) {
	  sprintf(errbuf, "   ERROR: Cannot open delta file '%s'.\n",deltafilename);
	  printErr(errbuf);
	}
	*******************************************************/
	fscanf(f,"%f",&inFloat);
	m->e[0][0]=1.0+inFloat;
	fscanf(f,"%f",&inFloat);
	m->e[2][0]=inFloat;
	fscanf(f,"%f",&inFloat);
	m->e[0][1]=inFloat;
	fscanf(f,"%f",&inFloat);
	m->e[2][1]=inFloat;
	fclose(f);
	return invertMatrix2D(m);
}

void usage(char *name)
{
 printf(
  "\n"
  "USAGE:\n"
  "  %s\n"
  "    [-rotate <deg>] [-scale <sX> <sY>] [-translate <tX> <tY>]\n"
  "    [-matrix <matrixFile>] [-ppf <ppfFile>] [-deltas <deltaFile>]\n"
  "    [-quadratic <quadFile>] [-warp <warpImages>] [-background <fill>]\n"
  "    [-nearest | -bilinear | -sinc | -kernel <sizeX> <sizeY>\n"
  "                                       | -fileKernel <kernelFile>]\n"
  "    [-byte [-map <min> <max>] | -char [-map <min> <max>]\n"
  "                                       | -short | -int | -float | -double]\n"
  "    [-width <width>] [-height <height>] [-sameSize] [-asDDR <ddr>]\n"
  "    [-log <file>]\n"
  "    <in file> <out file>\n"
  "\n",name);
 printf(
  "REQUIRED ARGUMENTS:\n"
  "  <in file>   an image file (with any extension) and DDR,\n"
  "  <out file>  an image file and DDR of the same type.\n"
  "\n");
 printf(
  "OPTIONAL ARGUMENTS:\n"
  "Transformations:\n"
  "            -rotate <deg>: Rotation is about the 0,0 coordinate\n"
  "                            (the top, left corner) of the input image.\n"
  "         -scale <sX> <sY>: Scale the image in the x and y directions\n"
  "     -translate <tX> <tY>: Translate the image in the x and y directions\n"
  "           The above three commands may be repeated, in any order,\n"
  "                    and are evaluated from left to right.\n"
  "     -matrix <matrixFile>: User defined set of matrix transformations\n"
  "                             See man page for format\n"
  "           -ppf <ppfFile>: for a correlate(1) .ppf coefficients file\n"
  "      -deltas <deltaFile>: for a calc_deltas(1) offsets file\n"
  "    -quadratic <quadFile>: from fit_quadratic\n"
  "       -warp <warpImages>: images (.horiz & .vert) with pixel values reflecting\n"
  "                            which direction to \"warp\" the output pixels to\n"
  "Remapping algorithms:\n"
  "                 -nearest: nearest-neighbor interpolation\n"
  "                -bilinear: bilinear interpolation (default)\n"
  "                    -sinc: specifies an 8x8 dynamically generated Sinc image\n"
  "                             kernel.  (often used with interferonometrically\n"
  "                             generated SAR images)\n"
  "  -kernel <sizeX> <sizeY>: uniform image convolution kernel of user-specified\n"
  "                             size\n"
  " -fileKernel <kernelFile>: file-specified image convolution kernel\n"
  "Output data type:\n"
  "            -char | -byte: 8 bit integers\n"
  "                   -short: 16 bit integers\n"
  "                     -int: 32 bit integers\n"
  "                   -float: 32 bit floating point data\n"
  "                  -double: 64 bit floating point data\n"
  "Clipping:\n"
  "           -width <width>: Number of pixels (horizonatally) in the output image\n"
  "         -height <height>: Number of pixels (vertically) in the output image\n"
  "                -sameSize: Leave the image the same size\n"
  "             -asDDR <ddr>: Specify output size & projection with <ddr>\n"
  "Other:\n"
  "           -log <logFile>: Copy the output to a log file.\n"
  "       -background <fill>: Color to set background pixels to\n"
  "                             (floating point value)\n"
  "         -map <min> <max>: min & max read in, then scaled from 0 to 255 in\n"
  "                             output image.  This requires -char or -byte to\n"
  "                             be specified!!\n"
 /*
  "Example: if you want to scale a floating point amplitude image\n"
  "(called mimage1.amp with DDR image1.ddr) by a factor of 0.9 in the Y\n"
  "direction, and blur the image slightly (2x3 kernel), you would\n"
  "remap image1.amp smallerImage1.amp -scale 1.0 0.9 -kernel 2 3\n"
 */
  "\n");
 printf(
  "DESPRIPTION:\n"
  "   remap: remaps (moves around, scales, and rotates) an image file\n"
  "   Works with byte, 2-byte, long, float (4-byte), double (8-byte),\n"
  "   and complex data.\n"
  "\n");
 printf("Version %4.2f, ASF SAR TOOLS\n\n",VERSION);
 exit(1);
}


/**********************************Main Entry Point: Process_CLAs***********************/
/*process_CLAs parses the command line arguments to find the mapping and sampling functions.
  It also returns a pixel type (iff the user specifies one).*/
int process_CLAs(int argc, char *argv[],mappingFunction *map,
	sampleFunction *samp,int *outWidth,int *outHeight,struct DDR **asDDR)
{
	int currArg=1;
#define CONCATENATE_MATRIX(thisMatrix) addMatrix(*map,thisMatrix);
	int pixelType=0;
	*map=createMap(matrixMap,NULL);/*Assume the user wants a matrix Map*/
	*samp=createSamp(bilinearSamp);/*Assume the user wants a bilinear sampling*/
	while (currArg<argc)
	{
		char *key=argv[currArg++];

	/*In no particular order, we test for mapping functions...*/
		if (strmatch(key,"-rotate")||strmatch(key,"-rotation"))
		{
			CHECK_ARG(1) /*one floating-point argument: rotation (degrees)*/
			CONCATENATE_MATRIX(rotationMatrix2D(-atof(GET_ARG(1))*M_PI/180.0));
		} else if (strmatch(key,"-scale"))
		{
			CHECK_ARG(2) /*two floating-point arguments: sx and sy*/
			CONCATENATE_MATRIX(scaleMatrix2D(atof(GET_ARG(2)),atof(GET_ARG(1))));
		} else if (strmatch(key,"-translate")||strmatch(key,"-translation"))
		{
			CHECK_ARG(2) /*two floating-point arguments: tx and ty*/
			CONCATENATE_MATRIX(translationMatrix2D(atof(GET_ARG(2)),atof(GET_ARG(1))));
		} else if (strmatch(key,"-matrix"))
		{
			CHECK_ARG(1) /*one filename argument*/
			CONCATENATE_MATRIX(readMatrixFile(GET_ARG(1)));
		} else if (strmatch(key,"-deltas"))
		{
			CHECK_ARG(1) /*one filename argument*/
			CONCATENATE_MATRIX(readDeltaFile(GET_ARG(1)));
		}else if (strmatch(key,"-ppf"))
		{
			CHECK_ARG(1) /*one filename argument*/
			CONCATENATE_MATRIX(invertMatrix2D(readPPFFile(GET_ARG(1))));
		}else if (strmatch(key,"-quadratic"))
		{
			CHECK_ARG(1) /*one filename argument*/
			*map=createMap(quadraticMap,(void *)GET_ARG(1));
		}else if (strmatch(key,"-warp"))
		{
			CHECK_ARG(1) /*one filename argument*/
			*map=createMap(warpMap,(void *)GET_ARG(1));
		}
	/*...then sampling functions...*/
		else if (strmatch(key,"-nearest"))
			*samp=createSamp(nearestSamp);
		else if (strmatch(key,"-bilinear"))
			*samp=createSamp(bilinearSamp);
		else if (strmatch(key,"-kernel"))
		{
			CHECK_ARG(2) /*two integer arguments: kernel size x and y*/
			*samp=createKernelSamp(atoi(GET_ARG(2)),atoi(GET_ARG(1)));
		} else if (strmatch(key,"-fileKernel"))
		{
			CHECK_ARG(1) /*one kernelFile argument*/
			*samp=readKernelFile(GET_ARG(1));
		}
		else if (strmatch(key,"-sinc"))
			*samp=createSamp(sincSamp);
	/*...and pixel types...*/
		else if (strmatch(key,"-map"))
		{
			CHECK_ARG(2) /*two floating-point arguments: min and max*/
			minFlt=atof(GET_ARG(2));
			maxFlt=atof(GET_ARG(1));
			printf("   Input min=%f, max=%f\n",minFlt,maxFlt);
		}
		else if (strmatch(key,"-background"))
		{
			CHECK_ARG(1) /*one floating-point argument: background fill*/
			backgroundFill=atof(GET_ARG(1));
			printf("   Background Fill Value=%f\n",backgroundFill);
		}
		else if (strmatch(key,"-byte")||strmatch(key,"-char")) pixelType=1;
		else if (strmatch(key,"-short")) pixelType=2;
		else if (strmatch(key,"-long")) pixelType=3;
		else if (strmatch(key,"-float")) pixelType=4;
		else if (strmatch(key,"-double")) pixelType=5;
	/*...and finally the clipping options.*/
		else if (strmatch(key,"-width")) 
		{
			CHECK_ARG(1)/*One size argument*/
			*outWidth=atoi(GET_ARG(1));
		} else if (strmatch(key,"-height")) 
		{
			CHECK_ARG(1)/*One size argument*/
			*outHeight=atoi(GET_ARG(1));
		} else if (strmatch(key,"-asDDR")) 
		{
			struct DDR *ddr=(struct DDR *)MALLOC(sizeof(struct DDR));
			CHECK_ARG(1)/*One DDR filename argument*/
			c_getddr(GET_ARG(1),ddr);
			*asDDR=ddr;
		} else if (strmatch(key,"-sameSize")) 
			*outHeight=*outWidth=-1;
                else if (strmatch(key,"-log"))
                {
                        CHECK_ARG(1)/*One size argument*/
                        logflag=1;
			sprintf(logFile, "%s", GET_ARG(1));
			fLog = FOPEN(logFile, "a");
                }
		else {printf("   *****Unrecognized keyword: %s\n\n",argv[currArg-1]);usage(argv[0]);}

		if (2 == (argc-currArg)) return pixelType;
	}
	return -1; /* Just here to keep compiler from complaining */
}
