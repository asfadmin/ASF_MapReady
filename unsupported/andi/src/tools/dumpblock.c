/* dumpblock dumps the specified block of a raw data file to stdout in one of
* the following formats
*	
*	each line contains values: Number IValue QValue
*	where Number is the count number of the sample and Value is the value
*
* the block can be selected via Image Scan Number or the current number of
* the block
*/

 
#include <stdio.h>
#include <string.h
#include <malloc.h>
#include "drdapi.h"

#define PROGRAM "dumpblock"
#define MAJ_VER 1
#define MIN_VER 0
#define FACILITY "Alaska SAR Facility"

struct {
	char *pszInFile;
	ulong luBlock;
	} g_cmdLine;

/* prototypes */
void Hello();
void Bye();
void Usage();
void DumpData();

 
/**********************************************************************
* main
*/
int main(argc,argv)
int argc;
char **argv;
{
DRDFILE *pIn;
FILE *pOut;

Hello();

if (! CmdLine(argc,argv) ) {
	Usage();
	return 1;
	}
else if (!drdInit(PROGRAM,MAJ_VER,MIN_VER,FACILITY) ) {
	fprintf(stderr,"Error initializing drd library\n");
	return 1;
	}
/* open the files */
else if ( NULL == (pIn = drdOpen(g_cmdLine.pszInFile)) ) {
	fprintf(stderr,"Error opening file '%s'\n",g_cmdLine.pszInFile);
	return 1;
	}
/* not possible in REPORT and AUX file */
else if (    !strcmp(pIn->szType,AUX_FILETYPE)
	  || !strcmp(pIn->szType,REPORT_FILETYPE) )
	{
	fprintf(stderr,"Cannot handle file with type '%s'\n",pIn->szType);
	return 1;
	}
/* now process data */
else {
	DumpData(pIn);
	}

if (NULL != pIn) 
	drdClose(pIn);
drdExit();

Bye();
}

/**********************************************************************
* Hello says Hello
* Bye says See you
* Usage explains
*/
void Hello()
{
fprintf(stderr,"%s V%d.%d Dumps the specified block to the standard output\n",
	PROGRAM,MAJ_VER,MIN_VER);
}

void Bye()
{
fprintf(stderr,"%s says: Dump your file as long as you can\n",PROGRAM);
}

void Usage()
{
fprintf(stderr,"usage:\n");
fprintf(stderr,"\tdumpblock infile blocknr\n");
fprintf(stderr,"\t          infile input file name\n");
fprintf(stderr,"\t          blocknr nr of the block to dump (first is 1)\n");
fprintf(stderr,"\nOfficial Calibration Version\n");
}

/**********************************************************************
* CmdLine analyzes the command line
* returns 0 if an error occures
*/
int CmdLine(argc,argv)
int argc;
char **argv;
{
if (argc != 3)
	return 0;

/* argv[0] = "dumpblock" */
/* argv[1] = inputfile */
/* argv[2] = blocknr */
g_cmdLine.pszInFile = argv[1];
g_cmdLine.luBlock = (ulong)atol(argv[2]);
if (g_cmdLine.luBlock <= 0)
	return 0;
return 1;
}

/**********************************************************************
* DumpData
*	does the job
*/
void DumpData(pFH)
DRDFILE *pFH;
{
ulong uLength,luScanNo;
SCcomplex *pBuffer;
register ulong count;

/* got first Block */
if ( !drdGotoFirstBlock(pFH) ) {
	fprintf(stderr,"Cannot find first data block\n");
	return;
	}

/* find the convinient block */
for (count = 1L; count < g_cmdLine.luBlock; count++) {
	if (!drdGotoNextBlock(pFH) ) {
		fprintf(stderr,"Block %lu not available! last Block is %lu\n",
				g_cmdLine.luBlock, count-1 ) ;
		return;
		}
	}

/* now read the block */
uLength = drdGetCurrentBlockLength(pFH);
pBuffer = (SCcomplex *)malloc(uLength);
drdGetCurrentBlock(pFH,&luScanNo,pBuffer);
fprintf(stderr,"Dumping Block %lu, Image Scan # %lu\n",
			g_cmdLine.luBlock,luScanNo);

/* and dump it */
for (count = 0; count < uLength/2; count++)
	printf("%8lu\t%f\t %f\n",count,
		(double)pBuffer[count].I + 0.5,(double)pBuffer[count].Q + 0.5);

fprintf(stderr,"%8lu values dumped\n",count);
 
}
