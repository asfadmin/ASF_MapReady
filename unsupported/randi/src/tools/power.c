/* Power.c calculates the power of a specific pulse or 
* calculates the mean power and the standard deviation of a bunch of pulses
* The pulses can be calibration pulses, pulse replicas, echo or noise scans
*
* Version 1.0 08-28-1994 by Hans-Joerg Wagner
*/
#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include "drdapi.h"

#define PROGRAM "POWER"
#define VER_MIN 0
#define VER_MAJ 1
#define FACILITY "Alaska SAR Facility"
#define POWERARRAY	20

/* command line structure */
struct {
	char *pszInFile;
	ulong luPulse;
	} g_cmdLine;

/* Prototypes */
void Hello();
void Bye();
void Usage();
int CmdLine();
DRDFILE *OpenFile();
void CalculateAndPrint();
double power();

/**********************************************************************
* main program 
*/
main(argc,argv)
int argc;
char **argv;
{
DRDFILE *pFile;


Hello();

if (!CmdLine(argc,argv))
	Usage();
else {
if (!drdInit(PROGRAM,VER_MAJ,VER_MIN,FACILITY) )
	fprintf(stderr,"Error initializing DRD library\n");
else {
  if ( NULL == ( pFile = OpenFile() ) )
	fprintf(stderr,"Error opening file\n");
  
  else {
	CalculateAndPrint();
	drdClose(pFile);
	}
}
}
drdExit();
Bye();
}

/**********************************************************************
* Hello says hello
* Bye says good bye
* Usage explains how to use
*/
void Hello()
{
fprintf(stderr,"POWER     calculates the power of samples in DRD files\n");
}

void Bye()
{
fprintf(stderr,"POWER ... to the calibration department!!!! (Hi Jason)\n");
}

void Usage()
{
fprintf(stderr,"\tUsage:\n");
fprintf(stderr,"\t   power infile [pulse]\n\n");
fprintf(stderr,"\t         infile    full name of the input file\n");
fprintf(stderr,"\t         pulse     number of pulse to analyze. The first\n");
fprintf(stderr,"\t                   pulse has the number 1. Standard is 0\n");
fprintf(stderr,"\t                   and forces the program to calculate the\n");
fprintf(stderr,"\t                   mean and standard deviation over all pulses\n");
}

/**********************************************************************
* CmdLine
* decodes the command line
* returns 0 if an error occures
*/
int CmdLine(argc,argv)
int argc;
char **argv;
{

/* argv[1] = infile */
/* argv[2] = pulse number */

if (argc < 2) {
	fprintf(stderr,"too few arguments\n");
	return 0;
	}
else if (argc > 3) {
	fprintf(stderr,"too much arguments\n");
	return 0;
	}
g_cmdLine.pszInFile = argv[1];
if (3 == argc) 
	g_cmdLine.luPulse = atol(argv[2]);
else
	g_cmdLine.luPulse = 0L;

return 1;



}

/**********************************************************************
* OpenFile
* openes the input file and returns a pointer to the file
* returns NULL if not successful
*/
DRDFILE *OpenFile()
{
DRDFILE *pFile;

pFile = drdOpen(g_cmdLine.pszInFile);

if (NULL != pFile) {
 
    /* test from file type if file is processable by program */
    if ( strcmp(pFile->szType,ECHO_FILETYPE)
        && strcmp(pFile->szType,NOISE_FILETYPE)
        && strcmp(pFile->szType,REP_FILETYPE)
        && strcmp(pFile->szType,CAL_FILETYPE)  )
	{
	fprintf(stderr,"Cannot process file type '%s'\n",pFile->szType);
	drdClose(pFile);
	pFile = NULL;
	}
    } /* end if */
return pFile;
}

/**********************************************************************
* CalculateAndPrint
* does the work. It calculates the values and printd the results dependent
* on the command line
*/
void CalculateAndPrint(pFile)
DRDFILE *pFile;
{
ulong count,luPulses,luLength,luImageScan;
SCcomplex *pBuffer;
double fPower,fMinPower,fMaxPower,fStdDev;
double *afPower,*afTmp;

/* goto first pulse */
if ( !drdGotoFirstBlock(pFile) ) {
	fprintf(stderr,"Cannot go to first block in input file\n");
	return;
	}


/* if pulse number is given */
if (g_cmdLine.luPulse) {
    /* search pulse */
    for (count = 1L; count < g_cmdLine.luPulse; count++) {
	if ( !drdGotoNextBlock(pFile) ) {
	    fprintf(stderr,"Cannot read pulse #%lu, Only %lu pulses in file\n",
				g_cmdLine.luPulse,count);
	    drdClose(pFile);
	    return;
	    }
	}

    /* read the pulse and analyze it */
    luLength = drdGetCurrentBlockLength(pFile);
    pBuffer = (SCcomplex *)malloc(luLength);
    drdGetCurrentBlock(pFile,&luImageScan,pBuffer);

    /* analyze pulse */
    fPower = power(pBuffer,luLength/sizeof(SCcomplex));
    /* print results */
    printf("Pulse #%lu of file '%s'\n",g_cmdLine.luPulse,g_cmdLine.pszInFile);
    printf("      P = %f\n",fPower);

    /* free buffer */
    free(pBuffer);
    }
else {
    /* allocate an initial ammount of memory for power field */
    afPower = (double *)malloc(POWERARRAY * sizeof(double));
    if (NULL == afPower) {
	fprintf(stderr,"Cannot allocate buffer for analysation\n");
	return;
	}

    /* iteration over every pulse */
    for (luPulses = 1L; /* end condition is a break */; luPulses++) {
	fprintf(stderr,"Processing pulse %lu\r",luPulses);
	/* increase buffer of powerarray if necessary */
	
	if ( ! (luPulses % POWERARRAY) ) {
	    count = luPulses/POWERARRAY;
	    
	    
	    afTmp = (double *)realloc(afPower,
			(count+1)*POWERARRAY*sizeof(double));
	    if (NULL == afTmp) {
		free(afPower);
		fprintf(stderr,"Cannot expand memory for power array\n");
		return;
		}
		

		
		
	    afPower = afTmp;
	} /* end if more powermemory */

	/* allocate memory for buffer */	
	luLength = drdGetCurrentBlockLength(pFile);
	
	pBuffer = (SCcomplex *)malloc(luLength);
	if (NULL == pBuffer) {
	    fprintf(stderr,"Cannot allocate memory for read buffer\n");
	    return;
	    }
	drdGetCurrentBlock(pFile,&luImageScan,pBuffer);
	
	/* analyze pulse */
	afPower[luPulses-1] = power(pBuffer,luLength/sizeof(SCcomplex));

	/* goto next block or stop */
	if ( ! drdGotoNextBlock(pFile) )
	    break;
	} /* end for every pulse */
    fprintf(stderr,"Processed %lu pulses\n",luPulses);

    /* calculate mean */
    fPower = 0.0;
    for (count = 0L; count < luPulses; count++)
	fPower += afPower[count];
    fPower /= (double)luPulses;

    /* calculate standard deviation */
    fStdDev = 0.0;
    for (count = 0L; count < luPulses; count++)
	fStdDev += (afPower[count] - fPower) * (afPower[count] - fPower);
    fStdDev = sqrt( fStdDev / (double)luPulses );

    /* find minimum and maximum */
    fMinPower = afPower[0]; fMaxPower = afPower[0];
    for (count = 1L; count < luPulses; count++) {
	if (afPower[count] < fMinPower) fMinPower = afPower[count];
	if (afPower[count] > fMaxPower) fMaxPower = afPower[count];
	}

    /* print result */
    printf(" Mean Power %f     StdDev %f    Min %f   Max %f\n",
		fPower,fStdDev,fMinPower,fMaxPower);
    }
}

/**********************************************************************
* power calculates the power over the impulse:
*  power = 1/N * sum( (abs(x))**2 )
*/
double power(pArray,luLength)
SCcomplex *pArray;
ulong luLength;
{
register ulong count;
register double p = 0.0, I,Q;

for (count = 0; count < luLength; count++) {
	I = (double)pArray[count].I + 0.5;
	Q = (double)pArray[count].Q + 0.5;
	p+= I*I + Q*Q;
	}
p /= ((double)count);
return p;
}
