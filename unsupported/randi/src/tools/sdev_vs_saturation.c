/* sdev_vs_saturation.c
* sdev_vs_saturation program
*
* Adapted from sdevf.c October 6, 1994 by Jason Williams
* This program calculates the same sdev stuff as sdevf.c, and then also
* calculates the % saturation, measured as % of cells within highest
* and lowest bins.  It then creates a map of % saturation and generates
* a plot of % saturation vs standard deviation.
*
* sdevf notes from hans:
* calculates the standard deviation of cells within an echo file
* calculates just for I or Q channel or for abs
* output as Ascii-file every line is one row of calculation
* if image scans are missed, the standard deviation will be calculated
*           if more than half the image scans are found. Otherwise
*           the standard deviation is set to 0
*
* First setup 07-27-1994 by Hans-Joerg Wagner
*
* Modified 9/2/94 by Jason Williams to pass window labels to dx
*	also fixed so A option writes to a file
*/

#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include "drdapi.h"
#include "hjwlib.h"
 
#define PROGRAM         "SDEV_VS_SATURATION"
#define MAJORVER        1
#define MINORVER        0
#define FACILITY        "Alaska SAR Facility"
#define OUTFILE_EXT	".saturation.plot"
#define IMGFILE_EXT	".saturation.image"
 
/* prototypes */
void Hello();
void Bye();
void Usage();
int CommandLine();
void CalculateAndWrite();
int ReadCluster();
int CalculateCluster();
void PrintCalcStatus();
void TransformToImageFile();
void CallDx();

/* global structure for command line arguments */
struct {
        char *pszInFile;
	int scans,sampls;
        } g_cmdLine;

/**********************************************************************
* main
*/
main(argc,argv)
int argc;
char **argv;
{
Hello();
if (!CommandLine(argc,argv)) {
        Usage();
        }
else if ( ! drdInit(PROGRAM,MAJORVER,MINORVER,FACILITY) ) {
        printf("error initializing DRD library\n");
        }
else {
	CalculateAndWrite();
	drdExit();
	}
Bye();
}

/**********************************************************************
* Hello says hello
* Bye says good bye
*/
void Hello()
{
printf("%s V%d.%d calculates a field of saturations for an echo file\n",
                PROGRAM,MAJORVER,MINORVER);
printf("Assumes that the scan width does not differ within the image. This\n");
printf("is OK for ERS-1, but may not be good for JERS-1,RADARSAT and others\n");
}
 
void Bye()
{
printf("%s says good bye - enjoy the result          \n",PROGRAM);
}

/**********************************************************************
* Usage explains the usage of the tool
*/
void Usage()
{
printf("usage:\n");
printf("\tsdev_vs_saturation infile scans samples\n\n");
printf("\t     infile     input file root name.\n");
printf("\t     scans	  how many scans to accumulate\n");
printf("\t     samples    how many samples to accumulate\n");
printf("\t     the files produced have the same root name as infile and\n");
printf("\t     followed by a extention (i.saturation.plot for result and i.saturation.image\n");

}

/**********************************************************************
* CommandLine 
* interprets the command line
* returns 0 if an error occures
*/ 
int CommandLine(argc,argv)
int argc;
char **argv;
{
int count;
 
if (argc < 4 ) 
        return 0;

/* argv[0] = "sdev_vs_saturation"  */
/* argv[1] = infile   */
/* argv[2] = scans    */
/* argv[3] = sampls   */
g_cmdLine.pszInFile = argv[1];
g_cmdLine.scans = atoi(argv[2]);
g_cmdLine.sampls = atoi(argv[3]);

return (g_cmdLine.sampls > 0 && g_cmdLine.scans > 0);
}

/**********************************************************************
* Calculate and write
* assumed is, that all scans have the same width. This is OK for
* ERS1, perhaps for JERS and not for RADARSAT
*/
void CalculateAndWrite()
{
register FILE *fileOut = NULL,*fileImageOut = NULL;
char *pszImage;
DRDFILE *pAux, *echo;
ushort uLength,uBlockPerLine;
char *apSample,*pTmp,*pszTmp;
register int	*abValid;
double  *alfSat, *alfMean, *alfStdDev;
register int count;
ulong luRows;
int numBits;
short int highBin, lowBin;
double lfMax=-1.0;
double lfMin=-1.0;

/* open input files */
echo = echoOpen(g_cmdLine.pszInFile);
if ( NULL == echo ) {
	printf("error opening input file %s.echo\n",g_cmdLine.pszInFile);
	return;
	}
echoGotoFirstScan(echo);

/* open the output files */
    fileOut = fopendef(NULL,g_cmdLine.pszInFile,OUTFILE_EXT,"w+");
    if (NULL == fileOut) {
	fprintf(stderr,"cannot open output file for saturation\n");
	Bye();
	} /* no calculation of I part because cannot open file */
    else {
	fileImageOut=fopendef(NULL,g_cmdLine.pszInFile,IMGFILE_EXT,"w+");
        pszImage = g_hjw_filename;
	if (NULL == fileImageOut) {
	    fprintf(stderr,"cannot open output image file\n");
	    Bye();
	    }
	}

/* Allocate buffer for apSample and abValid*/
uLength = echoGetCurrentScanLength(echo);
if (uLength <= 0) return;
uBlockPerLine = uLength /(g_cmdLine.sampls * sizeof(SCcomplex)); 
apSample = (char *)calloc(g_cmdLine.scans,uLength * sizeof(char));
abValid = (int *)calloc(g_cmdLine.scans,sizeof(int));
alfMean = (double *)calloc(uLength/g_cmdLine.sampls,sizeof(double));
alfStdDev = (double *)calloc(uLength/g_cmdLine.sampls,sizeof(double));
alfSat = (double *)calloc(uLength/g_cmdLine.sampls,sizeof(double));

/* find out how many bits quantization and calc bin no's */
/* HEY! This has been hardcoded for now, implement this the right way!!! */
if (NULL == (pAux =drdOpenOtherFileType(g_cmdLine.pszInFile,AUX_FILEEXT)))
        printf("Cannot open matching auxiliary file\n");
numBits = 5;
highBin = (short int) (pow(2,numBits-1) - 1);
lowBin  = (short int) (-pow(2,numBits-1));
printf("highBin = %d, lowBin = %d\n", highBin, lowBin);

/* everything OK? */
if (NULL == apSample || NULL == abValid || NULL==alfMean || NULL==alfStdDev)
	printf("Error allocating memory\n");

/* only calculation if there is something to calculate */
else if (1)   /* hard wired by Jason since we always want to do this now */
    {
    /* calculating and writing to text file */
    for (luRows = 0L; ReadCluster(echo,apSample,abValid,uLength); luRows++) {
	if (CalculateCluster(highBin, lowBin, alfMean,alfStdDev,alfSat,apSample,abValid,uLength) ) {
	    for(count = 0; count < uBlockPerLine; count++) {
		/* search max and min for later normalizatinon */
		if (alfSat[count] > lfMax || lfMax < 0.0) 
			lfMax = alfSat[count];
	
		if ( (alfSat[count] < lfMin || lfMin < 0.0) 
			&& alfSat[count] > 0.0)
			{ lfMin = alfSat[count]; }

		/* output to file */
		if (1) /* hardcoded */
		    fprintf(fileOut," %lf",alfSat[count]);
		}
	    }
	else {
	    for(count = 0; count < uBlockPerLine; count++) {
		if (1) fprintf(fileOut," ERROR");
		}
	    }
	if (1)
	    fprintf(fileOut,"\n");
	} /* end while */

    /* writing image file */
    /* this is performed by rereading the output files, normalizing the
    * values and writing them as 8 bit unsigned integer values
    */
    if (1) rewind(fileOut);

    if (1) {
	printf("SDEVF saturation I min = %lf,max = %lf\n",
							lfMin,lfMax);
	TransformToImageFile(fileOut,fileImageOut,lfMin,lfMax);
        }

    /* announcing the size of the file */
    printf("SDEVF the resulting images have %lu rows and %u columns\n",
		luRows,uBlockPerLine);
    } /* end else */

/* freeing memory and closing files */
if (NULL != apSample) free(apSample);
if (NULL != abValid) free(abValid);
if (NULL != alfMean) free(alfMean);
if (NULL != alfStdDev) free(alfStdDev);
if (NULL != fileIout) close(fileIout);
if (NULL != fileIimageOut) close(fileIimageOut);
if (NULL != fileQout) close(fileQout);
if (NULL != fileQimageOut) close(fileQimageOut);
if (NULL != fileAout) close(fileAout);
if (NULL != fileAimageOut) close(fileAimageOut);

echoClose(echo);

/* calling dx to display */
if (g_cmdLine.bCalculateI)
	CallDx(pszIimage,uBlockPerLine,luRows);
if (g_cmdLine.bCalculateQ)
        CallDx(pszQimage,uBlockPerLine,luRows);
if (g_cmdLine.bCalculateAbs)
        CallDx(pszAimage,uBlockPerLine,luRows);

return;
}

/**********************************************************************
* CallDx 
* calls dx with file, uBlockPerLine and luRows as parameters
* Jason mod -- Modified 9/2/94 to include window name for dx 
*/
void CallDx(file,uBlockPerLine,luRows)
char *file;
ushort uBlockPerLine;
ulong luRows;
{
char *pTmp;
static char pszDx[] = "dx %s -d %u %lu -l %s &";

pTmp = malloc(strlen(pszDx) + 2*strlen(file) + 17);
if (NULL == pTmp)
    fprintf("Error allocating memory to start dx\n");
else {
    sprintf(pTmp,pszDx,file,uBlockPerLine,luRows,file);
    printf("%s\n",pTmp);
    system(pTmp);
    }
free(pTmp);
} 

/**********************************************************************
* ReadCluster 
* reads one Cluster of samples. One Cluster is an array of 
* g_cmdLine.scans scans of the echo file
*
* returns 0 if end of file
*/
int ReadCluster(echo,apSample,apValid,uLength)
DRDFILE *echo;
char *apSample;
int *apValid;
ushort uLength;
{
int count;
static int bFirstSample = 1;
static ulong luExpectedScanNumber = 0;
ulong luScanNumber;

PrintCalcStatus();
for (count = 0; count < g_cmdLine.scans; count++) {
    if (echoGetCurrentScanLength(echo) != uLength) {
        printf("Error - length of echo scan is not constant\n");
        return;
        }
    echoGetCurrentScan(echo,&luScanNumber,((char *)apSample)+ uLength * count);
    if (luScanNumber == luExpectedScanNumber || bFirstSample) {
	bFirstSample = 0;
	apValid[count] = 1;
	luExpectedScanNumber = luScanNumber + 1;
	if ( !echoGotoNextScan(echo) )
		break;
	}
    else {
	apValid[count] = 0;
	printf("missing image sample %lu\n",luExpectedScanNumber);
	luExpectedScanNumber ++;
	}
    } /* end for */
/* if end of file return false */
return (count >= g_cmdLine.scans);
}
	
/**********************************************************************
* CalculateCluster
* calculates the means and standard deviations of one cluster.
* One Cluster is an array of g_cmdLine.scans scans of the echo file
*		(and saturations! :-> )
* The outer most loop iterates over every block within the cluster, i.e.
* over every standard deviation pixel. The second loop iterates over the
* scans within the cluster and the inner most loop iterates over the 
* values within this scan and block.
*
* returns 0 if less than half of the scans were valid
*/
int CalculateCluster(hiBin, loBin, apMean,apStdDev,apSat,apSample,apValid,uLength)
short int hiBin, loBin;
double *apSat,*apMean,*apStdDev;
char *apSample;
int *apValid;
ushort uLength;
{
int nValids = 0;
register int count;
register double x,y;
register SCcomplex *pTmp;
register int count1,count2,count3;
register int countSat = 0;
int cl = uLength/sizeof(SCcomplex); /* length of apSample in SCcomplex units */

/* calculate the means */
for(count2 = 0;count2 < cl/(g_cmdLine.sampls);count2++) { /*every part of line*/
    apMean[count2] = 0.0;
    for(count1 = 0; count1 < g_cmdLine.scans; count1++) { /*every line*/
	if (apValid[count1]) {
	    pTmp = ((SCcomplex *)apSample) +count1*cl +count2*g_cmdLine.sampls;
	    for(count3 = 0;count3 < g_cmdLine.sampls;count3++) { /*every value*/
		apMean[count2] += 0.5 + pTmp[count3].I;
                if (pTmp[count3] == hiBin || pTmp[count3] == loBin) {
		    countSat++;
		}
	    } /* end for every value */
	/* count the valid lines */
	if (!count2) nValids ++;
	} /* end for every line */
    apMean[count2] /= (double)nValids * (double)g_cmdLine.sampls;
    apSat[count2] = 100.0 * (double)countSat / (double)(nValids*g_cmdLine.sampls);
    } /* end for every part of line  */

/* is enough valids */
if (nValids <= g_cmdLine.scans / 2)
	return 0;

/* calculate stddevs */
for(count2 = 0;count2 < cl/g_cmdLine.sampls;count2++) {
    apStdDev[count2] = 0.0;
    for(count1=0; count1 < g_cmdLine.scans; count1++) {
	if (apValid[count1]) {
	    pTmp = ((SCcomplex *)apSample) +count1*cl +count2*g_cmdLine.sampls;
	    for(count3 = 0; count3 < g_cmdLine.sampls; count3++) {
	 	if (1) { /* hardcoded switch */
	 	    x = (0.5 + pTmp[count3].I)  - apMean[count2];
		    apStdDev[count2] += x * x;
		    }
		}
	    }
	}
    apStdDev[count2] = sqrt(apStdDev[count2] 
			/ ((double)nValids*(double)g_cmdLine.sampls) );
    }
return 1;
}

/**********************************************************************
* PrintCalcStatus
* prints a line on the screen for the status of calculation
* prints nStdSample if sample is 0
*/
void PrintCalcStatus()
{
static ulong nCount = 0;
fprintf(stderr,"Calculating line %8lu                 \r",nCount);
nCount++;
}

/**********************************************************************
* TransformToImageFile
* transforms the file contents of the text file to an image file
* reads real numbers. These numbers should be between min and max
* Normalizes and creates 8 bit images out of the data
* every line in infile represents one line in the image file
* the numbers in infile should be seperated by spaces
*/
#define ALLOCINC 50
void TransformToImageFile(infile,outfile,min,max)
FILE *infile,*outfile;
double min,max;
{
char *buffer;
unsigned allocated = ALLOCINC + 1;
char *pTmp;
register double lf;
uchar cTmp;
register ulong luLine = 1L;

buffer = malloc(allocated);
while(!feof(infile)) {
	fprintf(stderr,"transfering line %8lu to image file\r",luLine++);
	/* read whole line */
	buffer = fgets_unknown_length(infile,buffer,&allocated);
	if (NULL == buffer) {
		printf("Memory allocation error in TransformToImageFile\n");
		exit(0);
		}
	/* translate buffer and write */
	pTmp = buffer;
	while( '\n' != (*pTmp) && '\0' != (*pTmp) ) {
	    lf = strtod(pTmp,&pTmp);
	    cTmp = (uchar)((lf - min)*255/(max - min));
	    if (!fwrite( &cTmp,sizeof(uchar),1,outfile) )
		printf("writing error\n");
	    }
	} /* end while ! feof */
free (buffer);
return;
}
		
