/* sdevf.c
* sdevf program
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
 
#define PROGRAM         "SDEVF"
#define MAJORVER        1
#define MINORVER        0
#define FACILITY        "Alaska SAR Facility"
#define OUTFILE_I_EXT	".stddev.i"
#define OUTFILE_Q_EXT	".stddev.q"
#define OUTFILE_A_EXT	".stddev.a"
#define IMGFILE_I_EXT	".stddev.i.image"
#define IMGFILE_Q_EXT	".stddev.q.image"
#define IMGFILE_A_EXT	".stddev.a.image"
 
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
	int  bCalculateI;
	int  bCalculateQ;
	int  bCalculateAbs;
	int scans,sampls;
        } g_cmdLine;

/* structure for IQA values */
struct IQA {
	double I,Q,A;
	};

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
printf("%s V%d.%d calculates a field of standard deviations of an echo file\n",
                PROGRAM,MAJORVER,MINORVER);
printf("Assumes that the scan width does not differ within the immage. This\n");
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
printf("\tsdevf infile scans samples [{+|-}type]\n\n");
printf("\t     infile     input file root name.\n");
printf("\t                the extention .echo will be appended\n");
printf("\t     scans	  how many scans to accumulate\n");
printf("\t     samples    how many samples to accumulate\n");
printf("\t     type       type of file to calculate. With + only the listed\n");
printf("\t                types are produced, with - the listed types are\n");
printf("\t                not produced. Valid types are \n");
printf("\t                I - real part\n");
printf("\t                Q - imaginary part\n");
printf("\t                A - absolute value\n");
printf("\t                default is +IQ\n");
printf("\t     the files produced have the same root name than infile and\n");
printf("\t     followed by a extention (i.stddev for result and i.stddev.image\n");

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

/* argv[0] = "sdevf"  */
/* argv[1] = infile   */
/* argv[2] = scans    */
/* argv[3] = sampls   */
g_cmdLine.pszInFile = argv[1];
g_cmdLine.scans = atoi(argv[2]);
g_cmdLine.sampls = atoi(argv[3]);

g_cmdLine.bCalculateI = 1;
g_cmdLine.bCalculateQ = 1;
g_cmdLine.bCalculateAbs = 0;

if (argc == 5) {
    if ('-' == argv[4][0]) {
	g_cmdLine.bCalculateI = (NULL == strchr(argv[4],'I'));
	g_cmdLine.bCalculateQ = (NULL == strchr(argv[4],'Q'));
	g_cmdLine.bCalculateAbs = (NULL == strchr(argv[4],'A'));
	}
    else if ('+' == argv[4][0]) {
	g_cmdLine.bCalculateI = (NULL != strchr(argv[4],'I'));
	g_cmdLine.bCalculateQ = (NULL != strchr(argv[4],'Q'));
	g_cmdLine.bCalculateAbs = (NULL != strchr(argv[4],'A'));
	}
    else
	return 0;
    }
else if (argc > 5)
	return 0;
	
return (g_cmdLine.sampls > 0 && g_cmdLine.scans > 0);
}

/**********************************************************************
* Calculate and write
* assumed is, that all scans have the same width. This is OK for
* ERS1, perhaps for JERS and not for RADARSAT
*/
void CalculateAndWrite()
{
register FILE *fileIout = NULL,*fileIimageOut = NULL;
char *pszIimage;
register FILE *fileQout = NULL,*fileQimageOut = NULL;
char *pszQimage;
register FILE *fileAout = NULL,*fileAimageOut = NULL;
char *pszAimage;
DRDFILE *echo;
ushort uLength,uBlockPerLine;
char *apSample,*pTmp,*pszTmp;
register int	*abValid;
register struct IQA *alfMean,*alfStdDev;
register struct IQA lfMax,lfMin;
register int count;
ulong luRows;

lfMax.I = -1.0; lfMax.Q = -1.0; lfMax.A = -1.0;
lfMin.I = -1.0; lfMin.Q = -1.0; lfMin.A = -1.0;

/* open input files */
echo = echoOpen(g_cmdLine.pszInFile);
if ( NULL == echo ) {
	printf("error opening input file %s.echo\n",g_cmdLine.pszInFile);
	return;
	}
echoGotoFirstScan(echo);

/* open the output files */
if (g_cmdLine.bCalculateI) {
    fileIout = fopendef(NULL,g_cmdLine.pszInFile,OUTFILE_I_EXT,"w+");
    if (NULL == fileIout) {
	fprintf(stderr,"cannot open output file for I-part\n");
	g_cmdLine.bCalculateI = 0;
	} /* no calculation of I part because cannot open file */
    else {
	fileIimageOut=fopendef(NULL,g_cmdLine.pszInFile,IMGFILE_I_EXT,"w+");
        pszIimage = g_hjw_filename;
	if (NULL == fileIimageOut) {
	    fprintf(stderr,"cannot open output image file for I-part\n");
	    g_cmdLine.bCalculateI = 0;
	    }
	}
    }
if (g_cmdLine.bCalculateQ) {
    fileQout = fopendef(NULL,g_cmdLine.pszInFile,OUTFILE_Q_EXT,"w+");
    if (NULL == fileQout) {
        fprintf(stderr,"cannot open output file for Q-part\n");
	g_cmdLine.bCalculateQ = 0;
	}
    else {
        fileQimageOut = fopendef(NULL,g_cmdLine.pszInFile,IMGFILE_Q_EXT,"w+");
	pszQimage = g_hjw_filename;
        if (NULL == fileQout) {
            fprintf(stderr,"cAnnot open output image file for Q-part\n");
	    g_cmdLine.bCalculateQ = 0;
	    }
	}
    }
if (g_cmdLine.bCalculateAbs) {
    fileAout = fopendef(NULL,g_cmdLine.pszInFile,OUTFILE_A_EXT,"w+");
    if (NULL == fileAout) {
        fprintf(stderr,"cannot open output file for A-part\n");
	g_cmdLine.bCalculateAbs = 0; /* No calculation of a because of error */
	}
    else {
	fileAimageOut = fopendef(NULL,g_cmdLine.pszInFile,IMGFILE_A_EXT,"w+");
	pszAimage = g_hjw_filename;
	if (NULL == fileAimageOut) {
            fprintf(stderr,"cannot open image output file for A-part\n");
	    g_cmdLine.bCalculateAbs = 0;
	    }
	}
    }

/* Allocate buffer for apSample and abValid*/
uLength = echoGetCurrentScanLength(echo);
if (uLength <= 0) return;
uBlockPerLine = uLength /(g_cmdLine.sampls * sizeof(SCcomplex)); 
apSample = (char *)calloc(g_cmdLine.scans,uLength * sizeof(char));
abValid = (int *)calloc(g_cmdLine.scans,sizeof(int));
alfMean = (struct IQA *)calloc(uLength/g_cmdLine.sampls,sizeof(struct IQA));
alfStdDev = (struct IQA *)calloc(uLength/g_cmdLine.sampls,sizeof(struct IQA));

/* everything OK? */
if (NULL == apSample || NULL == abValid || NULL==alfMean || NULL==alfStdDev)
	printf("Error allocating memory\n");

/* only calculation if there is something to calculate */
else if (g_cmdLine.bCalculateAbs 
		|| g_cmdLine.bCalculateI 
		|| g_cmdLine.bCalculateQ) 
    {
    /* calculating and writing to text file */
    for (luRows = 0L; ReadCluster(echo,apSample,abValid,uLength); luRows++) {
	if (CalculateCluster(alfMean,alfStdDev,apSample,abValid,uLength) ) {
	    for(count = 0; count < uBlockPerLine; count++) {
		/* search max and min for later normalizatinon */
		if (alfStdDev[count].I > lfMax.I || lfMax.I < 0.0) 
			lfMax.I = alfStdDev[count].I;
		if (alfStdDev[count].Q > lfMax.Q || lfMax.Q < 0.0) 
			lfMax.Q = alfStdDev[count].Q;
		if (alfStdDev[count].A > lfMax.A || lfMax.A < 0.0) 
			lfMax.A = alfStdDev[count].A;
	
		if ( (alfStdDev[count].I < lfMin.I || lfMin.I < 0.0) 
			&& alfStdDev[count].I > 0.0)
			{ lfMin.I = alfStdDev[count].I; }
		if ( (alfStdDev[count].Q < lfMin.Q || lfMin.Q < 0.0) 
			&& alfStdDev[count].Q > 0.0)
			{ lfMin.Q = alfStdDev[count].Q; }
		if ( (alfStdDev[count].A < lfMin.A || lfMin.A < 0.0) 
			&& alfStdDev[count].A > 0.0)
			{ lfMin.A = alfStdDev[count].A; }

		/* output to file */
		if (g_cmdLine.bCalculateI)
		    fprintf(fileIout," %lf",alfStdDev[count].I);
		if (g_cmdLine.bCalculateQ)
		    fprintf(fileQout," %lf",alfStdDev[count].Q);
		if (g_cmdLine.bCalculateAbs)
		    fprintf(fileAout," %lf",alfStdDev[count].A);
		}
	    }
	else {
	    for(count = 0; count < uBlockPerLine; count++) {
		if (g_cmdLine.bCalculateI) fprintf(fileIout," ERROR");
		if (g_cmdLine.bCalculateQ) fprintf(fileQout," ERROR");
		if (g_cmdLine.bCalculateAbs) fprintf(fileAout," ERROR");
		}
	    }
	if (g_cmdLine.bCalculateI)
	    fprintf(fileIout,"\n");
	if (g_cmdLine.bCalculateQ)
	    fprintf(fileQout,"\n");
	if (g_cmdLine.bCalculateAbs)
	    fprintf(fileAout,"\n");
	} /* end while */

    /* writing image file */
    /* this is performed by rereading the output files, normalizing the
    * values and writing them as 8 bit unsigned integer values
    */
    if (g_cmdLine.bCalculateI) rewind(fileIout);
    if (g_cmdLine.bCalculateQ) rewind(fileQout);
    if (g_cmdLine.bCalculateAbs) rewind(fileAout);

    if (g_cmdLine.bCalculateI) {
	printf("SDEVF standard deviation I min = %lf,max = %lf\n",
							lfMin.I,lfMax.I);
	TransformToImageFile(fileIout,fileIimageOut,lfMin.I,lfMax.I);
        }
    if (g_cmdLine.bCalculateQ) {
	printf("SDEVF standard deviation Q min = %lf,max = %lf\n",
							lfMin.Q,lfMax.Q);
	TransformToImageFile(fileQout,fileQimageOut,lfMin.Q,lfMax.Q);
        }
    if (g_cmdLine.bCalculateAbs) {
	printf("SDEVF standard deviation A min = %lf,max = %lf\n",
                                                        lfMin.A,lfMax.A);
	TransformToImageFile(fileAout,fileAimageOut,lfMin.A,lfMax.A);
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
*
* The outer most loop iterates over every block within the cluster, i.e.
* over every standard deviation pixel. The second loop iterates over the
* scans within the cluster and the inner most loop iterates over the 
* values within this scan and block.
*
* returns 0 if less than half of the scans were valid
*/
int CalculateCluster(apMean,apStdDev,apSample,apValid,uLength)
struct IQA *apMean,*apStdDev;
char *apSample;
int *apValid;
ushort uLength;
{
int nValids = 0;
register int count;
register double x,y;
register SCcomplex *pTmp;
register int count1,count2,count3;
int cl = uLength/sizeof(SCcomplex); /* length of apSample in SCcomplex units */

/* calculate the means */
for(count2 = 0;count2 < cl/(g_cmdLine.sampls);count2++) { /*every part of line*/
    apMean[count2].I = 0.0;
    apMean[count2].Q = 0.0;
    apMean[count2].A = 0.0;
    for(count1 = 0; count1 < g_cmdLine.scans; count1++) { /*every line*/
	if (apValid[count1]) {
	    pTmp = ((SCcomplex *)apSample) +count1*cl +count2*g_cmdLine.sampls;
	    for(count3 = 0;count3 < g_cmdLine.sampls;count3++) { /*every value*/
		if (g_cmdLine.bCalculateI)
		    apMean[count2].I += 0.5 + pTmp[count3].I;
		if (g_cmdLine.bCalculateQ)
		    apMean[count2].Q += 0.5 + pTmp[count3].Q;
		if (g_cmdLine.bCalculateAbs) {
		    x = 0.5+pTmp[count3].I; y =0.5+pTmp[count3].Q;
		    apMean[count2].A += sqrt(x*x + y*y);
		    }
		}
	    } /* end for every value */
	/* count the valid lines */
	if (!count2) nValids ++;
	} /* end for every line */
    apMean[count2].I /= (double)nValids * (double)g_cmdLine.sampls;
    apMean[count2].Q /= (double)nValids * (double)g_cmdLine.sampls;
    apMean[count2].A /= (double)nValids * (double)g_cmdLine.sampls;
    } /* end for every part of line  */

/* is enought valids */
if (nValids <= g_cmdLine.scans / 2)
	return 0;

/* calculate stddevs */
for(count2 = 0;count2 < cl/g_cmdLine.sampls;count2++) {
    apStdDev[count2].I = 0.0;
    apStdDev[count2].Q = 0.0;
    apStdDev[count2].A = 0.0;
    for(count1=0; count1 < g_cmdLine.scans; count1++) {
	if (apValid[count1]) {
	    pTmp = ((SCcomplex *)apSample) +count1*cl +count2*g_cmdLine.sampls;
	    for(count3 = 0; count3 < g_cmdLine.sampls; count3++) {
	 	if (g_cmdLine.bCalculateI) {
	 	    x = (0.5 + pTmp[count3].I)  - apMean[count2].I;
		    apStdDev[count2].I += x * x;
		    }
		if (g_cmdLine.bCalculateQ) {
		    x = (0.5 + pTmp[count3].Q) - apMean[count2].Q;
		    apStdDev[count2].Q += x * x;
		    }
		if (g_cmdLine.bCalculateAbs) {
		    x = 0.5+pTmp[count3].I; y =0.5+pTmp[count3].Q;
		    x = sqrt(x*x + y*y) - apMean[count2].A;
		    apStdDev[count2].A += x * x;
		    }
		}
	    }
	}
    apStdDev[count2].I = sqrt(apStdDev[count2].I 
			/ ((double)nValids*(double)g_cmdLine.sampls) );
    apStdDev[count2].Q = sqrt(apStdDev[count2].Q
                        / ((double)nValids*(double)g_cmdLine.sampls) );
    apStdDev[count2].A = sqrt(apStdDev[count2].A
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
		
