/* histo.c
* histo program
* calculates a histogram over a echo or/and noise data file
* created by the ANDI tool
*
* First setup 07-26-1994 by Hans-Joerg Wagner
*/
#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include "drdapi.h"

#define PROGRAM		"HISTO"
#define MAJORVER	1
#define MINORVER	0
#define FACILITY	"Alaska SAR Facility"

/* prototypes */
void Hello();
void Bye();
void Usage();
void CalculateAndPrint();
int CommandLine();
void OutputHistogram();
void PlotHistogram();
void CCHisto();
double MeanFromHisto();
double StddevFromHisto();
double MinFromHisto();
double MaxFromHisto();


/* global structure for command line arguments */
struct {
	char *pszInFile;
	char *pszOutFile;
	int bPlot;
	int bPrintValues;
	enum { echo, noise, plain } whichFile;
	} g_cmdLine;


main(argc,argv)
int argc;
char **argv;
{
Hello();
if (!CommandLine(argc,argv)) {
	Usage();
	exit(1);
	}
if ( ! drdInit(PROGRAM,MAJORVER,MINORVER,FACILITY) ) {
	printf("error initializing DRD library\n");
	exit(1);
	}

CalculateAndPrint();

drdExit();
Bye();
}

/**********************************************************************
* Hello says hello
* Bye says good bye
*/
void Hello()
{
printf("%s V%d.%d calculates a histogram for a file\n",
		PROGRAM,MAJORVER,MINORVER);
}

void Bye()
{
printf("%s says good bye - enjoy the result\n",PROGRAM);
}

/**********************************************************************
* Usage
* explains the usage
*/
void Usage()
{
printf("usage:\n");
printf("\thisto infile -o outfile -p\n\n");
printf("\tinfile   name of a DRD file\n");
printf("\t-o outfile  writes the histogram values to output file\n");
printf("\t         if outfile is not given, values are printet to screen\n");
printf("\t-p	   plot as graphic to screen\n");
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

if (argc < 2 ) 
	return 0;

/* argv[0] = "histo"			*/
/* argv[1] = infile			*/
g_cmdLine.pszInFile = argv[1];
g_cmdLine.pszOutFile = NULL;
g_cmdLine.bPlot = 0;
g_cmdLine.bPrintValues = 0;

for (count = 2; count < argc; count++) {
    if (argv[count][0] == '-') {
	switch (argv[count][1]) {
	    case 'p':
		g_cmdLine.bPlot = 1;
		break;
	    case 'o':
		/* output file */
		if (argv[count][2] || NULL != g_cmdLine.pszOutFile)
			return 0;
		g_cmdLine.bPrintValues = 1;
		count++;
		if (count >= argc)
			break;
		if (argv[count][0] == '-') {
			count--;
			break;
			}
		g_cmdLine.pszOutFile = argv[count];
		break;
	    default:
		return 0;
	    }
	}
    }
return 1;
}

/**********************************************************************
* CalculateAndPrint
* does what the name says
*/
void CalculateAndPrint()
{
ulong 	aluHistoI[256];  /* offset 128 equals 0 bzw 0.5 */
ulong 	aluHistoQ[256];
ulong	luDummy,luBytesProcessed = 0;
DRDFILE *drdFile = NULL;
FILE	*file = NULL,*outfile = stdout;
int	end = 0, error = 0, count;
char	*buffer;
ushort	uLength;
double	fStddevI,fStddevQ;
double	fMeanI,fMeanQ;
double	fMaxI,fMaxQ,fMinI,fMinQ;

/* reset histogram buffers */
for (count = 0; count < 256; count++) {
	aluHistoI[count] = 0L;
	aluHistoQ[count] = 0L;
	}

/* open files */
drdFile = drdOpen(g_cmdLine.pszInFile);
if (NULL == drdFile) {
	printf("error opening input file\n");
	return;
	}
if (       strcmp(drdFile->szType,REP_FILETYPE) 
	&& strcmp(drdFile->szType,CAL_FILETYPE)
	&& strcmp(drdFile->szType,ECHO_FILETYPE)
	&& strcmp(drdFile->szType,NOISE_FILETYPE)
	&& strcmp(drdFile->szType,ZERO_FILETYPE) )
    {
    printf("Cannot process file type '%s'\n",drdFile->szType);
    return;
    }

error = !drdGotoFirstBlock(drdFile);
if (error) {
	printf("error finding start position in input file\n");
	return;
	}
if (NULL != g_cmdLine.pszOutFile) {
	outfile = fopen(g_cmdLine.pszOutFile,"w");
	if (NULL == outfile) {
		printf("Cannot open outputfile %s\n",g_cmdLine.pszOutFile);
		return;
		}
	}

/* calculate histogram */
do {
	uLength = drdGetCurrentBlockLength(drdFile);
	buffer = malloc(uLength);
	drdGetCurrentBlock(drdFile,&luDummy,buffer);
	CCHisto(aluHistoI,aluHistoQ,buffer,uLength);
	free(buffer);
	luBytesProcessed += uLength;
	printf("%10lu Bytes processed\r",luBytesProcessed);
	} while(drdGotoNextBlock(drdFile));
printf("\n");

/* output of Histogram */
if (g_cmdLine.bPrintValues)
	OutputHistogram(outfile,aluHistoI,aluHistoQ);

/* calculate mean and standard deviation */
fMeanI = MeanFromHisto(aluHistoI);
fMeanQ = MeanFromHisto(aluHistoQ);
fStddevI = StddevFromHisto(aluHistoI,fMeanI);
fStddevQ = StddevFromHisto(aluHistoQ,fMeanQ);
fMaxI = MaxFromHisto(aluHistoI);
fMaxQ = MaxFromHisto(aluHistoQ);
fMinI = MinFromHisto(aluHistoI);
fMinQ = MinFromHisto(aluHistoQ);

/* output to stdout */
fprintf(outfile,"\n    mean       s.dev      Min        Max\n");
fprintf(outfile,"I %8.3lf   %8.3lf   %8.1lf   %8.1lf\n",
		fMeanI,fStddevI,fMinI,fMaxI);
fprintf(outfile,"Q %8.3lf   %8.3lf   %8.1lf   %8.1lf\n",
		fMeanQ,fStddevQ,fMinQ,fMaxQ);
fprintf(outfile,"\nI-Q-imballance = %lf\n\n",fabs(fMeanI - fMeanQ));

/* plot of Histogram */
if (g_cmdLine.bPlot)
	PlotHistogram(aluHistoI,aluHistoQ);

/* close files */
drdClose(drdFile);
if (NULL != g_cmdLine.pszOutFile)
	fclose(outfile);
}

/**********************************************************************
* OutputHistogram
* in the moment no output of Histogram
*/
void OutputHistogram(outfile,histoI,histoQ)
FILE *outfile;
ulong *histoI;
ulong *histoQ;
{
int count,i,count0 = 0;

/* first skip leading 0 */
for (count = 0; count < 256 && histoI[count]==0 && histoQ[count]==0;count++);

/* printing histogram */
fprintf(outfile,"Histogram:\n    n\t       I\t       Q\n");
/* if both histogram entries are 0 it does not print the 0's until another */
/* entry not equal to 0 is found. Thus trailing 0's will not be print */
for (;count < 256; count++) {
    if ( histoI[count] != 0 || histoQ[count] != 0 ) {
	for (i=0; i < count0; i++)
	    fprintf(outfile,"%6.1lf\t%8lu\t%8lu\n",(double)count-127.5,0L,0L);
	fprintf(outfile,"%6.1lf\t%8lu\t%8lu\n",(double)count - 127.5,
					histoI[count],histoQ[count]);
	count0 = 0;
	}
    else
	count0++;
    }
}

/**********************************************************************
* CCHisto adds the given buffer to the histogram
* the histograms I and Q are assumed with an index range from -128 to
* + 127.
*/
void CCHisto(Iin,Qin,buffer,length)
ulong *Iin,*Qin;
char *buffer;
ushort length;
{
register int count;
register char *b = buffer;
register ulong *I = Iin;
register ulong *Q = Qin;
register ushort l = length;

for (count = 0; count < l; count++) {
	I[ b[count++] + 128 ] ++;
	Q[ b[count] + 128 ] ++;
	}
}

/**********************************************************************
* MeanFromHisto
* calculates the mean value from a histogram. It expects a histogram of
* 256 entries. The zero based index 128 represents the value 0.5
*/
double MeanFromHisto(histo)
ulong *histo;
{
register int count;
register ulong elements = 0;
double mean = 0;

for(count = 0; count < 256; count++)  {
	mean += histo[count] * ( (double)count - 127.5 );
	elements += histo[count];
	}

return mean/(double)elements;
}

/**********************************************************************
* StddevFromHisto
* calculates the standard deviation from a histogram. It expects a histogram
* with 256 entries. The zero based index 128 represents the value 0.5
* The second parameter is the already calculated mean!!!
*/
double StddevFromHisto(histo,mean)
ulong *histo;
double mean;
{
register int count;
register ulong elements = 0;
double sdev,x;

for(count = 0; count < 256; count++) {
	x = ((double)count - 127.5) - mean;
	sdev += histo[count] * x * x;
	elements += histo[count];
	}
return sqrt(sdev/(double)elements);
}

/**********************************************************************
* MinFromHisto MaxFromHisto
* searches for the minimum/maximum value within a histogram, that is countet
* more than 0 times. The function expects a histogram with 256 entries.
* The zero based index 128 represents the value 0.5
*/
double MinFromHisto(histo)
ulong *histo;
{
register int count;

for (count = 0; count < 256 && histo[count] == 0; count++);
return (double)count - 127.5;
}

double MaxFromHisto(histo)
ulong *histo;
{
register int count;

for (count = 255; count >= 0 && histo[count] == 0; count--);
return (double)count - 127.5;
}

/**********************************************************************
* PlotHistogram
* plots an I and an Q histogram to the screen useing the tool Diagram
*/
void PlotHistogram(histoI,histoQ)
ulong *histoI;
ulong *histoQ;
{
int count,i,count0 = 0;
FILE *Ifile,*Qfile;
char *pszIfile, *pszQfile;
char *pszTmp;
static char szPlotCommand[]="diagram %s -t '%c-Histogram of %s' -w '%c-Histogram' &";

/* open temp files to transfer data */
pszIfile = tempnam(NULL,"histo");
if (NULL == pszIfile)
	{ printf("Error creating temporary file\n"); return; }
Ifile = fopen(pszIfile,"w");
if (NULL == Ifile)
	{ printf("Error creating temporary file\n"); return; }
pszQfile = tempnam(NULL,"histo");
if (NULL == pszQfile)
        { printf("Error creating temporary file\n"); return; }
Qfile = fopen(pszQfile,"w");

/* writing header */
fprintf(Ifile,"X amplitude\nY count\n");
fprintf(Qfile,"X amplitude\nY count\n");

/* skip leading 0 */
for (count = 0; count < 256 && histoI[count]==0 && histoQ[count]==0;count++);
 
/* if both histogram entries are 0 it does not print the 0's until another */
/* entry not equal to 0 is found. Thus trailing 0's will not be print */
for (;count < 256; count++) {
    if ( histoI[count] != 0 || histoQ[count] != 0 ) {
        for (i=0; i < count0; i++) {
            fprintf(Ifile,"%6.1lf\t%8lu\n",(double)count-127.5,0L);
            fprintf(Qfile,"%6.1lf\t%8lu\n",(double)count-127.5,0L);
	    }
        fprintf(Ifile,"%6.1lf\t%8lu\n",(double)count - 127.5,histoI[count]);
        fprintf(Qfile,"%6.1lf\t%8lu\n",(double)count - 127.5,histoQ[count]);
        count0 = 0;
        }
    else
        count0++;
    }

/* close files */
fclose(Ifile);
fclose(Qfile);

/* call diagram function */
pszTmp = malloc(strlen(szPlotCommand) + strlen(pszIfile) +
					strlen(g_cmdLine.pszInFile));
if (NULL == pszTmp) {
	printf("malloc error\n");
	return;
	}
sprintf(pszTmp,szPlotCommand, pszIfile,'I', g_cmdLine.pszInFile,'I' );
system (pszTmp);
free(pszTmp);
pszTmp = malloc(strlen(szPlotCommand) + strlen(pszIfile) +
					strlen(g_cmdLine.pszInFile));
if (NULL == pszTmp) {
	printf("malloc error\n");
	return;
	}
sprintf(pszTmp,szPlotCommand,pszQfile,'Q', g_cmdLine.pszInFile,'Q' );
system(pszTmp);
free(pszTmp);
}

