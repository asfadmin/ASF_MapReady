/* anapulse 
* analyzes calibration or replica pulses
* in the current state it analyzes the first pulse of the dataset. It is
* planed to extend the analysis to all pulses and calculate mean values
* minimal maximal and standard deviations of the results for every pulse
*
* The folowing analyzis is done over the pulses:
* pulse duration
* FFT over the whole pulse -> frequence range, power
* FFT over parts of the pulse to get a frequence over time dependency
*
* Programmed by Hans-Joerg Wagner
*
*/
#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include "/sonja/calval/cal/include/sarmacros.h"   /* for Complex data type */
#include "drdapi.h"

#define FACILITY	"Alaska SAR Facility"
#define PROGRAM 	"ANAPULSE"
#define VER_MAJOR	0
#define VER_MINOR	1
/* V 0.1  08-15-1994 by Hans-Joerg Wagner */

/* FPOINT has to be a power of 2 !!!! */
#define FPOINT  32

/* Makro calculates the next greater value that is a power of 2 */
#define NEXT_POW2(x)  ((int)exp2( ceil( log2( (double)x ) ) ) )

/* prototypes for complex functions */
double Cabs();

/* prototypes */
void Hello();
void Bye();
void Help();
int AnalyseFile();
void FrequencyAnalysis();
void FrequencyOverTime();
void AnalyseBuffer();
int CmdLine();
void ShowDiagram();
void RotateBuffer();
void InvertBuffer();
void PlotAmplitude();
void PlotDouble();
void HammingWindow();
void RectWindow();

struct {
	char *pszInFile;
	ulong luPulseNumber;
	} g_cmdLine;


main(argc,argv)
int argc;
char **argv;
{
DRDFILE *pFile,*pAux;
char *pTmp;

Hello();

if (!CmdLine(argc,argv))
	Help();
else if (!drdInit(PROGRAM,VER_MAJOR,VER_MINOR,FACILITY) )
	printf("%s cannot initialize drd library\n",PROGRAM);
else if (NULL == (pAux =drdOpenOtherFileType(g_cmdLine.pszInFile,AUX_FILEEXT)))
	printf("Cannot open matching auxiliary file\n");
else if (NULL == (pFile = drdOpen(g_cmdLine.pszInFile))) {
	printf("Error opening input file %s\n",g_cmdLine.pszInFile);
	drdClose(pAux);
	}
else {
	AnalyseFile(pFile,pAux);
	drdClose(pFile);
	drdClose(pAux);
	}
Bye();
}

/**********************************************************************
* Hello says hello
* Bye says bye
* Help prints help
*/
void Hello()
{
printf("%s analysis of calibration pulses or replica pulses\n",PROGRAM);
}

void Bye()
{
printf("%s end. I hope my results made no trouble\n",PROGRAM);
}

void Help()
{
printf("usage:\n");
printf("\t anapulse infile [pulseno]\n");
printf("\t       infile		name of the input file with extention\n");
printf("\t       pulseno        number of the pulse to analyze. Without\n");
printf("\t                      this argument every pulse is analyzed and\n");
printf("\t                      output contains average and stddev borders\n");
printf("\t                      the first pulse has the number 1\n");
}

/* CmdLine analyzes the command line */
int CmdLine(argc,argv)
int argc;
char **argv;
{
char *pTmp;

if (argc < 2 || argc > 3)
	return 0;

g_cmdLine.pszInFile = argv[1];
g_cmdLine.luPulseNumber = (argc > 2) ? atol(argv[2]) : 0L;

return 1;
}

/**********************************************************************
* AnalyseFile analyzes the file given.
* assumes that all samples within one file have the same size
* returns 0 on error
*/
int AnalyseFile(pFH,pAux)
DRDFILE *pFH,*pAux;
{
ushort buflen,i,nValues;
ulong count;
ulong luImScNo;
double fSampl,fFc;
Complex *acFrequency;
double *afTimeToFreq,*afTTFmean,*afFreqMean;
/* to assure that buffer size and frequencies do not change */
double statSampl = 0.0, statFc = 0.0;

/* finding first datablock */
if ( !drdGotoFirstBlock(pFH) ) {
	printf("Error finding first data block in file\n");
	return 0;
	}

/* get the size of the current data block */
buflen = drdGetCurrentBlockLength(pFH);
if (0 == buflen) {
	printf("Error getting Blocklength\n");
	return 0;
	}
nValues = buflen / sizeof(ByteComplex);

/* allocatin space for acFrequency and acTimeToFreq */
acFrequency = (Complex *)malloc(NEXT_POW2(nValues) * sizeof(Complex));
afTimeToFreq = (double *)malloc(nValues / FPOINT * 2 * sizeof(double));
if (NULL == acFrequency | NULL == afTimeToFreq) {
	fprintf(stderr,"Error allocating memory for analysis\n");
	return 0;
	}

/* now everything is ready for analysis */
/* there are two cases to distinguish - a special pulse or the average over
* every pulse.
* first lets analyse a special pulse
*/
if (0L != g_cmdLine.luPulseNumber) {
    /* find the pulse */
    for (count = 1; count < g_cmdLine.luPulseNumber; count++) {
	if (!drdGotoNextBlock(pFH) ) {
	    printf("Cannot find Pulse # %lu\n",g_cmdLine.luPulseNumber);
	    return 0;
	    }
	}
    /* is the assumption right? */
    if (buflen != drdGetCurrentBlockLength(pFH) ) {
	printf("The buffer length of the first and the %lu. pulse differ\n",
				g_cmdLine.luPulseNumber);
	return 0;
	}
    /* analyze buffer */
    GetAndAnalysePulse(pFH,buflen,pAux,acFrequency,afTimeToFreq,&fSampl,&fFc);

    /* and print results */
    PlotAmplitude(acFrequency,1,NEXT_POW2(nValues),
                fFc-fSampl/2.0,fFc+fSampl/2.0,
                "frequency", "amplitude",
                "Frequency of pulse",
                "anapulse plot");

    PlotDouble(afTimeToFreq, nValues / FPOINT * 2 - 1,
                (double)FPOINT/2.0/fSampl,
                (double)(nValues-FPOINT/2.0)/fSampl,
                "Time","frequency",
                "Frequency to Time dependency","anapulse plot");
    } /* end if only a single pulse */
/* now if more than one pulse */
else {
    /* I need more space */
    afFreqMean = (double *)calloc(NEXT_POW2(nValues),sizeof(double));
    afTTFmean = (double *)calloc(nValues / FPOINT * 2,sizeof(double));
    /* now calculate the means */
    for (count = 0; ; count++) {
	printf("Analysing block %lu\r",count+1);
	/* test the asumption */
	if (buflen != drdGetCurrentBlockLength(pFH) ) {
	    printf("The buffer length of the first and the %lu. pulse differ\n",
                                g_cmdLine.luPulseNumber);
            return 0;
            }
	/* analyse current pulse */
	GetAndAnalysePulse(pFH,buflen,pAux,acFrequency,afTimeToFreq,
								&fSampl,&fFc);
	/* test the frequency and samplespeed assumptions */
	if (0.0 == statSampl) statSampl = fSampl;
	if (0.0 == statFc) statFc = fFc;
	if (statSampl != fSampl || statFc != fFc) {
		printf("Sample or center frequency changed at pulse %lu\n",
					g_cmdLine.luPulseNumber);
		return 0;
		}
	/* now add everything to the means */
	for (i = 0; i < NEXT_POW2(nValues); i++)
	    afFreqMean[i] += Cabs(acFrequency[i]);
	for (i = 0; i < nValues / FPOINT * 2 - 1; i++)
	    afTTFmean[i] += afTimeToFreq[i];
	if (!drdGotoNextBlock(pFH) ) 
		break;
	} /* end for every pulse */
    /* divide */
    for (i = 0; i < NEXT_POW2(nValues); i++)
            afFreqMean[i] /= (double) count;
    for (i = 0; i < nValues / FPOINT * 2 - 1; i++)
            afTTFmean[i] /= (double) count;

    /* and print results */
    printf("Analysed %lu pulses\n",count);
    PlotDouble(afFreqMean,NEXT_POW2(nValues),
                fFc-fSampl/2.0,fFc+fSampl/2.0,
                "frequency", "amplitude",
                "Frequency of pulse",
                "anapulse plot");
 
    PlotDouble(afTTFmean, nValues / FPOINT * 2 - 1,
                (double)FPOINT/2.0/fSampl,
                (double)(nValues-FPOINT/2.0)/fSampl,
                "Time","frequency",
                "Frequency to Time dependency","anapulse plot");
    free(afFreqMean);
    free(afTTFmean);
    } /* end mean over all pulses */

free(acFrequency);
free(afTimeToFreq);

return 1;
}

/**********************************************************************
* GetAndAnalysePulse
* read the current pulse from pFH and analyzes it.
* returns the results in acFrequency and afTimeToFreq
* acFrequency has to have at least the size buflen/2
* afTimeToFreq has to have at least the size buflen/FPOINT
* in *pfFsampl the sample frequency is returned and in *pfFc the center
* frequency
*
* return 0 if an error occures
*/
int GetAndAnalysePulse(pFH,buflen,pAux,acFrequency,afTimeToFreq,pfFsampl,pfFc)
DRDFILE *pFH,*pAux;
int buflen;		/* size of buffer to read */
Complex *acFrequency;
double *afTimeToFreq;
double *pfFsampl,*pfFc;
{
char *buffer;
ByteComplex *source;
ushort nValues,count;
ulong luImScNo;
double fMixShift;
Complex *acValues;
 
buffer = malloc(buflen);
if (NULL == buffer) {
        printf("Error allocating memory\n");
        return 0;
        }
 
if ( !drdGetCurrentBlock(pFH,&luImScNo,buffer) ) {
        printf("Error reading data block\n");
        return 0;
        }
 
nValues = buflen / sizeof(ByteComplex);
 
/* allocatin space for acFrequency and acTimeToFreq */
acValues = (Complex *)malloc(nValues * sizeof(Complex));
if (NULL == acValues) {
        fprintf(stderr,"Error allocating memory for analysis\n");
        return 0;
	}

/* generating double data field */
source = (ByteComplex *)buffer;
for(count = 0; count < nValues; count++) {
        acValues[count].real = (float)source[count].real + 0.5;
        acValues[count].imag = (float)source[count].imag + 0.5;
        }
 
/* reading sampling rate */
(*pfFsampl) = 1000.0 * (double)auxGetSamplingRate(pAux,pFH->szType,luImScNo);
(*pfFc) = 1000.0 * (double)auxGetRadarFrequency(pAux,luImScNo);
fMixShift = 1000.0 *
                (double)auxGetMixingFrequencyShift(pAux,pFH->szType,luImScNo);
 
FrequencyAnalysis(acFrequency,acValues,nValues,*pfFsampl,*pfFc,fMixShift);
FrequencyOverTime(afTimeToFreq,acValues,nValues,*pfFsampl,*pfFc,fMixShift);

free(acValues);
free(buffer);
}

/**********************************************************************
* FrequencyAnalysis
* executes a fourier transformation over the signal in fSource
* and stores the result in fDest.
* the buffer fDest has to have at least the size of the next pow 2 value
* to nSamples.
* fDest contains as result the frequency range from fFcenter - fFsamples/2
* to fFcenter + fFsamples/2 in NEXT_POW2(nSamples) equidistant values
*
* Due to the signal path before be sampled after the fft there are more 
* steps to get the original signal. First the result has to be inverted
* i.e. the value[0] will become value[nmax] etc. This is because the
* signal sampled was a lower single side band signal. Than the pcDest
* needs to be rotated due to the undersampling of the signal. For more
* detailed description see the "Summarization of the OGRC-DATA provided
* by the ERS-1 Satellite" document
*/
void FrequencyAnalysis(pcDest,pcSource,nSamples,fFsamples,fFcenter,fMixShift)
Complex *pcDest,*pcSource;
int nSamples;
double fFsamples,fFcenter,fMixShift;
{
int k,nrot;
double frot;

/* copy the data to destination by filling it up to NEXT_POW2(nSamples) with
* zeros to perform the fft
*/
RectWindow(pcDest, pcSource, nSamples);

/* Now execute the fft forewards */
fft1d(pcDest,NEXT_POW2(nSamples),-1);

/* Adapt values to frequency window. Rotate right
* see Summaryzation of the OGRC data of the ERS-1 Satellite,
* paragraph "Sampling"
* The buffer has to be inverted too, because the signal is a
* lower single side band signal
*/
k = (long)(((fFcenter - fMixShift) - fFsamples/2.0)/fFsamples);
frot = (fFcenter-fMixShift) - (0.5 + (double)k)*fFsamples;
nrot = (long)(frot*(double)NEXT_POW2(nSamples)/fFsamples);
InvertBuffer(pcDest,(ulong)NEXT_POW2(nSamples));
RotateBuffer(pcDest,(ulong)NEXT_POW2(nSamples),nrot);
}

/**********************************************************************
* FrequencyOverTime
* analyzes pfSource for a time to frequency dependency. Therefore
* a window of size FPOINT (Hamming window function) is moved over the 
* samples in pfSource. Over the values within this window a fft is
* performed and the maximum value of the result is mapped to its
* fitting frequency.
* The window is moved in FPOINT/2 steps
* after returning pfDest holds nSamples / FPOINT * 2 - 1 values.
*
* To calulate the final frequency basicly the same steps as described
* for the  FrequencyAnalysis function are required. For further information
* see the "Summarization of the OGRC-Data provided by the ERS-1 Sattelite" doc.
*/
void FrequencyOverTime(pfDest,pcSource,nSamples,fFsamp,fFcent,fMixShift)
double *pfDest;
Complex *pcSource;
int nSamples;
double fFsamp,fFcent,fMixShift;
{
long k,nrot;
int count,i,nMax;
double frot,fMax;
Complex *pcTransBuf = (Complex *)malloc(FPOINT * sizeof(double));
 
k = (long)(((fFcent- fMixShift) - fFsamp/2.0)/fFsamp);
frot = (fFcent- fMixShift) - (0.5 + (double)k)*fFsamp;
nrot = (long)(frot*(double)FPOINT/fFsamp);
 
if (NULL == pcTransBuf) {
        fprintf(stderr,"Error allocating memory for pfTimeFreq\n");
        return;
        }
if (NEXT_POW2(FPOINT) != FPOINT) {
	fprintf(stderr,"internal error!! FPOINT is not a power of 2\n");
	exit(1);
	}
 
for(count = 0; count < nSamples / FPOINT * 2 - 1; count++) {
        /* select area */
        HammingWindow(pcTransBuf, pcSource + count * FPOINT/2, FPOINT);
        /* transformation */
        fft1d(pcTransBuf,FPOINT,-1);
        InvertBuffer(pcTransBuf,FPOINT);
        RotateBuffer(pcTransBuf,FPOINT,nrot);
        /* find maximum , only possitive frequencies */
        nMax = 0; fMax = 0.0;
        for (i = 0; i < FPOINT; i++) {
                if (Cabs(pcTransBuf[i]) > fMax) {
                        nMax = i; fMax = Cabs(pcTransBuf[i]);
                        }
                }
        /* entry into field */
        pfDest[count] = (fFcent) - fFsamp/2.0 +
                ((double)nMax)*fFsamp/(double)(FPOINT);
        /* printf("nMax %d, pfDest[%d] = %lf\n",nMax,count,pfDest[count]); */
        }
}

/**********************************************************************
* RotateBuffer 
* rotates the values in pfBuffer for luRotate bytes
*/
void RotateBuffer(pfBuffer,luLength,luRotate)
double *pfBuffer;
ulong luLength;
ulong luRotate;
{
double fTmp;
register ulong count,count1;

luRotate %= luLength;
for (count1=0; count1 < luRotate; count1 ++) {
    fTmp = pfBuffer[0];
    for (count=0; count < luLength-1; count++)
	pfBuffer[count] = pfBuffer[count+1];
    pfBuffer[luLength-1] = fTmp;
    }
} 

/**********************************************************************
* InvertBuffer
* inverts the index in buffer. i.e. pfBuffer[0] becomes pfBuffer[luLength]...
*/
void InvertBuffer(pfBuffer,luLength)
double *pfBuffer;
ulong luLength;
{
register double fTmp;
register ulong count;

for (count = 0; count < luLength/2; count++) {
	fTmp = pfBuffer[count];
	pfBuffer[count] = pfBuffer[luLength - count - 1];
	pfBuffer[luLength - count - 1] = fTmp;
	}
}

/**********************************************************************
* PlotAmplitude
* expects an complex two dimensional array and plots the amplitudes in y axis.
* the x axis is linear divided between xmin and xmax
* n is the number of y values corresponding to 1 x value
* m is the number of x - y tuples
* the array expected has the dimensions [m][n] where m is the # of the
* value of x and n is the # of the y value. I.e. all values corresponding
* to one x values are stored continously in the buffer, followed by the next
* group for the next x value
* xtext and ytext are the axii labels
*/
void PlotAmplitude(a,n,m,xmin,xmax,pszXtext,pszYtext,pszTitle,pszWindow)
Complex *a;
int n,m;
double xmin,xmax;
char *pszXtext,*pszYtext;
char *pszTitle,*pszWindow;
{
char *pTmpName,*pszCmdLine;
FILE *file;
register int i,j;
char *pTmp;
double x, xDelta = (xmax - xmin)/(double)m;
static char szDiaCmd[] = "diagram %s -g -d &";

/* crate a temporary file */
pTmpName = tempnam(NULL,"anapulse");
if (NULL == pTmpName) {
	fprintf(stderr,"Error generating temporary file name\n");
	return;
	}
file = fopen(pTmpName,"w");
if (NULL == file) {
	fprintf(stderr,"Error opening temporary file\n");
	return;
	}

/* writing header */
if (NULL != pszTitle)
    fprintf(file,"TITLE %s\n",pszTitle);
if (NULL != pszWindow)
    fprintf(file,"WINDOW %s\n",pszWindow);
if (NULL != pszXtext)
    fprintf(file,"X %s\n",pszXtext);
if (NULL != pszYtext)
    fprintf(file,"Y %s\n",pszYtext);

printf("print data into file\n");
/* print data into file */
for (i = 0,x=xmin; i < m; x+=xDelta,i++) {
    fprintf(file,"%lf",x);
    for(j = 0; j < n; j++)
	fprintf(file,"\t%lf",Cabs((a+i*n)[j]));
    fprintf(file,"\n");
    }

/* close file */
fclose(file);

/* start diagram program */
printf("generating command line for diagram\n");
pszCmdLine = malloc( strlen(szDiaCmd) + strlen(pTmpName) + 2 );
if (NULL == pszCmdLine) {
	fprintf(stderr,"Error allocating memory for command line\n");
	unlink(pTmpName);
	return;
	}
sprintf(pszCmdLine,szDiaCmd,pTmpName);
printf("starting program\n%s\n",pszCmdLine);
system(pszCmdLine);
free(pszCmdLine);
}

/**********************************************************************
* PlotDouble
* expects an double array and plots the values in y axis.
* xtext and ytext are the axii labels
*/
void PlotDouble(a,buflen,xmin,xmax,pszXtext,pszYtext,pszTitle,pszWindow)
double *a;
int buflen;
double xmin,xmax;
char *pszXtext,*pszYtext;
char *pszTitle,*pszWindow;
{
char *pTmpName,*pszCmdLine;
FILE *file;
register int i;
char *pTmp;
double x, xDelta = (xmax - xmin)/(double)buflen;
static char szDiaCmd[] = "diagram %s -g -d &";

/* crate a temporary file */
pTmpName = tempnam(NULL,"anapulse");
if (NULL == pTmpName) {
	fprintf(stderr,"Error generating temporary file name\n");
	return;
	}
file = fopen(pTmpName,"w");
if (NULL == file) {
	fprintf(stderr,"Error opening temporary file\n");
	return;
	}

/* writing header */
if (NULL != pszTitle)
    fprintf(file,"TITLE %s\n",pszTitle);
if (NULL != pszWindow)
    fprintf(file,"WINDOW %s\n",pszWindow);
if (NULL != pszXtext)
    fprintf(file,"X %s\n",pszXtext);
if (NULL != pszYtext)
    fprintf(file,"Y %s\n",pszYtext);

printf("print data into file\n");
/* print data into file */
for (i = 0,x=xmin; i < buflen; x+=xDelta,i++) 
    fprintf(file,"%lf\t%lf\n",x,a[i]);

printf("printed data into file\n");
/* close file */
fclose(file);

/* start diagram program */
printf("generating command line for diagram\n");
pszCmdLine = malloc( strlen(szDiaCmd) + strlen(pTmpName) + 2 );
if (NULL == pszCmdLine) {
	fprintf(stderr,"Error allocating memory for command line\n");
	unlink(pTmpName);
	return;
	}
sprintf(pszCmdLine,szDiaCmd,pTmpName);
printf("starting program\n%\ns",pszCmdLine);
system(pszCmdLine);
free(pszCmdLine);
}

/**********************************************************************
* HammingWindow
* copies n values from src to dest applying the Hamming window function
* on both, the real and the imaginary part of the data
*
* then it fills the dest field with 0 up to the next power 2 index
* WARNING: the destination field may have to be larger than the src field
* according to the next power of 2 of values
*/
void HammingWindow(dest,src,n)
Complex *dest,*src;
int n;
{
register int i;
float hamming;
int end = NEXT_POW2(n);

for (i=0; i<n; i++) {
	hamming = 0.54 - 0.46 * cos( 2.0 * PI * (double)i / (double)(n-1) );
	dest[i].real = hamming * src[i].real;
	dest[i].imag = hamming * src[i].imag;
	}
for (; i < end; i++)
        Czero(dest[i]);
}

/**********************************************************************
* RectWindow
* copies n values from src to dest and fills with 0 up to the next power
* of 2.
* WARNING: the destination field may have to be larger than the src field
* according to the next power of 2 of values
*/
void RectWindow(dest,src,n)
Complex *dest,*src;
int n;
{
register int i,end = NEXT_POW2(n);


for (i=0; i < n; i++) {
	dest[i].real = src[i].real;
	dest[i].imag = src[i].imag;
	}
for (; i < end; i++)
	Czero(dest[i]);
}






