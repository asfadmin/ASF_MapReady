/*SPECAN SAR Processor Include File
by Orion Lawlor, ASF  11/98.

SPECAN-- SPECtral ANalysis SAR processor.
The basic idea is:
A chunk of raw SAR signal data has information about
many processed pixels around it-- some are before this 
chunk (at lower frequencies), some are after this chunk
(at higher frequencies) and some are right at this chunk.
It turns out, we can get a small, subsampled picture of
many surrounding pixels by just FFT'ing a small piece of
signal data.  To focus this picture better, we have to
"deramp" the signal data by the negative of the chirp
slope before the FFT.

It's a pretty efficient approach in general, but is
especially nice for quick-look processing (e.g., to
generate lo-res browse images).
*/

#define SPECAN_MAXFFT 2048 /*Longest possible FFT for SPECAN.*/
typedef struct {
/*Parameters you must fill out.*/
	double iSamp;/*Input sampling period [s] (=1/fs, =pixSize/c)*/
	double chirpSlope;/*Chirp slope [Hz/s]*/
	double chirpCenter;/*Chirp center frequency [Hz]*/
	double bandwidth;/*Fraction of bandwidth to use [fraction]*/
	int fftLen;/*Length of Specan's FFT's.*/
	double powerCenter;/*Center of signal power, [Hz]*/
/*Calculated Parameters.*/
	double oSamp;/*Output sampling period [s] (=outPixSize/c)*/
	double oFirst,oLast;/*First and last samples in output space.*/
	FCMPLX deramp[SPECAN_MAXFFT];
/*Interesting Calculated Parameters.*/
	int iFirst,iLast;/*First and last input samples, relative to start of specan buffer*/
	int iNum;/*Number of valid input pixels.*/
	int oNum;/*Number of valid output pixels.*/
	double oCenter;/*Location of power center frequency in output space.*/
} specan_struct; /*A structure describing one dimension of specan processing.*/

/*SAR Process an entire file, chunk-by-chunk, writing amplitude
to outFile (in specan_file.c)*/
void specan_file(getRec *inFile,int nLooks,FILE *outFile,
	specan_struct *rng,specan_struct *az,
	int *out_lines,int *out_samples); 

/*Internal SPECAN patch-by-patch routines (in specan_patch.c)*/
typedef struct {
	int iWid,iHt;/*Width, height of input buffer.*/
	int oWid,oHt;/*Width, height of output buffer.*/
	int nxPatch;/*Number of patches in x.*/
	int nLooks;/*Number of radiometric looks in output.*/
	int outSeq;/*Offset between each patch in output space.*/
	double inSeq;/*Offset between each patch in input space.*/
	specan_struct rng;
	specan_struct az;
	float scallop[SPECAN_MAXFFT];/*Anti-scalloping amplitude scale factors.*/
} specan_patch; 


void init_patch(specan_patch *s);
void read_patch(specan_patch *s,getRec *inFile,FCMPLX *in,int patchNo);
void specan_process_patch(specan_patch *s,FCMPLX *in,float *amp_out);
void write_patch(specan_patch *s,float *amp_out,FILE *outFile);


/*Internal SPECAN processing routines (in specan.c)*/
void specan_init(specan_struct *s);/*Computes calculated parameters from filled-in parameters.*/
void specan_process(specan_struct *s,FCMPLX *input,FCMPLX *output);/*Perform SPECAN SAR Processing.*/


