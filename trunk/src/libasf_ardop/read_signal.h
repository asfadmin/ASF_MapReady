/* Read_signal.h:
The external interface to the SAR signal-data reading routines 
in read_signal.c.
*/
#ifndef __read_signalh
#define __read_signalh

/*A structure to store parameters for each line of signal data*/
typedef struct {
	int shiftBy;/*Amount by which to shift this line (Window shift)*/
	float scaleBy;/*Amount by which to multiply this line (AGC)*/
} signalLineRec;

typedef struct {
/*Public fields:*/
	long long nLines,nSamples;/*Number of lines and samples in signal data.*/
	int lineSize,sampleSize;/*Number of bytes in a line and a sample.*/
	
/*Private fields:*/
	FILE *fp_in;
	int header;/*Number of bytes in header (file header + line header)*/
	unsigned char *inputArr;/*Input array, size=sampleSize*nSamples;*/
	float dcOffsetI,dcOffsetQ;/*Average values for I and Q.*/
	char flipIQ;/*'n'-- do not flip I and Q values. 'y'-- flip I and Q values.*/
	signalLineRec *lines;/*Information about each line of data (can be NULL)*/
} getRec;

/*For fetching SAR echo data:*/
getRec * fillOutGetRec(char file[]);
void getSignalLine(const getRec *r,long long lineNo,complexFloat *destArr,int readStart,int readLen);
void freeGetRec(getRec *r);

/*For fetching the range pulse replica (range reference function).*/
void fetchReferenceFunction(char *fname,complexFloat *ref,int refLen);

#endif
