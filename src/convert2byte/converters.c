/****************************************************************
FUNCTION NAME: multilook

SYNTAX:

PARAMETERS:
    NAME:    TYPE:        PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"
#include "asf_meta.h"

/* PROTOTYPES */
void linear_conversion(FILE *fpin, FILE *fpout, meta_parameters *inMeta,
                       meta_parameters *outMeta);
void multilook(FILE *fpin, FILE *fpout,
               meta_parameters *inMeta, meta_parameters *outMeta,
               int lookLine, int lookSample,
               int stepLine, int stepSample);
int get_lines(FILE *fp,meta_parameters *meta,int line_number,int lines_to_get,
              double *buffer);

#define SQR(X) ((X)*(X))

void linear_conversion(FILE *fpin, FILE *fpout, meta_parameters *inMeta,
                       meta_parameters *outMeta)
{
	int line, sample;
	int percent_complete;
	int num_lines = inMeta->general->line_count;
	int num_samples = inMeta->general->sample_count;
	double slope, offset;
	double *inBuffer = (double *) MALLOC(num_samples*sizeof(double));
	double *outBuffer = (double *) MALLOC(num_samples*sizeof(double));

	/* Factors to convert pixels fo [0..255]
	 * byte = slope * in + offset
	 * 0    = slope * min + offset
	 * 255  = slope * max + offset
	 * Therefore: */
	slope = 255.0 / (inMeta->stats->max - inMeta->stats->min);
	offset = -slope * inMeta->stats->min;

	percent_complete=0;
	for (line=0; line<num_lines; line++) {
		if (!quietflag && (line*100/num_lines==percent_complete)) {
			printf("\rConverting data to byte: %3d%% complete.",
				percent_complete++);
			fflush(NULL);
		}
		get_double_line(fpin, inMeta, line, inBuffer);
		for (sample=0; sample<num_samples; sample++) {
			outBuffer[sample] = slope*inBuffer[sample] + offset;
			if (outMeta->general->data_type == BYTE &&
			    outBuffer[sample] < 0) outBuffer[sample] = 0.0;
			if (outMeta->general->data_type == BYTE &&
			    outBuffer[sample] > 255) outBuffer[sample] = 255.0;
		}
		put_double_line(fpout, outMeta, line, outBuffer);
	}
	if (!quietflag)
		printf("\rConverted data to byte:  100%% complete.\n\n");
	if (!quietflag && logflag)
		fprintf(fLog,"Converted data to byte:  100%% complete.\n\n");

	FREE(inBuffer);
	FREE(outBuffer);
}


void multilook(FILE *fpin, FILE *fpout,
               meta_parameters *inMeta, meta_parameters *outMeta,
               int lookLine, int lookSample,
               int stepLine, int stepSample)
{
    int col;
    int ii, jj;
    int rowOffset;
    int count;
    int line_position;
    int linesRead;
    int lookArea = lookLine * lookSample;
    int outBufferSize = outMeta->general->sample_count;
    int numInLines = inMeta->general->line_count;
    int numInSamples = inMeta->general->sample_count;
    int extraArea = (lookLine-stepLine) * inMeta->general->sample_count;
    int stepArea = stepLine * inMeta->general->sample_count;
    int percent_complete;
    double mult = 128.0 / inMeta->stats->mean;
    double *tempBuffer;
    double *inBuffer;
    double *outBuffer;

/* allocate buffers */
    inBuffer = (double *) MALLOC (numInSamples * lookLine * sizeof(double));
    outBuffer = (double *) MALLOC (outBufferSize*sizeof(double));
    tempBuffer = (double *) MALLOC (outBufferSize*sizeof(double));

/* Multilook & convert to byte */
    percent_complete=0;
    /* First fill the inBuffer with lookLine lines, then we'll boxcar through
     * the data */
    linesRead = get_lines(fpin,inMeta,0,lookLine,inBuffer);
    for (line_position=linesRead; linesRead>0; line_position+=stepLine)
    {
        /* Keep user informed of progress */
        if (!quietflag && (line_position*100/numInLines==percent_complete)) {
	    printf("\rMultilooking data and converting to byte: %3d%% complete.",
	           percent_complete++);
            fflush(NULL);
        }

        /* Zero out temp buffer */
        for (count=0; count<outBufferSize; count++)
            tempBuffer[count]=0.0;

        /* Loop over lines/samples to sum the squares for this kernel */
        rowOffset=0;
	/* Loop through lines in look window*/
        for (ii=0; ii<linesRead; ii++) { 
            count = 0;
	    /* Loop through samples (by stepSample increments) */
            for (jj=0; jj<numInSamples; jj+=stepSample) {
                /*Sum the squares of each sample on this line of the look window*/
                for (col=jj; (col<jj+stepSample)&&(col<numInSamples); col++) {
                    tempBuffer[count] += SQR(inBuffer[rowOffset+col]);
                }
                count++;
            }
            rowOffset += numInSamples;
        }

        /* convert sums of squares to output bytes & write output line*/
        for (count=0; count<outBufferSize; count++)
        {
            outBuffer[count] = sqrt(tempBuffer[count]/lookArea)*mult+0.5;
            if (outBuffer[count] > 255.0)
                outBuffer[count] = 255;
        }
        put_double_line(fpout,outMeta,(line_position-1)/stepLine,outBuffer);

        /* Move lines that won't be 'step'ed over to the front of the buffer
	 * (boxcar-ing) */
        for (ii=stepArea, jj=0; jj<extraArea; ii++,jj++)
            inBuffer[jj]=inBuffer[ii];

        /* Add new lines into the buffer */
        linesRead = get_lines(fpin,inMeta,line_position,stepLine,&inBuffer[jj]);

        /* Adjust linesRead to reflect the correct number of lines in the
	 * inBuffer; if there are no lines left in the inBuffer, we're done,
	 * get outta here! */
        if (linesRead)
	    linesRead += lookLine-stepLine;
        else {
            linesRead = lookLine-stepLine;
            lookLine -= stepLine;
            extraArea = (lookLine-stepLine) * numInSamples;
        }
    }

/* Report that thine request hast been finished. */
    if (!quietflag) {
	printf("\rMultilooked data and converted to byte:   100%% complete.\n\n");
    }
    if (!quietflag && logflag) {
        fprintf(fLog,"Multilooked data and converted to byte:   100%% complete.\n\n");
    }
}


/*******************************************************************************
 * get_lines:
 * Gets a given number of lines from a given place in the file via
 * get_double_line so that the data comes back in correct endian order. */
int get_lines (FILE *fp, meta_parameters *meta, int lineNumber, int lines_to_get,
               double *buffer)
{
    int ii;
    int lineLength = meta->general->sample_count;

    for (ii=0; ii<lines_to_get; ii++) {
        if ((lineNumber+ii) < meta->general->line_count) {
            get_double_line(fp, meta, (lineNumber+ii), &buffer[lineLength*ii]);
        }
        else
            break;
    }
    return ii; /* return the amount of lines read */
}

