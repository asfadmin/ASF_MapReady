#include "asf.h"
#include "decoder.h"
#include "ceos.h"
#include "asf_endian.h"


void createMeta_ceos(bin_state *s, struct dataset_sum_rec *dssr, char *inName,
                     char *outName);

/*********************************
 * openCeos:
 * Open the given CEOS image, seek to the beginning of the file, and return a
 * file pointer  */
FILE *openCeos(char *fName, char *outN, bin_state *s)
{
	FILE *ret=FOPEN(fName,"rb");
	FSEEK64(ret,0,0);/*Seek to beginning of file*/
	getNextCeosLine(ret, s, fName, outN);/*Skip over first line of file*/
	return ret;
}

/*********************************
 * getNextCeosLine:
 * Reads the next entire CEOS record from the given file. Returns pointer into
 * static buffer. */
signalType *getNextCeosLine(FILE *f, bin_state *s, char *inN, char *outN)
{
	static int headerLen=12,totHeaderLen=192;
	static signalType buffer[100000];/*Input Buffer*/
	struct HEADER head;
	int length;
        struct dataset_sum_rec dssr;

	if (headerLen!=fread(&head,1,headerLen,f))
	{
		/* create metadata file */
        	get_dssr(inN,&dssr);
       		createMeta_ceos(s,&dssr,inN,outN);
		/* write out AISP input parameter file */
		if (fabs(dssr.crt_dopcen[0])<15000)
        	  writeAISPparams(s,outN,dssr.crt_dopcen[0],dssr.crt_dopcen[1],dssr.crt_dopcen[2]);
		else
        	  writeAISPparams(s,outN,0,0,0);
        	writeAISPformat(s,outN);
		
	        printf("   Wrote %i lines of raw signal data.\n\n",s->nLines);
	        if (logflag) {
	          sprintf(logbuf,"   Wrote %i lines of raw signal data.\n\n",s->nLines);
	          printLog(logbuf);
	          StopWatchLog(fLog);
	        }
		StopWatch();
		exit(EXIT_SUCCESS);
	}
	length=bigInt32(head.recsiz);
	FREAD(&buffer,1,length-12,f);
	return &buffer[totHeaderLen-headerLen];
}
