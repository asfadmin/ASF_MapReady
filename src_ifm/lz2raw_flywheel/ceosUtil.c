#include "asf.h"
#include "decoder.h"
#include "ceos.h"
#include "asf_endian.h"

/*********************************
openCeos:
	Open the given CEOS image, seek to
the beginning of the file, and return a file pointer
*/
FILE *openCeos(const char *fName)
{
	FILE *ret=FOPEN(fName,"rb");
	FSEEK64(ret,0,0);/*Seek to beginning of file*/
	getNextCeosLine(ret);/*Skip over first line of file*/
	return ret;
}

/*********************************
getNextCeosLine:
	Reads the next entire CEOS record from
the given file.  Returns pointer into static
buffer.
*/
signalType *getNextCeosLine(FILE *f)
{
	static int headerLen=12,totHeaderLen=192;
	static signalType buffer[100000];/*Input Buffer*/
	struct HEADER head;
	int length;
	if (headerLen!=fread(&head,1,headerLen,f))
	{
		printf("Finished with input file!\n");
		StopWatch();
		exit(1);
	}
	length=bigInt32(head.recsiz);
	FREAD(&buffer,1,length-12,f);
	return &buffer[totHeaderLen-headerLen];
}
