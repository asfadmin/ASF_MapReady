#include "asf.h"
#include "decoder.h"
#include "ceos.h"
#include "asf_endian.h"


void linkFlag(short, char*);
void createMeta_ceos(bin_state *s,struct dataset_sum_rec *dssr,char *inN,char *outN);

/*********************************
openCeos:
	Open the given CEOS image, seek to
the beginning of the file, and return a file pointer
*/
FILE *openCeos(char *fName, char *outN, bin_state *s)
{
	FILE *ret=FOPEN(fName,"rb");
	FSEEK64(ret,0,0);/*Seek to beginning of file*/
	getNextCeosLine(ret, s, fName, outN);/*Skip over first line of file*/
	return ret;
}

/*********************************
getNextCeosLine:
	Reads the next entire CEOS record from
the given file.  Returns pointer into static
buffer.
*/
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
		
		linkFlag(2,NULL);
	        printf("   Wrote %i lines of raw signal data.\n\n",s->nLines);
	        if (logflag) {
	          sprintf(logbuf,"   Wrote %i lines of raw signal data.\n\n",s->nLines);
	          printLog(logbuf);
	          StopWatchLog(fLog);
	        }
		StopWatch();
		exit(0);
	}
	length=bigInt32(head.recsiz);
	FREAD(&buffer,1,length-12,f);
	return &buffer[totHeaderLen-headerLen];
}

/*********************************
linkFlag:
	Used to make & remove .L & .D links to
	 .ldr & .raw  extensions for compatibility
*/
void linkFlag(short link, char *inName)
{
	char command[256];
	static short L=0;
	static short D=0;
	static char file[256];
	
	if (inName != NULL)
	{
		strcpy(file,inName);
	}
	switch (link) {
	 case 0: /*HACK: link .ldr file over to .L file-- keeps get_facdr happy*/
		sprintf(command,"ln -s %s %s",appendExt(file,".ldr"),appendExt(file,".L"));
		system(command);
		L=1;
		break;
	 case 1: /*HACK: link .raw (or whatever) over to .D file-- keeps get_iof happy*/
		sprintf(command,"ln -s %s %s",file,appendExt(file,".D"));
		system(command);
		D=1;
		break;
	 case 2: /* Remove ugly hack */
		if (L)
		{
			sprintf(command,"rm %s",appendExt(file,".L"));
			system(command);
			L=0;
		}
		if (D)
		{
			sprintf(command,"rm %s",appendExt(file,".D"));
			system(command);
			D=0;
		}
		break;
	}		
}
