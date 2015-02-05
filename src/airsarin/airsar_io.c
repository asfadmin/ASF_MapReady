/****************************************************************
FUNCTION NAME: airsar_io routines

SYNTAX:

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

	J. Badgley	4/01	Initial creation
	T. Logan        6/01    Modified to malloc returns string

****************************************************************/        
#include "asf.h"
#include "airsar_io.h"

void readAirSARLine(FILE *fp,int *dest,int hb,int lb,int y,meta_parameters *meta)
{
    int ns;
    int linelen;
    unsigned char *buf;
    int i;

    int headerBytes = hb;
    int lineBytes = lb;

    ns = meta->general->sample_count;
    linelen = ns * meta->general->data_type;
    buf=(unsigned char*)MALLOC(linelen);

    if(fp == NULL) {
	printf("NULL File Pointer!\n");
	exit(1);
    }

    FSEEK(fp, headerBytes+y*lineBytes, 0);
    ASF_FREAD(buf, 1, linelen, fp);
    if (meta->general->data_type == ASF_BYTE)
	for( i = 0; i < ns; i++ )
		dest[i]=buf[i];

    else if (meta->general->data_type == INTEGER16)
	for( i = 0; i < ns; i++)
		dest[i]=(buf[2*i]<<8)+buf[2*i+1];
    else {
	printf("Attempted to read unsupported data type from image file!\n");
   	fclose(fp);
	exit(1);
    }

   return;

}

char* get_airsar(char* fname, char* Header, char* Record)
{
	FILE* fp;
	char airsar_rec[50];
	int HDR=0, REC=0;
	char c;
	int i;
	int rl;
	static char chOut[256];

	rl=strlen(Record);
	fp=FOPEN(fname, "r");
	if(strncmp(Header,"FIRST",5)==0) HDR=1;

        while(!feof(fp) && !REC){
	  ASF_FREAD(airsar_rec, 1, 50, fp);
	  if(airsar_rec[0]==0){ 
		while(( c = getc(fp) ) == 0){};
	  	ungetc(c,fp);
	  }
          strcpy(chOut, linetail(airsar_rec));

	  if(!HDR)
	    if( !strcmp(chOut, Header) ) HDR=1;
	  if(HDR && !REC) {
            REC = 1;
	    for(i=0; i<rl; i++) {
		if(airsar_rec[i]!=Record[i]){i = rl; REC=0;}
	    }
          }
	}
	FCLOSE(fp);
	return chOut;
}	

char* linetail(char* strIn)
{
	int i=49;
	static char chOut[50];

	while(strIn[i] != ' ') i--;
	strcpy(chOut, &strIn[++i]);

	return chOut;
}


