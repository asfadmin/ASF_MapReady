/****************************************************************
FUNCTION NAME: Write_ppf

SYNTAX:	writePPF(ceosFile, ppfFile, dopplerOffsetPRF)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    ceosfile    char[]          Name of input CEOS file leader
    ppfFile     char[]		Name of output ppf file
    doppler..   double		doppler centroid offset in PRFs

DESCRIPTION:
 
  A procedure to create a Processing Parameter File, given
  a CEOS leader file and a doppler offset (determined via 
  doppler trending).  

RETURN VALUE: 	None

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

   1.0 	5/98	O. Lawlor 	Initial Implementation
   1.1  5/98	T. Logan	Hacks .L extension off ppfFile & adds .dop 
   1.2  5/98	O. Lawlor	Second PPF version, adds .prm
   1.3  6/98	O. Lawlor	Writes out mail message, if possible.

****************************************************************/
#include "asf.h"
#include <sys/time.h>
#include "ceos.h"
#include "odl.h"

#define VERSION 1.0

#ifndef LOCAL_PRM
#define LOCAL_PRM 0
#endif

#ifndef STANDALONE
#define STANDALONE 0
#endif

#ifndef MAIL_MESSAGE
#define MAIL_MESSAGE 1
#endif

#if LOCAL_PRM
	#define PPF_DESTINATION_DIRECTORY "prm/"
#else
	#define PPF_DESTINATION_DIRECTORY "/export/sps/proc_params/"
#endif

void writeDopplerDelta(char *ceosFile, double dopplerOffsetPRF);
void writePPF(char *ceosFile, char *ppfFile, double dopplerOffsetPRF);
void writeArr(ODL odl, char *field, double arr[],int nElem,char *unit);

void createPPFname(const char *ceosName,char *ppfName)
{
	int i;
	char ppfBase[200];
	strcpy(ppfBase,ceosName);
/*Find ceos extension (.L)*/
	i=strlen(ppfBase);
        while(i>0 && ppfBase[i]!='.') i--;
        if (i<=0)
        	{printf("The name %s has no extension!\n",ppfBase);return;}
        ppfBase[i]=0;/*trim off the extension.*/
        i-=5;/*Hack off five characters.*/

	/* Replace product type of U with S, otherwise copy over the product type */        
	if (ppfBase[i+1] != 'U') ppfBase[i-1]=ppfBase[i+1];
        else ppfBase[i-1] = 'S';

        ppfBase[i]=0;/*trim off the version, pixel size, and product type.*/
/*Now, we remove the (existing) path name).*/
	while (i>0 && ppfBase[i]!='/') i--;
	if (i>0)
		strcpy(&ppfBase[0],&ppfBase[i+1]);
/*Paste on the new extension.*/
	strcpy(ppfName,PPF_DESTINATION_DIRECTORY);
/*And paste on the new extension*/
	strcat(ppfName,ppfBase);
 	strcat(ppfName,".prm");/*Extension is now ".prm"*/
}

void writeDopplerDelta(char *ceosFile, double dopplerOffsetPRF)
{
	FILE *mail_message=NULL;/*Informative list of changed files/dopplers.*/
	char ppfFile[256];
#if MAIL_MESSAGE
  	struct dataset_sum_rec dssr;
	if (mail_message==NULL)
		mail_message=fopen("mail_message","a");
	get_dssr(ceosFile, &dssr);
	if (mail_message)
 		fprintf(mail_message,"%s\t%s\t%f\n",
 			ceosFile,dssr.beam1,dopplerOffsetPRF);
 	fclose(mail_message);
#endif
 	createPPFname(ceosFile,ppfFile);
	writePPF(ceosFile,ppfFile,dopplerOffsetPRF);
}

#define check(val) chck(__LINE__,(int)(val))
void chck(int line,int ret)
{
	if (ret!=0)
		return;
	printf("ERROR!! Command returned %d on line %d of writePPF.c!\n",ret,line);
	exit(1);
}

/*WritePPF:
	Write a Processing Parameter File in ODL
containing a given doppler offset.*/
void writePPF(char *ceosFile, char *ppfFile, double dopplerOffsetPRF)
{
#define odlHead "PROCESSING_PARAMETERS."
	char *msgTemplate =
"OBJECT = PROCESSING_PARAMETERS\n"
"\n"
"OBJECT = COMMON_HEADER\n"
"        MSG_TYPE = \"PROCESSING_PARAMTERS\"\n"
"        TIME = 1990-001T00:00:01\n"
"        SOURCE = \"STEP_DOPPLER_TOOL\"\n"
"        DESTINATION = \"SPS\"\n"
"        NUMBER_OF_RECORDS = 1\n"
"END_OBJECT = COMMON_HEADER\n"
"PLATFORM = \"RADARSAT-1\"\n"
"REVOLUTION = -1\n"
"FRAME_ID = -1\n"
"PRODUCT_TYPE = \"\"\n"
"NO_BEAMS = 1\n"
"BEAM1 = \"\" \n"
"BEAM2 = \"\" \n"
"BEAM3 = \"\" \n"
"BEAM4 = \"\" \n"
"PRF1 = 0.000 \n"
"PRF2 = 0.000 \n"
"PRF3 = 0.000 \n"
"PRF4 = 0.000 \n"
"ALT_DOPCEN     = ( )\n"
"ALT_DOPCEN_DELTA  = ( )\n"
"CRT_DOPCEN     = ( )\n"
"CRT_DOPCEN_DELTA  = ( )\n"
"ALT_RATE       = ( )\n"
"CRT_RATE       = ( )\n"
"END_OBJECT = PROCESSING_PARAMETERS\n";
	ODL odl;
	char *odlString;
	char err[200]={0};
	char *ext;
	struct timeval tv;
	struct timezone tz;
	
	FILE *ppf;
	double zeroVec[3]={0.0,0.0,0.0};
	double dopDelVec[3]={0.0,0.0,0.0};
	
	struct dataset_sum_rec dssr;
	struct VFDRECV facdr;
	char type[40],revBuf[20],frameBuf[20];
	int rev,frame;
	
	/*Extract Metadata parameters.*/
	get_facdr(ceosFile,&facdr);
	get_dssr(ceosFile, &dssr);
	dopDelVec[0]=dopplerOffsetPRF*dssr.prf1;
	strncpy(revBuf,&facdr.dataid[2],5);revBuf[5]=0;
	strncpy(frameBuf,facdr.imageid,3);frameBuf[3]=0;
	sscanf(revBuf,"%d",&rev);
	sscanf(frameBuf,"%d",&frame);
	
	switch(dssr.product_id[12])
	{
		case 'S':
			strcpy(type,"STANDARD");break;
		case 'Q':
			strcpy(type,"QUICKLOOK");break;
		case 'X':
			strcpy(type,"COMPLEX");break;
		case 'R':
			strcpy(type,"RAMP");break;
		default:
			strcpy(type,"UNKNOWN IMAGE TYPE ");
			type[strlen(type)]=dssr.product_id[12];
			type[strlen(type)]=0;
	}

	/*Open Processing Parameter File Message Template*/
	ODLinit();
	odl=ODLparse(msgTemplate,strlen(msgTemplate),err);
	if (odl==NULL)
		printf("ODL ERROR! %s!\n",err);
	
	/*Set time field*/
	if (gettimeofday(&tv, &tz) != -1 )
		ODLSetVal(odl, odlHead"COMMON_HEADER.TIME", &tv);
		
	/*Set info. fields*/
	check(ODLSetString(odl,odlHead"PLATFORM","RADARSAT-1"));
	check(ODLSetInt(odl,odlHead"REVOLUTION",rev,"Revs"));
	check(ODLSetInt(odl,odlHead"FRAME_ID",frame,"Frames"));
	check(ODLSetString(odl,odlHead"PRODUCT_TYPE",type));
	check(ODLSetInt(odl,odlHead"NO_BEAMS",dssr.no_beams,""));
	check(ODLSetString(odl,odlHead"BEAM1",dssr.beam1));
	check(ODLSetString(odl,odlHead"BEAM2",dssr.beam2));
	check(ODLSetString(odl,odlHead"BEAM3",dssr.beam3));
	check(ODLSetString(odl,odlHead"BEAM4",dssr.beam4));
	check(ODLSetDouble(odl,odlHead"PRF1",dssr.prf1,"Hz"));
	check(ODLSetDouble(odl,odlHead"PRF2",dssr.prf2,"Hz"));
	check(ODLSetDouble(odl,odlHead"PRF3",dssr.prf3,"Hz"));
	check(ODLSetDouble(odl,odlHead"PRF4",dssr.prf4,"Hz"));
	
	/*Set Doppler fields*/
	dssr.alt_dopcen[0]+=dopDelVec[0];
	writeArr(odl,odlHead"ALT_DOPCEN",dssr.alt_dopcen,3,"Hz");
	writeArr(odl,odlHead"ALT_DOPCEN_DELTA",zeroVec,3,"Hz");
	dssr.crt_dopcen[0]+=dopDelVec[0];
	writeArr(odl,odlHead"CRT_DOPCEN",dssr.crt_dopcen,3,"Hz");
	writeArr(odl,odlHead"CRT_DOPCEN_DELTA",dopDelVec,3,"Hz");
	writeArr(odl,odlHead"ALT_RATE",dssr.alt_rate,3,"Hz/sec");
	writeArr(odl,odlHead"CRT_RATE",dssr.crt_rate,3,"Hz/sec");
	
	/*Extract resulting ODL message*/
	odlString=ODLToStr(odl,"PROCESSING_PARAMETERS");
	
	ODLFree(odl);
	
	/*Write message to file*/
	ppf=fopen(ppfFile, "w");
	if (ppf==NULL)
		{printf("Cannot open output file %s!\n Using standard output.\n",ppfFile);ppf=stdout;}
	fprintf(ppf,"%s",odlString);
	if (ppf!=stdout)
		fclose(ppf);
}

/*WriteArr:
	Add each element of the given double array
to the ODL Array in "odl.field". */

void writeArr(ODL odl, char *field, double *dublArr, int nElem,char *unit)
{
	int i;
	ODL label=Find(odl,field,strlen(field));
	ODL arr=Value(label);
	
	for (i=0;i<nElem;i++)
	{
		ODL dubl;
		check(dubl=(ODL)Double(_Init, malloc(100),unit,strlen(unit)));
		check(SetVal(dubl,dublArr[i]));
		check(Elem(arr,dubl));
	}
}

#if STANDALONE
main(int argc,char *argv[])
{
	int argNo=2;
	float dop;
	if (argc<3) 
         {
	   printf("Usage: writePPF <offset> <ceos...>\n");
	   printf("\nASF STEP Tools, Version %.2f\n",VERSION);
	   exit(1);
         }
	sscanf(argv[1],"%f",&dop);
	while (argNo<argc)
		writeDopplerDelta(argv[argNo++],dop);
	return 1;
}
#endif
