/*************************************************
ASF-STEP Level zero utilites:

ceos2raw:
	Converts a Level-0 CEOS Signal product into
a raw input file, suitable for use with AISP.
Switches to work with ERS, JERS, or RADARSAT.

0.8  Orion Lawlor, 8/3/98
1.0  O. Lawlor, 3/99
1.1  Dave Koster, 6/00 Modified to read files larger than 2GB
*/
#include "asf.h"
#include "decoder.h"
#include "ceos.h"
#include "dateUtil.h"

#define VERSION 1.1

/*********************************
createMeta:
	Create outN.meta file using information
from CEOS in the inN.L file.
*/
/*Return the length (in lines) of the given CEOS image*/
int ceosLen(char *inN)
{
	struct IOF_VFDR viof;/*Imagery Options File (values)*/
	get_ifiledr(inN,&viof);
	return viof.numofrec;
}
/*Internal Metadata creation routines (asf_ceos.a)*/
meta_parameters *raw_init(void);
void ceos_init(const char *in_fName,meta_parameters *sar);
void propagate_state(meta_parameters *meta,int nStVec);


void createMeta_ceos(bin_state *s,struct dataset_sum_rec *dssr,char *inN,char *outN)
{
	meta_parameters *meta=raw_init();
	
	/* Read look direction, time & state vectors from CEOS file
         ---------------------------------------------------------*/
	if (extExists(inN,".ldr"))
	{/*HACK: link .ldr file over to .L file-- keeps get_facdr happy*/
		char command[256];
		sprintf(command,"ln -s %s %s",appendExt(inN,".ldr"),appendExt(inN,".L"));
		system(command);
		if (findExt(inN)&&(0!=strcmp(findExt(inN),".D")))
		{/*Link .raw (or whatever) over to .D file-- keeps get_iof happy*/
			sprintf(command,"ln -s %s %s",inN,appendExt(inN,".D"));
			system(command);
		}
	}
	ceos_init(inN,meta);
	s->lookDir=meta->geo->lookDir;
	
        /* Check for VEXCEL LZP Data-- has odd state vectors
         --------------------------------------------------*/
	if (0==strncmp(dssr->sys_id,"SKY",3))
	{
	/*Correct for wrong time of image start-- image start time
	(from DSSR) is *actually* in the center of the image.*/
		double imgLen;/*Half of length of image, in seconds*/
		int i;
		imgLen=ceosLen(inN)/2.0/s->prf;
		printf("VEXCEL Level-0 CEOS: Shifted by %f seconds...\n",imgLen);
		/*Correct the image start time*/
		meta->stVec->second-=imgLen;
		if (meta->stVec->second<0)
			{meta->stVec->julDay--;meta->stVec->second+=24*60*60;}
		/*Correct the time of the state vectors, which are *relative* to image start.*/
		for (i=0;i<meta->stVec->num;i++)
			meta->stVec->vecs[i].time+=imgLen;
	/*State vectors are too far apart or too far from image as read-
	   propagate them*/
		propagate_state(meta,3);
	}
	
	/* Update s-> fields with new state vector
         ----------------------------------------*/
	addStateVector(s,&meta->stVec->vecs[0].vec);

        /* Update fields for which we have decoded header info.
         -----------------------------------------------------*/
	updateMeta(s,meta);

        /* Write out and free the metadata structure
         ------------------------------------------*/
	meta_write(meta,outN);
	meta_free(meta);
}

/********************************
convertMetadata:
	Creates AISP .in and .fmt files,
as well as determining the number of lines in the l0 file,
by reading the granule (.gran) file.
*/

bin_state *convertMetadata_ceos(char *inN,char *outN,int *nLines,readPulseFunc *readNextPulse)
{
	bin_state *s;
	struct dataset_sum_rec dssr;
	char *satName;
	
/*Figure out what kind of SAR data we have, and initialize the appropriate decoder.*/
	get_dssr(inN,&dssr);
	satName=dssr.mission_id;
	
	if (0==strncmp(satName,"E",1))
		s=ERS_ceos_decoder_init(inN,outN,readNextPulse);
	else if (0==strncmp(satName,"J",1))
		s=JRS_ceos_decoder_init(inN,outN,readNextPulse);
	else if (0==strncmp(satName,"R",1))
		s=RSAT_ceos_decoder_init(inN,outN,readNextPulse);
	else 
		{printf("Unrecognized satellite '%s'!\n",satName);exit(1);}
	createMeta_ceos(s,&dssr,inN,outN);
	
/*Write out AISP input parameter files.*/
	writeAISPparams(s,outN,dssr.crt_dopcen[0],dssr.crt_dopcen[1],dssr.crt_dopcen[2]);
	writeAISPformat(s,outN);

/*Clean up and leave.*/
	*nLines=1000000;
	
	return s;
}


/*****************************
main:
	Open input file.
	Read each echo pulse
	Write each echo pulse
*/
int main(int argc,char *argv[])
{
	int outLine,nTotal;
	char *inName,*outName;
	FILE *out;
	
	bin_state *s;
	readPulseFunc readNextPulse;
	iqType *iqBuf;
	
/*Parse CLA's.*/
	StartWatch();

	if (argc!=3 && argc!=4)
		{printf("Usage: %s <input> <output>  \n"
		"\n"
		"  This program decodes the input VEXCEL\n"
		"Level-0 CEOS format signal data into AISP-compatible\n"
		"raw data.  Currently supports ERS, JERS, Radarsat \n"
		"Strip mode (all beams) but not Radarsat ScanSAR mode.\n"
		"ASF-STEP Tools, 1998.  Version %.2f\n",argv[0],VERSION);exit(1);}
	inName=argv[1];
	outName=appendExt(argv[2],".raw");

/*First, we read the metadata to determine where window position shifts happen,
as well as the number of lines in the image.*/
	s=convertMetadata_ceos(inName,outName,&nTotal,&readNextPulse);
	iqBuf=(iqType *)MALLOC(sizeof(iqType)*2*(s->nSamp));
	
/*Now we just loop over the output lines, writing as we go.*/
	out=FOPEN(outName,"wb");
	getNextCeosLine(s->binary);/*Skip CEOS header.*/
	s->nLines=0;
	for (outLine=0;outLine<nTotal;outLine++)
	{
	/*Now read and write pulse of data.*/
		readNextPulse(s,iqBuf);
		FWRITE(iqBuf,s->nSamp,2,out);
	/*Write status information to screen.*/
		if (outLine%1000==0)
			printf("Converting line %d\n",outLine);
		s->nLines++;
	}

	return 0;
}
