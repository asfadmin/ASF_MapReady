#include "asf.h"
#include "decoder.h"
#include "ceos.h"
#include "dateUtil.h"

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


/******************************************************************************
 * createMeta_ceos:
 * Create outN.meta file using information from CEOS in the inN.L file.  */
void createMeta_ceos(bin_state *s, struct dataset_sum_rec *dssr, char *inN,
                     char *outN)
{
	meta_parameters *meta=raw_init();
	extern int sprocketFlag;
		
	ceos_init(inN,meta);
	s->lookDir = meta->sar->look_direction;
	
        /* Check for VEXCEL LZP Data-- has odd state vectors
         --------------------------------------------------*/
	if (0==strncmp(dssr->sys_id,"SKY",3))
	{
	/* Correct for wrong time of image start-- image start time
	 * (from DSSR) is *actually* in the center of the image.*/
		double imgLen;/*Half of length of image, in seconds*/
		int i;
		imgLen=ceosLen(inN)/2.0/s->prf;
		printf("   VEXCEL Level-0 CEOS: Shifted by %f seconds...\n",imgLen);
		/*Correct the image start time*/
		meta->state_vectors->second -= imgLen;
		if (meta->state_vectors->second<0) {
			meta->state_vectors->julDay--;
			meta->state_vectors->second+=24*60*60;
		}
		/* Correct the time of the state vectors, which are *relative*
		 * to image start.*/
		for (i=0;i<meta->state_vectors->vector_count;i++)
			meta->state_vectors->vecs[i].time += imgLen;
	/* State vectors are too far apart or too far from image as read --
	 * propagate them*/
		if (!quietflag) {
		  printf("   Updating state vectors...  ");
		  fflush(NULL);
		}
		propagate_state(meta, 3, (s->nLines / s->prf / 2.0) );
		if (!quietflag) printf("Done.\n");
	}
	
	/* Update s-> fields with new state vector
         ----------------------------------------*/
	addStateVector(s,&meta->state_vectors->vecs[0].vec);

	/* Update fields for which we have decoded header info.
	 -----------------------------------------------------*/
	updateMeta(s,meta,NULL,0);

        /* Write out and free the metadata structure
         ------------------------------------------*/
	meta_write(meta,outN);
	if (sprocketFlag) {
		char sprocketName[256];
		create_name(sprocketName,outN,".metadata");
		meta_write_sprocket(sprocketName,meta,dssr);
	}
	meta_free(meta);
}

/******************************************************************************
 * convertMetadata:
 * Creates AISP .in and .fmt files, as well as determining the number of lines
 * in the l0 file, by reading the granule (.gran) file.  */
bin_state *convertMetadata_ceos(char *inN, char *outN, int *nLines,
                                readPulseFunc *readNextPulse)
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
	else {
		printf("Unrecognized satellite '%s'!\n",satName);
		exit(EXIT_FAILURE);
	}
	createMeta_ceos(s,&dssr,inN,outN);
	
/*Write out AISP input parameter files.*/
	writeAISPparams(s,outN,dssr.crt_dopcen[0],dssr.crt_dopcen[1],dssr.crt_dopcen[2]);
	writeAISPformat(s,outN);

	*nLines=s->nLines;
	
	return s;
}
