/*Get_ceos:contains

getCeosRecord:
	Finds and returns a single record in the given CEOS file.

get_*:
	Finds and decodes CEOS record of the specified type
from the CEOS file.
*/
#include "asf.h"
#include <ctype.h>
#include "ceos.h"
#include "asf_endian.h"

/*Enumeration of the numeric types of various CEOS fields.*/
typedef enum {
	CEOS_ATDR=40,/*Attitude data record.*/
	CEOS_DHR=70, /*Data histograms record.*/
	CEOS_DQSR=60,/*Data quality summary record.*/
	CEOS_DSSR=10,/*Data set summary record.*/
	CEOS_FACDR=210,/*Facility-related data record.*/
	CEOS_FACDR_old=200,/*Pre-1996 Facdr*/
	CEOS_MPDR=20,/*Map-projection data record.*/
	CEOS_PPDR=30,/*Platform position data record.*/
	CEOS_RADDR=50,/*Radiometric data record.*/
	CEOS_RSR=80,/*Range Spectra Record.*/
	CEOS_IFILEDR=192,/*Imagery options file.*/
	CEOS_FDR=300   /*File Descriptor Record*/
} CEOS_RECORD_TYPE;

int getCeosRecord(char *inName,CEOS_RECORD_TYPE recordType,int recordNo,unsigned char **buff)
{
	FILE *fp;
	char leaderName[256];
	struct HEADER  bufhdr;
        int nOccurences=0,era;
	
	if (recordType==CEOS_FACDR) {
		era=set_era(inName,leaderName,2);/*Facility-related data record (in .trl).*/
}
	else if (recordType==CEOS_IFILEDR) {
		era=set_era(inName,leaderName,0);/*Imagery options file (in .D)*/
}
	else {
		if (recordType==CEOS_FDR){
			recordType=CEOS_IFILEDR;/*If looking for FDR record type, set it to IFILEDR type*/
		}
		era=set_era(inName,leaderName,1);/*Else, in .L.*/
	}
	
	fp=fopen(leaderName, "r");
	if (fp==NULL)
	{
		fprintf(stderr,"Can't read SAR leader file '%s'!\n",leaderName);
		exit(1);
	}

	while (1==fread(&bufhdr, 12, 1, fp))
	{
		int itype,length;
		itype = bufhdr.rectyp[1];
		length=bigInt32(bufhdr.recsiz);
		*buff=(unsigned char *)MALLOC(length);
		*(struct HEADER *)*buff=bufhdr;
		fread((*buff)+12, length-12, 1, fp);
		
		if ((itype==recordType)||
			(itype==CEOS_FACDR_old&&recordType==CEOS_FACDR)) 
		{/*We have the correct kind of record.*/
			nOccurences++;
			if (nOccurences==recordNo)
			{ /*This is the correct occurence! Clean up and return.*/
				fclose(fp); 
				return era; 
			}
			else /*Move on.*/
				free(*buff);
		}
		else /*Move on.*/
			free(*buff);
	}
	
	fclose(fp); 

	if ( recordType!=CEOS_MPDR && recordType!=CEOS_DQSR
	     && recordType!=CEOS_DHR ) {
	  fprintf(stderr,
	          "WARNING: * Removal of obsolete code in getCeosRecord may cause your program to\n"
	          "         * fail ungracefully, since it couldn't read SAR leader file record of\n"
		  "         * type %i from SAR leader file named '%s'.\n", 
		  recordType, leaderName); 
	}
	return -1;
}

int get_atdr(char *filename,struct att_data_rec *rec)
{
	unsigned char *buff;
	int era;
	if ( (era = getCeosRecord(filename,CEOS_ATDR,1,&buff)) != -1) {
	  Code_ATDR(buff,rec,fromASCII);
	  free(buff);
	}
	return era;
}

int get_dhr(char *filename,struct data_hist_rec *rec)
{
	unsigned char *buff;
        int era;
	if ((era=getCeosRecord(filename,CEOS_DHR,2,&buff)) != -1)
        {
		Code_DHR(buff,rec,fromASCII); 
	 	free(buff);
	}
	return era;
}

int get_sdhr(char *filename,struct data_hist_rec *rec)
{
	unsigned char *buff;
        int era;
	if ( (era = getCeosRecord(filename,CEOS_DHR,1,&buff)) != -1) {
	  Code_DHR(buff,rec,fromASCII); 
	  free(buff); 
	}
	return era;
}

int get_dqsr(char *filename,struct qual_sum_rec *rec)
{
	unsigned char *buff;
	int era;
	if ((era=getCeosRecord(filename,CEOS_DQSR,1,&buff)) != -1) {
		Code_DQS(buff,rec,era,fromASCII);
		free(buff);
	}
	return era;
}

int get_dssr(char *filename,struct dataset_sum_rec *rec)
{
	unsigned char *buff;
	int era;
	if ( (era = getCeosRecord(filename,CEOS_DSSR,1,&buff)) != -1) {
	  Code_DSSR(buff,rec,era,fromASCII);
	  free(buff);
	}
	return era;
}

int get_facdr(char *filename,struct VFDRECV *rec)
{
	unsigned char *buff;
	int era;
	if ( (era = getCeosRecord(filename,CEOS_FACDR,1,&buff)) != -1) {
	  Code_FACDR(buff,rec,era,fromASCII);
	  free(buff);
	}
	return era;
}

int get_mpdr(char *filename,struct VMPDREC *rec)
{
	unsigned char *buff;
	int era=getCeosRecord(filename,CEOS_MPDR,1,&buff);
	if (era!=-1)
	{
		Code_MPDR(buff,rec,fromASCII);
		free(buff);
	}
	return era;
}

int get_ppdr(char *filename,struct pos_data_rec *rec)
{
	unsigned char *buff;
	int era;
	if ( (era = getCeosRecord(filename,CEOS_PPDR,1,&buff)) != -1) {
	  Code_PPDR(buff,rec,fromASCII);
	  free(buff);
	}
	return era;
}

int get_raddr(char *filename,struct VRADDR *rec)
{
	unsigned char *buff;
	int era;
	if ( (era = getCeosRecord(filename,CEOS_RADDR,1,&buff)) != -1) {
	  Code_RADDR(buff,rec,fromASCII);
	  free(buff);
	}
	return era;
}

int get_rsr(char *filename,struct rng_spec_rec *rec)
{
	unsigned char *buff;
	int era;
	if ( (era = getCeosRecord(filename,CEOS_RSR,1,&buff)) != -1) {
	  Code_RSR(buff,rec,fromASCII);
	  free(buff);
	}
	return era;
}

int get_ifiledr(char *filename,struct IOF_VFDR *vfdr)
{
	unsigned char *buff;
	int era;
	if ( (era = getCeosRecord(filename,CEOS_IFILEDR,1,&buff)) != -1) {
	  Code_IOF((unsigned char *)buff, vfdr,fromASCII);
	  free(buff);
	}
	return era;
}

int get_fdr(char *filename,struct FDR *fdr)
{
	unsigned char *buff;
	int era;
	if ( (era = getCeosRecord(filename,CEOS_FDR,1,&buff)) != -1) {
	  Code_FDR(buff,fdr,fromASCII);
	  free(buff);
	}
	return(era);
}
