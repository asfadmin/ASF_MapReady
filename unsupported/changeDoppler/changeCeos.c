#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "caplib.h"
#include "ceos.h"
#include "ddr.h"
#include "math.h"
#include "geolocate.h"

/*** Macro Definitions ***************************************************/
#define         SQR(A)  (double)((A)*(A))  /* Square of a double 	 */
#define         ECC2    (ECC_E*ECC_E)      /* Eccentricity squared       */
#define         RE2     (RE*RE)		   /* Equatorial Radius Squared  */
#define         RP2     (RP*RP)		   /* Polar Radius Squared       */
#define DMAX(A,B)           (((A)>(B)) ? (A) : (B))
#define DMIN(A,B)           (((A)<(B)) ? (A) : (B))

typedef struct {
	int type;
	int len;
	unsigned char *buff;
	void *next;
} ceosRec;

ceosRec *readCeosFile(char *inName)
{
	ceosRec *head=NULL,*tail=NULL;
	FILE 	*fp;
	char  leaderName[256];
	struct  HEADER  bufhdr;
	
	set_era(inName,leaderName,2);
	fp=FOPEN(leaderName, "r");
	
	while (1==fread(&bufhdr, 12, 1, fp))
	{
		int itype,length;
		unsigned char *buff;
   		ceosRec *newRec=(ceosRec *)malloc(sizeof(ceosRec));
		itype = bufhdr.rectyp[1];
 		length = bufhdr.recsiz;
		buff=(unsigned char *)malloc(length);
		*(struct HEADER *)buff=bufhdr;
		FREAD(buff+12, length-12, 1, fp);
		newRec->type=itype;
		newRec->len=length;
		newRec->buff=buff;
		newRec->next=NULL;
		if (tail==NULL)
			head=tail=newRec;
		else
			tail=(ceosRec *)(tail->next=(void *)newRec);
	}	
	FCLOSE(fp);
	return head;
}

void writeCeosFile(char *outName, ceosRec *recList)
{
	FILE *fp=FOPEN(outName,"w");
	while (recList!=NULL)
	{
		FWRITE(recList->buff,recList->len,1,fp);
		recList=(ceosRec *)(recList->next);
	}
	FCLOSE(fp);
}

void mod_dssr(unsigned char *buff);
void mod_mpdr(unsigned char *buff);
void mod_facdr(unsigned char *buff);
void modifyCeosList(ceosRec *recList)
{
	while (recList!=NULL)
	{
		switch(recList->type)
		{
			case 10: /*Data set summary record*/
				mod_dssr(recList->buff);
				break;
			case 20: /*Map projection Data Record.*/
				mod_mpdr(recList->buff);
				break;
			case 210: /*Facility Related Data Record (new).*/
			case 200: /*Facility Related Data Record.*/
				mod_facdr(recList->buff);
				break;
			default:
				break;/*Do nothing.*/	
		}
		recList=(ceosRec *)(recList->next);
	}
}
void find_facdr(ceosRec *recList,struct VFDRECV *facdr)
{
	while (recList!=NULL)
	{
		if ((recList->type==210)||(recList->type==200))
		{
			Code_FACDR(recList->buff,facdr,1,fromASCII);
			return;
		}
		recList=(ceosRec *)(recList->next);
	}
	printf("Couldn't find FACDR!\n");
	exit(1);
}
extern double dopplerChange;

void changeCEOS(char *ceosIn, char *ceosOut)
{
	ceosRec *ceosList;
	ceosList=readCeosFile(ceosIn);
	modifyCeosList(ceosList);
	writeCeosFile(ceosOut,ceosList);
}
void convertL_for_doppler(double lat_d,double lon,double *lat_out,double *lon_out);

void latLonVert(double * lat, double *lon)
{
	convertL_for_doppler(*lat,*lon,lat,lon);
}

void mod_dssr(unsigned char *buff)
{
	struct dataset_sum_rec dssr;
	Code_DSSR(buff,&dssr,1,fromASCII);
	latLonVert(&dssr.pro_lat,&dssr.pro_long);
	dssr.sys_id[5]='+';
	dssr.crt_dopcen[0]+=dopplerChange;
	printf("Dataset summary record changed!\n");
	Code_DSSR(buff,&dssr,1,toASCII);
}
void mod_mpdr(unsigned char *buff)
{
	struct VMPDREC mpdrec;
	Code_MPDR(buff,&mpdrec,fromASCII);
	printf("Map proj. data record: '%s'\n",mpdrec.mpdesig);
	
	Code_MPDR(buff,&mpdrec,toASCII);
}
void mod_facdr(unsigned char *buff)
{
	struct VFDRECV facdr;
	Code_FACDR(buff,&facdr,1,fromASCII);
	latLonVert(&facdr.imgclat,&facdr.imgclon);
	latLonVert(&facdr.nearslat,&facdr.nearslon);
	latLonVert(&facdr.nearelat,&facdr.nearelon);
	latLonVert(&facdr.farslat,&facdr.farslon);
	latLonVert(&facdr.farelat,&facdr.farelon);
	facdr.dpplrfrq+=dopplerChange;
	printf("Facility related data record changed!\n");
	Code_FACDR(buff,&facdr,1,toASCII);
}
