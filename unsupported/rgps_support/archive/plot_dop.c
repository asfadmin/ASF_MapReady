#include "caplib.h"
#include <math.h>
#include "ceos.h"
#include "geolocate.h"

void writeDopplerDelta(char *ceosFile, double dopplerOffsetPRF);
void correctDop(char *fname,double offsetPRF)
{
	system("mkdir prm > /dev/null 2>&1\n");
	writeDopplerDelta(fname,offsetPRF);	
}

char *substr(char *input,int start,int num)
{
	int i;
	char *str=(char *)malloc((num+1)*sizeof(char));
	for (i=0;i<num;i++)
		str[i]=input[start+i];
	str[num]=0;
	return str;
}

void processImage_viaDop(char *ceos, FILE *imgInfo)
{
	stateVector stVec;
	double yaw,sr0;
	double prf,estDoppler,ceosDoppler;
	struct dataset_sum_rec dssr;
	struct VFDRECV facdr;

/*Fetch CEOS records.*/
	get_facdr(ceos,&facdr);
	get_dssr(ceos,&dssr);
	
/*Extract parameters from CEOS.*/
	yaw=0.05;
	sr0=facdr.sltrngfp*1000.0;
	prf=dssr.prf;
	ceosDoppler=dssr.crt_dopcen[0]/prf;
	
	printf("Image %s\n",ceos,yaw);
	get_ceos_stVec(ceos,0.0,&stVec);
	
	estDoppler=yaw2doppler(&stVec,sr0,yaw,'L')/prf;/*<-- for RAMP, satellite is left-looking.*/

#if 0
/*Estimage doppler based on yaw angle.*/
	#define MIN_YAW -0.02
	#define MAX_YAW 0.14
	if ((yaw>=MIN_YAW) && (yaw<=MAX_YAW))
		printf("need not be reprocessed.\n");
	else if (yaw<MIN_YAW)
		{printf("must be reprocessed at +1 prf.\n");correctDop(ceos,+1.0);}
	else /*if (yaw>MAX_YAW)*/
		{printf("must be reprocessed at -1 prf.\n");correctDop(ceos,-1.0);}
	fprintf(imgInfo,"%s  %s  %6.4f %8.2f\n",
		substr(facdr.dataid,2,5), /*Revolution*/
		substr(facdr.imageid,0,3),/*Frame*/
		yaw, /*Yaw in degrees.*/
		ceosDoppler); /*Doppler in Hz.*/
#endif
	fprintf(imgInfo,"%s  %s  %6.4f %8.2f\n",
		substr(facdr.dataid,2,5), /*Revolution*/
		substr(facdr.imageid,0,3),/*Frame*/
		estDoppler, /*Estimated Doppler in Hz.*/
		ceosDoppler); /*CEOS Doppler in Hz.*/
}

int main(int argc,char *argv[])
{
	int i;
	FILE *imgInfo;
	if (argc<2)
		{printf("Usage: rgpsDop <files...>\nDetermines which frames have bad dopplers.\n");exit(1);}
#define imgInfoName "image_info"
	imgInfo=fopen(imgInfoName,"w");
	for (i=1;i<argc;i++)
		processImage_viaDop(argv[i],imgInfo);
	if (imgInfo)
	{
		fclose(imgInfo);
		printf("Trend: Doppler (vertical) vs. Frame # (horizontal)\n");
		system("cat "imgInfoName" | plotcol 2 3\n");
		printf("Trend: Doppler (vertical) vs. Expected Doppler(horizontal)\n");
		system("cat "imgInfoName" | plotcol 4 3\n");
	}
	return 0;
}
