/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   Internal-only routine.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include <ctype.h>
#include "meta_init.h"

/*ceos_init:
	Reads SAR structure parameters from CEOS
into existing meta_parameters structure.  Calls
the facility-specific decoders below.
*/
void ceos_init(const char *in_fName,meta_parameters *sar)
{
	char fName[255],fac[50],sys[50],ver[50];
	ceos_description *ceos;

        /* Fetch the Data Set Summary Record, which is always present.
         ------------------------------------------------------------*/
	struct dataset_sum_rec dssr;
	strcpy(fName,in_fName);
	get_dssr(fName,&dssr);

        /* Extract everything we possibly can from the DSSR.
         --------------------------------------------------*/
	dssr.rng_gate*=get_units(dssr.rng_gate,EXPECTED_RANGEGATE);
	sar->geo->slantFirst=dssr.rng_gate*speedOfLight/2.0;
	
	dssr.wave_length*=get_units(dssr.wave_length,EXPECTED_WAVELEN);
	sar->geo->wavelen=dssr.wave_length;
	
	if (dssr.clock_ang>=0.0)
		sar->geo->lookDir='R';
	else
		sar->geo->lookDir='L';
	
	dssr.rng_samp_rate*=get_units(dssr.rng_samp_rate,EXPECTED_FS);
	sar->geo->rngPixTime=dssr.n_rnglok/dssr.rng_samp_rate;
	sar->geo->xPix=dssr.pixel_spacing;
	sar->geo->azPixTime=dssr.n_azilok/dssr.prf;
	sar->geo->yPix=dssr.line_spacing;
	sar->geo->dopRange[0]=dssr.crt_dopcen[0];
	sar->geo->dopRange[1]=dssr.crt_dopcen[1];
	sar->geo->dopRange[2]=dssr.crt_dopcen[2];
	sar->geo->dopAz[0]=dssr.alt_dopcen[0];
	sar->geo->dopAz[1]=dssr.alt_dopcen[1];
	sar->geo->dopAz[2]=dssr.alt_dopcen[2];
	
        /* Read information about the sensor/processor/version.
         -----------------------------------------------------*/
	sar->info = (extra_info *) MALLOC (sizeof(extra_info));

	strcpy(sar->info->sensor,dssr.mission_id);
	strtok(sar->info->sensor," ");/*Remove spaces from field.*/

	strcpy(fac,dssr.fac_id);strtok(fac," ");/*Remove spaces from field*/
	strcpy(sys,dssr.sys_id);strtok(sys," ");/*Remove spaces from field*/
	strcpy(ver,dssr.ver_id);strtok(ver," ");/*Remove spaces from field*/
	sprintf(sar->info->processor,"%s/%s/%s",fac,sys,ver);

        sar->info->orbit = atoi(dssr.revolution);
        strcpy(sar->info->satBinTime,dssr.sat_bintim);
        strcpy(sar->info->satClkTime,dssr.sat_clktim);
        strcpy(sar->info->mode,dssr.beam1);
        sar->info->bitErrorRate = 0.0;
	sar->info->prf = dssr.prf;

        /* If we can find the processed data histogram record, we can
           get some information about image size from there.  This
           info may later get overwritten with information from the
           facdr, if its available.  */
	{
	  struct data_hist_rec pdhr;
	  strcpy(fName,in_fName);
	  if ( get_dhr(fName, &pdhr) != -1 ) {
	    sar->ifm->orig_nLines = (pdhr.data)->ns_lin;
	    sar->ifm->orig_nSamples = (pdhr.data)->ns_pix;
	  }
	}

        /* Figure out which facility this data comes from.
         -----------------------------------------------*/
	ceos=get_ceos_description(fName);

        /* Fetch state vectors.
         ---------------------*/
	ceos_init_stVec(fName,ceos,sar);

        /* Decode facility-dependant values.
         ---------------------------------*/
	if (ceos->facility==ASF)
		ceos_init_asf(fName,ceos,sar);

	/* Set the number of looks correctly
	 ----------------------------------*/
	if (ceos->satellite==ERS) sar->ifm->nLooks = 5;
	else if (ceos->satellite==JERS) sar->ifm->nLooks = 3;
	else if (ceos->satellite==RSAT)
	 {
		double looks;
		double look = meta_look(sar,0,sar->ifm->orig_nSamples/2);
		looks = ((sar->geo->xPix/sin(look))/sar->geo->yPix) + 0.5;
		sar->ifm->nLooks = (int) looks;

		if (0==strncmp(sar->info->mode,"FN",2)) sar->ifm->nLooks = 1;
	 }
}

/*get_ceos_description:
Extract a ceos_description structure from given CEOS file.
This contains "meta-meta-"data, data about the CEOS, such
as the generating facility, a decoded product type, etc.*/
ceos_description *get_ceos_description(char *fName)
{
	char *versPtr,*satStr;
	char *prodStr,*procStr;
	ceos_description *ceos=(ceos_description *)MALLOC(sizeof(ceos_description));
/*Fetch DSSR*/
	get_dssr(fName,&ceos->dssr);
	
/*Determine the sensor.*/
	satStr=ceos->dssr.mission_id;
	if (0==strncmp(satStr,"E",1)) ceos->satellite=ERS;
	else if (0==strncmp(satStr,"J",1)) ceos->satellite=JERS;
	else if (0==strncmp(satStr,"R",1)) ceos->satellite=RSAT;
	else {
		printf("Get_ceos_description Warning! Unknown sensor '%s'!\n",satStr);
		ceos->satellite=unknownSatellite;
	}
	
/*Determine the processor version.*/
	ceos->version=0.0;/*Default is zero.*/
	versPtr=ceos->dssr.ver_id;
	while (!isdigit(*versPtr)) versPtr++;
	sscanf(versPtr,"%lf",&ceos->version);

/*Set other fields to unknown (to be filled out by facility-specific
	init. routines)*/
	procStr=ceos->dssr.sys_id;
	prodStr=ceos->dssr.product_type;
	ceos->processor=unknownProcessor;
	ceos->product=unknownProduct;
	
/*Determine the facility that processed the data.*/
	if (0==strncmp(ceos->dssr.fac_id,"ASF",3))
	{/*Alaska SAR Facility Image*/
/*Determine the image type and processor ID.*/
		ceos->facility=ASF;
		if (0==strncmp(procStr,"ASP",3)) ceos->processor=ASP;
		else if (0==strncmp(procStr,"SPS",3)) ceos->processor=SPS;
		else if (0==strncmp(procStr,"PREC",3)) ceos->processor=PREC;
		else if (0==strncmp(procStr,"AISP",4)) ceos->processor=AISP;
		else if (0==strncmp(procStr,"PP",2)) ceos->processor=PP;
		else if (0==strncmp(procStr,"SP2",3)) ceos->processor=SP2;
		else if (0==strncmp(procStr,"AMM",3)) ceos->processor=AMM;
		else if ( 0==strncmp(procStr,"SKY",3) 
			  || 0==strncmp(procStr, "FOCUS", 5) ) {
		  /*Is VEXCEL level-0 processor, not ASF*/
			ceos->facility=VEXCEL;
			ceos->processor=FOCUS;
			ceos->product=CCSD;
			return ceos;
		} else {
			printf("Get_ceos_description Warning! Unknown ASF processor '%s'!\n",procStr);
			ceos->processor=unknownProcessor;
		}
	
		if (0==strncmp(prodStr,"LOW",3)) ceos->product=LOW_REZ;
		else if (0==strncmp(prodStr,"FUL",3)) ceos->product=HI_REZ;
		else if (0==strncmp(prodStr,"SCANSAR",7)) ceos->product=SCANSAR;
		else if (0==strncmp(prodStr,"CCSD",4)) ceos->product=CCSD;
		else if (0==strncmp(prodStr,"COMPLEX",7)) ceos->product=CCSD;
		else {
			printf("Get_ceos_description Warning! Unknown ASF product type '%s'!\n",prodStr);
			ceos->product=unknownProduct;
		}
		
	}
	else if (0==strncmp(ceos->dssr.fac_id,"ES",2))
	{/*European Space Agency Image*/
		ceos->facility=ESA;
		
		if (0==strncmp(prodStr,"SAR RAW SIGNAL",14)) ceos->product=CCSD;
		else {
			printf("Get_ceos_description Warning! Unknown ESA product type '%s'!\n",prodStr);
			ceos->product=unknownProduct;
		}
	} else
	{
		printf("*************************************\n"
			"SEVERE WARNING!!!!  Unknown CEOS Facility '%s'!\n"
			"****************************************\n",
			ceos->dssr.fac_id);
		ceos->facility=unknownFacility;
	}
	
	return ceos;
}

