/****************************************************************
FUNCTION NAME:

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "sarout.h"
#include "ddr.h"

void modifyLeader (int mode, int inputCeos, ceosLeader *data, struct DDR *ddr,
		meta_parameters *meta);

void modifyLeader (int mode, int inputCeos, ceosLeader *data, struct DDR *ddr,
		meta_parameters *meta)
 {

   if (mode != CEOS_CCSD)
    {
     strcpy(data->dssr.rngcmp_desg,"SYNTHETIC CHIRP");
     strcpy(data->dssr.fac_id,"ASF-STEP");
    }

   if (mode == CEOS_SIC)
    {
	if (inputCeos) 
          {
	   /* The assumption made here is that when creating a SIC,
              the input CEOS leader file is from an ASF CCSD product
            -------------------------------------------------------*/
            char *strPtr;

  	    strPtr = strrchr(data->dssr.product_id, 'C'); *strPtr = 'X';
  	    strPtr = strrchr(data->dssr.fac_code, 'C'); *strPtr = 'X';
  	    strPtr = strrchr(data->facdr.imageid, 'C'); *strPtr = 'X';
  	    strPtr = strrchr(data->facdr.sitename, 'C'); *strPtr = 'X';

	    data->dssr.iq_ratio = data->dssr.i_bias/ data->dssr.q_bias;
	  }

        strcpy(data->dssr.product_type,"COMPLEX");
        strcpy(data->dssr.algor_id,"RANGE DOPPLER");
	strcpy(data->dssr.azi_weight,"NONE");
	strcpy(data->dssr.rng_weight,"COSINE ON A PEDASTAL");	

        data->dssr.n_azilok = ddr->line_inc;
        data->dssr.n_rnglok = ddr->sample_inc;
        data->facdr.nlooksaz = ddr->line_inc;
        data->facdr.nlooksra = ddr->sample_inc;
        data->dssr.bnd_azilok = data->dssr.bnd_azi / data->dssr.n_azilok;
        data->dssr.bnd_rnglok = data->dssr.bnd_rng / data->dssr.n_rnglok;
	    
    } /* end if mode is CEOS_SIC */
  
  if (mode == CEOS_LOW)
   {
    int  dataSpacing = 0;	

    /* Set data spacing field for square pixel ground range images 
     ------------------------------------------------------------*/	
    if (strncmp(data->facdr.grndslnt,"GROUND",6)==0 &&
        data->facdr.azpixspc == data->facdr.rapixspc)
       {
	 while (pow(2.0,(float)dataSpacing)*6.25 < data->facdr.rapixspc)
		dataSpacing++;
	 if ((pow(2.0,(float)dataSpacing)*6.25) != data->facdr.rapixspc)
		dataSpacing=0;
       }

    if (inputCeos)
     {
       /* No assumption can be made about the origin of the input CEOS 
       -------------------------------------------------------------*/
       char tmpName[256];
	 
       /* Modify name if it is ASF format  
        --------------------------------*/
       strcpy(tmpName,data->dssr.product_id);
       if (strlen(tmpName) == 16 &&
          (isalpha(tmpName[0])&&isdigit(tmpName[1])&&isdigit(tmpName[2])&&
           isdigit(tmpName[3])&&isdigit(tmpName[4])&&isdigit(tmpName[5])&&
           isdigit(tmpName[6])&&isdigit(tmpName[7])&&isdigit(tmpName[8])&&
           isdigit(tmpName[9])&&isalnum(tmpName[10])&&isdigit(tmpName[11])&&
           isalpha(tmpName[12])&&isdigit(tmpName[13])&&isdigit(tmpName[14])&&
           isdigit(tmpName[15])))
         {
	     char tmpChar;

	     /* Set the projection type field
	      -------------------------------*/ 
	     if (meta->geo->type == 'P')
               {
		 if (meta->geo->proj->type == 'A') tmpChar = 'G';
		 else tmpChar = meta->geo->proj->type;
	       }	
	     else tmpChar = meta->geo->type; 

	     tmpName[10] = tmpChar;

	     sprintf(&tmpChar,"%1i", dataSpacing);
	     tmpName[11] = tmpChar;

	     /* Set the processor mode 
	      -----------------------*/
	     tmpName[12] = 'S';

	     strcpy(data->dssr.product_id,tmpName);
	     strcpy(data->facdr.imageid,tmpName);

	 } /* End if ASF format name */

     } /* End if inputCeos */


    /* Set Product Type Field based on mode and pixel spacing
     -------------------------------------------------------*/
    strcpy(data->dssr.product_type,"DETECTED DATA");
    switch (data->dssr.mission_id[0])
     {
	case 'E':
	case 'J':
	    if (dataSpacing == 1) strcpy(data->dssr.product_type,"FULL");
	    else if (dataSpacing == 4)  strcpy(data->dssr.product_type,"LOW");
	    break;
	case 'R':
   	    if ( strncmp(meta->info->mode,"SW",2)==0 ||
   	         strncmp(meta->info->mode,"SN",2)==0)
	      {
	        if (dataSpacing == 3) strcpy(data->dssr.product_type,"FULL");
  	        else if (dataSpacing == 4)  strcpy(data->dssr.product_type,"MED");
  	        else if (dataSpacing == 6)  strcpy(data->dssr.product_type,"LOW");
	      }
	    else if (strncmp(meta->info->mode,"WD",2)==0)
              {
                if (dataSpacing == 2) strcpy(data->dssr.product_type,"FULL");
                else if (dataSpacing == 5)  strcpy(data->dssr.product_type,"LOW");
              } 
	    else if (strncmp(meta->info->mode,"FN",2)==0)
              {
                if (dataSpacing == 0) strcpy(data->dssr.product_type,"FULL");
                else if (dataSpacing == 3)  strcpy(data->dssr.product_type,"LOW");
              } 
	    else
	      {
		if (dataSpacing == 1) strcpy(data->dssr.product_type,"FULL");
	        else if (dataSpacing == 4)  strcpy(data->dssr.product_type,"LOW");
	      }
     }



   } /* End if mode is ceos_low */ 		

  if (mode == CEOS_CCSD)
    {
        strcpy(data->dssr.product_type,"UNPROCESSED SIGNAL DATA");
        strcpy(data->dssr.algor_id,"NONE");
	strcpy(data->dssr.azi_weight,"NONE");
	strcpy(data->dssr.rng_weight,"NONE");	

        data->facdr.nlooksaz = 1.0;
        data->facdr.nlooksra = 1.0;
        data->dssr.n_azilok = 1.0;
        data->dssr.n_rnglok = 1.0;
        data->facdr.weightaz = 0.0;
        data->facdr.weightra = 0.0;

        data->dssr.bnd_azilok = data->dssr.bnd_azi / data->dssr.n_azilok;
        data->dssr.bnd_rnglok = data->dssr.bnd_rng / data->dssr.n_rnglok;

    } 

 }
