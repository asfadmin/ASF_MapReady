/****************************************************************
FUNCTION NAME: radio_fill

SYNTAX:

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------

DESCRIPTION: This function uses the *.ant and *.noise files
to add the noise vector and the calibration parameters a_0, a_1,
and a_2 to the radiometric data record.

RETURN VALUE:0 if all is well, -1 if error opening or reading files

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY: Written by Jeremy Nicoll to support AISP ERS-1,-2 
calibration 4-01-02.

****************************************************************/
#include "ceos.h"
#include "asf.h"
#include "ddr.h"
#include "sarout.h"
#include "asf_meta.h"
#include "ifm.h"
#include "odl.h"

 int radio_fill(struct VRADDR *radiometricDataRecord, const char *inName)
 { 
    /*   radio_fill fills up the radiometric data record, which has structure VRADDR. 
	 inName is the prefix of the input filename of the files with suffixes 
	 .ant and .noise. */
    char *ant_name, *noise_name;    /* full names of files to be read */
    FILE *noisePtr;                 /* Pointer to noise_name's data stream */
    ODL odl;                        /* Pointer to ant_name's ODL stream */
    char errC;                      /* Error value returned by ODLinit */
    int err=-1, tableEntries,index; /* err holds the error flag, tableEntries is read */
				    /* in from the *.noise, index is a counter */
    double *table;                  /* array that receives noise vector */
   
    /* Opens and reads the .ant, then fills radiometricDataRecord.a fields */
    if (extExists(inName,".ant"))   /* Read .ant file if possible */
    {
        ant_name=appendExt(inName,".ant");       
        /* Check to see if we have any trouble opening the antenna pattern file */
	if(ODLinit()!=0) 
        {
	     sprintf(errbuf, "   ERROR: Cannot Initialize ODL Structure, exiting\n");
	     printErr(errbuf);
	}
	odl=ODLparse(ant_name,0,&errC);
	if(errC!=0) 
        {
	     sprintf(errbuf, "   ERROR: Could not Parse the ODL Structure, Possible Invalid antenna pattern file, exiting\n");
	     printErr(errbuf);
	}
	
        /* We opened the antenna pattern file, read in vectors */		
        radiometricDataRecord->a[0]=(double)ODLGetDouble(odl,"CAL_PARAM.DETAILED_METADATA.CALIB_FAC.NOISE_FACT",&err);
       radiometricDataRecord->a[1]=(double)ODLGetDouble(odl,"CAL_PARAM.DETAILED_METADATA.CALIB_FAC.LINEAR_CONV_FACT",&err);
       radiometricDataRecord->a[2]=(double)ODLGetDouble(odl,"CAL_PARAM.DETAILED_METADATA.CALIB_FAC.OFFSET_CONV_FACT",&err);
       
       
    } /* end .ant portion */

    /* Opens and reads the .noise, then fills radiometricDataRecord.noise fields */
    if (extExists(inName,".noise")) /*Read .noise file if possible*/
    {
        noise_name=appendExt(inName,".noise");
        noisePtr=FOPEN(noise_name,"r");
        if(fscanf(noisePtr,"%d", &tableEntries)==EOF) err=-1; 
	table=(double *)MALLOC(sizeof(double)*tableEntries);
        for(index=0;index<tableEntries;index++) 
	{
	    if(fscanf(noisePtr,"%lf",&(table[index]))==EOF) err=-1;
	    radiometricDataRecord->noise[index]=table[index];
        }
	FCLOSE(noisePtr);
    }	
    else err=-1;
    return err;
    /* -1 if there was a problem, 0 otherwise */
 };



















































































































































