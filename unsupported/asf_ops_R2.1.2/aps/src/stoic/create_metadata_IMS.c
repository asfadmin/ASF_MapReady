#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)create_metadata_IMS.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/stoic/SCCS/s.create_metadata_IMS.c"

/*
** Purpose: 
**  <filename>.M   metadata file
**  <filename>.D   data file
** The base <filename> would be the same for both files.  An example of
** the metadata file for the GHA data file, based on my last email message
** would look like the following
** Example
** OBJECT = DAPPS_FILE_METADATA
** 
**     OBJECT = COMMON_HEADER
**         TIME = 1996-001T12:30:45.000
**         SOURCE = "FAIF"
**         DESTINATION = "IMS"
**         MSG_TYPE = "DAPPS_FILE_METADATA"
**         NUMBER_OF_RECORDS = 1
**     END_OBJECT = COMMON_HEADER
** 
**     OBJECT = CATALOG_METADATA
**         FILE_NAME = "GHA_DATA.D"
**         FILE_FORMAT = "ASF"
**         SOURCE = "ASF"
**         DESTINATION = "ASF"
**         FA_FILE_TYPE = "ASF_GHA"
**         GEN_FILE_TYPE = "GREENWICH_HOUR_ANGLE"
**         VALID_START_TIME = 1996-001T12:00:00.000
**         VALID_END_TIME = 1996-005T12:00:00.000
**     END_OBJECT = CATALOG_METADATA
** 
** 
** Author:  Lisa Nguyen (section 334)
** Date:    Jan, 1996
** Modify:
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
 
/* #include "dapps_defs.h" */
#define TRUE 1
#define FALSE 0
#include "timeconv.h"


FILE *fpin, *fpout;
#define FIELDTB "%s%s"

void open_files(char *infile, char *outfile);
int wrt_gha_metadata_(char *infile);
int wrt_stoic_metadata_(char *infile);
int aps2ims_archive(char *dataset,char *file_stamp,char *path_name);

/* 
** Function: wrt_gha_metata()
** Description: read the ACS_GHA_FIXES to get the start valid time
**              and last record to end valid time
**              create the gha_metadata when the GHA append to
**              ACS_GHA_FIXES file
** Returns:     int TRUE/FALSE
** Author:	Lisa Nguyen (section 334)
** Date:        Jan, 1996
** Modify:
*/

int wrt_gha_metadata_(char *infile)
{
     int ier;
     int  total_table_map;
     char ASF_value[80];
     char WFF_value[80];

     int  yyyy;
     int  ddd;
     int  hh;
     int  mm;
     int  ss;
     char hhmmss[12];
     char nowtime[30];
     char dp_time[30];
     char time_stamp[30];
     char file_stamp[30];
     char data_file_stamp[30];
     char start_time[30];
     char end_time[30];
     char dataset[80];
     char outfile[256];
     char path_name[256];
     char command_cp[512];

/*   
** Time Stamp
*/
         if(!tc_systime2asf_(nowtime))
             return(FALSE);
         sscanf(nowtime,"%4d:%3d:%2d:%2d:%2d",&yyyy,&ddd,&hh,&mm,&ss);
         sprintf(dp_time,"%.4d-%.3dT%.2d:%.2d:%.2d.000",yyyy,ddd,hh,mm,ss);
         sprintf(nowtime,"%.4d%.3d%.2d%.2d%.2d",yyyy,ddd,hh,mm,ss);
         sprintf(file_stamp,"GHA_FIXES_%s",nowtime);     
         sprintf(path_name,"%s/",getenv("APS_2_IMS"));
         sprintf(outfile,"%s%s.M",path_name,file_stamp);
	/*
        ** copy ACS_GHA_FIXES from latest_gha_fixes to APS_2_IMS directory
        */
         sprintf(data_file_stamp,"GHA_FIXES_%s.D",nowtime);     
         sprintf(command_cp,"/bin/cp %s/ACS_GHA_FIXES %s/%s",
                getenv("latest_gha_fixes"),getenv("APS_2_IMS"),data_file_stamp);
         system(command_cp);

/*
** Open the input and output
*/
     open_files(infile,outfile);

/* 
** Read the ACS_GHA_FIXED
*/


     total_table_map= 0;
     while( fscanf(fpin,FIELDTB, &ASF_value,&WFF_value) != EOF){
           if(total_table_map == 1)
           strcpy(start_time,end_time);
           sscanf(ASF_value,"%4d:%3d:%12s",&yyyy,&ddd,&hhmmss);
           sprintf(end_time,"%.4d-%.3dT%12s",yyyy,ddd,hhmmss);
           total_table_map++;
     }
     if(total_table_map == 1)
     strcpy(start_time,end_time);


         

         fprintf(fpout,"\nOBJECT = DAPPS_FILE_METADATA\n\n");
         fprintf(fpout,"    OBJECT = COMMON_HEADER\n");
         fprintf(fpout,"        TIME = %s\n",dp_time);
         fprintf(fpout,"        SOURCE = \"FAIF\"\n");
         fprintf(fpout,"        DESTINATION = \"IMS\"\n");
         fprintf(fpout,"        MSG_TYPE = \"DAPPS_FILE_METADATA\"\n");
         fprintf(fpout,"        NUMBER_OF_RECORDS = 1\n");
         fprintf(fpout,"    END_OBJECT = COMMON_HEADER\n\n");

         fprintf(fpout,"    OBJECT = CATALOG_METADATA\n");
         fprintf(fpout,"        FILE_NAME = \"%s\"\n",data_file_stamp); 
         fprintf(fpout,"        FILE_FORMAT = \"ASF\"\n");
         fprintf(fpout,"        FILE_CREATION_TIME = %s\n",dp_time);
         fprintf(fpout,"        SOURCE = \"ASF\"\n");
         fprintf(fpout,"        DESTINATION = \"ASF\"\n");
         fprintf(fpout,"        FA_FILE_TYPE = \"ASF_GHA\"\n");
         fprintf(fpout,"        GEN_FILE_TYPE = \"GREENWICH_HOUR_ANGLE\"\n");
         fprintf(fpout,"        VALID_START_TIME = %s\n",start_time); 
         fprintf(fpout,"        VALID_END_TIME = %s\n",end_time); 
         fprintf(fpout,"    END_OBJECT = CATALOG_METADATA\n\n");
         fprintf(fpout,"END_OBJECT = DAPPS_FILE_METADATA\n\n");
         fprintf(fpout,"END\n");

/*
** Close the input and output
*/
     fclose(fpin);
     fclose(fpout);
/* 
** Archive to: 
** 	table = granules_681
**	directory = /ims_repository/FTS_DEV/granules_681
*/
        strcpy(dataset,"ASF GREENWICH HOUR ANGLE");
	ier = aps2ims_archive(dataset,file_stamp,path_name);
     
     return(TRUE);
}


/*
** Purpose: 
**  <filename>.M   metadata file
**  <filename>.D   data file
** The base <filename> would be the same for both files.  An example of
** the metadata file for the GHA data file, based on my last email message
** would look like the following
** Example
** OBJECT = DAPPS_FILE_METADATA
** 
**     OBJECT = COMMON_HEADER
**         TIME = 1996-001T12:30:45.000
**         SOURCE = "FAIF"
**         DESTINATION = "IMS"
**         MSG_TYPE = "DAPPS_FILE_METADATA"
**         NUMBER_OF_RECORDS = 1
**     END_OBJECT = COMMON_HEADER
** 
**     OBJECT = CATALOG_METADATA
**         FILE_NAME = "STOIC_FILE.D"
**         FILE_FORMAT = "ORIGINAL"
**         SOURCE = "JPL"
**         DESTINATION = "ASF"
**         FA_FILE_TYPE = "JPL_STOIC"
**         GEN_FILE_TYPE = "STOIC"
**         VALID_START_TIME = 1996-001T12:00:00.000
**         VALID_END_TIME = 1996-005T12:00:00.000
**     END_OBJECT = CATALOG_METADATA
** 
** 
** Author:  Lisa Nguyen (section 334)
** Date:    Jan, 1996
** Modify:
*/

/* 
** Function: wrt_stoic_metata()
** Description: read the first record of stoicfile to get the start valid time
**              and end valid time
**              everytime the transfer to IMS
** Returns:     int TRUE/FALSE
** Author:	Lisa Nguyen (section 334)
** Date:        Jan, 1996
** Modify:
*/

int wrt_stoic_metadata_(char *infile)
{
     char ASF_value[80];
     char WFF_value[80];

     int  ier;
     int  julian_day;
     int  start_yr;
     int  start_mos;
     int  start_day;

     int  end_yr;
     int  end_mos;
     int  end_day;

     int  yyyy;
     int  ddd;
     int  hh;
     int  mm;
     int ss;
     char nowtime[22];
     char dp_time[22];
     char time_stamp[22];
     char file_stamp[22];
     char data_file_stamp[22];
     char start_valid_time[22];
     char end_valid_time[22];
     char first_record[80];
     char dataset[80];
     char outfile[256];
     char path_name[256];
     char command_cp[512];

     char sourceDir[256];
     char name[80]; 

/*   
** Time Stamp
*/
         if(!tc_systime2asf_(nowtime))
                return(FALSE);
         sscanf(nowtime,"%4d:%3d:%2d:%2d:%2d",&yyyy,&ddd,&hh,&mm,&ss);
         sprintf(dp_time,"%.4d-%.3dT%.2d:%.2d:%.2d.000",yyyy,ddd,hh,mm,ss);
         sprintf(nowtime,"%.4d%.3d%.2d%.2d%.2d",yyyy,ddd,hh,mm,ss);
         sprintf(file_stamp,"STOIC_%s",nowtime);     
         sprintf(path_name,"%s/",getenv("APS_2_IMS"));
         sprintf(outfile,"%s%s.M",path_name,file_stamp);

	/*
        ** copy stoicfile from latest_stoicfile to APS_2_IMS directory
        */
         sprintf(data_file_stamp,"STOIC_%s.D",nowtime);     
         sprintf(command_cp,"/bin/cp %s/stoicfile %s/%s",
                getenv("latest_stoicfile"),getenv("APS_2_IMS"),data_file_stamp);
         system(command_cp);

/*
** Open the input and output
*/
     open_files(infile,outfile);

/* 
** Read the stoicfile 
*/


     fscanf(fpin,FIELDTB, &ASF_value);
     sscanf(ASF_value," TIMPOL='STOIC/KEOF.LD%s",&first_record);
     sscanf(first_record,"%2d%2d%2d/PT%2d%2d%2d",&start_yr,&start_mos,
           &start_day,&end_yr,&end_mos,&end_day);
     /*
     ** Convert the start time to julian day
     */
     if(start_yr > 70)
         start_yr = 1900 + start_yr;
     else 
         start_yr = 2000 + start_yr; 
     if(!tc_cal2doy_(start_yr,start_mos,start_day,&julian_day))
     {
               fclose(fpin);
               fclose(fpout);
               return(FALSE);
     }
     sprintf(start_valid_time,"%.4d-%.3dT00:00:00.000",start_yr, julian_day);
     
     /*
     ** Convert the end time to julian day
     */
     if(end_yr > 70)
         end_yr = 1900 + end_yr;
     else 
         end_yr = 2000 + end_yr; 
     if(!tc_cal2doy_(end_yr,end_mos,end_day,&julian_day))
     {
               fclose(fpin);
               fclose(fpout);
               return(FALSE);
     }
     sprintf(end_valid_time,"%.4d-%.3dT00:00:00.000",end_yr, julian_day);

         fprintf(fpout,"\nOBJECT = DAPPS_FILE_METADATA\n\n");
         fprintf(fpout,"    OBJECT = COMMON_HEADER\n");
         fprintf(fpout,"        TIME = %s\n",dp_time);
         fprintf(fpout,"        SOURCE = \"FAIF\"\n");
         fprintf(fpout,"        DESTINATION = \"IMS\"\n");
         fprintf(fpout,"        MSG_TYPE = \"DAPPS_FILE_METADATA\"\n");
         fprintf(fpout,"        NUMBER_OF_RECORDS = 1\n");
         fprintf(fpout,"    END_OBJECT = COMMON_HEADER\n\n");

         fprintf(fpout,"    OBJECT = CATALOG_METADATA\n");
         fprintf(fpout,"        FILE_NAME = \"%s\"\n",data_file_stamp); 
         fprintf(fpout,"        FILE_FORMAT = \"ORIGINAL\"\n");
         fprintf(fpout,"        FILE_CREATION_TIME = %s\n",dp_time);
         fprintf(fpout,"        SOURCE = \"JPL\"\n");
         fprintf(fpout,"        DESTINATION = \"ASF\"\n");
         fprintf(fpout,"        FA_FILE_TYPE = \"JPL_STOIC\"\n");
         fprintf(fpout,"        GEN_FILE_TYPE = \"STOIC\"\n");
         fprintf(fpout,"        VALID_START_TIME = %s\n",start_valid_time); 
         fprintf(fpout,"        VALID_END_TIME = %s\n",end_valid_time); 
         fprintf(fpout,"    END_OBJECT = CATALOG_METADATA\n\n");
         fprintf(fpout,"END_OBJECT = DAPPS_FILE_METADATA\n\n");
         fprintf(fpout,"END\n");

/*
** Close the input and output
*/
     fclose(fpin);
     fclose(fpout);
/* 
** Archive to: 
** 	table = granules_681
**	directory = /ims_repository/FTS_DEV/granules_681
*/
         strcpy(dataset,"ASF STOIC");
 	ier = aps2ims_archive(dataset,file_stamp,path_name);
/*
** End of subroutine
*/
     return(TRUE);
}

/*
** open_files()
** Function that attempts to open both the input and output disk data files.
** If either fopen() call fails, the program abouts.
**/

void open_files(char *infile, char *outfile)
{
     if ((fpin = fopen(infile,"r")) == NULL){
        printf("Cannot read %s. Abort\n",infile);
        return;
     }
     if ((fpout= fopen(outfile,"w")) == NULL){
        printf("Cannot write %s. Abort\n",outfile);
        return;
     }
}
