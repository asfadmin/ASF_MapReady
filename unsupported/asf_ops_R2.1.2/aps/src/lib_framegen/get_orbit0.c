#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		get_orbit0.c

Description:	

External Functions Defined:
	get_orbit0
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)get_orbit0.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_framegen/SCCS/s.get_orbit0.c"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "framegen.h"


/*==============================================================================
Function:      get_orbit0 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 15:07:23 PDT 1996

Notes:		
==============================================================================*/
int  get_orbit0(char *orbit_fn,char *platform, char *mode,  
                     long revolution,
                     struct orbit0_struct *orbit0)
{
FILE *fptb;
char linein[132];

int  str_hr,str_min, str_sec ;
int  end_hr,end_min, end_sec ;
int  ctr_hr,ctr_min, ctr_sec ;

char t1[30];
int    i, stat;


   if ((fptb = fopen(orbit_fn,"r")) == NULL){
      printf("Cannot open map table %s\n",orbit_fn);
      return(-1);
   }

   while (fgets(linein, 132, fptb) != NULL)
   {
     stat = sscanf(linein,"%s%s%d%d%d%d%s%s",
         orbit0->platform, orbit0->mode, &orbit0->days_cycle,
         &orbit0->rev_cycle, &orbit0->start_rev, &orbit0->end_rev,
         t1,orbit0->frame_mode);
     if (stat == 7)
     {
        fprintf (stderr, "old format orbit0 file - FRAME_MODE -> ARCTIC\n");
        strcpy(orbit0->frame_mode,"ARCTIC");
     }

     stat = odl2fg(t1,&(orbit0->start_time));
      for (i=0; i<FRAME_REV; i++)
      {
         fscanf(fptb,"%d:%d:%d %d:%d:%d %d:%d:%d %f %f %f %f %f %f %f %f %f %f",
            &str_hr,&str_min,&str_sec, 
            &end_hr,&end_min,&end_sec,
            &ctr_hr,&ctr_min,&ctr_sec,

            &(orbit0->frame[i].lat[NEAR_START]),
            &(orbit0->frame[i].lon[NEAR_START]),
   
            &(orbit0->frame[i].lat[NEAR_END])  ,
            &(orbit0->frame[i].lon[NEAR_END]) ,

            &(orbit0->frame[i].lat[FAR_START]) ,
            &(orbit0->frame[i].lon[FAR_START]) ,

            &(orbit0->frame[i].lat[FAR_END])   ,
            &(orbit0->frame[i].lon[FAR_END])   ,

            &(orbit0->frame[i].lat[CENTER])    ,
            &(orbit0->frame[i].lon[CENTER])); 

         orbit0->frame[i].time[START_TM] =
             (double)(str_hr*3600 + str_min*60 + str_sec);
         orbit0->frame[i].time[END_TM]   =
             (double)(end_hr*3600 + end_min*60 + end_sec);
         orbit0->frame[i].time[CTR_TM]    =
             (double)(ctr_hr*3600 + ctr_min*60 + ctr_sec);

         orbit0->frame[i].ascdec = (i >225 && i < 675) ? 'D' : 'A';
      }
     if(strcmp(orbit0->platform,platform) == 0  &&
        strcmp(orbit0->mode, mode)== 0 && 
        revolution >=  orbit0->start_rev &&
        revolution <=  orbit0->end_rev)
     { 
      fclose(fptb);
      return(0);
     }
   }
   fclose(fptb);
   return(-1);
}

