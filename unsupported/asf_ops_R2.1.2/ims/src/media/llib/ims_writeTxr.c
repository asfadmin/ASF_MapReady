static char *sccs = "@(#)ims_writeTxr.c	5.1  03/17/96";
/********************************************************************
*
*  Name: WriteTXR
*
*  Module Type: Procedure    Language: C
*
*  Purpose:
*    This program writes text record to the volume directory file
*    on magnetic disk.
*
*  Input Parameters:
*
*  Name          Type        Description
*  file_desc     int         File descriptor
*  curr_rec      int         Current record number
*  ord_med       char        <orderid><mediatype>
*  prod_id       char        product id
*  prod_typ      char        product type
*  prod_time     char        product creation time
*  start_vol     int         start vol for this file
*  curr_vol      int         current volume for this file
*  tot_vol       int         total volumes for this file
*  cent_time     char        center time for scene
*  cent_lat      float       center lat for scene
*  cent_lon      float       center lon for scene
*  sat           char        satellite
*  rev           int         orbit number
*
*  Return Status:
*  Name          Type        Description
*  istat         int         Return write status, =-1 if error
*
*  Modification History:
*
*  Date:   17 Jan 1989  8:51:24   Revision:   1.0   Author:   INGRID
*  Date:   30 Aug 1989 14:15:28   Revision:   1.1   Author:   TOM
*  Date:   05 Oct 1989 10:09:48   Revision:   1.2   Author:   INGRID
*  Date:   18 Jul 1990  9:26:46   Revision:   2.0   Author:   DBMAN
*  Date:   16 Oct 1991 14:20:10   Revision:   2.1   Author:   INGRID
*  Date:   08 Apr 1992 19:51:08   Revision:   2.2   Author:   INGRID
*
*********************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <ctype.h>
#include    <sys/types.h>
#include    <rectxr.h>
#include    <unistd.h>
#include    <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_util.h>
#include <ims_qi.h>


int WriteTXR (file_desc, curr_rec, ord_med, prod_id, prod_typ,
            prod_time, start_vol, tot_vol, cent_time, cent_lat,
            cent_lon, sat, rev)
int         file_desc, curr_rec, tot_vol, start_vol, rev;
char        *ord_med, *prod_id, *prod_typ, *prod_time, *cent_time,
            *sat;
double       cent_lat, cent_lon;
{

int              index, istat, cdrom_flag;
static int       nbytes=360;
char             strng2[3], strng5[6], strng6[7], strng7[8];
static char      *med_id   = "MEDIA ID: ";
static char      *location = "MEDIA CREATION: USA NASA ASF ACS ";
static struct rectxr txr = {
          0,18,63,18,18,0,
          'A',' ',
          ' ',' '
          };

void  SwapBytes( unsigned int, unsigned int * );


/*initializations*/
   for (index=0; index<40; index++)
      txr.prodtypspec[index] = ' ';     /*field 9, product type spec*/
   for (index=0; index<60; index++)
      txr.facility[index] = ' ';        /*field 10, location,date,time*/
    for (index=0; index<40; index++)
      txr.phyvolid[index] = ' ';        /*field 11, phy volume id*/
   for (index=0; index<40; index++)
      txr.sceneid[index] = ' ';         /*field 12, scene identificatn*/
   for (index=0; index<40; index++)
      txr.sceneloc[index] = ' ';        /*field 13, scene location*/
   for (index=0; index<20; index++)
      txr.spare1[index] = ' ';          /*field 14, spare*/
   for (index=0; index<104; index++)
      txr.spare2[index] = ' ';          /*field 15, blanks*/
   cdrom_flag = 0;
   if ( strncmp(ord_med,"CD_",3)==0 ) cdrom_flag = 1;

/*setup for the text record*/
   SwapBytes( (unsigned int) curr_rec,(unsigned int *) &txr.recnum);
        /*current rec num*/
   SwapBytes( (unsigned int) 360, (unsigned int *) &txr.reclen);
        /*record length*/
   (void) strncpy( txr.prodtypspec, "PRODUCT: ",9);
   (void) strncpy( txr.phyvolid,med_id,strlen(med_id));
   if (cdrom_flag == 0)                /*product spec,  phy vol id*/
   {
       (void) strncpy(&txr.prodtypspec[9], ord_med, strlen(ord_med)-2);

 (void) strncpy(&txr.phyvolid[strlen(med_id)],ord_med,strlen(ord_med));
       (void) sprintf(strng2,"%02u\0",start_vol);
       (void) strncpy(&txr.phyvolid[strlen(med_id)+strlen(ord_med)],
            strng2,2);
       (void) strncpy(&txr.phyvolid[strlen(med_id)+strlen(ord_med)+2],
            " MEDIA ",7);
       (void) strncpy(&txr.phyvolid[32],strng2,2);
       (void) sprintf(strng2,"%02u\0",tot_vol);
       (void) strncpy(&txr.phyvolid[34]," OF \0",4);
       (void) strncpy(&txr.phyvolid[38],strng2,2);
   }
   else
   {
       (void) strncpy(&txr.prodtypspec[9], ord_med+3,
            strlen(ord_med)-3);
       (void) strncpy(&txr.phyvolid[strlen(med_id)],ord_med+3,
            strlen(ord_med)-3);
       (void) strncpy(&txr.phyvolid[strlen(med_id)+strlen(ord_med)-3],
            " MEDIA ",7);
       (void) strncpy(&txr.phyvolid[32],"01 OF 01",8);
   }
   (void) strncpy(&txr.prodtypspec[21], prod_id, strlen(prod_id));
   (void) strncpy(&txr.prodtypspec[35], prod_typ, strlen(prod_typ));
        /*prod spec*/
   (void) strncpy( txr.facility,location,strlen(location));
   (void) strncpy( &txr.facility[strlen(location)],
                prod_time, strlen(prod_time));      /*location, time*/

    if (rev != 0)
    {
       (void) strncpy( txr.sceneid,"CENTTIME: \0",10);
       (void) strncpy(&txr.sceneid[10],sat,2);
       (void) sprintf(strng5,"%05u\0",rev);
       (void) strncpy(&txr.sceneid[13],strng5,5);
       (void) strncpy(&txr.sceneid[19],cent_time,strlen(cent_time));
            /*scene id*/
       (void) strncpy( txr.sceneloc,"CENTER LOCATION: \0",17);
       if (cent_lat  >=  0.0)   (void) strncpy(&txr.sceneloc[17],"N\0",
            1);
       else     (void) strncpy(&txr.sceneloc[17],"S\0",1);
       (void) sprintf(strng6,"%2.6f\0",cent_lat);
       (void) strncpy(&txr.sceneloc[18],strng6,6);
       (void) strncpy(&txr.sceneloc[24],", E\0",3);
       (void) sprintf(strng7,"%2.7f\0",cent_lon);
       (void) strncpy(&txr.sceneloc[27],strng7,7);         /*scene loc*/
    }
   istat =  write (file_desc, &txr, nbytes);
   if(  istat  <=  0  )  istat = IMS_ERROR;
   else  istat = IMS_OK;
   return(istat);
}   /*  WriteTXR    */
