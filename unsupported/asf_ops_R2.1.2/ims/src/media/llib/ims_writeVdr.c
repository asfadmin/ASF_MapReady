static char *sccs = "@(#)ims_writeVdr.c	5.1  03/17/96";
/********************************************************************
*
*  Name: WriteVDR
*
*  Module Type: Procedure    Language: C
*
*  Purpose:
*    This program writes volume descriptor record to a magnetic
*    file of <order#><media grp#>.vdf<seq#>.
*
*  Input Parameters:
*
*  Name          Type        Description
*  file_desc     int         File descriptor
*  logi_vol      char        Logical volume name
*  sw_ver        char        Software version
*  tot_phy       int         Total physical volume
*  curr_phy      int         Current physical sequence number
*  file_num      int         File number after vol dir file
*  num_fptr      int         number of file pointer records
*  num_txr       int         number of text records
*
*  Return Status:
*  Name          Type        Description
*  istat         int         Return status, =-1, i/o error
*
*  Note: Struct RECVDR  has field 30 according to Mar 89 version of
*        CEOS, it actually is part of the spare in verison Oct 89.
*
*  Modification History:
*
*  Date:   17 Jan 1989  8:51:14   Revision:   1.0   Author:   INGRID
*  Date:   13 Apr 1989 18:45:46   Revision:   1.1   Author:   TOM
*  Date:   30 Aug 1989 14:15:36   Revision:   1.2   Author:   TOM
*  Date:   05 Oct 1989 10:09:34   Revision:   1.3   Author:   INGRID
*  Date:   18 Jul 1990  9:27:26   Revision:   2.0   Author:   DBMAN
*  Date:   16 Oct 1991 14:22:42   Revision:   2.1   Author:   INGRID
*
*********************************************************************/


#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <sys/types.h>
#include    <time.h>
#include    <recvdr.h>
#include    <unistd.h>
#include    <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_util.h>
#include <ims_qi.h>


int WriteVDR (file_desc, logi_vol, sw_ver, tot_phy, curr_phy,
                       file_num, num_fptr, num_txr)
int           file_desc, file_num, num_fptr, num_txr;
char          *logi_vol, *sw_ver;
int           tot_phy, curr_phy;
{

static struct recvdr vdr = {
           0, 192,192,18,18,0,
           'A',' ',' ',' ',
           'C','C','B','-','C','C','T','-','0','0','0','2',/*field 9*/
           ' ','E',
           ' ','B',
           ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
           ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
           ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
           ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
           ' ',' ',' ',' ',' ',' ',' ',' ',
           ' ','1',                                       /*field 17*/
           ' ',' ',
           ' ',' ',
           ' ',' ',' ',' ',
           ' ',' ',' ','1',
           ' ',' ',' ','1',
           ' ',' ',' ',' ',' ',' ',' ',' ',
           ' ',' ',' ',' ',' ',' ','0','0',                /*field 24*/
           'U','S','A',' ',' ',' ',' ',' ',' ',' ',' ',' ',
           'N','A','S','A',' ',' ',' ',' ',
           'A','S','F',' ',' ',' ',' ',' ',' ',' ',' ',' ',
           ' ',' ',' ',' ',
           ' ',' ',' ',' ',
           ' ',' ',' ',' '                                 /*field 30*/
           };
int           index, indexp1, temp, istat, logvol_len;
long           timesec;
static int    nbytes=360;
char          strng4[5], strng2[3], tim_string[28];
char          *tim_ptr;
static char month_ch[12][3] ={'J','a','n', 'F','e','b', 'M','a','r',
                              'A','p','r', 'M','a','y', 'J','u','n',
                              'J','u','l', 'A','u','g', 'S','e','p',
                              'O','c','t', 'N','o','v', 'D','e','c'};

void  SwapBytes( unsigned int, unsigned int * );


/*initializations*/
for (index=0; index<=187; index++)
      vdr.spare[index] = ' ';

/*setup fields*/
   SwapBytes( (unsigned int) 1, (unsigned int *) &vdr.recnum);
        /*rec seq num*/
   SwapBytes( (unsigned int) 360, (unsigned int *) &vdr.reclen);
        /*rec length*/
   (void) strncpy( vdr.software,sw_ver,strlen(sw_ver));/* sw version*/
   logvol_len = strlen(logi_vol);
   (void) strncpy( vdr.tapeid,logi_vol,logvol_len);    /*tape id*/
   (void) sprintf( strng2,"%02u",curr_phy);     /*with leading zero*/
   vdr.tapeid[logvol_len] = strng2[0];
   vdr.tapeid[logvol_len + 1] = strng2[1];
                                                /*logical vol id*/
   (void) strncpy( vdr.logi_vol, logi_vol, strlen(logi_vol)-2);
   for (index=0; index<=15; index++)    /*volume set id = log vol id*/
      vdr.volsetid[index] = vdr.logi_vol[index];
   (void) sprintf( strng2,"%2u",tot_phy);
   vdr.totvol[0] = strng2[0];               /*total # physical*/
   vdr.totvol[1] = strng2[1];
   vdr.phyfirst[0] = ' ';                   /*physical # of 1st tp*/
   vdr.phyfirst[1] = '1';
   vdr.phylast[0] = strng2[0];              /*physical # of last*/
   vdr.phylast[1] = strng2[1];
   (void) sprintf( strng2,"%2u",curr_phy);
   vdr.phycurr[0] = strng2[0];              /*physical # of curr*/
   vdr.phycurr[1] = strng2[1];
   (void) sprintf( strng4,"%4u",file_num);  /*file # after vol dir*/
   (void) strncpy( vdr.fstfile,strng4,4);
   (void) time (&timesec);
   tim_ptr = ctime (&timesec);      /*www mmm dd hh mm ss yyyy*/
   for (index=0; index<=25; index++)
      tim_string[index] = *(tim_ptr + index);
   (void) strncpy( vdr.logvoldate, &tim_string[20],4);      /*YYYY*/
   for (index=0; index<=11; index++)                /*MM*/
      {
      if (tim_string[4]==month_ch[index][0] &&
          tim_string[5]==month_ch[index][1] &&
          tim_string[6]==month_ch[index][2])
         {
          indexp1 = index + 1;
          (void) sprintf( strng2,"%02u",indexp1);
          vdr.logvoldate[4] = strng2[0];
          vdr.logvoldate[5] = strng2[1];
          goto hhmmss;
         }
      }
hhmmss:  vdr.logvoldate[6] = tim_string[8];         /*DD*/
   if (vdr.logvoldate[6]==' ') vdr.logvoldate[6]='0';
   vdr.logvoldate[7] = tim_string[9];
   vdr.logvoltime[0] = tim_string[11];              /*HHMMSS*/
   vdr.logvoltime[1] = tim_string[12];
   vdr.logvoltime[2] = tim_string[14];
   vdr.logvoltime[3] = tim_string[15];
   vdr.logvoltime[4] = tim_string[17];
   vdr.logvoltime[5] = tim_string[18];
   temp = num_fptr + 1 + num_txr;
   (void) sprintf( strng4,"%4u",num_fptr);     /*num of file pntrs*/
   (void) strncpy( vdr.pointers,strng4,4);
   (void) sprintf( strng4,"%4u",temp);          /*nrecs in vol dir*/
   (void) strncpy( vdr.numrecdir,strng4,4);
   (void) strcpy( strng4, "1   " );
        /*  tot, logical vols: only 1 allowed */
   (void) strncpy( vdr.totlogi,strng4,4);
   for (index=0;index<188;index++)
        vdr.spare[index] = ' ';
   istat = write (file_desc,  &vdr, nbytes);
   if(  istat  <=  0  )  istat = IMS_ERROR;
   else  istat = IMS_OK;
   return(istat);
}   /*  WriteVDR    */
