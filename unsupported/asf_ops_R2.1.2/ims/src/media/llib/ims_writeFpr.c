static char *sccs = "@(#)ims_writeFpr.c	5.2  03/10/97";
/********************************************************************
*
*  Name: WriteFPR
*
*  Module Type: Procedure    Language: C
*
*  Purpose:
*    This program writes file pointer record to a mag file
*    <order#><media_group #>.vdf<seq#>.
*
*  Input Parameters:
*
*  Name          Type        Description
*  file_desc     int         File descriptor
*  prod_id       chr*16      Product ID
*  curr_rec      int         Current rec number in vol dir file
*  class_cd      chr*4       File class code
*  file_num      int         FIle number in logical volume
*  num_recs      int         Number of rec in ref file
*  rec_fst       int         Rec len of first rec in file
*  rec-max       int         Maximum rec len of ref file
*  phy_vol1      int         Physical vol containing 1st rec
*  phy_vol2      int         Physical vol containing last rec
*  rec_beg       int         Record number of 1st rec in phy vol
*  rec_end       int         Record number of last rec in physical vol
*  ord_line      int         Order line number
*  media_id      chr*16      Media ID
*  proc_cd       chr*3       Process Code, SPS,OPS,GPI
*  rec_lentp     chr*4       Record length type
*
*  Return Status:
*  Name          Type        Description
*  istat         int         Function status, =-1, i/o error
*                                             =-2, wrong inut rec code
*  Modification History:
*
*  Date:   17 Jan 1989  8:51:20   Revision:   1.0   Author:   INGRID
*  Date:   30 Aug 1989 14:15:22   Revision:   1.1   Author:   TOM
*  Date:   05 Oct 1989 10:09:42   Revision:   1.2   Author:   INGRID
*  Date:   17 Oct 1989 16:57:10   Revision:   1.3   Author:   INGRID
*  Date:   18 Jul 1990  9:25:04   Revision:   2.0   Author:   DBMAN
*
******************************************************************** */

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <sys/types.h>
#include    <recfpr.h>
#include    <unistd.h>
#include    <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_util.h>
#include <ims_qi.h>


int  WriteFPR (file_desc, prod_id, curr_rec, class_cd, file_num,
                        num_recs, rec_fst, rec_max, curr_vol, phy_vol1,
                        phy_vol2, rec_beg, rec_end, ord_line, media_id,
                        proc_cd, rec_lentp)

int                    file_desc,file_num,num_recs,rec_fst,rec_max,
                       rec_beg,rec_end,curr_rec;
int                    phy_vol1,phy_vol2,curr_vol,ord_line;
char                   *prod_id, *class_cd, *media_id, *proc_cd,
                        *rec_lentp;
{

int                    index, istat;
int                    nbytes=360;
char                   sar_leadr[]  = "SARLEADER FILE",
                       sar_data[]   = "IMAGERY OPTIONS FILE",
                       sar_tralr[]  = "SARTRAILER FILE",
                       mix_binary[] = "MIXED BINARY AND ASCII",
                       m_binary[]   = "MBAA",
                       fix_rectyp[] = "FIXED LENGTH",
                       fixcd[]      = "FIXD",
                       var_rectyp[] = "VARIABLE LEN",
                       varcd[]      = "VARE";
char                   strng2[5],strng8[12];
char                   str[129];
int                    i;

struct recfpr fpr = {
    0,219,192,18,18,0,
    'A',' ',' ',' ',                         /*field 7*/
    ' ',' ',' ',' ',
    ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
    ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
    ' ',' ', ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
    ' ',' ',' ',' ',                         /*field 12*/
    ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
     ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
    ' ',' ',' ',' ',                         /*field 14*/
    ' ',' ',' ',' ',' ',' ',' ',' ',
    ' ',' ',' ',' ',' ',' ',' ',' ',
    ' ',' ',' ',' ',' ',' ',' ',' ',
    ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ', /*field 18*/
    ' ',' ',' ',' ',
    ' ',' ',
    ' ',' ',                                 /*field 21*/
    ' ',' ',' ',' ',' ',' ',' ',' ',
    ' ',' ',' ',' ',' ',' ',' ',' '
    };

void  SwapBytes( unsigned int, unsigned int * );


/*initializations*/

    SwapBytes( (unsigned int) curr_rec,(unsigned int *)&fpr.recnum);
        /*rec seq num*/
    SwapBytes( (unsigned int)360,(unsigned int *)&fpr.reclen);
        /*rec length*/
    (void) sprintf( strng8,"%4u",file_num); /*file num in logi vol*/
    for (index=0; index<4; index++)
        fpr.fnumb[index] = strng8[index];
                                                /*product id*/
    (void) strncpy( fpr.fid, prod_id, strlen(prod_id));
                                                /*ref file class code*/
    (void) strncpy( fpr.fclasscd, class_cd, strlen(class_cd));
    (void) sprintf( strng8,"%8u", num_recs);       /*nrec in ref file*/
    (void) strncpy( fpr.fnumrec,strng8,8);
    (void) sprintf( strng8,"%8u",rec_fst);   /*recl of 1st rec in ref*/
    (void) strncpy( fpr.frecfst,strng8,8);
    (void) sprintf( strng8,"%8u",rec_max);
    (void) strncpy( fpr.frecmax,strng8,8);     /*max recl = rec len*/
    (void) sprintf( strng2, "%2u", phy_vol1);  /*1st rec phy vol*/
    fpr.phyvolfst[0] = strng2[0];
    fpr.phyvolfst[1] = strng2[1];
    (void) sprintf( strng2, "%2u", phy_vol2);    /*last rec phy vol*/
    fpr.phyvollast[0] = strng2[0];
    fpr.phyvollast[1] = strng2[1];
    if (curr_vol>=phy_vol1 && curr_vol<=phy_vol2)
        (void) sprintf( strng8, "%8u", rec_beg);  /*beg rec in phy vol*/
    else
        (void) sprintf( strng8, "%8u", 0);       /*zero, no rec in vol*/
    (void) strncpy( fpr.recfirst,strng8,8);
    if (curr_vol>=phy_vol1 && curr_vol<=phy_vol2)
        (void) sprintf( strng8, "%8u", rec_end);  /*end rec in phy vol*/
    else
        (void) sprintf( strng8, "%8u", 0);       /*zero, no rec in vol*/
    (void) strncpy( fpr.reclast,strng8,8);
    for (index=0; index<100; index++)
        fpr.spare1[index] = ' ';
    (void) sprintf ( str, "%u", ord_line);    /*acs order line num*/
    i = strlen( str );
    /*
    **  get mod 100 value
    */
    if(  i  ==  1  )  fpr.orderline[0] = ' ';
    else   fpr.orderline[0] = str[i-2];
    fpr.orderline[1] = str[i-1];
                                                /*media id*/
    for (index=0; index<16; index++)
        fpr.mediaid[index] = ' ';
    (void) strncpy( fpr.mediaid, media_id, strlen(media_id));
    (void) strncpy( fpr.proccode,proc_cd,3);
    for (index=0; index<81; index++)            /*blank the rest*/
        fpr.spare2[index] = ' ';


/*setup different types of file pointer record*/
    /*sar leader record*/
    if (class_cd[3]=='l' || class_cd[3]=='L')
        {
        (void) strncpy( fpr.fclass,sar_leadr,14); /*ref file class*/
        (void) strncpy( fpr.fdtype,mix_binary,22);/*ref file data type*/
        (void) strncpy( fpr.fdtcode,m_binary,4);
        (void) strncpy(fpr.frectype,var_rectyp,
             strlen(var_rectyp));           /*ref file rec len type*/
        (void) strncpy(fpr.ftypecode,varcd,strlen(varcd));
                                            /*ref file rec type code*/
        }
    /*sar data file pointer record*/
    else
        {
        if (class_cd[3]=='p' || class_cd[3]=='P')
            {
            (void) strncpy( fpr.fclass,sar_data,20);  /*ref file class*/
            (void) strncpy( fpr.fdtype,mix_binary,22);
                /* ref file data type */
            (void) strncpy( fpr.fdtcode,m_binary,4);
            if (strcmp(rec_lentp,fixcd)==0)
                {
                (void) strncpy(fpr.frectype,fix_rectyp,
                     strlen(fix_rectyp));    /*ref file rec len type*/
                (void) strncpy(fpr.ftypecode,fixcd,strlen(fixcd));
                    /*ref file rec type cd*/
            }
            else
                {
                (void) strncpy(fpr.frectype,var_rectyp,
                     strlen(var_rectyp));    /*ref file rec len type*/
                (void) strncpy(fpr.ftypecode,varcd,strlen(varcd));
                    /*ref file rec type cd*/
            }
        }
        /*sar trailer file pointer record*/
        else{
            if (class_cd[3]=='t' || class_cd[3]=='T'){
                (void) strncpy( fpr.fclass,sar_tralr,15);
                    /*ref file class*/
                (void) strncpy( fpr.fdtype,mix_binary,22);
                    /*ref file data type*/
                (void) strncpy( fpr.fdtcode,m_binary,4);
                (void) strncpy(fpr.frectype,var_rectyp,
                    strlen(var_rectyp));    /*ref file rec len type*/
                (void) strncpy(fpr.ftypecode,varcd,strlen(varcd));
                    /* ref fl rec type code*/
            }
            else{
                istat = -2;
                goto end;
            }
        }
    }
    istat = write (file_desc,  &fpr, nbytes);
    end:;
    if(  istat  >  0  )  istat = IMS_OK;
    else  istat = IMS_ERROR;
      return (istat);
}   /*  WriteFPR    */
