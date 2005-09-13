/*******************************************************************************
NAME			      C_PUTDDR

PURPOSE	     Output records 1 and 2 of a DDR file

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
K. Zanter	      Jan. 1988 	Original development
B. Ailts	      Apr. 1988		Replaced newlas.h with las.h
					Changed the calling sequence -- only
					  one structure
					Replaced the ls___ status checks with
					  the constat E_SUCC
					Replaced cerrmsg with c_errmsg
D. Akkerman	      Jan. 1990		Installed code to ensure that
					required parameters are specified.
B. Ailts	      Dec. 1990		Updeated error messages
P. Denny              Jan. 2003         Add metadata interaction for phasing
                                         out of DDRs

PROJECT       LAS

ALGORITHM 
   Ensure that required parameters were specified.
   Create the associated DDR file name from the input host image name.
   Open the DDR file for write access using label services.  
   Write the DDRINT record (first record) 
   Write the DDRDUB record (second record).  
   Close the DDR file.

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"
#include "diskio.h"

/* PROTOTYPE from meta_init.c */
int get_meta_ddr_struct_index(const char *name);

lasErr c_putddr(const char *hname,struct DDR *ddr)
{
    int   access;                   /* file access type                      */
    int   action;                   /* file close action                     */
    int   clen;                     /* length of char part of record         */
    int   dlen;                     /* length of data part of record         */
    FILE *fd;                       /* file descriptor                       */
    char  d_temp[DDSTCT][DDSYLN];   /* temporary for squeezed strings        */
    char *junk_temp,hostddr[1024];
    unsigned char *dbuf;            /* pointer to area where data is stuffed */
    int ii;                         /* Index for corresponding metadata      */

    /* Ensure that required parameters were specified.
    --------------------------------------------------*/
    if ((ddr->nl < 1) || (ddr->nl > MAXNL)) {
      c_errmsg("Invalid number of lines specified","putddr-badnl",NON_FATAL);
      return(E_FAIL);
    }
    if ((ddr->ns < 1) || (ddr->ns > MAXNS)) {
      c_errmsg("Invalid number of samples specified","putddr-badns",NON_FATAL);
      return(E_FAIL);
    }
    if ((ddr->nbands < 1) || (ddr->nbands > MAXBND)) {
      c_errmsg("Invalid number of bands specified","putddr-badbnds",NON_FATAL);
      return(E_FAIL);
    }
    if ((ddr->dtype < 1) || (ddr->dtype > 20)) {
      c_errmsg("Invalid data type specified","putddr-bdtype",NON_FATAL);
      return(E_FAIL);
    }

    strcpy(ddr->system,c_getsys());

    c_lsmknm(hname,".ddr",hostddr);

    access = 1;                          /* open DDR file for write access   */
    c_lsopen(&fd,hostddr,&access);

    /*  Place the string portion of the DDR into a temporary buffer
    ---------------------------------------------------------------*/
    junk_temp = (char *)&ddr->spare;
    strncpy(junk_temp,ddr->system,4);
    strcpy(d_temp[0],squeeze(ddr->system,DDSYLN));
    strcpy(d_temp[1],squeeze(ddr->proj_units,DDPULN));
    strcpy(d_temp[2],squeeze(ddr->last_used_date,DDLDLN));
    strcpy(d_temp[3],squeeze(ddr->last_used_time,DDLTLN));

    clen = DDSTCT * DDSYLN - 1;                /* set up and output record 1 */
    dlen = DISIZE * 4;
    dbuf = (unsigned char *) MALLOC(dlen);
    int2byteArr((int *)ddr,dbuf,DISIZE);
    c_lswrit(&fd,"DDRINT",&clen,&dlen,d_temp[0],dbuf,"I4");
    free(dbuf);

    /* Set up and output record 2
       There is no character part to this record 
    --------------------------------------------*/
    clen = 0;				  
    dlen = DDSIZE * 8;		  
    dbuf = (unsigned char *) &(ddr->proj_coef[0]);
    c_lswrit(&fd,"DDRDUB",&clen,&dlen,d_temp[0],dbuf,"R8");

    action = 0;                                 /* close associated DDR file */
    c_lsclos(&fd,hostddr,&action);

    {/*Create/write correct number of BDRs*/
	    int bandNo;
	    for (bandNo=1;bandNo<=ddr->nbands;bandNo++)
	    {
		    struct BDDR bdr;
		    int_c_intbdr(&bdr);
		    bdr.bandno=bandNo;
		    int_c_putbdr(hname,&bdr);
	    }
    }

    /* Find corresponding meta structure to fill necessary values to
    ----------------------------------------------------------------*/
    ii = get_meta_ddr_struct_index(hname);
    if ((ii>-1) && (ii<NUM_META_DDR_STRUCTS)) {
        if (meta_ddr_structs[ii].ddr != ddr) {
/*            printf("\n"
                   "Warning: Bad programming in the asf_meta library.\n"
                   "         Something that should not have happened in c_putddr() did.\n"
                   "         Please contact Patrick Denny (pdenny@asf.alaska.edu).\n");*/
        }
        if (meta_ddr_structs[ii].ddr == ddr) {
            meta_parameters *mds_meta;
            struct DDR *mds_ddr  = meta_ddr_structs[ii].ddr;
            int open_flag = 0;
            char meta_name[512];

            create_name(meta_name, meta_ddr_structs[ii].base_name, ".meta");

            /* Get the meta structure if its never been gotten (meta=NULL)
             * or its been free'd (meta->general==NULL) */
            if (meta_ddr_structs[ii].meta==NULL
	       || meta_ddr_structs[ii].meta->general==NULL) {
	        if (fileExists(meta_name)) {
                    mds_meta = meta_read(meta_name);
                    open_flag = 1;
		}
		else /* No meta struct/file */ {
		    return(E_SUCC);
		}
            }
            else
                {mds_meta = meta_ddr_structs[ii].meta;}

            /* Fill the meta structure with updated values */
            mds_meta->general->line_count     = mds_ddr->nl;
            mds_meta->general->sample_count   = mds_ddr->ns;
            mds_meta->general->start_line     = mds_ddr->master_line - 1;
            mds_meta->general->start_sample   = mds_ddr->master_sample - 1;
            mds_meta->sar->line_increment     = mds_ddr->line_inc;
            mds_meta->sar->sample_increment   = mds_ddr->sample_inc;
            if (0==strcmp(mds_ddr->system,"ieee-std"))
                    strcpy(mds_meta->general->system,"big_ieee");
            else if (0==strcmp(mds_ddr->system,"ieee-lil"))
                    strcpy(mds_meta->general->system,"lil_ieee");
            else if (0==strcmp(mds_ddr->system,"cray-unicos"))
                    strcpy(mds_meta->general->system,"cray_float");
            else /* "ibm-mvs" or "other-msc" */
                    strcpy(mds_meta->general->system,"???");
            if (mds_meta->sar->image_type=='P')
                    {strcpy(mds_meta->projection->units, mds_ddr->proj_units);}
            switch ( mds_ddr->dtype ) {
              case 0: /* DTYPE_BYTE */
              case DTYPE_BYTE:  mds_meta->general->data_type = BYTE;      break;
              case DTYPE_SHORT: mds_meta->general->data_type = INTEGER16; break;
              case DTYPE_LONG:  mds_meta->general->data_type = INTEGER32; break;
              case DTYPE_FLOAT: mds_meta->general->data_type = REAL32;    break;
              case DTYPE_DOUBLE:mds_meta->general->data_type = REAL64;    break;
              case DTYPE_COMPLEX:
                           mds_meta->general->data_type = COMPLEX_REAL32; break;
              default:
	            printf("WARNING: c_putddr: Unrecognized meta/DDR data type (%d).\n",
		           mds_ddr->dtype);
                    break;
            }
 	    if (open_flag) {
                meta_write(mds_meta, meta_ddr_structs[ii].base_name);
                meta_free(mds_meta);
            }
        }
    }
    return(E_SUCC);
}


