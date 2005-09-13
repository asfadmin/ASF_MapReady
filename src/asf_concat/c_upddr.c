/*******************************************************************************
NAME			      C_UPDDR

PURPOSE	     Updates records 1 and 2 of a DDR file

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replaced newlas.h with las.h
					Replaced cerrmsg with c_errmsg
					Changed calling sequences to the put
					and get ddr calls
					Added the rotated projection space
					subroutine
B. Ailts	      jul. 1988		COMB option can now have more output
					bands than input bands
B. Ailts	      Sept. 1988	Increased the buffer space for the 
					band records of the ddr when multiple
					bands are being combined
B.Ailts		      Jan. 1989		If COMB, all bands of one input image
					are not compared with the corresponding
					bands of another input image,  all the
					input bands of all the input images are
					compared with one another
B.Ailts		      Dec. 1989		Added code to free dynamic allocated 
					buffers
B.Ailts		      Dec. 1990		Updated error messages
T. Logan	       4/95		Removed TAE dependencies (ASF)
					Biggest change is removal of PARBLK

PROJECT       LAS

ALGORITHM 
   Retreive the ouptut DDR band indepenent records

   If no. of input images is equal to one
      Retreive the input DDR band indepenent records
      Copy the fields to the output ddr
   Else
      Retreive the band indepenent records of all the input DDRs
      Compare validity flags and values to see if the values of the
        output DDR will be copied from the input DDR or made invalid
   End if

    Based on the flags set by the above comparisons or the validity
      flag if there was only one input image, copy or recalculate the
      individual fields of the output DDR.
    Write the band indepenent records to the output DDR file

    If cflag is set to COMB
       Read the band depenent data of the input DDRs
       Compare the fields to the correct band of the output DDR
       If the band dependent fields are equal then
	  Copy the input band dependent fields to the output DDR
       Else
	  Invalidate the band depenent fields of the output DDR
    Else
       Read the band depenent data of the input DDRs
       Copy the fields to the correct band of the output DDR
       Write the output DDR to the DDR file
    Endif

    Return

ALGORITHM REFERENCES	none
*******************************************************************************/


#include "las.h"

lasErr FUNCTION c_upddr(const char *host_in[],char *host_out,int *nmrimg,int *window[],
		int bands[][MAXBND+1],int nbands[],
		double *line_inc,double *samp_inc,int *out_nbands,int *cflag)
{ /* c_upddr.c  */

struct DDR *in_ddr;		/* input ddrs			     */
static struct DDR out_ddr;		/* output ddr			     */

float tgbl;			/*  TAE tolerance global		     */

int corn_index;		/*  index to first DDR with valid corner flag*/
int gbl_flag=0;
int corner_flag;		/*  Processing corner validity flag	     */
int i;				/*  loop counter			     */
int in_bands;			/*  total number of input bands		     */
int inc_flag;			/*  Processing increment validity flag	     */
int inc_index;			/*  index to first DDR with valid increm. flag*/
int pdist_flag;		/*  Processing proj. distance validity flag  */
int pdist_index;		/*  index to first DDR with valid increm. flag*/
int ppar_flag;			/*  Processing corner proj. parameter flag   */
int proj_flag;			/*  Processing corner projection flag	     */
int status;			/*  status flag				     */
int zone_flag;			/*  Processing corner zone flag	     	     */


/*  Allocate space for the input DDRs
-------------------------------------*/
in_bands = 0;
for (i = 0; i < *nmrimg; i++)
   in_bands += nbands[i];
in_ddr = (struct DDR *)calloc(*nmrimg,sizeof(struct DDR));
if (in_ddr == NULL)
   {
   c_errmsg("Error allocating dynamic memory","upddr-alloc",NON_FATAL);
   return(E_FAIL);
   }

/*  Retrieve the DDR validity flag default from the parblk
----------------------------------------------------------*/
/*
val = p_find(parblk,"$DDRVFLG");
strcpy (vflag,SVAL(*val,0));
if (strcmp(vflag,"IN") == 0)
   gbl_flag = INVAL;
else
   gbl_flag = UNKNOW;
*/  /* ASF */

/*  Retrieve the DDR validity flag default from the parblk
----------------------------------------------------------*/
/*
val = p_find(parblk,"$PROJTOL");
tgbl = RVAL(*val,0);
*/  /* ASF */
tgbl = 2000000.0;

/*  Retreive the output ddr information -- this is done so that the 
    information placed in the ddr by eopen_ will not be lost
-------------------------------------------------------------------*/
status = c_getddr(host_out,&out_ddr);
if (status != E_SUCC)
   {
   c_errmsg("Error returned from getddr","upddr-call",NON_FATAL);
   return(E_FAIL);
   }

/*  if only one input image copy needed information from the input ddr
----------------------------------------------------------------------*/
if (*nmrimg == 1)
   {
   /* get input image DDR 
   ----------------------*/
   status = c_getddr(host_in[0],&in_ddr[0]);
   if (status != E_SUCC)
      {
      c_errmsg("Error returned from getddr","upddr-call",NON_FATAL);
      return(E_FAIL);
      }

   /*  Set the validity flags used by set_corners routines
   -------------------------------------------------------*/
   corner_flag = in_ddr[0].valid[DDCCV];
   corn_index= 0;
   proj_flag = in_ddr[0].valid[DDPCV];
   pdist_flag = in_ddr[0].valid[DDPDV];
   pdist_index = 0;
   inc_flag = in_ddr[0].valid[DDINCV];
   inc_index = 0;
   zone_flag = in_ddr[0].valid[DDZCV];
   ppar_flag = in_ddr[0].valid[DDPPV];
   set_master(&in_ddr[0],&out_ddr,inc_flag,window[0]);
   
   /*  Copy certain fields from the input DDR to the output DDR
   ------------------------------------------------------------*/
   out_ddr.valid[DDPCV] = in_ddr[0].valid[DDPCV];
   out_ddr.valid[DDZCV] = in_ddr[0].valid[DDZCV];
   out_ddr.valid[DDDCV] = in_ddr[0].valid[DDDCV];
   out_ddr.valid[DDPPV] = in_ddr[0].valid[DDPPV];
   out_ddr.valid[DDPUV] = in_ddr[0].valid[DDPUV];
   out_ddr.proj_code = in_ddr[0].proj_code;
   out_ddr.zone_code = in_ddr[0].zone_code;
   out_ddr.datum_code = in_ddr[0].datum_code;
   strcpy(out_ddr.proj_units,in_ddr[0].proj_units);

   for (i = 0; i < 15; i++)
       out_ddr.proj_coef[i] = in_ddr[0].proj_coef[i];

   }

else  /* multiple input images */
   {
   /*  Read the ddr information for each input ddr
   -----------------------------------------------*/
   for (i = 0; i < *nmrimg; i++)
      {
      status = c_getddr(host_in[i],&in_ddr[i]);
      if (status != E_SUCC)
	 {
         c_errmsg("Error returned from getddr","upddr-call",NON_FATAL);
	 return(E_FAIL);
	 }
      }
   /*  Copy or null the individual fields of the ddr
   -------------------------------------------------*/ 
   set_proj(in_ddr,&out_ddr,&proj_flag,*nmrimg);
   set_datum(in_ddr,&out_ddr,*nmrimg);
   set_zone(in_ddr,&out_ddr,&zone_flag,*nmrimg);
   set_ppar(in_ddr,&out_ddr,&ppar_flag,&proj_flag,*nmrimg);
   set_punit(in_ddr,&out_ddr,*nmrimg);

   /*  Set the flags of the individual fields of the ddr which may need to
       be recalculated.
   -----------------------------------------------------------------------*/
   set_pdis_flag(in_ddr,&pdist_flag,&pdist_index,*nmrimg);
   set_inc_flag(in_ddr,&inc_flag,&inc_index,*nmrimg);
   set_corn_flag(in_ddr,&corner_flag,window,&corn_index,
	  	 pdist_flag,tgbl,pdist_index,gbl_flag,*nmrimg);
   out_ddr.master_line = 1;
   out_ddr.master_sample = 1;
   } /* else multiple input images */

/*  Recalculate the individual fields of the ddr based upon their flags which
    were set above
-----------------------------------------------------------------------------*/
set_increment(inc_flag,&in_ddr[inc_index],&out_ddr,*line_inc,*samp_inc);
set_pdist(pdist_flag,&in_ddr[pdist_index],&out_ddr,*line_inc,*samp_inc);
set_corners(corner_flag,proj_flag,pdist_flag,zone_flag,ppar_flag,
	    &in_ddr[corn_index],&out_ddr,*line_inc,*samp_inc,
            &((*window)[corn_index*4]));


/*  Write the band indepenent information to the output ddr
-----------------------------------------------------------*/
status = c_putddr(host_out,&out_ddr);
if (status != E_SUCC)
   {
   c_errmsg("Error returned from putddr","upddr-call",NON_FATAL);
   return(E_FAIL);
   }

free(in_ddr);
return (E_SUCC);
} /*  c_upddr  */
