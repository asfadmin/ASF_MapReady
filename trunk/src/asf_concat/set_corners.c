/*******************************************************************************
NAME			       SET_CORNERS

PURPOSE	     Calculates the corner fields of the output DDR files

EXTERNAL ASSOCIATIONS
	This routine is called by C_UPDDR routine
	This routine is used in conjunction with SET_CORN_FLAG

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replace newlas.h with las.h
					Update to use current constants
B. Ailts	      Aug. 1988		Replaced rot_corners with c_rotcor
B. Ailts	      Sep. 1988		Updated the subsampling formula
L. Huewe	      Sep. 1988		Updated the subsampling formula
D. Steinwand 	      Apr. 1989		Updated so rotated projection spaces
					would update correctly
					
PROJECT       LAS

ALGORITHM 
   Set the null_flag according to the other processing flags
   If the corn_flag for equals INVAL
      Assign INVAL the the corner validity flag of the output DDR
      Assign a zero the the corner value of the output DDR

   Else 
      If the corn_flag for equals USAME
         Assign UKNOW the the corner validity flag of the output DDR
      Else 
         Assign VALID the the corner validity flag of the output DDR
      Endif
      Set the window flag
      Set the expansion flag
      Set the subsampling flag
      Set the rotation flag
      If the image has been windowed, expanded, or subsampled and the null_flag
      is true
         Assign INVAL the the corner validity flag of the output DDR
         Assign a zero the the corner values of the output DDR
      Else if the image has been windowed, expanded, or subsampled and the 
      null_flag is not true
	 If the image has been subsampled
	    Recalculate the number of lines and samples
         Else if the image has been expanded
	    Recalculate the window specifications
         Else 
            Assign the window specifications to temporary variables
	 Endif

         If the projection plane is rotated from the image plane
            Initialize the arguements and call c_rotcor
         Else 
            Call c_calcor
	 Endif
      Else
         Assign the input DDR corner values to the output DDR corner fields
      Endif
   Endif
   return
  

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"

void FUNCTION set_corners(int corner_flag,int proj_flag,int pdist_flag,int zone_flag,int ppar_flag,
		     const struct DDR in_ddr[],struct DDR *out_ddr,double line_inc,double samp_inc,int window[])

{

double  tnl;			/*  Temporary number of lines		   */
double  tns;			/*  Temporary number of samples		   */
double tsl;			/*  Temporary starting line		   */
double tss;			/*  Temporary starting sample		   */

int  exp_flag;			/*  expansion flag			   */
int  image[8];			/*  coordinates in image space		   */
int  null_flag;		/*  Null corners flag			   */
int  rot_flag;			/*  rotation flag			   */
double rwindow[4];		/*  rotation window specification	   */
int  sub_flag;			/*  subsampling flag			   */
int  wind_flag;		/*  window flag			  	   */

/*  Set Null flag according to the various processing flags
-----------------------------------------------------------*/
null_flag = FALSE;
if ((pdist_flag == INVAL) || (pdist_flag == UDIFF))
   null_flag = TRUE;

if ((proj_flag == INVAL) || (proj_flag == UDIFF))
   null_flag = TRUE;

if ((ppar_flag == INVAL) || (ppar_flag == UDIFF))
   null_flag = TRUE;

if ((in_ddr->proj_code == 1) && 
    (in_ddr->proj_coef[0] == 0) &&
    (in_ddr->proj_coef[1] == 0) && 
    ((zone_flag == INVAL) || (zone_flag == UDIFF)))
   null_flag = TRUE;

if ((in_ddr->proj_code == 2) && 
    ((zone_flag == INVAL) || (zone_flag == UDIFF)))
   null_flag = TRUE;

/*  Invalidate the corner coordinates of corner flag is INVAL
-------------------------------------------------------------*/
if (corner_flag == INVAL)
   {
   out_ddr->upleft[0] = 0;
   out_ddr->upleft[1] = 0;
   out_ddr->upright[0] = 0;
   out_ddr->upright[1] = 0;
   out_ddr->loleft[0] = 0;
   out_ddr->loleft[1] = 0;
   out_ddr->loright[0] = 0;
   out_ddr->loright[1] = 0;
   out_ddr->valid[DDCCV] = INVAL;
   }  /* if INVAL */

else    /* UEQ or VALID for flag, values equal */
   {
   /* set output corner validity flag */
   if (corner_flag == UNKNOW)
      out_ddr->valid[DDCCV] = UNKNOW;
   else
      out_ddr->valid[DDCCV] = VALID;
      
   /* set windowing flag 
   ---------------------*/
   wind_flag = FALSE;
   
   if (window[0] != 1)
      wind_flag = TRUE;
   if (window[1] != 1)
      wind_flag = TRUE;
   if (in_ddr->nl != window[2])
      wind_flag = TRUE;
   if (in_ddr->ns != window[3])
      wind_flag = TRUE;

   /* set subsampling flag 
   -----------------------*/
   sub_flag = FALSE;
   if ((line_inc > 1) || (samp_inc > 1))
      sub_flag = TRUE;

   /* set expand flag 
   ------------------*/
   exp_flag = FALSE;
   if (((line_inc < 1) && (line_inc > 0)) || ((samp_inc < 1) && (samp_inc > 0)))
      exp_flag = TRUE;

   /* Set rotation flag
   --------------------*/
   rot_flag = FALSE;
   if ((in_ddr->upleft[0] != in_ddr->upright[0]) || 
       (in_ddr->upleft[1] != in_ddr->loleft[1]))
      rot_flag = TRUE;

   /* set output corner values 
      If null_flag is TRUE and the image has been windowed, subsampled, 
      or expanded
         Invalididate the corner values 
      else if the image has been windowed, subsampled, or expanded
         Recalculate the corner values
      else
         Copy the input corner values to the output corner values
   --------------------------------------------------------------------*/
   if ((null_flag == TRUE) && ((wind_flag) || (sub_flag) || (exp_flag)))
      { /*  invalidate the output corners */
      out_ddr->upleft[0] = 0;
      out_ddr->upleft[1] = 0;
      out_ddr->upright[0] = 0;
      out_ddr->upright[1] = 0;
      out_ddr->loleft[0] = 0;
      out_ddr->loleft[1] = 0;
      out_ddr->loright[0] = 0;
      out_ddr->loright[1] = 0;
      out_ddr->valid[DDCCV] = INVAL;
      } /* if null_flag and windowed or subsampled */

   else if ((wind_flag) || (sub_flag) || (exp_flag)) 
      {  /* recalculate the output corners  */
      if (sub_flag)
         {
	 tsl = (double) window[0];
	 tss = (double) window[1];
         tnl = window[2] - ((window[2] - 1) % (int)line_inc);
         tns = window[3] - ((window[3] - 1) % (int)samp_inc);
	 }
      else if (exp_flag)
	 {
	 tsl = (window[0] - 1) + ((1 + line_inc) / 2);
	 tss = (window[1] - 1) + ((1 + samp_inc) / 2);
	 tnl = window[2] + (((1 / line_inc) - 1) * line_inc);
	 tns = window[3] + (((1 / samp_inc) - 1) * samp_inc);
	 }
      else
	 {
	 tsl = (double) window[0];
	 tss = (double) window[1];
         tnl = (double) window[2];
         tns = (double) window[3];
	 }

      /*  if the projection plane is rotated from the image plane use the 
	  c_rotcor routine to calculate the output coordinates, otherwise
	  use the c_calcor routine.
      ----------------------------------------------------------------------*/
      if (rot_flag)
	 {
	 image[0] = 1;
	 image[1] = 1;
	 image[2] = in_ddr->nl;
	 image[3] = 1;
	 image[4] = 1;
	 image[5] = in_ddr->ns;
	 image[6] = in_ddr->nl;
	 image[7] = in_ddr->ns;
	 rwindow[0] = (double) tsl;
	 rwindow[1] = (double) tss;
	 rwindow[2] = (double) tnl;
	 rwindow[3] = (double) tns;
         c_rotcor(in_ddr->upleft,image,rwindow,out_ddr->upleft);
	 }
      else
         c_calcor(&tsl,&tss,&tnl,&tns,in_ddr,out_ddr->upleft,out_ddr->upright,
	          out_ddr->loleft,out_ddr->loright);
      }

   else /* copy input to output */
      {
      out_ddr->upleft[0] = in_ddr->upleft[0];
      out_ddr->upleft[1] = in_ddr->upleft[1];
      out_ddr->upright[0] = in_ddr->upright[0];
      out_ddr->upright[1] = in_ddr->upright[1];
      out_ddr->loleft[0] = in_ddr->loleft[0];
      out_ddr->loleft[1] = in_ddr->loleft[1];
      out_ddr->loright[0] = in_ddr->loright[0];
      out_ddr->loright[1] = in_ddr->loright[1];
      } /* copy input to output  */

   } /* else if equal */

return;
}  /* set_corners */
