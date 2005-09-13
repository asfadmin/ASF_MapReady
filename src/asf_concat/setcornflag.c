/*******************************************************************************
NAME			       SET_CORN_FLAG

PURPOSE	     Compares the corner fields of two or more input DDR files

EXTERNAL ASSOCIATIONS
	This routine is called by C_UPDDR routine
	This routine is used in conjunction with SET_CORNERS

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replace newlas.h with las.h
					Changed the calling sequence -- only
					  one structure
					Update to use current constants
					Added a limit on precision of the
					comparisions
B. Ailts	      Oct. 1990		Changed the check for latlon coordinates
					It now treats latlon coordinates that 
					are stored in total seconds as 
					projection coordinates -- The error 
					that will occur in this case should
					be minimal.
B. Ailts	      Dec. 1990		Updated error messages
B. Ailts	      Jan. 1991		Needed more chnages to handle the 
					condition stated in the Oct 1990 update
					
PROJECT       LAS

ALGORITHM 
   Check the validity of the projection distance
      If INVAL the corners cannot be compared
   Check the corner validity flags for each input DDR
      Set the flags accordingly

   Check to see if the projection code equal geographic
   If it is not geographic
      If one of the corner valid flags is invalid 
         Set the corner flag argument to be INVAL

      If all are valid, calculate the input window in projection space and
      check to see if the values are all equal within a precision of tgbl
         If they are not equal 
	    Make the corner flag argument to be INVAL
         Else
            Set the corner flag argument to VALID
         Endif

      Else if all the validity flags are unknown, calculate the input window in
      projection space check to see if the values are within precision of tgbl
         If they are not equal 
	    Make the corner flag argument to be UDIFF
         Else
            Set the corner flag argument to USAME
         Endif

      Else the validity flags are both unknown and valid, calculate the input
      window in projection space of only the DDRs with valid flags set to 
      valid and check to see if the values are within the precision of tgbl
         If they are not equal 
	    Make the corner flag argument to be INVAL
         Else
  	    Set the corner index argument to the number of the first valid value
            Set the corner flag arguement to VALID
         Endif
   Else the projection code is geographic
      If one of the corner valid flags is invalid 
         Set the corner flag argument to be INVAL

      If all are valid, check to see if the input window values are all equal 
         If they are not equal 
	    Make the corner flag argument to be INVAL
         Else
            Set the corner flag argument to VALID
         Endif

      Else if all the validity flags are unknown, check to see if the input
      window values are equal
         If they are not equal 
	    Make the corner flag argument to be UDIFF
         Else
            Set the corner flag argument to USAME
         Endif

      Else the validity flags are both unknown and valid, check only the DDRs 
      with valid flags set to valid and check to see if the input window values
      are equal
         If they are not equal 
	    Make the corner flag argument to be INVAL
         Else
  	    Set the corner index argument to the number of the first valid value
            Set the corner flag arguement to VALID
         Endif
   Endif
   Return

ALGORITHM REFERENCES	none
*******************************************************************************/


#include "las.h"

void FUNCTION compare(const struct DDR *ddr1,struct DDR *ddr2,
		int window1[],int window2[],double pdist_y,double pdist_x,
		float tgbl,int *corner_flag,int gbl_flag,int image1,int image2,
		short line_inv,short samp_inv);
		
void FUNCTION set_corn_flag(struct DDR *ddr,int *corner_flag,int *window[],
		int *copy_index,int pdist_flag, float tgbl,int pdist_index,int gbl_flag,int nimg)

{

#define Window(x) (&((*window)[(x)*4]))
  
double pdist_x=-99;		/* projection distance in the x direction    */
double pdist_y=-99;		/* projection distance in the y direction    */

int first;			/* first valid field found flag		    */
int i;				/* counter -- current image		    */
int invalid;			/* invalid flagf			    */
int latlon;			/* latlon flag				    */
int unknown;			/* unknown flag				    */
int valid;			/* valid flag				    */

short line_inv[MAXIMG];		/* inverse flag -- designates if the proj.
				   coordinates are inversed from the norm */
short samp_inv[MAXIMG];		/* inverse flag -- designates if the proj.
				   coordinates are inversed from the norm */
char errtxt[ERRLEN + 1];

/*  Initialize flags
--------------------*/
invalid = FALSE;
valid = TRUE;
unknown = TRUE;
latlon = FALSE;

if (pdist_flag == INVAL)
   invalid = TRUE;
else
   {
   pdist_y = ddr[pdist_index].pdist_y;
   pdist_x = ddr[pdist_index].pdist_x;
   }

/*  Set flags as needed
-----------------------*/
for (i = 0; (i < nimg) && (invalid != TRUE); i++)
   {
   if (ddr[i].valid[DDCCV] == INVAL)
      invalid = TRUE;
   if (ddr[i].valid[DDCCV] != VALID)
      valid = FALSE;
   if (ddr[i].valid[DDCCV] != UNKNOW)
      unknown = FALSE;

   if ((ddr[i].proj_code == 0) && 
       (ddr[i].valid[DDPCV] == VALID))
      latlon = TRUE;
   if (ddr[i].loright[0] > ddr[i].upright[0])
      line_inv[i] = TRUE;
   else
      line_inv[i] = FALSE;
   if (ddr[i].loright[1] < ddr[i].loleft[1])
      samp_inv[i] = TRUE;
   else
      samp_inv[i] = FALSE;
   }

/*  If not a geographic projection calculate and compare the windows of the
    input images in projection space, otherwise just compare the windows
----------------------------------------------------------------------------*/
if (!latlon)
   {
   if (invalid)
      {
      *corner_flag = INVAL;
      *copy_index = 0;
      }

   else if (valid)
      {
      *corner_flag = VALID;
      *copy_index = 0;
      for (i = 0; (i < nimg - 1) && (*corner_flag != gbl_flag); i++)
         {
	 compare(&ddr[i],&ddr[i + 1],Window(i),Window(i + 1),
		 pdist_y,pdist_x,tgbl,corner_flag,gbl_flag,i + 1,i + 2,
                 line_inv[i],samp_inv[i]);
         *corner_flag = VALID;
         } /* for i < nimg */
      } /* else if valid */
      
   else if (unknown)
      {
      *corner_flag = UNKNOW;
      *copy_index = 0;
      for (i = 0; (i < nimg - 1) && (*corner_flag != gbl_flag); i++)
         {
	 compare(&ddr[i],&ddr[i + 1],Window(i),Window(i + 1),
		 pdist_y,pdist_x,tgbl,corner_flag,gbl_flag,i + 1,i + 2,
                 line_inv[i],samp_inv[i]);
         } /* for i < nimg */
      } /* else if unknown */
   
   else
      { /* unknown and valid */
      *corner_flag = VALID;
      first = TRUE;
      for (i = 0; (i < nimg) && (*corner_flag != gbl_flag); i++)
         {
         if (ddr[i].valid[DDCCV] == VALID)
	    {
	    if (first)
	       {
	       first = FALSE;
	       *copy_index = i;
	       } /* if first */
	    else
	       {
	       compare(&ddr[i],&ddr[*copy_index],Window(i),Window(*copy_index),
		       pdist_y,pdist_x,tgbl,corner_flag,gbl_flag,i + 1,
		       *copy_index + 1,line_inv[i],samp_inv[i]);
               }
	    } /*  if ddr[i].valid[DDPCOD] == valid */
        } /* for i < nimg */
      } /* else  unknown and valid */
   } /* if not lat lon */
else
   { /* if lat lon */
   if (invalid)
      {
      corner_flag = INVAL;
      *copy_index = 0;
      }

   else if (valid)
      {
      *corner_flag = VALID;
      *copy_index = 0;
      for (i = 0; (i < nimg - 1) && (*corner_flag != gbl_flag); i++)
         {
         if ((Window(i)[0] != Window(i + 1)[0]) ||
             (Window(i)[1] != Window(i + 1)[1]) ||
             (Window(i)[2] != Window(i + 1)[2]) ||
             (Window(i)[3] != Window(i + 1)[3]))
   		{
   		*corner_flag = gbl_flag;
   		sprintf(errtxt,"%s%d%s%d%s%f%s",
		        "Corner values of image ",i, " and ", i + 1,
		        " are not within ",tgbl, " pixels");
   		c_errmsg(errtxt,"upddr-corner",NON_FATAL);
   		}
         } /* for i < nimg */
      } /* else if valid */
      
   else if (unknown)
      {
      *corner_flag = UNKNOW;
      *copy_index = 0;
      } /* else if unknown */
   
   else
      { /* unknown and valid */
      *corner_flag = VALID;
      first = TRUE;
      for (i = 0; (i < nimg) && (*corner_flag != gbl_flag); i++)
         {
         if (ddr[i].valid[DDCCV] == VALID)
	    {
	    if (first)
	       {
	       first = FALSE;
	       *copy_index = i;
	       } /* if first */
	    else
	       {
               if ((Window(*copy_index)[0] != Window(i)[0]) ||
                   (Window(*copy_index)[1] != Window(i)[1]) ||
                   (Window(*copy_index)[2] != Window(i)[2]) ||
                   (Window(*copy_index)[3] != Window(i)[3]))
   		   {
   		   *corner_flag = gbl_flag;
   		   sprintf(errtxt,"%s%d%s%d%s%f%s",
			   "Corner values of image ",*copy_index, " and ",i,
			   " are not within ",tgbl, " pixels");
   		   c_errmsg(errtxt,"upddr-corner",NON_FATAL);
   		   }
               }
	    } /*  if ddr[i].valid[DDCCV] == valid */
        } /* for i < nimg */
      } /* else  unknown and valid */
   } /* if lat lon */
} /* set_corn_flag */

/*****************************************************************************
 
   FUNCTION:	compare
 
   ALGORITHM:
      For each of the corners of the input images
         Calculate the corner in projection space of the input DDRs using 
	 the window specifications
	 Compare the the corners of each of the input DDRs and set flag
	 accordingly
      Return
******************************************************************************/

void FUNCTION compare(const struct DDR *ddr1,struct DDR *ddr2,
		int window1[],int window2[],double pdist_y,double pdist_x,
		float tgbl,int *corner_flag,int gbl_flag,int image1,int image2,
		short line_inv,short samp_inv)
		
/*
struct DDR *ddr1;		  first input ddr			   
struct DDR *ddr2;		  second input ddr			   
int window1[];			  first input window			   
int window2[];			  second input window			   
double pdist_y;			  projection distance in Y direction     
double pdist_x;			  projection distance in X direction     
float tgbl;			  TAE tolerance global		   
int *corner_flag;		  Processing corner flag		   
int gbl_flag;			  TAE global validity flag		   
int image1;			  current image			   
int image2;			  comparison image	
short line_inv;
short samp_inv;		   
*/

{  /* compare */

double tsl_coor;		/* first temp. line proj. coordinate*/
double tsl_coor2;		/* second temp. line proj. coordinate*/
double tss_coor;		/* first temp. samp proj. coordinate*/
double tss_coor2;		/* second temp. samp proj. coordinate*/


char errtxt[ERRLEN];

/*  Calculate the corner cordinates in projection units of each of the two
    input DDRs and windows
--------------------------------------------------------------------------*/
if (!line_inv)
   {
   tsl_coor = ddr1->upleft[0] - (pdist_y * (window1[0] - 1.0));
   tsl_coor2 = ddr2->upleft[0] - (pdist_y * (window2[0] - 1.0));
   }
else
   {
   tsl_coor = (pdist_y * (window1[0] - 1.0)) + ddr1->upleft[0];
   tsl_coor2 = (pdist_y * (window2[0] - 1.0)) + ddr2->upleft[0];
   }

if (!samp_inv)
   {
   tss_coor = (pdist_x * (window1[1] - 1.0)) + ddr1->upleft[1];
   tss_coor2 = (pdist_x * (window2[1] - 1.0)) + ddr2->upleft[1];
   }
else
   {
   tss_coor = ddr1->upleft[1] - (pdist_x * (window1[1] - 1.0));
   tss_coor2 = ddr2->upleft[1] - (pdist_x * (window2[1] - 1.0));
   }

/*  Compare the two coordinates to see if they are within tolerance
-------------------------------------------------------------------*/
if ((fabs(tss_coor - tss_coor2) > (tgbl * pdist_x)) ||
    (fabs(tsl_coor - tsl_coor2) > (tgbl * pdist_y)))
   *corner_flag = gbl_flag;
      
/*  Calculate the corner cordinates in projection units of each of the two
    input DDRs and windows
--------------------------------------------------------------------------*/
if (!line_inv)
   {
   tsl_coor = ddr1->loleft[0] - (pdist_y * 
	      ((window1[2] + window1[0]) - ddr1->nl));
   tsl_coor2 = ddr2->loleft[0] - (pdist_y * 
  	       ((window2[2] + window2[0]) - ddr2->nl));
   }
else
   {
   tsl_coor = (pdist_y * (window1[0] - 1.0)) + ddr1->loleft[0];
   tsl_coor2 = (pdist_y * (window2[0] - 1.0)) + ddr2->loleft[0];
   }

if (!samp_inv)
   {
   tss_coor = (pdist_x * (window1[1] - 1.0)) + ddr1->loleft[1];
   tss_coor2 = (pdist_x * (window2[1] - 1.0)) + ddr2->loleft[1];
   }
else
   {
   tss_coor = ddr1->loleft[1] - (pdist_x * 
	      ((window1[3] + window1[1]) - ddr1->ns));
   tss_coor2 = ddr2->loleft[1] - (pdist_x * 
	       ((window2[3] + window2[1]) - ddr2->ns));
   }

/*  Compare the two coordinates to see if they are within tolerance
-------------------------------------------------------------------*/
if ((fabs(tss_coor - tss_coor2) > (tgbl * pdist_x)) ||
    (fabs(tsl_coor - tsl_coor2) > (tgbl * pdist_y)))
   *corner_flag = gbl_flag;
      
/*  Calculate the corner cordinates in projection units of each of the two
    input DDRs and windows
--------------------------------------------------------------------------*/
if (!line_inv)
   {
   tsl_coor = ddr1->upright[0] - (pdist_y * (window1[0] - 1.0)); 
   tsl_coor2 = ddr2->upright[0] - (pdist_y * (window2[0] - 1.0)); 
   }
else
   {
   tsl_coor = (pdist_y * ((window1[2] + window1[0]) - ddr1->nl)) +
		     ddr1->upright[0];
   tsl_coor2 = (pdist_y * ((window2[2] + window2[0]) - 
		      ddr2->nl)) + ddr2->upright[0];
   }
if (!samp_inv)
   {
   tss_coor = (pdist_x * ((window1[3] + window1[1]) - ddr1->ns)) +
		     ddr1->upright[1];
   tss_coor2 = (pdist_x * ((window2[3] + window2[1]) - 
		      ddr2->ns)) + ddr2->upright[1];
   }
else
   {
   tss_coor = ddr1->upright[1] - (pdist_x * (window1[1] - 1.0)); 
   tss_coor2 = ddr2->upright[1] - (pdist_x * (window2[1] - 1.0)); 
   }

/*  Compare the two coordinates to see if they are within tolerance
-------------------------------------------------------------------*/
if ((fabs(tss_coor - tss_coor2) > (tgbl * pdist_x)) ||
    (fabs(tsl_coor - tsl_coor2) > (tgbl * pdist_y)))
   *corner_flag = gbl_flag;
      
/*  Calculate the corner cordinates in projection units of each of the two
    input DDRs and windows
--------------------------------------------------------------------------*/
if (!line_inv)
   {
   tsl_coor = ddr1->loleft[0] - (pdist_y * 
	      ((window1[2] + window1[0]) - ddr1->nl));
   tsl_coor2 = ddr2->loleft[0] - (pdist_y * 
	       ((window2[2] + window2[0]) - ddr2->nl));
   }
else
   {
   tsl_coor = (pdist_y * ((window1[2] + window1[0]) - ddr1->nl)) +
		     ddr1->upright[0];
   tsl_coor2 = (pdist_y * ((window2[2] + window2[0]) -
		      ddr2->nl)) + ddr2->upright[0];
   }
if (!samp_inv)
   {
   tss_coor = (pdist_x * ((window1[3] + window1[1]) - ddr1->ns)) +
		     ddr1->upright[1];
   tss_coor2 = (pdist_x * ((window2[3] + window2[1]) - 
		      ddr2->ns)) + ddr2->upright[1];
   }
else
   {
   tss_coor = ddr1->loleft[1] - (pdist_x * 
	      ((window1[3] + window1[1]) - ddr1->ns));
   tss_coor2 = ddr2->loleft[1] - (pdist_x * 
	       ((window2[3] + window2[1]) - ddr2->ns));
   }

/*  Compare the two coordinates to see if they are within tolerance
-------------------------------------------------------------------*/
if ((fabs(tss_coor - tss_coor2) > (tgbl * pdist_x)) ||
    (fabs(tsl_coor - tsl_coor2) > (tgbl * pdist_y)))
   *corner_flag = gbl_flag;
   
if (gbl_flag == *corner_flag)
   {
   sprintf(errtxt,"Corner values of image %d and %d are not within %f pixels",
	   image1,image2,tgbl);
   c_errmsg(errtxt,"upddr-corner",NON_FATAL);
   }
   
return;
}  /*  compare  */
