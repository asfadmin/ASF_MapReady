/*******************************************************************************
NAME:      		   	CAL_LIN_SMP

PURPOSE:   	Calculate the starting line and sample of each of the input
		images in the output space and the number of lines
		and samples of the output space. 

PROGRAM HISTORY:
PROGRAMMER	  DATE		REASON
----------	  ----		------
B. Ailts       July  1988       Original development
B. Ailts       July  1988       Changed name in order work on IBM RT
T. Mittan      Jan   1992       Allow only upper left corner of DDR to
				be valid and then use nl and ns.
D. Etrheim     Sep   1992	Check pixel size against an Epsilon value. 
				Comparing floating point values directly can
				cause problems if an image is transfered from
				another machine.
D. Etrheim     Apr   1993       Correct problem in update mode of deleting the
                                first input image.  Free allocated space
T. Logan       Apr   1995	Removed TAE dependencies (ASF)

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:        LAS		                

ALGORITHM DESCRIPTION:
Allocate space for DDR, upper left, and lower right coordinates
Initialization of flags
Initialization of host name pointer
For every logical image
   For every image within a logical image
      Get the ddr information of input image
      Check validity of the corner projection coordinates
      Check the validity of the projection distance
      Check for the equality of the projection distance between input
       images.  Set pdist_y and pdist_x with projection distance of the
       first image and compare the other images to these values
      If a window is specified for the input image, new corner coordinates
       will have to be calculated.  If the corner coordinates are rotated
       call C_ROTCOR else call C_CALCOR.
      If no window is specified, assign the corner coordinates from the
       DDR to local variables
      Compare the corner coordinates of images that are in one logical
      image.  They have to be within PROJTOL.  Set the temporary coordinates
      with the coordinates of the first image and then compare the other
      input image coordinates to these temporary coordinates.
   end for
end for

For every logical image
    If upper left corner coor. have not been set, find the most upper
       left corner of the input images.  Take the coor. from the first
       image and then compare them to the other input image coordinates.
    If output size in proj. coor. have not been set, find the most lower
       right corner of the input images.  Take the coor. from the first
       image and then compare them to the other input image coordinates.
   Calculate the starting line and sample of each logical input image.
   If the starting line is less than one recalculate the window 
      specifications for that image.
   If the starting sample is less than one recalculate the window 
      specifications for that image.

Calculate the number of lines and sample to be in the output image
Return
ALGORITHM REFERENCES:     none
*******************************************************************************/
#include "asf.h"
#include "las.h"
#include "concat.h"

#define EPSLN 1.0e-3

void  cal_lin_smp(
	char *hostin[],		/*  input host file names		*/
	int *total_cnt,		/*  total number of input images	*/
	int *in_count,		/*  number of logical input images	*/
	int *numimg,		/*  number of images per logical image	*/
	int window[][4],	/*  window specifications of input images*/
	float projtol,		/*  projection coordinate tolerence	*/
				/*  ASF -- changed projtol to pass by value */
	int *ulcoors_flag,	/*  upper left coor. parm flag		*/
	int *outsize_flag,	/*  output size parm. flag		*/
	double *ulcoors,	/*  upper left coordinates of output	*/
	double *outsize,	/*  output size of output image		*/
	int *sl,		/*  starting line of input in the output space*/
	int *ss,		/*  starting samp of input in the output space*/
	int *nl,		/*  number of lines in the output space	*/
	int *ns,		/*  number of samples in the output space*/
	int out_window[][4]	/*  output window specifications	*/
	)
{  /*  cal_lin_smp  */

struct DDR *ddr;		/*  pointer to the input DDR structure  */

double ext_lor[2];		/*  extreme lower right corner		*/
double loleft[2];		/*  lower left corner			*/
double *loright;		/*  pointer to lower right of input images */
double nupleft[2];		/*  next upper left corner		*/
double nloright[2];		/*  next lower right corner		*/
double pdist_x=0.0;		/*  projection distance in x direction	*/
double pdist_y=0.0;		/*  projection distance in y direction	*/
double rwindow[4];		/*  rotate window specifications	*/
double tcoor[8];		/*  temporary coordinates		*/
double tloleft[2];		/*  temp. lower left coordinates	*/
double tloright[2];		/*  temp. lower right coordinates	*/
double tupleft[2];		/*  temp. upper left coordinates	*/
double tupright[2];		/*  temp. upper right coordinates	*/
double *upleft;			/*  upper left coordinates of input	*/
double upright[2];		/*  upper right coordinates of input	*/

int cur_image = 0;		/* current image			*/
int first_corn;			/* first corner flag			*/
int first_dist;			/* first projection distance flag	*/
int first_out;			/* first output size flag		*/
int first_ul;			/* first upper left flag		*/
int image;			/* current image of logical image	*/
int img_ls[8];			/* image window specification for rotate*/
int index;			/* index for current window specs.  	*/
int limage;			/* current logical image		*/

short line_inv = FALSE;
short samp_inv = FALSE;

char errtxt[ERRLEN];		/* error text buffer			*/

/* Allocate space for DDR, upper left, and lower right coordinates
------------------------------------------------------------------*/
ddr = (struct DDR *)MALLOC(sizeof(struct DDR));
upleft = (double *)calloc(*total_cnt,16);
loright = (double *)calloc(*total_cnt,16);

/*  Initialization of flags
---------------------------*/
first_dist = TRUE;
first_out = TRUE;
first_ul = TRUE;

/* Initialization of host name pointer
--------------------------------------*/

/*  for every logical image
---------------------------*/
for (limage = 0; limage < *in_count; limage++)
   {
   first_corn = TRUE;

   /*  for every image within a logical image
   ------------------------------------------*/
   for (image = 0; image < numimg[limage]; image++)
      {
      /*  Get the ddr information of input image
      ------------------------------------------*/
      c_getddr(hostin[limage],ddr);

      /* Check validity of the corner projection coordinates
      ------------------------------------------------------*/
      if (ddr->valid[DDCCV] == INVAL)
         {
         sprintf(errtxt,"Corner projection coordinates of image %s are invalid",
	         hostin[limage]);
         c_errmsg(errtxt,"concat-invalid",NON_FATAL);
         c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
         }

      /* Check the validity of the projection distance
      ------------------------------------------------*/
      if (ddr->valid[DDPDV] == INVAL)
         {
         sprintf(errtxt,"Corner projection distance of image %s is invalid",
	         hostin[limage]);
         c_errmsg(errtxt,"concat-invalid",NON_FATAL);
         c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
         }

      /*  Check for the equality of the projection distance between input
          images.  Set pdist_y and pdist_x with projection distance of the
          first image and compare the other images to these values
      --------------------------------------------------------------------*/
      if (first_dist)
	 {
	 pdist_y = ddr->pdist_y;
	 pdist_x = ddr->pdist_x;
	 first_dist = FALSE;
	 }
      else
	 if ((fabs(pdist_y - ddr->pdist_y) > EPSLN) || 
             (fabs(pdist_x - ddr->pdist_x) > EPSLN))
	    {
	    sprintf(errtxt,"%s %s %s","The projection distance of",
		hostin[limage],
	    "does not equal the projection distance of the other input images");
	    c_errmsg(errtxt,"concat-pdist",NON_FATAL);
	    c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
	    }

      /*  If a window is specified for the input image, new corner coordinates
          will have to be calculated.  If the corner coordinates are rotated
          call C_ROTCOR else call C_CALCOR.
      ------------------------------------------------------------------------*/
      if ((window[cur_image][0] != 1) || (window[cur_image][1] != 1) ||
  	  (window[cur_image][2] != ddr->nl) || 
	  (window[cur_image][3] != ddr->ns))
	 {
	 rwindow[0] = (double)window[cur_image][0];
	 rwindow[1] = (double)window[cur_image][1];
	 rwindow[2] = (double)window[cur_image][2];
	 rwindow[3] = (double)window[cur_image][3];
	 if ((fabs(ddr->upleft[0] - ddr->upright[0]) > projtol) && 
            ((ddr->upright[0] != ddr->loleft[0]) && 
             (ddr->upright[1] != ddr->loleft[1])))
	    {
	    img_ls[0] = 1;
	    img_ls[1] = 1;
	    img_ls[2] = ddr->nl;
	    img_ls[3] = 1;
	    img_ls[4] = 1;
	    img_ls[5] = ddr->ns;
	    img_ls[6] = ddr->nl;
	    img_ls[7] = ddr->ns;
	    c_rotcor(ddr->upleft,img_ls,rwindow,tcoor);

	    *(upleft + cur_image * 2) = tcoor[0];
	    *(upleft + (cur_image * 2) + 1) = tcoor[1];
	    loleft[0]  = tcoor[2];
	    loleft[1]  = tcoor[3];
	    upright[0] = tcoor[4];
	    upright[1] = tcoor[5];
	    *(loright + cur_image * 2) = tcoor[6];
	    *(loright + (cur_image * 2) + 1) = tcoor[7];
	    }
	 else
	    c_calcor(&rwindow[0],&rwindow[1],&rwindow[2],&rwindow[3],ddr,
		     upleft + cur_image * 2,upright,loleft,
		     loright + cur_image * 2);
	 }  /*  if windowed  */

      /*  If no window is specified, assign the corner coordinates from the
          DDR to local variables
      ---------------------------------------------------------------------*/
      else
	 {
	 *(upleft + cur_image * 2) = ddr->upleft[0];
	 *(upleft + (cur_image * 2) + 1) = ddr->upleft[1];

         /* if only the upper left corner value is specified
            use the NL and NS to calculate the other corners.
         --------------------------------------------------*/
	 if ((ddr->upright[0] == 0) && (ddr->upright[1] == 0) &&
              (ddr->loleft[0] == 0) && (ddr->loleft[1] == 0))
            {
	    upright[0] = ddr->upleft[0];
	    upright[1] = ddr->upleft[1] + ((ddr->ns - 1) * ddr->pdist_x);
	    loleft[0]  = ddr->upleft[0] - ((ddr->nl - 1) * ddr->pdist_y);
	    loleft[1]  = ddr->upleft[1];
	    *(loright + cur_image * 2) = ddr->upleft[0] - 
                                         ((ddr->nl - 1) * ddr->pdist_y);
	    *(loright + (cur_image * 2) + 1) = ddr->upleft[1] + 
                                               ((ddr->ns - 1) * ddr->pdist_x); 
            }
         else
            {
	    upright[0] = ddr->upright[0];
	    upright[1] = ddr->upright[1];
	    loleft[0]  = ddr->loleft[0];
	    loleft[1]  = ddr->loleft[1];
	    *(loright + cur_image * 2) = ddr->loright[0];
	    *(loright + (cur_image * 2) + 1) = ddr->loright[1];
            }
	 }  /* not windowed  */

      /*  Compare the corner coordinates of images that are in one logical
      image.  They have to be within PROJTOL.  Set the temporary coordinates
      with the coordinates of the first image and then compare the other
      input image coordinates to these temporary coordinates.
      ----------------------------------------------------------------------*/
      if (first_corn)
	 {
	 tupleft[0]  = *(upleft + cur_image * 2);
	 tupleft[1]  = *(upleft + (cur_image * 2) + 1);
	 tupright[0] = upright[0];
	 tupright[1] = upright[1];
	 tloleft[0]  = loleft[0];
	 tloleft[1]  = loleft[1];
	 tloright[0] = *(loright + cur_image * 2);
	 tloright[1] = *(loright + (cur_image * 2) + 1);
	 first_corn  = FALSE;
	 }  /*  first_corn  */
      else
     	 {
         nupleft[0]  = *(upleft + cur_image * 2);
         nupleft[1]  = *(upleft + (cur_image * 2) + 1);
         nloright[0] = *(loright + cur_image * 2);
         nloright[1] = *(loright + (cur_image * 2) + 1);
	 if ((fabs(tupleft[0]  - nupleft[0])  > projtol) ||
	     (fabs(tupleft[1]  - nupleft[1])  > projtol) ||
	     (fabs(tloleft[0]  - loleft[0])   > projtol) ||
	     (fabs(tloleft[1]  - loleft[1])   > projtol) ||
	     (fabs(tupright[0] - upright[0])  > projtol) ||
	     (fabs(tupright[1] - upright[1])  > projtol) ||
	     (fabs(tloright[0] - nloright[0]) > projtol) ||
	     (fabs(tloright[1] - nloright[1]) > projtol))
	    {
	    sprintf(errtxt,"%s %s %s","The projection coordinates of",
		hostin[limage],
		    "are not within tolerance of the other input images");
	    c_errmsg(errtxt,"concat-tolerance",NON_FATAL);
	    c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
	    }  /*  if coordinates not within tolerance  */
	 } /*  not first_corn  */

      /*  If upper left corner coor. have not been set, find the most upper
          left corner of the input images.  Take the coor. from the first
          image and then compare them to the other input image coordinates.
      ---------------------------------------------------------------------*/
      if (*(loright + cur_image * 2) > upright[0])
         line_inv = TRUE;
      if (*(loright + (cur_image * 2) + 1) < loleft[1])
         samp_inv = TRUE;
      if (!*ulcoors_flag)
         {
	 if (first_ul)
	    {
            if (!line_inv)
               {
	       if (*(upleft + cur_image * 2) > upright[0])
	          ulcoors[0] = *(upleft + cur_image * 2);
	       else
	          ulcoors[0] = upright[0];
               }
            else
               {
	       if (*(upleft + cur_image * 2) < upright[0])
	          ulcoors[0] = *(upleft + cur_image * 2);
	       else
	          ulcoors[0] = upright[0];
               }

            if (!samp_inv)
               {
	       if (*(upleft + (cur_image * 2) + 1) < loleft[1])
	          ulcoors[1] = *(upleft + (cur_image * 2) + 1);
	       else
	          ulcoors[1] = loleft[1];
               }
            else
               {
	       if (*(upleft + (cur_image * 2) + 1) > loleft[1])
	          ulcoors[1] = *(upleft + (cur_image * 2) + 1);
	       else
	          ulcoors[1] = loleft[1];
               }
	    first_ul = FALSE;
	    }
	 else
	    {
            if (!line_inv)
               {
	       if (ulcoors[0] < *(upleft + cur_image * 2))
	           ulcoors[0] = *(upleft + cur_image * 2);
	       if (ulcoors[0] < upright[0])
	           ulcoors[0] = upright[0];
               }
            else
               {
	       if (ulcoors[0] > *(upleft + cur_image * 2))
	           ulcoors[0] = *(upleft + cur_image * 2);
	       if (ulcoors[0] > upright[0])
	           ulcoors[0] = upright[0];
               }
            if (!samp_inv)
               {
	       if (ulcoors[1] > *(upleft + (cur_image * 2) + 1))
	           ulcoors[1] = *(upleft + (cur_image * 2) + 1);
	       if (ulcoors[1] > loleft[1])
	           ulcoors[1] = loleft[1];
               }
            else
               {
	       if (ulcoors[1] < *(upleft + (cur_image * 2) + 1))
	           ulcoors[1] = *(upleft + (cur_image * 2) + 1);
	       if (ulcoors[1] < loleft[1])
	           ulcoors[1] = loleft[1];
               }
	    }  /*  not first_ul  */
         }  /*  if !*ulcoors_flag  */
	 
      /*  If output size in proj. coor. have not been set, find the most lower
          right corner of the input images.  Take the coor. from the first
          image and then compare them to the other input image coordinates.
      ---------------------------------------------------------------------*/
      if (!*outsize_flag)
         {
	 if (first_out)
	    {
            /*  This check is made specifically for images that have the 
                projection coordinates in total seconds.  The southern 
                hemisphere need to check for the largest projection
                coordinate instead of the smallest
            ---------------------------------------------------------------*/
            if (!line_inv)
               {
	       if (*(loright + cur_image * 2) < loleft[0])
	          ext_lor[0] = *(loright + cur_image * 2);
	       else
	          ext_lor[0] = loleft[0];
               }
            else
               {
	       if (*(loright + cur_image * 2) > loleft[0])
	          ext_lor[0] = *(loright + cur_image * 2);
	       else
	          ext_lor[0] = loleft[0];
               }

            /*  This check is made specifically for images that have the 
                projection coordinates in total seconds.  The western
                hemisphere needs to check for the smallest projection
                coordinate instead of the largest
            ---------------------------------------------------------------*/
            if (!samp_inv)
               {
	       if (*(loright + (cur_image * 2) + 1) > upright[1])
	          ext_lor[1] = *(loright + (cur_image * 2) + 1);
	       else
	          ext_lor[1] = upright[1];
               }
	    else
               {
	       if (*(loright + (cur_image * 2) + 1) < upright[1])
	          ext_lor[1] = *(loright + (cur_image * 2) + 1);
	       else
	          ext_lor[1] = upright[1];
               }
	    first_out = FALSE;
	    }
	 else
	    {
            /*  This check is made specifically for images that have the 
                projection coordinates in total seconds.  The southern 
                hemisphere need to check for the largest projection
                coordinate instead of the smallest
            ---------------------------------------------------------------*/
            if (!line_inv)
               {
	       if (ext_lor[0] > *(loright + cur_image * 2))
	           ext_lor[0] = *(loright + cur_image * 2);
	       if (ext_lor[0] > loleft[0])
	           ext_lor[0] = loleft[0];
               }
            else
               {
	       if (ext_lor[0] < *(loright + cur_image * 2))
	           ext_lor[0] = *(loright + cur_image * 2);
	       if (ext_lor[0] < loleft[0])
	           ext_lor[0] = loleft[0];
               }
            /*  This check is made specifically for images that have the 
                projection coordinates in total seconds.  The western
                hemisphere needs to check for the smallest projection
                coordinate instead of the largest
            ---------------------------------------------------------------*/
            if (!samp_inv)
               {
	       if (ext_lor[1] < *(loright + (cur_image * 2) + 1))
	           ext_lor[1] = *(loright + (cur_image * 2) + 1);
	       if (ext_lor[1] < upright[1])
	           ext_lor[1] = upright[1];
               }
            else
               {
	       if (ext_lor[1] > *(loright + (cur_image * 2) + 1))
	           ext_lor[1] = *(loright + (cur_image * 2) + 1);
	       if (ext_lor[1] > upright[1])
	           ext_lor[1] = upright[1];
               }
	    }
         if (!line_inv) outsize[0] = ulcoors[0] - ext_lor[0] + pdist_y;
         else outsize[0] = ext_lor[0] - ulcoors[0] + pdist_x;
         if (!samp_inv) outsize[1] = ext_lor[1] - ulcoors[1] + pdist_x;
         else outsize[1] = ulcoors[1] - ext_lor[1] + pdist_y;
         }  /*  if !*outsize_flag  */
      cur_image ++;
      }  /*  for image < numimg[limage]  */
   }  /*  for limage < *in_count  */

/*  calculate the starting line and sample of each logical input image.
-----------------------------------------------------------------------*/
index = 0;
for (limage = 0; limage < *in_count; limage++)
   {
   if (!line_inv) sl[limage] = (ulcoors[0] - *(upleft+index*2)) / pdist_y + 1;
   else sl[limage] = (*(upleft + index * 2) - ulcoors[0]) / pdist_y + 1;
   out_window[limage][SL] = sl[limage];
   if (!samp_inv) ss[limage] = (*(upleft+(index*2)+1)-ulcoors[1]) / pdist_x+1;
   else ss[limage] = (ulcoors[1] - *(upleft + (index * 2) + 1)) / pdist_x + 1;
   out_window[limage][SS] = ss[limage];

/*
   if ((sl[limage] = (ulcoors[0] - *(upleft + index * 2)) / pdist_y + 1) < 1)
        sl[limage] = (*(upleft + index * 2) - ulcoors[0]) / pdist_y + 1;
   if ((ss[limage] = (*(upleft + (index * 2) + 1) - ulcoors[1])/pdist_x + 1)< 1)
        ss[limage] = (ulcoors[1] - *(upleft + (index * 2) + 1)) / pdist_x + 1;
*/

   /*  If the starting line is less than one recalculate the window 
       specifications for that image.
   ----------------------------------------------------------------*/
   if (sl[limage] < 1)
      {
      cur_image = index;
      for (image = 0; image < numimg[limage]; image++)
        {
         window[cur_image][0] += abs(sl[limage] - 1);
         window[cur_image][2] = (window[cur_image][2]-window[cur_image][0])+1;
	 cur_image ++;
	} /*  for image < numimg[limage]  */
      sl[limage] = 1;
      out_window[limage][SL] = sl[limage];
      } /* if sl[image] < 1  */

   /*  If the starting sample is less than one recalculate the window 
       specifications for that image.
   ------------------------------------------------------------------*/
   if (ss[limage] < 1)
      {
      cur_image = index;
      for (image = 0; image < numimg[limage]; image++)
         {
         window[cur_image][1] += abs(ss[limage] - 1);
         window[cur_image][3] = (window[cur_image][3]-window[cur_image][1])+1;
	 cur_image ++;
	 } /*  for image < numimg[limage]  */
      ss[limage] = 1;
      out_window[limage][SS] = ss[limage];
      } /* if ss[image] < 1  */
	 
   index += numimg[limage];
   }  /*  for limage < *in_count  */

/*  Calculate the number of lines and sample to be in the output image
----------------------------------------------------------------------*/
*nl = outsize[0] / pdist_y;
*ns = outsize[1] / pdist_x;

free(ddr);
free(upleft);
free(loright);

return;
}  /*  cal_lin_smp  */
