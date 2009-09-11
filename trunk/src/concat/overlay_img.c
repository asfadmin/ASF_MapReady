/*******************************************************************************
NAME:      	 OVERLAY_IMG

PURPOSE:   	To overlay the input image into the output image

PROGRAM HISTORY:
PROGRAMMER	  DATE		REASON
----------	  ----		------
B. Wilsey      Aug   1984	Original development
D. Akkerman    Aug   1987       PR #4247 modified to allow for single band 
				 specification of a multi-band images
K. Zanter      Sept. l987       NEWLAS (conversion to 'C')
B. Ailts       July  1988       LAS5.0 conversion -- added -AUTO subcommand
T. Mittan      Jan   1992	Added update option, and averaging option.
				also, maskval can be a range
T. Mittan      Mar   1992	Modified averaging option to choose which
				direction the averaging should be performed
D. Etrheim     Apr   1993       Correct problem in update mode of deleting the
                                first input image.  Free allocated space.
T. Logan       Apr   1995	Removed TAE dependencies (ASF)
				  Removed TAE PARBLK parameter
				  Removed timer routines
				  removed calls to c_clean
				  changed maskval to scalar
				  
COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:        LAS		                

ALGORITHM DESCRIPTION:
Initialization 
Ensure that window specification does not exceed the image bounds
Open image for reading
Allocate space for image I/O buffer
Set up file group
Do initial read
Set up and start timed processing message
Switch on dtype -- EBYTE, EWORD, ELONG, or EREAL
    Type cast to correct pointer
    For each line
       For each band
          For each sample
             If input image sample does not eqaul mask
	        If replace or (leave and output image sample equals mask)
		   Output sample equals input sample
       Read/write each image 
    Break for switch statement
Close images
Free buffer
Return

ALGORITHM REFERENCES:     none
*******************************************************************************/
#include "asf.h"
#include "las.h"
#include "locinc.h"
#include "concat.h"

extern int doConcat; /*Copy data?*/
extern int colorConcat;		 /* Create color image? 	      */
extern int colorCur;		 /* Current color band. 	      */


void overlay_img(char  *hostin,            /* pointer to input host file descriptor     */
      	      	 int    nmrimg,            /* number of images in current specification */
      	      	 int    wind[][4],         /* window specification                      */
                 int    bands[][MAXBND+1], /* band specification                        */
                 int    nbands[],          /* number of bands in each image             */
                 int   *sl,                /* pointer to starting line location         */
                 int   *ss,                /* pointer to starting sample location       */
                 float *mask,              /* pointer to mask value                     */
                 char  *hostout,           /* pointer to output host file descriptor    */
      	      	 int   *dtype,             /* pointer to data type of output image      */
      	      	 int   *totbnd,            /* pointer to number of bands in output image*/
      	      	 int   *nl,                /* pointer to number of lines in output image*/
      	      	 int   *ns,                /* pointer to number of samples per line     */
      	      	 int    replace,           /* replace option for overlap condition      */
      	      	 int    leave,             /* leave option for overlap condition        */
      	      	 int    aver,              /* average option for the overlap condition  */
      	      	 int   *update,            /* update flag                               */
      	      	 int   *laver              /* left average array                        */
		)
{
 float   *rthebuf;		 /* float pointer to thebuf		      */
 int	access;			 /* image I/O access type		      */
 int	band;			 /* loop index counting the bands	      */
 int    bufsiz;			 /* size of image I/O buffer		      */
 int    distance;                /* number of overlap pixels on a certain line*/
 int	curent;		         /* current line number for display message   */
 int   	i;			 /* loop index				      */
 int    index;			 /* index				      */
 int    lmask;			 /* int mask value		      	      */
 int    lmask1;			 /* int mask value		      	      */
 int    *lthebuf;		 /* int pointer to thebuf		      */
 int    *ltemp;
 int    *ltemp1;
 int    nl1;			 /* true number of lines		      */
 int    ns1;			 /* true number of samples		      */
 extern  int image_starts_in[MAXIMG * MAXBND + 1];/* offsets into thebuf for input  */
 extern  int image_starts_out[MAXBND+1];	  /* offsets into thebuf for output */
 int	samp;			 /* loop index counting samples		      */
 int	sl1;			 /* true starting line			      */
 int	ss1;			 /* true starting sample		      */
 int	status;			 /* function return status code		      */
 int    temp_bands[MAXBND+1];	 /* temporary bands array for (update) eopens */
 int	temp_val;		 /* temporary value			      */
 int	zero = 0;		 /* value of zero			      */
 int    opt[3];

 float   mfactor=1;		 /* multiplication factor		      */
 float   rate=0;		 /* rate of change per pixel		      */
 float   *ftemp;
 float   *ftemp1;

 short   wmask;			 /* short mask value			      */
 short   wmask1;		 /* short mask value			      */
 short	*wtemp;
 short	*wtemp1;
 short   *wthebuf;		 /* short pointer to thebuf		      */

 char *hname_ptr;		 /* pointer to current host name	      */
 unsigned char  byte_background; /* BYTE mask value			      */
 unsigned char  *byte_ptr_in;	 /* BYTE mask value			      */
 unsigned char  *byte_ptr_out;	 /* BYTE mask value			      */
 unsigned char  *thebuf;	 /* pointer to image I/O buffer		      */

 /* Initialization
 -----------------*/
 hname_ptr = hostin;

 /* ensure that window specification does not exceed the image bounds
 --------------------------------------------------------------------*/
 for (i = 0; i < nmrimg; i++)
    {
 if (!*update)
    {
    temp_val = *nl - *sl + 1;
    wind[i][NL] = wind[i][NL] < temp_val ? wind[i][NL] : temp_val;
    temp_val = *ns - *ss + 1;
    wind[i][NS] = wind[i][NS] < temp_val ? wind[i][NS] : temp_val;
    } 
 else /* if in update mode */
    {
    temp_val = *nl - *sl + 1;
    wind[i][NL] = wind[i][NL] < temp_val ? wind[i][NL] : temp_val;
    temp_val = *ns - *ss + 1;
    wind[i][NS] = wind[i][NS] < temp_val ? wind[i][NS] : temp_val;
    /*if ((wind[i][SL] + wind[i][NL]) < (*nl + *sl))
        wind[i][NL] = wind[i][NL] - *sl + 1;
    if ((wind[i][SS] + wind[i][NS]) < (*ns + *ss))
        wind[i][NS] = wind[i][NS] - *ss + 1;*/
/*
    wind[i][NS] = abs(wind[i][NS] - wind[i][SS] + *ss) < 
    abs(temp_val) ? wind[i][NL] : temp_val;
    temp_val = *nl - *sl + 1;
    wind[i][NL] = wind[i][NL] < temp_val ? wind[i][NL] : *nl;
    temp_val = *ns + *ss;
    wind[i][NS] = wind[i][NS] < temp_val ? wind[i][NS] : *ns;
*/
    }
    }

 /*  Check for negative offsets
 ------------------------------*/
 if (*sl <= zero) 
   {
   sl1 = 1;
   nl1 = wind[0][NL] - abs(*sl) - 1;
   }
 else
   {
   sl1 = *sl;
   nl1 = wind[0][NL];
   }
 if (nl1 <= 0)
   {
   c_errmsg("Offsets outside output image bounds","informational",NON_FATAL);
   return;
   }

 if (*ss <= zero)
   {
   ss1 = 1;
   ns1 = wind[0][NS] - abs(*ss) - 1;
   }
 else
   {
   ss1 = *ss;
   ns1 = wind[0][NS];
   }
 if (ns1 <= 0)
   {
   c_errmsg("Offsets outside output image bounds","informational",NON_FATAL);
   return;
   }

 /* open output "padded" image for update
 ----------------------------------------*/
 opt[0] = opt[2] = 0;
 opt [1] = 0;
 access = IUPDATE;
 for (i = 0; i < *totbnd; i++)
    temp_bands[i] = i+1;
 temp_bands[*totbnd] = 0;
 status = c_eopens(&fdesc[0],hostout,temp_bands,&sl1,&ss1,&nl1,
   	          &ns1,image_starts_out,&access,dtype,&zero,opt);
   if (status != E_SUCC)
      {
      if (*update)
         c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
     /* else c_clean(vblk,"concat",hostout,&one); */ /*ASF*/
      }

 /* open bands of "inset" image for reading
 ------------------------------------------*/
 access = IREAD;
 index = 0;
 for (i = 0; i < nmrimg; i++)
   {
   /*  Check for negative offsets
   ------------------------------*/
   if (*sl <= zero) 
      {
      sl1 = abs(*sl) + wind[i][SL] + 1;
      nl1 = wind[i][NL] - abs(*sl) - 1;
      }
   else
      {
      sl1 = wind[i][SL];
      nl1 = wind[i][NL];
      }
   if (nl1 <= 0)
	continue;

   if (*ss <= zero)
      {
      ss1 = abs(*ss) + wind[i][SS] + 1;
      ns1 = wind[0][NS] - abs(*ss) - 1;
      }
   else
      {
      ss1 = wind[i][SS];
      ns1 = wind[i][NS];
      }
   if (ns1 <= 0)
	continue;

   status = c_eopens(&fdesc[i+1],hname_ptr,bands[i],&sl1,&ss1,&nl1,&ns1,
		    &image_starts_in[index],&access,dtype,&zero,&zero);
   if (status != E_SUCC)
      {
      if (*update)
         c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
      /* else c_clean(vblk,"concat",hostout,&one); */  /*ASF*/
      }

   hname_ptr += CMLEN;
   index += nbands[i];
   }

 fdesc[nmrimg+1] = 0;

 /* allocate space for image I/O buffer
 --------------------------------------*/
 if ((thebuf = (unsigned char *) c_ealloc(fdesc,&bufsiz)) == NULL)
   {
   c_errmsg("Error allocating dynamic memory","concat-alloc",NON_FATAL);
   if (c_eclose(fdesc) != E_SUCC)
      {
      c_errmsg("Error closing file","concat-close",NON_FATAL);
      c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);
      }
   if (*update)
      c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
   /* else c_clean(vblk,"concat",hostout,&one); */ /*ASF*/
   }

 /* set up file group
 --------------------*/
 if (c_egroup(fdesc,&gdesc,thebuf,&bufsiz) != E_SUCC)
   {
   if (c_eclose(fdesc) != E_SUCC)
      {
      c_errmsg("Error closing file","concat-close",NON_FATAL);
      c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);
      }
   if (*update)
      c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
   /* else c_clean(vblk,"concat",hostout,&one); */  /*ASF*/
   }
 /* initial estep
 ----------------*/
 if (c_estep(&gdesc) != E_SUCC)
   {
   if (c_egclse(&gdesc) != E_SUCC)
      {
      c_errmsg("Error closing file","concat-close",NON_FATAL);
      c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);
      }
   if (*update)
      c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
   /* else c_clean(vblk,"concat",hostout,&one); */  /*ASF*/
   }

 /* set up and start timed processing message
 --------------------------------------------*/
 curent = 1;

 /* overlay "inset" image into "padded" image.  Based on the data type execute 
    the correct processing loop.
 -----------------------------------------------------------------------------*/
 if (doConcat)
  switch (*dtype)
   {
   case EBYTE:
      byte_background  = (unsigned char ) *mask;			/*ASF*/
      for (curent = 0; curent < nl1; curent++) /*For each line*/
         {
         
         for (band = 0; band < *totbnd; band++) /*For each output band*/
          {
          if (colorConcat)
          {
          	byte_ptr_in  = (thebuf + *(image_starts_in + 0));
          	byte_ptr_out = (thebuf + *(image_starts_out + colorCur));
          }
          else
          {
          	byte_ptr_in  = (thebuf + *(image_starts_in + band));
          	byte_ptr_out = (thebuf + *(image_starts_out + band));
          }
	  if (replace) /*The default.*/
            {
            for (samp = 0; samp < ns1; samp++, byte_ptr_in++, byte_ptr_out++)
               if (*byte_ptr_in != byte_background)
                  *byte_ptr_out = *byte_ptr_in;
            }
          else
          if (leave) 
            for (samp = 0; samp < ns1; samp++,byte_ptr_in++,byte_ptr_out++)
               {
               if (*byte_ptr_in  !=  byte_background)
                  if (*byte_ptr_out == byte_background)
	               *byte_ptr_out = *byte_ptr_in;
	        }  
          else
	    { /* if (aver) */
            /* calculate the overlap distance
            -------------------------------*/
            for (distance = 1,samp = 0; samp < ns1; samp++, byte_ptr_in++, byte_ptr_out++)
              {
              if (( *byte_ptr_in != byte_background) &&
                  ( *byte_ptr_out != byte_background))
	      distance ++;
	      }
            if (colorConcat)
            {
          	byte_ptr_in  = (thebuf + *(image_starts_in + 0));
          	byte_ptr_out = (thebuf + *(image_starts_out + colorCur));
            }
            else
            {
          	byte_ptr_in  = (thebuf + *(image_starts_in + band));
           	byte_ptr_out = (thebuf + *(image_starts_out + band));
            }
	    rate = 1.0 / distance;
	    mfactor = 1.0;
            for (samp = 0; samp < ns1; samp++, byte_ptr_in++, byte_ptr_out++)
               {
               if (*byte_ptr_in != byte_background)
                  {
	          if (*byte_ptr_out != byte_background)
	               *byte_ptr_out  = *byte_ptr_in;
	          else
	             {
	             mfactor -= rate;
                     if (laver[curent])
                       {
	               *byte_ptr_out  = ((1.0-mfactor) * (*byte_ptr_out)) + 
	               (mfactor * (*byte_ptr_in));
                       }
                     else
                       {
	               *byte_ptr_out  = (mfactor * (*byte_ptr_out)) + 
	               ((1.0-mfactor) * (*byte_ptr_in));
                       }
                     }
                  }
	        }
	      }
             }
	    if (c_estep(&gdesc) != E_SUCC)
              {
              if (c_egclse(&gdesc) != E_SUCC)
                {
                c_errmsg("Error closing file","concat-close",NON_FATAL);
                c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);
                }
              if (*update)
                 c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
              /* else c_clean(vblk,"concat",hostout,&one); */ /*ASF*/
              }
         }  /* for curent < nl1  */
      break;

   case EWORD:
      wmask = (short ) *mask;			/*ASF*/
      wmask1= (short ) *mask;			/*ASF*/
      wthebuf = (short *)thebuf;
      for (curent = 0; curent < nl1; curent++)
         {
         for (band = 0; band < *totbnd; band++)
            {
          /* calculate the overlap distance if averaging
          ---------------------------------------------*/
          wtemp  = (wthebuf + *(image_starts_in + band));
          wtemp1 = (wthebuf + *(image_starts_out + band));
	  if (aver)
	  {
	  distance = 1;
          for (samp = 0; samp < ns1; samp++)
            {
            if (((*(wtemp+samp)  < wmask) || 
                ( *(wtemp+samp)  > wmask1)) &&
                ((*(wtemp1+samp) < wmask) || 
                ( *(wtemp1+samp) > wmask1)))
	    distance += 1;
	    }
	  rate = 1.0 / distance;
	  mfactor = 1.0;
	  }
            for (samp = 0; samp < ns1; samp++)
               {
               if ((*(wtemp+samp) < wmask) || 
                  ( *(wtemp+samp) > wmask1))
                  {
	          if ((replace) ||
	              ((leave) && ((*(wtemp1+samp) >= wmask)
                      && (*(wtemp1+samp) <= wmask1))))
	                  *(wtemp1+samp) = *(wtemp+samp);
		  else
	          if (aver)
		     {
	              if ((*(wtemp1+samp) >= wmask) &&
	                  (*(wtemp1+samp) <= wmask1))
	                 *(wtemp1+samp)  = *(wtemp+samp);
		      else
			{
			mfactor -= rate;
                        if (laver[curent])
	                    *(wtemp1+samp)  = ((1.0-mfactor) * (*(wtemp1+samp)))
			    + (mfactor * (*(wtemp+samp)));
                        else
	                    *(wtemp1+samp)  = (mfactor * (*(wtemp1+samp)))
			    + ((1.0 - mfactor) * (*(wtemp+samp)));
                        }
		     }
	          }
	       }  /* for samp < ns1  */
            }
	    if (c_estep(&gdesc) != E_SUCC)
              {
              if (c_egclse(&gdesc) != E_SUCC)
                {
                c_errmsg("Error closing file","concat-close",NON_FATAL);
                c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);
                }
              if (*update)
                 c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
              /* else c_clean(vblk,"concat",hostout,&one); */ /*ASF*/
              } 
         }  /* for curent < nl1  */
      break;

   case ELONG:
      lmask = (int ) *mask;			/*ASF*/
      lmask1= (int ) *mask;			/*ASF*/
      lthebuf = (int *)thebuf;
      for (curent = 0; curent < nl1; curent++)
         {
         for (band = 0; band < *totbnd; band++)
            {
          /* calculate the overlap distance if averaging
          ---------------------------------------------*/
          ltemp  = (lthebuf + *(image_starts_in + band));
          ltemp1 = (lthebuf + *(image_starts_out + band));
	  if (aver)
	  {
	  distance = 1;
          for (samp = 0; samp < ns1; samp++)
            {
            if (((*(ltemp+samp)  < lmask) || 
                ( *(ltemp+samp)  > lmask1)) &&
                ((*(ltemp1+samp) < lmask) || 
                ( *(ltemp1+samp) > lmask1)))
	    distance += 1;
	    }
	  rate = 1.0 / distance;
	  mfactor = 1.0;
	  }
            for (samp = 0; samp < ns1; samp++)
               {
               if (( *(ltemp+samp) < lmask) || 
                  (  *(ltemp+samp) > lmask1))
                  {
	          if ((replace) ||
	              ((leave) && ((*(ltemp1+samp) >= lmask)
                      && (*(ltemp1+samp) <= lmask1))))
	                 *(ltemp1+samp) = *(ltemp+samp);
		  else
	          if (aver)
		     {
	              if ((*(ltemp1+samp) >= lmask) &&
	                  (*(ltemp1+samp) <= lmask1))
	                 *(ltemp1+samp)  = *(ltemp+samp);
		      else
			{
			mfactor -= rate;
                        if (laver[curent])
	                    *(ltemp1+samp)  = ((1.0-mfactor) * (*(ltemp1+samp)))
			    + (mfactor * (*(ltemp+samp)));
                        else
	                    *(ltemp1+samp)  = (mfactor * (*(ltemp1+samp)))
			    + ((1.0 - mfactor) * (*(ltemp+samp)));
                        }
		     }
	          }
	       }  /* for samp < ns1  */
	    }
	    if (c_estep(&gdesc) != E_SUCC)
              {
              if (c_egclse(&gdesc) != E_SUCC)
                {
                c_errmsg("Error closing file","concat-close",NON_FATAL);
                c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);
                }
              if (*update)
                 c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
	      /* else c_clean(vblk,"concat",hostout,&one); */ /*ASF*/
              } 
         }  /* for curent < nl1  */
      break;

   case EREAL:
      rthebuf = (float *)thebuf;
      for (curent = 0; curent < nl1; curent++)
         {
         for (band = 0; band < *totbnd; band++)
            {
          /* calculate the overlap distance if averaging
      	  ---------------------------------------------*/
          ftemp  = (rthebuf + *(image_starts_in + band));
          ftemp1 = (rthebuf + *(image_starts_out + band));
	  if (aver)
	  {
	  distance = 1;
          for (samp = 0; samp < ns1; samp++)
            {
            if (((*(ftemp+samp)  < *mask) || 
                ( *(ftemp+samp)  > *mask)) &&
                ((*(ftemp1+samp) < *mask) || 
                ( *(ftemp1+samp) > *mask)))			/*ASF*/
	    distance += 1;
	    }
	  rate = 1.0 / distance;
	  mfactor = 1.0;
	  }
            for (samp = 0; samp < ns1; samp++)
               {
               if (( *(ftemp+samp) < *mask) || 
                  (  *(ftemp+samp) > *mask))			/*ASF*/
                  {
	          if ((replace) ||
	              ((leave) && ((*(ftemp1+samp) >= *mask)	
                      && (*(ftemp1+samp) <= *mask))))		/*ASF*/
	                 *(ftemp1+samp) = *(ftemp+samp);
		  else
	          if (aver)
		     {
	              if ((*(ftemp1+samp) >= *mask) &&
	                  (*(ftemp1+samp) <= *mask))		/*ASF*/
	                 *(ftemp1+samp)  = *(ftemp+samp);
		      else
			{
			mfactor -= rate;
			if (laver[curent])
	                    *(ftemp1+samp)  = ((1.0-mfactor) * (*(ftemp1+samp)))
			    + (mfactor * (*(ftemp+samp)));
                        else
	                    *(ftemp1+samp)  = (mfactor * (*(ftemp1+samp)))
			    + ((1.0 - mfactor) * (*(ftemp+samp)));
                        }
		     }
	          }
	       }  /* for samp < ns1  */
	    }
	    if (c_estep(&gdesc) != E_SUCC)
              {
              if (c_egclse(&gdesc) != E_SUCC)
                {
                c_errmsg("Error closing file","concat-close",NON_FATAL);
                c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);
                }
              if (*update)
                 c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
	      /* else c_clean(vblk,"concat",hostout,&one); */  	/*ASF*/
              } 
         }  /* for curent < nl1  */
      break;
   }  /* switch datatype  */

 /* close images
 ---------------*/
 if (c_egclse(&gdesc) != E_SUCC)
   {
   c_errmsg("Error closing file","concat-close",NON_FATAL);
   c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);
   }

 /* free buffer
 --------------*/
 free(thebuf);

 return;
}
