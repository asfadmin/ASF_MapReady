/***************************************************************************
*
*
*                         NCSA HDF version 3.3r1
*                            September 20, 1993
*
* NCSA HDF Version 3.3 source code and documentation are in the public
* domain.  Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
*
* We ask, but do not require, that the following message be included in all
* derived works:
*
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign, in collaboration with the
* Information Technology Institute of Singapore.
*
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
*
*****************************************************************************/

/*-----------------------------------------------------------------------------c
 *    dfufp2i.c
 *
 * Purpose:  Utility function to convert floating point data to 8-bit 
 *           raster image set (RIS8) format, storing the results in 
 *           an hdf file.
 *                                   -----------
 *                                  |           | ----------> RIS8
 *       floating point data   ---> | fp_to_hdf |   and/or
 *         (in an array)            |           | ----------> SDS
 *                                   -----------
 *
 * Invokes:  libdf.a
 *
 * Includes: stdio.h, ctype.h, "df.h"
 *
 * Public function:
 *      DFUfptoimage: sets up structs with input params, calls process()
 * 
 * Private functions:
 *      process: main driver routine: transforms the data to an image and 
 *               stores it in the file
 *      generate_scale: generates a scale, if none provided
 *      convert_interp: creates an interpolated image
 *      pixrep_scaled: creates an expanded image using scales provided
 *      compute_offsets: called by pixrep_scaled 
 *      pixrep_simple: creates an expanded image assuming equal gaps in scales
 *
 * Fortran stub function:
 *      duif2i - intermediate, called by fortran functions in DFUfptoimage.f
 *
 * Remarks:
 *      This routine is very similar to the utility fp_to_hdf, which
 *      takes its input from one or more files, rather than from internal
 *      memory. 
 *      Another difference is that this routine allows compression (run
 *      length encoding), whereas fp_to_hdf does not at present (8/31/89).
 *      Since this routine is meant to mimic many of the features of
 *      NCSA DataScope, much of the code has been taken directly from
 *      the DataScope source.
 *
 *  National Center for Supercomputing Applications
 *  University of Illinois, Urbana-Champaign
 *
 *  by Mike Folk (mfolk@ncsa.uiuc.edu)
 *  Beta version: 9/1/89
 *  Released:     6/5/90
 *
 *  This program is in the public domain
 *
 *--------------------------------------------------------------------------*/

#include <ctype.h>
#include "hdf.h"
#include "dfufp2i.h"
#include "dfsd.h"

/**********************************************************************
*
*  Header information
*
***********************************************************************/

/*
*  global definitions
*/

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE    1
#endif /* TRUE */
#define EXPAND  1   /* -e: expand image with pixel replication */
#define INTERP  2   /* -i: expand image with bilinear interpolation */


#ifndef DFUFP2I_FNAMES
#   define DFUFP2I_FNAMES
#ifdef DF_CAPFNAMES
#   define nduif2i       FNAME(DUIF2I)
#else
#   define nduif2i       FNAME(duif2i)
#endif /* DF_CAPFNAMES */
#endif /* DFUFP2I_FNAMES */

/*-----------------------------------------------------------------------------
 * Name:    duif2i
 * Purpose: Intermediate Fortran callable version of DFUfptoimage
 *          (See DFUfptoimage for details)
 *
 * Invokes: DFUfptoimage
 *---------------------------------------------------------------------------*/

    FRETVAL(int)
#ifdef PROTOTYPE
nduif2i(int32 *hdim, int32 *vdim, float32 *max, float32 *min, float32 hscale[],
	float32 vscale[], float32 data[], uint8 *palette, _fcd outfile,
    int *ct_method, int32 *hres, int32 *vres, int *compress, int *lenfn)
#else
nduif2i(hdim,vdim,max,min,hscale,vscale,data,palette,
                                   outfile,ct_method,hres,vres,compress,lenfn)

    int32 *hdim, *vdim; /* horizontal and vertical dimensions of input data */
    float32 *max, *min,/* maximum and minimum values in the data */
            hscale[]   /* horizontal and vertical scales */,
            vscale[],
            data[];    /* input data */
    uint8 *palette;    /* palette to be stored with the image */
    _fcd outfile;      /* name of file to store image in */
    int  *ct_method;   /* color transform method: 1=EXPAND; 2=INTERP */
    int32 *hres, *vres; /* resolutions desired for output image      */
    int *compress,      /* flag: 0 = don't compress; 1=do compression */
         *lenfn;        /* length of outfile string */
#endif /* PROTOTYPE */
{
    char *fn;
    int  ret;
    
    fn = HDf2cstring(outfile, *lenfn);
    ret = DFUfptoimage( *hdim, *vdim, *max, *min, hscale, vscale, data,
            _fcdtocp(palette), fn, *ct_method, *hres, *vres, *compress );
    HDfreespace(fn);
    return(ret);
}


/*-----------------------------------------------------------------------------s
 * DFUfptoimage()
 *  
 * Purpose:sets up structs with input params, calls process()
 * Inputs:
 *     hdim, vdim: horizontal and vertical dimensions of input data
 *     max, min:   maximum and minimum values in the data
 *     hscale,vscale: optional horizontal and vertical scales
 *     data:       input data
 *     palette:    optional palette to be stored with the image
 *     outfile:n   name of hdf file to store image in 
 *     ct_method:  color transform method: 1=EXPAND; 2=INTERP
 *     hres, vres: resolutions desired for output image
 *     compress:   compression flag: 0=don't; 1=do
 * Returns: 0 on success, -1 on failure with error set
 * Users:       HDF HLL (high-level library) users, utilities, other routines
 * Invokes: process
 * Remarks: none
 *----------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFUfptoimage(int32 hdim, int32 vdim, float32 max, float32 min,
        float32 *hscale, float32 *vscale, float32 *data, uint8 *palette,
        char *outfile, int ct_method, int32 hres, int32 vres, int compress)
#else
int DFUfptoimage(hdim,vdim,max,min,hscale,vscale,data,palette,
                                   outfile,ct_method,hres,vres,compress)

int32   hdim, vdim,      /* horizontal and vertical dimensions of input data */
        hres, vres;      /* resolutions desired for output image      */
float32 max, min,        /* maximum and minimum values in the data */
        *hscale,*vscale, /* horizontal and vertical scales */
        *data;           /* input data */
uint8   *palette;        /* palette to be stored with the image */
char    *outfile;        /* name of file to store image in */
int     ct_method,       /* color transform method: 1=EXPAND; 2=INTERP */
        compress;        /* flag: 0 = don't compress; 1=do compression */
#endif /* PROTOTYPE */
{
    struct Input in;
    struct Output out;

    in.hdim = hdim;
    in.vdim = vdim;
    in.max = max;
    in.min = min;
    in.is_hscale = (hscale == NULL) ? FALSE : TRUE;
    in.is_vscale = (vscale == NULL) ? FALSE : TRUE;
    in.hscale = hscale;
    in.vscale = vscale;
    in.data = data;
    in.is_pal = (palette == NULL) ? FALSE : TRUE;
    in.ct_method = ct_method;
    HDstrcpy(out.outfile, outfile);  /* get outfile name */
    out.palette = palette;         /* get palette address (may be NULL) */
    out.hres = hres;
    out.vres = vres;
    out.compress = compress ? 11 : 0; /* 0=>don't; 11=>RLE compression */

    /* tloc1 = time((long *) 0); */  /* these 4 lines for debugging */
    process(&in,&out);
    /* tloc2 = time((long *) 0); */
    /* printf("Time:    %ld\n",tloc2-tloc1); */
    return 0;
} /* end of DFUfptoimage */

/*-----------------------------------------------------------------------------s
 * process
 *
 * Purpose:   to transform the data to an image and stores it in the file
 * Inputs:
 *     in:   structure with information about data to be converted to image
 *     out:  structure with information about image
 * Returns: 0 on success, -1 on failure with error set
 * Users:    DFUfptoimage
 * Invokes: from libdf.a: DFR8setpalette, Hopen, Hclose, DFR8addimage
 *          local: generate_scale, pixrep_scaled, pixrep_simple,convert_interp 
 * Remarks: none
 *----------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int process(struct Input *in, struct Output *out)
#else
int process(in, out)
struct Input *in;
struct Output *out;
#endif /* PROTOTYPE */
{
    int ret;
    int32   file_id;
        
/*    printinput(in);*/   /* for debugging */

    if (in->is_pal) {
        ret = DFR8setpalette((uint8*)out->palette);  /* output as HDF palette */
        if (ret < 0) return ret;
    }

    file_id = Hopen(out->outfile, DFACC_WRITE, 0);
    Hclose(file_id);

    /*
    *  allocate buffers for output and scales 
    */
    if (!in->is_hscale)
        in->hscale = (float32 *) HDgetspace((uint32)(1+in->hdim)*sizeof(float32));
    if (!in->is_vscale)
        in->vscale = (float32 *) HDgetspace((uint32)(1+in->vdim)*sizeof(float32));
    out->hres = (out->hres <= in->hdim) ? in->hdim : out->hres ;
    out->vres = (out->vres <= in->vdim) ? in->vdim : out->vres ;
    out->image = (uint8 *) HDgetspace((uint32)out->hres * out->vres);

    /*
    *  if necessary, generate x and y scales
    */
    if (!in->is_hscale)
        generate_scale(in->hdim, in->hscale);
    if (!in->is_vscale)
        generate_scale(in->vdim, in->vscale);
    /*
    *  output raster hdf file 
    */
    if (in->ct_method == EXPAND) {
        if (in->is_hscale || in->is_vscale)
            pixrep_scaled(in, out);
        else
            pixrep_simple(in, out);
    } else
        convert_interp(in, out);

/*    printoutput(out);*/   /* for debugging */

    ret = DFR8addimage(out->outfile, (char *)out->image,
                             out->hres,out->vres,(int16)out->compress);
    if (ret < 0) return ret;
    /*
    *  free allocated space
    */
    if (!in->is_hscale) HDfreespace((char *)in->hscale);
    if (!in->is_vscale) HDfreespace((char *)in->vscale);
    HDfreespace((char *)out->image);
    return 0;
} /* end of process */


/*-----------------------------------------------------------------------------
 * generate_scale
 *
 * Purpose:   to generate the scale 1 2 3 ... dim
 * Input: 
 *     dim:   length of scale 
 * Output:
 *     scale: array of floating point numbers from 1 to dim
 * Returns: 0 on success, -1 on failure with error set
 * Users:    process
 * Invokes: none
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int generate_scale(int32 dim, float32 *scale)
#else
int generate_scale(dim, scale)
int32 dim;
float32 *scale;
#endif /* PROTOTYPE */
{
    int32 i;

    for (i=0; i <= dim; i++)
        *scale++ = (float32) i;
    return 0;
}


/*-----------------------------------------------------------------------------
 * printinput
 *
 * Purpose:   debugging: prints input values to stdout
 * Input: 
 *     in:    struct with all input values
 * Returns: 0 on success, -1 on failure with error set
 * Users:    process and other local routines
 * Invokes: none
 * Remarks: none
 *---------------------------------------------------------------------------*/
/*  This function is commented out of the code!! */
#ifdef DEBUG_HDF
#ifdef PROTOTYPE
int printinput(struct Input *in)
#else
int printinput(in)
struct Input *in;
#endif /* PROTOTYPE */
{
    int i,j; 

    printf("\nmax: %8.2f   min: %8.2f\n", in->max, in->min);

    printf("There %s a palette\n", in->is_pal ? "IS" : "is NOT");
    printf("color tranform method = %s\n",
                           (in->ct_method==EXPAND)?"expand":"interpolate");
    if ( in->hscale != NULL) {
        printf("\nHorizontal scale:\n");
        for (i=0; i<(int)(in->hdim); i++)
          printf("%8.2f",in->hscale[i]);
    }
    else
        printf("\nNo horizontal scale\n");

    if ( in->vscale != NULL) {
        printf("\nVertical scale:\n");
        for (i=0; i<(int)(in->vdim); i++)
          printf("%8.2f",in->vscale[i]);
    }
    else
        printf("\nNo vertical scale.\n");

    printf("\n");
    printf("Data:");
    for(i=0;i<(int)(in->vdim) && i<11; i++) {
        printf("\n");
        for (j=0; j< (int)(in->hdim); j++)
            printf("%6.1f ",in->data[i*in->hdim+j]);
    }
    printf("\n");
    return 0;
} /* end of print_input */
#endif  /* DEBUG_HDF */

/*-----------------------------------------------------------------------------
 * printoutput
 *
 * Purpose:   debugging: prints input values to stdout
 * Input: 
 *     out:    struct with all output values
 * Returns: 0 on success, -1 on failure with error set
 * Users:    process and other local routines
 * Invokes: none
 * Remarks: none
 *---------------------------------------------------------------------------*/
/*  This function is commented out of the code!! */
#ifdef DEBUG_HDF
#ifdef PROTOTYPE
int printoutput(struct Output *out)
#else
int printoutput(out)
struct Output *out;
#endif /* PROTOTYPE */
{
    int i,j;
    printf("\n");
      for(i=0;i<(int)(out->vres) && i<20; i++) {
          printf("\n");
          for (j=0; j< (int)(out->hres); j++)
             if (j<19)
                printf("%4d",(uint8) out->image[i*out->hres + j]);
      }
     printf("\n");
    return 0;
} /* end of printoutput */
#endif  /* DEBUG_HDF */



/***************************************************************************
*
*  Next comes the routine for performing bilinear interpolation
*
****************************************************************************/


/*-----------------------------------------------------------------------------
 * convert_interp
 *
 * Purpose:   Create an interpolated image from the data array
 * Input: 
 *     in:    struct with all input values
 *     out:   struct with all output values
 * Returns: 0 on success, -1 on failure with error set
 * Users:   process
 * Invokes: none
 * Remarks: Uses a bilinear interpolation method to fill in the picture.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int convert_interp(struct Input *in, struct Output *out)
#else
int convert_interp(in, out)
struct Input   *in;
struct Output  *out;
#endif /* PROTOTYPE */
{
    register int j,theval;
    float32 *f,*dxs,*dys,*xv,*yv,*lim,delx,dely,pt,xrange,yrange,range,zy,
        *z1,*z2,*z3,*z4,z;
    register uint8 *p;
    uint8 *xinc;
    int32 i,*yoffs;

    p = (uint8 *) out->image; /* space for interpolated image */

    range = in->max - in->min;
    xrange = *(in->hscale + in->hdim-1) - *in->hscale;
    yrange = *(in->vscale + in->vdim-1) - *in->vscale;
    delx = xrange / out->hres;              /* x axis increment in image */
    dely = yrange / out->vres;              /* y axis increment in image */
    
    dxs = (float32 *)HDgetspace((uint32)sizeof(float32)*out->hres);
                                            /* temp space for dx's */
    dys = (float32 *)HDgetspace((uint32)sizeof(float32)*out->vres);
                                            /* temp space for dy's */
    xinc = (uint8 *)HDgetspace((uint32)out->hres);
    yoffs = (int32 *)HDgetspace((uint32)(out->vres+1)*sizeof(int32));
    yoffs[0] = 0;
    
    if (range < 0) range = -range;          /* max must be > min */
    
    f = dys;                                /* beginning of dys to fill in */
    yv = in->vscale;                        /* beginning and end of yvals */
    lim = in->vscale + in->vdim-2;
    
    if (yrange > 0) {
        for (i=0; i<out->vres; i++) {        /* fill in dy's */
            pt = dely*(float32)i + *in->vscale;/* scaled pos in new image */
            
            while (*(yv+1) < pt && yv < lim) {        /* move y pointer */
                yv++; yoffs[i]++;
            }
            *f++ = (*(yv+1) - pt)/(*(yv+1) - *yv);/* calc dy pcnt and put in */
            
            yoffs[i+1] = yoffs[i];
        }
    } 
    else {                                        /* decrementing instead */
        yrange = -yrange;    
        
        for (i=0; i<out->vres; i++) {        /* fill in dy's */
            pt = dely*(float32)i + *in->vscale;

            while (*(yv+1) > pt && yv < lim) {    /* move y pointer */
                yv++; yoffs[i]++;
            }
            *f++ = -(*(yv+1) - pt)/(*(yv+1) - *yv);/* calc dy pcnt and put in */
            
            yoffs[i+1] = yoffs[i];
        }
    }

    f = dxs;                            /* beginning of dxs to fill in */
    xv = in->hscale;                    /* beginning and end of xvals */
    lim = in->hscale + in->hdim-2;
    
    if (xrange > 0) {
        for (i=0; i<out->hres; i++) {        /* fill in dx's */
            pt = delx*(float32)i + *in->hscale;
            xinc[i] = 0;
            
            while (*(xv+1) < pt && xv < lim) {    /* move xv pointer */
                xv++;  xinc[i]++;
            }
            *f++ = (*(xv+1) - pt)/(*(xv+1) - *xv);/* calc dy prct and put in */
        }
    } 
    else {                                        /* decrementing instead */
        xrange = -xrange;    
        
        for (i=0; i<out->hres; i++) {        /* fill in dx's */
            pt = delx*(float32)i + *in->hscale;
            xinc[i] = 0;
    
            while (*(xv+1) > pt && xv < lim) {    /* move y pointer */
                xv++;  xinc[i]++;
            }
            *f++ = -(*(xv+1) - pt)/(*(xv+1) - *xv);/* calc dy pcnt and put in */
        }
    }

/*
*   Do the interpolation for each point in the target image.
*   We take advantage of the fact that we know the target is evenly spaced 
*   along both axes.
*/
    yv = dys;
    
    for (i= 0; i < out->vres; i++,yv++) {
        
        z1 = in->data + in->hdim*(yoffs[i]);
        z2 = z1 + 1;
        z3 = z1 + in->hdim;
        z4 = z3 + 1;
    
        xv = dxs;
        zy = *yv;
        
        for (j=0; j < (int)(out->hres); j++,xv++) {    /* for each target point */
            
            z1 += xinc[j];         /* xinc == 0 when we don't need to shift */
            z2 += xinc[j];
            z3 += xinc[j];
            z4 += xinc[j];
            
            z = (*z1 - *z3 - *z2 + *z4)*(*xv)*zy +        /* weighted sum */
                (*z3 - *z4)*(*xv) + (*z2 - *z4)*zy + *z4;
                
            theval = (int)(1.0 + 237.9*(z - in->min)/range);/* scaled value  */
            if (theval >= 240 || theval < 1)
                *p++ = 0;
            else
                *p++ = (uint8)theval;
        }
    }
    HDfreespace( (char *) dxs);
    HDfreespace( (char *) dys);
    HDfreespace( (char *) xinc);
    HDfreespace( (char *) yoffs);
    return 0;
} /* end of convert_interp */


/****************************************************************************
*
*  Next come the routines for pixel replication
*
* 
*  Two routines to create expanded image via pixel replication
*
*  pixrep_scaled replicates pixels according to given scales
*  pixrep_simple replicates the same number of pixels for each point
*
******************************************************************************/

/*-----------------------------------------------------------------------------
 * pixrep_scaled
 *
 * Purpose:   Create an expanded image from the data array
 * Input: 
 *     in:    struct with all input values
 *     out:   struct with all output values
 * Returns: 0 on success, -1 on failure with error set
 * Users:   process
 * Invokes: compute_offsets
 * Remarks: Uses pixel replication to fill in the picture. Replicates
 *          pixels according to in->vscale and in->hscale
 *---------------------------------------------------------------------------*/

#ifdef  PROTOTYPE
int pixrep_scaled(struct Input *in, struct Output *out)
#else
int pixrep_scaled(in, out)
struct Input   *in;
struct Output  *out;
#endif /* PROTOTYPE */
{
    register int32 j;
    float *data, range, ratio;
    register uint8 *image, *prevrow;
    uint8 *pixvals;
    int32 i, theval, *hoffsets, *voffsets, prevoffset;

    data = in->data;                          /* space for data */
    image = (uint8 *) out->image;  /* space for image */

    range = in->max - in->min;
    if (range < 0) range = -range;            /* max must be > min */
    
    hoffsets = (int32 *)HDgetspace((uint32)(out->hres+1)*sizeof(int32));
    voffsets = (int32 *)HDgetspace((uint32)(out->vres+1)*sizeof(int32));
    pixvals = (uint8 *)HDgetspace((uint32)in->hdim+1);
    compute_offsets(in->hscale,in->hdim,hoffsets,out->hres);
    compute_offsets(in->vscale,in->vdim,voffsets,out->vres);
    
    prevoffset = voffsets[0] - 1;
    ratio = (float32)237.9 / range;

    for (i=0; i<out->vres; i++) {   /* for each row, store pixel vals */

        if (voffsets[i] > prevoffset) {  /* if new data row, compute pix vals */

            for (j=0; j<in->hdim; j++) {  /* compute vals for each data point*/
                theval = (int) (1.5 + ratio*(*data++ - in->min));
                if (theval >= 240 || theval < 1) theval = 0;
                pixvals[j] = (uint8) theval;
            }

            for (j=0; j<out->hres; j++)       /* put row of pix vals into */
                *image++ = pixvals[hoffsets[j]]; /* next row of image */
        } 

        else {                         /* else repeating a previous row */
            prevrow = image - out->hres;
            for (j=0; j<out->hres; j++)   /* put previous  row of pix vals*/
                *image++ = *prevrow++;       /* into next row of image */
        }
        prevoffset = voffsets[i];
    }
    HDfreespace( (char *) hoffsets);
    HDfreespace( (char *) voffsets);
    HDfreespace( (char *) pixvals);
    return 0;
} /* end of pixrep_scaled */
            

/*-----------------------------------------------------------------------------
 * compute_offsets
 *
 * Purpose:  For each pixel position on the horizontal or vertical 
 *           dimension, compute the offet of the corresponding value 
 *           in the scale array.
 * Input: 
 *     scale: the scale
 *     dim:   length of scale
 *     res:   resolution: length of the array 'offsets'
 * Output:
 *     offsets: the set of offsets that were computed
 *     out:   struct with all output values
 * Returns: 0 on success, -1 on failure with error set
 * Users:   pixrep_scaled
 * Invokes: compute_offsets
 * Remarks: The array 'offsets' can be used to determine which scaled 
 *          pixel value to place in the final picture.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int compute_offsets(float32 *scale, int32 dim, int32 *offsets, int32 res)
#else
int compute_offsets(scale, dim, offsets, res)
float32 *scale;
int32 dim,         /* dimension of scale */ 
      res;         /* resolution: length of array 'offsets' */
int32 *offsets;
#endif /* PROTOTYPE */
{
    int32 i,j;
    float32 *midpt, pt, delta;
 
    midpt = (float32 *) HDgetspace((uint32)sizeof(float32)*dim);

    for (i=0; i < dim-1; i++) {                  /* compute all midpoints */
        midpt[i] = (scale[i] + scale[i+1])/(float32)2.0;
/*        printf("midpt[%d]=%8.1f\tscale[%d]=%8.1f\n",i,midpt[i],i,scale[i]);*/
    }
    midpt[i] = scale[i] + scale[i] - midpt[i-1]; /* tack one onto end */

    delta = (*(scale+dim-1) - *scale)/(res-1); /* amt of change along scale */
                                               /* per pixel position */
    offsets[0] = 0;
    pt = *scale;          /* base point has value of 1st scale item */

    for (i=1, j=0; i < res; i++) {  /* compute & store offsets of pix vals */
        pt += delta;
        offsets[i] = offsets[i-1];  /* keep offsets same until past midpt */
        while (pt >= midpt[j]) {
            offsets[i]++;
            j++;
        }
    }
    HDfreespace( (char *) midpt);
    return 0;
} /* end of compute_offsets */
    

/*-----------------------------------------------------------------------------
 * pixrep_simple
 *
 * Purpose:   Create an expanded image from the data array
 * Input: 
 *     in:    struct with all input values
 *     out:   struct with all output values
 * Returns: 0 on success, -1 on failure with error set
 * Users:   process
 * Invokes: compute_offsets
 * Remarks: Uses pixel replication to fill in the picture. Replicates
 *          the same number of pixels for each point
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int pixrep_simple(struct Input *in, struct Output *out)
#else
int pixrep_simple(in, out)
struct Input   *in;
struct Output  *out;
#endif /* PROTOTYPE */
{
    int32 i,j;
    uint8 raster_val;
    register uint8 *image, *row_buf;
    float32 *in_row_ptr, *in_buf;
    float32 ratio, delh, delv, hblockend, vblockend;

    ratio = (float32)237.9 / (in->max - in->min);
    image = (uint8 *) out->image;
    in_buf = in->data;

    delh = ( (float32) out->hres) / in->hdim;  /* horiz block size */
    delv = ( (float32) out->vres) / in->vdim;  /* vert block size  */

/*
* Compute expanded image
* Do it a vertical block at a time
* (Note the trick with the counters i and j vis-a-vis the blockends.)
*/
    vblockend = delv;
    for(i=0; i<out->vres; i++, vblockend += delv) { 
        in_row_ptr = in_buf;          
        row_buf = image;              /* start of next NEW row of output */

        /* compute raster values for this row */
        hblockend = delh;
        for(j=0; j<out->hres; j++, hblockend += delh) { 

            raster_val = (uint8)
                             (1.5 + ratio * (float32) (*in_row_ptr++-in->min));
            *image++ = raster_val;

            for (; j<(int32)hblockend-1; j++) /* store vals for this blk of this row*/
                *image++ = raster_val;
        }

        /* repeat same row for whole vertical block*/
        for(; i<(int32)vblockend-1; i++)
            for (j=0; j<out->hres; j++)
                *image++ = row_buf[j];
         
        in_buf += in->hdim;            /* move to next row in input array */
    }
    return 0;
} /* end of pixrep_simple() */

