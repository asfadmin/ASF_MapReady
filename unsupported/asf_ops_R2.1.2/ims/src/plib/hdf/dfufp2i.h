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

#ifndef DFUFP2IM_H /* avoid re-inclusion */
#define DFUFP2IM_H

/*
* definitions of structs used by routines: Input & Output
*/

struct Input {
    int32
        hdim, vdim;     /* horizontal and vertical dimensions of input data */
    int
        is_pal,         /* flag to tell whether there is a palette */
        is_vscale,      /* flags telling whether scales were included  */
        is_hscale,
        ct_method;      /* color transform method: EXPAND or INTERP */
    float32
        max, min,        /* max and min values of the data */
        *hscale,*vscale, /* horizontal and vertical scales */
        *data;           /* floating point data */
};

struct Output {
    int32
        hres,vres;  /* horizontal and vertical resolution of image */
    int compress;   /* compression scheme */
    char
        outfile[32]; /* output file name */
    uint8
        *image;     
    uint8
        *palette;   
};

/*----------------------------------------------------------------------------*/
/*                           Function Prototypes                              */

#if defined c_plusplus || defined __cplusplus
extern "C" {
#endif /* c_plusplus || __cplusplus */

/* prototypes for dfufp2im.c */

#ifdef OLD_WAY
extern int duif2i_(int32 *hdim, int32 *vdim, float32 *max, float32 *min,
		   float32 hscale[], float32 vscale[], float32 data[],
           uint8 *palette, _fcd outfile, int *ct_method, int32 *hres,
           int32 *vres, int *compress, int *lenfn);
extern int DFUfptoimage(int32 hdim, int32 vdim, float32 max, float32 min,
                        float32 *hscale, float32 *vscale, float32 *data,
                        uint8 *palette, char *outfile, int ct_method,
                        int32 hres, int32 vres, int compress);
#endif
extern int process
    PROTO((struct Input *in, struct Output *out));
extern int generate_scale
    PROTO((int32 dim, float32 *scale));
extern int convert_interp
    PROTO((struct Input *in, struct Output *out));
extern int pixrep_scaled
    PROTO((struct Input *in, struct Output *out));
extern int compute_offsets
    PROTO((float32 *scale, int32 dim, int32 *offsets, int32 res));
extern int pixrep_simple
    PROTO((struct Input *in, struct Output *out));

#if defined c_plusplus || defined __cplusplus
}
#endif /* c_plusplus || __cplusplus */

#endif /* DFUFP2IM_H */
