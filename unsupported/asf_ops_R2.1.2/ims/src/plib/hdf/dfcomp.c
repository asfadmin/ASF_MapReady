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

/*-----------------------------------------------------------------------------
 * File:    dfcomp.c
 * Purpose: File compression
 * Invokes: df.c dfimcomp.c df.h
 * Contents:
 *  DFputcomp: compress image and write it to HDF file
 *  DFgetcomp: read compressed image from HDF file and decompress it
 *  DFCrle: compress string using run length encoding
 *  DFCunrle: decompress string using run length encoding
 * Remarks: DFgetcomp and DFputcomp constitute a general compression interface
 *---------------------------------------------------------------------------*/

/* This module (dfcomp.c) is in charge of the general compression information */
#define COMPRESS_MASTER
#include "hdf.h"
#undef COMPRESS_MASTER

#include "herr.h"

#define R8_MAX_BLOCKS 32
#define R8_MAX_LENGTH 512

/*-----------------------------------------------------------------------------
 * Name:    DFputcomp
 * Purpose: Compress and write images to HDF file
 * Inputs:  file_id: pointer to HDF file
 *          tag, ref: tag, ref of compressed image for writing out
 *          image: image to be compressed
 *          xdim, ydim: dimensions of image
 *          palette: palette associated with image
 *          newpal: modified palette, produced if compression scheme is IMCOMP
 *          scheme: compression scheme to be used
 *          cinfo: additional information needed for compression
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF programmers, DF8putrig, other routines
 * Invokes: DFCrle, DFCimcomp, DFaccess, DFwrite, DFIcheck
 * Remarks: IMCOMP modifies the palette associated with the image
 *          Hence the palette and newpal arguments
 *          This is a general compression interface - to be used anytime image
 *          compression is needed in HDF
 *          Note that rseq does its own compression, because that is part of
 *          the interactive color raster protocol
 *          The space needed for compression and decompression can be allocated
 *          statically or dynamically, depending on the DF_DYNAMIC flag, and
 *          for entire image or part of it (reused) depending on availability
 *          Accordingly, writing out is whole image, or row by row
 *          Note that compression is always row by row for RLE.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFputcomp(int32 file_id, uint16 tag, uint16 ref, uint8 *image, int32 xdim,
             int32 ydim, uint8 *palette, uint8 *newpal, int16 scheme,
             comp_info *cinfo)
#else
int DFputcomp(file_id, tag, ref, image, xdim, ydim, palette, newpal, scheme, cinfo)
    int32 file_id;
    uint16 tag, ref;
    uint8 *image;
    int32 xdim, ydim;
    uint8 *palette;
    uint8 *newpal;
    int16 scheme;
    comp_info *cinfo;
#endif
{
    char *FUNC="DFputcomp";
    uint8 *buffer;             /* buffer to hold compressed image */
    uint8 *in;                 /* pointer to input for compression */
    uint8 *out;                /* pointer to space for compressed output */
    int32 cisize;              /* maximum size of compressed image */
    int32 crowsize;            /* maximum size of compressed row */
    intn  buftype;             /* buftype = 1: buffer enough for whole image*/
                               /* buftype = 2: buffer holds 1 row */
    int32 n;                   /* number of compressed bytes produced */
    int32 total;               /* total compressed bytes produced so far */
    int32 i;
    intn ret=0;
    int32 aid;

    if (!HDvalidfid(file_id) || !tag || !ref || xdim <= 0 || ydim <= 0 ||
            !image) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    switch (scheme) {
        case DFTAG_RLE:     /* RLE compression (8-bit or 24-bit(?) images */
            cisize = ydim*(xdim*121/120+1); /* 120 chars can compress to 121! */
            crowsize = xdim*121/120 + 128;

            /* allocate buffer for compression */
            buffer = (uint8 *) HDgetspace((uint32)cisize);
            if (!buffer) {
                buffer = (uint8 *) HDgetspace((uint32)crowsize);
                if (!buffer) {
                    HERROR(DFE_NOSPACE);
                    return FAIL;
                }
                buftype = 2;        /* compress and write out row by row */
            }
            else    /* can hold whole image, then write */
                buftype = 1;       

            in = image;
            out = buffer;
            n = total = 0;          /* no bytes compressed so far */

            if (buftype == 2) {
                int32 num_blocks;
                int32 block_length;

                num_blocks = (ydim > (int32)R8_MAX_BLOCKS) ?
                        (int32)R8_MAX_BLOCKS : ydim;
                block_length = (xdim > (int32)R8_MAX_LENGTH) ?
                        (int32)R8_MAX_LENGTH : xdim;
                aid = HLcreate(file_id, tag, ref, block_length, num_blocks);
                if (aid == FAIL)
                    return FAIL;
            }

       /* compress row by row */
            for (i=0; i<ydim; i++) {
                n = DFCIrle((VOIDP)in, (VOIDP)out, xdim); /* compress row */
                in += xdim;                /* move input pointer */
                total += n;                /* keep running total */
                if (buftype==1)       /* can hold whole image */
                    out = &buffer[total]; /* move out buffer pointer */
                else {                /* buffer too small, */
                                      /* write out what was produced */
                    if (Hwrite(aid, n, buffer) == FAIL) {
                        ret = -1;       /* flag value */
                        break;
                    }
                    out = buffer;   /* reset output pointer */
                }
            }

            if (buftype==1) { /* write out entire image */
                ret = Hputelement(file_id, tag, ref, buffer, total);
                HDfreespace((VOIDP)buffer);
            }
            break;

        case DFTAG_IMC:     /* IMCOMP compression (8-bit images) */
            if (!palette || !newpal) { /* need palette and newpal */
                HERROR(DFE_ARGS);
                return FAIL;
            }
            cisize = xdim*ydim/4;  /* IMCOMP always cuts to 1/4 */

            buffer = (uint8 *) HDgetspace((uint32)cisize);
            if (!buffer) {
                HERROR(DFE_NOSPACE);
                return FAIL;
            }

            DFCIimcomp(xdim, ydim, image, buffer, palette, newpal, 0);
            ret = Hputelement(file_id, tag, ref, buffer, cisize);

            HDfreespace((VOIDP)buffer);
            break;

        case DFTAG_JPEG:        /* JPEG compression (for 24-bit images) */
        case DFTAG_GREYJPEG:    /* JPEG compression (for 8-bit images) */
            ret=DFCIjpeg(file_id,tag,ref,xdim,ydim,(VOIDP)image,scheme,cinfo);
            break;

        default:                   /* unknown compression scheme */
            HERROR(DFE_BADSCHEME);
            return FAIL;
    }
    return(ret);
}   /* end DFputcomp() */

/*-----------------------------------------------------------------------------
 * Name:    DFgetcomp
 * Purpose: Read compressed image and decompress it
 * Inputs:  file_id: HDF file pointer
 *          tag, ref: id of image to be decompressed
 *          image: space to return decompressed image in
 *          xdim, ydim: dimensions of decompressed image
 *          scheme: compression scheme used
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF programmers, DF8getrig, other routines
 * Invokes: DFIcheck, DFIfind, DFaccess, DFread, DFCunrle, DFCunimcomp
 * Remarks: Will use dynamic/static memory allocation for buffer
 *          will read in image in parts if memory insufficient
 *          Decompression of rle is not necessarily row by row
 *          Other encodings can also be decoded with this
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFgetcomp(int32 file_id, uint16 tag, uint16 ref, uint8 *image, int32 xdim,
             int32 ydim, uint16 scheme)
#else
int DFgetcomp(file_id, tag, ref, image, xdim, ydim, scheme)
    int32 file_id;
    uint16 tag, ref;
    uint8 *image;
    int32 xdim, ydim;
    uint16 scheme;
#endif
{
    char *FUNC="DFgetcomp";
    uint8 *buffer;
    uint8 *in;
    uint8 *out;
    int32 cisize, crowsize, buflen, bufleft; /* bufleft: bytes left in buffer*/

    int32 i;
    int32 totalread;
    int32 n;
    int32 aid;

    if (!HDvalidfid(file_id) || !tag || !ref || xdim <= 0 || ydim <= 0
            || !image) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* Only do this stuff for non-JPEG compressed images */
    if(scheme!=DFTAG_JPEG && scheme!=DFTAG_GREYJPEG) {
        aid = Hstartread(file_id, tag, ref);
        if (aid == FAIL) {
            HERROR(DFE_NOMATCH);
            return FAIL;
        }
        if (Hinquire(aid, (int32*)NULL, (uint16*)NULL, (uint16*)NULL, &cisize,
                    (int32*)NULL, (int32*)NULL, (int16*)NULL, (int16*)NULL) == FAIL) {
           return FAIL;
        }
      } /* end if */

    switch (scheme) {
        case DFTAG_RLE:
            crowsize = xdim*121/120 + 128; /* max size of a row */

            buffer = (uint8 *) HDgetspace((uint32)cisize);
            if (!buffer) {
                buffer = (uint8 *) HDgetspace((uint32)crowsize);
                if (!buffer) {
                    HERROR(DFE_NOSPACE);
                    Hendaccess(aid);
                    return FAIL;
                  } /* end if */
                buflen = crowsize;
              } /* end if */
            else
                buflen = cisize;

            in = buffer;
            out = image;
            if ((n=Hread(aid, buflen, in))<0) {
                HDfreespace((VOIDP)buffer);
                Hendaccess(aid);
                return(-1);
              } /* end if */
            totalread = n;
            bufleft = n;
            for (i=0; i<ydim; i++) {
                n = DFCIunrle(in, out, xdim, !i); /* no of bytes used up */
                /* last arg=TRUE if i=0 - resets decompress */
                in += n;
                out += xdim;
                bufleft -= n;
                /* check if more bytes may be needed for next read */
                if ((bufleft<crowsize) && (totalread<cisize)) {
                    HDmemcpy(buffer, in, (size_t)bufleft);
                    in = buffer;
                    if ((n=Hread(aid,buflen-bufleft,(uint8 *)&in[bufleft]))<0) {
                        HDfreespace((VOIDP)buffer);
                        Hendaccess(aid);
                        return FAIL;
                      } /* end if */
                    totalread += n;
                    bufleft += n;
                  } /* end if */
              } /* end for */

            Hendaccess(aid);
            HDfreespace((VOIDP)buffer);
            break;

        case DFTAG_IMC:
            crowsize = xdim;        /* size of compressed row */

            buffer = (uint8 *) HDgetspace((uint32)cisize);
            if (!buffer) {
                buffer = (uint8 *) HDgetspace((uint32)crowsize);
                if (!buffer) {
                    HERROR(DFE_NOSPACE);
                    Hendaccess(aid);
                    return FAIL;
                  } /* end if */
                buflen = crowsize;
              } /* end if */
            else
                buflen = cisize;
            if (buflen>=cisize) {
                if (Hread(aid, cisize, buffer) < cisize) {
                    HDfreespace((VOIDP)buffer);
                    Hendaccess(aid);
                    return FAIL;
                  } /* end if */
           /* HDfreespace(buffer); */
                Hendaccess(aid);
                DFCIunimcomp(xdim, ydim, buffer, image);
                HDfreespace((VOIDP)buffer);
                break;              /* go to end of switch */
              } /* end if */

            in = buffer;            /* if can only read piecemeal */
            out = image;
            if ((n=Hread(aid, buflen, in))<0) {
                HDfreespace((VOIDP)buffer);
                Hendaccess(aid);
                return FAIL;
              } /* end if */
            totalread = n;
            bufleft = n;
            for (i=0; i<ydim; i+=4) {
                DFCIunimcomp(xdim, (int32)4, in, out);
                in += xdim;
                out += 4*xdim;
                bufleft -= xdim;
                if ((bufleft<crowsize) && (totalread<cisize)) {
                    HDmemcpy(buffer, in, (size_t)bufleft);
                    in = buffer;
                    if ((n=Hread(aid,buflen-bufleft,(uint8 *)&in[bufleft]))<0) {
                        HDfreespace((VOIDP)buffer);
                        Hendaccess(aid);
                        return FAIL;
                      } /* end if */
                    totalread += n;
                    bufleft += n;
                  } /* end if */
              } /* end for */

            HDfreespace((VOIDP)buffer);
            Hendaccess(aid);
            break;

        case DFTAG_JPEG:
        case DFTAG_GREYJPEG:
            if(DFCIunjpeg(file_id, tag, ref, (VOIDP)image, xdim, ydim, scheme)==FAIL)
                return(FAIL);
            break;

        default:                 /* unknown scheme */
            HERROR(DFE_ARGS);
            return FAIL;
      } /* end switch */

    return SUCCEED;
}   /* end DFgetcomp() */

