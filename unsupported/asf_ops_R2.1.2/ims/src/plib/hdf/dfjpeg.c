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
 * File:    dfjpeg.c
 * Purpose: JPEG image compression algorithm
 * Invokes: JPEG library functions
 * Contents:
 *  DFCIjpeg: compress image using JPEG compression
 * Remarks: DFCIjpeg() compress images using the JPEG library functions.
 *      This file (dfjpeg.c) and dfunjpeg.c should remain the only HDF files
 *      that has to know about how to use the JPEG routines.
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "herr.h"
#include "jinclude.h"

/* Static variables for JPEG compression (eventually these need to be stored */
/* in the JPEG structure to hand around (to allow the routines to be */
/* re-entrant)) */
PRIVATE int32 img_file_id;  /* File ID for the HDF file */
PRIVATE uint16 img_tag;     /* tag number of the image to write out */
PRIVATE uint16 img_ref;     /* reference number of the image to write out */
PRIVATE int32 jdata_aid;    /* AID for writing out chunks of the image */
PRIVATE int32 img_xdim,     /* X and Y dimensions of the image to compress */
    img_ydim;
PRIVATE int32 byte_count;   /* count of bytes emitted with emite_byte() */
PRIVATE uint8 *img_ptr;     /* Pointer to the image to compress */
PRIVATE intn img_scheme;    /* What type of image comp. are we doing? 24 or 8 bit */

/****************************************************************************/
/* Routines for taking JPEG information and writing it back to the HDF file */
/****************************************************************************/

/*
 * To output to something other than a stdio stream, you'd need to redefine
 * these macros.
 */

/* Write a single byte */
#ifdef OLD_WAY
#define emit_byte(cinfo,x)  putc((x), cinfo->output_file)
#else
#define emit_byte(aid,x)    do {uint8 c=(uint8)x; Hwrite((int32)aid,(int32)1,&c); \
    byte_count++;} while(0)
#endif

#ifdef OLD_WAY
/* Write some bytes from a (char *) buffer */
#define WRITE_BYTES(cinfo,dataptr,datacount)  \
  { if (JFWRITE(cinfo->output_file, dataptr, datacount) \
	!= (size_t) (datacount)) \
      ERREXIT(cinfo->emethods, "Output file write error"); }
#else
/* Write some bytes from a (char *) buffer */
#define WRITE_BYTES(cinfo,dataptr,datacount)  \
  { if (Hwrite(jdata_aid , datacount, dataptr) != (int32) (datacount)) \
      ERREXIT(cinfo->emethods, "Output file write error"); }
#endif

/* Clean up and verify successful output */
#define CHECK_OUTPUT(cinfo)  \
  { fflush(cinfo->output_file); \
    if (ferror(cinfo->output_file)) \
      ERREXIT(cinfo->emethods, "Output file write error"); }


/* End of stdio-specific code. */


typedef enum {			/* JPEG marker codes */
  M_SOF0  = 0xc0,
  M_SOF1  = 0xc1,
  M_SOF2  = 0xc2,
  M_SOF3  = 0xc3,
  
  M_SOF5  = 0xc5,
  M_SOF6  = 0xc6,
  M_SOF7  = 0xc7,
  
  M_JPG   = 0xc8,
  M_SOF9  = 0xc9,
  M_SOF10 = 0xca,
  M_SOF11 = 0xcb,
  
  M_SOF13 = 0xcd,
  M_SOF14 = 0xce,
  M_SOF15 = 0xcf,
  
  M_DHT   = 0xc4,
  
  M_DAC   = 0xcc,
  
  M_RST0  = 0xd0,
  M_RST1  = 0xd1,
  M_RST2  = 0xd2,
  M_RST3  = 0xd3,
  M_RST4  = 0xd4,
  M_RST5  = 0xd5,
  M_RST6  = 0xd6,
  M_RST7  = 0xd7,
  
  M_SOI   = 0xd8,
  M_EOI   = 0xd9,
  M_SOS   = 0xda,
  M_DQT   = 0xdb,
  M_DNL   = 0xdc,
  M_DRI   = 0xdd,
  M_DHP   = 0xde,
  M_EXP   = 0xdf,
  
  M_APP0  = 0xe0,
  M_APP15 = 0xef,
  
  M_JPG0  = 0xf0,
  M_JPG13 = 0xfd,
  M_COM   = 0xfe,
  
  M_TEM   = 0x01,
  
  M_ERROR = 0x100
} JPEG_MARKER;


#ifdef PROTOTYPE
VOID emit_marker (int32 aid, JPEG_MARKER mark)
#else
VOID emit_marker (aid, mark)
int32 aid;
JPEG_MARKER mark;
#endif
/* Emit a marker code */
{
  emit_byte(aid, 0xFF);
  emit_byte(aid, mark);
}


#ifdef PROTOTYPE
LOCAL VOID emit_2bytes (int32 aid, int value)
#else
LOCAL VOID emit_2bytes (aid, value)
int32 aid;
int value;
#endif
/* Emit a 2-byte integer; these are always MSB first in JPEG files */
{
  emit_byte(aid, (value >> 8) & 0xFF);
  emit_byte(aid, value & 0xFF);
}


#ifdef PROTOTYPE
LOCAL int emit_dqt (compress_info_ptr cinfo, int32 aid, int index)
#else
LOCAL int emit_dqt (cinfo, aid, index)
compress_info_ptr cinfo;
int32 aid;
int index;
#endif
/* Emit a DQT marker */
/* Returns the precision used (0 = 8bits, 1 = 16bits) for baseline checking */
{
  QUANT_TBL_PTR data = cinfo->quant_tbl_ptrs[index];
  int prec = 0;
  int i;
  
  for (i = 0; i < DCTSIZE2; i++) {
    if (data[i] > 255)
      prec = 1;
  }

  emit_marker(aid, M_DQT);    /* 2 */
  
  emit_2bytes(aid, prec ? DCTSIZE2*2 + 1 + 2 : DCTSIZE2 + 1 + 2);     /* 2 */

  emit_byte(aid, index + (prec<<4));  /* 1 */
  
  for (i = 0; i < DCTSIZE2; i++) {      /* prec *64 */
    if (prec)
      emit_byte(aid, data[i] >> 8);
    emit_byte(aid, data[i] & 0xFF);
  }

  return prec;
}   /* 133 bytes written */


#ifdef PROTOTYPE
LOCAL VOID emit_dht (compress_info_ptr cinfo, int index, bool is_ac)
#else
LOCAL VOID emit_dht (cinfo, index, is_ac)
compress_info_ptr cinfo;
int index;
bool is_ac;
#endif
/* Emit a DHT marker */
{
  HUFF_TBL * htbl;
  int length, i;
  
  if (is_ac) {
    htbl = cinfo->ac_huff_tbl_ptrs[index];
    index += 0x10;		/* output index has AC bit set */
  } else {
    htbl = cinfo->dc_huff_tbl_ptrs[index];
  }

  if (htbl == NULL)
    ERREXIT1(cinfo->emethods, "Huffman table 0x%02x was not defined", index);
  
  if (! htbl->sent_table) {
    emit_marker(jdata_aid, M_DHT);  /* 2 */
    
    length = 0;
    for (i = 1; i <= 16; i++)
      length += htbl->bits[i];
    
    emit_2bytes(jdata_aid, length + 2 + 1 + 16);    /* 2*/
    emit_byte(jdata_aid, index);                    /* 1 */
    
    for (i = 1; i <= 16; i++)                   /* 16 */
      emit_byte(jdata_aid, htbl->bits[i]);
    
    for (i = 0; i < length; i++)
      emit_byte(jdata_aid, htbl->huffval[i]);       /* <=256 */
    
    htbl->sent_table = TRUE;
  }
}   /* 21+256=277 bytes written */


#ifdef PROTOTYPE
LOCAL VOID emit_dac (compress_info_ptr cinfo)
#else
LOCAL VOID emit_dac (cinfo)
compress_info_ptr cinfo;
#endif
/* Emit a DAC marker */
/* Since the useful info is so small, we want to emit all the tables in */
/* one DAC marker.  Therefore this routine does its own scan of the table. */
{
  char dc_in_use[NUM_ARITH_TBLS];
  char ac_in_use[NUM_ARITH_TBLS];
  int length, i;
  
  for (i = 0; i < NUM_ARITH_TBLS; i++)
    dc_in_use[i] = ac_in_use[i] = 0;
  
  for (i = 0; i < cinfo->num_components; i++) {
    dc_in_use[cinfo->comp_info[i].dc_tbl_no] = 1;
    ac_in_use[cinfo->comp_info[i].ac_tbl_no] = 1;
  }
  
  length = 0;
  for (i = 0; i < NUM_ARITH_TBLS; i++)
    length += dc_in_use[i] + ac_in_use[i];
  
  emit_marker(jdata_aid, M_DAC);                /* 2 */
  
  emit_2bytes(jdata_aid, length*2 + 2);         /* 2 */
  
  for (i = 0; i < NUM_ARITH_TBLS; i++) {    /* 32 */
    if (dc_in_use[i]) {
      emit_byte(jdata_aid, i);
      emit_byte(jdata_aid, cinfo->arith_dc_L[i] + (cinfo->arith_dc_U[i]<<4));
    }
    if (ac_in_use[i]) {
      emit_byte(jdata_aid, i + 0x10);
      emit_byte(jdata_aid, cinfo->arith_ac_K[i]);
    }
  }
}       /* 36 bytes written */


#ifdef PROTOTYPE
LOCAL VOID emit_dri (compress_info_ptr cinfo,int32 aid)
#else
LOCAL VOID emit_dri (cinfo, aid)
compress_info_ptr cinfo;
int32 aid;
#endif
/* Emit a DRI marker */
{
  emit_marker(aid, M_DRI);                            /* 2 */
  
  emit_2bytes(aid, 4);    /* fixed length */          /* 2 */

  emit_2bytes(aid, (int) cinfo->restart_interval);    /* 2 */
}   /* 6 bytes written */


#ifdef PROTOTYPE
LOCAL VOID emit_sof (compress_info_ptr cinfo, int32 aid, JPEG_MARKER code)
#else
LOCAL VOID emit_sof (cinfo, aid, code)
compress_info_ptr cinfo;
int32 aid;
JPEG_MARKER code;
#endif
/* Emit a SOF marker */
{
  int i;
  
    emit_marker(aid, code);         /* 2 */
  
    emit_2bytes(aid, 3 * cinfo->num_components + 2 + 5 + 1); /* length */   /* 2*/

    if (cinfo->image_height > 65535L || cinfo->image_width > 65535L)
        ERREXIT(cinfo->emethods, "Maximum image dimension for JFIF is 65535 pixels");

    emit_byte(aid, cinfo->data_precision);              /* 1 */
    emit_2bytes(aid, (int) cinfo->image_height);        /* 2 */
    emit_2bytes(aid, (int) cinfo->image_width);         /* 2 */

    emit_byte(aid, cinfo->num_components);              /* 1 */

    for (i = 0; i < cinfo->num_components; i++) {         /* 3*3 */
        emit_byte(aid, cinfo->comp_info[i].component_id);
        emit_byte(aid, (cinfo->comp_info[i].h_samp_factor << 4)
		     + cinfo->comp_info[i].v_samp_factor);
        emit_byte(aid, cinfo->comp_info[i].quant_tbl_no);
    }
}   /* 19 bytes written */


#ifdef PROTOTYPE
LOCAL VOID emit_sos (compress_info_ptr cinfo)
#else
LOCAL VOID emit_sos (cinfo)
compress_info_ptr cinfo;
#endif
/* Emit a SOS marker */
{
  int i;
  
  emit_marker(jdata_aid, M_SOS);            /* 2 */
  
  emit_2bytes(jdata_aid, 2 * cinfo->comps_in_scan + 2 + 1 + 3); /* length */    /* 2 */
  
  emit_byte(jdata_aid, cinfo->comps_in_scan);   /* 1 */
  
  for (i = 0; i < cinfo->comps_in_scan; i++) {  /* 3*2 */
    emit_byte(jdata_aid, cinfo->cur_comp_info[i]->component_id);
    emit_byte(jdata_aid, (cinfo->cur_comp_info[i]->dc_tbl_no << 4)
		     + cinfo->cur_comp_info[i]->ac_tbl_no);
  }

  emit_byte(jdata_aid, 0);      /* Spectral selection start */      /* 1 */
  emit_byte(jdata_aid, DCTSIZE2-1); /* Spectral selection end */    /* 1 */
  emit_byte(jdata_aid, 0);      /* Successive approximation */      /* 1 */
}   /* 14 bytes written */


#ifdef PROTOTYPE
LOCAL VOID emit_jfif_app0 (int32 aid,compress_info_ptr cinfo)
#else
LOCAL VOID emit_jfif_app0 (aid, cinfo)
int32 aid;
compress_info_ptr cinfo;
#endif
/* Emit a JFIF-compliant APP0 marker */
{
  /*
   * Length of APP0 block	(2 bytes)
   * Block ID			(4 bytes - ASCII "JFIF")
   * Zero byte			(1 byte to terminate the ID string)
   * Version Major, Minor	(2 bytes - 0x01, 0x01)
   * Units			(1 byte - 0x00 = none, 0x01 = inch, 0x02 = cm)
   * Xdpu			(2 bytes - dots per unit horizontal)
   * Ydpu			(2 bytes - dots per unit vertical)
   * Thumbnail X size		(1 byte)
   * Thumbnail Y size		(1 byte)
   */
  
  emit_marker(aid, M_APP0);       /* 2 */
  
  emit_2bytes(aid, 2 + 4 + 1 + 2 + 1 + 2 + 2 + 1 + 1); /* length */   /* 2*/

  emit_byte(aid, 'J');    /* Identifier */            /* 1 */
  emit_byte(aid, 'F');                                /* 1 */
  emit_byte(aid, 'I');                                /* 1 */
  emit_byte(aid, 'F');                                /* 1 */
  emit_byte(aid, 0);                                  /* 1 */
  emit_byte(aid, 1);      /* Major version */         /* 1 */
  emit_byte(aid, 1);      /* Minor version */         /* 1 */
  emit_byte(aid, cinfo->density_unit); /* Pixel size information */   /* 1 */
  emit_2bytes(aid, (int) cinfo->X_density);           /* 2 */
  emit_2bytes(aid, (int) cinfo->Y_density);           /* 2 */
  emit_byte(aid, 0);      /* No thumbnail image */    /* 1 */
  emit_byte(aid, 0);                                  /* 1 */
}   /* 18 bytes written */


/*
 * Write the file header.
 */


#ifdef PROTOTYPE
GLOBAL VOID write_file_header (compress_info_ptr cinfo)
#else
GLOBAL VOID write_file_header (cinfo)
compress_info_ptr cinfo;
#endif
{
    int32 aid;
    char qt_in_use[NUM_QUANT_TBLS];
    int i, prec;
    bool is_baseline;

    /* Get an AID to write the JPEG header into.  1024 bytes should be more */
    /*  than enough (it should fit into 575) */
    aid=Hstartwrite(img_file_id,img_scheme,img_ref,1024);

    emit_marker(aid, M_SOI);    /* first the SOI */       /* 2 */

    if (cinfo->write_JFIF_header) /* next an optional JFIF APP0 */
        emit_jfif_app0(aid, cinfo);                  /* 18 */

  /* Emit DQT for each quantization table. */
  /* Note that doing it here means we can't adjust the QTs on-the-fly. */
  /* If we did want to do that, we'd have a problem with checking precision */
  /* for the is_baseline determination. */

    for (i = 0; i < NUM_QUANT_TBLS; i++)
        qt_in_use[i] = 0;

    for (i = 0; i < cinfo->num_components; i++)
        qt_in_use[cinfo->comp_info[i].quant_tbl_no] = 1;

    prec = 0;
    for (i = 0; i < NUM_QUANT_TBLS; i++) {      /* 4*133 */
        if (qt_in_use[i])
            prec += emit_dqt(cinfo, aid, i);
    }
  /* now prec is nonzero iff there are any 16-bit quant tables. */

    if (cinfo->restart_interval)
        emit_dri(cinfo,aid);                /* 6 */

  /* Check for a non-baseline specification. */
  /* Note we assume that Huffman table numbers won't be changed later. */
    is_baseline = TRUE;
    if (cinfo->arith_code || (cinfo->data_precision != 8))
        is_baseline = FALSE;
    for (i = 0; i < cinfo->num_components; i++) {
        if (cinfo->comp_info[i].dc_tbl_no > 1 || cinfo->comp_info[i].ac_tbl_no > 1)
        is_baseline = FALSE;
    }
    if (prec && is_baseline) {
        is_baseline = FALSE;
    /* If it's baseline except for quantizer size, warn the user */
        TRACEMS(cinfo->emethods, 0,
            "Caution: quantization tables are too coarse for baseline JPEG");
    }


  /* Emit the proper SOF marker */
    if (cinfo->arith_code)
/* 19 */emit_sof(cinfo, aid, M_SOF9);    /* SOF code for arithmetic coding */
    else if (is_baseline)
/* 19 */emit_sof(cinfo, aid, M_SOF0);    /* SOF code for baseline implementation */
    else
/* 19 */emit_sof(cinfo, aid, M_SOF1);    /* SOF code for non-baseline Huffman file */
/* 19+532+6+18=575 bytes written */
    Htrunc(aid,byte_count); /* truncate the JPEG header */
    Hendaccess(aid);        /* done writing to the JPEG header */

    /* Open the JPEG data for writing */
    if((jdata_aid=Hstartwrite(img_file_id,img_tag,img_ref,1024))==FAIL) {
        /* What _should_ we do on an error?  We can't return an error,  */
        /*  because this routine is called from the JPEG subroutines... */
fprintf(stderr,"Error from Hstartwrite\n");
HEprint(stderr,0);  /* print all the errors */
      } /* end if */
    /* Make the dataset appendable */
    if(Happendable(jdata_aid)==FAIL) {
        /* What _should_ we do on an error?  We can't return an error,  */
        /*  because this routine is called from the JPEG subroutines... */
fprintf(stderr,"Error from Happendable\n");
HEprint(stderr,0);  /* print all the errors */
      } /* end if */
}   /* end write_file_header */


/*
 * Write the start of a scan (everything through the SOS marker).
 */

#ifdef PROTOTYPE
GLOBAL VOID write_scan_header (compress_info_ptr cinfo)
#else
GLOBAL VOID write_scan_header (cinfo)
compress_info_ptr cinfo;
#endif
{
  int i;

  if (cinfo->arith_code) {
    /* Emit arith conditioning info.  We will have some duplication
     * if the file has multiple scans, but it's so small it's hardly
     * worth worrying about.
     */
    emit_dac(cinfo);
  } else {
    /* Emit Huffman tables.  Note that emit_dht takes care of
     * suppressing duplicate tables.
     */
    for (i = 0; i < cinfo->comps_in_scan; i++) {
      emit_dht(cinfo, cinfo->cur_comp_info[i]->dc_tbl_no, FALSE);
      emit_dht(cinfo, cinfo->cur_comp_info[i]->ac_tbl_no, TRUE);
    }
  }

  emit_sos(cinfo);
}   /* end write_scan_header */


/*
 * Write some bytes of compressed data within a scan.
 */

#ifdef PROTOTYPE
GLOBAL VOID write_jpeg_data (compress_info_ptr cinfo, char *dataptr, int datacount)
#else
GLOBAL VOID write_jpeg_data (cinfo, dataptr, datacount)
compress_info_ptr cinfo;
char *dataptr;
int datacount;
#endif
{
  WRITE_BYTES(cinfo, (uint8 *)dataptr, datacount);
}   /* end write_jpeg_data */


/*
 * Finish up after a compressed scan (series of write_jpeg_data calls).
 */

#ifdef PROTOTYPE
GLOBAL VOID write_scan_trailer (compress_info_ptr cinfo)
#else
GLOBAL VOID write_scan_trailer (cinfo)
compress_info_ptr cinfo;
#endif
{
  /* no work needed in this format */
}   /* end write_scan_trailer */


/*
 * Finish up at the end of the image.
 */

#ifdef PROTOTYPE
GLOBAL VOID write_file_trailer (compress_info_ptr cinfo)
#else
GLOBAL VOID write_file_trailer (cinfo)
compress_info_ptr cinfo;
#endif
{
    emit_marker(jdata_aid, M_EOI);

    Hendaccess(jdata_aid);      /* Stop using the AID for the JPEG data */
}   /* write_file_trailer */


/*
 * The method selection routine for standard JPEG header writing.
 * This should be called from c_ui_method_selection if appropriate.
 */

#ifdef PROTOTYPE
GLOBAL VOID jselwhdf (compress_info_ptr cinfo)
#else
GLOBAL VOID jselwhdf (cinfo)
compress_info_ptr cinfo;
#endif
{
  cinfo->methods->write_file_header = write_file_header;
  cinfo->methods->write_scan_header = write_scan_header;
  cinfo->methods->write_jpeg_data = write_jpeg_data;
  cinfo->methods->write_scan_trailer = write_scan_trailer;
  cinfo->methods->write_file_trailer = write_file_trailer;
}   /* end jselwhdf() */

/*****************************************************************************/
/* Routines for getting the image data and giving it back to the JPEG engine */
/*****************************************************************************/

/*-----------------------------------------------------------------------------
 * Name:    input_init
 * Purpose: Initialize for input; return image size and component data
 * Inputs:
 *      cinfo - JPEG compression structure
 * Returns: nothing
 * Users:   JPEG software (this is a function pointer)
 * Invokes: none
 * Remarks:
 *      This routine must return five pieces of information about the incoming
 *      image, and must do any setup needed for the get_input_row routine.
 *      The image information is returned in fields of the cinfo struct.
 *---------------------------------------------------------------------------*/
#ifdef PROTOTYPE
GLOBAL VOID input_init (compress_info_ptr cinfo)
#else
GLOBAL VOID input_init (cinfo)
compress_info_ptr cinfo;
#endif
{
    cinfo->image_width = img_xdim;        /* width in pixels */
    cinfo->image_height = img_ydim;       /* height in pixels */

  /* JPEG views an image as being a rectangular array of pixels, with each
   * pixel having the same number of "component" values (color channels).
   * You must specify how many components there are and the colorspace
   * interpretation of the components.  Most applications will use RGB data or
   * grayscale data.  If you want to use something else, you'll need to study
   * and perhaps modify jcdeflts.c, jccolor.c, and jdcolor.c.
   */
    if(img_scheme==DFTAG_JPEG) {    /* 24-bit images */
        cinfo->input_components = 3;
        cinfo->in_color_space = CS_RGB;
      } /* end if */
    else {  /* 8-bit (greyscale) images */
        cinfo->input_components = 1;
        cinfo->in_color_space = CS_GRAYSCALE;
      } /* end else */

    cinfo->data_precision = 8;        /* bits per pixel component value */
  /* In the current JPEG software, data_precision must be set equal to
   * BITS_IN_JSAMPLE, which is 8 unless you twiddle jconfig.h.  Future
   * versions might allow you to say either 8 or 12 if compiled with
   * 12-bit JSAMPLEs, or up to 16 in lossless mode.  In any case,
   * it is up to you to scale incoming pixel values to the range
   *   0 .. (1<<data_precision)-1.
   * If your image data format is fixed at a byte per component,
   * then saying "8" is probably the best long-term solution.
   */
}   /* end input_init */


/*-----------------------------------------------------------------------------
 * Name:    get_input_row
 * Purpose: Read next row of pixels into pixel_row[][]
 * Inputs:
 *      cinfo - JPEG compression structure
 *      pixel_row - storage for the next row of pixels to compress
 * Returns: nothing
 * Users:   JPEG software (this is a function pointer)
 * Invokes: none
 * Remarks:
 *  This function is called repeatedly and must supply the next row of pixels
 * on each call.  The rows MUST be returned in top-to-bottom order if you want
 * your JPEG files to be compatible with everyone else's.
 *  The data is to be returned into a 2-D array of JSAMPLEs, indexed as
 *		JSAMPLE pixel_row[component][column]
 * where component runs from 0 to cinfo->input_components-1, and column runs
 * from 0 to cinfo->image_width-1 (column 0 is left edge of image).  Note that
 * this is actually an array of pointers to arrays rather than a true 2D array,
 * since C does not support variable-size multidimensional arrays.
 * JSAMPLE is typically typedef'd as "unsigned char".
 *---------------------------------------------------------------------------*/
#ifdef PROTOTYPE
GLOBAL VOID get_input_row (compress_info_ptr cinfo, JSAMPARRAY pixel_row)
#else
GLOBAL VOID get_input_row (cinfo, pixel_row)
compress_info_ptr cinfo;
JSAMPARRAY pixel_row;
#endif
{
  /* This example shows how you might read RGB data (3 components)
   * from an input file in which the data is stored 3 bytes per pixel
   * in left-to-right, top-to-bottom order.
   */
    register JSAMPROW ptr0, ptr1, ptr2;
    register long col;

    if(img_scheme==DFTAG_JPEG) {    /* 24-bit images */
        ptr0 = pixel_row[0];
        ptr1 = pixel_row[1];
        ptr2 = pixel_row[2];
        for (col = 0; col < cinfo->image_width; col++) {
            *ptr0++ = (JSAMPLE) *img_ptr++;     /* red */
            *ptr1++ = (JSAMPLE) *img_ptr++;     /* green */
            *ptr2++ = (JSAMPLE) *img_ptr++;     /* blue */
          } /* end for */
      } /* end if */
    else {      /* 8-bit images */
        HDmemcpy(pixel_row[0],img_ptr,cinfo->image_width);
        img_ptr+=cinfo->image_width;
      } /* end else */
}   /* end get_input_row() */


/*-----------------------------------------------------------------------------
 * Name:    input_term
 * Purpose: Finish up at end of input
 * Inputs:
 *      cinfo - JPEG compression structure
 * Returns: nothing
 * Users:   JPEG software (this is a function pointer)
 * Invokes: none
 * Remarks:
 * This termination routine will very often have no work to do,
 * but you must provide it anyway.
 * Note that the JPEG code will only call it during successful exit;
 * if you want it called during error exit, you gotta do that yourself.
 *---------------------------------------------------------------------------*/
#ifdef PROTOTYPE
GLOBAL VOID input_term (compress_info_ptr cinfo)
#else
GLOBAL VOID input_term (cinfo)
compress_info_ptr cinfo;
#endif
{
}   /* end input_term() */

/*-----------------------------------------------------------------------------
 * Name:    c_ui_method_selection
 * Purpose: Determine what output JPEG file format is to be written.
 * Inputs:
 *      cinfo - JPEG compression structure
 * Returns: nothing
 * Users:   JPEG software (this is a function pointer)
 * Invokes: none
 * Remarks:
 * This routine must determine what output JPEG file format is to be written,
 * and make any other compression parameter changes that are desirable.
 * This routine gets control after the input file header has been read
 * (i.e., right after input_init has been called).
 *---------------------------------------------------------------------------*/
#ifdef PROTOTYPE
GLOBAL VOID c_ui_method_selection (compress_info_ptr cinfo)
#else
GLOBAL VOID c_ui_method_selection (cinfo)
compress_info_ptr cinfo;
#endif
{
  /* If the input is gray scale, generate a monochrome JPEG file. */
    if (cinfo->in_color_space == CS_GRAYSCALE)
        j_monochrome_default(cinfo);

  /* For now, select HDF output format. */
    jselwhdf(cinfo);
}   /* end c_ui_method_selection() */

/***********************************************************************/
/* HDF callable routine for writing out an image with JPEG compression */
/***********************************************************************/

/*-----------------------------------------------------------------------------
 * Name:    DFCIjpeg
 * Purpose: compress an image using the JPEG compression algorithm
 * Inputs:
 * Returns: 0 on success, -1 on failure
 * Users:   HDF programmers, DFputcomp, other routines
 * Invokes: JPEG library routines (lots of them...)
 * Remarks: Uses the JPEG library routines.  The reason this routine doesn't
 *          compress into a buffer (like IMCOMP and RLE methods) is because
 *          the size of the buffer cannot be predicted before-hand and since
 *          24-bit images are already huge, I don't want to try allocating a
 *          worst-case buffer.  This means that this routine has to do the
 *          writing of the compressed image itself, instead of allowing
 *          DFputcomp() to write out the entire image at once.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFCIjpeg(int32 file_id,uint16 tag,uint16 ref,int32 xdim, int32 ydim,
    VOIDP image, int16 scheme, comp_info *scheme_info)
#else
intn DFCIjpeg(file_id, tag, ref, xdim, ydim, image, scheme, scheme_info)
    int32 file_id;
    uint16 tag;
    uint16 ref;
    int32 xdim;
    int32 ydim;
    VOIDP image;
    int16 scheme;
    comp_info *scheme_info;
#endif
{
  /* These three structs contain JPEG parameters and working data.
   * They must survive for the duration of parameter setup and one
   * call to jpeg_compress; typically, making them local data in the
   * calling routine is the best strategy.
   */
    struct Compress_info_struct cinfo;
    struct Compress_methods_struct c_methods;
    struct External_methods_struct e_methods;

    img_file_id=file_id;    /* keep the file ID around */
    img_tag=tag;        /* keep dataset's tag around */
    img_ref=ref;        /* keep reference number around */
    img_ptr=(uint8 *)image;   /* Set the static pointer to the image to read */
    img_xdim=xdim;      /* Keep local copies of the X and Y dimensions */
    img_ydim=ydim;
    img_scheme=(intn)scheme;  /* Type of image compression we are going to do */
    byte_count=0;       /* reset byte_count */

  /* Initialize the system-dependent method pointers. */
    cinfo.methods = &c_methods;   /* links to method structs */
    cinfo.emethods = &e_methods;
  /* Here we use the default JPEG error handler, which will just print
   * an error message on stderr and call exit().  See the second half of
   * this file for an example of more graceful error recovery.
   */
    jselerror(&e_methods);    /* select std error/trace message routines */
  /* Here we use the standard memory manager provided with the JPEG code.
   * In some cases you might want to replace the memory manager, or at
   * least the system-dependent part of it, with your own code.
   */
    jselmemmgr(&e_methods);   /* select std memory allocation routines */
  /* If the compressor requires full-image buffers (for entropy-coding
   * optimization or a noninterleaved JPEG file), it will create temporary
   * files for anything that doesn't fit within the maximum-memory setting.
   * (Note that temp files are NOT needed if you use the default parameters.)
   * You can change the default maximum-memory setting by changing
   * e_methods.max_memory_to_use after jselmemmgr returns.
   * On some systems you may also need to set up a signal handler to
   * ensure that temporary files are deleted if the program is interrupted.
   * (This is most important if you are on MS-DOS and use the jmemdos.c
   * memory manager back end; it will try to grab extended memory for
   * temp files, and that space will NOT be freed automatically.)
   * See jcmain.c or jdmain.c for an example signal handler.
   */

  /* Here, set up pointers to your own routines for input data handling
   * and post-init parameter selection.
   */
    c_methods.input_init = input_init;
    c_methods.get_input_row = get_input_row;
    c_methods.input_term = input_term;
    c_methods.c_ui_method_selection = c_ui_method_selection;

  /* Set up default JPEG parameters in the cinfo data structure. */
    j_c_defaults(&cinfo, scheme_info->jpeg.quality,
            scheme_info->jpeg.force_baseline);
  /* Note: 75 is the recommended default quality level; you may instead pass
   * a user-specified quality level.  Be aware that values below 25 will cause
   * non-baseline JPEG files to be created (and a warning message to that
   * effect to be emitted on stderr).  This won't bother our decoder, but some
   * commercial JPEG implementations may choke on non-baseline JPEG files.
   * If you want to force baseline compatibility, pass TRUE instead of FALSE.
   * (If non-baseline files are fine, but you could do without that warning
   * message, set e_methods.trace_level to -1.)
   */

  /* At this point you can modify the default parameters set by j_c_defaults
   * as needed.  For a minimal implementation, you shouldn't need to change
   * anything.  See jcmain.c for some examples of what you might change.
   */
    e_methods.trace_level=(-1); /* no warning messages */

#ifdef QAK
  /* Select the input and output files.
   * Note that cinfo.input_file is only used if your input reading routines
   * use it; otherwise, you can just make it NULL.
   * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
   * requires it in order to write binary files.
   */

    cinfo.input_file = NULL;  /* if no actual input file involved */

    if ((cinfo.output_file = fopen(filename, "wb")) == NULL) {
        fprintf(stderr, "can't open %s\n", filename);
        exit(1);
    }
#endif

  /* Here we go! */
  jpeg_compress(&cinfo);

  /* That's it, son.  Nothin' else to do, except close files. */
#ifdef QAK
  /* Here we assume only the output file need be closed. */
  fclose(cinfo.output_file);
#endif

  /* Note: if you want to compress more than one image, we recommend you
   * repeat this whole routine.  You MUST repeat the j_c_defaults()/alter
   * parameters/jpeg_compress() sequence, as some data structures allocated
   * in j_c_defaults are freed upon exit from jpeg_compress.
   */
   return(SUCCEED);     /* we must be ok... */
}   /* end DFCIjpeg() */

