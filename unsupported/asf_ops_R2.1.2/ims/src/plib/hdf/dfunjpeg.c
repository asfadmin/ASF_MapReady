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
 * File:    dfunjpeg.c
 * Purpose: JPEG image decompression algorithm
 * Invokes: JPEG library functions
 * Contents:
 *  DFCIunjpeg: decompress image using JPEG compression
 * Remarks: DFCIunjpeg() decompresses JPEG encoded images using the JPEG
 *      library functions.  The dfjpeg.c file and this file (dfunjpeg.c) should
 *      remain the only HDF files that has to know about how to use the JPEG
 *      routines.
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
PRIVATE uint8 *img_ptr;     /* Pointer to the image to compress */
PRIVATE intn img_scheme;    /* What type of image comp. are we doing? 24 or 8 bit */

typedef enum {          /* JPEG marker codes */
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


/*
 * Routines to parse JPEG markers & save away the useful info.
 */

#ifdef PROTOTYPE
LOCAL int32 get_2bytes (decompress_info_ptr cinfo)
#else
LOCAL int32 get_2bytes (cinfo)
decompress_info_ptr cinfo;
#endif
/* Get a 2-byte unsigned integer (e.g., a marker parameter length field) */
{
  int32 a;
  
  a = JGETC(cinfo);
  return (a << 8) + JGETC(cinfo);
}


#ifdef PROTOTYPE
LOCAL VOID skip_variable (decompress_info_ptr cinfo, int code)
#else
LOCAL VOID skip_variable (cinfo, code)
decompress_info_ptr cinfo;
int code;
#endif
/* Skip over an unknown or uninteresting variable-length marker */
{
    int32 length;
  
    length = get_2bytes(cinfo);
  
    TRACEMS2(cinfo->emethods, 1,
            "Skipping marker 0x%02x, length %u", code, (int) length);
  
    for (length -= 2; length > 0; length--)
        (void) JGETC(cinfo);
}   /* end skip_variable() */


#ifdef PROTOTYPE
LOCAL VOID get_dht (decompress_info_ptr cinfo)
#else
LOCAL VOID get_dht (cinfo)
decompress_info_ptr cinfo;
#endif
/* Process a DHT marker */
{
    int32 length;
    uint8 bits[17];
    uint8 huffval[256];
    int i, index, count;
    HUFF_TBL **htblptr;
  
    length = get_2bytes(cinfo)-2;
  
    while (length > 0) {
        index = JGETC(cinfo);

        TRACEMS1(cinfo->emethods, 1, "Define Huffman Table 0x%02x", index);
      
        bits[0] = 0;
        count = 0;
        for (i = 1; i <= 16; i++) {
            bits[i] = (uint8) JGETC(cinfo);
            count += bits[i];
        }

        TRACEMS8(cinfo->emethods, 2, "        %3d %3d %3d %3d %3d %3d %3d %3d",
             bits[1], bits[2], bits[3], bits[4],
             bits[5], bits[6], bits[7], bits[8]);
        TRACEMS8(cinfo->emethods, 2, "        %3d %3d %3d %3d %3d %3d %3d %3d",
             bits[9], bits[10], bits[11], bits[12],
             bits[13], bits[14], bits[15], bits[16]);

        if (count > 256)
            ERREXIT(cinfo->emethods, "Bogus DHT counts");

        for (i = 0; i < count; i++)
            huffval[i] = (uint8) JGETC(cinfo);

        length -= 1 + 16 + count;

        if (index & 0x10) {     /* AC table definition */
            index -= 0x10;
            htblptr = &cinfo->ac_huff_tbl_ptrs[index];
        } else {            /* DC table definition */
            htblptr = &cinfo->dc_huff_tbl_ptrs[index];
        }

        if (index < 0 || index >= NUM_HUFF_TBLS)
            ERREXIT1(cinfo->emethods, "Bogus DHT index %d", index);

        if (*htblptr == NULL)
            *htblptr = (HUFF_TBL *) (*cinfo->emethods->alloc_small) (SIZEOF(HUFF_TBL));

        memcpy((void *) (*htblptr)->bits, (void *) bits,
            SIZEOF((*htblptr)->bits));
        memcpy((void *) (*htblptr)->huffval, (void *) huffval,
            SIZEOF((*htblptr)->huffval));
    }
}   /* end get_dht() */


#ifdef PROTOTYPE
LOCAL VOID get_dac (decompress_info_ptr cinfo)
#else
LOCAL VOID get_dac (cinfo)
decompress_info_ptr cinfo;
#endif
/* Process a DAC marker */
{
    int32 length;
    int index, val;

    length = get_2bytes(cinfo)-2;
  
    while (length > 0) {
        index = JGETC(cinfo);
        val = JGETC(cinfo);

        TRACEMS2(cinfo->emethods, 1,
             "Define Arithmetic Table 0x%02x: 0x%02x", index, val);

        if (index < 0 || index >= (2*NUM_ARITH_TBLS))
            ERREXIT1(cinfo->emethods, "Bogus DAC index %d", index);

        if (index >= NUM_ARITH_TBLS) { /* define AC table */
            cinfo->arith_ac_K[index-NUM_ARITH_TBLS] = (uint8) val;
        } else {            /* define DC table */
            cinfo->arith_dc_L[index] = (uint8) (val & 0x0F);
            cinfo->arith_dc_U[index] = (uint8) (val >> 4);
            if (cinfo->arith_dc_L[index] > cinfo->arith_dc_U[index])
                ERREXIT1(cinfo->emethods, "Bogus DAC value 0x%x", val);
        }

        length -= 2;
    }
}   /* end get_dac() */


#ifdef PROTOTYPE
LOCAL VOID get_dqt (decompress_info_ptr cinfo)
#else
LOCAL VOID get_dqt (cinfo)
decompress_info_ptr cinfo;
#endif
/* Process a DQT marker */
{
    int32 length;
    int n, i, prec;
    uint16 tmp;
    QUANT_TBL_PTR quant_ptr;
  
    length = get_2bytes(cinfo) - 2;
  
    while (length > 0) {
        n = JGETC(cinfo);
        prec = n >> 4;
        n &= 0x0F;

        TRACEMS2(cinfo->emethods, 1,
            "Define Quantization Table %d  precision %d", n, prec);

        if (n >= NUM_QUANT_TBLS)
            ERREXIT1(cinfo->emethods, "Bogus table number %d", n);
      
        if (cinfo->quant_tbl_ptrs[n] == NULL)
            cinfo->quant_tbl_ptrs[n] = (QUANT_TBL_PTR)
        (*cinfo->emethods->alloc_small) (SIZEOF(QUANT_TBL));
        quant_ptr = cinfo->quant_tbl_ptrs[n];

        for (i = 0; i < DCTSIZE2; i++) {
            tmp = (uint16)JGETC(cinfo);
            if (prec)
                tmp = (tmp<<8) + (uint16)JGETC(cinfo);
            quant_ptr[i] = tmp;
        }

        for (i = 0; i < DCTSIZE2; i += 8) {
            TRACEMS8(cinfo->emethods, 2, "        %4d %4d %4d %4d %4d %4d %4d %4d",
                quant_ptr[i  ], quant_ptr[i+1], quant_ptr[i+2], quant_ptr[i+3],
                quant_ptr[i+4], quant_ptr[i+5], quant_ptr[i+6], quant_ptr[i+7]);
        }

        length -= DCTSIZE2+1;
        if (prec) length -= DCTSIZE2;
    }
}   /* end get_dqt() */


#ifdef PROTOTYPE
LOCAL VOID get_dri (decompress_info_ptr cinfo)
#else
LOCAL VOID get_dri (cinfo)
decompress_info_ptr cinfo;
#endif
/* Process a DRI marker */
{
    if (get_2bytes(cinfo) != 4)
        ERREXIT(cinfo->emethods, "Bogus length in DRI");

    cinfo->restart_interval = (uint16) get_2bytes(cinfo);

    TRACEMS1(cinfo->emethods, 1,
	   "Define Restart Interval %d", cinfo->restart_interval);
}   /* end get_dri() */


#ifdef PROTOTYPE
LOCAL VOID get_app0 (decompress_info_ptr cinfo)
#else
LOCAL VOID get_app0 (cinfo)
decompress_info_ptr cinfo;
#endif
/* Process an APP0 marker */
{
#define JFIF_LEN 14
    int32 length;
    uint8 b[JFIF_LEN];
    int buffp;

    length = get_2bytes(cinfo) - 2;

  /* See if a JFIF APP0 marker is present */

    if (length >= JFIF_LEN) {
        for (buffp = 0; buffp < JFIF_LEN; buffp++)
            b[buffp] = (uint8) JGETC(cinfo);
        length -= JFIF_LEN;

        if (b[0]=='J' && b[1]=='F' && b[2]=='I' && b[3]=='F' && b[4]==0) {
      /* Found JFIF APP0 marker: check version */
      /* Major version must be 1 */
            if (b[5] != 1)
                ERREXIT2(cinfo->emethods, "Unsupported JFIF revision number %d.%02d",
                        b[5], b[6]);
      /* Minor version should be 0 or 1, but try to process anyway if newer */
            if (b[6] != 0 && b[6] != 1)
                TRACEMS2(cinfo->emethods, 0, "Warning: unknown JFIF revision number %d.%02d",
                    b[5], b[6]);
      /* Save info */
            cinfo->density_unit = b[7];
            cinfo->X_density = ((uint16)b[8] << 8) + b[9];
            cinfo->Y_density = ((uint16)b[10] << 8) + b[11];
      /* Assume colorspace is YCbCr, unless UI has overridden me */
            if (cinfo->jpeg_color_space == CS_UNKNOWN)
                cinfo->jpeg_color_space = CS_YCbCr;
            TRACEMS3(cinfo->emethods, 1, "JFIF APP0 marker, density %dx%d  %d",
                    cinfo->X_density, cinfo->Y_density, cinfo->density_unit);
        } else {
            TRACEMS(cinfo->emethods, 1, "Unknown APP0 marker (not JFIF)");
        }
    } else {
        TRACEMS1(cinfo->emethods, 1,
                "Short APP0 marker, length %d", (int) length);
    }

    while (length-- > 0)      /* skip any remaining data */
        (void) JGETC(cinfo);
}   /* end get_app0() */


#ifdef PROTOTYPE
LOCAL VOID get_sof (decompress_info_ptr cinfo, int code)
#else
LOCAL VOID get_sof (cinfo, code)
decompress_info_ptr cinfo;
int code;
#endif
/* Process a SOFn marker */
{
    int32 length;
    short ci;
    int c;
    jpeg_component_info * compptr;
  
    length = get_2bytes(cinfo);
  
    cinfo->data_precision = JGETC(cinfo);
    cinfo->image_height   = get_2bytes(cinfo);
    cinfo->image_width    = get_2bytes(cinfo);
    cinfo->num_components = JGETC(cinfo);

    TRACEMS4(cinfo->emethods, 1,
	   "Start Of Frame 0x%02x: width=%u, height=%u, components=%d",
	   code, (int) cinfo->image_width, (int) cinfo->image_height,
	   cinfo->num_components);

  /* We don't support files in which the image height is initially specified */
  /* as 0 and is later redefined by DNL.  As long as we have to check that,  */
  /* might as well have a general sanity check. */
    if (cinfo->image_height <= 0 || cinfo->image_width <= 0
            || cinfo->num_components <= 0)
        ERREXIT(cinfo->emethods, "Empty JPEG image (DNL not supported)");

#ifdef EIGHT_BIT_SAMPLES
    if (cinfo->data_precision != 8)
        ERREXIT(cinfo->emethods, "Unsupported JPEG data precision");
#endif
#ifdef TWELVE_BIT_SAMPLES
    if (cinfo->data_precision != 12) /* this needs more thought?? */
        ERREXIT(cinfo->emethods, "Unsupported JPEG data precision");
#endif
#ifdef SIXTEEN_BIT_SAMPLES
    if (cinfo->data_precision != 16) /* this needs more thought?? */
        ERREXIT(cinfo->emethods, "Unsupported JPEG data precision");
#endif

    if (length != (cinfo->num_components * 3 + 8))
        ERREXIT(cinfo->emethods, "Bogus SOF length");

    cinfo->comp_info = (jpeg_component_info *) (*cinfo->emethods->alloc_small)
			(cinfo->num_components * SIZEOF(jpeg_component_info));
  
    for (ci = 0; ci < cinfo->num_components; ci++) {
        compptr = &cinfo->comp_info[ci];
        compptr->component_index = ci;
        compptr->component_id = JGETC(cinfo);
        c = JGETC(cinfo);
        compptr->h_samp_factor = (c >> 4) & 15;
        compptr->v_samp_factor = (c     ) & 15;
        compptr->quant_tbl_no  = JGETC(cinfo);
      
        TRACEMS4(cinfo->emethods, 1, "    Component %d: %dhx%dv q=%d",
            compptr->component_id, compptr->h_samp_factor,
            compptr->v_samp_factor, compptr->quant_tbl_no);
    }
}   /* end get_sof() */


#ifdef PROTOTYPE
LOCAL VOID get_sos (decompress_info_ptr cinfo)
#else
LOCAL VOID get_sos (cinfo)
decompress_info_ptr cinfo;
#endif
/* Process a SOS marker */
{
    int32 length;
    int i, ci, n, c, cc;
    jpeg_component_info * compptr;
  
    length = get_2bytes(cinfo);
  
    n = JGETC(cinfo);  /* Number of components */
    cinfo->comps_in_scan = n;
    length -= 3;
  
    if (length != (n * 2 + 3) || n < 1 || n > MAX_COMPS_IN_SCAN)
        ERREXIT(cinfo->emethods, "Bogus SOS length");

    TRACEMS1(cinfo->emethods, 1, "Start Of Scan: %d components", n);
  
    for (i = 0; i < n; i++) {
        cc = JGETC(cinfo);
        c = JGETC(cinfo);
        length -= 2;

        for (ci = 0; ci < cinfo->num_components; ci++)
          if (cc == cinfo->comp_info[ci].component_id)
        break;

        if (ci >= cinfo->num_components)
          ERREXIT(cinfo->emethods, "Invalid component number in SOS");

        compptr = &cinfo->comp_info[ci];
        cinfo->cur_comp_info[i] = compptr;
        compptr->dc_tbl_no = (c >> 4) & 15;
        compptr->ac_tbl_no = (c     ) & 15;

        TRACEMS3(cinfo->emethods, 1, "    c%d: [dc=%d ac=%d]", cc,
             compptr->dc_tbl_no, compptr->ac_tbl_no);
    }
  
    while (length-- > 0)
        (void) JGETC(cinfo);
}   /* end get_sos() */


#ifdef PROTOTYPE
LOCAL VOID get_soi (decompress_info_ptr cinfo)
#else
LOCAL VOID get_soi (cinfo)
decompress_info_ptr cinfo;
#endif
/* Process an SOI marker */
{
    int i;
  
    TRACEMS(cinfo->emethods, 1, "Start of Image");

  /* Reset all parameters that are defined to be reset by SOI */

    for (i = 0; i < NUM_ARITH_TBLS; i++) {
        cinfo->arith_dc_L[i] = 0;
        cinfo->arith_dc_U[i] = 1;
        cinfo->arith_ac_K[i] = 5;
    }
    cinfo->restart_interval = 0;

    cinfo->density_unit = 0;  /* set default JFIF APP0 values */
    cinfo->X_density = 1;
    cinfo->Y_density = 1;

    cinfo->CCIR601_sampling = FALSE; /* Assume non-CCIR sampling */
}   /* end get_soi */


#ifdef PROTOTYPE
LOCAL int next_marker (decompress_info_ptr cinfo)
#else
LOCAL int next_marker (cinfo)
decompress_info_ptr cinfo;
#endif
/* Find the next JPEG marker */
/* Note that the output might not be a valid marker code, */
/* but it will never be 0 or FF */
{
    int c, nbytes;

    nbytes = 0;
    do {
        do {            /* skip any non-FF bytes */
            nbytes++;
            c = JGETC(cinfo);
        } while (c != 0xFF);
        do {            /* skip any duplicate FFs */
            nbytes++;
            c = JGETC(cinfo);
        } while (c == 0xFF);
    } while (c == 0);     /* repeat if it was a stuffed FF/00 */

    if (nbytes != 2)
        TRACEMS2(cinfo->emethods, 1, "Skipped %d bytes before marker 0x%02x",
            nbytes-2, c);

    return c;
}   /* end next_marker() */


#ifdef PROTOTYPE
LOCAL JPEG_MARKER process_tables (decompress_info_ptr cinfo)
#else
LOCAL JPEG_MARKER process_tables (cinfo)
decompress_info_ptr cinfo;
#endif
/* Scan and process JPEG markers that can appear in any order */
/* Return when an SOI, EOI, SOFn, or SOS is found */
{
    int c;

    while (TRUE) {
        c = next_marker(cinfo);
      
        switch (c) {
            case M_SOF0:
            case M_SOF1:
            case M_SOF2:
            case M_SOF3:
            case M_SOF5:
            case M_SOF6:
            case M_SOF7:
            case M_JPG:
            case M_SOF9:
            case M_SOF10:
            case M_SOF11:
            case M_SOF13:
            case M_SOF14:
            case M_SOF15:
            case M_SOI:
            case M_EOI:
            case M_SOS:
                return ((JPEG_MARKER) c);
      
            case M_DHT:
                get_dht(cinfo);
                break;
      
            case M_DAC:
                get_dac(cinfo);
                break;
      
            case M_DQT:
                get_dqt(cinfo);
                break;

            case M_DRI:
                get_dri(cinfo);
                break;

            case M_APP0:
                get_app0(cinfo);
                break;

            case M_RST0:        /* these are all parameterless */
            case M_RST1:
            case M_RST2:
            case M_RST3:
            case M_RST4:
            case M_RST5:
            case M_RST6:
            case M_RST7:
            case M_TEM:
                TRACEMS1(cinfo->emethods, 1, "Unexpected marker 0x%02x", c);
                break;

            default:    /* must be DNL, DHP, EXP, APPn, JPGn, COM, or RESn */
                skip_variable(cinfo, c);
                break;
        }
    }
}   /* end process_tables() */



/*
 * Initialize and read the file header (everything through the SOF marker).
 */

#ifdef PROTOTYPE
GLOBAL VOID read_file_header (decompress_info_ptr cinfo)
#else
GLOBAL VOID read_file_header (cinfo)
decompress_info_ptr cinfo;
#endif
{
    int c;

    /* Start reading from the header */
    jdata_aid=Hstartread(img_file_id,img_scheme,img_ref);

  /* Demand an SOI marker at the start of the file --- otherwise it's
   * probably not a JPEG file at all.  If the user interface wants to support
   * nonstandard headers in front of the SOI, it must skip over them itself
   * before calling jpeg_decompress().
   */
    if (JGETC(cinfo) != 0xFF  ||  JGETC(cinfo) != M_SOI)
        ERREXIT(cinfo->emethods, "Not a JPEG file");

    get_soi(cinfo);       /* OK, process SOI */

  /* Process markers until SOF */
    c = process_tables(cinfo);

    switch (c) {
        case M_SOF0:
        case M_SOF1:
            get_sof(cinfo, c);
            cinfo->arith_code = FALSE;
            break;
      
        case M_SOF9:
            get_sof(cinfo, c);
            cinfo->arith_code = TRUE;
            break;

        default:
            ERREXIT1(cinfo->emethods, "Unsupported SOF marker type 0x%02x", c);
            break;
      } /* end switch */

  /* Figure out what colorspace we have */
  /* (too bad the JPEG committee didn't provide a real way to specify this) */

    switch (cinfo->num_components) {
        case 1:
            cinfo->jpeg_color_space = CS_GRAYSCALE;
            break;

        case 3:
            /* if we saw a JFIF marker, leave it set to YCbCr; */
            /* also leave it alone if UI has provided a value */
            if (cinfo->jpeg_color_space == CS_UNKNOWN) {
                short cid0 = cinfo->comp_info[0].component_id;
                short cid1 = cinfo->comp_info[1].component_id;
                short cid2 = cinfo->comp_info[2].component_id;

                /* assume it's JFIF w/out marker */
                if (cid0 == 1 && cid1 == 2 && cid2 == 3)
                    cinfo->jpeg_color_space = CS_YCbCr;
                /* prototype's YIQ matrix */
                else if (cid0 == 1 && cid1 == 4 && cid2 == 5)
                    cinfo->jpeg_color_space = CS_YIQ;
                else {
                    TRACEMS3(cinfo->emethods, 0,
                        "Unrecognized component IDs %d %d %d, assuming YCbCr",
                        cid0, cid1, cid2);
                    cinfo->jpeg_color_space = CS_YCbCr;
                  } /* end else */
              } /* end if */
            break;

        case 4:
            cinfo->jpeg_color_space = CS_CMYK;
            break;

        default:
            cinfo->jpeg_color_space = CS_UNKNOWN;
            break;
      } /* end switch */

    Hendaccess(jdata_aid);

    if((jdata_aid=Hstartread(img_file_id,img_tag,img_ref))==FAIL) {
        /* What _should_ we do on an error?  We can't return an error,  */
        /*  because this routine is called from the JPEG subroutines... */
fprintf(stderr,"Error from Hstartread\n");
HEprint(stderr,0);  /* print all the errors */
      } /* end if */
    else {  /* Successful Hstartread() on the JPEG data, prime the buffer */
        cinfo->next_input_byte = cinfo->input_buffer + MIN_UNGET;

        cinfo->bytes_in_buffer = (int) Hread(jdata_aid, JPEG_BUF_SIZE,
                (uint8 *)cinfo->next_input_byte);
      } /* end else */

}   /* end read_file_header() */


/*
 * Read the start of a scan (everything through the SOS marker).
 * Return TRUE if find SOS, FALSE if find EOI.
 */

#ifdef PROTOTYPE
GLOBAL bool read_scan_header (decompress_info_ptr cinfo)
#else
GLOBAL bool read_scan_header (cinfo)
decompress_info_ptr cinfo;
#endif
{
    int c;
  
  /* Process markers until SOS or EOI */
    c = process_tables(cinfo);
  
    switch (c) {
        case M_SOS:
            get_sos(cinfo);
            return TRUE;
    
        case M_EOI:
            TRACEMS(cinfo->emethods, 1, "End Of Image");
            return FALSE;

        default:
            ERREXIT1(cinfo->emethods, "Unexpected marker 0x%02x", c);
            break;
      } /* end switch */
    return FALSE;         /* keeps lint happy */
}   /* read_scan_header() */


/*
 * Reload the input buffer after it's been emptied, and return the next byte.
 * See the JGETC macro for calling conditions.
 */

#ifdef PROTOTYPE
GLOBAL int read_jpeg_data (decompress_info_ptr cinfo)
#else
GLOBAL int read_jpeg_data (cinfo)
decompress_info_ptr cinfo;
#endif
{
    cinfo->next_input_byte = cinfo->input_buffer + MIN_UNGET;

#ifdef OLD_WAY
    cinfo->bytes_in_buffer = (int) JFREAD(cinfo->input_file,
            cinfo->next_input_byte, JPEG_BUF_SIZE);
#else
    cinfo->bytes_in_buffer = (int) Hread(jdata_aid, JPEG_BUF_SIZE,
            (uint8 *)cinfo->next_input_byte);
#endif
  
    if (cinfo->bytes_in_buffer <= 0)
        ERREXIT(cinfo->emethods, "Unexpected EOF in JPEG file");

    return JGETC(cinfo);
}   /* read_jpeg_data() */


/*
 * Finish up after a compressed scan (series of read_jpeg_data calls);
 * prepare for another read_scan_header call.
 */

#ifdef PROTOTYPE
GLOBAL VOID read_scan_trailer (decompress_info_ptr cinfo)
#else
GLOBAL VOID read_scan_trailer (cinfo)
decompress_info_ptr cinfo;
#endif
{
  /* no work needed */
}


/*
 * Finish up at the end of the file.
 */

#ifdef PROTOTYPE
GLOBAL VOID read_file_trailer (decompress_info_ptr cinfo)
#else
GLOBAL VOID read_file_trailer (cinfo)
decompress_info_ptr cinfo;
#endif
{
#ifdef QAK
  /* no work needed */
#else
    Hendaccess(jdata_aid);
#endif
}


/*
 * The method selection routine for standard JPEG header reading.
 * Note that this must be called by the user interface before calling
 * jpeg_decompress.  When a non-JFIF file is to be decompressed (TIFF,
 * perhaps), the user interface must discover the file type and call
 * the appropriate method selection routine.
 */

#ifdef PROTOTYPE
GLOBAL VOID jselrhdf (decompress_info_ptr cinfo)
#else
GLOBAL VOID jselrhdf (cinfo)
decompress_info_ptr cinfo;
#endif
{
  cinfo->methods->read_file_header = read_file_header;
  cinfo->methods->read_scan_header = read_scan_header;
  /* For HDF files, we supply read_jpeg_data... */
  cinfo->methods->read_jpeg_data = read_jpeg_data;
  cinfo->methods->read_scan_trailer = read_scan_trailer;
  cinfo->methods->read_file_trailer = read_file_trailer;
}

/*
 * To accept the image data from decompression, you must define four routines
 * output_init, put_color_map, put_pixel_rows, and output_term.
 *
 * You must understand the distinction between full color output mode
 * (N independent color components) and colormapped output mode (a single
 * output component representing an index into a color map).  You should use
 * colormapped mode to write to a colormapped display screen or output file.
 * Colormapped mode is also useful for reducing grayscale output to a small
 * number of gray levels: when using the 1-pass quantizer on grayscale data,
 * the colormap entries will be evenly spaced from 0 to MAX_JSAMPLE, so you
 * can regard the indexes are directly representing gray levels at reduced
 * precision.  In any other case, you should not depend on the colormap
 * entries having any particular order.
 * To get colormapped output, set cinfo->quantize_colors to TRUE and set
 * cinfo->desired_number_of_colors to the maximum number of entries in the
 * colormap.  This can be done either in your main routine or in
 * d_ui_method_selection.  For grayscale quantization, also set
 * cinfo->two_pass_quantize to FALSE to ensure the 1-pass quantizer is used
 * (presently this is the default, but it may not be so in the future).
 *
 * The output file writing modules (jwrppm.c, jwrgif.c, jwrtarga.c, etc) may be
 * useful examples of what these routines should actually do, although each of
 * them is encrusted with a lot of specialized code for its own file format.
 */


#ifdef PROTOTYPE
GLOBAL VOID output_init (decompress_info_ptr cinfo)
#else
GLOBAL VOID output_init (cinfo)
decompress_info_ptr cinfo;
#endif
/* This routine should do any setup required */
{
  /* This routine can initialize for output based on the data passed in cinfo.
   * Useful fields include:
   *	image_width, image_height	Pretty obvious, I hope.
   *	data_precision			bits per pixel value; typically 8.
   *	out_color_space			output colorspace previously requested
   *	color_out_comps			number of color components in same
   *	final_out_comps			number of components actually output
   * final_out_comps is 1 if quantize_colors is true, else it is equal to
   * color_out_comps.
   *
   * If you have requested color quantization, the colormap is NOT yet set.
   * You may wish to defer output initialization until put_color_map is called.
   */
}


/*
 * This routine is called if and only if you have set cinfo->quantize_colors
 * to TRUE.  It is given the selected colormap and can complete any required
 * initialization.  This call will occur after output_init and before any
 * calls to put_pixel_rows.  Note that the colormap pointer is also placed
 * in a cinfo field, whence it can be used by put_pixel_rows or output_term.
 * num_colors will be less than or equal to desired_number_of_colors.
 *
 * The colormap data is supplied as a 2-D array of JSAMPLEs, indexed as
 *		JSAMPLE colormap[component][indexvalue]
 * where component runs from 0 to cinfo->color_out_comps-1, and indexvalue
 * runs from 0 to num_colors-1.  Note that this is actually an array of
 * pointers to arrays rather than a true 2D array, since C does not support
 * variable-size multidimensional arrays.
 * JSAMPLE is typically typedef'd as "unsigned char".  If you want your code
 * to be as portable as the JPEG code proper, you should always access JSAMPLE
 * values with the GETJSAMPLE() macro, which will do the right thing if the
 * machine has only signed chars.
 */

#ifdef PROTOTYPE
GLOBAL VOID put_color_map (decompress_info_ptr cinfo, int num_colors,
        JSAMPARRAY colormap)
#else
GLOBAL VOID put_color_map (cinfo, num_colors, colormap)
decompress_info_ptr cinfo;
int num_colors;
JSAMPARRAY colormap;
#endif
/* Write the color map */
{
#ifndef OLD_WAY
#ifdef QAK
  /* You need not provide this routine if you always set cinfo->quantize_colors
   * FALSE; but a safer practice is to provide it and have it just print an
   * error message, like this:
   */
  fprintf(stderr, "put_color_map called: there's a bug here somewhere!\n");
#endif
#else
    uint8 *img_pal,     /* Pointer to an array to store the palette in */
        *tmp_pal;
    intn i;             /* local counting variable */
    register JSAMPROW ptr0, ptr1, ptr2;

    tmp_pal=img_pal=HDgetspace(768);
    if(img_pal==NULL)
        return;
    ptr0 = colormap[0];     /* this code assumes only 3 components, cuz thats */
    ptr1 = colormap[1];     /* all HDF handles at the moment... */
    ptr2 = colormap[2];
    for(i=0; i<num_colors; i++) {
        *tmp_pal++=GETJSAMPLE(*ptr0);   /* red */
        ptr0++;
        *tmp_pal++=GETJSAMPLE(*ptr1);   /* green */
        ptr1++;
        *tmp_pal++=GETJSAMPLE(*ptr2);   /* blue */
        ptr2++;
      } /* end for */
    DFR8Dsetreadpal(img_pal);
    HDfreespace(img_pal);
#endif
}


/*
 * This function is called repeatedly, with a few more rows of pixels supplied
 * on each call.  With the current JPEG code, some multiple of 8 rows will be
 * passed on each call except the last, but it is extremely bad form to depend
 * on this.  You CAN assume num_rows > 0.
 * The data is supplied in top-to-bottom row order (the standard order within
 * a JPEG file).  If you cannot readily use the data in that order, you'll
 * need an intermediate array to hold the image.  See jwrrle.c for an example
 * of outputting data in bottom-to-top order.
 *
 * The data is supplied as a 3-D array of JSAMPLEs, indexed as
 *		JSAMPLE pixel_data[component][row][column]
 * where component runs from 0 to cinfo->final_out_comps-1, row runs from 0 to
 * num_rows-1, and column runs from 0 to cinfo->image_width-1 (column 0 is
 * left edge of image).  Note that this is actually an array of pointers to
 * pointers to arrays rather than a true 3D array, since C does not support
 * variable-size multidimensional arrays.
 * JSAMPLE is typically typedef'd as "unsigned char".  If you want your code
 * to be as portable as the JPEG code proper, you should always access JSAMPLE
 * values with the GETJSAMPLE() macro, which will do the right thing if the
 * machine has only signed chars.
 *
 * If quantize_colors is true, then there is only one component, and its values
 * are indexes into the previously supplied colormap.  Otherwise the values
 * are actual data in your selected output colorspace.
 */


#ifdef PROTOTYPE
GLOBAL VOID
put_pixel_rows (decompress_info_ptr cinfo, int num_rows, JSAMPIMAGE pixel_data)
#else
GLOBAL VOID put_pixel_rows (cinfo, num_rows, pixel_data)
decompress_info_ptr cinfo;
int num_rows;
JSAMPIMAGE pixel_data;
#endif
/* Write some rows of output data */
{
  /* This example shows how you might write full-color RGB data (3 components)
   * to an output file in which the data is stored 3 bytes per pixel.
   */
    register JSAMPROW ptr0, ptr1, ptr2;
    register long col;
    register int row;

    if(img_scheme == DFTAG_JPEG) {  /* check for 24-bit image */
        for (row = 0; row < num_rows; row++) {
            ptr0 = pixel_data[0][row];
            ptr1 = pixel_data[1][row];
            ptr2 = pixel_data[2][row];
            for (col = 0; col < cinfo->image_width; col++) {
                *img_ptr++=GETJSAMPLE(*ptr0);   /* red */
                ptr0++;
                *img_ptr++=GETJSAMPLE(*ptr1);   /* green */
                ptr1++;
                *img_ptr++=GETJSAMPLE(*ptr2);   /* blue */
                ptr2++;
              } /* end for */
          } /* end for */
      } /* end if */
    else {      /* must be 8-bit image */
        for (row = 0; row < num_rows; row++) {
            ptr0 = pixel_data[0][row];
            for (col = 0; col < cinfo->image_width; col++) {
                *img_ptr++=GETJSAMPLE(*ptr0);
                ptr0++;
              } /* end for */
          } /* end for */
      } /* end else */
}   /* end put_pixel_rows() */


#ifdef PROTOTYPE
GLOBAL VOID output_term (decompress_info_ptr cinfo)
#else
GLOBAL VOID output_term (cinfo)
decompress_info_ptr cinfo;
#endif
/* Finish up at the end of the output */
{
  /* This termination routine may not need to do anything. */
  /* Note that the JPEG code will only call it during successful exit; */
  /* if you want it called during error exit, you gotta do that yourself. */
}


/*
 * That's it for the routines that deal with writing the output image.
 * Now we have overall control and parameter selection routines.
 */


/*
 * This routine gets control after the JPEG file header has been read;
 * at this point the image size and colorspace are known.
 * The routine must determine what output routines are to be used, and make
 * any decompression parameter changes that are desirable.  For example,
 * if it is found that the JPEG file is grayscale, you might want to do
 * things differently than if it is color.  You can also delay setting
 * quantize_colors and associated options until this point. 
 *
 * j_d_defaults initializes out_color_space to CS_RGB.  If you want grayscale
 * output you should set out_color_space to CS_GRAYSCALE.  Note that you can
 * force grayscale output from a color JPEG file (though not vice versa).
 */

#ifdef PROTOTYPE
GLOBAL VOID d_ui_method_selection (decompress_info_ptr cinfo)
#else
GLOBAL VOID d_ui_method_selection (cinfo)
decompress_info_ptr cinfo;
#endif
{
  /* if grayscale input, force grayscale output; */
  /* else leave the output colorspace as set by main routine. */
    if (cinfo->jpeg_color_space == CS_GRAYSCALE)
        cinfo->out_color_space = CS_GRAYSCALE;

    if(img_scheme==DFTAG_GREYJPEG) {    /* for 8-bit raster output */
        cinfo->quantize_colors=TRUE;
        cinfo->desired_number_of_colors=256;
      } /* end if */

  /* select output routines */
    cinfo->methods->output_init = output_init;
    cinfo->methods->put_color_map = put_color_map;
    cinfo->methods->put_pixel_rows = put_pixel_rows;
    cinfo->methods->output_term = output_term;
}

/**********************************************************************/
/* HDF callable routine for reading in an image with JPEG compression */
/**********************************************************************/

/*-----------------------------------------------------------------------------
 * Name:    DFCIunjpeg
 * Purpose: decompress an image using the JPEG compression algorithm
 * Inputs:
 * Returns: 0 on success, -1 on failure
 * Users:   HDF programmers, DFputcomp, other routines
 * Invokes: JPEG library routines (lots of them...)
 * Remarks: Uses the JPEG library routines.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFCIunjpeg(int32 file_id, uint16 tag, uint16 ref, VOIDP image, int32 xdim,
    int32 ydim, int16 scheme)
#else
intn DFCIunjpeg(file_id, tag, ref, image, xdim, ydim, scheme)
    int32 file_id;
    uint16 tag;
    uint16 ref;
    VOIDP image;
    int32 xdim;
    int32 ydim;
    int16 scheme;
#endif
{
  /* These three structs contain JPEG parameters and working data.
   * They must survive for the duration of parameter setup and one
   * call to jpeg_decompress; typically, making them local data in the
   * calling routine is the best strategy.
   */
    struct Decompress_info_struct cinfo;
    struct Decompress_methods_struct dc_methods;
    struct External_methods_struct e_methods;

    img_file_id=file_id;    /* keep the file ID around */
    img_tag=tag;        /* keep dataset's tag around */
    img_ref=ref;        /* keep reference number around */
    img_ptr=(uint8 *)image;  /* Set the static pointer to the image to read */
    img_xdim=xdim;      /* Keep local copies of the X and Y dimensions */
    img_ydim=ydim;
    img_scheme=(intn)scheme;  /* Type of image compression we are going to do */

    /* Initialize the system-dependent method pointers. */
    cinfo.methods = &dc_methods;  /* links to method structs */
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

    /* If the decompressor requires full-image buffers (for two-pass color
     * quantization or a noninterleaved JPEG file), it will create temporary
     * files for anything that doesn't fit within the maximum-memory setting.
     * You can change the default maximum-memory setting by changing
     * e_methods.max_memory_to_use after jselmemmgr returns.
     * On some systems you may also need to set up a signal handler to
     * ensure that temporary files are deleted if the program is interrupted.
     * (This is most important if you are on MS-DOS and use the jmemdos.c
     * memory manager back end; it will try to grab extended memory for
     * temp files, and that space will NOT be freed automatically.)
     * See jcmain.c or jdmain.c for an example signal handler.
     */

    /* Here, set up the pointer to your own routine for post-header-reading
     * parameter selection.  You could also initialize the pointers to the
     * output data handling routines here, if they are not dependent on the
     * image type.
     */
    dc_methods.d_ui_method_selection = d_ui_method_selection;

    /* Set up default decompression parameters. */
    j_d_defaults(&cinfo, TRUE);
    /* TRUE indicates that an input buffer should be allocated.
     * In unusual cases you may want to allocate the input buffer yourself;
     * see jddeflts.c for commentary.
     */

    /* At this point you can modify the default parameters set by j_d_defaults
     * as needed; for example, you can request color quantization or force
     * grayscale output.  See jdmain.c for examples of what you might change.
     */

    /* Set up to read a HDF file containing JFIF or baseline-JPEG file. */
    /* This is the only JPEG file format currently supported. */
#ifdef QAK
    jselrjfif(&cinfo);
#else
    jselrhdf(&cinfo);
#endif

    /* Here we go! */
    jpeg_decompress(&cinfo);

#ifdef QAK
    /* That's it, son.  Nothin' else to do, except close files. */
    /* Here we assume only the input file need be closed. */
    fclose(cinfo.input_file);
#endif

    return(SUCCEED);     /* we must be ok... */
}   /* end DFCIunjpeg() */

