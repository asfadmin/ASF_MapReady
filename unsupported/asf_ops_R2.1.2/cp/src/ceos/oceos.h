/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1993, California Institute of Technology. U.S.              */
/* Government Sponsorship under NASA Contract NAS7-918 is acknowledged.      */
/*                                                                           */
/* "This software is provided to you "as is" and without warranty of         */
/*  any kind, expressed or implied, including but not limited to,            */
/*  warranties of performance, merchantability, or fitness for a             */
/*  particular purpose.  User bears all risk as to the quality and           */
/*  performance of the software."                                            */
/*                                                                           */
/*****************************************************************************/
/******************************************************************************
*                                                                             *
*                                                                             *
* Module:   oceos.h                          Program:   SIR-C GDPS            *
*                                            Author:    P. Barrett            *
*                                            Initiated: 03-FEB-93             *
*                                                                             *
*  modified for ASF Radarsat program:                                         *
*                                            Author:    D. Cuddy, JPL         *
*                                            Date:      25-Sep-94             *
*                                            Date:      09-Aug-95             *
*  modified to add the CSA Detail Processing Parameter Record:                * 
*                                            Date:      13-Nov-95,dtc         *
*  modified to be in sync with Data Dictionary V1.1:                          *
*                                            Date:      28-Nov-95, dtc        *
*                                                                             *
* --------------------------------------------------------------------------- *
*                                                                             *
* Abstract:     Header file for oceos.c and ceos_rd.c                         *
*                                                                             *
* Description:  This file includes definitions and constants used by          *
*               oceos.c and ceos_rd.c                                         *
*                                                                             *
******************************************************************************/


static char sccsid_oceos_h[] =
        "@(#)oceos.h	1.10 96/10/01 14:24:36";

#ifndef OCEOS_H
#define OCEOS_H


/*
 * If building task ceos_rd, include ceos_rd.h.
 * If building task oceos, do not include ceos_rd.h
 */
#ifdef CEOS_READER
#include "ceos_rd.h"
#endif

/* Revision Control System ID */
static char oceos_h_rcsid[] = "@(#)$Id$";



/******************************************************************************
*                                                                             *
*                          Constants                                          *
*                                                                             *
******************************************************************************/


/* General Values */


#define  OPS_SW_VERSION        "1.2"        /* OPS CEOS RDR version number */


#define  KILO                  1024         /* One k bytes = 1024 bytes */
#define  MAX_LINE_LENGTH       (16 * KILO)  /* Max pixels/samples per line */
#define  MAX_BYTES_PER_PIXEL   10           /* Max bytes per pixel */
#define  MAX_MISSING_PAIRS     5            /* Max missing line pairs */



/*************************
*                        *
* Record Header Values   *
*                        *
*************************/
/* Common 2-nd record sub-type code in pre-radarsat is 31 */
#define COM2SUB        31

/* Volume Directory - File Descriptor Record */

#define VDSEQ          1
#define VD1SUB         192
#define VDTYPE         192
#define VD2SUB         18
#define VD3SUB         18

/* Volume Directory File - SAR Leader Pointer Record */

#define VLSEQ          VDSEQ + 1
#define VL1SUB         219
#define VLTYPE         192
#define VL2SUB         18
#define VL3SUB         18

/* Volume Directory File - Imagery Options Pointer Record */

#define VISEQ          VLSEQ + 1
#define VI1SUB         219
#define VITYPE         192
#define VI2SUB         18
#define VI3SUB         18

/* Volume Directory File - SAR Trailer Pointer Record */

#define VTSEQ          VISEQ + 1
#define VT1SUB         219
#define VTTYPE         192
#define VT2SUB         18
#define VT3SUB         18

/* Volume Directory File - Text Record */

#define VXSEQ          VTSEQ + 1
#define VX1SUB         18
#define VXTYPE         63
#define VX2SUB         18
#define VX3SUB         18

/* SAR Leader File - File Descriptor Record */

#define LDSEQ          1
#define LD1SUB         63
#define LDTYPE         192
#define LD2SUB         18
#define LD3SUB         18

/* SAR Leader File - Data Set Summary Record */

#define LSSEQ          LDSEQ + 1
#define LS1SUB         10
#define LSTYPE         10
#define LS2SUB         18
#define LS3SUB         20

/* SAR Leader File - Map Projection Data Record */

#define LMSEQ         LSSEQ + 1
#define LM1SUB        10
#define LMTYPE        20
#define LM2SUB        18
#define LM3SUB        20

/* SAR Leader File - Platform Position Record */

#define LPSEQ         LMSEQ + 1
#define LP1SUB        10
#define LPTYPE        30
#define LP2SUB        18
#define LP3SUB        20

/* SAR Leader File - Attitude Data Record */

#define LASEQ         LPSEQ + 1
#define LA1SUB        10
#define LATYPE        40
#define LA2SUB        18
#define LA3SUB        20

/* SAR Leader File - Radiometric Data Record */

#define LRSEQ         LASEQ + 1
#define LR1SUB        10
#define LRTYPE        50
#define LR2SUB        18
#define LR3SUB        20

/* SAR Leader File - Radiometric Compensation Record */

#define LCSEQ         LRSEQ + 1
#define LC1SUB        10
#define LCTYPE        51
#define LC2SUB        18
#define LC3SUB        20

/* SAR Leader File - Data Quality Summary Record */

#define LQSEQ         LCSEQ + 1
#define LQ1SUB        10
#define LQTYPE        60
#define LQ2SUB        18
#define LQ3SUB        20

/* SAR Leader File - Data Histograms Record */

#define LHSEQ        LQSEQ + 1
#define LH1SUB       10
#define LHTYPE       70
#define LH2SUB       18
#define LH3SUB       20

/* SAR Leader File - Range Spectra Record */

#define LZSEQ        LHSEQ + 1
#define LZ1SUB       10
#define LZTYPE       80
#define LZ2SUB       18
#define LZ3SUB       20

/* SAR Leader File - Digital Elevation Data Record */

#define LESEQ        LZSEQ + 1
#define LE1SUB       10
#define LETYPE       90
#define LE2SUB       18
#define LE3SUB       20

/* SAR Leader File - Radar Parameter Update Data Record */

#define LUSEQ        LESEQ + 1
#define LU1SUB       10
#define LUTYPE       100
#define LU2SUB       18
#define LU3SUB       20

/* SAR Leader File - Annotation Data Record */

#define LNSEQ        LUSEQ + 1
#define LN1SUB       10
#define LNTYPE       110
#define LN2SUB       18
#define LN3SUB       20

/* SAR Leader File - Detailed Processing Parameters Data Record */

#define LYSEQ        LNSEQ + 1
#define LY1SUB       90           /*** 10 ***/
#define LYTYPE       120
#define LY2SUB       18
#define LY3SUB       61           /*** 20 ***/

/* SAR Leader File - Calibration Data Record */

#define LBSEQ        LYSEQ + 1
#define LB1SUB       90           /*** 10 ***/
#define LBTYPE       130
#define LB2SUB       18
#define LB3SUB       20

/* SAR Leader File - Ground Control Points Data Record */

#define LGSEQ        LBSEQ + 1
#define LG1SUB       10
#define LGTYPE       140
#define LG2SUB       18
#define LG3SUB       20

#ifdef PRE_RADARSAT
/* SAR Leader File - Facility Related Data Record - pre Radarsat era */

#define LFSEQ        LGSEQ + 1
#define LF1SUB       90
#define LFTYPE       200
#define LF2SUB       18
#define LF3SUB       61

/* SAR Leader File - Facility Related Data Record ICE MOTION */

#define LF1ASUB       10
#define LIMTYPE       201

/* SAR Leader File - Facility Related Data Record ICE CLASS */

#define LICTYPE       202


/* SAR Leader File - Facility Related Data Record WAVE MOTION */

#define LWMTYPE       203
#else
/* SAR Leader File - Facility Related Data Record - Radarsat era  */

#define LFSEQ        LGSEQ + 1
#define LF1SUB       90
#define LFTYPE       210
#define LF2SUB       18
#define LF3SUB       61

#endif

/* Imagery Options File - File Descriptor Record */

#define IDSEQ        1
#define ID1SUB       63
#define IDTYPE       192
#define ID2SUB       18
#define ID3SUB       18

/* Imagery Options File - Reformatted Signal Data Record */

#define IRSEQ        IDSEQ + 1
#define IR1SUB       50
#define IRTYPE       10
#define IR2SUB       18
#define IR3SUB       20

/* Imagery Options File - Image Data Record */

#define IISEQ       IDSEQ + 1
#define II1SUB      50
#define IITYPE      11
#define II2SUB      18
#define II3SUB      20

/* SAR Trailer File - File Descriptor Record */

#define TDSEQ       1
#define TD1SUB      63
#define TDYTYPE     192
#define TD2SUB      18
#define TD3SUB      18

/* Null Volume Directory File - File Descriptor Record */

#define NDSEQ       1
#define ND1SUB      192
#define NDTYPE      192
#define ND2SUB      63
#define ND3SUB      18



/* Miscellaneous Constants */

#define LEFT_LOOK_DEG   -90.0
#define RIGHT_LOOK_DEG   90.0
#define POL_MATRIX_SIZE  32     /* size of 4 x 4 pol cal (complex) matrix */

/* Polarization Masks */

#define HH_MASK         1
#define HV_MASK         2
#define VV_MASK         4
#define VH_MASK         8

/* SAR Leader File Descriptor Record */
#define CEOS_SW_ID              1.1
#define SARL_FILE_NO            3
#define EMPTY_RECORD            0
#define N_SUMMARY_RECS          1
#define N_MAP_RECS              1
#define N_PLATFORM_RECS         1
#define N_ATTITUDE_RECS         1
#define N_ELEVATION_RECS        0      /* record not used by SIR-C */
#define N_ANNOTATION_RECS       0      /* record not used by SIR-C */
#define N_DET_PROC_REC          1
#define N_CALIBRATION_RECS      1
#define N_GROUND_PTS_RECS       0      /* record not used by SIR-C */
#define N_FACILITY_RECS         0      /* record not used by SIR-C */

/* Data Set Summary Record Constants */

#define DSS_LCF          1.0    /* linear conversion factor */

/* Radiometric Data Record Constants */

#define RADIO_N_SETS     1      /* number of data sets (fixed for SIR-C) */
#define RAD_ARRY_SZ      0      /* no table data provided */
#define RADIO_N_SAMPLES  0      /* number of samples */
#define RADIO_LCF        1.0    /* linear conversion factor */
#define RADIO_OCF        0      /* offset conversion factor */


/* Radiometric Compensation Data Record Constants */

#define RADIOC_N_SETS    1      /* number of data sets */
#define RADIO_COMP_N_REQ 1      /* number of recs to reconstitute table */
#define RADIO_TABLE_NO   1      /* table sequence number */
#define CORRES_1ST_SAMP  1      /* slant rg sample corresp to 1st correction */
#define RADIO_COMP_GP_SZ 1      /* sample group size */
#define MIN_SAMP_INDEX   1.0    /* minimum sample index */


/* Histogram Data Record Constants */

#define HIST_N_SETS      2      /* # of data sets: 1 raw, 1 image */
#define RAW_HISTOGRAM    0      /* raw histo is 1st data set */
#define IMAGE_HISTOGRAM  1      /* image histo is 2nd data set */
#define HIST_REQ_RECS    1      /* number of recs to reconstitute table */
#define HIST_TABLE_NO    1      /* table sequence number */
#define RAWHIST_MIN_VAL  0.0    /* minimum value in raw histogram */

/* Data Quality Summary Record Constants */

#define QUAL_N_CHAN      1      /* number of channels */
#define SEQN_LOC         1      /* sequence number */
#define SEQ_FLD_LEN      4      /* sequence field length */
#define CODE_LOC         SEQN_LOC + SEQ_FLD_LEN
#define CODE_FLD_LEN     4      /* recode code field length */
#define LEN_LOC          CODE_LOC + CODE_FLD_LEN
#define LEN_FLD_LEN      4

/* Range Spectra Record Constants */

#define SPECTRA_N_REQ    1      /* number of records required */
#define SPECTRA_SET_SZ   256    /* data set size */
#define SPECTRA_TABLE_NO 1      /* table sequence number */


/* Parameter Update Record Constants */

#define UPD_PIXEL_NUMBER 0      /* Pixel no. of parameter change */


/* Imagery Options: File Descriptor Record Constants */

#define RSD_BITS_SAMPLE  8      /* RSD bits per sample */
#define MLD_BITS_SAMPLE  16     /* MLD bits per sample */
#define MLD_PIXELS_GP    1      /* MLD, pixels per data group */
#define MLC_PIXELS_GP_DL 2      /* MLC, pixels per data group, dual pol */
#define MLC_PIXELS_GP_QD 3      /* MLC, pixels per data group, quad pol */
#define SLC_PIXELS_GP_SG 1      /* SLC, pixels per data group, single pol */
#define SLC_PIXELS_GP_DL 2      /* SLC, pixels per data group, dual pol */
#define SLC_PIXELS_GP_QD 4      /* SLC, pixels per data group, quad pol */
#define RSD_PIXELS_GP    1      /* RSD, pixels per data group */
#define MLD_BYTES_GP     2      /* MLD, bytes per group */
#define MLC_DUAL_BYTES   5      /* MLC, dual pol, bytes per group */
#define MLC_QUAD_BYTES   10     /* MLC, quad pol, bytes per group */
#define SLC_SING_BYTES   4      /* SLC, single pol, bytes per group */
#define SLC_DUAL_BYTES   6      /* SLC, dual pol, bytes per group */
#define SLC_QUAD_BYTES   10     /* SLC, quad pol, bytes per group */
#define RSD_BYTES_GP     1      /* RSD, bytes per group */
#define LEFT_BORDER_PIX  0      /* number of left border pixels */
#define RIGHT_BORDER_PIX 0      /* number of right border pixels */
#define TOP_BORDER_PIX   0      /* number of top border pixels */
#define BOTTOM_BD_PIX    0      /* number of bottom border pixels */
#define N_RECS_LINE      1      /* number of physical recs per line */
#define N_RECS_CHANNEL   1      /* n of records per multichannel line */
#define IMG_PREFIX_LEN   0      /* length of prefix data per line */
#define IMG_SUFFIX_LEN   0      /* length of suffix data per line */
#define N_LEFT_FILL      0      /* number of left fill bits per pixel */
#define N_RIGHT_FILL     0      /* number of right fill bits per pixel */


/* Strings */

#define YES_STR          "YES"
#define NO_STR           "NO"
#define ASCII_STR        "A"
#define CEOS_DOC         "CEOS-SAR-CCT"
#define DOC_VERS         "A"
#define REV_LEVEL        "A"
#define SEQ_FLAG         "FSEQ"
#define LOC_FLAG         "FTYP"
#define REC_LOC_FLAG     "FLGT"
#define MOTION_COMP      "00"
#define PROC_FACILITY    "JPL"
#define NONE             "NONE"
#define COS_SQUARE       "COS SQUARE PLUS %01.2f PEDESTAL HT"
#define SIRC_STR         "SIR-C"
#define HYPHEN_STR       "-"
#define LSTR             "L"
#define CSTR             "C"
#define XSTR             "X"
#define RES_STR          "HI"
#define TX_H             "H"
#define TX_V             "V"
#define TX_HV            "HV"
#define RX_H             "H"
#define RX_V             "V"
#define RX_HV            "HV"
#define ANAL_CHIRP_STR   "ANALYTIC CHIRP"
#define DIGIT_CHIRP_STR  "DIGITAL CHIRP"
#define MEAS_CHIRP_STR   "MEASURED CHIRP"
#define BIT_QUANT_STR    "UNIFORM"
#define BFPQ_STR         "(8,4)BFPQ"
#define OFF_STR          "OFF"
#define ON_STR           "ON"
#define MLD_STR          "MULTI-LOOK DETECTED"
#define MLC_STR          "MULTI-LOOK COMPLEX"
#define SLC_STR          "SINGLE-LOOK COMPLEX"
#define RSD_STR          "REFORMATTED SIGNAL DATA"
#define PROC_ALGOR_STR   "FREQUENCY DOMAIN CONVOLUTION"
#define INCR_STR         "INCREASE"
#define DECR_STR         "DECREASE"
#define RANGE_STR        "RANGE"
#define BIAS_STR         "0"
#define GAIN_STR         "1"
#define ANNOT_PTS_STR    "0"
#define GND_RG_STR       "GROUND RANGE"
#define SLT_RG_STR       "SLANT RANGE"
#define CROSS_PROD_STR   "CROSS PRODUCTS"
#define SCAT_MTX_STR     "SCATTERING MATRX"
#define NOT_APPL_STR     "N/A"
#define RAW_DATA_HIST    "RAW DATA"
#define IMAGE_DATA_HIST  "IMAGE DATA"
#define DWP_DESCR_STR    "DATA WINDOW POSITION"
#define GAIN_DESCR_STR   "RECEIVER GAIN"
#define ALL_POLS_STR     "ALL POLS"
#define EME_STR          "EME"
#define ARIES_STR        "ARIES MEAN OF 1950 CARTESIAN"
#define GREENWICH_STR    "GREENWICH TRUE OF DATE"
#define COMPENS_DESCR    "RG RADIOMETRIC CORRECTION VECTOR"
#define ELEV_ANT_PAT     "ELEVATION ANTENNA PATTERN"
#define FLIGHT_1         "SRL-1"
#define FLIGHT_2         "SRL-2"
#define FLIGHT_3         "SRL-3"
#define FLIGHT_4         "SRL-4"
#define BITS_PER_SAMPLE  " "
#define INTERLEAVING_IND "BSQ"
#define POWER_DETECTED   "POWER DETECTED"
#define COMPR_CROSS_PROD "COMPRESSED CROSS-PRODUCTS"
#define COMPR_SCAT_MATRIX "COMPRESSED SCATTERING MATRIX"
#define REAL_BYTE        "REAL BYTE"
#define VARIES           "VARIES"
#define ASCENDING_STR    "ASCENDING"
#define DESCENDING_STR   "DESCENDING"


/* SAR Channels */

#define XVV              00
#define LVV_LHV          10     /* L - Pol 9 */
#define LHH              11     /* L - Pol 0 */
#define LHV              12     /* L - Pol 1 */
#define LVV              13     /* L - Pol 2 */
#define LVH              14     /* L - Pol 3 */
#define LQUAD            15     /* L - Pol 4 */
#define LHH_LHV          16     /* L - Pol 5 */
#define LVV_LVH          17     /* L - Pol 6 */
#define LHH_LVV          18     /* L - Pol 7 */
#define LHH_LVH          19     /* L - Pol 8 */
#define CVV_CHV          20     /* C - Pol 9 */
#define CHH              21     /* C - Pol 0 */
#define CHV              22     /* C - Pol 1 */
#define CVV              23     /* C - Pol 2 */
#define CVH              24     /* C - Pol 3 */
#define CQUAD            25     /* C - Pol 4 */
#define CHH_CHV          26     /* C - Pol 5 */
#define CVV_CVH          27     /* C - Pol 6 */
#define CHH_CVV          28     /* C - Pol 7 */
#define CHH_CVH          29     /* C - Pol 8 */
#define LHV_LVH          30     /* L - Pol 10 */
#define CHV_CVH          31     /* C - Pol 10 */
/* Product Level Codes */

#define RSD_PROD_CODE    "0.0"
#define SLC_PROD_CODE    "1.0"
#define ML_PROD_CODE     "1.5"


/******************************************************************************
*                                                                             *
*                 Structures which define CEOS Data Records                   *
*                                                                             *
******************************************************************************/


/***************************************************
*                                                  *
*                      Header                      *
*                                                  *
* ..the 12-byte preamble at the beginning of every *
* CEOS data record.  These are the only binary     *
* values stored in the CEOS files.  Everything     *
* else is ASCII.                                   *
*                                                  *
***************************************************/

typedef struct descp_rec{
    long  rec_seq;     /* record sequence number */
    unsigned char  rec_sub1;    /* first record subtype */
    unsigned char  rec_type;    /* record type code */
    unsigned char  rec_sub2;    /* second record subtype */
    unsigned char  rec_sub3;    /* third record subtype */
    long  length;      /* length of this record */

} desc_rec;


/****************************
*                           *
*  Volume Directory File    *
*  Volume Descriptor Record *
*                           *
****************************/

typedef struct  vol_desc_rec {
    desc_rec desc;              /* header info */
    char  ascii_flag[3];        /* ASCII flag */
    char  spare_vdr_1[3];       /* spare */
    char  format_doc[13];       /* control document number */
    char  format_ver[3];        /* revision number (00-99) */
    char  format_rev[3];        /* file design revision number */
    char  software_id[13];      /* software ID */
    char  media_id[17];         /* physical tape ID */
    char  logvol_id[17];        /* logical set ID */
    char  volset_id[17];        /* volume set ID */
    short phyvol_cnt;           /* number of physical volumes */
    short first_phyvol;         /* first physical vol sequence number */
    short last_phyvol;          /* last physical vol sequence number */
    short curr_phyvol;          /* this physical vol number */
    short first_file;           /* first reference file in volume */
    short volset_log;           /* logical volume in set */
    short phyvol_log;           /* logical volume in physical volume */
    char  logvol_date[9];       /* tape creation date */
    char  logvol_time[9];       /* tape creation time */
    char  logvol_country[13];   /* tape creating country */
    char  logvol_agency[9];     /* tape creating agency */
    char  logvol_facility[13];  /* tape creating facility */
    short n_filepoint;          /* number of pointer records */
    short n_voldir;             /* number of records */
    short n_logvol;             /* number of log vol in the set */
    char product_id[17];        /* product id */
    char  spare_vdr_2[93];      /* spare */
    char  spare_vdr_3[101];     /* spare */

} Vol_Desc_Rec;


/****************************
*                           *
*  Volume Directory File    *
*  File Pointer Record      *
*                           *
****************************/

typedef struct file_ptr_rec {
    desc_rec desc;              /* header info */
    char  ascii_flag[3];        /* ascii flag */
    char  spare_fpr_1[3];       /* spare */
    short file_num;             /* file number */
    char  product_id[17];       /* filename = product id */
    char  file_class[29];       /* file class */
    char  file_code[5];         /* file class code */
    char  data_type[29];        /* data type */
    char  data_code[5];         /* data type code */
    long  nrec;                 /* number of records */
    long  first_len;            /* first record length */
    long  max_len;              /* max record length */
    char  len_type[13];         /* record type */
    char  len_code[5];          /* record type code */
    short first_phyvol;         /* start file volume number */
    short last_phyvol;          /* end file volume number */
    long  first_rec;            /* first record number on tape */
    long  last_rec;             /* last record number on tape */
    char  spare_fpr_2[101];     /* spare */
    char  spare_fpr_3[101];     /* spare */

} File_Ptr_Rec;


/****************************
*                           *
*  Volume Directory File    *
*  Text Record              *
*                           *
****************************/

typedef struct text_rec {
    desc_rec desc;              /* header info */
    char  ascii_flag[3];        /* ascii flag */
    char  cont_flag[3];         /* Continuation flag */
    char  product_type[41];     /* product type specifier */
    char  product_create[61];   /* location, date/time prod creation */
    char  phyvol_ids[41];       /* physical volume ID */
    char  scene_id[41];         /* scene ID */
    char  scene_loc[41];        /* scene location */
    char  copyright_info[21];   /* copyright info */
    char  spare_text_2[105];    /* spare */

} Text_Rec;


/****************************
*                           *
*  SAR Leader File:         *
*  File Descriptor Record   *
*                           *
****************************/

typedef struct sarl_desc_rec {
    desc_rec desc;              /* header info */
    char  ascii_flag[3];        /* ASCII flag */
    char  spare_fdr_1[3];       /* spare */
    char  format_doc[13];       /* document format */
    char  format_rev[3];        /* format revision ("A") */
    char  design_rev[3];        /* record format revision level */
    char  software_id[13];      /* software ID */
    short file_num;             /* file number */
    char  product_id[17];       /* filename = product id */
    char  rec_seq_flag[5];      /* record sequence & location type flag */
    long  seq_loc;              /* sequence number location */
    short seq_len;              /* sequence number field length */
    char  rec_code[5];          /* record code and location type flag */
    long  code_loc;             /* record code location */
    short code_len;             /* record code field length */
    char  rec_len[5];           /* record length and location type flag */
    long  rlen_loc;             /* record length location */
    short rlen_len;             /* record length field length */
    char  spare_fdr_2[5];       /* spare */
    char  spare_fdr_3[65];      /* OPS Reader s/w version */
    long  n_dataset;            /* number of data set summary records */
    long  l_dataset;            /* record length */
    long  n_map_proj;           /* number of map projection data records */
    long  l_map_proj;           /* record length */
    long  n_plat_pos;           /* number of platform position records */
    long  l_plat_pos;           /* record length */
    long  n_att_data;           /* number of attitude data records */
    long  l_att_data;           /* record length */
    long  n_radi_data;          /* number of radiometric data records */
    long  l_radi_data;          /* record length */
    long  n_radi_comp;          /* number of radio compensation records */
    long  l_radi_comp;          /* record length */
    long  n_qual_sum;           /* number of data quality summary records */
    long  l_qual_sum;           /* record length */
    long  n_data_hist;          /* number of data histogram records */
    long  l_data_hist;          /* record length */
    long  n_rang_spec;          /* number of range spectra records */
    long  l_rang_spec;          /* record length */
    long  n_dem_desc;           /* number of digital elevation model recs */
    long  l_dem_desc;           /* record length */
    long  n_radar_par;          /* number of radar parameter update recs */
    long  l_radar_par;          /* record length */
    long  n_anno_data;          /* number of annotation data records */
    long  l_anno_data;          /* record length */
    long  n_det_proc;           /* number of detailed processing records */
    long  l_det_proc;           /* record length */
    long  n_cal;                /* number of calibration data records */
    long  l_cal;                /* record length */
    long  n_gcp;                /* number of ground control pts records */
    long  l_gcp;                /* record length */
    char  spare_fdr_4[10];      /* spare */
    long  n_fac_data;           /* number of facility data records */
    long  l_fac_data;           /* record length */
    char  spare_fdr_5[289];     /* spare */

} Sarl_Desc_Rec;


/****************************
*                           *
*  SAR Leader File:         *
*  Data Set Summary Record  *
*                           *
****************************/

typedef struct dataset_sum_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* Data Set Summary: record sequence number */
    short sar_chan;             /* SAR channel indicator */
    char  product_id[17];       /* site ID = product id */
    char  scene_des[33];        /* site name */
    char  inp_sctim[33];        /* image center GMT: YYYYMMDDhhmmssttt */
    char  asc_des[17];          /* Ascending/descending */
    float pro_lat;              /* latitude at scene center */
    float pro_long;             /* longitude at scene center */
    float pro_head;             /* Processed scene center heading */
    char  ellip_des[17];        /* ellipsoid designator */
    float ellip_maj;            /* ellipsoid semimajor axis (km) */
    float ellip_min;            /* ellipsoid semiminor axis (km) */
    float earth_mass;           /* Earth's mass */
    float grav_const;           /* Gravitational constant */
    float ellip_j[3];           /* Ellipsoid J2-4 parameters */
#ifndef PRE_RADARSAT
    char  spare2[17];           /* spare */
#endif
    float terrain_h;            /* average terrain height */
    long  sc_lin;               /* image center line number (azimuth) */
    long  sc_pix;               /* image center pixel number (range) */
    float scene_len;            /* image length in km */
    float scene_wid;            /* image width in km */
#ifdef PRE_RADARSAT
    char  spare2[17];           /* spare */
#else
    char  spare_dss_3[17];      /* spare */
#endif
    short nchn;                 /* number of SAR channels */
#ifdef PRE_RADARSAT
    char  spare3[5];            /* spare */
#else
    char  spare_dss_5[5];       /* spare */
#endif
    char  mission_id[17];       /* mission id */
    char  sensor_id[33];        /* sensor id: AAAAAA-BB-CCDD-EEFF */
    char  revolution[9];        /* orbit number */
    float plat_lat;             /* spacecraft latitude at nadir */
    float plat_long;            /* spacecraft longitude at nadir */
    float plat_head_scene;      /* sensor platform heading (degrees) */
    float clock_ang;            /* sensor clock angle rel to flight dir */
    float incident_ang;         /* incidence angle at image center */
    float frequency;            /* radar frequency (GHz) */
    float wave_length;          /* radar wavelength (m) */
    char  motion_comp[3];       /* motion compensation indicator */
    char  pulse_code[17];       /* range pulse code specifier */
    float ampl_coef[5];         /* range chirp coefficients */
    float phas_coef[5];         /* range phase coefficients */
    long  chirp_ext_ind;        /* chirp extraction index */
#ifdef PRE_RADARSAT  
    char  spare5[9];            /* spare */
#else
    char  spare_dss_6[9];       /* spare */
#endif
    float rng_samp_rate;        /* range complex sampling rate */
    float rng_gate;             /* range gate at early edge */
    float rng_length;           /* range pulse length */
    char  baseband_f[5];        /* base band conversion flag */
    char  rngcmp_f[5];          /* range compressed flag */
    float gn_polar;             /* receiver gain for like pol */
    float gn_cross;             /* receiver gain for cross pol */
    long  chn_bits;             /* quantization bits per channel */
    char  quant_desc[13];       /* quantizer description */
    float i_bias;               /* I channel DC bias */
    float q_bias;               /* Q channel DC bias */
    float iq_ratio;             /* I/Q channel ratio */
    float spare_dss_7;          /* spare */
    float spare_dss_8;          /* spare */
    float ele_sight;            /* electronic boresight */
    float mech_sight;           /* mechanical boresight */
    char  echo_track[5];        /* echo tracker flag */
    float prf;                  /* nominal PRF */
    float elev_beam;            /* antenna elevation 3dB beam width */
    float azi_beam;             /* antenna azimuth 3dB beam width */
    char  sat_bintim[17];       /* Satellite binary time */
    char  sat_clktim[33];       /* Satellite clock time */
    long  sat_clkinc;           /* Satellite clock increment */
#ifdef PRE_RADARSAT
    char  spare8[9];            /* spare */
#else
    char  spare_dss_9[9];       /* spare */
#endif
    char  fac_id[17];           /* processing facility */
    char  sys_id[9];            /* processing system */
    char  ver_id[9];            /* processor version */
    char  fac_code[17];         /* facility process code */
    char  lev_code[17];         /* product code */
    char  product_type[33];     /* product type */
    char  algor_id[33];         /* processing algorithm */
    float n_azilok;             /* number of looks in azimuth */
    float n_rnglok;             /* number of looks in range */
    float bnd_azilok;           /* bandwidth per look in azimuth */
    float bnd_rnglok;           /* bandwidth per look in range */
    float bnd_azi;              /* processor bandwidth (azimuth) */
    float bnd_rng;              /* processor bandwidth (range) */
    char  azi_weight[33];       /* weighting function (azimuth) */
    char  rng_weight[33];       /* weighting function (range) */
    char  data_inpsrc[17];      /* data input source: HDDC id */
    float rng_res;              /* nominal resolution (range) */
    float azi_res;              /* nominal resolution (azimuth) */
    float radi_stretch[2];      /* radiometric stretch terms (bias, gain) */
    float alt_dopcen[3];        /* along track Doppler freq terms */
#ifdef PRE_RADARSAT
    char  spare9[17];           /* spare */
#else
    char  spare_dss_10[17];     /* spare */
#endif
    float crt_dopcen[3];        /* cross track Doppler freq terms */
    char  time_dir_pix[9];      /* time direction (range) */
    char  time_dir_lin[9];      /* time direction (azimuth) */
    float alt_rate[3];          /* Along track Doppler rate terms */
#ifdef PRE_RADARSAT
    char  spare10[17];          /* spare */
#else
    char  spare_dss_12[17];     /* spare */
#endif
    float crt_rate[3];          /* Cross track Doppler rate terms */
#ifdef PRE_RADARSAT
    char  spare11[17];          /* spare */
#else
    char  spare_dss_13[17];     /* spare */
#endif
    char  line_cont[9];         /* line content indicator */
    char  clutterlock_flg[5];   /* clutter lock flag */
    char  auto_focus[5];        /* autofocussing flag */
    float line_spacing;         /* line spacing (m) */
    float pixel_spacing;        /* pixel spacing (m) */
    char  rngcmp_desg[17];      /* range compression designator */
#ifdef PRE_RADARSAT
    char  spare12[273];         /* spare */
    int   annot_pts;            /* number of annotation points */
    char  spare13[9];           /* spare */
    int   annot_line[64];       /* Line number of annotation start */
    int   annot_pixel[64];      /* Pixel number of annotation start */
    char  annot_text[64][37];   /* Annotation text */
    char  spare14[26];          /* spare */
#else
    char  spare_dss_14[17];     /* spare */
    char  spare_dss_15[17];     /* spare */
    int   no_beams;             /* number of beams */
    char  beam1[5];             /* beam 1 identifier */
    char  beam2[5];             /* beam 2 identifier */
    char  beam3[5];             /* beam 3 identifier */
    char  beam4[5];             /* beam 4 identifier */
    float prf1;                 /* PRF of beam 1 Hz */
    float prf2;                 /* PRF of beam 2 Hz */
    float prf3;                 /* PRF of beam 3 Hz */
    float prf4;                 /* PRF of beam 4 Hz */
    float rng_gate1;            /* range gate of beam 1 (usec) */
    float rng_gate2;            /* range gate of beam 2 (usec) */
    float rng_gate3;            /* range gate of beam 3 (usec) */
    float rng_gate4;            /* range gate of beam 4 (usec) */
    int   tot_pls_burst;        /* total pulses per burst */
    int   val_pls_burst;        /* valid pulses per burst */
    int   az_ovlp_nxt_img;      /* Range lines overlap in azimuth with next */
    int   rg_off_nxt_img;       /* pixel of offset in range with next */
    char  cal_params_file[33];  /* calibration parameter file */
    char  scan_results_file[33];/* scan results file */
    char  scanner_version[17];  /* scanner version */
    char  decode_version[17];   /* decode version */
    char  spare_dss_16[2126];   /* spare */
#endif
    struct dataset_sum_rec *next;
} Dataset_Sum;



/****************************
*                           *
*  SAR Leader File:         *
*  Map Projection Data Rec  *
*                           *
****************************/

typedef struct map_proj_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* Map Projection: record sequence number */
    char  spare_mpr_1[17];      /* spare */
    char  map_desc[33];         /* map projection descriptor */
    long  n_pixel;              /* number of pixels per line */
    long  n_line;               /* number of image lines */
    float pixel_spacing;        /* pixel spacing (m) */
    float line_spacing;         /* line spacing (m) */
    float osc_orient;           /* Output Scne cntr orientation */
    float orb_incl;             /* Orbital Inclination */
    float asc_node;             /* Ascending node */
    float isc_dist;             /* platform distance */
    float geo_alt;              /* geodetic altitude */
    float isc_vel;              /* ground speed at nadir */
    float plat_head;            /* platform heading */
    char  ref_ellip[33];        /* name of reference ellipsoid */
    float semi_major;           /* semimajor axis of ref ellipsoid */
    float semi_minor;           /* semiminor axis of ref ellipsoid */
    float datum_shift[3];       /* Datum shift parameters */
    float aux_datum_shift[3];   /* Aux Datum shift parameters */
    float scal_ellip;           /* Ellipsoid scale factor */
    char  projection[33];       /* Map projection descriptor */
    char  utm_desc[33];         /* UTM descriptor */
    char  utm_zone_sig[5];      /* UTM zone signature */
    float utm_east_orig;        /* Map origin, false easting */
    float utm_north_orig;       /* Map origin, false northing */
    float utm_cent_long;        /* Projection Center longitude */
    float utm_cent_lat;         /* Projection Center latitude */
    float utm_stand_par[2];     /* Standard parallels */
    float utm_scale;            /* Scale factor */
    char  ups_desc[33];         /* UPS descriptor */
    float ups_cent_long;        /* Projection Center longitude */
    float ups_cent_lat;         /* Projection Center latitude */
    float ups_scale;            /* Scale factor */
    char  nsp_desc[33];         /* NSP descriptor */
    float nsp_east_orig;        /* Map origin, false easting */
    float nsp_north_orig;       /* Map origin, false northing */
    float nsp_cent_long;        /* Projection center longitude */
    float nsp_cent_lat;         /* Projection center latitude */
    float nsp_stand_par[4];     /* Standard parllels */
    float nsp_stand_mer[3];     /* Standard meridians */
    char  spare_mpr_2[65];      /* spare 4X16 */ 
    float corner_ne[8];         /* corner northing/easting */
    float corner_ll[8];         /* corner latitude/longitude */
    float terr_height[4];       /* corner terrain height */
    double lp_conv_coef[8];     /* line/pixel coefficients */
    double mp_conv_coef[8];     /* Map proj coefficients */
    char  spare_mpr_3[37];      /* spare */
    struct map_proj_rec *next;
} Map_Proj;


/****************************
*                           *
*  SAR Leader File:         *
*  Platform Position Rec    *
*                           *
****************************/

/* data set structure */

typedef struct pos_vec_rec {    /* position vector record */
    double pos[3];              /* (X,Y,Z) position vector */
    double vel[3];              /* (X,Y,Z) velocity vector */
    struct pos_vec_rec *next;
} Pos_Vect_Rec;


/* main structure */

typedef struct pos_data_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* Platform Position: record sequence number */
    char  orbit_ele_desg[33];   /* Orbital elements designator */
    float orbit_ele[6];         /* orbital elements */
    short  ndata;               /* number of data sets */
    short  year;                /* year of first data point */
    short  month;               /* month of first data point */
    short  day;                 /* day of first data point */
    short  gmt_day;             /* day in year of first data point */
    double gmt_sec;             /* seconds in day of first data point */
    double data_int;            /* time interval between data points (s) */
    char  ref_coord[65];        /* reference coordinate system */
    double hr_angle;            /* GMT hour angle (degrees) */
    float alt_poserr;           /* Along track position error */
    float crt_poserr;           /* Cross track position error */
    float rad_poserr;           /* radial position error */
    float alt_velerr;           /* Along track velocity error */
    float crt_velerr;           /* Cross track velocity error */
    float rad_velerr;           /* Radial velocity error */
    Pos_Vect_Rec* pos_vect;     /* Data point position/velocity */
    char spare_ppr_1[243];      /* spare */
    struct pos_data_rec *next;
} Pos_Data;


/****************************
*                           *
*  SAR Leader File:         *
*  Attitude Data Record     *
*                           *
****************************/

/* data set structure */

typedef struct att_vect_rec {
    short gmt_day;              /* day in the year (GMT) */
    long  gmt_msec;             /* millisecond of the day (GMT) */
    short pitch_flag;           /* pitch data quality flag */
    short roll_flag;            /* roll data quality flag */
    short yaw_flag;             /* yaw data quality flag */
    float pitch;                /* pitch (degrees) */
    float roll;                 /* roll (degrees) */
    float yaw;                  /* yaw (degrees) */
    short pitch_rate_flag;      /* pitch rate data quality flag */
    short roll_rate_flag;       /* roll rate data quality flag */
    short yaw_rate_flag;        /* yaw rate data quality flag */
    float pitch_rate;           /* pitch rate (degrees/sec) */
    float roll_rate;            /* roll rate (degrees/sec) */
    float yaw_rate;             /* yaw_rate (degrees/sec) */
    struct att_vect_rec *next;  /* pointer to next attitude data set */
}  Att_Vect_Rec;


/* main structure */

typedef struct  att_data_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* sequence number of attitude */
    short npoint;               /* number of attitude data sets */
    Att_Vect_Rec* att_vect;     /* Data points */
    char spare_adr_1[641];      /* spare */
    struct att_data_rec *next;  /* pointer to list of attitude data sets */
} Att_Data;


/****************************
*                           *
*  SAR Leader File:         *
*  Radiometric Data Record  *
*    ASF specific format    *
*                           *
****************************/


typedef struct radi_data_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* radiometric data rec seq number */
    short n_data;               /* number of data sets */
    long  field_size;           /* radiometric data set size */
    char  chan_ind[5];          /* SAR channel indicator */
    char  spare_rdr_1[5];       /* spare */
    char  table_desig[25];      /* Table designator */
    long  n_samp;               /* no. of samples in look up table */
    char  samp_type[17];        /* sample type indicator */
    float noise_fact;           /* noise scale factor (a1) */
    float linear_conv_fact;     /* linear conversion factor (a2) */
    float offset_conv_fact;     /* offset conversion factor (a3) */
    char  spare_rdr_2[5];       /* spare */
    float lookup_tab[256];      /* 256 lookup values */
    struct radi_data_rec *next; /* ptr to next pol/ radi. data record */
} Radi_Data;


/****************************
*                           *
*  SAR Leader File:         *
*  Radiometric Comp Record  * Not in the ASF set for the Radarsat era *
*                           *
****************************/

/* table structure */
typedef struct radio_comp_tbl {
    float sample_offset;         /* compensation sample offset */
    float sample_gain;           /* compensation sample gain */
    struct radio_comp_tbl *next; /* link */
} Radio_Comp_Tbl;


/* data set structure */
typedef struct rad_comp_set {
    char  comp_data_type[8];    /* compensation data type designator */
    char  data_descr[33];       /* data descriptor */
    short req_recs;             /* number of required records */
    short table_seq_num;        /* table sequence number */
    long  num_pairs;            /* total number of tables */
    long  first_pixel;          /* pixel corresponding to first correction */
    long  last_pixel;           /* pixel corresponding to last correction */
    long  pixel_size;           /* pixel group size (subsample factor) */
    float min_samp_index;       /* minimum sample index */
    float min_comp_value;       /* minimum radiometric compensation value */
    float max_samp_index;       /* maximum sample index */
    float max_comp_value;       /* maximum radiometric compensation value */
    char  spare_rcr_2[17];      /* spare */
    long  n_table_entries;      /* number of comp pairs */
    Radio_Comp_Tbl  *tbl;       /* pointer to linked list */
    struct rad_comp_set *next;  /* pointer to next pol */
} Rad_Comp_Set;

/* main structure */

typedef struct radi_comp_rec {
    desc_rec desc;              /* header info */
    short    seq_num;           /* radio comp record seq number */
    short    sar_chan;          /* sar channel indicator */
    long     n_dset;            /* number of data sets */
    long     dset_size;         /* data set size */
    char     spare_rcr_1[5];    /* padding for link list */
    Rad_Comp_Set  *set;         /* pointer to comp, dataset */
    struct radi_comp_rec *next; /* pointer to next radi. comp. record */
} Radi_Comp;


/****************************
*                           *
*  SAR Leader File:         *
*  Data Quality Summary     *
*                           *
****************************/

typedef struct qual_sum_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* dqs record sequence number */
    char  chan_ind[5];          /* sar channel indicator */
    char  cali_date[7];         /* calibration date */
    short nchn;                 /* number of channels */
    float islr;                 /* integrated side lobe ratio */
    float pslr;                 /* peak side lobe ratio */
    float azi_ambig;            /* azimuth ambiguity */
    float rng_ambig;            /* range ambiguity */
    float snr;                  /* signal-to-noise ratio estimate */
    float ber;                  /* nominal bit error rate */
    float rng_res;              /* nominal slant range resolution */
    float azi_res;              /* nominal azimuth resolution */
    float rad_res;              /* nominal radiometric resolution */
    float dyn_rng;              /* instantaneous dynamic range */
    float abs_rad_unc_db;       /* nominal abs. radiometric uncertainty, db */
    float abs_rad_unc_deg;      /* nominal abs. radiometric uncertainty, deg */
    float rel_rad_unc[2][16];   /* rel short term radio calibr uncertainty */
                                /*   16 pairs of values in db and deg */
    float alt_locerr;           /* location error along track */
    float crt_locerr;           /* location error cross track */
    float alt_scale;            /* along track scale error */
    float crt_scale;            /* cross track scale error */
    float dis_skew;             /* geometric skew error */
    float ori_err;              /* image orientation error */
    float misreg[2][16];        /* misregistration error */
                                /*  16 pairs of values for along track */
                                /*  and cross track */
#ifdef PRE_RADARSAT
    char  spare2[279];          /* spare */
#else
    float nesz;                 /* nominal noise equiv sigma zero */
    float enl;                  /* Nominal equiv no of looks */
    char  tb_update[9];         /* default param table update date */
    char  cal_status[17];       /* calibration status */
    char  spare_dqs_2[23];      /* spare */
    char  cal_comment[201];     /* calibration comment */
#endif
    struct qual_sum_rec  *next; /* pointer to next pol/ quailty qum record */
} Qual_Sum;


/****************************
*                           *
*  SAR Leader File:         *
* Data Histograms Record    *
*                           *
****************************/

/* data set structure */

typedef struct histogram_dset {
    char  hist_desc[33];       /* histogram descriptor */
    short nrec;                /* required records */
    short tab_seq;             /* table sequence number */
    long  nbin;                /* table size, bytes */
    long  ns_lin;              /* number of pixels in line */
    long  ns_pix;              /* number of lines in image */
    long  ngrp_lin;            /* pixels/group, cross track */
    long  ngrp_pix;            /* pixels/group, along track */
    long  nsamp_lin;           /* number of groups, cross track */
    long  nsamp_pix;           /* number of groups, along track */
    float min_smp;             /* minimum pixel value */
    float max_smp;             /* maximum pixel value */
    float mean_smp;            /* mean sample value */
    float std_smp;             /* std deviation of sample value */
    float smp_inc;             /* sample value increment */
    float min_hist;            /* minimum table value */
    float max_hist;            /* maximum table value */
    float mean_hist;           /* histogram mean value */
    float std_hist;            /* histogram standard deviation */
    long  nhist;               /* histogram table size */
    long  *data_values_hist;   /* table values 1-256 */
    struct histogram_dset *next; 
} Hist_Data_Set;



/* main structure */

typedef struct data_hist_rec {
    desc_rec desc;               /* header info */
    short seq_num;               /* data histogram record seq no. */
    short sar_chan;              /* SAR channel */
    long  ntab;                  /* number of table sets */
    long  ltab;                  /* data set size */
    short DH_entry;              /* data entry entry */
    Hist_Data_Set*   data_set;   /* pointer to linked list */
    struct data_hist_rec  *next; /* pointer to next pol/ histogram record */
} Data_Hist;


/****************************
*                           *
*  SAR Leader File:         *
*  Range Spectra Record     *
*                           *
****************************/

typedef struct rng_spec_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* range spectra sequence no. */
    short sar_chan;             /* SAR channel */
    long  n_dset;               /* number of data sets */
    long  dset_size;            /* data set size */
    short req_recs;             /* number of records required */
    short table_no;             /* table sequence number */
    long  n_pixels;             /* number of pixels in line */
    long  pixel_offset;         /* offset from first pixel */
    long  n_lines;              /* number of lines integrated for spectra */
    float first_freq;           /* center freq of first spectra bin */
    float last_freq;            /* center freq of last spectra bin */
    float min_power;            /* minimum spectral power */
    float max_power;            /* maximum spectral power */
    char  spare_rsr_1[17];      /* spare */
    char  spare_rsr_2[17];      /* spare */
    long  n_bins;               /* number of freq bins in table */
    float data_values_spec[256];/* spectral data values 1-256 */
    char  spare_rsr_3[1053];    /* spare */
    struct rng_spec_rec *next;  /* pointer to next pol/ spectra record */
} Rng_Spec;

/****************************
*                           *
*  SAR Leader File:         *
*  Digital Elevation Record *
*                           *
****************************/

typedef struct corner_pts_rec {
    float cp_lat_1;              /* polygon corner pt. latitude */
    float cp_lon_1;              /* polygon corner pt. longitude */
    struct corner_pts_rec *next; /* pointer to next corner point */
} Corner_Pts;

typedef struct dem_desc_data {
    short poly_seq_num;         /* polygon sequence number */
    short num_crnr_pts;         /* Number of corner pts. for this record */
    char  spare_dem_1[9];       /* spare */
    Corner_Pts*  pts;           /* pointer to corner pts/lat/long */
    struct dem_desc_data *next; /* pointer to next DEM data descriptor data set */
} Dem_Desc;


typedef struct dig_elev_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* sequence no. */
    long  ttl_num_sets;         /* total number of DEM datasets */
    short DEM_seq_num;          /* Sequence num. of this DEM descriptor */
    char  source_DEM[33];       /* Original src of DEM */
    char  HT_ref_name[33];      /* Height datum reference name */
    char  gen_method[33];       /* generation method */
    char  raster_unit[13];      /* raster spacing unit */
    char  presentation_proj[33];/* presentation projection */
    float NS_raster;            /* North/South raster spacing */
    float EW_raster;            /* East/West raster spacing */
    char  resample[33];         /* Applied resampling method */
    float height_err;           /* RMS height error */
    float NS_loc_err;           /* RMS location error North/South */
    float EW_loc_err;           /* RMS location error East/West */
    float max_height;           /* Max height */
    float min_height;           /* Min height */
    float MEAN_height;          /* MEAN height */
    float STD_height;           /* STD of height */
    short num_polys;            /* Number of polygons in record */
    Dem_Desc*  set;             /* pointer to list of DEM desc data sets */
    char spare_dem_2[533];      /* spare */
    struct  dig_elev_rec *next; /* pointer to next DEM record */
} Digital_Elev;

/*********************************
*                                *
*  SAR Leader File:              *
*  Detail Processing Parameters  *
*  As defined by CSA in their    *
*  CDPF product specs            *
*  issue/rev = 3/0               *
*  date = apr22,1994             *
*                                *
*********************************/

typedef struct beam_info_rec {
    char beam_type[4];          /* Beam type                            (a3) */
    char beam_look_src[10];     /* Elevation beam look angle source     (a9) */
    float beam_look_ang;        /* Applied elev beam look angle, deg (f16.7) */
    float prf;                  /* Actual PRF (Hz)                   (f16.7) */
    struct beam_info_rec *next; /* pointer to next beam info rec             */
} Beam_Info;

typedef struct pix_count_rec {
    char pix_update[22];        /* pixel count update time             (a21) */
    long n_pix[4];              /* count of image pixels in beams      (4i8) */
    struct pix_count_rec *next; /* pointer to next pix count rec             */
} Pix_Count;

typedef struct temp_rec {
    long temp_set[4];           /* temperature settings                (4i4) */
    struct temp_rec *next;      /* pointer to next temp rec                  */
} Temp_Rec;

typedef struct dopcen_est_rec {
    float dopcen_conf;          /* Doppler Centroid conf measure     (f16.7) */
    float dopcen_ref_tim;       /* Doppler Centroid ref time         (f16.7) */
    float dopcen_coef[4];       /* Doppler Centroid coefficients    (4f16.7) */
    struct dopcen_est_rec *next; /* pointer to next dopcen rec               */
} Dopcen_Est;

typedef struct srgr_coefset_rec {
    char srgr_update[22];       /* SRGR update date/time               (a21) */
    float srgr_coef[6];         /* SRGR coefficients                (6f16.7) */
    struct srgr_coefset_rec *next; /* pointer to next SRGR rec               */
} SRGR_Coefset;

typedef struct proc_parm_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* record sequence no.                  (i4) */
    char spare_dpp_1[5];        /* spare                                (a4) */
    char inp_media[4];          /* Input media                          (a3) */
    short n_tape_id;            /* Number of input tape id              (i4) */
    char tape_id[10][9];        /* tape identifier                    (10a8) */
    char exp_ing_start[22];     /* Expected ingest start time          (a21) */
    char exp_ing_stop[22];      /* Expected ingest stop time           (a21) */
    char act_ing_start[22];     /* Actual ingest start time            (a21) */
    char act_ing_stop[22];      /* Actual ingest stop time             (a21) */
    char proc_start[22];        /* Processing start time               (a21) */
    char proc_stop[22];         /* Processing stop time                (a21) */
    float mn_sig_lev[10];       /* Mean signal levels across range (10f16.7) */
    short src_data_ind;         /* Source data quality indicator        (i4) */
    long miss_ln;               /* Number of missing lines              (i8) */
    long rej_ln;                /* Number of rejected lines             (i8) */
    long large_gap;             /* Number of time inconsistencies       (i8) */
    float bit_error_rate;       /* Measured bit error rate           (f16.7) */
    float fm_crc_err;           /* Percent of frames with CRC errors (f16.7) */
    long date_incons;           /* Number of date inconsistencies       (i8) */
    long prf_changes;           /* Number of unexpected PRF changes     (i8) */
    long delay_changes;         /* Number of delay changes              (i8) */
    long skipd_frams;           /* Number of skippped frames            (i8) */
    long rej_bf_start;          /* Rng lines reject before start time   (i8) */
    long rej_few_fram;          /* Rng lines reject: too few frames     (i8) */
    long rej_many_fram;         /* Rng lines reject: too many frames    (i8) */
    long rej_mchn_err;          /* Frames reject: master ch err         (i8) */
    long rej_vchn_err;          /* Frames reject: virtual ch err        (i8) */
    long rej_rec_type;          /* Frames reject: rec type err          (i8) */
    long prd_qual_ind;          /* Product quality index                (i4) */
    char qc_rating[7];          /* Quality control rating               (a6) */
    char qc_comment[81];        /* Quality control comment             (a80) */
    char sens_config[11];       /* Sensor configuration                (a10) */
    char sens_orient[10];       /* Sensor orientation                   (a9) */
    char sych_marker[9];        /* Frame synch marker                   (a8) */
    char rng_ref_src[13];       /* Range ref function source           (a12) */
    float rng_amp_coef[4];      /* Range ref ampl coeff             (4f16.7) */
    float rng_phas_coef[4];     /* Range ref phase coeff            (4f16.7) */
    float err_amp_coef[4];      /* Error function ampl coeff        (4f16.7) */
    float err_phas_coef[4];     /* Error function phase coeff       (4f16.7) */
    long pulse_bandw;           /* Pulse bandwidth code                 (i4) */
    char adc_samp_rate[6];      /* ADC sampling rate                    (a5) */
    float rep_agc_attn;         /* Replica AGC attenuation           (f16.7) */
    float gn_corctn_fctr;       /* Gain correction factor (dB)       (f16.7) */
    float rep_energy_gn;        /* Replica energy gain correction    (f16.7) */
    char orb_data_src[12];      /* Orbit data source                   (a11) */
    long pulse_cnt_1;           /* Pulse count 1                        (i4) */
    long pulse_cnt_2;           /* Pulse count 2                        (i4) */
    char beam_edge_rqd[4];      /* Beam edge detection requested        (a3) */
    float beam_edge_conf;       /* Beam edge confidence measure      (f16.7) */
    long pix_overlap;           /* Number of pixels in beam overlap     (i4) */
    long n_beams;               /* Number of beams                      (i4) */
    Beam_Info* beam_info;       /* Beam info               (repeats 4 times) */
    long n_pix_updates;         /* Number of pixel count updates        (i4) */
    Pix_Count* pix_count;       /* Beam pixel count       (repeats 20 times) */
    float pwin_start;           /* Processing window start time, sec (f16.7) */
    float pwin_end;             /* Processing window end time, sec   (f16.7) */
    char recd_type[9];          /* Recording type                       (a9) */
    float temp_set_inc;         /* Time increment btwn temp set, sec (f16.7) */
    long n_temp_set;            /* Number of temp settings              (i4) */
    Temp_Rec* temp;             /* Temperature settings   (repeats 20 times) */
    long n_image_pix;           /* Number of image pixels               (i8) */
    float prc_zero_pix;         /* Percent zero pixels               (f16.7) */
    float prc_satur_pix;        /* Percent saturated pixels          (f16.7) */
    float img_hist_mean;        /* Image histogram mean intensity    (f16.7) */
    float img_cumu_dist[3];     /* Image cumulative distribution    (3f16.7) */
    float pre_img_gn;           /* Pre-image calibration gain factor (f16.7) */
    float post_img_gn;          /* Post-image calibration gain factor(f16.7) */
    float dopcen_inc;           /* Time inc btwn Doppler Cen est, sec(f16.7) */
    long n_dopcen;              /* Number of Doppler Centroid est       (i4) */
    Dopcen_Est* dopcen_est;     /* Doppler Centroid rec   (repeats 20 times) */
    long dopamb_err;            /* Doppler ambiguity error              (i4) */
    float dopamb_conf;          /* Doppler ambiguity confidence meas.(f16.7) */
    float eph_orb_data[7];      /* Ephemeris orbit data             (7f16.7) */
    char appl_type[13];         /* Application type                    (a12) */
    double first_lntim;         /* Slow time of first image line    (d22.15) */
    double lntim_inc;           /* time inc btwn image lines        (d22.15) */
    long n_srgr;                /* Number of SRGR coeff sets            (i4) */
    SRGR_Coefset* srgr_coefset; /* SRGR coeff set         (repeats 20 times) */
    float pixel_spacing;        /* SGF product pixel spacing         (f16.7) */
    char pics_reqd[4];          /* GICS product required                (a3) */
    char wo_number[9];          /* Work order identifier                (a8) */
    char wo_date[21];           /* Work order entry date               (a20) */
    char satellite_id[11];      /* Satellite identifier                (a10) */
    char user_id[21];           /* User id                             (a20) */
    char complete_msg[4];       /* Completion message required flag     (a3) */
    char scene_id[16];          /* SGF product scene identifier        (a15) */
    char density_in[5];         /* Density of SGF product media         (a4) */
    char media_id[9];           /* SGF product identifer                (a8) */
    float angle_first;          /* Incidence angle of first pixel    (f16.7) */
    float angle_last;           /* Incidence angle of last pixel     (f16.7) */
    char prod_type[4];          /* GICS output product type             (a3) */
    char map_system[17];        /* Map system identifier               (a16) */
    double centre_lat;          /* GICS output, centre latitude     (d22.15) */
    double centre_long;         /* GICS output, centre longitude    (d22.15) */
    double span_x;              /* GICS output, size eastings, km   (d22.15) */
    double span_y;              /* GICS output, size northings, km  (d22.15) */
    char apply_dtm[4];          /* DTM correction to be applied flag    (a3) */
    char density_out[5];        /* GICS output product density          (a4) */
    char spare_dpp_2[248];      /* unused                             (a247) */
    struct proc_parm_rec *next; /* pointer to next detail proc parm record   */
} Proc_Parm;

typedef struct calib_data_rec {
    desc_rec desc;              /* header info */
    short seq_num;              /* record sequence no.                  (i4) */
    char spare_cdr_1[5];        /* spare                                (a4) */
    char spare_cdr_2[256];      /* TBD                                (a255) */
    struct calib_data_rec *next;/* pointer to next calibration data record   */
} Calib_Data;

/****************************
*                           *
*  SAR Leader File:         *
*  Facility Related Record  *
*                           *
****************************/

typedef struct fac_related_rec {
    desc_rec desc;               /* header info */
    char   *bogus;               /* filled if ice motion, ice class, wave motion */
    short  seq_num;              /* sequence no. */
    char   spare_frd_1[5];       /* spare */
    char   datatake_ID[15];      /* datatake ID */
    char   image_ID[12];         /* image ID */
    char   corr_year[6];         /* correlation year */
    char   corr_GMT[18];         /* correlation time */
    char   site_name[34];        /* reference image or site name */
    char   data_year[6];         /* data year */
    char   center_GMT[18];       /* center GMT */
    float  center_LAT;           /* center Latitude */
    float  center_LON;           /* center Longitude */
    float  near_start_LAT;       /* near start Latitude */
    float  near_start_LON;       /* near start Longitude */
    float  near_end_LAT;         /* near end Latitude */
    float  near_end_LON;         /* near end Longitude */
    float  far_start_LAT;        /* far start Latitude */
    float  far_start_LON;        /* far start Longitude */
    float  far_end_LAT;          /* far end Latitude */
    float  far_end_LON;          /* far end Longitude */
    float  actual_azimuth;       /* actual azimuth */
    float  actual_range;         /* actual range */
    long   actual_pixels;        /* actual pixels */
    long   actual_lines;         /* actual lines */
    long   total_pixels;         /* total pixels */
    long   total_lines;          /* total lines */
    char   media_id[8];          /* media ID */
    long   start_address;        /* start block */
    long   end_address;          /* end block */
    char   platform_name[18];    /* platform name */
    char   sensor_mode[34];      /* sensor and mode */
    float  PRF;                  /* PRF */
    float  ant_look_angle;       /* antenna look angle */
    float  data_rate;            /* data rate */
    float  data_win_pos;         /* data window position */
    float  range_gate_del;       /* range gate delay */
    float  track_angle;          /* track angle */
    char   ASC_DESC[3];          /* Ascending-Descending flag */
    float  S_C_altitude;         /* Spacecraft altitude */
    double X_position;           /* spacecraft x position */
    double Y_position;           /* spacecraft y position */
    double Z_position;           /* spacecraft z position */
    double X_velocity;           /* spacecraft x velocity */
    double Y_velocity;           /* spacecraft y velocity */
    double Z_velocity;           /* spacecraft z velocity */
    float  roll;                 /* spacecraft roll */
    float  yaw;                  /* spacecraft yaw */
    float  pitch;                /* spacecraft pitch */
    short  roll_flag;            /* spacecraft roll flag */
    short  yaw_flag;             /* spacecraft yaw flag */
    short  pitch_flag;           /* spacecraft pitch flag */
    float  roll_rate;            /* spacecraft roll rate */
    float  yaw_rate;             /* spacecraft yaw rate */
    float  pitch_rate;           /* spacecraft pitch rate */
    short  roll_rate_flag;       /* spacecraft roll rate flag */
    short  yaw_rate_flag;        /* spacecraft yaw rate flag */
    short  pitch_rate_flag;      /* spacecraft pitch rate flag */
    float  nadir_radius;         /* nadir radius */
    float  image_radius;         /* image radius */
    float  incidence_angle;      /* incidence angle */
    char   proc_version[9];      /* processor version */
    char   proc_type[4];         /* processor type */
    char   type_ephemeris[3];    /* type of ephemeris */
    float  looks_azimuth;        /* effective looks azimuth */
    float  looks_range;          /* effective looks range */
    float  azi_weight_fac;       /* weighing pedestal height azimuth */
    float  range_weight_fac;     /* weighing pedestal height range */
    char   look_energy_eq[5];    /* look energy normalization flag */
    float  induced_azimuth;      /* induced distortion in azimuth */
    float  induced_range;        /* induced distortion in range */
    float  gain;                 /* receiver gain */
    float  swath_velocity;       /* swath velocity */
    float  squint_angle;         /* squint angle */
    float  avg_terrain_ht;       /* average terrain height */
    short  processor_gain;       /* processor gain */
    char   deskew[5];            /* deskew applied flag */
    char   gnd_slant_flag[8];    /* ground or slant range flag */
    float  sl_rng_1st_pix;       /* slant range to 1st pixel */
    float  sl_rng_last_pix;      /* slant range to last pixel */
    long   start_sample;         /* start sample processed */
    char   clutterlock_flg[5];   /* clutterlock flag */
    float  dop_frq_const;        /* Doppler frequency constant */
    float  dop_frq_slope;        /* Doppler frequency slope */
    float  dop_frq_quad;         /* Doppler frequency quad */
    char   autofocus_flag[5];    /* autofocus flag */
    float  dop_frq_r_cnst;       /* Doppler frequency rate constant */
    float  dop_frq_r_slope;      /* Doppler frequency rate slope */
    float  dop_frq_r_quad;       /* Doppler frequency rate quad */
    float  azi_res;              /* resolution in azimuth */
    float  rng_res;              /* resolution in range */
    float  azimuth_pixel;        /* azimuth pixel */
    float  range_pixel;          /* range pixel */
    char   OBRC_flag[5];         /* OBRC flag */
    short  bits_sample;          /* bits sample */
    float  calib_est;            /* calibraton estimate */
    float  bit_err_rate;         /* bit err rate */
    float  SNR;                  /* Signal to Noise Ratio */
    float  est_noise_flr;        /* estimated noise floor */
    float  radio_m_resol;        /* radiometric resolution */
    long   satur_points;         /* saturated data points */
    char   spec_flag[5];         /* within specification flag */
#ifndef PRE_RADARSAT
/****************************
*                           *
*  SAR Leader File:         *
*  Facility Related Record  *
*  new Radarsat era         *
*  rec_type = 210  includes *
*  the following fields     *
*                           *
****************************/
    float  repl_agc;             /* chirp replica AGC value */
    float  temp_rx_lna;          /* temp of rcvr LNA */
    float  temp_rx_sub;          /* temp of rcvr subsystem */
    float  temp_rx_prot;         /* temp of rcvr protector */
    float  temp_cal_sys;         /* temp of calib system */
    float  rx_agc;               /* rcvr AGC value */
    float  pre_cal1_pow;         /* pre cal1 avg power */
    float  pre_cal2_pow;         /* pre cal2 avg power */
    float  post_cal1_pow;        /* post cal1 avg power */
    float  post_cal2_pow;        /* post cal2 avg power */
    float  repl_pow;             /* Replica avg power */
    float  ssar_roll_ang;        /* est ScanSAR roll angle */
    char   comment[101];         /* comment field */
#endif
    struct fac_related_rec *next;
} Fac_Related;

/****************************
*                           *
*  Imagery Options File     *
*  File Descriptor Record   *
*                           *
****************************/

typedef struct  img_desc_rec_t {
    desc_rec desc;              /* header info */
    char  ascii_flag[3];        /* ASCII flag */
    char  spare_ifd_1[3];       /* spare */
    char  format_doc[13];       /* document format */
    char  format_rev[3];        /* format revision ("A") */
    char  design_rev[3];        /* record format revision level */
    char  software_id[13];      /* software ID */
    short file_num;             /* file number */
    char  product_id[17];       /* file name = product id */
    char  rec_seq_flag[5];      /* record sequence and location type flag */
    long  seq_loc;              /* sequence number location */
    short seq_len;              /* sequence number field length */
    char  rec_code[5];          /* record code and location type flag */
    long  code_loc;             /* record code location */
    short code_len;             /* record code field length */
    char  rec_len[5];           /* record length and location type flag */
    long  rlen_loc;             /* record length location */
    short rlen_len;             /* record length field length */
    char  spare_ifd_2[5];       /* spare */
    char  spare_ifd_3[65];      /* OPS Reader s/w version */
    long  n_dataset;            /* number of data set summary records */
    long  l_dataset;            /* record length */
    char  spare_ifd_4[25];      /* spare */
    short nbit;                 /* number of bits per sample/pixel */
    short nsamp;                /* number of pixels per data group */
    short nbyte;                /* number of bytes per data group */
    char  justify[5];           /* sample justification and order */
    short nchn;                 /* number of SAR channels */
    long  nlin;                 /* number of lines per data set */
    short nleft;                /* number of left border pixels per line */
    long  ngrp;                 /* number of pixels per line */
    short nright;               /* number of right border pixels */
    short ntop;                 /* number of top border scan lines */
    short nbox1;                /* number of bottom border scan lines */
    char  intleav[5];           /* interleaving indicator */
    short nrec_lin;             /* number of records per line */
    short nrec_chn;             /* number of records per channel */
    short n_prefix;             /* length of prefix data per line */
    long  n_sar;                /* number of bytes per line */
    short n_suffix;             /* length of suffix data per line */
    char  spare_ifd_5[4];       /* spare */
    char  lin_loc[9];           /* line number locator */
    char  chn_loc[9];           /* channel number locator */
    char  time_loc[9];          /* time locator */
    char  left_loc[9];          /* left fill locator */
    char  right_loc[9];         /* right fill locator */
    char  pad_ind[5];           /* pad pixel indicator */
    char  spare_ifd_6[29];      /* spare */
    char  qual_loc[9];          /* quality code locator */
    char  cali_loc[9];          /* calibration info locator */
    char  gain_loc[9];          /* gain value locator */
    char  bias_loc[9];          /* bias value locator */
    char  type_id[29];          /* SAR data format type identifier */
    char  type_code[5];         /* data type code */
    short left_fill;            /* number of left fill bits within pixel */
    short right_fill;           /* number of right fill bits within pixel */
    long  pixel_rng;            /* maximum data range of pixel */
    char  spare_ifd_7[193];     /* reserved */

   /* NOTE: This record is padded out with spaces until it is equal to
    * the length of the Image Data Record (or Signal Data Record).
    * The amount of padding necessary is calculated at run time.
    * See the source code for details (imgopts.c).
    */

} Img_Desc_Rec;


/****************************
*                           *
*  Imagery Options File     *
*  RSD and Image Records    *
*                           *
****************************/

typedef struct img_record_t{
    desc_rec desc;              /* header info */
    unsigned char     img_line[MAX_LINE_LENGTH * MAX_BYTES_PER_PIXEL];

} Img_Record;


/****************************
*                           *
*  SAR Trailer File         *
*  File Descriptor Record   *
*   ASF will drop this file *
*                           *
****************************/


typedef struct sart_desc_rec {
    desc_rec desc;              /* header info */
    char  ascii_flag[3];        /* ASCII flag */
    char  spare_fdr_1[3];       /* spare */
    char  format_doc[13];       /* document format */
    char  format_rev[3];        /* format revision ("A") */
    char  design_rev[3];        /* record format revision level */
    char  software_id[13];      /* software ID */
    short file_num;             /* file number */
    char  product_id[17];       /* filename = product id */
    char  rec_seq_flag[5];      /* record sequence & location type flag */
    long  seq_loc;              /* sequence number location */
    short seq_len;              /* sequence number field length */
    char  rec_code[5];          /* record code and location type flag */
    long  code_loc;             /* record code location */
    short code_len;             /* record code field length */
    char  rec_len[5];           /* record length and location type flag */
    long  rlen_loc;             /* record length location */
    short rlen_len;             /* record length field length */
    char  spare_fdr_2[5];       /* spare */
    char  spare_fdr_3[65];      /* OPS Reader s/w version */
    long  n_dataset;            /* number of data set summary records */
    long  l_dataset;            /* record length */
    long  n_map_proj;           /* number of map projection data records */
    long  l_map_proj;           /* record length */
    long  n_plat_pos;           /* number of platform position records */
    long  l_plat_pos;           /* record length */
    long  n_att_data;           /* number of attitude data records */
    long  l_att_data;           /* record length */
    long  n_radi_data;          /* number of radiometric data records */
    long  l_radi_data;          /* record length */
    long  n_radi_comp;          /* number of radio compensation records */
    long  l_radi_comp;          /* record length */
    long  n_qual_sum;           /* number of data quality summary records */
    long  l_qual_sum;           /* record length */
    long  n_data_hist;          /* number of data histogram records */
    long  l_data_hist;          /* record length */
    long  n_rang_spec;          /* number of range spectra records */
    long  l_rang_spec;          /* record length */
    long  n_dem_desc;           /* number of digital elevation model recs */
    long  l_dem_desc;           /* record length */
    long  n_radar_par;          /* number of radar parameter update recs */
    long  l_radar_par;          /* record length */
    long  n_anno_data;          /* number of annotation data records */
    long  l_anno_data;          /* record length */
    long  n_det_proc;           /* number of detailed processing records */
    long  l_det_proc;           /* record length */
    long  n_cal;                /* number of calibration data records */
    long  l_cal;                /* record length */
    long  n_gcp;                /* number of ground control pts records */
    long  l_gcp;                /* record length */
    char  spare_fdr_4[10];      /* spare */
    long  n_fac_data;           /* number of facility data records */
    long  l_fac_data;           /* record length */
    char  spare_fdr_5[289];     /* spare */

} Sart_Desc_Rec;

/****************************
*                           *
*  Null Volume Direct File  *
*  Volume Descriptor Record *
*                           *
****************************/

typedef struct null_Desc_rec {
    desc_rec desc;              /* header info */
    char  ascii_flag[3];        /* ASCII flag */
    char  spare_nvd_1[3];       /* spare */
    char  format_doc[13];       /* control document number */
    char  format_ver[3];        /* revision number (00-99) */
    char  format_rev[3];        /* file design revision number */
    char  software_id[13];      /* software ID */
    char  media_id[17];         /* physical tape ID */
    char  logvol_id[17];        /* logical set ID */
    char  volset_id[17];        /* volume set ID */
    short phyvol_cnt;           /* number of physical volumes */
    short first_phyvol;         /* first physical vol sequence number */
    short last_phyvol;          /* last physical vol sequence number */
    short curr_phyvol;          /* this physical vol number */
    short first_file;           /* first reference file in volume */
    short volset_log;           /* logical volume in set */
    short logvol_vol;           /* logical volume in physical volume */
    char  spare_nvd_10[249];    /* spare */

} Null_Desc_Rec;


/******************************************************************************
*                                                                             *
*                  Structures which define CEOS Data Files                    *
*                                                                             *
******************************************************************************/

/***************************
*                          *
* SAR Leader File          *
*                          *
***************************/

typedef struct sarl_file_t {
    int read_count;
    int write_count;
    Sarl_Desc_Rec          descript;
    Dataset_Sum*           data_sum;
    Map_Proj*              map_proj;
    Pos_Data*              platform;
    Att_Data*              attitude;
    Radi_Data*             radio_data;
    Radi_Comp*             radio_comp;
    Qual_Sum*              data_qual;
    Data_Hist*             histogram; 
    Rng_Spec*              spectra;  
    Digital_Elev*          elevation;
    Proc_Parm*             detail;
    Calib_Data*            calibration;
    Fac_Related*           facility;
    
} SARL_ptr;


/***************************
*                          *
* Imagery Options File     *
*                          *
***************************/

typedef struct imgopt_file_t {

    Img_Desc_Rec             descript;
    Img_Record               *next;               /* ptr to next image */

} IMOP_ptr;


/***************************
*                          *
* SAR Trailer File         *
*                          *
***************************/

/*
 * No structure definition is needed to define this file because 
 * the SAR Trailer file consists of only the file description
 * record (sart_descript_t).  ASF will not include the file.
 */

typedef struct sart_file_t {

    Sart_Desc_Rec              descript;

} SART_ptr;




/******************************
*                             *
* Null Volume Descriptor File *
*                             *
******************************/

/*
 * No structure definition is needed because the Null Volume file
 * consists of only the file description record (null_descript_t).
 */
typedef struct null_file_t {

    Null_Desc_Rec              descript;

} NULL_ptr;

/***************************
*                          *
* Volume Directory File    *
*                          *
***************************/

typedef struct vol_ptr_recs {
    SARL_ptr                  *sarl_ptr;
    IMOP_ptr                  *img_ptr;
    SART_ptr                  *sart_ptr;
    NULL_ptr                  *null_ptr;
} VOLUME_ptrs;

typedef struct volume_file_t {
    Vol_Desc_Rec               descript;
    VOLUME_ptrs                *list; /* ptr to volume file list */
} VOLUME_FILE_ptr;


#endif /* OCEOS_H */

