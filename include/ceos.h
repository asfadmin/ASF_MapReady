/*******************************************************************************
NAME: ceos.h

PURPOSE:  Include file defining structure for CEOS file header/trailer/
          data record contents.

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           5/92   R. Guritz (ASF)
  1.1           9/93   T. Logan (ASF)   Added Imagery option file descriptor
                                          record
  1.2          10/93   T. Logan (ASF)   Added Radiometric Data Record
  1.3           2/94   M. Shindle <ASF> Added Map Projection Data Record
  2.0           5/96   T. Logan (ASF)   Added all the rest of the record types
                                        Modified existing value structures to
                                          include string values
  3.0           9/96   T. Logan (ASF)   Added all fields for post Radarsat CEOS
                                          structures
  3.1          11/97   T. Logan (ASF)   Made all records consistent with hdrs.
  4.0           2/02   R. Gens          Added ESA facility data record
  4.1           6/04   P. Denny         Added RSI radiometric data record

*******************************************************************************/
#ifndef __CEOS_H
#define __CEOS_H        /* include ceos.h only once */

#include <stdio.h>


/*** MODES *****/
#define CEOS_SLC     0
#define CEOS_FUL     1
#define CEOS_LOW     2
#define CEOS_CCSD    3

/*** RECORD TYPES ***/
/* Leader file */
#define IOFDR     10003           /* Imagery Option File Descriptor */
#define LFDR      10004           /* Leader File Descriptor         */
#define DSSR      10005           /* Data Set Summary               */
#define MPDR      10006           /* Map Projection Data            */
#define PPDR      10007           /* Platform Position Data         */
#define ATDR      10008           /* Attitude Data                  */
#define RADR      10009           /* Radiometric Data               */
#define DQSR      10010           /* Data Quality Summary           */
#define SDHR      10011           /* Signal Data Histogram Record   */
#define PDHR      10012           /* Processed Data Histogram       */
#define RNSR      10013           /* Range Spectra Record           */
#define DEMR      10014           /* Digital Elevation Model        */
#define FACDR     10015           /* Facility Data Record           */
#define ASFFACDR  10015           /* ASF Facility Data Record       */
#define ESAFACDR  10016           /* ESA Facility Data Record       */
#define PPR       10017           /* Processing Parameter Record    */
/* Volume directory file */
#define VDR       10018
#define LFPR      10019
#define DFPR      10020
#define TR        10021
/* Null volume file */
#define NVDR      10022
#define SHR       10023
#define RCDR      10024
#define AMPR      10025

/*** RECORD SIZES ***/
#define H_SZ    12
#define R_SZ    180
#define FDR_SZ  720


#define SLC_AVG 32768.0

struct IMG_STATS {
int   nbins;
float min_val; float max_val; float mean_val; float std_dev_val;
float min_tbl; float max_tbl; float mean_tbl; float std_dev_tbl;
int  tbl[32];
};

/* appears at the beginning of each record in all ASF files */
struct  HEADER {
        unsigned char   recnum[4];/*1 BIG-ENDIAN Int.*/
        unsigned char   rectyp[4];
        unsigned char   recsiz[4];/*1 BIG-ENDIAN Int.*/
};

/* appears at the beginning of each Radarsat Era Data Record */
struct RHEADER {
  int           line_num,
                rec_num,
                n_left_pixel,
                n_data_pixel,
                n_right_pixel,
                sensor_updf,
                acq_year,
                acq_day,
                acq_msec;
  short int     sar_cib,
                sar_chan_code,
                tran_polar,
                recv_polar;
  float         prf_bin,
                spare_1,
                sr_first,
                sr_mid,
                sr_last,
                fdc_first,
                fdc_mid,
                fdc_last,
                ka_first,
                ka_mid,
                ka_last,
                nadir_ang,
                squint_ang,
                null_f;
  int           spare_2_1,
                spare_2_2,
                spare_2_3,
                spare_2_4,
                geo_updf,
                lat_first,
                lat_mid,
                lat_last,
                long_first,
                long_mid,
                long_last,
                north_first,
                spare_3,
                north_last,
                east_first,
                spare_4,
                east_last,
                heading;
  double        spare_5;
};

// signal data record: ALOS comes with this
struct SHEADER {
  int           line_num,
                rec_num,
                n_left_pixel,
                n_data_pixel,
                n_right_pixel,
                sensor_updf,
                acq_year,
                acq_day,
                acq_msec;
  short int     sar_cib,
                sar_chan_code,
                tran_polar,
                recv_polar;
  int           prf,
                scan_id;
  short int     comp_flag,
                pulse_chirp;
  int           chirp_length,
                chirp_const,
                chirp_linear,
                chirp_quad,
                spare1,
                spare2,
                recv_gain,
                nought_flag,
                squint_angle,
                elev_nadir,
                squint_angle2,
                elev_nadir2,
                sr_first,
                win_pos,
                spare3,
                plat_pos,
                lat,
                lon,
                alt,
                gr_speed,
                vel_x,
                vel_y,
                vel_z,
                acc_x,
                acc_y,
                acc_z,
                track_angle1,
                track_angle2,
                pitch,
                roll,
                yaw;
};


/* Radiometric Data Record */
struct RADDR {
 char     seqnum[4],    /* Radiometric Data Record Sequence Number */
          datfield[4],  /* Number of radiometric data fields in this rec */
          setsize[8],   /* Radiometric data set size in bytes */
          sarchan[4],   /* SAR channel indicator */
          spare[4],
          luttype[24],  /* Look Up Table Designator */
          nosample[8],  /* Number of samples in LUT */
          samptype[16], /* Sample Type Designator */
          a[3][16],     /* Calibration coefficients */
          spare2[4],
          noise[512][16]; /* noise values */
};

/* Values for the Radiometric Data Record */
struct VRADDR {
   short  seqnum;       /* Radiometric Data Record Sequence Number */
   short  datfield;     /* Number of radiometric data fields in this rec */
   int    setsize;      /* Radiometric data set size in bytes */
   char   sarchan[5];   /* SAR channel indicator */
   char   spare[5];
   char   luttype[25];  /* Look Up Table Designator */
   int    nosample;     /* Number of samples in LUT */
   char   samptype[17]; /* Sample Type Designator */
   double a[3];         /* Calibration Coefficients */
   char   spare2[5];
   double noise[256];   /* noise values */
};
/* Values for the RSI format Radiometric Data Record */
struct RSI_VRADDR {
  short seq_num;          /* Record sequence number */
  short n_data;           /* Number of data sets */
  int field_size;         /* Data set size in bytes */
  char chan_ind[4];       /* SAR channel indicator */
  char spare1[4];         /* Unused */
  char table_desig[24];   /* Table designator */
  int n_samp;             /* Number of lookup table samples, generally 512 */
  char samp_type[16];     /* Sample type designator */
  short samp_inc;         /* Increment between table entries, range samples (pixels)*/
  double lookup_tab[512]; /* Output scaling gain table */
  char spare2[4];         /* Unused */
  double noise_scale;     /* Thermal noise reference level (dB) */
  double spare3;          /* Unused */
  double offset;          /* Scaling offset A3 (linear, set to 0 for SLC products*/
  double calib_const;     /* Calibration constant */
  char spare4[1512];      /* Unused */
};


/* Imagery Options File -- File Descriptor Record */
struct IOF_FDR {
   char   ascii_flag[2],
          spare1[2],
          format_doc[12],
          format_rev[2],
          design_rev[2],
          software_id[12],
          file_num[4],
          product_id[16],
          rec_seq_flag[4],
          seq_loc[8],
          seq_len[4],
          rec_code[4],
          code_loc[8],
          code_len[4],
          rec_len[4],
          rlen_loc[8],
          rlen_len[4],
          spare2[4],
          spare3[64],

          numofrec[6],  /* data records */
          reclen[6],    /* Record Length */
          blanks[24],   /* Reserved -- Blanks */

          /* sample group data */
          bitssamp[4],  /* bits per sample */
          sampdata[4],  /* samples per data group */
          bytgroup[4],  /* bytes per group */
          justific[4],  /* Justification and order of samples in data group */

          /* SAR related data in the record */
          sarchan[4],   /* SAR channels in this file */
          linedata[8],  /* lines per data set */
          lbrdrpxl[4],  /* left border pixels per line */
          datgroup[8],  /* Total number of data groups */
          rbrdrpxl[4],  /* right border pixels per line */
          topbrdr[4],   /* top border lines */
          botbrdr[4],   /* bottom border lines */
          interlv[4],   /* Interleave Indicator */

          /* Record Data in the file */
          recline[2],   /* physical records per line */
          mrecline[2],  /* physical records per multi channel line */
          predata[4],   /* bytes of prefix data per record */
          sardata[8],   /* bytes of sar data per record */
          sufdata[4],   /* bytes of suffix data per record */
          repflag[4],   /* prefix/suffix repeat flag */

          /* Prefix/Suffix Data Locators */
          DataLoc[104], /* I don't know what this stuff is for */

          /* SAR Data Pixel Description */
          formatid[28], /* SAR data format identifier */
          formcode[4],  /* SAR data format type code */
          leftfill[4],  /* Left fill bits per pixel */
          rigtfill[4],  /* Right fill bits per pixel */
          maxidata[8],  /* Maximum data range of pixel */

          padding[588];
};

/* Imagery Option File -- Values in File Descriptor Record */
struct IOF_VFDR {
   char   ascii_flag[2],
          spare1[2],
          format_doc[12],
          format_rev[2],
          design_rev[2],
          software_id[12];
   int    file_num;
   char   product_id[16],
          rec_seq_flag[4];
   int    seq_loc,
          seq_len;
   char   rec_code[4];
   int    code_loc,
          code_len;
   char   rec_len[4];
   int    rlen_loc,
          rlen_len;
   char   spare2[4],
          spare3[64];
   int    numofrec,          /* data records                         */
          reclen;            /* Record Length                        */
   char   spare4[25];        /* unused                               */
   int    bitssamp,          /* bits per sample                      */
          sampdata,          /* samples per data group               */
          bytgroup;          /* bytes per group                      */
   char   justific[5];       /* Justification and order of samples in data group */
   int    sarchan,           /* SAR channels in this file            */
          linedata,          /* lines per data set                   */
          lbrdrpxl,          /* left border pixels per line          */
          datgroup,          /* Total number of data groups          */
          rbrdrpxl,          /* right border pixels per line         */
          topbrdr,           /* top border lines                     */
          botbrdr;           /* bottom border lines                  */
   char   interlv[5];        /* Interleave Indicator                 */
   int    recline,           /* physical records per line            */
          mrecline,          /* physical recs per multi-channel line */
          predata,           /* bytes of prefix data per record      */
          sardata,           /* bytes of sar data per record         */
          sufdata;           /* bytes of suffix data per record      */
   char   repflag[5],        /* prefix/suffix repeat flag            */
          lin_loc[9],        /* line number locator                  */
          chn_loc[9],        /* channel number locator               */
          time_loc[9],       /* time locator                         */
          left_loc[9],       /* left fill locator                    */
          right_loc[9],      /* right fill locator                   */
          pad_ind[5],        /* pad pixel indicator                  */
          spare6[29],        /* unused                               */
          qual_loc[9],       /* quality code locator                 */
          cali_loc[9],       /* calibration info locator             */
          gain_loc[9],       /* gain value locator                   */
          bais_loc[9],       /* bias value locator                   */
          formatid[29],      /* SAR data format identifier           */
          formcode[5];       /* SAR data format type code            */
   int    leftfill,          /* Left fill bits per pixel             */
          rigtfill,          /* Right fill bits per pixel            */
          maxidata;          /* Maximum data range of pixel          */
};

/*********************************************
*   SAR Leader File: File Descriptor Record  *
*********************************************/
struct FDR {
   char   ascii_flag[2],
          spare1[2],
          format_doc[12],
          format_rev[2],
          design_rev[2],
          software_id[12];
   int    file_num;
   char   product_id[16],
          rec_seq_flag[4];
   int    seq_loc,
          seq_len;
   char   rec_code[4];
   int    code_loc,
          code_len;
   char   rec_len[4];
   int    rlen_loc,
          rlen_len;
   char   spare2[4],
          spare3[64];
   int    n_dssr,
          l_dssr,
          n_mpdr,
          l_mpdr,
          n_ppdr,
          l_ppdr,
          n_atdr,
          l_atdr,
          n_raddr,
          l_raddr,
          n_rcr,
          l_rcr,
          n_qsr,
          l_qsr,
          n_dhr,
          l_dhr,
          n_rsr,
          l_rsr,
          n_demdr,
          l_demdr,
          n_rpr,
          l_rpr,
          n_adr,
          l_adr,
          n_dpr,
          l_dpr,
          n_calr,
          l_calr,
          n_gcp,
          l_gcp,
          spare4[10],
          n_facdr,
          l_facdr;
    char  spare5[288];
};

/*************************************/
/* ASF Facility Related Data Record. */
/*************************************/
struct VFDRECV {
   short  seq_num;
   char   spare_frd_1[5];
   char   dataid[15],   /* Data take id */
          imageid[12],  /* SPS image identifier */
          coryear[6],   /* UTC year of image correlation */
          cortime[18],  /* UTC time of image correlation */
          sitename[34], /* Name of site covered */
          imgyear[6],   /* UTC year at the image center */
          imgtime[18];  /* UTC time at the image center */
   double imgclat,  /* Latitude of the image center (F16.7) in degrees */
          imgclon,  /* Longitude of the image center (F16.7) in degrees */
          nearslat, /* Lat at start of image frame in the near swath (F16.7) */
          nearslon, /* Long at start of image frame in the near swath (F16.7)*/
          nearelat, /* Lat at end of image frame in the near swath (F16.7) */
          nearelon, /* Long at end of image frame in the near swath (F16.7) */
          farslat,  /* Lat at start of image frame in the far swath (F16.7) */
          farslon,  /* Long at start of image frame in the far swath (F16.7) */
          farelat,  /* Lat at end of image frame in the far swath (F16.7) */
          farelon,  /* Long at end of image frame in the far swath (F16.7) */
          swazim,   /* Actual swath width (km) in azimuth direction (F16.7)*/
          swrange;  /* Actual swath width (km) in range direction (F16.7)*/
   int    npixels,  /* Actual (without filler) number of pixels per line(I8)*/
          nlines,   /* Actual (without filler) number of image lines (I8) */
          apixels,  /* Total (with filler) number of pixels per line (I8) */
          alines;   /* Total (with filler) number of image lines (I8) */
   char   mediaid[8],   /* Identification label of the media written to */
          sblock[18],   /* Location on the DCRSI where data begins */
          eblock[18],   /* Location on the DCRSI where data ends */
          platform[18], /* Name of platform for the sensor data was acquired */
          sensmode[34]; /* Sensor and mode of operation */
   double prfreq,   /* Pulse repitition frequency (F16.7) */
          antlook,  /* SAR antenna look angle (F16.7) */
          datarate, /* Data rate (F16.7) */
          datawin,  /* Data window position (F16.7) */
          rangegd,  /* Range gate delay (F16.7) */
          trackang; /* Track angle to True North (F16.7) */
   char   ascdesc[3];   /* Flag indicating whether the pass is asc/desc */
   double scalt,    /* Altitude of the spacecraft at the image center (F16.7)*/
          scxpos,   /* Spacecraft X-position at the image center (D22.15) */
          scypos,   /* Spacecraft Y-position at the image center (D22.15) */
          sczpos,   /* Spacecraft Z-position at the image center (D22.15) */
          scxvel,   /* Spacecraft X-velocity at the image center (D22.15) */
          scyvel,   /* Spacecraft Y-velocity at the image center (D22.15) */
          sczvel,   /* Spacecraft Z-velocity at the image center (D22.15) */
          scroll,   /* Spacecraft roll at the image center (E14.6) */
          scyaw,    /* Spacecraft yaw at the image center (E14.6) */
          scpitch;  /* Spacecraft pitch at the image center (E14.6) */
   int    qroll,     /* Quality flag for the spacecraft roll (I4) */
          qyaw,      /* Quality flag for the spacecraft yaw (I4) */
          qpitch;    /* Quality flag for the spacecraft pitch (I4) */
   double rroll,     /* Spacecraft roll rate at the image center (E14.6) */
          ryaw,      /* Spacecraft yaw rate at the image center (E14.6) */
          rpitch;    /* Spacecraft pitch rate at the image center (E14.6) */
   int    rqroll,    /* Quality flag for the spacecraft roll rate (I4) */
          rqyaw,     /* Quality flag for the spacecraft yaw rate (I4) */
          rqpitch;   /* Quality flag for the spacecraft pitch rate (I4) */
   double eradnadr,  /* Radius of the earth at nadir (F16.7) */
          eradcntr,  /* Radius of the earth at image center (F16.7) */
          incedang;  /* Incidence angle at the center of the image (F16.7) */
   char   procvers[9],  /* Version of the ASP (F7.2) */
          imgproct[4],  /* Image processing type identifier */
          ephemert[3];  /* Type of Ephemeris used identifier */
   double nlooksaz, /* Effective number of looks in azimuth (F16.7) */
          nlooksra, /* Effective number of looks in range (F16.7) */
          weightaz, /* Weighting pedestal height in azimuth (F16.7) */
          weightra; /* Weighting pedestal height in range (F16.7) */
   char   normener[5];  /* Look energy normalization flag */
   double indistaz, /* Known processing induced distortions in azimuth(F16.7)*/
          indistra, /* Known processing induced distortions in range (F16.7) */
          recgain,  /* Receiver gain (F16.7) */
          swathvel, /* Swath velocity (F16.7) */
          squintan, /* Squint angle (F16.7) */
          avgterht; /* Average terrain height above Geoid image center(F16.7)*/
   char   procgain[5],  /* Processor gain */
          deskewf[5],   /* Flag indicating whether Doppler Skew was removed */
          grndslnt[8];  /* Ground range / slant range flag */
   double sltrngfp,   /* Slant range to the first image pixel (F16.7) */
          sltrnglp;   /* Slant range to the last image pixel (F16.7) */
   int    strtsamp;   /* Start sample of signal data range line processed (I8)*/
   char   clttrlkf[5];  /* Flag indicating whether clutterlock was used */
   double dpplrfrq, /* Doppler frequency at the near range (F16.7)*/
          dpplrslp, /* Doppler frequency slope (F16.7)*/
          dpplrqdr; /* Doppler frequency quadratic term (F16.7)*/
   char   autfocsf[5];  /* Flag indicating whether autofocus was used */
   double dpplrrat, /* Doppler frequency rate at the near range (F16.7) */
          dpratslp, /* Doppler frequency rate slope (F16.7) */
          dpratqdr, /* Doppler frequency rate quadratic term (F16.7) */
          imresaz,  /* Nominal image resolution in azimuth (F16.7) */
          imresra,  /* Nominal image resolution in range (F16.7) */
          azpixspc, /* Pixel spacing in azimuth (F16.7) */
          rapixspc; /* Pixel spacing in range (F16.7) */
   char   rngcompf[5];  /* On-board range compression flag */
   int    bitssamp;  /* Bits per sample of the SAR signal data (I4) */
   double calestim, /* Calibrator estimate (F16.7) */
          biterrrt, /* Data transfer bit error rate (F16.7) */
          sigtonoi, /* Signal to noise ratio (F16.7) */
          estnoifl, /* Estimated noise floor (F16.7) */
          radiores; /* Radiometric resolution (F16.7) */
   int    nsatpnts;  /* Number of saturated points determined from hist (I8) */
   char   inspecf[5];   /* Flag to indicate whether image is within spec */

  /***** Values included in RADARSAT era CEOS structure ******/
  double  repl_agc;             /* chirp replica AGC value   */
  double  temp_rx_lna;          /* temp of rcvr LNA          */
  double  temp_rx_sub;          /* temp of rcvr subsystem    */
  double  temp_rx_prot;         /* temp of rcvr protector    */
  double  temp_cal_sys;         /* temp of calib system      */
  double  rx_agc;               /* rcvr AGC value            */
  double  pre_cal1_pow;         /* pre cal1 avg power        */
  double  pre_cal2_pow;         /* pre cal2 avg power        */
  double  post_cal1_pow;        /* post cal1 avg power       */
  double  post_cal2_pow;        /* post cal2 avg power       */
  double  repl_pow;             /* Replica avg power         */
  double  ssar_roll_ang;        /* est ScanSAR roll angle    */
  /***** Values included in RADARSAT era CEOS structure ******/

   char   comment[101];         /* comment field */
};

/*************************************/
/* ESA Facility Related Data Record. */
/*************************************/
struct ESA_FACDR {
   char seq_num[65]; /* name of facility data record */
   char qc_release[7]; /* date of last release of QC software */
   char spare1[3];   /* spare */
   char cal_update[7]; /* date of last calibration update */
   short qa_flag; /* overall QA summary flag (sum of the next 9 following flags */
   short prf_flag; /* PRF code change (0 = PRF constant in scene) */
   short samp_flag; /* sampling window start time change flag (0 = SWST constant) */
   short cal_flag; /* cal. system & receiver gain change flag (0 = Cal/Rx gain constant) */
   short chirp_flag; /* chirp replica quality flag (0 = Replica XCF in limits) */
   short dop_conf_flag; /*Doppler centroid confidence measure flag (0 = in limits) */
   short dop_val; /* Doppler centroid value (0 = Dopp-centroid less than PRF_2 */
   short dop_amb_conf_flag; /* Doppler ambiguity confidence measure flag (0 = in limits) */
   short out_flag; /* output data mean flag (0 = image mean or sd in limits) */
   short range_flag; /* on ground / on board range compressed flag (0 = OGRC) */
   short n_prf_changes; /* number of PRF code changes */
   short n_samp_changes; /* number of sampling window changes */
   short n_gain_changes; /* number of calibration sybsystem gain changes */
   short n_miss_lines; /* number of missing lines (i.e. raw data input lines) */
   short n_rec_gain_changes; /* number of receiver gain changes */
   double width_ccf; /* 3-dB pulse width of (first) Chirp Replica Cross Correlation Function (CCF) [samples] */
   double lobe_ccf; /* first side lobe level of chirp CCF [dB] */
   double islr; /* ISLR of chirp CCF function [dB]*/
   double dop_cent_conf; /* Doppler centroid confidence measure */
   double dop_amb_conf; /* Doppler ambiguity confidence measure */
   double main_i; /* estimated mean of I input data */
   double main_q; /* estimated mean of Q input data */
   double stddev_i; /* estimated standard deviation of I input data */
   double stddev_q; /* estimated standard deviation of Q input data */
   double cal_gain; /* calibration system gain of first processed line (telemetry value) */
   double rec_gain; /* receiver gain of first processed line (telemetry value) */
   double dop_amb; /* Doppler ambiguity number */
   char spare2[17]; /* spare */
   double bias_i; /* bias correction applied to I channel (to be added to the nominal bias) */
   double bias_q; /* bias correction applied to Q channel (to be added to the nominal bias) */
   double gain_i; /* I/Q gain imbalance correction (applied to I channel) */
   double gain_q; /* I/Q gain imbalance correction (applied to Q channel) */
   double non_ortho; /* I/Q non-orthogonality correction (applied to Q channel) */
   char spare3[17]; /* spare */
   double noise_pow; /* estimated nosie power per sample */
   char pulse_delay[17]; /* calibration pulse time delay [ns] */
   short n_cal_pulses; /* number of valid calibration pulses [pulses] */
   short n_noise_pulses; /* number of valid noise pulses [pulses] */
   short n_rep_pulses; /* number of valid replica pulses [pulses] */
   double f_samp_rep; /* first sample in replica [samples] */
   double cal_pulse_pow; /* mean calibration pulse power */
   double noise_pulse_pow; /* mean noise pulse power */
   double range_comp; /* range compression normalization factor */
   double rep_pulse_pow; /* replica pulse power */
   double inc_first_pix; /* incidence angle at first range pixel (at mid-azimuth) [degrees] */
   double inc_center_pix; /* incidence angle at center range pixel (at mid_azimuth) [degrees] */
   double inc_last_pix; /* incidence angle at last range pixel (at mid-azimuth) [degrees] */
   double sl_range_ref; /* slant range reference [km] */
   char spare4[13]; /* spare */
   short ant_pattern_flag; /* antenna pattern correction flag (0 = no correction) */
   double abs_cal_const; /* absolute calibration constant K [scalar] */
   double up_cal_const; /* upper bound calibration constant K (+ 3 std dev) */
   double low_cal_const; /* lower bound calibration constant K (- 3 std dev) */
   double sigma_0; /* estimated noise equivalent sigma 0 [dB] */
   char k_gen[7]; /* date on which K was generated */
   char k_ver[5]; /* K version number */
   short dup_lines; /* number of duplicated input lines */
   double ber; /* estimated bit error rate */
   char spare5[13]; /* spare */
   double img_mean; /* output image mean */
   double img_std_dev; /* output image standard deviation */
   double img_max; /* output image maximum value */
   char t_first_line[25]; /* time of raw data first input range line (UTC) */
   char t_asc_node[25]; /* time of ascending node state vectors (UTC) */
   double x_asc_pos, y_asc_pos, z_asc_pos; /* state vector positions ascending node in x,y,z [m] */
   double x_asc_vel, y_asc_vel, z_asc_vel; /* state vector velocities ascending node in x,y,z [m/s] */
   short out_bits; /* output pixel bit length [bits] */
   double gain1; /* processor gain #1 */
   double gain2; /* processor gain #2 */
   double gain3; /* processor gain #3 */
   short peak_loc_first; /* peak location of CCF between first extracted chirp and nominal chirp [samples] */
   double width_ccf2; /* 3-dB width of CCF between last extracted chirp and nominal chirp [samples] */
   double f_lobe; /* first side lobe level of chirp CCF between last extracted chirp and nominal chirp [dB] */
   double islr_ccf; /* ISLR of chirp CCF between last extracted chirp and nominal chirp [dB] */
   short peak_loc_last; /* peak location of CCF between last extracted chirp and nominal chirp [samples] */
   short roll_flag; /* roll tilt mode flag (0 = not in roll tilt mode) */
   short raw_data_flag; /* raw data correction flag (0 = correction with default parameters) */
   short look_flag; /* look detection flag (1 = power detected and summed) */
   short dop_amb_flag; /* Doppler ambiguity estiamtion flag (0 = no estimation done) */
   short az_base_flag; /* azimuth baseband conversion flag (0 = no conversion done) */
   short samp_per_line; /* samples per line used for the raw data analysis [samples] */
   short lines_skip; /* range lines skip factor for raw data analysis [lines] */
   char t_in_stvec[25]; /* time of input state vector (UTC) used for processed the image */
   double x_in_pos, y_in_pos, z_in_pos; /* input state vector position in x,y,z [m] */
   double x_in_vel, y_in_vel, z_in_vel; /* input state vector velocity in x,y,z [m/s] */
   short in_stvec_flag; /* input state vector type flag (0 = predicted, 1 = restituted, preliminary or precise) */
   double range_filt; /* window coefficient for range-matched filter */
   double az_filt; /* window coefficient for azimuth-matched filter */
   short update_filt; /* update period of range-matched filter [chirps] */
   double look_gains[8]; /* look scalar gains (up to 8 looks) */
   short samp_win_bias; /* sampling window start time bias [ns] */
   double dop_cube_coef; /* Doppler centroid cubic coefficient [Hz/sec3] */
   short prf_first_line; /* PRF code of first range line (telemetry value) */
   short prf_last_line; /* PRF code of last range line (telemetry value) */
   short win_first_line; /* sampling window start time code of first range line (telemetry value) */
   short win_last_line; /* sampling window start time code of last range line (telemetry value) */
   short cal_gain_last_line; /* calibration system gain of last processed line (telemetry value) */
   short rec_gain_last_line; /* receiver gain of last processed line (telemetry value) */
   short first_range_samp; /* first processed range sample */
   short fft_ratio; /* azimuth FFT/IFFT ratio */
   short n_az_blocks; /* number of azimuth blocks processed */
   int n_in_lines; /* number of raw data lines [lines] */
   short ini_dop_amb; /* initial Doppler ambiguity number */
   double chirp_quality[3]; /* chirp quality threshold [pixels,dB,dB] */
   double in_data_stats[4]; /* input data statistic thresholds */
   double dop_amb_thres[2]; /* Doppler ambiguity confidence thresholds */
   double out_data_stats[2]; /* output data statistics thresholds */
   char t_sat_first_line[17]; /* satellite binary time of first range line (telemetry value) */
   short n_val_pix; /* number of valid pixels per range line (the remaining pixels are zero padded [pixels] */
   short n_range_samp; /* number of range samples discarded during processing interpolations [samples] */
   double gain_low; /* I/Q gain imbalance - lower bound */
   double gain_up; /* I/Q gain imbalance - upper bound */
   double quad_low; /* I/Q quadrature departure - lower bound [degrees] */
   double quad_up; /* I/Q quadrature departure - upper bMod0_CGI236ound [degrees] */
   double look_bw; /* 3-dB look bandwidth [Hz] */
   double dop_bw; /* 3-dB processed Doppler bandwidth [Hz] */
   short range_spread; /* range spreading loss compensation flag (0 = no compensation) */
   char more_flags[17]; /* some more flags */
   short max_look; /* maximum value of look scalar gain flag (1 = automatically calculated) */
   short rep_norm_flag; /* replica normalization method flag */
   double gr2sr_poly[4]; /* 4 coefficients of the ground range to slant range conversion polynominal */
   double ant_elev_poly[5]; /* 5 coefficients of the antenna elevation pattern polynomial */
   double range_time_poly; /* range time of origin of antenna pattern polynomial */
   char spare6[10239]; /* spare */
};

/* Map Projection Data Record - Value Structure */
struct VMPDREC {
  char    mpdesc[33];    /* Map projection descriptor */
  int     npixels,       /* number of pixels per line of image */
          nlines;        /* number of lines in image */
  double  nomipd,        /* nominal inter-pixel distance in output (meters) */
          nomild,        /* nominal inter-line distance in output (meters) */
          orient,        /* Orientation at output scene center */
          orbitincl,     /* Actual platform orbital inclination (degrees) */
          ascnode,       /* Actual ascending node (degrees) */
          distplat,      /* Distance of platform from geocenter (meters) */
          altplat,       /* Altitude of platform rel. to ellipsoid (meters) */
          velnadir,      /* Actual ground speed at nadir (m/s) */
          plathead;      /* platform heading (degrees) */
  char    refelip[33];   /* Name of reference ellipsoid */
  double  remajor,       /* Semimajor axis of ref. ellipsoid (m) */
          reminor,       /* Semiminor axis of ref. ellipsoid (m) */
          datshiftx,     /* Datum shift parameter ref. to Greenwich dx (m) */
          datshifty,     /* datum shift perpendicular to Greenwich dy (m) */
          datshiftz,     /* datum shift direction of rotation axis dz (m) */
          datshift1,     /* 1st additional datum shift, rotation angle */
          datshift2,     /* 2nd additional datum shift, rotation angle */
          datshift3,     /* 3rd additional datum shift, rotation angle */
          rescale;       /* Scale factor of referenced ellipsoid */
  char    mpdesig[33],   /* Alphanumeric description of map projection */
          utmdesc[33],   /* UTM descriptor */
          utmzone[5];    /* Signature of UTM Zone */
  double  utmeast,       /* Map origin - false easting */
          utmnorth,      /* Map origin - false north */
          utmlong,       /* Center of projection longitude (deg) */
          utmlat,        /* Center of projection latitude (deg) */
          utmpara1,      /* 1st standard parallel (deg) */
          utmpara2,      /* 2nd standard parallel (deg) */
          utmscale;      /* UTM scale factor */
  char    upsdesc[33];   /* UPS descriptor */
  double  upslong,       /* Center of projection longitude (deg) */
          upslat,        /* Center of projection latitude (deg) */
          upsscale;      /* UPS scale factor */
  char    nspdesc[33];   /* NSP projection description */
  double  nspeast,       /* Map origin - false east */
          nspnorth,      /* Map origin - false north */
          nsplong,       /* Center of projection longitude (deg) */
          nsplat,        /* Center of projection latitude (deg) */
          nsppara1,      /* Standard parallels */
          nsppara2,      /* Standard parallels */
          nsppara3,      /* Standard parallels */
          nsppara4,      /* Standard parallels */
          nspcm1,        /* Central meridian */
          nspcm2,        /* Central meridian */
          nspcm3;        /* Central meridian */
  double  tlcnorth,      /* Top left corner north (m) */
          tlceast,       /* Top left corner east (m) */
          trcnorth,      /* Top right corner north (m) */
          trceast,       /* Top right corner east (m) */
          brcnorth,      /* Bottom right corner north (m) */
          brceast,       /* Bottom right corner east (m) */
          blcnorth,      /* Bottom left corner north (m) */
          blceast,       /* Bottom left corner east (m) */
          tlclat,        /* Top left corner latitude (deg) */
          tlclong,       /* Top left corner longitude (deg) */
          trclat,        /* Top right corner lat (deg) */
          trclong,       /* Top right corner int (deg) */
          brclat,        /* Bottom right corner latitude (deg) */
          brclong,       /* Bottom right corner longitude (deg) */
          blclat,        /* Bottom left corner latitude (deg) */
          blclong,       /* Bottom left corner longitude (deg) */
          tlcheight,     /* Top left corner terrain height (m) */
          trcheight,     /* Top right corner terrain height (m) */
          brcheight,     /* Bottom right corner terrain height (m) */
          blcheight,     /* Bottom left corner terrain height (m) */
          a11,           /* 8 coeff. to convert a line (L) and pixel (p) */
          a12,           /* position to the map projection frame of      */
          a13,           /* reference, say (E,N) where:                  */
          a14,           /*                                              */
          a21,           /*  E=A11+A12+A13+A14TL                         */
          a22,           /*  N=A21+A22+A23+A24+nP                        */
          a23,           /*                                              */
          a24,           /*                                              */
          b11,           /* 8 coeff. to convert from the map projection  */
          b12,           /* (E,N) to line (L) and pixel (P) position.    */
          b13,           /*                                              */
          b14,           /* L=B11+B12+B13+B14TETN                        */
          b21,           /* P=B21+B22+B23+B24TN                          */
          b22,           /*                                              */
          b23,           /*                                              */
          b24;           /*                                              */
};


/****************************
*                           *
*  SAR Leader File:         *
*  Data Set Summary Record  *
*                           *
****************************/

struct dataset_sum_rec {
    short  seq_num;              /* Data Set Summary: record sequence number */
    short  sar_chan;             /* SAR channel indicator */
    char   product_id[17];       /* site ID = product id */
    char   scene_des[33];        /* site name */
    char   inp_sctim[33];        /* image center GMT: YYYYMMDDhhmmssttt */
    char   asc_des[17];          /* Ascending/descending */
    double pro_lat;              /* latitude at scene center */
    double pro_long;             /* longitude at scene center */
    double pro_head;             /* Processed scene center heading */
    char   ellip_des[17];        /* ellipsoid designator */
    double ellip_maj;            /* ellipsoid semimajor axis (km) */
    double ellip_min;            /* ellipsoid semiminor axis (km) */
    double earth_mass;           /* Earth's mass */
    double grav_const;           /* Gravitational constant */
    double ellip_j[3];           /* Ellipsoid J2-4 parameters */

    /***** Value included in RADARSAT era CEOS structure ******/
    char  spare1[17];            /* spare */
    /***** Value included in RADARSAT era CEOS structure ******/

    double terrain_h;            /* average terrain height */
    double sc_lin;               /* image center line number (azimuth) */
    double sc_pix;               /* image center pixel number (range) */
    double scene_len;            /* image length in km */
    double scene_wid;            /* image width in km */
    char   spare2[17];           /* spare */
    short  nchn;                 /* number of SAR channels */
    char   spare3[5];            /* spare */
    char   mission_id[17];       /* mission id */
    char   sensor_id[33];        /* sensor id: AAAAAA-BB-CCDD-EEFF */
    char   revolution[9];        /* orbit number */
    double plat_lat;             /* spacecraft latitude at nadir */
    double plat_long;            /* spacecraft longitude at nadir */
    double plat_head_scene;      /* sensor platform heading (degrees) */
    double clock_ang;            /* sensor clock angle rel to flight dir */
    double incident_ang;         /* incidence angle at image center */
    double frequency;            /* radar frequency (GHz) */
    double wave_length;          /* radar wavelength (m) */
    char   motion_comp[3];       /* motion compensation indicator */
    char   pulse_code[17];       /* range pulse code specifier */
    double ampl_coef[5];         /* range chirp coefficients */
    double phas_coef[5];         /* range phase coefficients */
    int    chirp_ext_ind;        /* chirp extraction index */
    char   spare4[9];            /* spare */
    double rng_samp_rate;        /* range complex sampling rate */
    double rng_gate;             /* range gate at early edge */
    double rng_length;           /* range pulse length */
    char   baseband_f[5];        /* base band conversion flag */
    char   rngcmp_f[5];          /* range compressed flag */
    double gn_polar;             /* receiver gain for like pol */
    double gn_cross;             /* receiver gain for cross pol */
    int    chn_bits;             /* quantization bits per channel */
    char   quant_desc[13];       /* quantizer description */
    double i_bias;               /* I channel DC bias */
    double q_bias;               /* Q channel DC bias */
    double iq_ratio;             /* I/Q channel ratio */
    double spare_dss_7;          /* spare */
    double spare_dss_8;          /* spare */
    double ele_sight;            /* electronic boresight */
    double mech_sight;           /* mechanical boresight */
    char   echo_track[5];        /* echo tracker flag */
    double prf;                  /* nominal PRF */
    double elev_beam;            /* antenna elevation 3dB beam width */
    double azi_beam;             /* antenna azimuth 3dB beam width */
    char   sat_bintim[17];       /* Satellite binary time */
    char   sat_clktim[33];       /* Satellite clock time */
    int    sat_clkinc;           /* Satellite clock increment */
    char   spare5[9];            /* spare */
    char   fac_id[17];           /* processing facility */
    char   sys_id[9];            /* processing system */
    char   ver_id[9];            /* processor version */
    char   fac_code[17];         /* facility process code */
    char   lev_code[17];         /* product code */
    char   product_type[33];     /* product type */
    char   algor_id[33];         /* processing algorithm */
    double n_azilok;             /* number of looks in azimuth */
    double n_rnglok;             /* number of looks in range */
    double bnd_azilok;           /* bandwidth per look in azimuth */
    double bnd_rnglok;           /* bandwidth per look in range */
    double bnd_azi;              /* processor bandwidth (azimuth) */
    double bnd_rng;              /* processor bandwidth (range) */
    char   azi_weight[33];       /* weighting function (azimuth) */
    char   rng_weight[33];       /* weighting function (range) */
    char   data_inpsrc[17];      /* data input source: HDDC id */
    double rng_res;              /* nominal resolution (range) */
    double azi_res;              /* nominal resolution (azimuth) */
    double radi_stretch[2];      /* radiometric stretch terms (bias, gain) */
    double alt_dopcen[3];        /* along track Doppler freq terms */
    char   spare6[17];           /* spare */
    double crt_dopcen[3];        /* cross track Doppler freq terms */
    char   time_dir_pix[9];      /* time direction (range) */
    char   time_dir_lin[9];      /* time direction (azimuth) */
    double alt_rate[3];          /* Aint track Doppler rate terms */
    char   spare7[17];           /* spare */
    double crt_rate[3];          /* Cross track Doppler rate terms */
    char   spare8[17];           /* spare */
    char   line_cont[9];         /* line content indicator */
    char   clutterlock_flg[5];   /* clutter lock flag */
    char   auto_focus[5];        /* autofocussing flag */
    double line_spacing;         /* line spacing (m) */
    double pixel_spacing;        /* pixel spacing (m) */
    char   rngcmp_desg[17];      /* range compression designator */

    /****** Values included in PRE RADARSAT era data files *****/
    char   spare9[273];          /* spare */
    int    annot_pts;            /* number of annotation points */
    int    spare10[9];           /* spare */
    int    annot_line[64];       /* Line number of annotation start */
    int    annot_pixel[64];      /* Pixel number of annotation start */
    char   annot_text[64][37];   /* Annotation text */
    char   spare11[26];          /* spare */
    /****** Values included in PRE RADARSAT era data files *****/

    /****** Values included in RADARSAT era data files *****/
    char   spare_dss_14[17];     /* spare */
    char   spare_dss_15[17];     /* spare */
    int    no_beams;             /* number of beams */
    char   beam1[5];             /* beam 1 identifier */
    char   beam2[5];             /* beam 2 identifier */
    char   beam3[5];             /* beam 3 identifier */
    char   beam4[5];             /* beam 4 identifier */
    float  prf1;                 /* PRF of beam 1 Hz */
    float  prf2;                 /* PRF of beam 2 Hz */
    float  prf3;                 /* PRF of beam 3 Hz */
    float  prf4;                 /* PRF of beam 4 Hz */
    float  rng_gate1;            /* range gate of beam 1 (usec) */
    float  rng_gate2;            /* range gate of beam 2 (usec) */
    float  rng_gate3;            /* range gate of beam 3 (usec) */
    float  rng_gate4;            /* range gate of beam 4 (usec) */
    int    tot_pls_burst;        /* total pulses per burst */
    int    val_pls_burst;        /* valid pulses per burst */
    int    az_ovlp_nxt_img;      /* Range lines overlap in azimuth with next */
    int    rg_off_nxt_img;       /* pixel of offset in range with next */
    char   cal_params_file[33];  /* calibration parameter file used */
    char   scan_results_file[33]; /* name of scan results file used */
    char   scanner_version[17];  /* version of the scanner used     */
    char   decode_version[17];   /* version of the decode used      */
    char   spare_dss_16[2130];   /* spare */
    /****** Values included in RADARSAT era data files *****/

    /****** ESA data set summary record fields *****/
    double rng_time[3];             /* zero-Doppler range times */
    char   az_time_first[25];       /* zero-Doppler azimuth time first pixel */
    char   az_time_center[25];      /* zero-Doppler azimuth time center pixel */
    char   az_time_last[25];        /* zero-Doppler azimuth time last pixel */
    /****** ESA data set summary record fields *****/

  // Begin ALOS data set summary record fields
  short cal_data_indicator;   // calibration data indicator
  int start_cal_up;           // start line number of calibration at upper image
  int stop_cal_up;            // stop line number of calibration at upper image
  int start_cal_bottom;       // start line number of calibration at bottom image
  int stop_cal_bottom;        // stop line number of calibration at bottom image
  short prf_switch;           // PRF switching indicator
  int line_prf_switch;        // line locator of PRF switching
  double beam_center_dir;     // direction of a beam center in a scene scenter
  short yaw_steering;         // yaw steering mode flag
  short param_table;          // parameter table number of automatically setting
  double off_nadir_angle;     // nominal offnadir angle
  short ant_beam_num;         // antenna beam number
  char spare12[8];            // spare
  double incid_a[6];          // incidence angle parameter a
  // End ALOS data set summary record fields
};


/****************************
*                           *
*  SAR Leader File:         *
*  Platform Position Rec    *
*                           *
****************************/
struct pos_data_rec {
    short  seq_num;             /* Platform Position: record sequence # */
    char   orbit_ele_desg[33];  /* Orbital elements designator          */
    double orbit_ele[6];        /* orbital elements                     */
    short  ndata;               /* number of data sets                  */
    short  year;                /* year of first data point             */
    short  month;               /* month of first data point            */
    short  day;                 /* day of first data point              */
    short  gmt_day;             /* day in year of first data point      */
    double gmt_sec;             /* seconds in day of first data point   */
    double data_int;            /* time interval between data points(s) */
    char   ref_coord[65];       /* reference coordinate system          */
    double hr_angle;            /* GMT hour angle (degrees)             */
    double alt_poserr;          /* Aint track position error            */
    double crt_poserr;          /* Cross track position error           */
    double rad_poserr;          /* radial position error                */
    double alt_velerr;          /* Aint track velocity error            */
    double crt_velerr;          /* Cross track velocity error           */
    double rad_velerr;          /* Radial velocity error                */
    double pos_vec[64][6];      /* Data point position/velocity         */
                                /* 0-2 position,  3-5  velocity         */
    char spare_ppr_1[243];      /* spare                                */
};

/******************************************
*                                         *
*  SAR Leader File: Attitude Data Record  *
*                                         *
******************************************/

struct  att_vect_rec {
    short  gmt_day;             /* day in the year (GMT)        */
    int    gmt_msec;            /* millisecond of the day (GMT) */
    short  pitch_flag;          /* pitch data quality flag      */
    short  roll_flag;           /* roll data quality flag       */
    short  yaw_flag;            /* yaw data quality flag        */
    double pitch;               /* pitch (degrees)              */
    double roll;                /* roll (degrees)               */
    double yaw;                 /* yaw (degrees)                */
    short  pitch_rate_flag;     /* pitch rate data quality flag */
    short  roll_rate_flag;      /* roll rate data quality flag  */
    short  yaw_rate_flag;       /* yaw rate data quality flag   */
    double pitch_rate;          /* pitch rate (degrees/sec)     */
    double roll_rate;           /* roll rate (degrees/sec)      */
    double yaw_rate;            /* yaw_rate (degrees/sec)       */
    struct att_vect_rec *next;  /* pointer to next data set     */
};

struct  att_data_rec {
    short npoint;               /* number of attitude data sets */
    struct att_vect_rec *data;  /* pointer to list of data sets */
    char spare_adr_1[641];      /* spare                        */
};

/*********************************************
*                                            *
*  SAR Leader File: Radiometric Comp Record  *
*  Not in the ASF set for the Radarsat era   *
*                                            *
*********************************************/

struct Radio_Comp_Tbl {
    double sample_offset;        /* compensation sample offset */
    double sample_gain;          /* compensation sample gain */
    struct Radio_Comp_Tbl *next; /* link */
};

struct Rad_Comp_Set {
    char  comp_data_type[8];    /* compensation data type designator */
    char  data_descr[33];       /* data descriptor */
    short req_recs;             /* number of required records */
    short table_seq_num;        /* table sequence number */
    int  num_pairs;             /* total number of tables */
    int  first_pixel;           /* pixel corresponding to first correction */
    int  last_pixel;            /* pixel corresponding to last correction */
    int  pixel_size;            /* pixel group size (subsample factor) */
    double min_samp_index;      /* minimum sample index */
    double min_comp_value;      /* minimum radiometric compensation value */
    double max_samp_index;      /* maximum sample index */
    double max_comp_value;      /* maximum radiometric compensation value */
    char  spare_rcr_2[17];      /* spare */
    int  n_table_entries;       /* number of comp pairs */
    struct Radio_Comp_Tbl *tbl; /* pointer to linked list */
    struct Rad_Comp_Set *next;  /* pointer to next pol */
};

struct radi_comp_rec {
    short    seq_num;           /* radio comp record seq number */
    short    sar_chan;          /* sar channel indicator */
    int     n_dset;             /* number of data sets */
    int     dset_size;          /* data set size */
    char     spare_rcr_1[5];    /* padding for link list */
    struct Rad_Comp_Set  *set;  /* pointer to comp, dataset */
    struct radi_comp_rec *next; /* pointer to next radi. comp. record */
};

/*******************************************
*                                          *
*  SAR Leader File:  Data Quality Summary  *
*                                          *
*******************************************/

struct qual_sum_rec {
    short seq_num;              /* dqs record sequence number       */
    char  chan_ind[5];          /* sar channel indicator            */
    char  cali_date[7];         /* calibration date                 */
    short nchn;                 /* number of channels               */
    double islr;                /* integrated side lobe ratio       */
    double pslr;                /* peak side lobe ratio             */
    double azi_ambig;           /* azimuth ambiguity                */
    double rng_ambig;           /* range ambiguity                  */
    double snr;                 /* signal-to-noise ratio estimate   */
    double ber;                 /* nominal bit error rate           */
    double rng_res;             /* nominal slant range resolution   */
    double azi_res;             /* nominal azimuth resolution       */
    double rad_res;             /* nominal radiometric resolution   */
    double dyn_rng;             /* instantaneous dynamic range      */
    double abs_rad_unc_db;      /* nominal abs. radiometric uncertainty, db  */
    double abs_rad_unc_deg;     /* nominal abs. radiometric uncertainty, deg */
    double rel_rad_unc[2][16];  /* rel short term radio calibr uncertainty   */
                                /* 16 pairs of values in db and deg */
    double alt_locerr;          /* location error along track       */
    double crt_locerr;          /* location error cross track       */
    double alt_scale;           /* along track scale error          */
    double crt_scale;           /* cross track scale error          */
    double dis_skew;            /* geometric skew error             */
    double ori_err;             /* image orientation error          */
    double misreg[2][16];       /* misregistration error: 16 pairs  */
                                /* for along track and cross track  */

    /****** Value included in PRE RADARSAT era CEOS structure ******/
    char  spare2[279];          /* spare */
    /****** Value included in PRE RADARSAT era CEOS structure ******/

    /****** Value included in RADARSAT era CEOS structure **********/
    double nesz;                /* nominal noise equiv sigma zero  */
    double enl;                 /* Nominal equiv no of looks       */
    char  tb_update[9];         /* default param table update date */
    char  cal_status[17];       /* Calibration status of data      */
    char  spare3[23];           /* spare                           */
    char  cal_comment[201];     /* Calibration comment field       */
    /****** Value included in RADARSAT era CEOS structure **********/
};


/****************************
*                           *
*  SAR Leader File:         *
* Data Histograms Record    *
*                           *
****************************/

struct hist_dset {
    char  hist_desc[33];      /* histogram descriptor          */
    short nrec;               /* required records              */
    short tab_seq;            /* table sequence number         */
    int  nbin;                /* table size, bytes             */
    int  ns_lin;              /* number of pixels in line      */
    int  ns_pix;              /* number of lines in image      */
    int  ngrp_lin;            /* pixels/group, cross track     */
    int  ngrp_pix;            /* pixels/group, along track     */
    int  nsamp_lin;           /* number of groups, cross track */
    int  nsamp_pix;           /* number of groups, along track */
    double min_smp;           /* minimum pixel value           */
    double max_smp;           /* maximum pixel value           */
    double mean_smp;          /* mean sample value             */
    double std_smp;           /* std deviation of sample value */
    double smp_inc;           /* sample value increment        */
    double min_hist;          /* minimum table value           */
    double max_hist;          /* maximum table value           */
    double mean_hist;         /* histogram mean value          */
    double std_hist;          /* histogram standard deviation  */
    int  nhist;               /* histogram table size          */
    int  *data_values_hist;   /* table values 1-256            */
    struct hist_dset *next;   /* pointer to next table set     */
};

struct data_hist_rec {
    short seq_num;            /* data histogram record seq no. */
    short sar_chan;           /* SAR channel                   */
    int  ntab;                /* number of table sets          */
    int  ltab;                /* data set size                 */
    struct hist_dset *data;   /* pointer to list of tables     */
};

/****************************
*                           *
*  SAR Leader File:         *
*  Range Spectra Record     *
*                           *
****************************/

struct rng_spec_rec {
    short seq_num;                /* range spectra sequence no. */
    short sar_chan;               /* SAR channel */
    int  n_dset;                  /* number of data sets */
    int  dset_size;               /* data set size */
    short req_recs;               /* number of records required */
    short table_no;               /* table sequence number */
    int  n_pixels;                /* number of pixels in line */
    int  pixel_offset;            /* offset from first pixel */
    int  n_lines;                 /* number of lines integrated for spectra */
    double first_freq;            /* center freq of first spectra bin */
    double last_freq;             /* center freq of last spectra bin */
    double min_power;             /* minimum spectral power */
    double max_power;             /* maximum spectral power */
    char  spare_rsr_1[17];        /* spare */
    char  spare_rsr_2[17];        /* spare */
    int  n_bins;                  /* number of freq bins in table */
    double data_values_spec[256]; /* spectral data values 1-256 */
    char  spare_rsr_3[1053];      /* spare */
};

/****************************
*                           *
*  SAR Leader File:         *
*  Digital Elevation Record *
*                           *
****************************/

struct corner_pts_rec {
    float cp_lat_1;              /* polygon corner pt. latitude */
    float cp_lon_1;              /* polygon corner pt. longitude */
    struct corner_pts_rec *next; /* pointer to next corner point */
};

struct dem_desc_data {
    short poly_seq_num;         /* polygon sequence number */
    short num_crnr_pts;         /* Number of corner pts. for this record */
    char  spare_dem_1[9];       /* spare */
    struct corner_pts_rec* pts; /* pointer to corner pts/lat/int */
    struct dem_desc_data *next; /* pointer to next DEM data desc. data set */
};

struct dig_elev_rec {
    short seq_num;              /* sequence no. */
    int  ttl_num_sets;          /* total number of DEM datasets */
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
    struct dem_desc_data* set;  /* pointer to list of DEM desc data sets */
    char spare_dem_2[533];      /* spare */
};

/***************************************************
*                                                  *
*  SAR Leader File:  Detail Processing Parameters  *
*  As defined by CSA in their CDPF product specs   *
*  issue/rev = 3/0  date = apr22,1994              *
*                                                  *
***************************************************/

typedef struct beam_info_rec {
    char beam_type[4];          /* Beam type                            (a3) */
    char beam_look_src[10];     /* Elevation beam look angle source     (a9) */
    float beam_look_ang;        /* Applied elev beam look angle, deg (f16.7) */
    float prf;                  /* Actual PRF (Hz)                   (f16.7) */
    struct beam_info_rec *next; /* pointer to next beam info rec             */
} Beam_Info;

typedef struct pix_count_rec {
    char pix_update[22];        /* pixel count update time             (a21) */
    int n_pix[4];               /* count of image pixels in beams      (4i8) */
    struct pix_count_rec *next; /* pointer to next pix count rec             */
} Pix_Count;

typedef struct temp_rec {
    int temp_set[4];            /* temperature settings                (4i4) */
    struct temp_rec *next;      /* pointer to next temp rec                  */
} Temp_Rec;

typedef struct dopcen_est_rec {
    float dopcen_conf;           /* Doppler Centroid conf measure     (f16.7) */
    float dopcen_ref_tim;        /* Doppler Centroid ref time         (f16.7) */
    float dopcen_coef[4];        /* Doppler Centroid coefficients    (4f16.7) */
    struct dopcen_est_rec *next; /* pointer to next dopcen rec                */
} Dopcen_Est;

typedef struct srgr_coefset_rec {
    char srgr_update[22];       /* SRGR update date/time               (a21) */
    float srgr_coef[6];         /* SRGR coefficients                (6f16.7) */
    struct srgr_coefset_rec *next; /* pointer to next SRGR rec               */
} SRGR_Coefset;

struct proc_parm_rec {
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
    int miss_ln;                /* Number of missing lines              (i8) */
    int rej_ln;                 /* Number of rejected lines             (i8) */
    int large_gap;              /* Number of time inconsistencies       (i8) */
    float bit_error_rate;       /* Measured bit error rate           (f16.7) */
    float fm_crc_err;           /* Percent of frames with CRC errors (f16.7) */
    int date_incons;            /* Number of date inconsistencies       (i8) */
    int prf_changes;            /* Number of unexpected PRF changes     (i8) */
    int delay_changes;          /* Number of delay changes              (i8) */
    int skipd_frams;            /* Number of skippped frames            (i8) */
    int rej_bf_start;           /* Rng lines reject before start time   (i8) */
    int rej_few_fram;           /* Rng lines reject: too few frames     (i8) */
    int rej_many_fram;          /* Rng lines reject: too many frames    (i8) */
    int rej_mchn_err;           /* Frames reject: master ch err         (i8) */
    int rej_vchn_err;           /* Frames reject: virtual ch err        (i8) */
    int rej_rec_type;           /* Frames reject: rec type err          (i8) */
    int prd_qual_ind;           /* Product quality index                (i4) */
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
    int pulse_bandw;            /* Pulse bandwidth code                 (i4) */
    char adc_samp_rate[6];      /* ADC sampling rate                    (a5) */
    float rep_agc_attn;         /* Replica AGC attenuation           (f16.7) */
    float gn_corctn_fctr;       /* Gain correction factor (dB)       (f16.7) */
    float rep_energy_gn;        /* Replica energy gain correction    (f16.7) */
    char orb_data_src[12];      /* Orbit data source                   (a11) */
    int pulse_cnt_1;            /* Pulse count 1                        (i4) */
    int pulse_cnt_2;            /* Pulse count 2                        (i4) */
    char beam_edge_rqd[4];      /* Beam edge detection requested        (a3) */
    float beam_edge_conf;       /* Beam edge confidence measure      (f16.7) */
    int pix_overlap;            /* Number of pixels in beam overlap     (i4) */
    int n_beams;                /* Number of beams                      (i4) */
    Beam_Info* beam_info;       /* Beam info               (repeats 4 times) */
    int n_pix_updates;          /* Number of pixel count updates        (i4) */
    Pix_Count* pix_count;       /* Beam pixel count       (repeats 20 times) */
    float pwin_start;           /* Processing window start time, sec (f16.7) */
    float pwin_end;             /* Processing window end time, sec   (f16.7) */
    char recd_type[9];          /* Recording type                       (a9) */
    float temp_set_inc;         /* Time increment btwn temp set, sec (f16.7) */
    int n_temp_set;             /* Number of temp settings              (i4) */
    Temp_Rec* temp;             /* Temperature settings   (repeats 20 times) */
    int n_image_pix;            /* Number of image pixels               (i8) */
    float prc_zero_pix;         /* Percent zero pixels               (f16.7) */
    float prc_satur_pix;        /* Percent saturated pixels          (f16.7) */
    float img_hist_mean;        /* Image histogram mean intensity    (f16.7) */
    float img_cumu_dist[3];     /* Image cumulative distribution    (3f16.7) */
    float pre_img_gn;           /* Pre-image calibration gain factor (f16.7) */
    float post_img_gn;          /* Post-image calibration gain factor(f16.7) */
    float dopcen_inc;           /* Time inc btwn Doppler Cen est, sec(f16.7) */
    int n_dopcen;               /* Number of Doppler Centroid est       (i4) */
    Dopcen_Est* dopcen_est;     /* Doppler Centroid rec   (repeats 20 times) */
    int dopamb_err;             /* Doppler ambiguity error              (i4) */
    float dopamb_conf;          /* Doppler ambiguity confidence meas.(f16.7) */
    float eph_orb_data[7];      /* Ephemeris orbit data             (7f16.7) */
    char appl_type[13];         /* Application type                    (a12) */
    double first_lntim;         /* Slow time of first image line    (d22.15) */
    double lntim_inc;           /* time inc btwn image lines        (d22.15) */
    int n_srgr;                 /* Number of SRGR coeff sets            (i4) */
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
};

struct PPREC {
    char beam_type[3];
};


/****************************/
/* Volume Descriptor Record */
/****************************/

struct VDREC {
        char flag1[3];          /* ASCII/EBDDIC flag */
        char blank[3];          /* Blanks */
        char format_doc[13];    /* Format control document */
        char super_doc[3];      /* Superstructure format control document */
        char super_doc_rev[3];  /* Superstructure record format revision */
        char sw_release[13];    /* Logical volume generating facility software release and revision level */
        char id[17];            /* ID of physical volume containing this volume descriptor */
        char vol_id[17];        /* Logical volume identifier */
        char vol_set_id[17];    /* Volume set identifier */
        int vol_nrs;            /* Various volume numbers 4 x I2 */
        short vol_nr;           /* First referenced file number in this physical volume within the logical volume */
        short vol_volset;       /* Logical volume number within volume set */
        short vol_phys_vol;     /* Logical volume number within physical volume */
        char create_date[9];    /* Logical volume creation date (YYYYMMDD) */
        char create_time[9];    /* Logical volume creation time (hhmmssdd, dd-deci-seconds) */
        char country[13];       /* Logical volume generation country */
        char agency[9];         /* Logical volume agency */
        char paf[13];           /* Logical volume generating facility */
        short fprs;             /* Number of pointer records in volume directory */
        short rec_nr;           /* Number of records in volume directory */
        char spare[93];         /* Volume descriptor spare segment (always blank filled) */
        char local[101];        /* Local use segment */
};


/***********************/
/* File Pointer Record */
/***********************/

struct FPREC {
        char flag1[3];          /* ASCII/EBCDIC flag */
        char blank[3];          /* Blanks */
        short nr;               /* Referenced file number */
        char name[17];          /* Referenced file name */
        char file_class[29];    /* Referenced file class */
        char class_code[5];     /* Referenced file class code */
        char data_type[29];     /* Referenced file data type */
        char type_code[5];      /* Referenced file data type code */
        int records;            /* Number of records in referenced file */
        int first_rec;          /* Referenced file - 1st record length */
        int max_rec;            /* Referenced file maximum record length */
        char rec_type[13];      /* Referenced file record length type */
        char rec_type_code[5];  /* Referenced file record length type code */
        short vol_nrs;          /* Referenced file physical volume start/end number 2 x I2 */
        int start;              /* Referenced file portion start, 1st record number for this physical volume */
        int end;                /* Referenced file portion end, last record number for this physical volume */
        char spare[101];        /* File pointer spare segment */
        char local[101];        /* Local use segment */
};


/***************/
/* Text Record */
/***************/

struct TREC {
        char flag1[3];          /* ASCII/EBCDIC flag */
        char flag2[3];          /* Continuation flag */
        char prod_type[41];     /* Product type specifier */
        char location[61];      /* Location and date/time of product creation */
        char vol_phys_id[41];   /* Physical volume identification */
        char scene_id[41];      /* Scene identification */
        char scene_loc[41];     /* Scene location */
        char spare1[21];        /* Spares */
        char spare2[105];       /* Spares */
};

// Radiometric Compensation Data Record
struct radio_comp_data_rec {
  short seq_num;              // Record sequence number
  short sar_channel;          // SAR channel indicator
  int num_rec;                // Number of data sets in record
  int data_size;              // Compensation data set size
  char data_desig[9][4];        // Compensation data designator
  char data_descr[33][4];       // Compensation data descriptor
  short num_comp_rec[4];        // Number of compensation records
  short rec_seq_num[4];         // Record sequence number
  int beam_tab_size[4];         // Number of beam table entries
  double beam_tab[256][4];       // Elevation gain beam profile
  char beam_type[17][4];        // Beam type
  double look_angle[4];           // Look angle of beam table centre
  double beam_tab_inc[4];         // Increment between beam table entries
};

// Scene Header Record - ALOS
struct scene_header_rec {
  char product_id[17];        // Product ID
  char uncorr_sc_id[17];      // Uncorrected scene ID
  double sc_lat;              // Level 1A and 1B1 scene latiude
  double sc_lon;              // Level 1A and 1B1 scene longitude
  double sc_line;             // Line number of level 1A and 1B1 scene center
  double sc_sample;           // Sample number of level 1A and 1B1 scene center
  char sc_time[33];           // Scene center time
  int time_off;               // Time offset from nominal RSP center
  char sc_shift[17];          // RSP ID
  int orbit_cycle;            // Orbits per cycle
  char sc_id[17];             // Level 1B2 scene ID
  double sc_lat2;             // Level 1B2 scene center latitude
  double sc_lon2;             // Level 1b2 scene center longitude
  double sc_line2;            // Line number for level 1B2 scene center
  double sc_sample2;          // Sample number for level 1B2 scene center
  char orient_angle[17];      // Orientation angle
  char inc_angle[17];         // Incidence angle
  char mission_id[17];        // Mission ID
  char sensor_id[17];         // Sensor ID
  int orbit;                  // Calculated orbit number
  char orbit_dir[17];         // Orbit direction
  char off_nadir_angle[17];   // Off-nadir mirror pointing angle
  char blank2[13];
  char acq_date[9];           // Acquisition date
  char center_loc[18];        // Latitude and longitude of scene center
  char blank3[18];
  char sensor_type[11];       // Type of sensor and spectrum identification
  char sun_angle[15];         // Sun angle at product scene center
  char proc_code[13];         // Processing code
  char project[13];           // Identification of component agent and project
  char work_scene_id[17];     // Scene ID of work order
  char blank4[907];
  int no_bands;               // Number of effective bands in image
  int samples;                // Number of pixels per line in image
  int lines;                  // Number of scene lines in image
  char blank5[33];
  int radio_res;              // Radiometric resolution
  char blank6[17];
  char level_opt[17];         // Level 1B2 option
  char resample[17];          // Resampling method
  char map_proj[17];          // Map projection
  char corr_level[17];        // Correction level
  int proj_recs;              // Number of map projection ancillary records
  int radio_recs;             // Number of radiometric ancillary records
  char blank7[33];
  int eff_bands;              // Effective band (CCD)
  char img_format[17];        // Image format
  double lat_ul;               // Latitude of scene left upper corner
  double lon_ul;               // Longitude of scene left upper corner
  double lat_ur;               // Latitude of scene right upper corner
  double lon_ur;               // Longitude of scene right upper corner
  double lat_ll;               // Latitude of scene left lower corner
  double lon_ll;               // Longitude of scene left lower corner
  double lat_lr;               // Latitude of scene right lower corner
  double lon_lr;               // Longitude of scene right lower corner
  char time_sys_status[3];    // Time system status
  char abs_nav_status[3];     // Absolute navigation status
  char att_det_flag[3];       // Attitude determination flag
  char acc_orbit[3];          // Accuracy of used orbit data
  char acc_att[3];            // Accuracy of used attitude data
  char yaw_flag[3];           // Yaw steering flag
};

// Map Projection Record - ALOS style
struct alos_map_proj_rec {
  int sample_count;           // Number of nominal pixels per line (1A, 1B1)
  int line_count;             // Number of nominal lines per scene (1A, 1B1)
  double x_pixel_size;        // Nominal inter-pixel distance at scene center
  double y_pixel_size;        // Nominal inter-line distance at scene center
  double image_skew;          // Image skew (milliradian) at scene center
  short hem;                  // Hemisphere - 0 is North, 1 is South
  int utm_zone;               // UTM zone number (1 to 60)
  char blank[33];
  double sc_cen_northing;     // Scene center position (northing - km)
  double sc_cen_easting;      // Scene center position (easting - km)
  char blank2[33];
  double angle_true_north;    // Angle map projection and true north
  char blank3[113];
  double lat_map_origin;      // Latitude of map projection origin (1B2)
  double lon_map_origin;      // Longitude of map projection origin (1B2)
  double ref_lat;             // Reference latitude (1B2)
  double ref_lon;             // Reference longitude (1B2)
  char blank4[33];
  double sc_center_x;         // X coordinates of the scene center (1B2)
  double sc_center_y;         // Y coordinates of the scene center (1B2)
  char blank5[33];
  double angle_true_north2;   // Angle map projection and true north
  double sample_count2;       // Number of nominal pixels per line (1B2)
  double line_count2;         // Number of nominal lines per scene (1B2)
  double x_pixel_size2;       // Nominal inter-pixel distance at scene center (1B2)
  double y_pixel_size2;       // Nominal inter-pixel distance at scene center (1B2)
  char blank6[49];
  double angle_true_north3;   // Angle map projection and true north
  double orbit_inc;           // Nominal satellite orbit inclination (degree)
  double lon_asc_node;        // Longitude of nominal ascending node (radian)
  double sat_height;          // Nominal satellite altitude (km)
  double gr_speed;            // Nominal ground speed (km/s)
  double head_angle;          // Satellite heading angle (radian)
  char blank7[17];
  double swath_angle;         // Swath angle (nominal - degree)
  double scan_rate;           // Nominal scan rate (scan/s)
  char ref_ellipsoid[17];     // Name of reference ellipsoid
  double ref_major_axis;      // Semimajor axis of reference ellipsoid (m)
  double ref_minor_axis;      // Semiminor axis of reference ellipsoid (m)
  char geod_coord_name[17];   // Geodetic coordinates name
  char blank8[129];
  double phi[10];             // latitude transformation coefficients
  double lambda[10];          // longitude transformaiton coefficients
  double i[10];               // pixel transformation coefficients
  double j[10];               // line transformation coefficients
  int a,b,c,d,e,f;            // map coefficients
  int phi_ccd[10][8];         // latitude transformation coefficients (CCDs)
  int lambda_ccd[10][8];      // longitude transformaiton coefficients (CCDs)
  int i_ccd[10][8];           // pixel transformation coefficients (CCDs)
  int j_ccd[10][8];           // line transformation coefficients (CCDs)
};

/*Prototypes for converting character buffers to records, and back again.*/
typedef enum {toASCII,fromASCII} codingDir;
void   Code_FDR_common(unsigned char *bf, struct FDR* q,codingDir dir);
void   Code_FDR(unsigned char *bf, struct FDR* q,codingDir dir);
void   Code_IOF(unsigned char *bf, struct IOF_VFDR* q,codingDir dir);
void   Code_MPDR(unsigned char *bf, struct VMPDREC* q,codingDir dir);
void   Code_DSSR(unsigned char *bf,struct dataset_sum_rec *q,int era, codingDir dir);
void   Code_PPDR (unsigned char *bf, struct pos_data_rec* q, codingDir dir);
void   Code_ATDR(unsigned char *bf, struct att_data_rec *q, codingDir dir);
int    Code_ATVR(unsigned char *bf, struct att_vect_rec *q, codingDir dir);
void   Code_RADDR(unsigned char *bf, struct VRADDR* q,codingDir dir);
void   Code_RSI_RADDR(unsigned char *bf, struct RSI_VRADDR* q,codingDir dir);
void   Code_DQS(unsigned char* bf,struct qual_sum_rec* q,int era,codingDir dir);
void   Code_DHR(unsigned char* bf, struct data_hist_rec* q, codingDir dir);
void   Code_DH(unsigned char *bf, struct hist_dset* q, codingDir dir);
void   Code_RSR(unsigned char *bf, struct rng_spec_rec *q, codingDir dir);
void   Code_ASF_FACDR(unsigned char *bf, struct VFDRECV *q, int era, codingDir dir);
void   Code_ESA_FACDR(unsigned char *bf, struct ESA_FACDR *q, codingDir dir);
void   Code_PPR(unsigned char *bf, struct PPREC *q, codingDir dir);
void   Code_RCDR(unsigned char *bf, struct radio_comp_data_rec *q, codingDir dir);
void   Code_SHR(unsigned char *bf, struct scene_header_rec *q, codingDir dir);
void   Code_AMPR(unsigned char *bf, struct alos_map_proj_rec *q, codingDir dir);

void   Code_VDR(unsigned char *bf, struct VDREC *q, codingDir dir);
void   Code_LFPR(unsigned char *bf, struct FPREC *q, codingDir dir);
void   Code_DFPR(unsigned char *bf, struct FPREC *q, codingDir dir);
void   Code_TR(unsigned char *bf, struct TREC *q, codingDir dir);

void   Code_NVDR(unsigned char *bf, struct VDREC *q, codingDir dir);


int set_era(const char *inbasename,char *outldrname,int operation);

/*Easier-to-use prototypes for fetching a single record from a CEOS file.*/
int get_atdr(const char *filename,struct att_data_rec *rec);
int get_dhr(const char *filename,struct data_hist_rec *rec);
int get_sdhr(const char *filename,struct data_hist_rec *rec);
int get_dqsr(const char *filename,struct qual_sum_rec *rec);
int get_dssr(const char *filename,struct dataset_sum_rec *rec);
int get_asf_facdr(const char *filename,struct VFDRECV *rec);
int get_esa_facdr(const char *filename,struct ESA_FACDR *rec);
int get_mpdr(const char *filename,struct VMPDREC *rec);
int get_ppdr(const char *filename,struct pos_data_rec *rec);
int get_raddr(const char *filename,struct VRADDR *rec);
int get_rsi_raddr(const char *filename, struct RSI_VRADDR *rec);
int get_rsr(const char *filename,struct rng_spec_rec *rec);
int get_ifiledr(const char *filename,struct IOF_VFDR *vfdr);
int get_fdr(const char *filename,struct FDR *rec);
int get_ppr(const char *filename,struct PPREC *rec);
int get_rcdr(const char *filename,struct radio_comp_data_rec *rcdr);
int get_shr(const char *filename, struct scene_header_rec *shr);
int get_ampr(const char *filename, struct alos_map_proj_rec *ampr);

int get_vdr(char *filename,struct VDREC *rec);
int get_lfpr(char *filename,struct FPREC *rec);
int get_dfpr(char *filename,struct FPREC *rec);
int get_tr(char *filename,struct TREC *rec);

int get_nvdr(char *filename,struct VDREC *rec);

#endif /* end of ceos.h */
