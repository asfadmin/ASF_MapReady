/********************************************************************
NAME:               CEOS.H

PURPOSE:  Include file defining structure for CEOS file header/trailer/
	  data record contents.

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           5/92   R. Guritz (ASF)
  1.1           9/93   T. Logan (ASF)   Added Imagery option file
					file descriptor record
  1.2		10/93  T. Logan (ASF)   Added Radiometric Data Record
  1.3		02/94  M. Shindle <ASF> Added Map Projection Data Record
  2.0		5/96   T. Logan (ASF)   Added all the rest of the record types
					Modified existing value structures to 
					include string values
  3.0		9/96   T. Logan (ASF)   Added all fields for post Radarsat CEOS
					structures
  3.1		11/97  T. Logan (ASF)   Made all records consistent with hdrs.
  
*********************************************************************/
#include "ceos.h"

/* appears at the beginning of each Radarsat Era Data Record */
struct HEADER default_header = {
/*  int           recnum */ 		" ",
/*  unsigned char rectyp[4] */ 		" ",
/*  int           recsiz */ 		" " 
};

struct RHEADER default_rheader = {
/*  int           line_num */ 		-99,
/*                rec_num */ 		-99,
/*                n_left_pixel */ 	-99,
/*                n_data_pixel */ 	-99,
/*                n_right_pixel */ 	-99,
/*                sensor_updf */	 0,
/*                acq_year */ 		-99,
/*                acq_day */ 		-99,
/*                acq_msec */ 		-99,
/*  short int     sar_cib */ 		1,
/*                sar_chan_code */ 	2,
/*                tran_polar */ 	0,
/*                recv_polar */ 	0,
/*  float         prf_bin */ 		-99,
/*                spare_1 */ 		0,
/*                sr_first */ 		-99,
/*                sr_mid */ 		-99,
/*                sr_last */ 		-99,
/*                fdc_first */ 		0,
/*                fdc_mid */ 		0,
/*                fdc_last */ 		0,
/*                ka_first */ 		0,
/*                ka_mid */ 		0,
/*                ka_last */ 		0,
/*                nadir_ang */ 		0,
/*                squint_ang */ 	0,
/*                null_f */ 		0,
/*  int           spare_2_1 */ 		0,
/*                spare_2_2 */ 		0,
/*                spare_2_3 */ 		0,
/*                spare_2_4 */ 		0,
/*                geo_updf */ 		0,
/*                lat_first */ 		0,
/*                lat_mid */ 		0,
/*                lat_last */ 		0,
/*                long_first */ 	0,
/*                long_mid */ 		0,
/*                long_last */ 		0,
/*                north_first */ 	0,
/*                spare_3 */ 		0,
/*                north_last */ 	0,
/*                east_first */ 	0,
/*                spare_4 */ 		0,
/*                east_last */ 		0,
/*                heading */ 		0,
/*  double        spare_5 */ 		0
};

/* Values for the Radiometric Data Record */
struct VRADDR default_raddr = {
/*   short  seqnum */ 		1,          		/* Radiometric Data Record Sequence Number */
/*   short  datfield */ 	1,        		/* Number of radiometric data fields in rec */
/*   int    setsize */ 		4212,      		/* Radiometric data set size in bytes */
/*   char   sarchan[5] */ 	"1",    		/* SAR channel indicator */
/*   char   spare[5] */ 	"",
/*   char   luttype[25]*/	"NOISE VS RANGE", 	/* Look Up Table Designator */
/*   int    nosample */ 	256,     		/* Number of samples in LUT */
/*   char   samptype[17] */ 	"INTENSITY",   		/* Sample Type Designator */
/*   double a[3] */ 		{0.0,0.0,0.0},         	/* Calibration Coefficients */
/*   char   spare2[5] */ 	{0.0,0.0,0.0,0.0,0.0}, 
/*   double noise[256], */  	{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				0.0,0.0}/* noise values */
};

/* Imagery Option File -- Values in File Descriptor Record */
struct IOF_VFDR default_ifdr = {
/*   char   ascii_flag[2] */ 	"A",
/*          spare1[2] */ 	"",
/*          format_doc[12] */ 	"CEOS-SAR-CCT",
/*          format_rev[2] */ 	"B",
/*         design_rev[2] */ 	"B",
/*          software_id[12] */ 	"SAROUT",
/*   int    file_num */ 	2,
/*   char   product_id[16] */ 	"fill_me",
/*          rec_seq_flag[4] */ 	"FSEQ",
/*   int    seq_loc */ 		1,
/*          seq_len */ 		4,
/*   char   rec_code[4] */ 	"FTYP",
/*   int    code_loc */ 	5,
/*          code_len */ 	4,
/*   char   rec_len[4] */ 	"FLGT",
/*   int    rlen_loc */ 	9,
/*          rlen_len */ 	4,
/*   char   spare2[4] */ 	"",
/*          spare3[64]*/ 	"",
/*   int    numofrec */         -99,    /* data records                       */
/*          reclen */           -99,    /* Record Length                      */
/*   char   spare4 */           "",     /* unused                             */
/*   int    bitssamp */         -99,    /* bits per sample                    */
/*          sampdata */         -99,    /* samples per data group             */
/*          bytgroup */         -99,    /* bytes per group                    */
/*   char   justific[5] */      "",     /* Justificat'n & order of samps group*/
/*   int    sarchan */          1,      /* SAR channels in this file          */
/*          linedata */         -99,    /* lines per data set                 */
/*          lbrdrpxl */         0,      /* left border pixels per line        */
/*          datgroup */         -99,    /* Total number of data groups        */
/*          rbrdrpxl */         -99,    /* right border pixels per line       */
/*          topbrdr */          0,      /* top border lines                   */
/*          botbrdr */          0,      /* bottom border lines                */
/*   char   interlv[5] */       "BSQ",  /* Interleave Indicator               */
/*   int    recline */          1,      /* physical records per line          */
/*          mrecline */         1,      /* physical recs per multi-channel line*/
/*          predata */          192,    /* bytes of prefix data per record    */
/*          sardata */          -99,    /* bytes of sar data per record       */
/*          sufdata */          0,      /* bytes of suffix data per record    */
/*   char   repflag[5] */       "    ", /* prefix/suffix repeat flag          */
/*          lin_loc[9] */       "  1354PB",  /* line number locator           */
/*          chn_loc[9] */       "  4952PB",  /* channel number locator        */
/*          time_loc[9] */      "  4554PB",  /* time locator                  */
/*          left_loc[9] */      "  21 4PB",  /* left fill locator             */
/*          right_loc[9] */     "  29 4PB",  /* right fill locator            */
/*          pad_ind[5] */       "    ", /* pad pixel indicator                */
/*          spare6[29] */       "",     /* unused                             */
/*          qual_loc[9] */      "",     /* quality code locator               */
/*          cali_loc[9] */      "",     /* calibration info locator           */
/*          gain_loc[9] */      "",     /* gain value locator                 */
/*          bais_loc[9] */      "",     /* bias value locator                 */
/*          formatid[29] */     "fill me", /* SAR data format identifier      */
/*          formcode[5] */      "UNK",  /* SAR data format type code          */
/*   int    leftfill */         0,      /* Left fill bits per pixel           */
/*          rigtfill */         0,      /* Right fill bits per pixel          */
/*          maxidata */         -99     /* Maximum data range of pixel        */
};

/*Facility Related Data Record.*/

struct VFDRECV default_facdr = {
/*   short  seq_num */ 		-99 ,
/*   char   spare_frd_1[5] */ 	"",
/*   char   dataid[15] */ 	"fill me",	/* Data take id */
/*          imageid[12] */ 	"fill me",  	/* SPS image identifier */
/*          coryear[6] */ 	"UNK",       	/* UTC year of image correlation */
/*          cortime[18] */ 	"fill me",  	/* UTC time of image correlation */
/*          sitename[34] */ 	"fill me", 	/* Name of site covered */
/*          imgyear[6] */ 	"UNK",       	/* UTC year at the image center */
/*          imgtime[18] */ 	"UNK",      	/* UTC time at the image center */
/*   double imgclat */ 		-99.0,  	/* Latitude of the image center (F16.7) degrees */
/*          imgclon */ 		-99.0,  	/* Longitude of the image center (F16.7) degrees */
/*          nearslat */ 	-99.0, 		/* Lat at start of image frame near swath (F16.7) */
/*          nearslon */ 	-99.0,		/* Long at start of image frame near swath (F16.7)*/
/*          nearelat */ 	-99.0, 		/* Lat at end of image frame near swath (F16.7) */
/*          nearelon */ 	-99.0, 		/* Long at end of image frame near swath (F16.7) */
/*          farslat */ 		-99.0,  	/* Lat at start of image frame far swath (F16.7) */
/*          farslon */ 		-99.0,  	/* Long at start of image frame far swath (F16.7) */
/*          farelat */ 		-99.0,  	/* Lat at end of image frame far swath (F16.7) */
/*          farelon */ 		-99.0,  	/* Long at end of image frame far swath (F16.7) */
/*          swazim */ 		-99.0,   	/* Actual swath width (km) azimuth (F16.7)*/
/*          swrange */ 		-99.0,  	/* Actual swath width (km) range (F16.7)*/
/*   int    npixels */ 		-99,    	/* Actual (without filler) pixels per line(I8)*/
/*          nlines */ 		-99,     	/* Actual (without filler) image lines (I8) */
/*          apixels */ 		-99,    	/* Total (with filler) pixels per line (I8) */
/*          alines */ 		-99,     	/* Total (with filler) image lines (I8) */
/*   char   mediaid[8] */ 	"",   		/* Identification label of the media written to */
/*          sblock[18] */ 	"0",   		/* Location on the DCRSI where data begins */
/*          eblock[18] */ 	"0",   		/* Location on the DCRSI where data ends */
/*          platform[18] */ 	"fill me", 	/* Name of platform for sensor data */
/*          sensmode[34] */ 	"fill me", 	/* Sensor and mode of operation */
/*   double prfreq */ 		-99.0,   	/* Pulse repitition frequency (F16.7) */
/*          antlook */ 		-99.0,  	/* SAR antenna look angle (F16.7) */
/*          datarate */ 	105.0, 		/* Data rate (F16.7) */
/*          datawin */ 		-99.0,  	/* Data window position (F16.7) */
/*          rangegd */ 		-99.0,  	/* Range gate delay (F16.7) */
/*          trackang */ 	-99.0, 		/* Track angle to True North (F16.7) */
/*   char   ascdesc[3] */ 	"X",   		/* Flag indicating whether the pass is asc/desc */
/*   double scalt */ 		-99.0,    	/* Altitude of the spacecraft image center (F16.7)*/
/*          scxpos */ 		-99.0,   	/* Spacecraft X-position image center (D22.15) */
/*          scypos */ 		-99.0,   	/* Spacecraft Y-position image center (D22.15) */
/*          sczpos */ 		-99.0,   	/* Spacecraft Z-position image center (D22.15) */
/*          scxvel */ 		-99.0,   	/* Spacecraft X-velocity image center (D22.15) */
/*          scyvel */ 		-99.0,   	/* Spacecraft Y-velocity image center (D22.15) */
/*          sczvel */ 		-99.0,   	/* Spacecraft Z-velocity image center (D22.15) */
/*          scroll */ 		-99.0,   	/* Spacecraft roll image center (E14.6) */
/*          scyaw */ 		-99.0,    	/* Spacecraft yaw image center (E14.6) */
/*          scpitch */ 		-99.0,  	/* Spacecraft pitch image center (E14.6) */
/*   int    qroll */ 		0,     		/* Quality flag for the spacecraft roll (I4) */
/*          qyaw */ 		0,      	/* Quality flag for the spacecraft yaw (I4) */
/*          qpitch */ 		0,    		/* Quality flag for the spacecraft pitch (I4) */
/*   double rroll */ 		0.0,     	/* Spacecraft roll rate image center (E14.6) */
/*          ryaw */ 		0.0,      	/* Spacecraft yaw rate image center (E14.6) */
/*          rpitch */ 		0.0,    	/* Spacecraft pitch rate image center (E14.6) */
/*   int    rqroll */ 		0,    		/* Quality flag for the spacecraft roll rate (I4) */
/*          rqyaw */ 		0,     		/* Quality flag for the spacecraft yaw rate (I4) */
/*          rqpitch */ 		0,   		/* Quality flag for the spacecraft pitch rate (I4) */
/*   double eradnadr */ 	-99.0,  	/* Radius of the earth at nadir (F16.7) */
/*          eradcntr */ 	-99.0,  	/* Radius of the earth at image center (F16.7) */
/*          incedang */ 	-99.0,  	/* Incidence angle center of the image (F16.7) */
/*   char   procvers[9] */ 	"",  		/* Version of the ASP (F7.2) */
/*          imgproct[4] */ 	"SF",  		/* Image processing type identifier */
/*          ephemert[3] */ 	"R",  		/* Type of Ephemeris used identifier */
/*   double nlooksaz */ 	-99.0, 		/* Effective number of looks in azimuth (F16.7) */
/*          nlooksra */ 	-99.0, 		/* Effective number of looks in range (F16.7) */
/*          weightaz */ 	-99.0, 		/* Weighting pedestal height in azimuth (F16.7) */
/*          weightra */ 	-99.0, 		/* Weighting pedestal height in range (F16.7) */
/*   char   normener[5] */ 	"NOT",  	/* Look energy normalization flag */
/*   double indistaz */ 	-99.0, 		/* Known processing induced distortions azimuth */
/*          indistra */ 	-99.0, 		/* Known processing induced distortions range   */
/*          recgain */ 		0.0,  		/* Receiver gain (F16.7) */
/*          swathvel */		-99.0, 		/* Swath velocity (F16.7) */
/*          squintan */ 	-99.0, 		/* Squint angle (F16.7) */
/*          avgterht */ 	0.0,   		/* Ave terrain height above Geoid image center */
/*   char   procgain[5] */ 	"0",   		/* Processor gain */
/*          deskewf[5] */ 	"UNK",  	/* Flag indicating if Doppler Skew was removed */
/*          grndslnt[8] */ 	"UNK",  	/* Ground range / slant range flag */
/*   double sltrngfp */ 	-99.0, 		/* Slant range to the first image pixel (F16.7) */
/*          sltrnglp */ 	-99.0, 		/* Slant range to the last image pixel (F16.7) */
/*   int    strtsamp */ 	1,     		/* Start samp of signal data range line processed */
/*   char   clttrlkf[5] */ 	"YES", 		/* Flag indicating if clutterlock was used */
/*   double dpplrfrq */ 	-99.0, 		/* Doppler frequency at the near range (F16.7)*/
/*          dpplrslp */ 	-99.0, 		/* Doppler frequency slope (F16.7)*/
/*          dpplrqdr */ 	-99.0, 		/* Doppler frequency quadratic term (F16.7)*/
/*   char   autfocsf[5] */ 	"NOT", 		/* Flag indicating whether autofocus was used */
/*   double dpplrrat */ 	-99.0, 		/* Doppler frequency rate near range (F16.7) */
/*          dpratslp */ 	-99.0, 		/* Doppler frequency rate slope (F16.7) */
/*          dpratqdr */ 	-99.0, 		/* Doppler frequency rate quadratic term (F16.7) */
/*          imresaz */ 		-99.0, 		/* Nominal image resolution in azimuth (F16.7) */
/*          imresra */ 		-99.0, 		/* Nominal image resolution in range (F16.7) */
/*          azpixspc */ 	-99.0, 		/* Pixel spacing in azimuth (F16.7) */
/*          rapixspc */ 	-99.0, 		/* Pixel spacing in range (F16.7) */
/*   char   rngcompf[5] */ 	"NOT", 		/* On-board range compression flag */
/*   int    bitssamp */ 	-99,  		/* Bits per sample of the SAR signal data (I4) */
/*   double calestim */ 	0.0, 		/* Calibrator estimate (F16.7) */
/*          biterrrt */ 	-99.0, 		/* Data transfer bit error rate (F16.7) */
/*          sigtonoi */ 	-99.0, 		/* Signal to noise ratio (F16.7) */
/*          estnoifl */ 	-21.0, 		/* Estimated noise floor (F16.7) */
/*          radiores */ 	0.1,   		/* Radiometric resolution (F16.7) */
/*   int    nsatpnts */ 	0.0, 		/* saturated points determined from hist (I8) */
/*   char   inspecf[5] */ 	"UNK", 		/* Flag to indicate if image is within spec */

    /***** Values included in RADARSAT era CEOS structure ******/
/*  double  repl_agc */ 	0.0,          /* chirp replica AGC value   */
/*  double  temp_rx_lna */ 	0.0,          /* temp of rcvr LNA          */
/*  double  temp_rx_sub */ 	0.0,          /* temp of rcvr subsystem    */
/*  double  temp_rx_prot */ 	0.0,          /* temp of rcvr protector    */
/*  double  temp_cal_sys */ 	0.0,          /* temp of calib system      */
/*  double  rx_agc */ 		0.0,          /* rcvr AGC value            */
/*  double  pre_cal1_pow */ 	0.0,          /* pre cal1 avg power        */
/*  double  pre_cal2_pow */ 	0.0,          /* pre cal2 avg power        */
/*  double  post_cal1_pow */ 	0.0,          /* post cal1 avg power       */
/*  double  post_cal2_pow */ 	0.0,          /* post cal2 avg power       */
/*  double  repl_pow */ 	0.0,          /* Replica avg power         */
/*  double  ssar_roll_ang */ 	0.0,          /* est ScanSAR roll angle    */
   /***** Values included in RADARSAT era CEOS structure ******/

/*   char   comment[101] */ 	""         	/* comment field */
};

/* Map Projection Data Record - Value Structure */
struct VMPDREC default_mpdr = {
/*  char  mpdesc[33] */ 	"GEOCODED",    	/* Map projection descriptor */
/*  int   npixels */ 		-99,     	/* number of pixels per line of image */
/*	  nlines */ 		-99,      	/* number of lines in image */
/*  double  nomipd */ 		-99.0,  	/* nominal inter-pixel distance in output (meters) */
/*	  nomild */ 		-99.0,  	/* nominal inter-line distance in output (meters) */
/*	  orient */ 		0.0,    	/* Orientation at output scene center */
/*	  orbitincl */ 		-99.0, 		/* Actual platform orbital inclination (degrees) */
/*	  ascnode */ 		-99.0,   	/* Actual ascending node (degrees) */
/*	  distplat */ 		-99.0,  	/* Distance of platform from geocenter (meters) */
/*	  altplat */ 		-99.0,   	/* Altitude of platform rel. to ellipsoid (m) */
/*	  velnadir */ 		-99.0,  	/* Actual ground speed at nadir (m/s) */
/*	  plathead */ 		-99.0,  	/* platform heading (degrees) */
/*  char    refelip[33] */ 	"GEM06",   	/* Name of reference ellipsoid */
/*  double  remajor */ 		6378.144,      	/* Semimajor axis of ref. ellipsoid (m) */
/*	  reminor */ 		6356.7549,     	/* Semiminor axis of ref. ellipsoid (m) */
/*	  datshiftx */		0.0, 		/* Datum shift parameter ref. to Greenwich dx (m) */
/*        datshifty */		0.0, 		/* datum shift perpendicular to Greenwich dy (m) */
/*	  datshiftz */		0.0, 		/* datum shift direction of rotation axis dz (m) */
/*	  datshift1 */		-9999.990, 	/* 1st additional datum shift, rotation angle */
/*	  datshift2 */		-9999.990, 	/* 2nd additional datum shift, rotation angle */
/*	  datshift3 */		-9999.990, 	/* 3rd additional datum shift, rotation angle */
/*	  rescale */ 		0.0,  		/* Scale factor of referenced ellipsoid */
/*  char  mpdesig[33] */ 	"fill me", 	/* Alphanumeric description of map proj */
/*	  utmdesc[33] */ 	"UNIVERSAL TRANSVERSE MERCATOR", 	/* UTM descriptor */
/*	  utmzone[5] */ 	"0000",		/* Signature of UTM Zone */
/*  double  utmeast */ 		0.0,       	/* Map origin - false easting */
/*	  utmnorth */ 		0.0,      	/* Map origin - false north */
/*	  utmlong */ 		0.0,       	/* Center of projection longitude (deg) */
/*	  utmlat */ 		0.0,        	/* Center of projection latitude (deg) */
/*	  utmpara1 */ 		0.0,      	/* 1st standard parallel (deg) */
/*	  utmpara2 */ 		0.0,      	/* 2nd standard parallel (deg) */
/*	  utmscale */ 		0.0,      	/* UTM scale factor */
/*  char    upsdesc[33] */ 	"",   		/* UPS descriptor */
/*  double  upslong */ 		0.0,       	/* Center of projection longitude (deg) */
/*	  upslat */ 		0.0,        	/* Center of projection latitude (deg) */
/*	  upsscale */ 		0.0,      	/* UPS scale factor */
/*  char    nspdesc[33] */ 	"LAMBERT CONFORMAL",	/* NSP projection description */
/*  double  nspeast */ 		0.0,       	/* Map origin - false east */
/*	  nspnorth */ 		0.0,      	/* Map origin - false north */
/*	  nsplong */ 		0.0,       	/* Center of projection longitude (deg) */
/*	  nsplat */ 		0.0,        	/* Center of projection latitude (deg) */
/*          nsppara1 */ 	0.0,      	/* Standard parallels */
/*	  nsppara2 */ 		0.0,      	/* Standard parallels */
/*	  nsppara3 */ 		-9999.99,      	/* Standard parallels */
/*	  nsppara4 */ 		-9999.99,      	/* Standard parallels */
/*	  nspcm1 */		-9999.99,      	/* Central meridian */
/*	  nspcm2 */ 		-9999.99,      	/* Central meridian */
/*	  nspcm3 */ 		-9999.99,      	/* Central meridian */
/*  double  tlcnorth */ 	-99.0,    	/* Top left corner north (m) */
/*	  tlceast */ 		-99.0,     	/* Top left corner east (m) */
/*	  trcnorth */ 		-99.0,    	/* Top right corner north (m) */
/*	  trceast */ 		-99.0,     	/* Top right corner east (m) */
/*	  brcnorth */ 		-99.0,    	/* Bottom right corner north (m) */
/*	  brceast */ 		-99.0,     	/* Bottom right corner east (m) */
/*	  blcnorth */ 		-99.0,    	/* Bottom left corner north (m) */
/*	  blceast */ 		-99.0,     	/* Bottom left corner east (m) */
/*	  tlclat */ 		-99.0,      	/* Top left corner latitude (deg) */
/*	  tlclong */ 		-99.0,     	/* Top left corner longitude (deg) */
/*        trclat */ 		-99.0,      	/* Top right corner lat (deg) */
/*	  trclong */ 		-99.0,     	/* Top right corner int (deg) */
/*	  brclat */ 		-99.0,      	/* Bottom right corner latitude (deg) */
/*	  brclong */ 		-99.0,     	/* Bottom right corner longitude (deg) */
/*	  blclat */ 		-99.0,      	/* Bottom left corner latitude (deg) */
/*	  blclong */ 		-99.0,     	/* Bottom left corner longitude (deg) */
/*	  tlcheight */ 		0.0,     	/* Top left corner terrain height (m) */
/*	  trcheight */ 		0.0,     	/* Top right corner terrain height (m) */
/*	  brcheight */ 		0.0,     	/* Bottom right corner terrain height (m) */
/*	  blcheight */ 		0.0,     	/* Bottom left corner terrain height (m) */
/*	  a11 */ 		0.0,           	/* 8 coeff. to convert line (L) and pixel (p) */
/*	  a12 */ 		0.0,           	/* position to the map projection frame of    */
/*	  a13 */ 		0.0,           	/* reference, say (E,N) where:                */
/*	  a14 */ 		0.0,           	/*                                            */
/*	  a21 */ 		0.0,           	/*  E  A11+A12+A13+A14TL                      */
/*	  a22 */ 		0.0,           	/*  N  A21+A22+A23+A24+nP                     */
/*	  a23 */ 		0.0,           	/*                                            */
/*	  a24 */ 		0.0,           	/*                                            */
/*	  b11 */ 		0.0,           	/* 8 coeff. to convert the map projection     */
/*	  b12 */ 		0.0,           	/* (E,N) to line (L) and pixel (P) position.  */
/*	  b13 */ 		0.0,           	/*                                            */
/*	  b14 */ 		0.0,           	/* L  B11+B12+B13+B14TETN                     */
/*	  b21 */ 		0.0,           	/* P  B21+B22+B23+B24TN                       */
/*	  b22 */ 		0.0,           	/*                                            */
/*	  b23 */ 		0.0,           	/*                                            */
/*	  b24 */ 		0.0           	/*                                            */
};



/****************************
*                           *
*  SAR Leader File:         *
*  Data Set Summary Record  *
*                           *
****************************/

struct dataset_sum_rec default_dssr = {
/*    short  seq_num */     	1,         	/* Data Set Summary: record sequence number */
/*    short  sar_chan */        1,     		/* SAR channel indicator */
/*    char   product_id[17] */  "",     	/* site ID / product id */
/*    char   scene_des[33] */   "",     	/* site name */
/*    char   inp_sctim[33] */   "fill me",     	/* image center GMT: YYYYMMDDhhmmssttt */
/*    char   asc_des[17] */     "",     	/* Ascending/descending */
/*    double pro_lat */         -99.0,     	/* latitude at scene center */
/*    double pro_long */        -99.0,     	/* longitude at scene center */
/*    double pro_head */        -99.0,     	/* Processed scene center heading */
/*    char   ellip_des[17] */   "GEM06",     	/* ellipsoid designator */
/*    double ellip_maj */       6378.144,     	/* ellipsoid semimajor axis (km) */
/*    double ellip_min */       6356.7549,     	/* ellipsoid semiminor axis (km) */
/*    double earth_mass */      398600.5,     	/* Earth's mass */
/*    double grav_const */      9.8000002,	/* Gravitational constant */
/*    double ellip_j[3] */      {0.0010826,-0.0000025,-1610000.0},     /* Ellipsoid J2-4 parameters */

    /***** Value included in RADARSAT era CEOS structure ******/
/*    char  spare1[17] */       "",     	/* spare */
    /***** Value included in RADARSAT era CEOS structure ******/

/*    double terrain_h */ 	0.0,    	/* average terrain height */
/*    double sc_lin */          -99.0,     	/* image center line number (azimuth) */
/*    double sc_pix */          -99.0,     	/* image center pixel number (range) */
/*    double scene_len */       -99.0,     	/* image length in km */
/*    double scene_wid */       -99.0,     	/* image width in km */
/*    char  spare2[17] */       "",     	/* spare */
/*    short nchn */             1,     		/* number of SAR channels */
/*    char  spare3[5] */        "",     	/* spare */
/*    char  mission_id[17] */   "",     	/* mission id */
/*    char  sensor_id[33] */    "",     	/* sensor id: AAAAAA-BB-CCDD-EEFF */
/*    char  revolution[9] */    "",     	/* orbit number */
/*    double plat_lat */        -99.0,     	/* spacecraft latitude at nadir */
/*    double plat_long */       -99.0,     	/* spacecraft longitude at nadir */
/*    double plat_head_scene */ -99.0,     	/* sensor platform heading (degrees) */
/*    double clock_ang */       -99.0,     	/* sensor clock angle rel to flight dir */
/*    double incident_ang */    -99.0,     	/* incidence angle at image center */
/*    double frequency */       -99.0,     	/* radar frequency (GHz) */
/*    double wave_length */     -99.0,     	/* radar wavelength (m) */
/*    char  motion_comp[3] */   "00",     	/* motion compensation indicator */
/*    char  pulse_code[17] */   "LINEAR FM CHIRPS",     /* range pulse code specifier */
/*    double ampl_coef[5] */    {0.0,0.0,0.0,0.0,0.0},     /* range chirp coefficients */
/*    double phas_coef[5] */    {0.0,0.0,0.0,0.0,0.0},     /* range phase coefficients */
/*    int  chirp_ext_ind */     0,     		/* chirp extraction index */
/*    char  spare4[9] */        "",     	/* spare */
/*    double rng_samp_rate */   -99.0,     	/* range complex sampling rate */
/*    double rng_gate */        -99.0,     	/* range gate at early edge */
/*    double rng_length */      -99.0,     	/* range pulse length */
/*    char  baseband_f[5] */    "YES",     	/* base band conversion flag */
/*    char  rngcmp_f[5] */      "NOT",     	/* range compressed flag */
/*    double gn_polar */        0.0,     	/* receiver gain for like pol */
/*    double gn_cross */        0.0,     	/* receiver gain for cross pol */
/*    int  chn_bits */          0,     		/* quantization bits per channel */
/*    char  quant_desc[13] */   "UNIFORM I,Q",  /* quantizer description */
/*    double i_bias */          -99.0,     	/* I channel DC bias */
/*    double q_bias */          -99.0,     	/* Q channel DC bias */
/*    double iq_ratio */        -99.0,     	/* I/Q channel ratio */
/*    double spare_dss_7 */     0.0,     	/* spare */
/*    double spare_dss_8 */     0.0,     	/* spare */
/*    double ele_sight */       -99.0,     	/* electronic boresight */
/*    double mech_sight */      0.0,	        /* mechanical boresight */
/*    char   echo_track[5] */   "OFF",          /* echo tracker flag */
/*    double prf */             -99.0,          /* nominal PRF */
/*    double elev_beam */       -99.0,          /* antenna elevation 3dB beam width */
/*    double azi_beam */        -99.0,          /* antenna azimuth 3dB beam width */
/*    char   sat_bintim[17] */  "",            	/* Satellite binary time */
/*    char   sat_clktim[33] */  "",          	/* Satellite clock time */
/*    int   sat_clkinc */       0,         	/* Satellite clock increment */
/*    char   spare5[9] */       "",     	/* spare */
/*    char   fac_id[17] */      "ASF-STEP",     /* processing facility */
/*    char   sys_id[9] */       "",     	/* processing system */
/*    char   ver_id[9] */       "",     	/* processor version */
/*    char   fac_code[17] */    "",     	/* facility process code */
/*    char   lev_code[17] */    "",     	/* product code */
/*    char   product_type[33]*/ "",     	/* product type */
/*    char   algor_id[33] */    "RANGE DOPPLER", /* processing algorithm */
/*    double n_azilok */        -99.0,     	/* number of looks in azimuth */
/*    double n_rnglok */        -99.0,     	/* number of looks in range */
/*    double bnd_azilok */      -99.0,     	/* bandwidth per look in azimuth */
/*    double bnd_rnglok */      -99.0,     	/* bandwidth per look in range */
/*    double bnd_azi */         -99.0,     	/* processor bandwidth (azimuth) */
/*    double bnd_rng */         -99.0,     	/* processor bandwidth (range) */
/*    char   azi_weight[33] */  "",     	/* weighting function (azimuth) */
/*    char   rng_weight[33] */  "",     	/* weighting function (range) */
/*    char   data_inpsrc[17] */ "",     	/* data input source: HDDC id */
/*    double rng_res */         -99.0,     	/* nominal resolution (range) */
/*    double azi_res */         -99.0,     	/* nominal resolution (azimuth) */
/*    double radi_stretch[2] */ {0.0,1.0},     	/* radiometric stretch terms (bias, gain) */
/*    double alt_dopcen[3] */   {0.0,0.0,0.0},  /* along track Doppler freq terms */
/*    char   spare6[17] */      "",     	/* spare */
/*    double crt_dopcen[3] */   {0.0,0.0,0.0},  /* cross track Doppler freq terms */
/*    char   time_dir_pix[9] */ "",     	/* time direction (range) */
/*    char   time_dir_lin[9] */ "",     	/* time direction (azimuth) */
/*    double alt_rate[3] */     {0.0,0.0,0.0},  /* Along track Doppler rate terms */
/*    char   spare7[17] */      "",     	/* spare */
/*    double crt_rate[3] */     {0.0,0.0,0.0},  /* Cross track Doppler rate terms */
/*    char   spare8[17] */      "",     	/* spare */
/*    char   line_cont[9] */    "",     	/* line content indicator */
/*    char   clutterlock_flg[5] */"YES",   	/* clutter lock flag */
/*    char   auto_focus[5] */   "NOT",     	/* autofocussing flag */
/*    double line_spacing */    -99.0,     	/* line spacing (m) */
/*    double pixel_spacing */   -99.0,     	/* pixel spacing (m) */
/*    char   rngcmp_desg[17] */ "SYNTHETIC CHIRP",     	/* range compression designator */

    /****** Values included in PRE RADARSAT era data files *****/
/*    char   spare9[273] */     "",     	/* spare */
/*    int    annot_pts */       -99,     	/* number of annotation points */
/*    int    spare10[9] */      {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0},     /* spare */
/*    int    annot_line[64] */  {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
			 	 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0},     /* Line number of annotation start */
/*    int    annot_pixel[64] */ {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
			 	 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0},    /* Pixel number of annotation start */
/*  char   annot_text[64][37]*/ {"","","","","","","","","","","","","","","","","","",
				 "","","","","","","","","","","","","","","","","","",
				 "","","","","","","","","","","","","","","","","","",
				 "","","","","","","","","",""},   /* Annotation text */
/*    char   spare11[26] */     "",    		/* spare */
    /****** Values included in PRE RADARSAT era data files *****/

    /****** Values included in RADARSAT era data files *****/
/*    char   spare_dss_14[17] */ "",    	/* spare */
/*    char   spare_dss_15[17] */ "",    	/* spare */
/*    int    no_beams */         1,    		/* number of beams */
/*    char   beam1[5] */         "",    	/* beam 1 identifier */
/*    char   beam2[5] */         "",    	/* beam 2 identifier */
/*    char   beam3[5] */         "",    	/* beam 3 identifier */
/*    char   beam4[5] */         "",    	/* beam 4 identifier */
/*    float  prf1 */             0.0,    	/* PRF of beam 1 Hz */
/*    float  prf2 */             0.0,    	/* PRF of beam 2 Hz */
/*    float  prf3 */             0.0,   	/* PRF of beam 3 Hz */
/*    float  prf4 */             0.0,    	/* PRF of beam 4 Hz */
/*    float  rng_gate1 */        0.0,    	/* range gate of beam 1 (usec) */
/*    float  rng_gate2 */        0.0,    	/* range gate of beam 2 (usec) */
/*    float  rng_gate3 */        0.0,    	/* range gate of beam 3 (usec) */
/*    float  rng_gate4 */        0.0,    	/* range gate of beam 4 (usec) */
/*    int    tot_pls_burst */    0,    	/* total pulses per burst */
/*    int    val_pls_burst */    0,    	/* valid pulses per burst */
/*    int    az_ovlp_nxt_img */  0,    	/* Range lines overlap in azimuth with next */
/*    int    rg_off_nxt_img */   0,    	/* pixel of offset in range with next */
/*    char   cal_params_file[33] */     "",  	/* calibration parameter file used */
/*    char   scan_results_file[33] */   "", 	/* name of scan results file used */
/*    char   scanner_version[17] */     "",  	/* version of the scanner used     */
/*    char   decode_version[17] */      "",   	/* version of the decode used      */
/*    char   spare_dss_16[2130] */      "" 	/* spare */
    /****** Values included in RADARSAT era data files *****/
};


/****************************
*                           *
*  SAR Leader File:         *
*  Platform Position Rec    *
*                           *
****************************/
struct pos_data_rec default_ppdr = {
/*    short seq_num */        	-9,      	/* Platform Position: record sequence # */
/*    char  orbit_ele_desg[33]*/"ORBITAL KEPLERIAN ELEMENTS",   /* Orbital elements designator          */
/*    double orbit_ele[6] */    {-99.0,-99.0,-99.0,-99.0,-99.0,-99.0},    /* orbital elements                     */
/*    short  ndata */    	3,           	/* number of data sets                  */
/*    short  year */            -99,    	/* year of first data point             */
/*    short  month */           -99,    	/* month of first data point            */
/*    short  day */             -99,    	/* day of first data point              */
/*    short  gmt_day */         -99,    	/* day in year of first data point      */
/*    double gmt_sec */         -99.0,    	/* seconds in day of first data point   */
/*    double data_int */        -99.0,    	/* time interval between data points(s) */
/*    char  ref_coord[65] */    "GEOCENTRIC EQUATORIAL INERTIAL",    /* reference coordinate system          */
/*    double hr_angle */        -99.0,    	/* GMT hour angle (degrees)             */
/*    double alt_poserr */      60.0,    	/* Aint track position error           */
/*    double crt_poserr */      15.0,    	/* Cross track position error           */
/*    double rad_poserr */      25.0,    	/* radial position error                */
/*    double alt_velerr */      0.027,   	/* Aint track velocity error           */
/*    double crt_velerr */      0.015,    	/* Cross track velocity error           */
/*    double rad_velerr */      0.04,    	/* Radial velocity error                */
/*    double pos_vec[64][6] */  {{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,0.0,0.0},
				{0.0,0.0,0.0,0.0,0.0,0.0}},
      				/* Data point position/velocity         */
                                /* 0-2 position,  3-5  velocity         */
/*    char spare_ppr_1[243];*/ ""      /* spare                                */
}; 

/******************************************
*                                         *
*  SAR Leader File: Attitude Data Record  *
*                                         *
******************************************/

struct  att_vect_rec default_atvr = {
/*    short gmt_day */  	0,   /* day in the year (GMT)        */
/*    int  gmt_msec */          0,   /* millisecond of the day (GMT) */
/*    short pitch_flag */   	0,   /* pitch data quality flag      */
/*    short roll_flag */     	0,   /* roll data quality flag       */
/*    short yaw_flag */       	0,   /* yaw data quality flag        */
/*    double pitch */         	0.0, /* pitch (degrees)              */
/*    double roll */          	0.0, /* roll (degrees)               */
/*    double yaw */            	0.0, /* yaw (degrees)                */
/*    short pitch_rate_flag */ 	0,   /* pitch rate data quality flag */
/*    short roll_rate_flag */ 	0,   /* roll rate data quality flag  */
/*    short yaw_rate_flag */  	0,   /* yaw rate data quality flag   */
/*    double pitch_rate */    	0.0, /* pitch rate (degrees/sec)     */
/*    double roll_rate */     	0.0, /* roll rate (degrees/sec)      */
/*    double yaw_rate */      	0.0  /* yaw_rate (degrees/sec)       */
/*    struct att_vect_rec *next */   /* pointer to next data set     */
};

struct  att_data_rec default_atdr = {
/*    short npoint */           0    /* number of attitude data sets */
/*    struct att_vect_rec *data */  /* pointer to list of data sets  */
/*    char spare_adr_1[641] */      /* spare                         */
};

/*******************************************
*                                          *
*  SAR Leader File:  Data Quality Summary  *
*                                          *
*******************************************/

struct qual_sum_rec default_dqsr = {
/*    short seq_num */          0,       /* dqs record sequence number       */
/*    char  chan_ind[5] */      "",      /* sar channel indicator            */
/*    char  cali_date[7] */     "",      /* calibration date                 */
/*    short nchn */             0,       /* number of channels               */
/*    double islr */            0.0,      /* integrated side lobe ratio       */
/*    double pslr */            0.0,      /* peak side lobe ratio             */
/*    double azi_ambig */       0.0,      /* azimuth ambiguity                */
/*    double rng_ambig */       0.0,      /* range ambiguity  		    */
/*    double snr */             0.0,      /* signal-to-noise ratio estimate   */
/*    double ber */             0.0,      /* nominal bit error rate           */
/*    double rng_res */         0.0,      /* nominal slant range resolution   */
/*    double azi_res */         0.0,      /* nominal azimuth resolution       */
/*    double rad_res */         0.0,      /* nominal radiometric resolution   */
/*    double dyn_rng */         0.0,      /* instantaneous dynamic range      */
/*    double abs_rad_unc_db */ 	0.0,     /* nominal abs. radiometric uncertainty, db  */
/*    double abs_rad_unc_deg */	0.0,     /* nominal abs. radiometric uncertainty, deg */
/*    double rel_rad_unc[2][16] */ {{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
			   	      0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0},  
				     {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
			   	      0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}}, 
					    /* rel short term radio calibr uncertainty   */
                                	    /* 16 pairs of values in db and deg */
/*    double alt_locerr */      0.0,      /* location error along track       */
/*    double crt_locerr */      0.0,      /* location error cross track       */
/*    double alt_scale */     	0.0,      /* along track scale error          */
/*    double crt_scale */     	0.0,      /* cross track scale error          */
/*    double dis_skew */      	0.0,      /* geometric skew error             */
/*    double ori_err */       	0.0,      /* image orientation error          */
/*    double misreg[2][16] */       /* misregistration error: 16 pairs  */
 				{{0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
			   	      0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0},  
				     {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
			   	      0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}}, 
				/* for along track and cross track  */

    /****** Value included in PRE RADARSAT era CEOS structure ******/
/*    char  spare2[279] */      "",    /* spare */
    /****** Value included in PRE RADARSAT era CEOS structure ******/

    /****** Value included in RADARSAT era CEOS structure **********/
/*    double nesz */           	0.0,     /* nominal noise equiv sigma zero  */
/*    double enl */            	0.0,     /* Nominal equiv no of looks       */
/*    char  tb_update[9] */    	"",     /* default param table update date */
/*    char  cal_status[17] */	"",	/* Calibration status of data      */
/*    char  spare3[23] */     	"",      /* spare 			   */
/*    char  cal_comment[201] */ "",    /* Calibration comment field       */
    /****** Value included in RADARSAT era CEOS structure **********/
}; 


/****************************
*                           *
*  SAR Leader File:         *
* Data Histograms Record    *
*                           *
****************************/

struct hist_dset default_hd = {
/*    char  hist_desc[33] */  "",     /* histogram descriptor 		*/
/*    short nrec */           1,     /* required records 		*/
/*    short tab_seq */        1,     /* table sequence number 	*/
/*    int  nbin */            0,    /* table size, bytes 		*/
/*    int  ns_lin */          0,    /* number of pixels in line 	*/
/*    int  ns_pix */          0,    /* number of lines in image 	*/
/*    int  ngrp_lin */        0,    /* pixels/group, cross track 	*/
/*    int  ngrp_pix */        0,    /* pixels/group, along track 	*/
/*    int  nsamp_lin */       0,    /* number of groups, cross track	*/
/*    int  nsamp_pix */       0,    /* number of groups, along track */
/*    double min_smp */       -99.0,     /* minimum pixel value 		*/
/*    double max_smp */       -99.0,     /* maximum pixel value 		*/
/*    double mean_smp */      -99.0,     /* mean sample value 		*/
/*    double std_smp */       -99.0,     /* std deviation of sample value */
/*    double smp_inc */       -99.0,     /* sample value increment 	*/
/*    double min_hist */      -99.0,     /* minimum table value 		*/
/*    double max_hist */      -99.0,     /* maximum table value 		*/
/*    double mean_hist */     -99.0,     /* histogram mean value 		*/
/*    double std_hist */      -99.0,     /* histogram standard deviation  */
/*    int  nhist */           0    /* histogram table size 		*/
/*    int  *data_values_hist */  /* table values 1-256 		*/
/*    struct hist_dset *next */    /* pointer to next table set 	*/
};

struct data_hist_rec default_dhr = {
/*    short seq_num */        0,   /* data histogram record seq no. */
/*    short sar_chan */       0,   /* SAR channel                   */
/*    int  ntab */            0,   /* number of table sets          */
/*    int  ltab */            0    /* data set size                 */
/*    struct hist_dset *data */    /* pointer to list of tables     */
};

/****************************
*                           *
*  SAR Leader File:         *
*  Range Spectra Record     *
*                           *
****************************/

struct rng_spec_rec default_rsr = {
/*    short seq_num */   	1,               /* range spectra sequence no. */
/*    short sar_chan */         1,      /* SAR channel */
/*    int  n_dset */            1,     /* number of data sets */
/*    int  dset_size */         4032,     /* data set size */
/*    short req_recs */         1,      /* number of records required */
/*    short table_no */         1,      /* table sequence number */
/*    int  n_pixels */          2048,     /* number of pixels in line */
/*    int  pixel_offset */      0,     /* offset from first pixel */
/*    int  n_lines */           64,     /* number of lines integrated for spectra */
/*    double first_freq */      0,      /* center freq of first spectra bin */
/*    double last_freq */       -99.0,      /* center freq of last spectra bin */
/*    double min_power */       -99.0,      /* minimum spectral power */
/*    double max_power */       -99.0,      /* maximum spectral power */
/*    char  spare_rsr_1[17] */  "",      /* spare */
/*    char  spare_rsr_2[17] */  "",      /* spare */
/*    int  n_bins */            128,     /* number of freq bins in table */
/*    double data_values_spec[256] */ { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
					/* spectral data values 1-256 */
/*    char  spare_rsr_3[1053] */ ""     /* spare */
};

