#ifndef _LAS_H_
#define _LAS_H_

void byte2intArr(unsigned char *inBuf,int *outArr,int nInts);
void int2byteArr(int *inArr,unsigned char *outBuf,int nInts);

/***************** begin ddr.h *****************/
struct DDR                /* ddr for integer data */
    {
    int nl;              /* number of lines in image                */
    int ns;              /* number of samples in image              */
    int nbands;          /* number of bands in image                */
    int dtype;           /* data type of pixels:                    */
                          /*   =1:  unsigned char                    */
                          /*   =2:  short                            */
                          /*   =3:  int                             */
                          /*   =4:  float                            */
    int master_line;     /* line relative to master image            */
    int master_sample;   /* sample relative to master image         */
    int valid[8];         /* valid flags:                            */
                          /*   =0:  not valid                        */
                          /*   =1:  valid                            */
                          /*   =2:  unknown                          */
    int proj_code;       /* GCTP projection code                    */
                          /*  refer to GCTP document for valid codes */
    int zone_code;       /* UTM or State Plane zone                 */
                          /*  refer to GCTP document for valid codes */
    int datum_code;      /* GCTP datum code                         */
                          /*  refer to GCTP document for valid codes */
    int spare;            /* spare integer value; for future use or
                             expansion                               */

    char system[12];       /* computer system data is on (with NULL)  */
    char proj_units[12];   /* Projection units (GCTP units+other) (with NULL) */
    char last_used_date[12]; /* last access date        (with NULL)          */
                            /* NOTE: All ddr dates are stored as     */
                            /* "dd-mmm-yy".  For example, a date     */
                            /* of December 31, 1986 is stored as     */
                            /* "31-dec-86" with the month in lower   */
                            /* case.                                 */
    char last_used_time[12]; /* last access time        (with NULL)  */
                            /* NOTE: All ddr times are stored        */
                            /* using a twenty-four hour clock.       */
                            /* Seconds are speperated by a colon.    */
                            /* Example: 1:05:55 pm is stored as      */
                            /* 1305:55                               */

    double proj_coef[15]; /* GCTP projection parameters              */
                          /*  refer to GCTP document for field definitions*/
    double upleft[2];     /* Corner coordinates entered              */
    double loleft[2];     /*   in latitude/longitude or              */
    double upright[2];    /*   northing/easting order or             */
    double loright[2];    /*   projection coordinates (y/x)          */
    double pdist_y;       /* projection distance/pixel (y-direction) */
    double pdist_x;       /* projection distance/pixel (x-direction) */
    double line_inc;      /* line increment for sampling             */
    double sample_inc;    /* sample increment for sampling           */
                          /* NOTE:  The line/sample increments are   */
                          /*   the values applied to the original    */
                          /*   image to obtain this image.  If re-   */
                          /*   or sub-sampling was not applied, the  */
                          /*   value contained in these fields will  */
                          /*   be 1.0                                */
    };

struct BDDR
    {
    int bandno;   /* band no. of the record                  */
    int valid;            /* min/max validity flag of the band       */
                          /*   =0:  not valid                        */
                          /*   =1:  valid                            */
                          /*   =2:  bounded                          */

    double minval;        /* minimum value of the band               */
    double maxval;        /* maximum value of the band               */

    char source[32];      /* source of data (with NULL)              */
    char instrument[32];  /* type of sensor (with NULL)              */
    char direction[64];   /* direction of capture process(with NULL) */
    char date[10];        /* capture date (with NULL)                */
                          /* NOTE: All ddr dates are stored as       */
                          /* "dd-mmm-yy".  For example, a date       */
                          /* of December 31, 1986 is stored as       */
                          /* "31-dec-86" with the month in lower     */
                          /* case.                                   */
    char time[8];         /* capture time (with NULL)                */
                          /* NOTE: All ddr times are stored          */
                          /* using a twenty-four hour clock.         */
                          /* Seconds are speperated by a colon.      */
                          /* Example: 1:05:55 pm is stored as        */
                          /* 1305:55                                 */
    };

/*  Below are the constants to be used as keys within GETDDF and PUTDDF
-----------------------------------------------------------------------*/
#define DDNL 0            /* number of lines                           */
#define DDNS 1            /* number of samples                         */
#define DDNB 2            /* number of bands                           */
#define DDDTYP 3          /* Data type                                 */
#define DDML 4            /* Master line                               */
#define DDMS 5            /* Master sample                             */
#define DDVFLG 6          /* Validity flag array                       */
#define DDPCOD 7          /* Projection code                           */
#define DDZCOD 8          /* Zone code                                 */
#define DDDCOD 9          /* Datum code                                */
#define DDSYS 10          /* System                                    */
#define DDUNIT 11         /* Projection Unit                           */
#define DDLDAT 12         /* Last use dated                            */
#define DDLTIM 13         /* Last used time                            */
#define DDPCF 14          /* Projection coeficients                    */
#define DDUL 15           /* Upper left                                */
#define DDLL 16           /* Lower left                                */
#define DDUR 17           /* Upper right                               */
#define DDLR 18           /* Lower right                               */
#define DDPDY 19          /* Projection distance-y                     */
#define DDPDX 20          /* Projection distance-x                     */
#define DDLINC 21         /* Line increment                            */
#define DDSINC 22         /* Sample increment                          */

/*  Below are the constants to be used as keys within GETBDF and PUTBDF
-----------------------------------------------------------------------*/
#define DDBAND 100        /* band number                               */
#define DDMMV 101         /* Min/max validity flag                     */
#define DDMIN 102         /* Minimum value                             */
#define DDMAX 103         /* Maximum value                             */
#define DDSRC 104         /* Source                                    */
#define DDINST 105        /* Instrument                                */
#define DDDIR 106         /* Direction                                 */
#define DDCDAT 107        /* Capture date                              */
#define DDCTIM 108        /* Capture time                              */

/* Below are the constants to retrieve the correct validity flag from the
   validity flag array                                                   
-----------------------------------------------------------------------*/
#define DDPCV 0         /* projection code validity flag               */
#define DDZCV 1         /* zone code validity flag                     */
#define DDDCV 2         /* datum code validity flag                    */
#define DDPPV 3         /* projection parameters validity flag         */
#define DDPUV 4         /* ground units validity flag                  */
#define DDPDV 5         /* ground distance validity flag               */
#define DDCCV 6         /* corner coordinates validity flag            */
#define DDINCV  7       /* line/sample increments validity flag        */

/*  Below of Misc. DDR constants
------------------------------------------------------------------------*/
#define E_PROT -2         /* return status of write protected           */
#define INVAL 0           /* invalid validity flag status               */
#define VALID 1           /* valid validity flag                        */
#define UNKNOW 2          /* unknown and equal validity flag            */
#define BOUND 2           /* bounded min/max validity flag              */
#define COMB 1            /* images are being combined into one band    */
#define NOCOMB 0          /* images are not being combined into one band*/

/*  Below are constants used only within the DDR support routines
------------------------------------------------------------------------*/
#define USAME 2           /* unknown and equal validity flag            */
#define UDIFF 3           /* unknown and not equal validity flag        */

#define DISIZE 18         /* number of integers in record 1 of DDR      */
#define DDSIZE 27         /* number of doubles in record 2 of DDR       */
#define DBSIZE 2          /* number of doubles in band record(s) of DDR */

#define DDTSLN 152        /* length of all strings in band record of DDR*/
#define DDSRLN 32         /* length of the SOURCE character string      */
#define DDINLN 32         /* length of the INSTRUMENT character string  */
#define DDDRLN 64         /* length of the DIRECTION character string   */
#define DDCDLN 10         /* length of the capture DATE character string*/
#define DDCTLN 8          /* length of the capture TIME character string*/
#define DDBNLN 4          /* length of the band no. character string    */
#define DDVLLN 2          /* length of the valid flag character string  */

#define DDSYLN 12         /* length of the SYSTEM character string      */
#define DDPULN 12         /* length of the PROJ_UNITS character string  */
#define DDLDLN 12         /* length of the LAST_USED_DATE char. string  */
#define DDLTLN 12         /* length of the LAST_USED_TIME char. string  */
#define DDSTCT 4          /* No. of string in record 1 of DDR file      */
#define DDNVAL 8          /* No. of valid flags stored in the DDR file  */


#ifndef lasErr
#define lasErr int
#endif

#ifndef FUNCTION
#define FUNCTION
#endif

void FUNCTION set_zone(const struct DDR in_ddr[],struct DDR *out_ddr,
		       int *zone_flag,int nimg);
void FUNCTION set_punit(const struct DDR in_ddr[],struct DDR *out_ddr,int nimg);
void FUNCTION set_proj(const struct DDR in_ddr[],struct DDR *out_ddr,
		       int *proj_flag,int nimg);
void FUNCTION set_ppar(const struct DDR in_ddr[],struct DDR *out_ddr,
		       int *ppar_flag,int *proj_flag,int nimg);
void FUNCTION set_pdist(int pdist_flag,const struct DDR in_ddr[],
                struct DDR *out_ddr,double line_inc,double samp_inc);
void FUNCTION set_master(const struct DDR in_ddr[],struct DDR *out_ddr,
			 int inc_flag,int window[]);
void FUNCTION set_inc_flag(const struct DDR ddr[],int *inc_flag,int *inc_index,
			   int nimg);
void FUNCTION set_datum(const struct DDR in_ddr[],struct DDR *out_ddr,int nimg);
void FUNCTION set_corners(int corner_flag,int proj_flag,int pdist_flag,
			  int zone_flag,int ppar_flag,
			  const struct DDR in_ddr[],struct DDR *out_ddr,
			  double line_inc,double samp_inc,int window[]);
void FUNCTION set_capture(const struct BDDR in_bddr[],struct BDDR *out_bddr,
			  int nimg);
void FUNCTION set_pdis_flag(const struct DDR ddr[],int *pdist_flag,
			    int *pdist_index,int nimg);
lasErr FUNCTION set_increment(int inc_flag,const struct DDR *in_ddr,
			      struct DDR *out_ddr,double in_line_inc,
			      double in_samp_inc);
void FUNCTION set_corn_flag(struct DDR ddr[],int *corner_flag,int *window[],
			    int *copy_index,int pdist_flag, float tgbl,
			    int pdist_index,int gbl_flag,int nimg);
void FUNCTION com_ppar(struct DDR ddr1,struct DDR ddr2,int *ppar_flag,
		       int default_flag,int image1,int image2);

double packed_deg(double deg);/*Convert from packed degree format used by
				LAS.*/

lasErr c_getddr(const char *hname,struct DDR *ddr);
void FUNCTION c_intddr(struct DDR *ddr);
lasErr c_putddr(const char *hname,struct DDR *ddr);

/*BDRs are now written by the c_putddr routine.  These routines are depricated*/
lasErr int_c_getbdr(const char  *hname,struct BDDR *bddr, int  *band);
void FUNCTION int_c_intbdr(struct BDDR *bddr);
lasErr FUNCTION int_c_putbdr(const char *hname,const struct BDDR *bddr);
/*End depricated.*/

void FUNCTION comp_sm_esq(struct DDR ddr1,struct DDR ddr2,int *ppar_flag,
        int default_flag,int image1,int image2);
void FUNCTION comp_prec(double ddr1_value,double ddr2_value,int   *ppar_flag,
        int   prec,int   default_flag,int   image1,int   image2);
void FUNCTION com_ppar(struct DDR ddr1,struct DDR ddr2,int *ppar_flag,
        int default_flag,int image1,int image2);

/*Get per-pixel data size from data type.*/
#define DTYPE_BYTE 1
#define DTYPE_SHORT 2
#define DTYPE_LONG 3
#define DTYPE_FLOAT 4
#define DTYPE_DOUBLE 5
#define DTYPE_COMPLEX 10
#define DTYPE_COMPLEXREAL 11
#define DTYPE_COMPLEXIMAG 12
/**************** end ddr.h *******************/

/***************** begin worgen.h *******************/
/* Boolean true and false 
-------------------------*/
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

/* define NULL 
--------------*/
#ifndef NULL
#define NULL 0
#endif

/* SUCCESS or FAIL return status,
   End of File, & Buffer to small
---------------------------------*/
enum {
        E_SUCC=0,
        E_FAIL=-1,
        E_EOF=-2,
        E_SMAL=-3
};
#ifndef lasErr
#define lasErr int
#endif

/* character string lengths
---------------------------*/
#define ERRLEN 256
#ifndef CMLEN
#define CMLEN 256
#endif

/* valid data types 
-------------------*/
#define EBYTE 1
#define EWORD 2
#define ELONG 3
#define EREAL 4
#define EDOUBLE 5
#define ECHAR 6
#define EUWORD 7
#define EULONG 8
#define NDTYPES 8
        
/* file access types 
--------------------*/
#define IREAD 0
#define IWRITE 1
#define IUPDATE 2

/*  define the key word FUNCTION  -- This is placed at the beginning of each 
    function declared in 'C' in order to designate the functions from each other
------------------------------------------------------------------------------*/
#ifndef FUNCTION                /*  if not defined                            */
#define FUNCTION                /*  define FUNCTION                           */
#endif


void print_mes(char *,char *);
#define LAS_FATAL -1
#define NON_FATAL 1
void c_errmsg(char      * message,char  * key, int      severity);
char * c_getsys(void);
FUNCTION char *squeeze(register const char *str,register int  len);
void FUNCTION c_up2low(register char *buf, register int   *size);
void c_low2up(register char *buf, register int *size);
lasErr FUNCTION c_sysset(const char *sysName,int *sysNumber);
lasErr FUNCTION c_pxsys(int insys, unsigned char *buf,int dtype,int size);
lasErr FUNCTION c_pxswap(unsigned char *buf, int ns, int size);
        
#ifndef FILE
#endif
lasErr FUNCTION c_lsopen ( FILE **fd, const char *hname, int *rwmode);
lasErr FUNCTION c_lsread(FILE **fd,const char * key, int *clen, int *dlen, 
                char *cbuf, unsigned char *dbuf, char *dtype);
lasErr FUNCTION c_lswrit(FILE   **fd, const char *key, int      *clen, int      *dlen, 
                const char *cbuf, const unsigned char *dbuf, const char *dtype);
lasErr FUNCTION c_lsstat (const char *hostname, int *mode);
void FUNCTION c_lsmknm(const char *inname,const char *ext,char *outname);
lasErr FUNCTION c_lsclos (FILE **fd, const char *hostname, int *action);
FUNCTION char *squeeze(register const char *str,register int  len);
/***************** end worgen.h *****************/

/***************** begin vll.h *****************/
#define READ    0
#define WRITE   1
#define APPEND  2
#define RW      3

#define LENL    13
#define TYPL    3
#define KEYL    16
#define HDRL    (LENL + TYPL + KEYL)

#ifndef min
#define min(a,b)        ((a) < (b) ? (a) : (b))
#endif

struct HDR
    {
    char  len[LENL];
    char  type[TYPL];
    char  key[KEYL];
    };

#ifdef VMS

#define unlink  delete

#define L_SET   0
#define L_INCR  1
#define L_XTND  2

#define R_OK    4
#define W_OK    2
#define X_OK    1
#define F_OK    0

#endif
/***************** end vll.h *******************/

/***************** begin sysdef.h ***************/
/*
  Defines the valid systems
*/
#define NMR_SYSTEMS      5             /* Number of systems defined         */

#define SYS_IEEE_STD     0              /* IEEE standard (ref 754)           */
#define SYS_IEEE_LIL     1              /* IEEE standard (ref 754) little-endian */
#define SYS_IBM_MVS          2          /* IBM MVS                           */
#define SYS_CRAY_UNICOS  3              /* Cray Y-MP Unicos                  */
#define SYS_OTHER_MSC    4              /* Misc. Other systems not covered   */

/*
  Strings for the same systems as above
*/
#define IEEE    "ieee-std"
#define IEEE_LIL "ieee-lil"
#define MVS     "ibm-mvs"
#define UNICOS  "cray-unicos"
#define MSC     "other-msc"

/*
  Underflow and Overflow flags
*/
#define SYS_ERR         0       /* Error on underflow and overflow           */
#define SYS_SET         1       /* Set to min/max for system                 */
#define SYS_USE         2       /* Use specified underflow & overflow        */

/*
  System code conversions
*/
#define SETN    0       /* Convert system string to system numerical code    */
#define SETS    1       /* Convert system numerical code to system string    */
/***************** end sysdef.h *****************/

/***************** begin imageio.h *********************/
#define MAXBND 256   /* Maximum number of bands to be processed            */
#define MAXNS  5000000 /* Maximum number of samples per line to be processed */
#define MAXNL  5000000 /* Maximum number of lines to be processed                  */

/* Arrays used for specifying windows contain four fields, sl, ss, nl, ns.
   Here are the definitions of those array indices.
---------------------------------------------------------------------------*/
#define SL 0
#define SS 1
#define NL 2
#define NS 3

#ifdef aiws
#define MAXIMG 20    /* Maximum number of images to be processed           */
#elif defined(gould)
#define MAXIMG 54    /* Maximum number of images to be processed           */
#elif defined(_IBMR2)
#define MAXIMG 54    /* Maximum number of images to be processed           */
#elif defined(vms)
#define MAXIMG 56    /* Maximum number of images to be processed           */
#elif defined(sgi)
#define MAXIMG 54    /* Maximum number of images to be processed           */
#elif defined(DGUX)
#define MAXIMG 54    /* Maximum number of images to be processed           */
#else
#define MAXIMG 50
#endif
/*  Maximum buffer size to be used for most memory allocations
--------------------------------------------------------------*/
#ifdef vms
#define MAXBUFS 65535             /* 65535 is the largest i/o vms can do */
#else
#define MAXBUFS 1024 * 1024 * 20.0 /* 20.0 megabyte of memory -estimated guess
                                     of the best amount of memory to allocate */
#endif

/* method types 
---------------*/
#define STEP 1
#define RANDOM 2

#define NLABELS 0
#define LABSIZ 512
#define ORG I_CS

/* fdesc flags 
--------------*/
#define CONVERSION 1
#define EDONE 2
#define MOVEDATA 4


/* BLOCK IMAGE PROCESSING CONSTANTS
-----------------------------------*/

/* boundary fill techniques
---------------------------*/
#define NOFIL 0
#define MIRR  1

/* end of image window return status
------------------------------------*/
#define E_EOIW   1

/* no data conversion required
------------------------------*/
#define SAME  0

/* 'next flag' options
----------------------*/
#define CREAT  -1
#define INIT    0
#define CURNT   1
#define NEXT    2

/* Compression type
-------------------*/
#define COMPRES 5
#define NO_COMPRES 6
/***************** end imageio.h ***********************/

/***************** begin ieee.h ******************/
#define IEEE_R4_EXCESS  126
#define IEEE_R8_EXCESS 1022
#define IEEE_POW_LEXP     0     /* low index of IEEE power table */
#define IEEE_POW_HEXP   253     /* high index of IEEE power table */
#define IEEE_MINEXP    -126
#define IEEE_MAXEXP     127

static double IEEE_pow[] =
  {
  1.17549435082228750796873653722224567782e-38,   /* pow(2,-126) index   0 */
  2.35098870164457501593747307444449135564e-38,   /* pow(2,-125) index   1 */
  4.70197740328915003187494614888898271127e-38,   /* pow(2,-124) index   2 */
  9.40395480657830006374989229777796542255e-38,   /* pow(2,-123) index   3 */
  1.88079096131566001274997845955559308451e-37,   /* pow(2,-122) index   4 */
  3.76158192263132002549995691911118616902e-37,   /* pow(2,-121) index   5 */
  7.52316384526264005099991383822237233804e-37,   /* pow(2,-120) index   6 */
  1.50463276905252801019998276764447446761e-36,   /* pow(2,-119) index   7 */
  3.00926553810505602039996553528894893522e-36,   /* pow(2,-118) index   8 */
  6.01853107621011204079993107057789787043e-36,   /* pow(2,-117) index   9 */
  1.20370621524202240815998621411557957409e-35,   /* pow(2,-116) index  10 */
  2.40741243048404481631997242823115914817e-35,   /* pow(2,-115) index  11 */
  4.81482486096808963263994485646231829635e-35,   /* pow(2,-114) index  12 */
  9.62964972193617926527988971292463659269e-35,   /* pow(2,-113) index  13 */
  1.92592994438723585305597794258492731854e-34,   /* pow(2,-112) index  14 */
  3.85185988877447170611195588516985463708e-34,   /* pow(2,-111) index  15 */
  7.70371977754894341222391177033970927415e-34,   /* pow(2,-110) index  16 */
  1.54074395550978868244478235406794185483e-33,   /* pow(2,-109) index  17 */
  3.08148791101957736488956470813588370966e-33,   /* pow(2,-108) index  18 */
  6.16297582203915472977912941627176741932e-33,   /* pow(2,-107) index  19 */
  1.23259516440783094595582588325435348386e-32,   /* pow(2,-106) index  20 */
  2.46519032881566189191165176650870696773e-32,   /* pow(2,-105) index  21 */
  4.93038065763132378382330353301741393546e-32,   /* pow(2,-104) index  22 */
  9.86076131526264756764660706603482787092e-32,   /* pow(2,-103) index  23 */
  1.97215226305252951352932141320696557418e-31,   /* pow(2,-102) index  24 */
  3.94430452610505902705864282641393114837e-31,   /* pow(2,-101) index  25 */
  7.88860905221011805411728565282786229673e-31,   /* pow(2,-100) index  26 */
  1.57772181044202361082345713056557245935e-30,   /* pow(2, -99) index  27 */
  3.15544362088404722164691426113114491869e-30,   /* pow(2, -98) index  28 */
  6.31088724176809444329382852226228983739e-30,   /* pow(2, -97) index  29 */
  1.26217744835361888865876570445245796748e-29,   /* pow(2, -96) index  30 */
  2.52435489670723777731753140890491593495e-29,   /* pow(2, -95) index  31 */
  5.04870979341447555463506281780983186991e-29,   /* pow(2, -94) index  32 */
  1.00974195868289511092701256356196637398e-28,   /* pow(2, -93) index  33 */
  2.01948391736579022185402512712393274796e-28,   /* pow(2, -92) index  34 */
  4.03896783473158044370805025424786549593e-28,   /* pow(2, -91) index  35 */
  8.07793566946316088741610050849573099185e-28,   /* pow(2, -90) index  36 */
  1.61558713389263217748322010169914619837e-27,   /* pow(2, -89) index  37 */
  3.23117426778526435496644020339829239674e-27,   /* pow(2, -88) index  38 */
  6.46234853557052870993288040679658479348e-27,   /* pow(2, -87) index  39 */
  1.29246970711410574198657608135931695870e-26,   /* pow(2, -86) index  40 */
  2.58493941422821148397315216271863391739e-26,   /* pow(2, -85) index  41 */
  5.16987882845642296794630432543726783479e-26,   /* pow(2, -84) index  42 */
  1.03397576569128459358926086508745356696e-25,   /* pow(2, -83) index  43 */
  2.06795153138256918717852173017490713391e-25,   /* pow(2, -82) index  44 */
  4.13590306276513837435704346034981426783e-25,   /* pow(2, -81) index  45 */
  8.27180612553027674871408692069962853566e-25,   /* pow(2, -80) index  46 */
  1.65436122510605534974281738413992570713e-24,   /* pow(2, -79) index  47 */
  3.30872245021211069948563476827985141426e-24,   /* pow(2, -78) index  48 */
  6.61744490042422139897126953655970282853e-24,   /* pow(2, -77) index  49 */
  1.32348898008484427979425390731194056571e-23,   /* pow(2, -76) index  50 */
  2.64697796016968855958850781462388113141e-23,   /* pow(2, -75) index  51 */
  5.29395592033937711917701562924776226282e-23,   /* pow(2, -74) index  52 */
  1.05879118406787542383540312584955245256e-22,   /* pow(2, -73) index  53 */
  2.11758236813575084767080625169910490513e-22,   /* pow(2, -72) index  54 */
  4.23516473627150169534161250339820981026e-22,   /* pow(2, -71) index  55 */
  8.47032947254300339068322500679641962051e-22,   /* pow(2, -70) index  56 */
  1.69406589450860067813664500135928392410e-21,   /* pow(2, -69) index  57 */
  3.38813178901720135627329000271856784821e-21,   /* pow(2, -68) index  58 */
  6.77626357803440271254658000543713569641e-21,   /* pow(2, -67) index  59 */
  1.35525271560688054250931600108742713928e-20,   /* pow(2, -66) index  60 */
  2.71050543121376108501863200217485427856e-20,   /* pow(2, -65) index  61 */
  5.42101086242752217003726400434970855713e-20,   /* pow(2, -64) index  62 */
  1.08420217248550443400745280086994171143e-19,   /* pow(2, -63) index  63 */
  2.16840434497100886801490560173988342285e-19,   /* pow(2, -62) index  64 */
  4.33680868994201773602981120347976684570e-19,   /* pow(2, -61) index  65 */
  8.67361737988403547205962240695953369141e-19,   /* pow(2, -60) index  66 */
  1.73472347597680709441192448139190673828e-18,   /* pow(2, -59) index  67 */
  3.46944695195361418882384896278381347656e-18,   /* pow(2, -58) index  68 */
  6.93889390390722837764769792556762695313e-18,   /* pow(2, -57) index  69 */
  1.38777878078144567552953958511352539063e-17,   /* pow(2, -56) index  70 */
  2.77555756156289135105907917022705078125e-17,   /* pow(2, -55) index  71 */
  5.55111512312578270211815834045410156250e-17,   /* pow(2, -54) index  72 */
  1.11022302462515654042363166809082031250e-16,   /* pow(2, -53) index  73 */
  2.22044604925031308084726333618164062500e-16,   /* pow(2, -52) index  74 */
  4.44089209850062616169452667236328125000e-16,   /* pow(2, -51) index  75 */
  8.88178419700125232338905334472656250000e-16,   /* pow(2, -50) index  76 */
  1.77635683940025046467781066894531250000e-15,   /* pow(2, -49) index  77 */
  3.55271367880050092935562133789062500000e-15,   /* pow(2, -48) index  78 */
  7.10542735760100185871124267578125000000e-15,   /* pow(2, -47) index  79 */
  1.42108547152020037174224853515625000000e-14,   /* pow(2, -46) index  80 */
  2.84217094304040074348449707031250000000e-14,   /* pow(2, -45) index  81 */
  5.68434188608080148696899414062500000000e-14,   /* pow(2, -44) index  82 */
  1.13686837721616029739379882812500000000e-13,   /* pow(2, -43) index  83 */
  2.27373675443232059478759765625000000000e-13,   /* pow(2, -42) index  84 */
  4.54747350886464118957519531250000000000e-13,   /* pow(2, -41) index  85 */
  9.09494701772928237915039062500000000000e-13,   /* pow(2, -40) index  86 */
  1.81898940354585647583007812500000000000e-12,   /* pow(2, -39) index  87 */
  3.63797880709171295166015625000000000000e-12,   /* pow(2, -38) index  88 */
  7.27595761418342590332031250000000000000e-12,   /* pow(2, -37) index  89 */
  1.45519152283668518066406250000000000000e-11,   /* pow(2, -36) index  90 */
  2.91038304567337036132812500000000000000e-11,   /* pow(2, -35) index  91 */
  5.82076609134674072265625000000000000000e-11,   /* pow(2, -34) index  92 */
  1.16415321826934814453125000000000000000e-10,   /* pow(2, -33) index  93 */
  2.32830643653869628906250000000000000000e-10,   /* pow(2, -32) index  94 */
  4.65661287307739257812500000000000000000e-10,   /* pow(2, -31) index  95 */
  9.31322574615478515625000000000000000000e-10,   /* pow(2, -30) index  96 */
  1.86264514923095703125000000000000000000e-09,   /* pow(2, -29) index  97 */
  3.72529029846191406250000000000000000000e-09,   /* pow(2, -28) index  98 */
  7.45058059692382812500000000000000000000e-09,   /* pow(2, -27) index  99 */
  1.49011611938476562500000000000000000000e-08,   /* pow(2, -26) index 100 */
  2.98023223876953125000000000000000000000e-08,   /* pow(2, -25) index 101 */
  5.96046447753906250000000000000000000000e-08,   /* pow(2, -24) index 102 */
  1.19209289550781250000000000000000000000e-07,   /* pow(2, -23) index 103 */
  2.38418579101562500000000000000000000000e-07,   /* pow(2, -22) index 104 */
  4.76837158203125000000000000000000000000e-07,   /* pow(2, -21) index 105 */
  9.53674316406250000000000000000000000000e-07,   /* pow(2, -20) index 106 */
  1.90734863281250000000000000000000000000e-06,   /* pow(2, -19) index 107 */
  3.81469726562500000000000000000000000000e-06,   /* pow(2, -18) index 108 */
  7.62939453125000000000000000000000000000e-06,   /* pow(2, -17) index 109 */
  1.52587890625000000000000000000000000000e-05,   /* pow(2, -16) index 110 */
  3.05175781250000000000000000000000000000e-05,   /* pow(2, -15) index 111 */
  6.10351562500000000000000000000000000000e-05,   /* pow(2, -14) index 112 */
  1.22070312500000000000000000000000000000e-04,   /* pow(2, -13) index 113 */
  2.44140625000000000000000000000000000000e-04,   /* pow(2, -12) index 114 */
  4.88281250000000000000000000000000000000e-04,   /* pow(2, -11) index 115 */
  9.76562500000000000000000000000000000000e-04,   /* pow(2, -10) index 116 */
  1.95312500000000000000000000000000000000e-03,   /* pow(2,  -9) index 117 */
  3.90625000000000000000000000000000000000e-03,   /* pow(2,  -8) index 118 */
  7.81250000000000000000000000000000000000e-03,   /* pow(2,  -7) index 119 */
  1.56250000000000000000000000000000000000e-02,   /* pow(2,  -6) index 120 */
  3.12500000000000000000000000000000000000e-02,   /* pow(2,  -5) index 121 */
  6.25000000000000000000000000000000000000e-02,   /* pow(2,  -4) index 122 */
  1.25000000000000000000000000000000000000e-01,   /* pow(2,  -3) index 123 */
  2.50000000000000000000000000000000000000e-01,   /* pow(2,  -2) index 124 */
  5.00000000000000000000000000000000000000e-01,   /* pow(2,  -1) index 125 */
  1.00000000000000000000000000000000000000e+00,   /* pow(2,   0) index 126 */
  2.00000000000000000000000000000000000000e+00,   /* pow(2,   1) index 127 */
  4.00000000000000000000000000000000000000e+00,   /* pow(2,   2) index 128 */
  8.00000000000000000000000000000000000000e+00,   /* pow(2,   3) index 129 */
  1.60000000000000000000000000000000000000e+01,   /* pow(2,   4) index 130 */
  3.20000000000000000000000000000000000000e+01,   /* pow(2,   5) index 131 */
  6.40000000000000000000000000000000000000e+01,   /* pow(2,   6) index 132 */
  1.28000000000000000000000000000000000000e+02,   /* pow(2,   7) index 133 */
  2.56000000000000000000000000000000000000e+02,   /* pow(2,   8) index 134 */
  5.12000000000000000000000000000000000000e+02,   /* pow(2,   9) index 135 */
  1.02400000000000000000000000000000000000e+03,   /* pow(2,  10) index 136 */
  2.04800000000000000000000000000000000000e+03,   /* pow(2,  11) index 137 */
  4.09600000000000000000000000000000000000e+03,   /* pow(2,  12) index 138 */
  8.19200000000000000000000000000000000000e+03,   /* pow(2,  13) index 139 */
  1.63840000000000000000000000000000000000e+04,   /* pow(2,  14) index 140 */
  3.27680000000000000000000000000000000000e+04,   /* pow(2,  15) index 141 */
  6.55360000000000000000000000000000000000e+04,   /* pow(2,  16) index 142 */
  1.31072000000000000000000000000000000000e+05,   /* pow(2,  17) index 143 */
  2.62144000000000000000000000000000000000e+05,   /* pow(2,  18) index 144 */
  5.24288000000000000000000000000000000000e+05,   /* pow(2,  19) index 145 */
  1.04857600000000000000000000000000000000e+06,   /* pow(2,  20) index 146 */
  2.09715200000000000000000000000000000000e+06,   /* pow(2,  21) index 147 */
  4.19430400000000000000000000000000000000e+06,   /* pow(2,  22) index 148 */
  8.38860800000000000000000000000000000000e+06,   /* pow(2,  23) index 149 */
  1.67772160000000000000000000000000000000e+07,   /* pow(2,  24) index 150 */
  3.35544320000000000000000000000000000000e+07,   /* pow(2,  25) index 151 */
  6.71088640000000000000000000000000000000e+07,   /* pow(2,  26) index 152 */
  1.34217728000000000000000000000000000000e+08,   /* pow(2,  27) index 153 */
  2.68435456000000000000000000000000000000e+08,   /* pow(2,  28) index 154 */
  5.36870912000000000000000000000000000000e+08,   /* pow(2,  29) index 155 */
  1.07374182400000000000000000000000000000e+09,   /* pow(2,  30) index 156 */
  2.14748364800000000000000000000000000000e+09,   /* pow(2,  31) index 157 */
  4.29496729600000000000000000000000000000e+09,   /* pow(2,  32) index 158 */
  8.58993459200000000000000000000000000000e+09,   /* pow(2,  33) index 159 */
  1.71798691840000000000000000000000000000e+10,   /* pow(2,  34) index 160 */
  3.43597383680000000000000000000000000000e+10,   /* pow(2,  35) index 161 */
  6.87194767360000000000000000000000000000e+10,   /* pow(2,  36) index 162 */
  1.37438953472000000000000000000000000000e+11,   /* pow(2,  37) index 163 */
  2.74877906944000000000000000000000000000e+11,   /* pow(2,  38) index 164 */
  5.49755813888000000000000000000000000000e+11,   /* pow(2,  39) index 165 */
  1.09951162777600000000000000000000000000e+12,   /* pow(2,  40) index 166 */
  2.19902325555200000000000000000000000000e+12,   /* pow(2,  41) index 167 */
  4.39804651110400000000000000000000000000e+12,   /* pow(2,  42) index 168 */
  8.79609302220800000000000000000000000000e+12,   /* pow(2,  43) index 169 */
  1.75921860444160000000000000000000000000e+13,   /* pow(2,  44) index 170 */
  3.51843720888320000000000000000000000000e+13,   /* pow(2,  45) index 171 */
  7.03687441776640000000000000000000000000e+13,   /* pow(2,  46) index 172 */
  1.40737488355328000000000000000000000000e+14,   /* pow(2,  47) index 173 */
  2.81474976710656000000000000000000000000e+14,   /* pow(2,  48) index 174 */
  5.62949953421312000000000000000000000000e+14,   /* pow(2,  49) index 175 */
  1.12589990684262400000000000000000000000e+15,   /* pow(2,  50) index 176 */
  2.25179981368524800000000000000000000000e+15,   /* pow(2,  51) index 177 */
  4.50359962737049600000000000000000000000e+15,   /* pow(2,  52) index 178 */
  9.00719925474099200000000000000000000000e+15,   /* pow(2,  53) index 179 */
  1.80143985094819840000000000000000000000e+16,   /* pow(2,  54) index 180 */
  3.60287970189639680000000000000000000000e+16,   /* pow(2,  55) index 181 */
  7.20575940379279360000000000000000000000e+16,   /* pow(2,  56) index 182 */
  1.44115188075855872000000000000000000000e+17,   /* pow(2,  57) index 183 */
  2.88230376151711744000000000000000000000e+17,   /* pow(2,  58) index 184 */
  5.76460752303423488000000000000000000000e+17,   /* pow(2,  59) index 185 */
  1.15292150460684697600000000000000000000e+18,   /* pow(2,  60) index 186 */
  2.30584300921369395200000000000000000000e+18,   /* pow(2,  61) index 187 */
  4.61168601842738790400000000000000000000e+18,   /* pow(2,  62) index 188 */
  9.22337203685477580800000000000000000000e+18,   /* pow(2,  63) index 189 */
  1.84467440737095516160000000000000000000e+19,   /* pow(2,  64) index 190 */
  3.68934881474191032320000000000000000000e+19,   /* pow(2,  65) index 191 */
  7.37869762948382064640000000000000000000e+19,   /* pow(2,  66) index 192 */
  1.47573952589676412928000000000000000000e+20,   /* pow(2,  67) index 193 */
  2.95147905179352825856000000000000000000e+20,   /* pow(2,  68) index 194 */
  5.90295810358705651712000000000000000000e+20,   /* pow(2,  69) index 195 */
  1.18059162071741130342400000000000000000e+21,   /* pow(2,  70) index 196 */
  2.36118324143482260684800000000000000000e+21,   /* pow(2,  71) index 197 */
  4.72236648286964521369600000000000000000e+21,   /* pow(2,  72) index 198 */
  9.44473296573929042739200000000000000000e+21,   /* pow(2,  73) index 199 */
  1.88894659314785808547840000000000000000e+22,   /* pow(2,  74) index 200 */
  3.77789318629571617095680000000000000000e+22,   /* pow(2,  75) index 201 */
  7.55578637259143234191360000000000000000e+22,   /* pow(2,  76) index 202 */
  1.51115727451828646838272000000000000000e+23,   /* pow(2,  77) index 203 */
  3.02231454903657293676544000000000000000e+23,   /* pow(2,  78) index 204 */
  6.04462909807314587353088000000000000000e+23,   /* pow(2,  79) index 205 */
  1.20892581961462917470617600000000000000e+24,   /* pow(2,  80) index 206 */
  2.41785163922925834941235200000000000000e+24,   /* pow(2,  81) index 207 */
  4.83570327845851669882470400000000000000e+24,   /* pow(2,  82) index 208 */
  9.67140655691703339764940800000000000000e+24,   /* pow(2,  83) index 209 */
  1.93428131138340667952988160000000000000e+25,   /* pow(2,  84) index 210 */
  3.86856262276681335905976320000000000000e+25,   /* pow(2,  85) index 211 */
  7.73712524553362671811952640000000000000e+25,   /* pow(2,  86) index 212 */
  1.54742504910672534362390528000000000000e+26,   /* pow(2,  87) index 213 */
  3.09485009821345068724781056000000000000e+26,   /* pow(2,  88) index 214 */
  6.18970019642690137449562112000000000000e+26,   /* pow(2,  89) index 215 */
  1.23794003928538027489912422400000000000e+27,   /* pow(2,  90) index 216 */
  2.47588007857076054979824844800000000000e+27,   /* pow(2,  91) index 217 */
  4.95176015714152109959649689600000000000e+27,   /* pow(2,  92) index 218 */
  9.90352031428304219919299379200000000000e+27,   /* pow(2,  93) index 219 */
  1.98070406285660843983859875840000000000e+28,   /* pow(2,  94) index 220 */
  3.96140812571321687967719751680000000000e+28,   /* pow(2,  95) index 221 */
  7.92281625142643375935439503360000000000e+28,   /* pow(2,  96) index 222 */
  1.58456325028528675187087900672000000000e+29,   /* pow(2,  97) index 223 */
  3.16912650057057350374175801344000000000e+29,   /* pow(2,  98) index 224 */
  6.33825300114114700748351602688000000000e+29,   /* pow(2,  99) index 225 */
  1.26765060022822940149670320537600000000e+30,   /* pow(2, 100) index 226 */
  2.53530120045645880299340641075200000000e+30,   /* pow(2, 101) index 227 */
  5.07060240091291760598681282150400000000e+30,   /* pow(2, 102) index 228 */
  1.01412048018258352119736256430080000000e+31,   /* pow(2, 103) index 229 */
  2.02824096036516704239472512860160000000e+31,   /* pow(2, 104) index 230 */
  4.05648192073033408478945025720320000000e+31,   /* pow(2, 105) index 231 */
  8.11296384146066816957890051440640000000e+31,   /* pow(2, 106) index 232 */
  1.62259276829213363391578010288128000000e+32,   /* pow(2, 107) index 233 */
  3.24518553658426726783156020576256000000e+32,   /* pow(2, 108) index 234 */
  6.49037107316853453566312041152512000000e+32,   /* pow(2, 109) index 235 */
  1.29807421463370690713262408230502400000e+33,   /* pow(2, 110) index 236 */
  2.59614842926741381426524816461004800000e+33,   /* pow(2, 111) index 237 */
  5.19229685853482762853049632922009600000e+33,   /* pow(2, 112) index 238 */
  1.03845937170696552570609926584401920000e+34,   /* pow(2, 113) index 239 */
  2.07691874341393105141219853168803840000e+34,   /* pow(2, 114) index 240 */
  4.15383748682786210282439706337607680000e+34,   /* pow(2, 115) index 241 */
  8.30767497365572420564879412675215360000e+34,   /* pow(2, 116) index 242 */
  1.66153499473114484112975882535043072000e+35,   /* pow(2, 117) index 243 */
  3.32306998946228968225951765070086144000e+35,   /* pow(2, 118) index 244 */
  6.64613997892457936451903530140172288000e+35,   /* pow(2, 119) index 245 */
  1.32922799578491587290380706028034457600e+36,   /* pow(2, 120) index 246 */
  2.65845599156983174580761412056068915200e+36,   /* pow(2, 121) index 247 */
  5.31691198313966349161522824112137830400e+36,   /* pow(2, 122) index 248 */
  1.06338239662793269832304564822427566080e+37,   /* pow(2, 123) index 249 */
  2.12676479325586539664609129644855132160e+37,   /* pow(2, 124) index 250 */
  4.25352958651173079329218259289710264320e+37,   /* pow(2, 125) index 251 */
  8.50705917302346158658436518579420528640e+37,   /* pow(2, 126) index 252 */
  1.70141183460469229370504062281061498880e+38    /* pow(2, 127) index 253 */
  };
/******************* end ieee.h *******************/

#endif
