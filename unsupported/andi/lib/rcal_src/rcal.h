/* this is the calibration library header file
 *
 * it contains:
 *             a set of path macros which must be set to reflect the installation machine
 *             a set of includes, *including* the all important (sarmacros.h)
 *             function declarations from rcal_ceos
 *             CEOS record macros
 *             function declarations from rcal_io
 *             function declarations from rcal_math
 *             function declarations from rcal_string
 *
 */

#ifndef RCAL_HEADER
#define RCAL_HEADER 1

/* include standard Xview include files if macro ROB_GRAPHICS has been defined */
#ifdef ROB_GRAPHICS
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <olgx/olgx.h>
#include <xview/canvas.h>
#include <xview/cms.h>
#include <xview/cursor.h>
#include <xview/xv_xrect.h>
#include <xview/scrollbar.h>
#include <xview/font.h>
#include <time.h>
#endif

/* 
 * Include sarmacros.h file (edit Makefile to set path to this file) 
 * note that this include comes AFTER the graphics includes so that
 * sarmacros can whack the #define of Complex in 'X.h'.
 */
#include SARINC

/* path macros; must be set for host machine */
#ifndef TARGET_DBASE_PATH
#define TARGET_DBASE_PATH "/sonja/calval/cal/src/disa/"
#endif
#ifndef TARGET_DBASE_FILE
#define TARGET_DBASE_FILE  "Targ.dbase"
#endif
#ifndef IDL_SOURCE_PATH
#define IDL_SOURCE_PATH "/sonja/calval/cal/src/idl/"
#endif


/* R C A L _ C E O S */

/* rcal_ceos functions:  external access; test/utility */
void  rcal_get_test_string_from_arbitrary_record();
void  rcal_get_test_double_FRR();
void  rcal_get_test_integer_FRR();
void  rcal_get_test_string_FRR();
void  rcal_get_test_double();
void  rcal_get_test_integer();
void  rcal_get_test_string();

/* rcal_ceos functions:  external access; functional */
void  rcal_get_rev_id ();
void  rcal_get_image_id ();
void  rcal_get_utc_time ();
void  rcal_get_correlation_time ();
void  rcal_get_image_lat_lon ();
void  rcal_get_img_pix_dims ();
void  rcal_get_3_state_vectors();
void  rcal_get_state_vector ();
void  rcal_get_platform_alt ();
void  rcal_get_boresight_angle ();
void  rcal_get_earth_radii();
void  rcal_get_slant_ranges();
void  rcal_get_proc_gain ();
void  rcal_get_resolution_and_pixel_size ();

/* for uncooperative (ancient) trailer files */
void  rcal_brute_get_img_pix_dims ();
void  rcal_brute_get_proc_gain ();
void  rcal_brute_get_slant_ranges ();

/* rcal_ceos functions:  internal access only (static) */
static int   rcal_ceos_get_value ();
static int   rcal_get_record_type ();
static void  rcal_get_integer_value ();
static void  rcal_get_double_value ();
static void  check ();

/* 
 * Your handy guide to 
 * CEOS ceos Ceos c e o s C E O S See-Eee-Oh-Ess
 * record labels 
 */

/* COMMON RECORDS */
/* volume descriptor record (ldr and tlr file record 1)        VDR */

/* LEADER FILE RECORDS */
/* data set summary record (ldr file record 2)                 DSS */
/* map projection data record (ldr file record 2.5)            MAP */
/* platform position record (ldr file record 3)                PPR */
/* attitude data record (ldr file record 4)                    ADR */
/* radiometric data record (ldr file record 5)                 RDR */
/* radiometric compensation record (ldr file record 6)         RCR */
/* data quality summary record (ldr file record 7)             DQS */
/* data histogram record (ldr file record 8)                   DHR */
/* range spectra record (ldr file record 9)                    RSR */

/* TRAILER FILE RECORDS */
/* detailed processing parameters record (tlr file record 2)   DPR */
/* calibration data record  (tlr file record 3)                CDR */
/* facility related record (tlr file record 4)                 FRR */

/* ANCIENT NUL RECORD */
/* archaic 'OLD' record from early data                        OLD */

/* C E O S   Record Labels (Rhino, Arista, etc...)  */
#define NUL 0
#define VDR 1
#define DSS 2
#define MAP 14    /* Jason mod -- had to add this one after rob forgot it*/
#define PPR 3
#define ADR 4
#define RDR 5
#define RCR 6
#define DQS 7
#define DHR 8
#define RSR 9
#define DPR 10
#define CDR 11
#define FRR 12
#define OLD 13


/* 
 * __________________________________________________
 *
 */

/* R C A L _ I O   rcal_io */
void    getInt();              /* default input function, gets a integer     */
void    getFloat();            /* default input function, gets a float       */
void    getDouble();           /* default input function, gets a double      */
void    getString();           /* default input function, gets a string      */
void    printMatrix();         /* diagnostic print function                  */
int     getFileLength();       /* determine out how many lines in a file     */
void    readMatrix();          /* read a matrix from a file into a buffer    */
void    writeVector();         /* write a vector to a file                   */
void    appendVector();        /* append a vector to a file                  */
int     checkDataType();       /* verify that a given data type is defined   */
void    rlogfileInit();        /* initialize a logfile                       */
void    rlogfileMsg();         /* write a message to a logfile               */
void    rlogfileBlankline();   /* write a blankline to a logfile             */
int     rIndex();              /* return a pointer index for a 2-D array     */
void    rWait();               /* wait n seconds                             */
void    copyVector();          /* generalized vector copy                    */

/* 
 * __________________________________________________
 *
 */

/* IDL functions */
void    idlXYPlotHard();     /* generate a histogram hardcoy using IDL     */
void    idlXYZSurface();     /* display a surface plot                     */

/* 
 * __________________________________________________
 *
 */

/* R C A L _ M A T H */
double  dstnc();
int     std_stat();
int     cpx_stat();
int     centroid();
int     matrix_product ();
int     xprod_3dvectors();
int     line_correlate();
void    findoffset();
void    convertPowerToDB();
int     twoExponent();
double  dBToLinear();
double  linearTodB();

/* simple Complex number functions */


/* 
 * Important note:  this section is within an ifndef because Xview stupidly 
 *   defines 'Complex' to be something else...
 */

/* in the following, a and b are Complex, s is (double) scalar      */
Complex Cmul();     /* (a, b):  multiply a times b                  */
Complex Cdiv();     /* (a, b):  divide a by b                       */
Complex Cadd();     /* (a, b):  add a and b                         */
Complex Csub();     /* (a, b):  subtract b from a                   */
Complex Csmul();    /* (s, a):  multiply a by s                     */
Complex Csdiv();    /* (s, a):  divide complex a by scalar s        */
Complex Cconj();    /* (a):     return conjugate of a               */
Complex Czero();    /* ():      return complex zero (0.0,  0.0)     */
Complex Cone();     /* ():      return complex 1    (1.0,  0.0)     */
Complex Cneg_one(); /* ():      return complex -1   (-1.0, 0.0)     */
Complex Ci();       /* ():      return complex i    (0.0,  1.0)     */
Complex Cneg_i();   /* ():      return complex -i   (0.0, -1.0)     */
Complex Cnorm();    /* (a):     return a normalized to abs(a) = 1.0 */
Complex Cphasor();  /* (s):     return a normalized with phase s    */
Complex Cneg();     /* (a):     return (-a.real, -a.imag)           */

     /* #endif  */

double  Cabs();
double  Cphase();
void    Cprintf();

void fourn();
void fft2d ();
void fft2d_rect ();
void fft1d();
void oversamp2d();
void oversamp2dCpx();
void explodeMatrix();
void dct1d();
void dct2d();

void complexPhaseFn1();   /* phasor accumulators, col and row directions           */
void complexPhaseFn2();   /* 2-D complex phasor multiply (time domain basebanding) */


/* 
 * __________________________________________________
 *
 */

/* R C A L _ S T R I N G */

/* rcal_string functions */
void  integer_to_string_format();
void  double_to_string_format();
void  no_null_strcpy();
void  itoa();
void  reverse();
void  new_fnm_extension();
void  rcal_create_leader_filename();
void  rcal_create_trailer_filename();
void  rcal_create_dadescri_filename();


#endif
