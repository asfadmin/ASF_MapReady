static char sccsid_product_h[] =  "@(#)product.h	1.4 97/01/10 10:54:35";

/* these are indices and values for the scan results file names */

#define FRAME_MODE_DEF FRAME_MODE_N /* default is ARCTIC */

#define FRAME_MODE_N   'N'  /*    ARCTIC  == north */
#define FRAME_MODE_S   'S'  /* ANTARCTIC  == south */

#define FRAME_MODE_RL FRAME_MODE_N  /*    ARCTIC ==? right looking */
#define FRAME_MODE_LL FRAME_MODE_S  /* ANTARCTIC ==? left looking */

/* #define FRAME_MODE_RL  'S'  /*    ARCTIC  == right looking */
/* #define FRAME_MODE_LL  'N'  /* ANTARCTIC  == left looking */

#define MEDIA_TYPE_DEF MEDIA_TYPE_FILE
#define MEDIA_TYPE_FILE  'F'
#define MEDIA_TYPE_SONY  'I'
#define MEDIA_TYPE_DCRSI 'D'

/* these are indices and values for the product id */

#define PROJECTION_INDEX 10
#define PROJECTION_DEFAULT PROJECTION_STD
#define PROJECTION_GROUND  'G'
#define PROJECTION_UTM     'U'
#define PROJECTION_LAMBERT 'L'
#define PROJECTION_PS      'P'
#define PROJECTION_STD     'S'
#define PROJECTION_TERR    'T'

#define PIX_SPACE_INDEX 11
#define PIX_SPACE_DEF PIX_SPACE_100
#define PIX_SPACE_ZERO 0
#define PIX_SPACE_12_5 1
#define PIX_SPACE_25   2
#define PIX_SPACE_50   3
#define PIX_SPACE_100  4
#define PIX_SPACE_200  5
#define PIX_SPACE_400  6
#define PIX_SPACE_800  7
#define PIX_SPACE_1600 8


#define PROC_MODE_INDEX 12
#define PROC_MODE_DEF    PROC_MODE_STD
#define PROC_MODE_STD    'S'
#define PROC_MODE_CCSD   'C'
#define PROC_MODE_RAMP   'R'
#define PROC_MODE_USER   'U'
#define PROC_MODE_UNCOMP 'N'
#define PROC_MODE_QL     'Q'
#define PROC_MODE_CX     'X'

#define CAL_START_INDEX PROJECTION_INDEX /* where cal product chars start */
#define CAL_DIGIT_NUMBER '4'
#define CAL_DIGIT_LETTER 'F'


/* miscellaneous things related to file naming conventions */

#define VERSION_LEN           3     /* number of digits in version number */
#define IMAGE_FILE_EXT        "D"   /* suffix for image file "*.D" */
#define LEADER_FILE_EXT       "L"   /* suffix for leader file "*.L" */
#define AVG_FILE_EXT          "avg" /* suffix for avg file "*.avg" */
#define PMF_FILE_EXT          "M"   /* suffix for pmf file "*.M" */
#define SCAN_RESULTS_FILE_EXT "D"   /* suffix for scan results file "*.D" */
#define SCAN_RESULTS_FILE_LEN 19    /* still not nailed down by sys engr */

