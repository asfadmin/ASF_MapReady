/* platform change 6.2 to 6.4: UBSIZE changes to NBPSCTR */

#define ASP_READ_FRAME_IN_SRF 

#define CHECK_FILE_EXISTENCE  /* enable for delivery */
#define CHECK_FRAME_EXISTENCE /* enable for delivery */
#define CHECK_DISK_SPACE  /* enable for delivery */
                    /* need this commented out for testing on some machines */
                    /* when the file system does not have enough room */
                    /* for a product (this tells the CP to ignore the fact) */

#define RETURN_SCAN_RESULTS_FRAME /* if readScanResults should process frame */


#define FLEXIBLE_PPS /* */



#define FILE_TYPE_IMAGE  0
#define FILE_TYPE_PMF    1
#define FILE_TYPE_OTHER  2


static char sccsid_cpbuildMsg_c[] =  "@(#)cpbuildMsg.c 4.193 97/07/11 17:29:46";


#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <libgen.h> /* basename */
#include <string.h>
#include <unistd.h> /* gethostname */
#include <bstring.h>  /* bzero */
#include <stdlib.h>  /* atoi */
#include <sys/types.h>
#include <sys/param.h> /* MAXPATHLEN , NBPSCTR , MAXHOSTNAMELEN */
#include <netinet/in.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <syslog.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>
#include <Xm/PushBG.h>
#include <task.h>

#include "asf.h"    /* contains tuan's value_data for agg */
#include "logUtils.h" /* printfLLog */
#include "memUtils.h" /* doMalloc, doFree */
#include "asfcommon.h"
#include "cpdefines.h"
#include "cpconfig.h"
#include "listcore.h"
#include "que_sys.h"

#include "cpbuildMsg.h"
#include "cplogs.h"
#include "cprtns.h"
#include "cprtns_xwp.h"
#include "cpworkProc.h"
#include "listmstrq.h"
#include "validate.h"
#include "utils.h"

#include "product.h"

menuStateType GLOBAL_state;

long SSP2allocTime[MAX_SSP2];

#include "cpMsgs.h" /* have to include this after GLOBAL_state declaration */

static ODL readScanResults(char *filename, int requested_frame,
                    ODL writeOutODL, frameInfo *frame);
extern int setObjectVal(ODL odl, char* name, ...);
extern char *chooseCalParamsFile();
extern char getFrameModeChar();

extern const char* get_dataset(char* product_id, char *product_type, 
              char* mode, char *frame_mode, char *msg_type, char *compensated);


char getMediaTypeChar(char *mediaTypeStr)
{
  char mediaType = MEDIA_TYPE_DEF;

  mediaType = 
    (strcmp(mediaTypeStr, SPS_mediaTypes[SPS_mediaType_DCRSI]) == 0 ) ? 
       MEDIA_TYPE_DCRSI : 
    (strcmp(mediaTypeStr, SPS_mediaTypes[SPS_mediaType_SONY]) == 0 ) ? 
       MEDIA_TYPE_SONY : 
       MEDIA_TYPE_DEF;

#ifdef BUILD_DEBUG
printf("getMediaTypeChar: %s mediaType %c\n", mediaTypeStr, mediaType);
#endif

  return(mediaType);
}


int sendAck(ODL readInODL, char *namePtr, int jobId)
{
  ODL ack;
  int sockfd;

  if ((sockfd = GetSocketGivenName_PIDLIST(namePtr)) == -1) {
     printfLLog(LOG_ERR, CANNOT_GET, "process socket");
     return(0);
  }


                       /* send SUBSYSTEM_ACK message back to the subsystem */
  if (! (ack = GetAckMsg(readInODL, SUBSYSTEM_ACK))) {
    printfLLog(LOG_ERR, "can't access %s\n", SUBSYSTEM_ACK);
    return(0);
  }
  ODLSetVal(ack, ACK_HDR_SOURCE, ASF_CP);
  ODLSetVal(ack, ACK_HDR_DEST, namePtr );
  ODLSetVal(ack, ACK_BODY_JOB_ID, jobId);
  WriteMsgToClient(sockfd, ack);
  ODLFree(ack);
  return(1);
}

/*----------------------------------------------------------
 * NAME:
 *  getNumBeamsGivenMode
 *
 * DESCRIPTION:
 *   return integer number of beams given "MODE" string
 *
 * NOTES:
 *--------------------------------------------------------*/
static int getNumBeamsGivenMode(char *str)
{
printf("getNumBeamsGivenMode: %s\n", str);
  if (strcmp(str, SPS_modes[SPS_mode_SWA]) == 0 ||
      strcmp(str, SPS_modes[SPS_mode_SWB]) == 0)
    return(SS_NUM_SW_BEAMS);
  else if (strcmp(str, SPS_modes[SPS_mode_SNA]) == 0)
    return(SS_NUM_SNA_BEAMS);
  else if (strcmp(str, SPS_modes[SPS_mode_SNB]) == 0)
    return(SS_NUM_SNB_BEAMS);
  else if (strncmp(str, SPS_modeStr_ST, 2) == 0)
    return(SS_NUM_ST_BEAMS);
  else if (strncmp(str, SPS_modeStr_EH, 2) == 0)
    return(SS_NUM_EH_BEAMS-1);
  else /* unknown mode */
    return(NOT_SET_ID);

} /* getNumBeamsGivenMode */

/*----------------------------------------------------------
 * NAME:
 *  getNumCalProducts
 *
 * DESCRIPTION:
 *   return integer number of beams given mode and type strings
 *
 * NOTES:
 *--------------------------------------------------------*/
static int getNumCalProducts(char *mode, char *type, char *compensated)
{
       /* only CAL_SET will generate these products the CP needs to look for */
  if (strcmp(type, SPS_productTypes[SPS_product_CAL_SET]) != 0)
    return(0);
  if (strcmp(mode, SPS_modes[SPS_mode_SWA]) == 0 ||
      strcmp(mode, SPS_modes[SPS_mode_SWB]) == 0)
    return(SS_NUM_SW_BEAMS-1);
  else if (strcmp(mode, SPS_modes[SPS_mode_SNA]) == 0)
    return(SS_NUM_SNA_BEAMS-1);
  else if (strcmp(mode, SPS_modes[SPS_mode_SNB]) == 0)
    return(SS_NUM_SNB_BEAMS-1);
  else if (strncmp(mode, SPS_modeStr_ST, 2) == 0)
    return(SS_NUM_ST_BEAMS-1);
  else if (strncmp(mode, SPS_modeStr_EH, 2) == 0)
    return(SS_NUM_EH_BEAMS-1);
  else /* unknown mode */
    return(NOT_SET_ID);

} /* getNumCalProducts */



/*----------------------------------------------------------
 * NAME:
 *  isPP
 *
 * DESCRIPTION:
 *   return true if job is candidate for PP
 *
 * NOTES:
 *--------------------------------------------------------*/
static int isPP(char *mode)
{
  int modeId = getModeId(mode), isOne = False;

  if (modeId == MODE_EXT_HIGH_ID || modeId == MODE_EXT_LOW_ID ||
      modeId == MODE_FINE_ID || modeId == MODE_WIDE_ID) 
      isOne = True;

#ifdef BUILD_DEBUG
printf("isPP: mode %s returning %d\n", mode, isOne);
#endif
  return(isOne);

} /* isPP */



/*----------------------------------------------------------
 * NAME:
 *  buildCalProductId
 *
 * DESCRIPTION:
 *  returns the product id for calibration products
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

static char *buildCalProductId(int pvsProdNum, char *productId)
{
  int calStart=CAL_START_INDEX;
  char *p;

#ifdef BUILD_DEBUG
printf("buildCalProductId: prodnum %d, incoming productId %s\n", pvsProdNum, productId);
#endif
  if (pvsProdNum < 0 || pvsProdNum > MAX_CAL_PRODUCTS || productId == NULL)
    return(NULL);

  if (strlen(productId) < 15) /*  */
    return(NULL);

    /* get pointer to start of actual product id within full pathname */
  p = (p = strrchr(productId, '/')) ? p+1 : productId;

     /* these three lines assume that the three 'special' digits  */
     /* in the cal product id are sequential... if not, change calStart */

  p[calStart++] = pvsProdNum + 0x30; /* make it a char not int*/
  p[calStart++] = CAL_DIGIT_NUMBER;
  p[calStart++] = CAL_DIGIT_LETTER;

#ifdef BUILD_DEBUG
printf("buildCalProductId: returning productId %s\n", productId);
#endif
  return(productId);

} /* buildCalProductId */

/*----------------------------------------------------------
 * NAME:
 *  buildCalProdNameGivenImage
 *
 * DESCRIPTION:
 *  returns the pvs product name given the main image name
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *buildCalProdNameGivenImage(int pvsProdNum, char *imageName)
{
  int pathLen;  /* offset into file name when a path is present */
  char *pvsName, *p;

  if (pvsProdNum < 0 || pvsProdNum > MAX_CAL_PRODUCTS || imageName == NULL)
    return(NULL);

  p = (p = strrchr(imageName, '/')) ? p+1 : imageName; /* strip path off old */
  pathLen = strlen(imageName) - strlen(p);

  pvsName = doMalloc(strlen(imageName)+1);
  strcpy(pvsName, imageName);


  pvsName[pathLen+10] = pvsProdNum + 0x30; /* make it a char not int*/
  pvsName[pathLen+11] = CAL_DIGIT_NUMBER;
  pvsName[pathLen+12] = CAL_DIGIT_LETTER;

#ifdef BUILD_DEBUG
printf("\tbuildCalProdNameGivenImage returning %s\n", pvsName);
#endif
  return(pvsName);

} /* buildCalProdNameGivenImage */


/*----------------------------------------------------------
 * NAME:
 *  GetIMSmsgTypeAsText
 *
 * DESCRIPTION:
 *  an access function that maps the master queue status to a
 *  text string and returns a pointer to that string
 *
 * NOTES:
 *  VISIBLE -- for evaluation
 *
 *---------------------------------------------------------*/
char *GetIMSmsgTypeAsText(int status)
{
 char *ptr;

    ptr = doMalloc(MAX_IMS_MSG_TEXT);
    switch(status) {
      case IMS_TYPE_GET_VERSION:
        strcpy(ptr, IMS_TYPE_GET_VERSION_TEXT);
        break;
      case IMS_TYPE_GET_SCAN_RESULT:
        strcpy(ptr, IMS_TYPE_GET_SCAN_RESULT_TEXT);
        break;
      case IMS_TYPE_GET_CAL_PARAMS:
        strcpy(ptr, IMS_TYPE_GET_CAL_PARAMS_TEXT);
        break;
      case IMS_TYPE_STORE_CAL_PRODUCTS:
        strcpy(ptr, IMS_TYPE_STORE_CAL_PRODUCTS_TEXT);
        break;
      case IMS_TYPE_STORE_PRODUCTS:
        strcpy(ptr, IMS_TYPE_STORE_PRODUCTS_TEXT);
        break;
      case IMS_TYPE_STORE_SCAN_RESULT:
        strcpy(ptr, IMS_TYPE_STORE_SCAN_RESULT_TEXT);
        break;
      case IMS_TYPE_STORE_SCAN_METADATA:
        strcpy(ptr, IMS_TYPE_STORE_SCAN_META_TEXT);
        break;
      default:
        strcpy(ptr, "UNDEFINED");
        break;
      }
      return(ptr);
} /* GetIMSmsgTypeAsText */


/*----------------------------------------------------------
 * NAME:
 *  getExpectedTime
 *
 * DESCRIPTION:
 *  returns expected processing time of a product in seconds
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
#define SINGLE_TIME 1L
#define DOUBLE_TIME 2L

long getExpectedTime(char *productType, char *compensated)
{
  int isCompensated=False, isUncompensated=False, isStandard=False;

  isCompensated = strcmp(compensated, "YES") == 0;
  isUncompensated = strcmp(compensated, "NO") == 0;
  isStandard = strcmp(productType, 
                      SPS_productTypes[SPS_product_STD]) == 0;

  return(isStandard? SINGLE_TIME : DOUBLE_TIME);

} /* getExpectedTime */

/*----------------------------------------------------------
 * NAME:
 *  getSSP2Time
 *
 * DESCRIPTION:
 *  returns expected processing time of a product in seconds
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
#define SINGLE_TIME 1L
#define DOUBLE_TIME 2L

static long getSSP2Time(int whichSSP2)
{
  return(SSP2allocTime[whichSSP2]);

} /* getSSP2Time */

/*----------------------------------------------------------
 * NAME:
 *  decrementSSP2Time
 *
 * DESCRIPTION:
 *  modify expected processing time of a ssp2
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void decrementSSP2Time(int whichSSP2, long productTime)
{
#ifdef ASSIGN_DEBUG
printf("decrementSSP2Time: decrementing SSP2 %d product time by %d\n", productTime);
#endif
  SSP2allocTime[whichSSP2] -= productTime;

} /* decrementSSP2Time */


/*----------------------------------------------------------
 * NAME:
 *  getExpectedSize
 *
 * DESCRIPTION:
 *  returns expected size of a product in blocks
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

/* these #defines are in bytes.  will be converted to blocks when returned */

#define SSP_DECODE_SIZE 1200000000  
#define PP_DECODE_SIZE   500000000  

#define CSD_SIZE     305100000
#define COMPLEX_SIZE 107400000
#define CTS_12_5_SIZE 69000000
#define CTS_100_SIZE 1300000

#define SS_5BEAM_50_SIZE  124000000
#define SS_3BEAM_50_SIZE   46000000
#define SS_5BEAM_100_SIZE  30000000
#define SS_3BEAM_100_SIZE  12000000
#define SS_5BEAM_400_SIZE   2000000
#define SS_3BEAM_400_SIZE    860000

#define SS_GEO_5BEAM_50_SIZE  238894272
#define SS_GEO_3BEAM_50_SIZE   86713536
#define SS_GEO_5BEAM_100_SIZE  60464832
#define SS_GEO_3BEAM_100_SIZE  22123200
#define SS_GEO_5BEAM_400_SIZE   4057152
#define SS_GEO_3BEAM_400_SIZE   1549632

#define PP_FF_COMPLEX_SIZE 1074000000
#define PP_RAMP_SIZE         69000000


static long getExpectedSize(ODL odl, long *decodeSize)
{
  int isCts=False, isSSP=False, isaPP=False;
  int isComplex=False, isCsd=False, isRamp=False;
  int isCompensated=False, isUncompensated=False;
  int is5beam=False, is3beam=False;
  long size=-1;

  *decodeSize=0;
  isComplex = strcmp(GLOBAL_state.productT_buf, 
                       SPS_productTypes[SPS_product_CX]) == 0;
  isRamp = strcmp(GLOBAL_state.productT_buf, 
                   SPS_productTypes[SPS_product_RAMP]) == 0;
  if (isCts = (getModeId(GLOBAL_state.mode_buf) == MODE_CONTINUOUS_ID)) {
    isCsd = strcmp(GLOBAL_state.productT_buf, 
                   SPS_productTypes[SPS_product_CCSD]) == 0;
  }
  else if (getModeId(GLOBAL_state.mode_buf) == MODE_SCANSAR_ID) {
        isSSP = True;

    isCompensated = strcmp(GLOBAL_state.compensated, "YES") == 0;
    isUncompensated = strcmp(GLOBAL_state.compensated, "NO") == 0;

    is5beam = strcmp(GLOBAL_state.mode_buf, SPS_modes[SPS_mode_SWA]) == 0 ||
              strcmp(GLOBAL_state.mode_buf, SPS_modes[SPS_mode_SWB]) == 0 ;
    is3beam = strcmp(GLOBAL_state.mode_buf, SPS_modes[SPS_mode_SNA]) == 0 ||
              strcmp(GLOBAL_state.mode_buf, SPS_modes[SPS_mode_SNB]) == 0 ;
  }
  else if (isPP(GLOBAL_state.mode_buf))
    isaPP = True;
  else
    return(size);  /* size is still -1 at this point */

#ifdef BUILD_DEBUG
printf("\ngetExpectedSize: isCts %d isCsd %d isComplex %d ", isCts, isCsd, isComplex);
printf("isSSP %d is5beam %d is3beam %d pixelSpacing %lf\n", isSSP , is5beam, is3beam, GLOBAL_state.pixelSpacing);
printf("isaPP %d isRamp %d isCompensated %d isUncompensated %d\n", isaPP , isRamp, isCompensated, isUncompensated);
#endif
  if ( GLOBAL_state.subframe_id == 0 ) { 
       isCts = 0;
       isaPP = True;
  }
  if (isCts ) {
    if (isCsd)
      size = CSD_SIZE;
    else if (isComplex) {
         if ( GLOBAL_state.subframe_id == 0 )  
             size = PP_FF_COMPLEX_SIZE;
         else
             size = COMPLEX_SIZE;
    }
    else
      if (GLOBAL_state.pixelSpacing == 100.0)
        size = CTS_100_SIZE;
      else
        size = CTS_12_5_SIZE;
  }
  else if (isSSP) {   
      if (GLOBAL_state.pixelSpacing == 50.0) {
        if (is5beam)
          size = SS_GEO_5BEAM_50_SIZE;
        else if (is3beam)
          size = SS_GEO_3BEAM_50_SIZE;
      }
      else if (GLOBAL_state.pixelSpacing == 100.0) {
        if (is5beam)
          size = SS_GEO_5BEAM_100_SIZE;
        else if (is3beam)
          size = SS_GEO_3BEAM_100_SIZE;
      }
      else if (GLOBAL_state.pixelSpacing == 400.0) {
        if (is5beam)
          size = SS_GEO_5BEAM_400_SIZE;
        else if (is3beam)
          size = SS_GEO_3BEAM_400_SIZE;
      }
      *decodeSize = SSP_DECODE_SIZE;
  }
  else if (isaPP) {
      if (GLOBAL_state.subframe_id == 0 && isComplex)
        size = PP_FF_COMPLEX_SIZE;
      else if (isRamp)
        size = PP_RAMP_SIZE;
      else { /* assume the rest is EH low-res and full-res */
        if (GLOBAL_state.pixelSpacing == 100.0)
          size = CTS_100_SIZE;
        else if (GLOBAL_state.pixelSpacing == 12.5)
          size = CTS_12_5_SIZE;
      }
      *decodeSize = PP_DECODE_SIZE;
  }

  if (size != -1)
    size /= NBPSCTR; /* divide by user block size to get # blocks */
#ifdef BUILD_DEBUG
printf("getExpectedSize: returning %ld\n", size);
#endif

#ifdef CHECK_DISK_SPACE
  return(size);
#else
  return(1);
#endif

} /* getExpectedSize */

/*----------------------------------------------------------
 * NAME:
 *  assignSSP
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *assignSSP(char *productType, char *compensated, int *isSSP2)
{
#ifdef ROUND_ROBIN
  static int lastSSP2 = 0;
#else
  long minTime = LONG_MAX, productTime = 0;
  long ssp2Time = 0;
  int i, chosenSSP2 = 0;
#endif

/* if there is an ssp1, send to it; otherwise loop through the
   ssp2s in sequence */

#ifdef ASSIGN_DEBUG
printf("assignSSP: productType %s\n", productType);
#endif

  *isSSP2 = 0;

#ifdef ROUND_ROBIN
  if (lastSSP2 >= getNumSSP2s())
    lastSSP2 = 0;
  if (getNumSSP2s()) {
    *isSSP2 = 1;
    return(getSSP2name(lastSSP2++));
  }
#else
  productTime = getExpectedTime(productType, compensated);
  for (i=0; i < getNumSSP2s(); i++) {
    ssp2Time = getSSP2Time(i);
#ifdef ASSIGN_DEBUG
printf("SSP2 %d product time %ld ssp2Time %ld\n", i, productTime, ssp2Time);
#endif
    if (ssp2Time < minTime) {    /* pick SSP2 with smallest time allocated */
      chosenSSP2 = i;
      minTime = ssp2Time;
    }
  }
  if (getNumSSP2s()) {
    *isSSP2 = 1;
    SSP2allocTime[chosenSSP2] += productTime;
    return(getSSP2name(chosenSSP2));
  }
#endif

  return(NULL);

} /* end assignSSP */

/*----------------------------------------------------------
 * NAME:
 *  assignDir
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *assignDir(char * (*fn)(), long expectedSize)
{
  long maxSize=-1, thisSize=0;
  int whichDir=0, bestChoice = 0;
  int count=0;

#ifdef BUILD_DEBUG
printf("assignDir: required size %d\n", expectedSize); 
#endif

  for (; (*fn)(whichDir) && count < 10; whichDir++ , count++) {
    thisSize = getDirFree((*fn)(whichDir) );
    if (thisSize > maxSize) {
      maxSize = thisSize;
      bestChoice = whichDir;
    }
  }
#ifdef BUILD_DEBUG
printf("assignDir best choice %d max available %ld\n", bestChoice, maxSize); 
#endif
  if (maxSize > expectedSize)
    return((*fn)(bestChoice));
  else
    return(NULL);

} /* end assignDir */


/*----------------------------------------------------------
 * NAME:
 *  assignSSP2inputDir
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *assignSSP2inputDir(int whichSSP, long expectedSize)
{
  long maxSize=-1, thisSize=0;
  int whichDir=0, bestChoice = 0;
  char *dirName;

#ifdef BUILD_DEBUG
printf("assignSSP2inputDir: required size %d\n", expectedSize);
#endif

  printfLLog(LOG_DEBUG, EXPECTED_PRODUCT_SIZE, getSSP2name(whichSSP), "input", 
             expectedSize);

  if (expectedSize < 0)
    return(NULL);

  for (; dirName = getSSP2inputDirName(whichSSP, whichDir); whichDir++ ) {
    printfLLog(LOG_DEBUG, CHECKING_DISK_SPACE, dirName);

    thisSize = getDirFree(getSSP2inputDirName(whichSSP, whichDir) );
    if (thisSize > maxSize) {
      maxSize = thisSize;
      bestChoice = whichDir;
    }
  }
#ifdef BUILD_DEBUG
printf("ssp2 input dir best choice %d max size available %ld size required %ld\n", whichDir, maxSize, expectedSize);
#endif
  if (maxSize > expectedSize)
    return(getSSP2inputDirName(whichSSP, bestChoice));
  else {
    printfLLog(LOG_ERR, NO_DISK_SPACE, getSSP2name(whichSSP),  "input" );
    return(NULL);
  }


    /*return(getSSP2inputDirName(whichOne, whichDir)); */

} /* end assignSSP2inputDir */

/*----------------------------------------------------------
 * NAME:
 *  assignSSP2outputDir
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *assignSSP2outputDir(int whichSSP, long expectedSize)
{
  long maxSize=-1, thisSize=0;
  int whichDir=0, bestChoice = 0;
  char *dirName;

#ifdef BUILD_DEBUG
printf("assignSSP2outputDir: size %d\n", expectedSize);
#endif

  printfLLog(LOG_DEBUG, EXPECTED_PRODUCT_SIZE, getSSP2name(whichSSP), "output", 
             expectedSize);

  if (expectedSize < 0)
    return(NULL);

  for (; dirName = getSSP2outputDirName(whichSSP, whichDir); whichDir++ ) {
    printfLLog(LOG_DEBUG, CHECKING_DISK_SPACE, dirName);
    thisSize = getDirFree(getSSP2outputDirName(whichSSP, whichDir) );
    if (thisSize > maxSize) {
      maxSize = thisSize;
      bestChoice = whichDir;
    }
  }
#ifdef BUILD_DEBUG
printf("ssp2 output dir best choice %d max size available %ld\n", whichDir, maxSize);
#endif
  if (maxSize > expectedSize)
    return(getSSP2outputDirName(whichSSP, bestChoice));
  else {
    printfLLog(LOG_ERR, NO_DISK_SPACE, getSSP2name(whichSSP),  "output" );
    return(NULL);
  }

    /*return(getSSP2outputDirName(whichOne, whichDir)); */

} /* end assignSSP2outputDir */

/*----------------------------------------------------------
 * NAME:
 *  assignASPoutputDir
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *assignASPoutputDir(long expectedSize)
{
  long maxSize=-1, thisSize=0;
  int whichDir=0, bestChoice = 0;
  char *dirName;

  printfLLog(LOG_DEBUG, EXPECTED_PRODUCT_SIZE, getASPname(), "output",
             expectedSize);

  if (expectedSize < 0)
    return(NULL);

  for (; dirName = getASPoutputDirName(whichDir); whichDir++ ) {
    printfLLog(LOG_DEBUG, CHECKING_DISK_SPACE, dirName);
    thisSize = getDirFree(getASPoutputDirName(whichDir) );
    if (thisSize > maxSize) {
      maxSize = thisSize;
      bestChoice = whichDir;
    }
  }
  if (maxSize > expectedSize) 
    return(getASPoutputDirName(bestChoice));
  else  {
    printfLLog(LOG_ERR, NO_DISK_SPACE, getASPname(),   "output" );
    return(NULL);
  }

  /* return(getASPoutputDirName(whichDir)); */

} /* end assignASPoutputDir */

/*----------------------------------------------------------
 * NAME:
 * getDataFilesHost
 *
 * DESCRIPTION:
 *  get the host name that the data files will reside on
 *
 * NOTES:
 *  for this implementation assume that it will be same as
 *  the CP. Also, only get it once.
 *
 *---------------------------------------------------------*/
static char *getDataFilesHost(int hostDiff,char *namePtr)
{
 int retval;
 static char dataFilesHostBuf[MAXHOSTNAMELEN];

     
    if (hostDiff == 1)
       retval = getRDSHost(dataFilesHostBuf,namePtr);
    else
       retval = gethostname(dataFilesHostBuf, sizeof(dataFilesHostBuf));


  return(dataFilesHostBuf);

} /* end getDataFilesHost .................................*/

/*----------------------------------------------------------
 * NAME:
 *  mkImageDir
 *
 * DESCRIPTION:
 *   checks for the existence of image subdirectories
 *   and creates appropriate subdirectories if they do not exist
 *
 * NOTES:
 * returns nonzero on success, zero on error
 *
 *---------------------------------------------------------*/

#define DIR_MASK S_IRWXU + S_IRWXG + S_IRWXO

static int mkImageDir(char *pathName, char *satName, int jobId, 
                      subsysFilesType *sf)
{
  int stat, topDirExists = 0, lowerDirExists = 0;
  char wholeDir[MAXPATHLEN], topDir[MAXPATHLEN];

#ifdef BUILD_DEBUG
printf("mkImageDir: path %s sat %s jobid %d\n", pathName, satName, jobId);
#endif
  sprintf(topDir, "%s/%s", pathName, satName);
  sprintf(wholeDir, "%s/%s/%d", pathName, satName, jobId);

  if (!(topDirExists = dirExists(topDir))) {
    printfLLog(LOG_DEBUG, CREATING_DIR, topDir);
    stat = mkdir(topDir, DIR_MASK);
    topDirExists = stat ? 0 : 1; /* stat != 0 means error */
  }

  if (topDirExists && !(lowerDirExists = dirExists(wholeDir)) ) {
    printfLLog(LOG_DEBUG, CREATING_DIR, wholeDir);
    stat = mkdir(wholeDir, DIR_MASK);
    lowerDirExists = stat ? 0 : 1; /* stat != 0 means error */
  }
  if (lowerDirExists)
    strcpy(sf->dirName[sf->numDirs++], wholeDir);
  if (topDirExists)
    strcpy(sf->dirName[sf->numDirs++], topDir);

  return(lowerDirExists);
} /* mkImageDir */

/*----------------------------------------------------------
 * NAME:
 *  mkScanResultsDirName
 *
 * DESCRIPTION:
 *   checks for the existence of image subdirectories
 *   and creates appropriate subdirectories if they do not exist
 *
 * NOTES:
 * returns nonzero on success, zero on error
 *
 *---------------------------------------------------------*/

static char *mkScanResultsDirName(char *pathName, char *satName, int jobId,
   char* namePtr)
{
  char *wholeDir = doMalloc(MAXPATHLEN);
  char *topDir = doMalloc(MAXPATHLEN);

#ifdef BUILD_DEBUG
printf("mkScanResultsDir: path %s sat %s jobid %d\n", pathName, satName, jobId);
#endif
  sprintf(topDir, "%s/%s", pathName, satName);
  sprintf(wholeDir, "%s:%s/%s/%d", getDataFilesHost(2,namePtr), pathName, satName, jobId);

  doFree(topDir);
  return(wholeDir);

} /* mkScanResultsDirName */



/*----------------------------------------------------------
 * NAME:
 *  getReqMsgValues
 *
 * DESCRIPTION:
 *   read incoming request message values.
 *   on success, returns the value of the message type (a positive number)
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int getReqMsgValues(ODL ODLreqMsg)
{
  int i, msgTypeId, separate_tc = FALSE;
  int ok = 1;
  char *errmsg, *msgType;

  bzero(&GLOBAL_state, sizeof(GLOBAL_state)); 

  ok = ODLGetVal(ODLreqMsg, HDR_MSG_TYPE, &msgType); 
#ifdef BUILD_DEBUG
  printf("getReqMsgValues msgtype %s.\n", msgType);
#endif

  if (strcmp(msgType, "SCAN_REQUEST") == 0) {
    separate_tc = TRUE;
    msgTypeId = SCAN_REQUEST_ID;
    for (i=0,ok=1; ok&& scan_request[i].str != NULL; ) {
#ifdef CHECK_DEBUG
printf("checking scan_request value of %s\n", scan_request[i].str);
#endif
      if ((ok= ODLGetVal(ODLreqMsg, errmsg = scan_request[i].str,
         scan_request[i].ptr)  == 1) )  {
         i++;
#ifdef BUILD_DEBUG
printGlobalState();
#endif
      }
      else {
        printfLLog(LOG_DEBUG, "ODLGetVal failed for %s\n", errmsg); 
        ok = 0;
      }
    }
  }
  else if (strcmp(msgType, "FRAME_REQUEST") == 0) {
    separate_tc = TRUE;
    msgTypeId = FRAME_REQUEST_ID;
    for (i=0,ok=1; ok&& frame_request[i].str != NULL; ) {
#ifdef CHECK_DEBUG
printf("checking frame_request value of %s\n", frame_request[i].str);
printGlobalState();
#endif
      if ((ok= ODLGetVal(ODLreqMsg, errmsg = frame_request[i].str,
         frame_request[i].ptr)  == 1) )  {
         i++;
      }
      else {
        printfLLog(LOG_DEBUG, "ODLGetVal failed for %s\n", errmsg); 
        ok = 0;
      }
    }


/* now add some stuff to account for the fact that pps is */
/* sending us "flexible format" messages for frame requests. :-p */

#ifdef FLEXIBLE_PPS
    if (ok && strcmp(GLOBAL_state.productT_buf, 
                     SPS_productTypes[SPS_product_CX]) == 0) {
      for (i=0,ok=1; ok&& frame_request_flexible_subframe[i].str != NULL; ) {
#ifdef CHECK_DEBUG
printf("checking frame_request_flexible_subframe value of %s\n", frame_request_flexible_subframe[i].str);
printGlobalState();
#endif
        if ((ok= ODLGetVal(ODLreqMsg, 
              errmsg = frame_request_flexible_subframe[i].str,
              frame_request_flexible_subframe[i].ptr)  == 1) )  {
           i++;
        }
        else {
          printfLLog(LOG_DEBUG, "ODLGetVal failed for %s\n", errmsg); 
          ok = 0;
        }

      } 
    } /* flexible subframe */
    else { /* not complex so we need to fake out the subframe because */
           /* pps is not taking care of this when we return the msgs */
      GLOBAL_state.subframe_id = 1;
    }

    if (ok && strcmp(GLOBAL_state.projection_buf, "UTM") == 0) {
      for (i=0,ok=1; ok&& frame_request_flexible_utm[i].str != NULL; ) {
#ifdef CHECK_DEBUG
printf("checking frame_request_flexible_utm value of %s\n", frame_request_flexible_utm[i].str);
printGlobalState();
#endif
        if ((ok= ODLGetVal(ODLreqMsg, 
              errmsg = frame_request_flexible_utm[i].str,
              frame_request_flexible_utm[i].ptr)  == 1) )  {
           i++;
        }
        else {
          printfLLog(LOG_DEBUG, "ODLGetVal failed for %s\n", errmsg); 
          ok = 0;
        }

      } 
    } /* flexible utm */

    if (ok) {
      if (strcmp(GLOBAL_state.dtm_satBuf, SPS_platforms[SPS_platform_R1]) != 0) {
          for (i=0,ok=1; ok&& request_flexible_tc[i].str != NULL; ) {
#ifdef CHECK_DEBUG
printf("checking request_flexible_tc value of %s\n", request_flexible_tc[i].str);
printGlobalState();
#endif
            if ((ok= ODLGetVal(ODLreqMsg, errmsg = request_flexible_tc[i].str,
              request_flexible_tc[i].ptr)  == 1) )  {
             i++;
            }
            else {
              printfLLog(LOG_DEBUG, "ODLGetVal failed for %s\n", errmsg); 
              ok = 0;
            }
  
          }  /* process all flexible tc strings */
        }
        else { /* zero out tc items for radarsat jobs.  */
          GLOBAL_state.time_corr_rev = 0;
          GLOBAL_state.sat_time = 0;
          GLOBAL_state.clock_cycle = NULL;
/*
          bzero((char *) GLOBAL_state.time_corr_time_val , 
                sizeof(GLOBAL_state.time_corr_time_val));
*/
        }
    } /* ok */



#endif  /* FLEXIBLE_PPS */

#ifdef BUILD_DEBUG
printf("after checking frame_request \n");
printGlobalState();
#endif
  }


#ifdef FLEXIBLE_PPS
  if (ok && separate_tc) {
    if (strcmp(GLOBAL_state.dtm_satBuf, SPS_platforms[SPS_platform_R1]) != 0) {
      for (i=0,ok=1; ok&& request_flexible_tc[i].str != NULL; ) {
#ifdef CHECK_DEBUG
printf("checking request_flexible_tc value of %s\n", request_flexible_tc[i].str)
;
printGlobalState();
#endif
        if ((ok= ODLGetVal(ODLreqMsg,
              errmsg = request_flexible_tc[i].str,
              request_flexible_tc[i].ptr)  == 1) )  {
           i++;
        }
        else {
          printfLLog(LOG_DEBUG, "ODLGetVal failed for %s\n", errmsg); 
          ok = 0;
        }

      }
    } /* flexible tc */
  }
#endif

#ifdef GLOBAL_FILE_VERSION
/* set file version to the value retrieved from ims */
  if (ok) {
    GLOBAL_state.fileVersion = GetFileVersion_MSTRQLIST(GLOBAL_state.jobId);
    if (GLOBAL_state.fileVersion == -1) {
      errmsg = "Error getting file version\n";
      printfLLog(LOG_DEBUG,"GetFileVersion_MSTRQLIST failed for %s\n", errmsg);
      ok = 0;
    }
  }
#endif

  if (!ok) {
    printfLLog(LOG_ERR, ERROR_GETTING_REQ);
    printfLLog(LOG_DEBUG, DEB_ERROR_GETTING_REQ, "incoming", errmsg);
    return(-1);
  }

  return(msgTypeId);

} /* end getReqMsgValues.......................................*/



/*----------------------------------------------------------
 * NAME:
 *  setUpCommonReqMsgValues
 *
 * DESCRIPTION:
 *   set common fields for an outgoing ODL message 
 *   
 *
 * NOTES:
 *  this routine is ONLY CALLED from the X thread
 *---------------------------------------------------------*/
static int setUpCommonReqMsgValues(char *namePtr, ODL writeOutODL, int jobid)
{
  int ok = 0;
  char *errmsg, *objectName;
  ODL *tmpODL;

  tmpODL = Val(writeOutODL);
  objectName = Name(tmpODL[0]);
#ifdef BUILD_DEBUG
printf("in setupcommon... object name %s jobid %d name %s \n", objectName, jobid, namePtr);
/* printf("and odl is %s\n", ODLToStr(writeOutODL, NULL) );  */
#endif
   errmsg = doMalloc(200);
    sprintf(errmsg, "%s.%s", objectName, SOURCE); 
    if (setObjectVal(writeOutODL, SOURCE, ASF_CP)) { 
      sprintf(errmsg, "%s.%s", objectName, DESTINATION); 
      if (ODLSetVal(writeOutODL, errmsg, namePtr)) {
        sprintf(errmsg, "%s.%s", objectName, BODY_JOB_ID); 
          if (ODLSetVal(writeOutODL, errmsg, jobid)) 
            ok = 1;  /* if got to here all were written successfully */
      }
    }

  if (!ok) {
   printfLLog(LOG_ERR, CANNOT_SET, errmsg);
   return(-1);
  }
/*   ODLFree(tmpODL); */
  doFree(errmsg); 
  return(0);
}

 /* end setUpCommonReqMsgValues........................*/

/*----------------------------------------------------------
 * NAME:
 *  buildProductId
 *
 * DESCRIPTION:
 *   build prefix of product id name (not including directory path)
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

static char *buildProductId(int version, int jobId, int *qcType)
{
 char *fileNameBuf = doMalloc(MAXPATHLEN);
 char projection = PROJECTION_GROUND, procMode=PROC_MODE_STD;
 int modeId, pixelSpacing = 9; /* default is 9 */
 int isCts=False, isComplex=False, isCsd=False;
 int isaPP=False, isSSP=False, isRamp=False;
 int isQuickLook=False, isCompensated=False, isUncompensated=False;

#ifdef BUILD_DEBUG
printf("buildProductId: version %d jobid %d qctype %d\n", version, jobId, *qcType);
printGlobalState();
#endif

  bzero(fileNameBuf, MAXPATHLEN);

/* now we can have compensated/uncompensated ASP or SSP jobs */
/* check for pvs product types */

  isQuickLook = strcmp(GLOBAL_state.quicklook, "YES") == 0;
  isCompensated = strcmp(GLOBAL_state.compensated, "YES") == 0;
  isUncompensated = strcmp(GLOBAL_state.compensated, "NO") == 0;

/* check for asp product types */

  modeId = getModeId(GLOBAL_state.mode_buf);
  if (modeId == MODE_CONTINUOUS_ID) {
    isCts = True;
    isCsd = strcmp(GLOBAL_state.productT_buf,
                   SPS_productTypes[SPS_product_CCSD]) == 0;
    isComplex = strcmp(GLOBAL_state.productT_buf, 
                         SPS_productTypes[SPS_product_CX]) == 0;
    isRamp = strcmp(GLOBAL_state.productT_buf,
                   SPS_productTypes[SPS_product_RAMP]) == 0;
  }
  else if (getModeId(GLOBAL_state.mode_buf) == MODE_SCANSAR_ID)  {
        isSSP = True;
  }
  else if (isPP(GLOBAL_state.mode_buf)) {
    isaPP = True;
    isCts = True;
    isComplex = strcmp(GLOBAL_state.productT_buf,
                         SPS_productTypes[SPS_product_CX]) == 0;
    isRamp = strcmp(GLOBAL_state.productT_buf,
                   SPS_productTypes[SPS_product_RAMP]) == 0;
  }







#ifdef BUILD_DEBUG
printf("buildProductId: isCts %d isCsd %d isComplex %d ", isCts, isCsd, isComplex);
printf("isSSP %d pixelSpacing %lf\n", isSSP , GLOBAL_state.pixelSpacing);
printf("isaPP %d isRamp %d isCompensated %d isUncompensated %d\n", isaPP , isRamp, isCompensated, isUncompensated);
#endif


/* first check terrain corrected because frank said to */

  if (isSSP && strcmp(GLOBAL_state.terrain_correction_buf, "YES") == 0)
    projection = PROJECTION_TERR;

/* then set data projection */

  else if (GLOBAL_state.projection_buf) { /* only set for frame req */
    if (strcmp(GLOBAL_state.projection_buf, "PS") == 0) /* both ssp,asp */
        projection = PROJECTION_PS;
    else if (isSSP) {   /* process ssp-specific user choices */
      if (strcmp(GLOBAL_state.projection_buf, "ATCT") == 0)
        projection = PROJECTION_GROUND;
      else if (strcmp(GLOBAL_state.projection_buf, "UTM") == 0)
        projection = PROJECTION_UTM;
      else if (strcmp(GLOBAL_state.projection_buf, "PS") == 0)
        projection = PROJECTION_PS;
      else if (strcmp(GLOBAL_state.projection_buf, "LAMBERT") == 0)
        projection = PROJECTION_LAMBERT;
      else if (strcmp(GLOBAL_state.projection_buf, "GROUND_RANGE") == 0)
        projection = PROJECTION_GROUND;
      else
        projection = PROJECTION_GROUND;
    } /* ssp */
    else if (isCts) {   /* process asp-specific user choices */
      if (isComplex)  
        projection = GLOBAL_state.subframe_id + 0x30; /* make it a char not int*/
      else if (isCsd)
        projection = PROJECTION_STD;
      else if (strcmp(GLOBAL_state.projection_buf, "SLANT_RANGE") == 0)
        projection = PROJECTION_STD;
      else
        projection = PROJECTION_GROUND;  /* default */
    }
  }

/* now set pixel spacing */
  if (isComplex || isCsd)  
      pixelSpacing = PIX_SPACE_ZERO;
  else if (GLOBAL_state.pixelSpacing == 12.5)
      pixelSpacing = PIX_SPACE_12_5;
  else if (GLOBAL_state.pixelSpacing == 25.0)
      pixelSpacing = PIX_SPACE_25;
  else if (GLOBAL_state.pixelSpacing == 50.0)
      pixelSpacing = PIX_SPACE_50;
  else if (GLOBAL_state.pixelSpacing == 100.0)
      pixelSpacing = PIX_SPACE_100;
  else if (GLOBAL_state.pixelSpacing == 200.0)
      pixelSpacing = PIX_SPACE_200;
  else if (GLOBAL_state.pixelSpacing == 400.0)
      pixelSpacing = PIX_SPACE_400;
  else if (GLOBAL_state.pixelSpacing == 800.0)
      pixelSpacing = PIX_SPACE_800;
  else if (GLOBAL_state.pixelSpacing == 1600.0)
      pixelSpacing = PIX_SPACE_1600;

/* set processor mode */
  if (isCts) {           /* process asp-specific user choices */
    if (isCsd)
      procMode = PROC_MODE_CCSD;       /* ccsd */
    else if (isComplex)
      procMode = PROC_MODE_CX;      /* complex */
    else if (isQuickLook) 
      procMode = PROC_MODE_QL;      /* quick look */
    else if (isRamp) 
      procMode = PROC_MODE_RAMP;
    else if (isUncompensated)
      procMode = PROC_MODE_UNCOMP;      /* uncompensated: no radiometric correction */
    else if (GLOBAL_state.processingGain != DEF_PROCESSING_GAIN) 
      procMode = PROC_MODE_USER;      /* user-specified processing gain */
    else
      procMode = PROC_MODE_DEF;      /* default -- applies to both "GROUND_RANGE" and
			      "SLANT_RANGE" projections */
  } else if (isSSP) {    /* process ssp-specific user choices */
    if (isQuickLook)
      procMode = PROC_MODE_QL;      /* quick look */
    else if (isUncompensated)
      procMode = PROC_MODE_UNCOMP;      /* uncompensated: no radiometric correction */
    else if (GLOBAL_state.processingGain != DEF_PROCESSING_GAIN)
      procMode = PROC_MODE_USER;      /* user-specified processing gain */
    else
      procMode = PROC_MODE_DEF;      /* default -- applies to both "COMPENSATED" and
			      "STANDARD" product types */
  } else { /* placeholder for PP */
    procMode = PROC_MODE_DEF;
  }

/*
 * The following block of code is not needed for the R1B' delivery but
 * may be supported for later deliveries.
 *
 * Special note about the DESKEW setting:  the product mapping table for
 * R1B' does not support setting DESKEW to "NO" when the projection is
 * "GROUND_RANGE," and specifying "SLANT_RANGE" as the projection always
 * implies that DESKEW is "NO."  This reduces the possible number of
 * combinations and simplifies the code.  Also, the Product Spec may be
 * wrong when it states that SLANT_RANGE ==> processor mode = 'U'U'', since
 * a code for SLANT_RANGE ('S') is already included in the range for the
 * projection.
 */

/*
  else {
    if (GLOBAL_state.processingGain != DEF_PROCESSING_GAIN) 
      procMode = PROC_MODE_USER;
    else if (fabs(DEF_AVG_TERRAIN_HT - GLOBAL_state.avgTerrainHt) > 0.00000001) 
      procMode = PROC_MODE_USER; 
*/
/**** we need the scene center lat and lon to calcuate these defaults...
    else if (fabs(DEF_PS_REF_LAT - GLOBAL_state.standardLat) > 0.00000001) 
      procMode = PROC_MODE_USER;
    else if (fabs(DEF_PS_REF_LON - GLOBAL_state.standardLon) > 0.00000001) 
      procMode = PROC_MODE_USER; 
    else if (fabs(DEF_LAMBERT_LAT_N - GLOBAL_state.lambert_lat_n) > 0.00000001) 
      procMode = PROC_MODE_USER;
    else if (fabs(DEF_LAMBERT_LAT_S - GLOBAL_state.lambert_lat_s) > 0.00000001) 
      procMode = PROC_MODE_USER;
  }
*************/

 if (isCsd)
   *qcType = QC_TYPE_NONE;

  sprintf(fileNameBuf,"%s%05d%03d%c%d%c%03d",
          GLOBAL_state.dtm_satBuf,  /* satellite */
          GLOBAL_state.rev,         /* rev */
          GLOBAL_state.frameId,     /* frame id */
          projection,               /* projection */
          pixelSpacing,             /* pixel spacing */
          procMode,                 /* processor mode */
          version);

  printfLLog(LOG_DEBUG, "Product Id: %s qcType %d\n", fileNameBuf, *qcType);
  return(fileNameBuf);

} /* end buildProductId........................*/
/*----------------------------------------------------------
 * NAME:
 *  buildFullProductName
 *
 * DESCRIPTION:
 *   build complete prefix of image file name along with directory path
 *   and ending with a "."
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

static char *buildFullProductName(char *dirPath, char *productId, int jobId)
{
 char *fileNameBuf = doMalloc(MAXPATHLEN);
                                      /* construct the complete file name */
  sprintf(fileNameBuf,"%s/%s/%d/%s.",
          dirPath,                  /* directory path */
          GLOBAL_state.dtm_satBuf,  /* satellite directory */
          jobId,                    /* job id directory */
          productId);

  printfLLog(LOG_DEBUG, "Full product name: %s\n", fileNameBuf);
  return(fileNameBuf);
} /* buildFullProductName */

/*----------------------------------------------------------
 * NAME:
 *  buildScanFileBaseName
 *
 * DESCRIPTION:
 *   build prefix of file name along with directory path
 *   without the 3 digit version number at the end
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *buildScanFileBaseName(int jobId)
{
 char *fileNameBuf = doMalloc(MAXPATHLEN);
 char frameMode, mediaType;

  bzero(fileNameBuf, MAXPATHLEN);
  frameMode = getFrameModeChar(GLOBAL_state.frame_mode_buf);
  mediaType = getMediaTypeChar(GLOBAL_state.mediaType);
  sprintf(fileNameBuf,"%s%05d%02d%3s%c%c",
          GLOBAL_state.dtm_satBuf,  /* satellite */
          GLOBAL_state.rev,         /* rev */
          GLOBAL_state.seq,         /* sequence */
          GLOBAL_state.mode_buf,    /* instrument mode */
          frameMode,                /* frame mode */
          mediaType);                 /* media type */


  printfLLog(LOG_DEBUG, "Scan Results File base name: %s\n", fileNameBuf);

  return(fileNameBuf);

} /* end buildScanFileBaseName........................*/

/*----------------------------------------------------------
 * NAME:
 *  buildScanFileName
 *
 * DESCRIPTION:
 *   build prefix of file name along with directory path
 *   *with* the 3 digit version number at the end
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *buildScanFileName(int fileVersion, int jobId)
{
 char *fileNameBuf = doMalloc(MAXPATHLEN);
 char frameMode, mediaType;

  bzero(fileNameBuf, MAXPATHLEN);
  frameMode = getFrameModeChar(GLOBAL_state.frame_mode_buf);
  mediaType = getMediaTypeChar(GLOBAL_state.mediaType);

  sprintf(fileNameBuf,"%s%05d%02d%3s%c%c%03d",
          GLOBAL_state.dtm_satBuf,  /* satellite */
          GLOBAL_state.rev,         /* rev */
          GLOBAL_state.seq,         /* sequence */
          GLOBAL_state.mode_buf,    /* instrument mode */
          frameMode,                /* frame mode */
          mediaType,                  /* media type */
          fileVersion);

  printfLLog(LOG_DEBUG, "Scan Results File name: %s\n", fileNameBuf);
  return(fileNameBuf);

} /* end buildScanFileName........................*/




/*----------------------------------------------------------
 * NAME:
 *  buildScanFileNameGivenDirPath
 *
 * DESCRIPTION:
 *   build prefix of file name along with directory path
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *buildScanFileNameGivenDirPath(char *dirPath, int jobId)
{
 char *fileNameBuf = doMalloc(MAXPATHLEN);
 char frameMode , mediaType = MEDIA_TYPE_DEF;

  bzero(fileNameBuf, MAXPATHLEN);
  frameMode = getFrameModeChar(GLOBAL_state.frame_mode_buf);
  mediaType = getMediaTypeChar(GLOBAL_state.mediaType);

  sprintf(fileNameBuf,"%s/%s%05d%02d%3s%c%c%03d.",
          dirPath,                  /* directory path */
          GLOBAL_state.dtm_satBuf,  /* satellite */
          GLOBAL_state.rev,         /* rev */
          GLOBAL_state.seq,         /* sequence */
          GLOBAL_state.mode_buf,    /* instrument mode */
          frameMode,                /* frame mode */
          mediaType,                /* media type */
          GetFileVersion_MSTRQLIST(jobId));

#ifdef BUILD_DEBUG
printf("buildScanFileNameGivenDirPath returning %s\n", fileNameBuf);
#endif

  return(fileNameBuf);

} /* end buildScanFileNameGivenDirPath........................*/


/*----------------------------------------------------------
 * NAME:
 *  setMsgProductFromScanResults
 *
 * DESCRIPTION:
 *  sets the file entry in the ODL message to hostname:filespec
 *  by using the scan results filename for the core file name
 *
 * NOTES: returns 0 for error, nonzero for success
 *    args: writeOutODL: ODL to modify
 *              fileDir: directory name where product will reside
 *                  ext: file extension of product
 *               setStr: ODL field to be modified
 *                  err: descriptive string to be used in error message
 *          productName: returned value of completed destination string
 *                jobId: job id
 *
 *---------------------------------------------------------*/
static char *buildCorePart(int jobId, char *fileDir, char *ext, 
                    char *productName, int *qcType, int hostDiff,char *namePtr)
{
 int version;
 char *prefix, *productId, *dataFilesHostPtr;


  dataFilesHostPtr = getDataFilesHost(hostDiff,namePtr);        /* get hostname */
  printf("getHost host= %s hostDiff= %d\n", dataFilesHostPtr,hostDiff);

  if (dataFilesHostPtr == NULL)
    return(NULL);
  version = GetFileVersion_MSTRQLIST(jobId);              /* get file version */

  productId = buildProductId(version, jobId, qcType);
  prefix =  buildFullProductName(fileDir, productId, jobId);
  sprintf(productName, "%s:%s%s", dataFilesHostPtr, prefix, ext);
#ifdef BUILD_DEBUG
printf("buildCorePart returning product id %s, product %s\n", 
productId, productName);
#endif

  doFree(prefix);
  return(productId);

}

static int setMsgProductFromScanResults(ODL writeOutODL, char *fileDir, 
           char *ext, char *setStr, char *err, char *productName, int jobId, int hostDiff, char *namePtr)
{
 int qcType = QC_TYPE_IMAGE;
 char *productId;

#ifdef BUILD_DEBUG
printf("setMsgProductFromScanResults: fileDir %s ext %s setStr %s productName %s\n", 
fileDir, ext,  setStr, productName);
#endif

  productId = buildCorePart(jobId, fileDir, ext, productName, &qcType,hostDiff,namePtr);
  if (setObjectVal(writeOutODL, setStr, productName))
    printfLLog(LOG_DEBUG, FILE_EQUAL, err, productName);
  else {
    printfLLog(LOG_ERR, CANNOT_SET_FILE, err);
    return(0);
  }

  SetQCtype_MSTRQLIST(jobId, qcType); /* set qc type in mq list */

  doFree(productId); 
  return(1);

} /* setMsgProductFromScanResults */

static int setMsgImageProductFromScanResults(ODL writeOutODL, char *fileDir, 
           char *ext, char *setStr, char *err, char *productName, 
           int jobId, subsysFilesType *sf)
{
 int qcType = QC_TYPE_IMAGE;
 char *fileNamePtr, imageFileNameBuf[MAXPATHLEN], *p, *q;
 char *productId, *leaderFile, *avgFile;

#ifdef BUILD_DEBUG
printf("setMsgImageProductFromScanResults: fileDir %s ext %s setStr %s productName %s\n", 
fileDir, ext,  setStr, productName);
#endif
                                          /*    value passed in productName */
                                     /* is the leader file name; it was the */
  leaderFile = doMalloc(strlen(productName) +1);   /* return value from the */
  strcpy(leaderFile, productName);             /* last call to this routine */
                  /* the calling routine must do this in the correct order! */

  productId = buildCorePart(jobId, fileDir, ext, productName, &qcType,0,NULL);
  if (setObjectVal(writeOutODL, setStr, productName))
    printfLLog(LOG_DEBUG, FILE_EQUAL, err, productName);
  else {
    printfLLog(LOG_ERR, CANNOT_SET_FILE, err);
    return(0);
  }

  fileNamePtr = buildFullProductName(fileDir, productId, jobId);
  bzero(imageFileNameBuf, sizeof(imageFileNameBuf));

  sprintf(imageFileNameBuf, "%s:%s%s", getDataFilesHost(0,NULL), fileNamePtr, ext);

  avgFile = doMalloc(strlen(fileNamePtr) +4);       /* to add .avg to it */
  sprintf(avgFile, "%s%s", fileNamePtr, AVG_FILE_EXT); /* hardcode *.avg */

#ifdef SF_DEBUG
showSf_sf("setMsgImageProductFromScanResults ", sf); 
#endif

/* strip off the hostname from the image and leader file names */

  p = (p = strchr(leaderFile, ':')) ? p+1 : leaderFile;
  q = (q = strchr(imageFileNameBuf,':')) ? q+1 : imageFileNameBuf;

#ifdef BUILD_DEBUG
printf("setting leader %s and image files %s\n", p, q);
#endif
  SetQCFileNames_MSTRQLIST(jobId, p, q, avgFile);  

                                  /* set PRODUCT_ID field in message too */
  if (!setObjectVal(writeOutODL, BODY_PRODUCT_ID, productId)) {
    printfLLog(LOG_ERR, CANNOT_SET, "Product Id");
    doFree(productId);
    doFree(fileNamePtr);
    doFree(leaderFile);
    doFree(avgFile);
    return(0);
  }

  doFree(fileNamePtr);
  doFree(leaderFile);
  doFree(avgFile);

  SetQCtype_MSTRQLIST(jobId, qcType); /* set qc type in mq list */

  doFree(productId); 
  return(1);

} /* setMsgImageProductFromScanResults */



/*----------------------------------------------------------
 * NAME:
 *  replaceProductLoc
 *
 * DESCRIPTION:
 *  replace the hostname:dir part of a product name with a new
 *  hostname:dir string.
 *
 * NOTES: returns 0 for error, nonzero for success
 *
 *---------------------------------------------------------*/
static char *replaceProductLoc(char *oldName, char *newDir, 
                               char *platform, int jobId, char *namePtr)
{
 char *newName = doMalloc(MAXPATHLEN);
 char *p;

  p = (p = strrchr(oldName, '/')) ? p+1 : oldName; /* strip path off old */

  bzero(newName, MAXPATHLEN);
  /* sprintf(newName, "%s:%s/%s",  getDataFilesHost(), newDir, p); */
  sprintf(newName, "%s:%s/%s/%d/%s",  getDataFilesHost(2,namePtr), newDir, 
                                      platform, jobId, p);

#ifdef BUILD_DEBUG
printf("replaceProductLoc: old %s new %s\n", oldName, newName);
#endif

  return(newName);

} /* replaceProductLoc */

/*----------------------------------------------------------
 * NAME:
 *  replaceExt
 *
 * DESCRIPTION:
 *  replace the file extension (.EXT) with the provided string
 *
 *---------------------------------------------------------*/
static char *replaceExt(char *filename, char *ext)
{
 char *newName = doMalloc(MAXPATHLEN);
 char *pLoc;

  strcpy(newName, filename);
  pLoc = strrchr(filename, '.');
  if (pLoc == NULL)                /* if no '.' in the name, append one */
    newName[(strlen(filename)) ] = '.';    
  else
    newName[(strlen(filename) - strlen(pLoc)) +1 ] = '\0';

  strcat(newName, ext);

#ifdef BUILD_DEBUG
printf("replaceExt: old %s new %s\n", filename, newName);
#endif

  return(newName);
}

/*----------------------------------------------------------
 * NAME:
 *  replaceProductLocAndExt
 *
 * DESCRIPTION:
 *  replace the hostname:dir part of a product name with a new
 *  hostname:dir string.
 *
 * NOTES: returns 0 for error, nonzero for success
 *
 *---------------------------------------------------------*/

static char *replaceProductLocAndExt(char *oldName, char *newDir, char *platform, int jobId, char *namePtr)
{
 char *newName = doMalloc(MAXPATHLEN);
 char *p, *pLoc, *basepLoc;

  p = (p = strrchr(oldName, '/')) ? p+1 : oldName; /* strip path off old */

  pLoc = strchr(p, ' ');
  if (pLoc)
    p[strlen(p) - strlen(pLoc)] = '\0';         /* strip trailing blanks */

  bzero(newName, MAXPATHLEN);
  sprintf(newName, "%s:%s/%s/%d/%s", 
                    getDataFilesHost(2,namePtr), newDir, platform, jobId, p);

  basepLoc = strchr(basename(newName), '.');
  pLoc = strrchr(newName, '.');
  if (basepLoc == NULL)  /* if no '.' in basename, append one */
    newName[(strlen(newName)) ] = '.';
  else {     /* make sure that it's not a '.' in the directory name */
    newName[(strlen(newName) - strlen(pLoc)) +1 ] = '\0';
  }

  strcat(newName, SCAN_RESULTS_FILE_EXT);

#ifdef BUILD_DEBUG
printf("replaceProductLocAndExt returning: \n\told %s \n\tnew %s\n", oldName, newName);
#endif

  return(newName);


} /* replaceProductLocAndExt */

/*----------------------------------------------------------
 * NAME:
 *  buildScanResultsName
 *
 * DESCRIPTION:
 *  build the scan results file name *without* the hostname 
 *  using the message entries
 *
 * NOTES: returns 0 for error, nonzero for success
 *
 *---------------------------------------------------------*/

/* build WITHOUT host */
static char *buildScanResultsName(ODL writeOutODL, char *fileDir, char *ext,
             char *setStr, char *err, int jobId)
{
 int ok;
 char *fileNamePtr, fileName[MAXPATHLEN];
 char *fullName ;

#ifdef BUILD_DEBUG
printf("buildScanResultsName: dir %s ext %s\n", fileDir, ext);
#endif

  fileNamePtr = buildScanFileNameGivenDirPath(fileDir, jobId);

  sprintf(fileName, "%s%s", fileNamePtr, ext);

  fullName = doMalloc(MAXPATHLEN);
  bzero(fullName, MAXPATHLEN);
  sprintf(fullName, "%s%s", fileNamePtr, ext);

  if (ok = ODLSetString(writeOutODL, setStr, fullName))
    printfLLog(LOG_DEBUG, FILE_EQUAL, err, fullName);
  else {
   printfLLog(LOG_ERR, CANNOT_SET_FILE, err);
   doFree(fullName);
   return(NULL);
  }

  SetScanResultsFileName_MSTRQLIST(jobId, fileName);
  SetQCtype_MSTRQLIST(jobId, QC_TYPE_SCAN);
  doFree(fileNamePtr);
#ifdef BUILD_DEBUG
printf("leaving buildScanResultsName, returning %s\n", fullName);
#endif

  return(fullName);
} /* buildScanResultsName*/



/*----------------------------------------------------------
 * NAME:
 *  buildSpsDecodeRequest
 *
 * DESCRIPTION:
 *   build the SPS_DECODE_REQUEST message. return 0 on success
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int buildSpsDecodeRequest(int jobId, ODL writeOutODL, 
                                 subsysFilesType *sf,char *namePtr)
{
 int i, ok, ssp2Num, isSSP2,hostDiff;
 char fullName[MAXPATHLEN], *errmsg, *srf, *SSP2name,*inputDir,*outputDir;
 subsysConfigType *config;
 ODL odl;
 long decodeSize=0, expectedSize = -1; 
 frameInfo frame;

  for (i=0,ok=1; ok&& sps_decode_request[i].str != NULL; ) {
#ifdef SET_DEBUG
printf("setting sps_decode_request value of %s\n", sps_decode_request[i].str);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_decode_request[i].str,
         *sps_decode_request[i].ptr) == 1)) {
      if (strstr(sps_decode_request[i].str, "PLATFORM_TIME"))  
        makeUnsignedInt(Value(Lookup(writeOutODL, sps_decode_request[i].str)));
      i++;
    }
    else {
      printfLLog(LOG_DEBUG,"ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }
  for (i=0; ok&& sps_decode_request_time[i].str != NULL; ) {
#ifdef SET_DEBUG
printf("setting sps_decode_request_time value of %s\n", sps_decode_request_time[i].str);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_decode_request_time[i].str,
         sps_decode_request_time[i].ptr) == 1))
      i++;
    else {
      printfLLog(LOG_DEBUG, "ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }
  for (i=0; ok&& sps_decode_request_double[i].str != NULL; ) {
#ifdef SET_DEBUG
printf("setting sps_decode_request_double value of %s\n", sps_decode_request_double[i].str);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_decode_request_double[i].str,
         *(double *)sps_decode_request_double[i].ptr) == 1))
      i++;
    else {
      printfLLog(LOG_DEBUG, "ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }

/* replace scan results file dir with the one in the config file */
/* and replace its extension with the default scan results dir one */

  srf=replaceProductLocAndExt(GLOBAL_state.scanResultsFile, getScanResultsDir(),
                            GLOBAL_state.dtm_satBuf, jobId,namePtr);
  if (srf == NULL) {
    errmsg = DECODE_SCAN_RESULTS_FILE;
    ok = 0;
  }

  if (ok && 
     (odl = readScanResults(srf, GLOBAL_state.frameId, writeOutODL, &frame)) 
               == NULL) {
      doFree(srf);
      return(-1);
    }
    else
      if (odl)
        ODLFree(odl);

  if (!ODLSetString(writeOutODL, DECODE_SCAN_RESULTS_FILE, srf)) {
    doFree(srf);
    return(-1);
  }

#ifdef BUILD_DEBUG
printf("building SPS_DECODE_REQUEST for scan results file %s\n", srf);
#endif
  if (!ok) {
    printfLLog(LOG_ERR, ERROR_SETTING_MSG);
    printfLLog(LOG_DEBUG, DEB_ERROR_SETTING_MSG, SPS_DECODE_REQUEST, errmsg);
    doFree(srf);
    return(-1);
  }
  doFree(srf);

  config = getCPconfig();

  expectedSize = getExpectedSize(writeOutODL, &decodeSize) ;

                                                /* assign ssp subsystem */
  SSP2name = assignSSP(GLOBAL_state.productT_buf, 
                       GLOBAL_state.compensated, &isSSP2);
  if (SSP2name == NULL)  /* no ssp is defined at all! */
    return(-3);

#ifdef BUILD_DEBUG
printf("assignSSP assigned %s, is SSP2 %d\n", SSP2name, isSSP2);
#endif
  SetSSPname_MSTRQLIST(jobId, SSP2name);
  if (isSSP2) {
    ssp2Num = getSSP2numGivenName(SSP2name);
    inputDir = assignSSP2inputDir(ssp2Num, expectedSize+decodeSize/NBPSCTR);
#ifdef CHECK_DISK_SPACE
    if (inputDir == NULL)
      return(-2);
    SetInputDir_MSTRQLIST(jobId, inputDir);
    outputDir = assignSSP2outputDir(ssp2Num, expectedSize);  
    if (outputDir == NULL)
      return(-2);
    SetOutputDir_MSTRQLIST(jobId, outputDir);
#else
   inputDir=".";
   outputDir=".";
#endif
  }

  printfLLog(LOG_INFO, JOB_INPUT_DIR, jobId, inputDir);
  printfLLog(LOG_INFO, JOB_OUTPUT_DIR,  jobId, outputDir);
 
  bzero(sf->fileName, sizeof(sf->fileName));
/*   bzero(sf->productName, sizeof(sf->productName)); */
  sf->jobId = jobId;
  sf->numFiles = 0;


  if (!mkImageDir(inputDir, GLOBAL_state.dtm_satBuf, jobId, sf)) {
    printfLLog(LOG_ERR, MKDIR_ERROR, inputDir, GLOBAL_state.dtm_satBuf, jobId);
    return(-1);
  }

  strcpy(fullName, "");
  hostDiff = 1;
                                                /* build file names */
  if (!setMsgProductFromScanResults(writeOutODL, inputDir, 
       "echo", BODY_ECHO_FILE, ECHO_DATA, fullName, jobId,hostDiff,namePtr))
    return(-1);

  strcpy(sf->fileName[sf->numFiles++] , fullName);
  if (!setMsgProductFromScanResults(writeOutODL, inputDir,
       "aux", BODY_AUX_FILE,  AUX_DATA, fullName, jobId,hostDiff,namePtr))
    return(-1);

  strcpy(sf->fileName[sf->numFiles++] , fullName);
  if (!setMsgProductFromScanResults(writeOutODL, inputDir,
       "repl", BODY_REPL_FILE, REPLICA_DATA, fullName, jobId,hostDiff,namePtr))
    return(-1);

  strcpy(sf->fileName[sf->numFiles++] , fullName);
  if (!setMsgProductFromScanResults(writeOutODL, inputDir,
       "ephm", BODY_EPHEM_FILE, EPHEM_DATA, fullName, jobId,hostDiff,namePtr))
    return(-1);
  strcpy(sf->fileName[sf->numFiles++] , fullName);

  if (getModeId(GLOBAL_state.mode_buf) == MODE_SCANSAR_ID) { 
                                 /* only Scansar jobs create a burst file */
    if (!setMsgProductFromScanResults(writeOutODL, inputDir,
       "burst", BODY_BURST_FILE, BURST_DATA, fullName, jobId,hostDiff,namePtr))
      return(-1);
    strcpy(sf->fileName[sf->numFiles++] , fullName);
  }

  
  return(0);

} /* end buildSpsDecodeRequest.......................................*/

/*----------------------------------------------------------
 * NAME:
 *  buildSpsScanRequest
 *
 * DESCRIPTION:
 *   build the SPS_SCAN_REQUEST message. return 0 on success
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int buildSpsScanRequest(int jobId, ODL writeOutODL, subsysFilesType *sf,
 char* namePtr)
{
 int i, ok;
 char *errmsg, *fullName, *scanResultsDir;
 subsysConfigType *config;

#ifdef SF_DEBUG
showSf_sf("ENTERING buildbuildSpsScanRequest", sf);
#endif


  for (i=0,ok=1; ok&& sps_scan_request[i].str != NULL; ) {
#ifdef SET_DEBUG
printf("setting sps_scan_request value of %s\n", sps_scan_request[i].str);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_scan_request[i].str,
         *sps_scan_request[i].ptr) == 1)) {
      if (strstr(sps_scan_request[i].str, "PLATFORM_TIME"))  {
        makeUnsignedInt(Value(Lookup(writeOutODL, sps_scan_request[i].str)));
      }
#ifdef TNT
      if (strstr(sps_scan_request[i].str, "PLATFORM_TIME")) {
        ODL odl = Value(Lookup(writeOutODL, sps_scan_request[i].str));
        if (odl != NULL) ((Obj_t*) odl)->send = (Method_t) UnsignedInt;
      }
#endif
      i++;
}
    else {
      printfLLog(LOG_DEBUG, "ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }
  for (i=0; ok&& sps_scan_request_time[i].str != NULL; ) {
#ifdef SET_DEBUG
printf("setting sps_scan_request_time value of %s\n", sps_scan_request_time[i]
.str);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_scan_request_time[i].str,
         sps_scan_request_time[i].ptr) == 1))
      i++;
    else {
      printfLLog(LOG_DEBUG, "ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }

  for (i=0; ok&& sps_scan_request_double[i].str != NULL; ) {
#ifdef SET_DEBUG
printf("setting sps_scan_request_double value of %s\n", sps_scan_request_double[i].str);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_scan_request_double[i].str,
         *(double *)sps_scan_request_double[i].ptr) == 1))
      i++;
    else {
      printfLLog(LOG_DEBUG, "ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }


  if (!ok) {
    printfLLog(LOG_ERR, ERROR_SETTING_MSG);
    printfLLog(LOG_DEBUG, DEB_ERROR_SETTING_MSG, SPS_SCAN_REQUEST, errmsg);
    return(-1);
  }

  config = getCPconfig();
                                                /* build file names */


  bzero(sf->fileName, sizeof(sf->fileName));
/*  bzero(sf->productName, sizeof(sf->productName)); */
  sf->jobId = jobId;
  sf->numFiles = 0;

  if (!mkImageDir(config->scan_results_dirPtr, GLOBAL_state.dtm_satBuf,
                  jobId, sf)) {
    printfLLog(LOG_ERR, MKDIR_ERROR, config->scan_results_dirPtr, 
               GLOBAL_state.dtm_satBuf, jobId);
    return(-1);
  }

  scanResultsDir = mkScanResultsDirName(config->scan_results_dirPtr, 
                      GLOBAL_state.dtm_satBuf, jobId,namePtr);
  fullName =  buildScanResultsName(writeOutODL, scanResultsDir, 
        SCAN_RESULTS_FILE_EXT, SCAN_SCAN_RESULTS_FILE,SCAN_RESULTS,jobId);

  SetOutputDir_MSTRQLIST(jobId, scanResultsDir);
  doFree(scanResultsDir);
 
  if (fullName == NULL) {
    return(-1);
 }

  strcpy(sf->fileName[sf->numFiles++] , fullName);
/*  sf->numProducts = sf->numFiles; */
  doFree(fullName);

#ifdef SF_DEBUG
showSf_sf("LEAVING buildbuildSpsScanRequest", sf);
#endif
  return(0);


} /* end buildSpsScanRequest.......................................*/

/*----------------------------------------------------------
 * NAME:
 *  buildSpsFrameRequest
 *
 * DESCRIPTION:
 *  build the SPS_FRAME_REQUEST message. return 0 on success
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

static int buildSpsFrameRequest(int jobId, ODL writeOutODL, subsysFilesType *sf, char *namePtr)
{
 int i, ok, ssp2Num, cat, numCalProducts=0,hostDiff;
 char *srf, *p, fullName[MAXPATHLEN], *errmsg, *inputDir, *outputDir, 
       *dest, *cp, *cp1, *cp2, *chosenCalParams, imageName[MAXPATHLEN];
 ODL odl;
 long decodeSize=0, expectedSize = -1;
 frameInfo frame;

/* replace scan results file dir with the one in the config file */
/* and replace its extension with the default scan results dir one */

  srf = replaceProductLocAndExt(GLOBAL_state.scanResultsFile, 
                         getScanResultsDir(), GLOBAL_state.dtm_satBuf, jobId,namePtr);
  if (srf == NULL) {
    errmsg = FRAME_SCAN_RESULTS_FILE;
    ok = 0;
  }

  for (i=0,ok=1; ok&& sps_frame_request[i].str != NULL; ) {
#ifdef SET_DEBUG
printf("setting sps_frame_request value of %s\n", sps_frame_request[i].str); fflush(stdout);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_frame_request[i].str,
         *sps_frame_request[i].ptr) == 1))  {
      if (strstr(sps_frame_request[i].str, "PLATFORM_TIME"))  {
        makeUnsignedInt(Value(Lookup(writeOutODL, sps_frame_request[i].str)));
      }
      i++;
    }
    else {
      printfLLog(LOG_DEBUG, "ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }
  for (i=0; ok&& sps_frame_request_time[i].str != NULL; ) {
#ifdef SET_DEBUG
printf("setting sps_frame_request_time value of %s\n", sps_frame_request_time[i]
.str); fflush(stdout);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_frame_request_time[i].str,
         sps_frame_request_time[i].ptr) == 1))
      i++;
    else {
      printfLLog(LOG_DEBUG, "ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }

  for (i=0; ok&& sps_frame_request_double[i].str != NULL; ) {
#ifdef SET_DEBUG
printf("setting sps_frame_request_double value of %s\n", sps_frame_request_double[i]
.str);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_frame_request_double[i].str,
         *(double *)sps_frame_request_double[i].ptr) == 1))
      i++;
    else {
      printfLLog(LOG_DEBUG, "ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }


#ifdef ASP_READ_FRAME_IN_SRF
/* asp hack: initialize frame segment */
  if (ok) {

    if ((odl = readScanResults(srf, GLOBAL_state.frameId, writeOutODL, &frame)) 
                          == NULL) {
      return(-1);
    }
    else
      ODLFree(odl);

/* hack this up for the ASP *********************************************/

    if (!frame.found) {
#ifdef CHECK_FRAME_EXISTENCE  
       errmsg = "SCAN_RESULTS_FILE.BODY.SEGMENT.FRAME"; 
       printfLLog(LOG_ERR,"Frame %d not found in %s",GLOBAL_state.frameId,srf);
       ok = 0; 
#else  /* for testing don't care if frame is there or not */
         ; 
#endif
    }
    else {
      setObjectVal(writeOutODL, BODY_START_ADDRESS, frame.startAddr);
      setObjectVal(writeOutODL, BODY_END_ADDRESS, frame.endAddr);
      setObjectVal(writeOutODL, BODY_START_TIME, &frame.startTime);
      setObjectVal(writeOutODL, BODY_END_TIME, &frame.endTime);
    } /* frame.found */

  }

/* end of ASP hack *********************************************/
#endif


/* add hostname to scan results file string for setting in the message */
  if (ok) {
    if (!ODLSetString(writeOutODL, FRAME_SCAN_RESULTS_FILE, srf))
      return(-1);
    doFree(srf);
  }

  if (ODLSetVal(writeOutODL, FRAME_FILE_VERSION, 
                             GetFileVersion_MSTRQLIST(jobId)) == -1) {
    errmsg = "File Version";
    ok = 0;
  }

  if (!ok) {
    printfLLog(LOG_ERR, ERROR_SETTING_MSG);
    printfLLog(LOG_DEBUG, DEB_ERROR_SETTING_MSG, SPS_FRAME_REQUEST, errmsg);
    return(-1);
  }

  expectedSize = getExpectedSize(writeOutODL, &decodeSize) ;

                                                /* build file names */

  if (ODLGetVal(writeOutODL, FRAME_DEST, &dest) != 1) {
    printfLLog(LOG_ERR, "error getting DESTINATION\n");
    return (-1);
  }
  else  {
    if ((cat = GetSubsystemCategoryGivenName(dest)) == ASP_CATEGORY_ID)   {
      outputDir = assignASPoutputDir(expectedSize);
    }
    else {
      if (cat == SSP2_CATEGORY_ID) {
        ssp2Num = getSSP2numGivenName(dest);  /* use destination name */
        inputDir = GetInputDir_MSTRQLIST(jobId);
#ifdef CHECK_DISK_SPACE
        if (inputDir == NULL)
          return(-2);
        outputDir = assignSSP2outputDir(ssp2Num, expectedSize);   
        if (outputDir == NULL)
          return(-2);
#else
   inputDir=".";
   outputDir=".";
#endif
      }

    if (cat == SSP2_CATEGORY_ID)
      printfLLog(LOG_INFO, JOB_INPUT_DIR, jobId, inputDir);
    printfLLog(LOG_INFO, JOB_OUTPUT_DIR,  jobId, outputDir);

  if (!mkImageDir(inputDir, GLOBAL_state.dtm_satBuf, jobId, sf)) {
    printfLLog(LOG_ERR, MKDIR_ERROR, inputDir, GLOBAL_state.dtm_satBuf, jobId);
    return(-1);
  }

    hostDiff = 1;
    printf("Input Dir= %s\n", inputDir);

    strcpy(fullName, "");
    if (!setMsgProductFromScanResults(writeOutODL, inputDir,
       "echo", BODY_ECHO_FILE, ECHO_DATA, fullName, jobId,hostDiff,namePtr))
      return(-1);

    if (!setMsgProductFromScanResults(writeOutODL, inputDir,
       "aux", BODY_AUX_FILE, AUX_DATA, fullName, jobId,hostDiff,namePtr))
      return(-1);

    if (!setMsgProductFromScanResults(writeOutODL, inputDir,
       "repl", BODY_REPL_FILE, REPLICA_DATA, fullName, jobId,hostDiff,namePtr))
      return(-1);

    if (!setMsgProductFromScanResults(writeOutODL, inputDir,
       "ephm", BODY_EPHEM_FILE, EPHEM_DATA, fullName, jobId,hostDiff,namePtr))
      return(-1);
    if (getModeId(GLOBAL_state.mode_buf) == MODE_SCANSAR_ID) {
                                 /* only Scansar jobs create a burst file */
        if (!setMsgProductFromScanResults(writeOutODL, inputDir,
           "burst", BODY_BURST_FILE, BURST_DATA, fullName, jobId,hostDiff,namePtr))
          return(-1);
        strcpy(sf->fileName[sf->numFiles++] , fullName);
      }

    }

  }

#ifdef CHECK_DISK_SPACE
  if (outputDir == NULL)  /* NULL means no room for files on any file system */
    return(-2);
#else
   outputDir=".";
#endif

  SetOutputDir_MSTRQLIST(jobId, outputDir); /* save output dir */

  if (cat == SSP2_CATEGORY_ID) {
#ifdef BUILD_DEBUG
    printf("buildSpsFrameRequest: # beams %d mode %s type %s\n",
                 getNumBeamsGivenMode(GLOBAL_state.mode_buf),
                 GLOBAL_state.mode_buf,GLOBAL_state.productT_buf);
#endif

    numCalProducts = getNumCalProducts(GLOBAL_state.mode_buf,
                       GLOBAL_state.productT_buf, GLOBAL_state.compensated);
    SetNumCalProducts_MSTRQLIST(jobId,  numCalProducts);

  }


/* these fields are used by both SSP and ASP */

  sf->jobId = jobId;
  sf->numFiles=0;

  if (!mkImageDir(outputDir, GLOBAL_state.dtm_satBuf, jobId, sf)) {
    printfLLog(LOG_ERR, MKDIR_ERROR, outputDir, GLOBAL_state.dtm_satBuf, jobId);
    return(-1);
  }

  if (!setMsgProductFromScanResults(writeOutODL, outputDir, LEADER_FILE_EXT, 
          BODY_CEOS_LEADER_FILE, CEOS_LEADER, fullName, jobId,0,NULL))
    return(-1);
  strcpy(sf->fileName[sf->numFiles++], fullName);

  strcpy(imageName, fullName);
#ifdef SF_DEBUG
printf("adding %d cal products for %s\n", numCalProducts, imageName);
#endif
  for (i=1; i <= numCalProducts; i++)  { /* cal products named 1..n */
    strcpy(sf->fileName[sf->numFiles++], buildCalProductId(i, imageName));
  }
  
  if (!setMsgImageProductFromScanResults(writeOutODL, outputDir, 
          IMAGE_FILE_EXT, BODY_IMAGE_FILE, CEOS_DATA, fullName, jobId, sf))
    return(-1);
  strcpy(sf->fileName[sf->numFiles++] , fullName);

  strcpy(imageName, fullName);
#ifdef SF_DEBUG
printf("adding %d cal products for %s\n", numCalProducts, imageName);
#endif
  for (i=1; i <= numCalProducts; i++)    /* cal products named 1..n */
    strcpy(sf->fileName[sf->numFiles++], buildCalProductId(i, imageName));

  if (!setMsgProductFromScanResults(writeOutODL, outputDir, PMF_FILE_EXT, 
            BODY_PMF_FILE, PMF_DATA, fullName, jobId,0,NULL))
    return(-1);
  strcpy(sf->fileName[sf->numFiles++] , fullName);

  strcpy(imageName, fullName);
#ifdef SF_DEBUG
printf("adding %d cal products for %s\n", numCalProducts, imageName);
#endif
  for (i=1; i <= numCalProducts; i++)    /* cal products named 1..n */
    strcpy(sf->fileName[sf->numFiles++], buildCalProductId(i, imageName));

            /* strip off the hostname from the pmf file name */
  p = (p = strchr(fullName, ':')) ? p+1 : fullName;
  SetPMFfileName_MSTRQLIST(jobId, p);


/* also need to add the pvs product file names to the sf structure
   if this is an uncompensated or compensated product */


/* choose which of the two cal params files provided should be used */
  odl = GetSysReqGivenJobId_SYSQUE(dest, jobId);
  cp1 = ODLGetStr(odl, BODY_CALPARMS_FILE);
  cp2 = ODLGetStr(odl, BODY_CALPARMS_FILE_2);
  chosenCalParams = chooseCalParamsFile( cp1, cp2,
                p = GetProcessor_MSTRQLIST(jobId));

  if (chosenCalParams == NULL)
    printfLLog(LOG_ERR, NEITHER_CAL_PARAMS_MATCHED, cp1, cp2, p);
  else { 
    printfLLog(LOG_INFO, CAL_PARAMS_MATCHED, chosenCalParams);
    cp = replaceProductLoc(chosenCalParams, getCalParamsDir(),
                         GLOBAL_state.dtm_satBuf, jobId,namePtr);

    ODLSetString(writeOutODL, FRAME_CALPARMS, cp);
    doFree(cp);
  }

  return(0);


} /* end buildSpsFrameRequest.......................................*/


/*----------------------------------------------------------
 * NAME:
 *  buildCleanup_Msg
 *
 * DESCRIPTION:
 *   routine used to build a cleanup request message
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
ODL buildCleanup_Msg(char *namePtr, int jobid, char *platform, int rev, 
                     int discard)
{
 ODL writeOutODL;
 int ok = 1;

  printfLLog(LOG_INFO, SENDING_CLEANUP, namePtr );

  if ((writeOutODL = (ODL)GetNewMsg(CLEANUP_REQUEST)) == NULL) {
    printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, CLEANUP_REQUEST);
    return(NULL);
  }
  else {
    ODLSetVal(writeOutODL, CLEANUP_REQ_JOB_ID, jobid);
    if (ok) {
      ok = ODLSetString(writeOutODL, CLEANUP_HDR_SOURCE, ASF_CP);
    }
    if (ok) {
      ok = ODLSetString(writeOutODL, CLEANUP_HDR_DEST, namePtr);
    }
    if (discard)
      ok = ODLSetString(writeOutODL, CLEANUP_REQ_SAVE_FLAG, "NO");
    else
      ok = ODLSetString(writeOutODL, CLEANUP_REQ_SAVE_FLAG, "YES");
  }
  if (ok) {
    ok = ODLSetString(writeOutODL, CLEANUP_REQ_SAVE_DIR, 
                      getSaveDirGivenName(namePtr));
  }
  if (ok) {
    ok = ODLSetString(writeOutODL, CLEANUP_REQ_PLATFORM, platform);
  }
  if (ok) {
    ok = ODLSetVal(writeOutODL, CLEANUP_REQ_REV, rev);
  }

  if (!ok)
    printfLLog(LOG_ERR, "Error building %s", CLEANUP_REQUEST);

  return(writeOutODL);

} /* buildCleanup_Msg */


/*----------------------------------------------------------
 * NAME:
 *  buildCatalogMsg
 *
 * DESCRIPTION:
 *   build the IMS catalog message
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

static int buildCatalogMsg(ODL writeOutODL, int jobId, int catalogStep, 
                           int msgType, subsysFilesType *sf)
{
  int ok=0, qcType = QC_TYPE_NONE, ext_len, numCalStored;
  char *pp, *srf, *leaderFileNamePtr, *imageFileNamePtr, *errmsg, *namePtr;
  char *fileNamePtr, *productId, *cal, *chosenCalParams, *pLoc;
  char *compensated;
  subsysConfigType *config;
  frameInfo frame;
  ODL odl;

#ifdef SF_DEBUG
showSf_sf("ENTERING buildCatalogMsg", sf);
#endif
  sf->numFiles = 0;
  namePtr = getIMSnameGivenCatalogStep(catalogStep);
#ifdef IMS_DEBUG
printf("buildCatalogMsg: name %s step %d msgType %d \n", namePtr, catalogStep, msgType );
#endif

  config = getCPconfig();
                               /* set message header */
  ODLSetVal(writeOutODL, CATALOG_REQ_JOB_ID, jobId);
  if (ODLSetVal(writeOutODL, errmsg = CATALOG_HDR_DEST, namePtr))
      if (ODLSetVal(writeOutODL, errmsg = CATALOG_HDR_SOURCE, ASF_CP))
          ok = 1;  /* if got to here all is ok;
                      otherwise errmsg contains your error string */

#ifdef IMS_DEBUG
printf("buildCatalogMsg: setting body, ok %d \n", ok);
#endif
                               /* set message body */
  if (ok) {
    ODLSetString(writeOutODL, CATALOG_HDR_MSG_TYPE,
           pp = GetIMSmsgTypeAsText(catalogStep));
    doFree(pp);

/* intialize all the strings to null, then set only the appropriate ones */

      ODLSetString(writeOutODL, CATALOG_REQ_SCAN_RESULTS, "");
      ODLSetString(writeOutODL, CATALOG_REQ_CALPARMS, "");
      ODLSetString(writeOutODL, CATALOG_REQ_IMAGE_FILE, "");
      ODLSetString(writeOutODL, CATALOG_REQ_CEOS_FILE, "");
      ODLSetString(writeOutODL, CATALOG_REQ_PMF_FILE, "");
      ODLSetString(writeOutODL, CATALOG_REQ_QC_STATUS, "");
      ODLSetString(writeOutODL, CATALOG_REQ_FRAME_MODE, "");
      ODLSetString(writeOutODL, CATALOG_REQ_COMPENSATION_FLAG, "");

/* set the appropriate keywords in the msg based on the catalog step */

#ifdef IMS_DEBUG
printf("buildCatalogMsg: switch catalog step %d\n", catalogStep);
#endif
      if (catalogStep == IMS_TYPE_GET_SCAN_RESULT || 
          (catalogStep == IMS_TYPE_GET_VERSION && msgType==FRAME_REQUEST_ID) ||
           catalogStep == IMS_TYPE_STORE_PRODUCTS ||
           catalogStep == IMS_TYPE_STORE_CAL_PRODUCTS) {

/* build proper scan results file name for those who need it */
          srf = replaceProductLocAndExt(GLOBAL_state.scanResultsFile, 
                config->scan_results_dirPtr, GLOBAL_state.dtm_satBuf, jobId,namePtr);
          if (srf == NULL) {
            errmsg = CATALOG_SCAN_RESULTS_FILE;
            ok = 0;
          }
      }

      if (ok) {
      switch (catalogStep) {
        case IMS_TYPE_GET_VERSION: /* remove 3-digit version and ext: vvv.ext */
          sf->numDirs = 0;  /* initialize for entire life of job */
#ifdef GET_VERSION_FIX /* not enabled in delivery -- yet */
          if (msgType == FRAME_REQUEST_ID) {
            productId = buildProductId(0, jobId, &qcType);
            if (productId == NULL) {
              errmsg = CATALOG_REQ_PRODUCT_ID;
              ok = 0;
            }
            else {
              ext_len = strlen(productId);
              productId[ext_len-VERSION_LEN] = '\0'; /* remove ver from end */
              ODLSetString(writeOutODL, CATALOG_REQ_NAME,  productId);
              doFree(productId);
            }
          }
#else
          ext_len = strlen(SCAN_RESULTS_FILE_EXT) + VERSION_LEN + 1; /* '.' */
          if (msgType == FRAME_REQUEST_ID) {
            pp = (pp = strrchr(srf, '/')) ? pp+1 : srf;
            if (pp != NULL) {
               pp[SCAN_RESULTS_FILE_LEN-ext_len] = '\0'; /* remove ver+ext */
               ODLSetString(writeOutODL, CATALOG_REQ_NAME,  pp);
            }
          /* doFree(srf); */
          }

#endif
          else if (msgType == SCAN_REQUEST_ID) {
            productId = buildScanFileBaseName(GLOBAL_state.jobId);
            if (productId != NULL) {
               ODLSetString(writeOutODL, CATALOG_REQ_NAME,  productId);
               doFree(productId);
            }
          }
          break;
        case IMS_TYPE_GET_SCAN_RESULT:
          if (!mkImageDir(config->scan_results_dirPtr, GLOBAL_state.dtm_satBuf,
                          jobId, sf)) {
            printfLLog(LOG_ERR, MKDIR_ERROR, config->scan_results_dirPtr, 
                       GLOBAL_state.dtm_satBuf, jobId);
            ok = 0;
          }
          else {
            strcpy(sf->fileName[sf->numFiles++] , srf);
            ODLSetString(writeOutODL, CATALOG_REQ_SCAN_RESULTS, srf);


            productId = basename(srf);      /* strip off path */
            if (productId != NULL) {
              pLoc = strrchr(productId, '.'); 
              if (pLoc != NULL)            /* strip off ".ext" if exists */
                 productId[(strlen(productId) - strlen(pLoc))  ] = '\0';
               ODLSetString(writeOutODL, CATALOG_REQ_PRODUCT_ID,  productId);
/*               doFree(productId); */
            }
            else {
              errmsg = CATALOG_REQ_PRODUCT_ID;
              ok = 0;
            }

          }
          doFree(srf);
          break;
        case IMS_TYPE_GET_CAL_PARAMS:
          { char *p;

          if (!mkImageDir(config->cal_params_dirPtr, GLOBAL_state.dtm_satBuf,
                          jobId, sf)) {
            printfLLog(LOG_ERR, MKDIR_ERROR, config->cal_params_dirPtr, 
                       GLOBAL_state.dtm_satBuf, jobId);
            ok = 0;
          }
          else {
              extern ODL GetSysReqGivenJobId_SYSQUE();
              ODL odl;
              char *cp1, *cp2, *cp;

/* choose which of the two cal params files provided should be used */
            odl = GetSysReqGivenJobId_SYSQUE(namePtr, jobId);
            cp1 = ODLGetStr(odl, BODY_CALPARMS_FILE);
            cp2 = ODLGetStr(odl, BODY_CALPARMS_FILE_2);
            chosenCalParams = chooseCalParamsFile( cp1, cp2, 
                p = GetProcessor_MSTRQLIST(jobId));

            if (chosenCalParams == NULL) {
              printfLLog(LOG_ERR, NEITHER_CAL_PARAMS_MATCHED, cp1, cp2, p);
              ok = 0;
              break;
            }

            printfLLog(LOG_INFO, CAL_PARAMS_MATCHED, chosenCalParams);
            cp = replaceProductLoc(chosenCalParams, getCalParamsDir(),
                         GLOBAL_state.dtm_satBuf, jobId,namePtr);

            if (cp == NULL) {
              errmsg = CATALOG_REQ_CALPARMS;
              ok = 0;
              break;
            }
            strcpy(sf->fileName[sf->numFiles++] , cp);
            ODLSetString(writeOutODL, CATALOG_REQ_CALPARMS, cp);

            productId = buildProductId(GetFileVersion_MSTRQLIST(jobId),
                 jobId, &qcType);
            if (productId == NULL) {
              errmsg = "Error building product id";
              ok = 0;
            }
            else  {
              ODLSetString(writeOutODL, CATALOG_REQ_PRODUCT_ID,  productId);
              doFree(productId);
            }

          }

          }

          break;
        case IMS_TYPE_STORE_PRODUCTS:
/* before we build the product id we must read the scan results file */
          if ((odl = readScanResults(srf, GLOBAL_state.frameId, writeOutODL,
                  &frame)) == NULL) {
              errmsg = "Error reading scan results file";
              ok = 0;
          }
          else
              ODLFree(odl);

          if (srf != NULL)
            doFree(srf);


          productId = buildProductId(GetFileVersion_MSTRQLIST(jobId),
                 jobId, &qcType);
          if (productId == NULL) {
            errmsg = "Error building product id";
            ok = 0;
          }
          else {
            ODLSetString(writeOutODL, CATALOG_REQ_PRODUCT_ID,  productId);
/*             doFree(productId);  */
          }


/* readScanResults will set this field
          ODLSetString(writeOutODL, CATALOG_REQ_FRAME_MODE,  
                       GLOBAL_state.frame_mode_buf);
*/
          ODLSetString(writeOutODL, CATALOG_REQ_MODE,  GLOBAL_state.mode_buf);
          ODLSetString(writeOutODL, CATALOG_REQ_PRODUCT_ID,  productId);
          ODLSetString(writeOutODL, CATALOG_REQ_PRODUCT_TYPE,  
                   GLOBAL_state.productT_buf);
          compensated = ODLGetStr(GetSysReqGivenJobId_SYSQUE(namePtr, jobId), 
                       BODY_COMPENSATION_FLAG);
          ODLSetString(writeOutODL, CATALOG_REQ_COMPENSATION_FLAG, compensated);
          if (GetQCFileNames_MSTRQLIST(jobId, &leaderFileNamePtr,
                                &imageFileNamePtr, &fileNamePtr) == -1) {
                 errmsg = CANNOT_GET_QC_FILES;
                 ok = 0;
          }
          else if (GetPMFfileName_MSTRQLIST(jobId, &fileNamePtr) == -1) {
                errmsg = CANNOT_GET_PMF_FILE;
                ok = 0;
          }
          if (ok) { /* got the image, leader and pmf files successfully */
            ODLSetString(writeOutODL, CATALOG_REQ_PMF_FILE, fileNamePtr);
            ODLSetString(writeOutODL, CATALOG_REQ_IMAGE_FILE, imageFileNamePtr);
            ODLSetString(writeOutODL, CATALOG_REQ_CEOS_FILE, leaderFileNamePtr);
          }
          break;
        case IMS_TYPE_STORE_CAL_PRODUCTS:
/* before we build the product id we must read the scan results file */
          if ((odl = readScanResults(srf, GLOBAL_state.frameId, writeOutODL,
                  &frame)) == NULL) {
              errmsg = "Error reading scan results file";
              ok = 0;
          }
          else
              ODLFree(odl);

          if (srf != NULL)
            doFree(srf);

          productId = buildProductId(GetFileVersion_MSTRQLIST(jobId),
                 jobId, &qcType);
          if (productId == NULL) {
            errmsg = "Error building product id";
            ok = 0;
          }
          else {
            ODLSetString(writeOutODL, CATALOG_REQ_PRODUCT_ID,  productId);
          /* doFree(productId); */
          }


          ODLSetString(writeOutODL, CATALOG_REQ_MODE,  GLOBAL_state.mode_buf);
          ODLSetString(writeOutODL, CATALOG_REQ_PRODUCT_TYPE,
                   GLOBAL_state.productT_buf);
          if (GetQCFileNames_MSTRQLIST(jobId, &leaderFileNamePtr,
                                &imageFileNamePtr, &fileNamePtr) == -1) {
                 errmsg = CANNOT_GET_QC_FILES;
                 ok = 0;
          }
          else if (GetPMFfileName_MSTRQLIST(jobId, &fileNamePtr) == -1) {
                errmsg = CANNOT_GET_PMF_FILE;
                ok = 0;
          }
          numCalStored = GetNumCalProductsStored_MSTRQLIST(jobId);
          if (ok) { /* got the image, leader and pmf files successfully */

            productId = buildCalProductId(numCalStored+1, productId);
            ODLSetString(writeOutODL, CATALOG_REQ_PRODUCT_ID,  productId);

            cal = buildCalProdNameGivenImage(numCalStored+1, fileNamePtr);
            ODLSetString(writeOutODL, CATALOG_REQ_PMF_FILE, cal);
            doFree(cal);
            cal = buildCalProdNameGivenImage(numCalStored+1, imageFileNamePtr);
            ODLSetString(writeOutODL, CATALOG_REQ_IMAGE_FILE, cal);
            doFree(cal);
            cal = buildCalProdNameGivenImage(numCalStored+1, leaderFileNamePtr);
            ODLSetString(writeOutODL, CATALOG_REQ_CEOS_FILE, cal);
            doFree(cal);
          }
          doFree(productId);

          break;
        case IMS_TYPE_STORE_SCAN_RESULT:
        case IMS_TYPE_STORE_SCAN_METADATA:
          GetScanResultsFileName_MSTRQLIST(jobId, &fileNamePtr);
          ODLSetString(writeOutODL, CATALOG_REQ_SCAN_RESULTS, fileNamePtr);
          if ((qcType = GetQCtype_MSTRQLIST(jobId)) == QC_TYPE_ACCEPT)
            ODLSetString(writeOutODL, CATALOG_REQ_QC_STATUS, "ACCEPT");
          else if (qcType == QC_TYPE_SCAN_REJECT)
            ODLSetString(writeOutODL, CATALOG_REQ_QC_STATUS, "REJECT");
          if (catalogStep == IMS_TYPE_STORE_SCAN_METADATA) {
         
            strcpy(sf->fileName[sf->numFiles++], pp= replaceExt(fileNamePtr, 
                                                            PMF_FILE_EXT));
            doFree(pp);
          }
          break;
        default:
          errmsg = "Catalog Step";
          ok = 0;
          break;
      } /* switch catalogStep */
    } /* if inner ok */
  } /* if outer ok */

  if (!ok) {
   printfLLog(LOG_ERR, CANNOT_SET, errmsg);
   return (-1);
  }
#ifdef SF_DEBUG
showSf_sf("LEAVING buildCatalogMsg", sf);
#endif
  return(0);

} /* buildCatalogMsg */

/*----------------------------------------------------------
 * NAME:
 *  buildJobStatusMsg
 *
 * DESCRIPTION:
 *   build the PPS job status message
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

int buildJobStatusMsg(sysQueElemType *sqel, ODL writeOutODL, int isRetry)
{
  int i, ok=0, qcType = QC_TYPE_NONE,retries;
  char *errmsg, *productId, *msgTypeStr, *commentStr="";
  const char *dataset;
  char *compensated, *prodTypeStr, *modeStr, *namePtr=getPPSname(), *frame_mode;
#ifdef START_END_TIMES
  struct timeval mqTime;
  char *ppsStartTimes[] =  { JOB_STATUS_SCAN_START,
                             JOB_STATUS_DECODE_START,
                             JOB_STATUS_FRAME_START,
                             JOB_STATUS_QC_START,
                             JOB_STATUS_CATALOG_START} ;
  char *ppsEndTimes[] =  {   JOB_STATUS_SCAN_END,
                             JOB_STATUS_DECODE_END,
                             JOB_STATUS_FRAME_END,
                             JOB_STATUS_QC_END,
                             JOB_STATUS_CATALOG_END} ;
#endif

  char *statusType = isRetry ? "INTERMEDIATE" : "FINAL";
  char *statusStr = isRetry ? "RETRY" : "CANCEL/FAIL";

  /*writeOutODL = (ODL) GetNewMsg(SPS_JOB_STATUS); */

  namePtr = getPPSname();
  if (ODLSetVal(writeOutODL, errmsg=JOB_STATUS_HDR_DEST, namePtr))
    if (ODLSetVal(writeOutODL, errmsg=JOB_STATUS_JOB_ID, sqel->jobId))
      if (ODLSetVal(writeOutODL, errmsg = JOB_STATUS_HDR_SOURCE, ASF_CP))
      ok = 1;  /* if got to here all is ok;
                      otherwise errmsg contains your error string */

  for (i=0,ok=1; ok&& sps_job_status[i].str != NULL; ) {
#ifdef PPS_DEBUG
printf("setting sps_job_status value of %s\n", sps_job_status[i].str);
#endif
    if ((ok=ODLSetVal(writeOutODL, errmsg = sps_job_status[i].str,
         *sps_job_status[i].ptr) == 1))
      i++;
    else {
      printfLLog(LOG_DEBUG, "ODLSetVal failed for %s\n", errmsg);
      ok = 0;
    }
  }


#ifdef START_END_TIMES
  for (i=P_SCAN; ok && i < P_CATALOG; i++) {
    getStartTime_MSTRQLIST(sqel->jobId, i, &mqTime);
    if ( mqTime.tv_sec != 0L && mqTime.tv_usec != 0L &&
          ODLSetVal(writeOutODL, ppsStartTimes[i],  &mqTime) == 0) {
      printf("error setting pps time #%d\n", i);
      ok=0;
    }
    getEndTime_MSTRQLIST(sqel->jobId, i, &mqTime);
    if ( mqTime.tv_sec != 0L && mqTime.tv_usec != 0L &&
        ODLSetVal(writeOutODL, ppsEndTimes[i],  &mqTime) == 0) {
      printf("error setting pps time #%d\n", i);
      ok=0;
    }
  }
#endif
  if (ok) {
    ok = 0; /* reset ok flag */
    if (ODLSetVal(writeOutODL, errmsg = JOB_STATUS_STATUS_TYPE, statusType) )
      if (ODLSetVal(writeOutODL, errmsg = JOB_STATUS_REQ_TYPE,
                    ODLGetStr(sqel->sysODLreq, HDR_MSG_TYPE) ) )
        ok = 1;
  }

  if (ok) {
    ok = 0; /* reset ok flag */
    retries = GetRetry_MSTRQLIST(sqel->jobId);
    if (((qcType = GetQCtype_MSTRQLIST(sqel->jobId)) == QC_TYPE_ACCEPT ||
         qcType == QC_TYPE_DONE ) && retries == 0)
       statusStr = "COMPLETED";
    else {
       commentStr = GetFailureReason_MSTRQLIST(sqel->jobId);
    }
    if (ODLSetVal(writeOutODL, errmsg = JOB_STATUS_STATUS, statusStr) )
      if (ODLSetVal(writeOutODL, errmsg = JOB_STATUS_COMMENT, commentStr) )
        ok = 1;
  }



  if (ok) {
    msgTypeStr = ODLGetStr(sqel->sysODLreq, HDR_MSG_TYPE);
    if (msgTypeStr != NULL) {
      if (strncmp(msgTypeStr, "FRAME", strlen("FRAME") ) == 0) {
        ODLSetVal(writeOutODL, errmsg=JOB_STATUS_REQ_TYPE, "FRAME");
        productId = buildProductId(GetFileVersion_MSTRQLIST(sqel->jobId),
                                 sqel->jobId, &qcType) ;
      }
      else if (strncmp(msgTypeStr, "SCAN", strlen("SCAN") ) == 0) {
        ODLSetVal(writeOutODL,errmsg=JOB_STATUS_REQ_TYPE, "SCAN");
        productId = buildScanFileName(GetFileVersion_MSTRQLIST(sqel->jobId),
                                 sqel->jobId);
      }
      if (productId == NULL)
        ok = 0;
    }
  }

  modeStr = ODLGetStr(sqel->sysODLreq, BODY_MODE);
  if (modeStr == NULL) 
    ok = 0;

/* only bother trying to set dataset if the job completed succcessfully */

  if (ok && (strcmp(statusStr, "COMPLETED") == 0) ) {
    compensated = ODLGetStr(sqel->sysODLreq, BODY_COMPENSATION_FLAG);
    frame_mode = ODLGetStr(sqel->sysODLreq, BODY_FRAME_MODE);
    prodTypeStr = ODLGetStr(sqel->sysODLreq, BODY_PRODUCT_TYPE);

#ifdef BUILD_DEBUG
printf("sq->odl is %s\n", ODLToStr(sqel->sysODLreq, NULL) );
printf("compensated %s frame_mode %s\n", compensated, frame_mode);
#endif
    dataset = get_dataset(productId, prodTypeStr, modeStr, frame_mode,
                     msgTypeStr, compensated);
    ok = 0;
    if (dataset == NULL)
      errmsg = "dataset";  /* not ok, don't send message */
                      /* note that for CANCEL/FAIL, we DO send "" dataset */
    else {
      if (ODLSetVal(writeOutODL, errmsg = JOB_STATUS_DATASET, dataset) ) {
        ok = 1;
#ifdef PPS_DEBUG
        printf("buildJobStatusMsg: dataset is %s\n", dataset);
#endif
      }
    }
  }
  
  if (ok) {
/* caution: before setting product id, we need the GLOBAL_state struct to
   contain all the proper values for this job */
    ok = 0;
    if (ODLSetVal(writeOutODL, errmsg = JOB_STATUS_PRODUCT_ID, productId) )
      ok = 1;
    doFree(productId);
  }
  if (!ok) {
   printfLLog(LOG_ERR, CANNOT_SET, errmsg);
 
   ODLFree(writeOutODL);
   return (-1);
  }
  return(0);

} /* buildJobStatusMsg */

/*----------------------------------------------------------
 * NAME:
 *  buildSubsys_Msg
 *
 * DESCRIPTION:
 *   top level routine used when a subsystem message is to be built
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
ODL buildSubsys_Msg(char *namePtr, sysQueElemType *sqel, subsysFilesType *sf)
{
 ODL writeOutODL;
 int retval, msgType, cat;
 char *charPtr, *msgStr,  *RDSPtr;

#ifdef SF_DEBUG
showSf_sf("ENTERING buildSubsys_Msg", sf);
#endif

  retval =  getReqMsgValues(sqel->sysODLreq); /* get all values out of msg */
  if (retval == -1) {
    ASFlogMessage(ASF_CP, WP_ERROR_BOX,  CANNOT_GET_JOB_REQ_VALUES_TEXT);
    return(NULL);
  }
  else
    msgType = retval;

  printfLLog(LOG_INFO, "Building message for subsystem %s\n", namePtr);
 
  switch(cat = GetSubsystemCategoryGivenName(namePtr)) {
    case RDS_CATEGORY_ID:
      if (msgType == FRAME_REQUEST_ID) {
        if ((writeOutODL = (ODL)GetNewMsg(SPS_DECODE_REQUEST)) == NULL)
          printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, SPS_DECODE_REQUEST);
        else {
          if (GetMediaFlagGivenJobId_SYSQUE(namePtr, sqel->jobId) != MEDIA_CHECK_NO)
            setObjectVal(writeOutODL, "BODY.CHECK_MEDIA_ID", "YES");
          else
            setObjectVal(writeOutODL, "BODY.CHECK_MEDIA_ID", "NO");
          retval = setUpCommonReqMsgValues(namePtr,writeOutODL, 
                         sqel->jobId);
          if (retval != -1)
            retval = buildSpsDecodeRequest(sqel->jobId, writeOutODL, sf,namePtr);
          if (retval == -1) {
           ASFlogMessage(ASF_CP,WP_ERROR_BOX, CANNOT_BUILD_MSG_TEXT, 
                         SPS_DECODE_REQUEST, sqel->jobId);
           ODLFree(writeOutODL);
           return(NULL);
          }
          else if (retval == -2) {
           ASFlogMessage(ASF_CP, WP_ERROR_BOX, ERROR_DEVICES_FULL, namePtr);
           ODLFree(writeOutODL);
           return(NULL);
          }
          else if (retval == -3) {
           ASFlogMessage(ASF_CP, WP_ERROR_BOX, ERROR_NO_SSP, namePtr);
           ODLFree(writeOutODL);
           return(NULL);
          }

        }
      }
      else if (msgType == SCAN_REQUEST_ID) {
        if ((writeOutODL = (ODL)GetNewMsg(SPS_SCAN_REQUEST)) == NULL)
          printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, SPS_SCAN_REQUEST);
        else {
          if (GetMediaFlagGivenJobId_SYSQUE(namePtr, sqel->jobId) != MEDIA_CHECK_NO)
            setObjectVal(writeOutODL, "BODY.CHECK_MEDIA_ID", "YES");
          else
            setObjectVal(writeOutODL, "BODY.CHECK_MEDIA_ID", "NO");
          retval = setUpCommonReqMsgValues(namePtr,writeOutODL,
                         sqel->jobId);
          if (retval != -1)
            retval = buildSpsScanRequest(sqel->jobId, writeOutODL, sf,namePtr);
          if (retval == -1) {
            ASFlogMessage(ASF_CP,WP_ERROR_BOX, CANNOT_BUILD_MSG_TEXT, 
                          SPS_SCAN_REQUEST, sqel->jobId);
           ODLFree(writeOutODL);
            return(NULL);
          }
          else if (retval == -2) {
           ASFlogMessage(ASF_CP, WP_ERROR_BOX, ERROR_DEVICES_FULL, namePtr);
           ODLFree(writeOutODL);
           return(NULL);
          }
        }
      }
      else {  /* invalid message type */
          printfLLog(LOG_ERR, CANNOT_DETERMINE, "message type");
          return(NULL);
      }

     break;

    case SSP2_CATEGORY_ID:
      if ((writeOutODL = (ODL)GetNewMsg(SPS_FRAME_REQUEST)) == NULL) {
         printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, SPS_FRAME_REQUEST);
         return(NULL);
      }

      retval = setUpCommonReqMsgValues(namePtr,writeOutODL,sqel->jobId);
      if (retval != -1)  {
        setObjectVal(writeOutODL, "BODY.CHECK_MEDIA_ID", "NO");
        RDSPtr = GetRDSname_MSTRQLIST(sqel->jobId);
        retval = buildSpsFrameRequest(sqel->jobId, writeOutODL, sf,RDSPtr);
      }
      if (retval == -1) {
        ASFlogMessage(ASF_CP,WP_ERROR_BOX, CANNOT_BUILD_MSG_TEXT, 
                      SPS_FRAME_REQUEST, sqel->jobId);
        ODLFree(writeOutODL);
        return(NULL);
      }
      else if (retval == -2) {
        ASFlogMessage(ASF_CP, WP_ERROR_BOX, ERROR_DEVICES_FULL, namePtr);
        ODLFree(writeOutODL);
        return(NULL);
      }
      break;

    case ASP_CATEGORY_ID:
      if (msgType == SCAN_REQUEST_ID) {
        if ((writeOutODL = (ODL)GetNewMsg(SPS_SCAN_REQUEST)) == NULL) {
          printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, SPS_FRAME_REQUEST);
          return(NULL);
        }
      }
      else if (msgType == FRAME_REQUEST_ID) {
        if ((writeOutODL = (ODL)GetNewMsg(SPS_FRAME_REQUEST)) == NULL) {
          printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, SPS_FRAME_REQUEST);
          return(NULL);
        }
      }
      else {
          printfLLog(LOG_ERR, CANNOT_DETERMINE, "message type");
          return(NULL);
      }

      if (GetMediaFlagGivenJobId_SYSQUE(namePtr, sqel->jobId) != MEDIA_CHECK_NO)
        setObjectVal(writeOutODL, "BODY.CHECK_MEDIA_ID", "YES");
      else
        setObjectVal(writeOutODL, "BODY.CHECK_MEDIA_ID", "NO");

      retval = setUpCommonReqMsgValues(namePtr,writeOutODL,sqel->jobId);
      if (retval != -1) 
        if (msgType == FRAME_REQUEST_ID) {
          sf->numFiles = 0;  /* for setting numProducts correctly */ 
          RDSPtr = GetRDSname_MSTRQLIST(sqel->jobId);
          retval = buildSpsFrameRequest(sqel->jobId, writeOutODL, sf,RDSPtr);
          msgStr = SPS_FRAME_REQUEST;
        }
        else if (msgType == SCAN_REQUEST_ID) {
          retval = buildSpsScanRequest(sqel->jobId, writeOutODL, sf,namePtr);
          msgStr = SPS_SCAN_REQUEST;
        }
        if (retval == -1) {
          ASFlogMessage(ASF_CP, WP_ERROR_BOX, CANNOT_BUILD_MSG_TEXT,
                      msgStr, sqel->jobId);
          ODLFree(writeOutODL);
          return(NULL);
        }
        else if (retval == -2) {
          ASFlogMessage(ASF_CP, WP_ERROR_BOX, ERROR_DEVICES_FULL, namePtr);
          ODLFree(writeOutODL);
          return(NULL);
        }

      break;
    case PPS_CATEGORY_ID:
      { int isRetry=0;

        if ((writeOutODL = (ODL)GetNewMsg(SPS_JOB_STATUS)) == NULL) {
          printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, SPS_JOB_STATUS);
          return(NULL);
        }

        retval = setUpCommonReqMsgValues(namePtr, writeOutODL, sqel->jobId);
        if (retval == -1) {
          ASFlogMessage(ASF_CP, WP_ERROR_BOX, CANNOT_BUILD_MSG_TEXT, 
                        STATUS_MESSAGE, sqel->jobId);
          ODLFree(writeOutODL);
          return(NULL);
        }
        isRetry = (GetRetry_MSTRQLIST(sqel->jobId) > 0 ) ? 1 : 0;
        retval = buildJobStatusMsg(sqel, writeOutODL, isRetry);
        if (retval == -1) {
          ASFlogMessage(ASF_CP,WP_ERROR_BOX, CANNOT_BUILD_MSG_TEXT,
                        getPPSname(), sqel->jobId);
          ODLFree(writeOutODL);
          return(NULL);
        }

      }
 
      break;

    case IMS_CATEGORY_ID:
      if ((writeOutODL = (ODL)GetNewMsg(CATALOG_REQUEST)) == NULL) {
        printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, CATALOG_REQUEST);
        return(NULL);
      }
      sf->numFiles=0;  /* don't expect any files to be written by ims */
      retval = buildCatalogMsg(writeOutODL, sqel->jobId, 
                 GetCatalogStep_MSTRQLIST(sqel->jobId), msgType, sf);
      if (retval == -1) {
        ASFlogMessage(ASF_CP, WP_ERROR_BOX, CANNOT_BUILD_MSG_TEXT, 
                      CATALOG_REQUEST, sqel->jobId);
        ODLFree(writeOutODL);
        return(NULL);
      }

      break;
    default:
      printfLLog(LOG_ERR, INVALID_SUBSYS_CATEGORY, namePtr, cat);
      return(NULL);
  } /* end switch */

  charPtr = ODLToStr(writeOutODL, NULL);
#ifdef PRINT_OUTGOING
  printf("CP sending out %s\n", charPtr); 
#endif
 
  ODLFree(charPtr);
#ifdef SF_DEBUG
showSf_sf("LEAVING buildSubsys_Msg", sf);
#endif
  return(writeOutODL);

} /* end buildSubsys_Msg */    


/*----------------------------------------------------------
 * NAME:
 *  readScanResults
 *
 * DESCRIPTION:
 *   read the scan results file and return as odl format
 *   also returns the frame odl object for the frame id requested
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

static ODL readScanResults(char *filename, int frameId, ODL writeOutODL, frameInfo *frame)
{
   int  i, j, k, l, *odlFrameId, err, segmentN, frameN, segment_id = 0, ok ;
   ODL  odl, *bodyODL, *segmtODL, *imageODL;
   char *errmsg, errstr[256];
   char *cval, *p;
   double *dval;
   int *ival;
   GMT_t *tval;

#ifdef BUILD_DEBUG
printf("readScanResults: frame %d %s\n", frameId, filename);
#endif
    p = (p = strchr(filename, ':')) ? p+1 : filename;


   if ((odl = ODLparse(p, 0, errstr)) == NULL) {
      printfLLog(LOG_ERR, CANNOT_OPEN_SCAN_RESULTS,  p);
      return(NULL);
   }
   printfLLog(LOG_INFO, "Reading Scan Results File %s:",  p);

    ODLGetVal(odl, SRF_ACTIVITY_ID, &GLOBAL_state.activityBuf);
    ODLGetVal(odl, SRF_RECORDER_ID, &GLOBAL_state.recorderBuf);
    ODLGetVal(odl, SRF_STATION_ID, &GLOBAL_state.stationID);
    ODLGetVal(odl, SRF_FRAME_MODE, &GLOBAL_state.frame_mode_buf);

    ODLGetVal(odl, SRF_START_ADDRESS, &GLOBAL_state.startAddr);
    ODLGetVal(odl, SRF_END_ADDRESS, &GLOBAL_state.endAddr);
    ODLGetVal(odl, SRF_START_TIME, &GLOBAL_state.startTime);
    ODLGetVal(odl, SRF_END_TIME, &GLOBAL_state.endTime);

#ifdef SRF_DEBUG
/* printf("readScanResults: about to set odl %s\n", ODLToStr(writeOutODL, NULL) );  */
#endif
    setObjectVal(writeOutODL, BODY_ACTIVITY_ID, GLOBAL_state.activityBuf);
    setObjectVal(writeOutODL, BODY_RECORDER_ID, GLOBAL_state.recorderBuf);
    setObjectVal(writeOutODL, BODY_STATION_ID, GLOBAL_state.stationID);
    setObjectVal(writeOutODL, BODY_FRAME_MODE, GLOBAL_state.frame_mode_buf);
    setObjectVal(writeOutODL, BODY_START_ADDRESS, GLOBAL_state.startAddr);
    setObjectVal(writeOutODL, BODY_END_ADDRESS, GLOBAL_state.endAddr);
    setObjectVal(writeOutODL, BODY_START_TIME, &GLOBAL_state.startTime);
    setObjectVal(writeOutODL, BODY_END_TIME, &GLOBAL_state.endTime);
#ifdef SRF_DEBUG
printf("readScanResults: just set odl %s\n", ODLToStr(writeOutODL, NULL) ); 
#endif

   frameN = 0;
   if ((bodyODL = (ODL*) Val(Lookup(odl, SRF_BODY))) == NULL) {
      printfLLog(LOG_ERR, SRF_CONTENT_ERROR, SRF_BODY);
      ODLFree(odl);
      return(NULL);
   }

  ok = 1;

  if (!ok) {
    printfLLog(LOG_ERR, ERROR_GETTING_REQ);
    printfLLog(LOG_DEBUG, DEB_ERROR_GETTING_REQ, "scan_results_file", errmsg);

    ODLFree(odl);
    return(NULL);
  }
#ifndef RETURN_SCAN_RESULTS_FRAME
  return(odl);  /* don't execute code after this point, for now */
#endif


/* this is code for extracting a specific frame from the scan results file */
/* currently the CP doesn't need to do this, but the code works */

   frame->found = 0; /* initialize to 'no good' */

   if (segmentN = ODLGetInt(odl, BODY_SEGMENT_COUNT,&err),
      err == -1) {
      printfLLog(LOG_ERR, SRF_CONTENT_ERROR, BODY_SEGMENT_COUNT);
      ODLFree(odl);
      return(NULL);
   }
  frame->segment = segmentN;

   for (i=0; bodyODL[i]!=NULL; ++i) {
                                                    /* find SEGMENT */
      if (strcasecmp(Name(bodyODL[i]), "SEGMENT") ||
          (segmtODL = (ODL*) Val(bodyODL[i])) == NULL) {
         continue;
      }
      segment_id++;
      for (j=0; segmtODL[j]!=NULL; j++) {

                                                       /* find IMAGE_FRAME */
         if (strcasecmp(Name(segmtODL[j]), "FRAME") ||
             (imageODL = (ODL*) Val(segmtODL[j])) == NULL)
            continue;
         else
            frameN++;
         /* find FRAME_ID from IMAGE_FRAME */
         for (k=0; imageODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(imageODL[k]), "FRAME_ID") == 0)
               break;
         }
         if (imageODL[k] == NULL) {
            printfLLog(LOG_ERR, SRF_CONTENT_ERROR, "FRAME_ID");
            ODLFree(odl);
            return(NULL);
         }
         odlFrameId = (int*) Val(imageODL[k]);

         if (*odlFrameId == frameId) { /* found the requested frame */

           frame->found = 1;

           for (l=0; imageODL[l] != NULL; l++) {   
             if (!strcasecmp(Name(imageODL[l]), START_ADDRESS) ) {
               ival = (int *) Val(imageODL[l]);
               frame->startAddr = *ival;
             }
             if (!strcasecmp(Name(imageODL[l]), END_ADDRESS) ) {
               ival = (int *) Val(imageODL[l]);
               frame->endAddr = *ival;
             }
             if (!strcasecmp(Name(imageODL[l]), START_TIME) ) {
	       tval = (GMT_t *) Val(imageODL[l]);
               frame->startTime.tv_sec = mkTime(&tval->tm); 
	       frame->startTime.tv_usec = tval->tv_usec;
             }
             if (!strcasecmp(Name(imageODL[l]), END_TIME) ) {
               tval = (GMT_t *) Val(imageODL[l]);
               frame->endTime.tv_sec = mkTime(&tval->tm); 
	       frame->endTime.tv_usec = tval->tv_usec;
             }

             if (!strcasecmp(Name(imageODL[l]), CENTER_LAT) ) {
               dval = (double *) Val(imageODL[l]);
               frame->centerLat = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), CENTER_LON) ) {
               dval = (double *) Val(imageODL[l]);
               frame->centerLon = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), CENTER_TIME) ) {
               tval = (GMT_t *) Val(imageODL[l]);
               frame->centerTime.tv_sec = mkTime(&tval->tm); 
	       frame->centerTime.tv_usec = tval->tv_usec;
             }
             if (!strcasecmp(Name(imageODL[l]), NEAR_START_LAT) ) {
               dval = (double *) Val(imageODL[l]);
               frame->nearStartLat = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), NEAR_START_LON) ) {
               dval = (double *) Val(imageODL[l]);
               frame->nearStartLon = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), NEAR_END_LAT) ) {
               dval = (double *) Val(imageODL[l]);
               frame->nearEndLat = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), NEAR_END_LON) ) {
               dval = (double *) Val(imageODL[l]);
               frame->nearEndLon = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), FAR_START_LAT) ) {
               dval = (double *) Val(imageODL[l]);
               frame->farStartLat = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), FAR_START_LON) ) {
               dval = (double *) Val(imageODL[l]);
               frame->farStartLon = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), FAR_END_LAT) ) {
               dval = (double *) Val(imageODL[l]);
               frame->farEndLat = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), FAR_END_LON) ) {
               dval = (double *) Val(imageODL[l]);
               frame->farEndLon = *dval;
             }
             if (!strcasecmp(Name(imageODL[l]), ASC_DESC) ) {
               cval = (char *) Val(imageODL[l]);
               frame->asc_desc = cval;
             }
                                                  /* now find sv record */
             if (!strcasecmp(Name(imageODL[l]), STATE_VECTOR_RECORD) ) {
               ODLGetVal(imageODL[l], NUMBER_OF_VECTORS,
                  &GLOBAL_state.sv_num_vectors);
               ODLGetVal(imageODL[l], SVR_SVM_PLATFORM,
                  &GLOBAL_state.sv_satBuf);
               ODLGetVal(imageODL[l], SVR_SVM_PRECISION,
                  &GLOBAL_state.sv_typeBuf);

               ODLGetVal(imageODL[l], SVR_SVM_COORDSYS,
                  &GLOBAL_state.sv_coordSysBuf);
               ODLGetVal(imageODL[l], SVR_SVM_REV, &GLOBAL_state.sv_rev);
               ODLGetVal(imageODL[l], SVR_SVD_X_POS, &GLOBAL_state.sv_x_pos);
               ODLGetVal(imageODL[l], SVR_SVD_Y_POS, &GLOBAL_state.sv_y_pos);
               ODLGetVal(imageODL[l], SVR_SVD_Z_POS, &GLOBAL_state.sv_z_pos);
               ODLGetVal(imageODL[l], SVR_SVD_X_VEL, &GLOBAL_state.sv_x_vel);
               ODLGetVal(imageODL[l], SVR_SVD_Y_VEL, &GLOBAL_state.sv_y_vel);
               ODLGetVal(imageODL[l], SVR_SVD_Z_VEL, &GLOBAL_state.sv_z_vel);
               ODLGetVal(imageODL[l], SVR_SVD_TIME, &GLOBAL_state.sv_time_val);


               setObjectVal(writeOutODL,BODY_SVR_NUM_VEC,
                  GLOBAL_state.sv_num_vectors);
               setObjectVal(writeOutODL, BODY_SVR_SVM_PLATFORM,
                  GLOBAL_state.sv_satBuf);
               setObjectVal(writeOutODL, BODY_SVR_SVM_PRECISION,
                  GLOBAL_state.sv_typeBuf);
               setObjectVal(writeOutODL, BODY_SVR_SVM_COORDSYS,
                  GLOBAL_state.sv_coordSysBuf);
               setObjectVal(writeOutODL, BODY_SVR_SVM_REV,
                  GLOBAL_state.sv_rev);
               setObjectVal(writeOutODL, BODY_SVR_SVD_X_POS,
                  GLOBAL_state.sv_x_pos);
               setObjectVal(writeOutODL, BODY_SVR_SVD_Y_POS,
                  GLOBAL_state.sv_y_pos);
               setObjectVal(writeOutODL, BODY_SVR_SVD_Z_POS,
                  GLOBAL_state.sv_z_pos);
               setObjectVal(writeOutODL, BODY_SVR_SVD_X_VEL,
                  GLOBAL_state.sv_x_vel);
               setObjectVal(writeOutODL, BODY_SVR_SVD_Y_VEL,
                  GLOBAL_state.sv_y_vel);
               setObjectVal(writeOutODL, BODY_SVR_SVD_Z_VEL,
                  GLOBAL_state.sv_z_vel);
               setObjectVal(writeOutODL, BODY_SVR_SVD_TIME,
                  &GLOBAL_state.sv_time_val);
 
               frame->sv_precision = GLOBAL_state.sv_typeBuf;

             }
           }


         }

      }
   }

   return(odl); 

} /* readScanResults.................*/
