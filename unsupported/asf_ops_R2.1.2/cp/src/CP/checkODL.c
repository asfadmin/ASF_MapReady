static char sccsid_checkODL_c[] =  "@(#)checkODL.c	1.9 97/08/01 09:30:04";


#include <sys/time.h>
#include <X11/Intrinsic.h>

#include "odl.h"
#include "cpdefines.h"

#include "cpbuildMsg.h"  /* for menuStateType */

extern menuStateType GLOBAL_state; 
#include "cpMsgs.h"
#include "validate.h"


#define inRange(val, min, max) (min <= val && max >= val) ? 1 : 0 


int domainCheckCommon (ODL msg, char *errmsg)
{
  char *str;
  int ival, odlval, retval =1;
  double dval;
  struct timeval tval;
  unsigned int uval;

#ifdef DEBUG
printf("domainCheckCommon: errmsg %s 0x%x at 0x%x\n", errmsg, errmsg, &errmsg);
#endif
  ival = ODLGetInt(msg, strcpy(errmsg,BODY_JOB_ID), &odlval);
  if (odlval == -1) 
    return 0;
  if (!inRange(ival, SPS_JOB_ID_MIN, SPS_JOB_ID_MAX))
    return(0);

  ival = ODLGetInt(msg, strcpy(errmsg, BODY_REV), &odlval);
  if (odlval == -1) 
    return 0;
  if (!inRange(ival, SPS_REV_MIN, SPS_REV_MAX))
    return(0);

#ifdef DEBUG
printf("domainCheckCommon: validating strings, errmsg %s \n", errmsg);
#endif


  if (!validateStr(msg, strcpy(errmsg,BODY_MODE), SPS_modes, 
      XtNumber(SPS_modes)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_PLATFORM), SPS_platforms, 
      XtNumber(SPS_platforms)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_INSERT_TOP), SPS_yesNoChoices, 
      XtNumber(SPS_yesNoChoices)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_ACTIVITY_ID), SPS_activityIDs, 
      XtNumber(SPS_activityIDs)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_MEDIA_TYPE), SPS_mediaTypes, 
      XtNumber(SPS_mediaTypes)))
    return(0);

/* state vector data object */

  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_SVR_SVD_X_POS), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_POSITION_MIN, SPS_POSITION_MAX))
    return(0);

  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_SVR_SVD_X_VEL), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_VELOCITY_MIN, SPS_VELOCITY_MAX))
    return(0);

  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_SVR_SVD_Y_POS), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_POSITION_MIN, SPS_POSITION_MAX))
    return(0);

  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_SVR_SVD_Y_VEL), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_VELOCITY_MIN, SPS_VELOCITY_MAX))
    return(0);

  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_SVR_SVD_Z_POS), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_POSITION_MIN, SPS_POSITION_MAX))
    return(0);

  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_SVR_SVD_Z_VEL), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_VELOCITY_MIN, SPS_VELOCITY_MAX))
    return(0);

/* state vector metadata object */
  if (!validateStr(msg, strcpy(errmsg,BODY_SVR_SVM_PRECISION), 
      SPS_sv_precisions, XtNumber(SPS_sv_precisions)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_SVR_SVM_PLATFORM), SPS_platforms, 
      XtNumber(SPS_platforms)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_SVR_SVM_COORDSYS), SPS_sv_coord_sys,
      XtNumber(SPS_sv_coord_sys)))
    return(0);
  ival = ODLGetInt(msg, strcpy(errmsg,BODY_SVR_SVM_REV), &odlval);
  if (odlval == -1) 
    return 0;
  if (!intInRange(ival, SPS_REV_MIN, SPS_REV_MAX))
    return(0);

/* time correlation object */
#ifdef TRUST_TC
  ival = ODLGetInt(msg, strcpy(errmsg,BODY_TC_REV), &odlval);
  if (odlval == -1) 
    return 0;
  if (!inRange(ival, 0, SPS_REV_MAX)) /* hack up b/c pps sends 0 */
    return(0);

  odlval = ODLGetVal(msg, strcpy(errmsg,BODY_TC_TIME), &tval);
  if (odlval)
    timeval_to_time(tval);
  else
    return(0);

  odlval = ODLGetVal(msg, strcpy(errmsg,BODY_TC_PLATFORM_TIME), &uval);
  if (!odlval)
    return(0);
#endif

/* gha correction object??? */


#ifdef DEBUG
printf("domainCheckCommon returning %d\n", retval);
#endif
  return(retval);

} /* domainCheckCommon */

int domainCheckScan (ODL msg, char *errmsg)
{
  char *str;
  int ival, odlval, retval =1;
  struct timeval tval;
  time_t startTime, endTime;
  int startAddr, endAddr;

#ifdef DEBUG
printf("domainCheckScan:\n");
#endif
/***** don't check station id values right now...  
  if (!validateStr(msg, strcpy(errmsg,BODY_STATION_ID), SPS_stationIDs, 
      XtNumber(SPS_stationIDs)))
    return(0);
************/
/***** don't check recorder id values right now... they're  in flux! 
  if (!validateStr(msg, strcpy(errmsg,BODY_RECORDER_ID), SPS_recorderIDs, 
      XtNumber(SPS_recorderIDs)))
    return(0);
************/
  if (!validateStr(msg, strcpy(errmsg,BODY_FRAME_MODE), SPS_frameModes, 
      XtNumber(SPS_frameModes)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg, BODY_DATA_DIRECTION), 
       SPS_dataDirections, XtNumber(SPS_dataDirections)))
    return(0);


/* start/end time checks */
  odlval = ODLGetVal(msg, strcpy(errmsg,BODY_START_TIME), &tval);
  if (odlval)
    startTime = timeval_to_time(tval);
  else
    return(0);
  odlval = ODLGetVal(msg, strcpy(errmsg,BODY_END_TIME), &tval);
  if (odlval)
    endTime = timeval_to_time(tval);
  else
    return(0);
  if (endTime <= startTime) {
    strcpy(errmsg , "Start time not less than end time");
    return(0);
  }

/* start/end addr checks */
  startAddr = ODLGetInt(msg, strcpy(errmsg,BODY_START_ADDRESS), &odlval);
  if (odlval == -1) {
    return(0);
  }
  endAddr = ODLGetInt(msg, strcpy(errmsg,BODY_END_ADDRESS), &odlval);
  if (odlval == -1)
    return(0);
  if (endAddr <= startAddr) {
    strcpy(errmsg , "Start address not less than end address");
    return(0);
  }


#ifdef DEBUG
printf("domainCheckScan SUCCESS\n");
#endif


  return(retval);

} /* domainCheckScan */

int domainCheckFrame (ODL msg, char *errmsg)
{
  char *str;
  double dval;
  int ival;
  unsigned int uval;
  int odlval, retval =1;
  struct timeval tval;

#ifdef DEBUG
printf("domainCheckFrame:%s\n", ODLToStr(msg, NULL) );
#endif

  if (!validateDouble(msg, strcpy(errmsg,BODY_PIXEL_SPACING), 
        SPS_pixelSpacings, XtNumber(SPS_pixelSpacings)))
    return(0);

  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_AVG_TERRAIN_HT), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_AVG_TERRAIN_HT_MIN, SPS_AVG_TERRAIN_HT_MAX))
    return(0);

  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_PS_REFERENCE_LAT), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_PS_REFERENCE_LAT_MIN, SPS_PS_REFERENCE_LAT_MAX))
    return(0);
  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_PS_REFERENCE_LON), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_PS_REFERENCE_LON_MIN, SPS_PS_REFERENCE_LON_MAX))
    return(0);


  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_LAMBERT_LAT_N), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_LAMBERT_LAT_N_MIN, SPS_LAMBERT_LAT_N_MAX))
    return(0);
  dval = ODLGetDouble(msg, strcpy(errmsg,BODY_LAMBERT_LAT_S), &odlval);
  if (odlval == -1) 
    return 0;
  if (!doubleInRange(dval, SPS_LAMBERT_LAT_S_MIN, SPS_LAMBERT_LAT_S_MAX))
    return(0);

  ival = ODLGetInt(msg, strcpy(errmsg,BODY_FRAME_ID), &odlval);
  if (odlval == -1) 
    return 0;
  if (!intInRange(ival, SPS_FRAME_ID_MIN, SPS_FRAME_ID_MAX))
    return(0);
  ival = ODLGetInt(msg, strcpy(errmsg,BODY_PROCESSING_GAIN), &odlval);
  if (odlval == -1) 
    return 0;
  if (!intInRange(ival, SPS_PROCESSING_GAIN_MIN, SPS_PROCESSING_GAIN_MAX))
    return(0);
#ifdef DO_SUBFRAME
  ival = ODLGetInt(msg, strcpy(errmsg,BODY_SUBFRAME_ID), &odlval);
  if (odlval == -1) 
    return 0;
  if (!intInRange(ival, SPS_SUBFRAME_ID_MIN, SPS_SUBFRAME_ID_MAX))
    return(0);
#endif
#ifdef DO_UTM
  ival = ODLGetInt(msg, strcpy(errmsg,BODY_UTM_ZONE), &odlval);
  if (odlval == -1) 
    return 0;
  if (!intInRange(ival, SPS_UTM_ZONE_MIN, SPS_UTM_ZONE_MAX))
    return(0);
#endif

  if (!validateStr(msg, strcpy(errmsg,BODY_PRODUCT_TYPE), SPS_productTypes, 
      XtNumber(SPS_productTypes)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_PROJECTION), SPS_projections, 
      XtNumber(SPS_projections)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_OUTPUT_FORMAT), SPS_outputFormats, 
      XtNumber(SPS_outputFormats)))
    return(0);

  if (!validateStr(msg, strcpy(errmsg,BODY_PRODUCT_TYPE), SPS_productTypes, 
      XtNumber(SPS_productTypes)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_TERRAIN_CORRECTION), 
      SPS_yesNoChoices, XtNumber(SPS_yesNoChoices)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg,BODY_DESKEW), SPS_yesNoChoices, 
      XtNumber(SPS_yesNoChoices)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg, BODY_COMPENSATION_FLAG), 
       SPS_yesNoChoices, XtNumber(SPS_yesNoChoices)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg, BODY_QUICKLOOK_FLAG), 
       SPS_yesNoChoices, XtNumber(SPS_yesNoChoices)))
    return(0);
  if (!validateStr(msg, strcpy(errmsg, BODY_DATA_DIRECTION), 
       SPS_dataDirections, XtNumber(SPS_dataDirections)))
    return(0);

#ifdef DEBUG
printf("domainCheckFrame returning %d\n", retval);
#endif
  return(retval);

}



int domainCheck(ODL msg, char *errmsg)
{
    int jobId , retval;
    char *s, *msgType, *msgMode, *modeStr, *namePtr;

#ifdef DEBUG
printf("entering domainCheck\n");
#endif

   msgType = ODLGetString(msg, "COMMON_HEADER.MSG_TYPE", &retval);
   if (msgType == NULL)  {
     printf("couldn't read COMMON_HEADER.MSG_TYPE\n");
     return(0);
    }

   msgMode = ODLGetString(msg, "BODY.MODE", &retval);
   if (msgMode == NULL) {
     printf("couldn't read BODY.MODE\n");
     free(msgType);
     return(0);
    }


   ODLGetInt(msg, "BODY.JOB_ID", &jobId);

#ifdef DEBUG
printf("calling domainCheckCommon , errmsg %s \n", errmsg);
#endif
   retval = domainCheckCommon(msg, errmsg);
   if (!retval) {
#ifdef DEBUG
      printf("domainCheck FAILURE, errmsg %s \n", errmsg);
#endif
      free(msgMode);
      free(msgType);
      return(0);
   }

   if (strncmp(msgType, "FRAME", 5) == NULL) {
         retval = domainCheckFrame(msg, errmsg);
   }
   else if (strncmp(msgType, "SCAN", 4) == NULL) {
         retval = domainCheckScan(msg, errmsg);
   }

#ifdef DEBUG
printf("domainCheck returning %d, errmsg %s\n", retval, errmsg);
#endif
  free(msgMode);
  free(msgType);
  return(retval);

} /* domainCheck */
