static char sccsid_profile_c[] = "@(#)profile.c	1.12 97/01/27 18:33:20";


#include <sys/time.h>
#include <sys/syslog.h>

#include "odl.h"
#include "cpdefines.h"
#include "cpconfig.h"
#include "validate.h"
#include "logUtils.h"
#include "cplogs.h"

#define CAL_PARAMS_SSS_LEN  3

#define CAL_PARAMS_SSS_ASP "ASP"
#define CAL_PARAMS_SSS_SSP "SSP"
#define CAL_PARAMS_SSS_PP0 "PP0"

#define CAL_PARAMS_SSS_E1A "E1A"
#define CAL_PARAMS_SSS_E2A "E2A"
#define CAL_PARAMS_SSS_J1A "J1A"

#define CAL_PARAMS_SSS_E1P "E1P"
#define CAL_PARAMS_SSS_E2P "E2P"
#define CAL_PARAMS_SSS_J1P "J1P"


static char *getCalParamsCategory(char *calParamsFile)
{
#ifdef DEBUG
printf("getCalParamsCategory: %s\n", calParamsFile);
#endif
  if ((strncmp(calParamsFile, CAL_PARAMS_SSS_E1A, CAL_PARAMS_SSS_LEN) == 0) ||
      (strncmp(calParamsFile, CAL_PARAMS_SSS_E2A, CAL_PARAMS_SSS_LEN) == 0) ||
      (strncmp(calParamsFile, CAL_PARAMS_SSS_J1A, CAL_PARAMS_SSS_LEN) == 0) )
    return(ASP_CATEGORY);

  if ((strncmp(calParamsFile, CAL_PARAMS_SSS_E1P, CAL_PARAMS_SSS_LEN) == 0) ||
      (strncmp(calParamsFile, CAL_PARAMS_SSS_E2P, CAL_PARAMS_SSS_LEN) == 0) ||
      (strncmp(calParamsFile, CAL_PARAMS_SSS_J1P, CAL_PARAMS_SSS_LEN) == 0) )
    return(PP_CATEGORY);

/* radarsat */
  if ((strncmp(calParamsFile, CAL_PARAMS_SSS_ASP, CAL_PARAMS_SSS_LEN) == 0) )
    return(ASP_CATEGORY);
  if ((strncmp(calParamsFile, CAL_PARAMS_SSS_SSP, CAL_PARAMS_SSS_LEN) == 0) )
    return(SSP2_CATEGORY);
  if ((strncmp(calParamsFile, CAL_PARAMS_SSS_PP0, CAL_PARAMS_SSS_LEN) == 0) )
    return(PP_CATEGORY); /* is this right?  PP0 ==> PP? */

  return(NULL);
}


char *chooseCalParamsFile(char *cp1, char *cp2, char *processor)
{
  int len;
  char *p;

#ifdef DEBUG
printf("chooseCalParamsFile: cp1 %s cp2 %s proc %s\n", cp1, cp2, processor);
#endif

  p = getCalParamsCategory(cp1);
#ifdef DEBUG
printf("getCalParamsCategory for %s returned %s\n", processor, p);
#endif
  if (p != NULL) {
    len = Min (strlen(p), CAL_PARAMS_SSS_LEN);
    if (strncmp(p,  processor, len) == 0)
      return(cp1);
  }

  if (cp2 != NULL) {
    p = getCalParamsCategory(cp2);
#ifdef DEBUG
printf("getCalParamsCategory for %s returned %s\n", processor, p);
#endif
    if (p != NULL) {
      len = Min (strlen(p), CAL_PARAMS_SSS_LEN);
      if (strncmp(p,  processor, len) == 0)
        return(cp2);
    }

  }

  return(NULL);
}

int calParamsMatch(char *cp, char *processor)
{
  int len;
  char *p;

#ifdef DEBUG
printf("calParamsMatch: cp %s processor %s\n", cp, processor);
#endif
  if (cp == NULL)
    return(0);
  p = getCalParamsCategory(cp);
  if (p != NULL) {
    len = Min (strlen(p), CAL_PARAMS_SSS_LEN);

    if (strncmp(p,  processor, len) == 0)
      return(1);
  }

  return(0);
}


char *determineProcessor(ODL msg, char *errmsg)
{
  int jp, retval, jobId;
  int numModes=0, numPlatforms=0, numProductTypes=0, 
      numQflags=0, numCflags=0, numSubframeIds=0, numPixelSpacings=0;
  int numFrameModes=0, numDeskews, numProjections, numTerrainCorrections;
  char **productTypes, **qflags, **cflags, **modes, **platforms, *processor;
  char **frame_modes, **deskews, **projections, **terrainCorrections;
  char *msgType, *cp1, *cp2;
  int *subframeIds;
  double *pixelSpacings;

  jobId = ODLGetInt(msg, BODY_JOB_ID, &retval);
  printfLLog(LOG_DEBUG, "Performing job profile check for job %d", jobId);

  strcpy(errmsg, "");

   msgType = ODLGetStr(msg, "COMMON_HEADER.MSG_TYPE");
   if (msgType == NULL)  {
     printfLLog(LOG_ERR, "couldn't read COMMON_HEADER.MSG_TYPE");
     return(0);
    }

  if (strncmp(msgType, "SCAN", 4) == NULL) {
    return("DEFAULT/SCAN");
  }

  cp1 = ODLGetStr(msg, BODY_CALPARMS_FILE);
  cp2 = ODLGetStr(msg, BODY_CALPARMS_FILE_2);
  if (cp1 == NULL) {
    strcpy(errmsg, NO_CAL_PARAM_FILE);
    return(0);
  }
  printfLLog(LOG_DEBUG, "Primary cal params file %s for job %d", cp1, jobId);
  if (cp2 != NULL)
    printfLLog(LOG_DEBUG, "Secondary cal params file %s for job %d",cp2,jobId);


  for (jp=0; jp < getNumJobProfiles(); jp++) {
#ifdef DEBUG
    printfLLog(LOG_DEBUG, "Checking job profile %d\n", jp); 
#endif

    modes = getJobProfileModes(jp, &numModes);
    printStringArray("modes", numModes, modes);

    productTypes = getJobProfileProductTypes(jp, &numProductTypes);
    printStringArray("product types", numProductTypes, productTypes);

    platforms = getJobProfilePlatforms(jp, &numPlatforms);
    printStringArray("platforms", numPlatforms, platforms);

    qflags = getJobProfileQuicklookFlags(jp, &numQflags);
    printStringArray("quicklook flags", numQflags, qflags);

    cflags = getJobProfileCompensationFlags(jp, &numCflags);
    printStringArray("compensation flags", numCflags, cflags);

    pixelSpacings = getJobProfilePixelSpacings(jp, &numPixelSpacings);
    printDoubleArray("pixelSpacings", numPixelSpacings, pixelSpacings);

    subframeIds = getJobProfileSubframeIds(jp, &numSubframeIds);
    printIntArray("subframe ids", numSubframeIds, subframeIds);

    frame_modes = getJobProfileFrameModes(jp, &numFrameModes);
    printStringArray("frame_modes", numFrameModes, frame_modes);

    deskews = getJobProfileDeskews(jp, &numDeskews);
    printStringArray("deskews", numDeskews, deskews);

    projections = getJobProfileProjections(jp, &numProjections);
    printStringArray("projections", numProjections, projections);

    terrainCorrections = getJobProfileTerrainCorrections(jp, &numTerrainCorrections);
    printStringArray("terrainCorrections", numTerrainCorrections, terrainCorrections);


    if (validateStr(msg, BODY_MODE, modes, numModes) &&
        validateStr(msg, BODY_PLATFORM, platforms, numPlatforms) &&
        validateStr(msg, BODY_PRODUCT_TYPE, productTypes, numProductTypes) &&
        validateStr(msg, BODY_QUICKLOOK_FLAG, qflags, numQflags) &&
        validateStr(msg, BODY_FRAME_MODE, frame_modes, numFrameModes) &&
        validateStr(msg, BODY_DESKEW, deskews, numDeskews) &&
        validateStr(msg, BODY_PROJECTION, projections, numProjections) &&
        validateStr(msg, BODY_TERRAIN_CORRECTION, terrainCorrections, numTerrainCorrections) &&
        validateStr(msg, BODY_COMPENSATION_FLAG, cflags, numCflags) &&
        validateInt(msg, BODY_SUBFRAME_ID, subframeIds, numSubframeIds) &&
        validateDouble(msg,BODY_PIXEL_SPACING,pixelSpacings,numPixelSpacings)){
     /* now compare cal params-selected processor against cp-determined one */
      processor = getJobProfileSubsystem(jp);
      printfLLog(LOG_DEBUG, "Profile selected processor %s for job %d", 
                  processor, jobId);
#ifdef DEBUG
printf("i %d found match, now comparing CP-processor %s, errmsg %.6s\n\tcp1 %s cp2 %s\n", jp, processor, errmsg, cp1, cp2);
#endif
      if (calParamsMatch(cp1, processor) ) { /* if primary cal params matched */
        printfLLog(LOG_DEBUG, "Cal params %s chosen for job %d", cp1, jobId);
        return(processor);                   /* return the processor name */
        }
      else if (cp2 != NULL && strcmp(cp2,"")) { /* if there is a secondary */
        printfLLog(LOG_DEBUG, "Cal params %s invalid for job %d", cp1, jobId);
        if (calParamsMatch(cp2, processor) ) { /* and it happens to match  */
          printfLLog(LOG_DEBUG, "Cal params %s chosen for job %d", cp2, jobId);
          return(processor);                  /* return the processor name; */
        }
      }
                                          /* otherwise go back and check */
      strcpy(errmsg, CAL_PARAM_MISMATCH); /* the other profiles for a match */
      }
  } 
/* if got to here, no profiles matched, or the cal params processor did */
/*   not match the processor in the any selected */

/* if errmsg is already set, it was a cal params mismatch */
/* if errmsg is not set, it was a job profile mismatch */

  if (strcmp(errmsg, "") == 0) {
    printfLLog(LOG_DEBUG, "No job profiles matched for job %d", jobId);
    strcpy(errmsg, JOB_PROFILE_MISMATCH);
  }
  else {
    printfLLog(LOG_DEBUG, "Cal params mismatched for job %d", jobId);
  }


  return(NULL);
} /* determineProcessor */
