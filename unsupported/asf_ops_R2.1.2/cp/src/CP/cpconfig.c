/* #define ALLOW_ZERO_PPS_IMS /* for testing; allow no pps or ims object */

static char sccsid_cpconfig_c[] = "@(#)cpconfig.c	4.56 97/01/03 15:52:28";

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <fcntl.h> /* open */
#include <unistd.h> /* read */
#include <stdlib.h> /* malloc, free */

#include <syslog.h>

#include "asf.h"
#include "cpdefines.h"
#include "cpconfig.h"
#include "cplogs.h"
#include "memUtils.h"  /* doMalloc */
#include "utils.h"  /* for convertEnvFile() prototype */
#include "cprtns.h"

/* start global defines */

static subsysConfigType  GLOBAL_subsysConfig;
static char GLOBAL_portStr[10];

/* this macro sets a directory string to a certain value if the dir was NULL */
/* to prevent the directory-based CP commands from errors... */

#define setValidDir(dir, retval, defValue) \
   if (dir == NULL || retval == -1)  \
     dir = convertEnvFile(defValue); 

/* end global defines */

extern int ODLGetStringArray(); /* should be defined in library's odl.h */

char *getRDSHost(char * buf, char *namePtr)
{
  int whichRDS;
  
  printf("namePtr = %s", namePtr);
  whichRDS = getRDSnumGivenName(namePtr);
  printf(" whichRDS= %d, host = %s\n", whichRDS, (char *)GLOBAL_subsysConfig.RDShostPtr[whichRDS]);
  strcpy (buf, (char *)GLOBAL_subsysConfig.RDShostPtr[whichRDS]);
  return (0);

} /* end getRDSHost */


/*----------------------------------------------------------
 * NAME:
 *  getNumJobProfiles
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getNumJobProfiles()
{
  return(GLOBAL_subsysConfig.numJobProfiles);
} /* end getNumJobProfiles */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfileSubsystem
 *
 * DESCRIPTION:  returns subsystem name for a particular profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char * getJobProfileSubsystem(int which)
{
  return(GLOBAL_subsysConfig.jobProfileSubsystem[which]);
} /* end getJobProfileSubsystem */


/*----------------------------------------------------------
 * NAME:
 *  getJobProfilePixelSpacings
 *
 * DESCRIPTION: returns an array containing the valid
 *              pixel spacings for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

double *getJobProfilePixelSpacings(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_pixel_spacings[which];
  return(GLOBAL_subsysConfig.jobProfilePixelSpacings[which]);

} /* getJobProfilePixelSpacings */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfileSubframeIds 
 *
 * DESCRIPTION: returns an array containing the valid
 *              subframe ids for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

int *getJobProfileSubframeIds(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_subframe_ids[which];
  return(&GLOBAL_subsysConfig.jobProfileSubframeIds[which]); 

} /* getJobProfileSubframeIds */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfilePlatforms
 *
 * DESCRIPTION: returns an array containing the valid
 *              platforms for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char ** getJobProfilePlatforms(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_platforms[which];
  return(GLOBAL_subsysConfig.jobProfilePlatforms[which]);

} /* getJobProfilePlatforms */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfileModes
 *
 * DESCRIPTION: returns an array containing the valid
 *              modes for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char ** getJobProfileModes(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_modes[which];
  return(GLOBAL_subsysConfig.jobProfileModes[which]);

} /* getJobProfileModes */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfileProductTypes
 *
 * DESCRIPTION: returns an array containing the valid
 *              product types for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char ** getJobProfileProductTypes(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_product_types[which];
  return(GLOBAL_subsysConfig.jobProfileProductTypes[which]);

} /* getJobProfileProductTypes */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfileQuicklookFlags
 *
 * DESCRIPTION: returns an array containing the valid
 *              quicklook flags for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char ** getJobProfileQuicklookFlags(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_quicklook_flags[which];
  return(GLOBAL_subsysConfig.jobProfileQuicklookFlags[which]);

} /* getJobProfileQuicklookFlags */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfileCompensationFlags
 *
 * DESCRIPTION: returns an array containing the valid
 *              compensation flags for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char ** getJobProfileCompensationFlags(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_compensation_flags[which];
  return(GLOBAL_subsysConfig.jobProfileCompensationFlags[which]);

} /* getJobProfileCompensationFlags */


/*----------------------------------------------------------
 * NAME:
 *  getJobProfileFrameModes
 *
 * DESCRIPTION: returns an array containing the valid
 *              frame modes for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char ** getJobProfileFrameModes(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_frame_modes[which];
  return(GLOBAL_subsysConfig.jobProfileFrameModes[which]);

} /* getJobProfileFrameModes */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfileDeskews
 *
 * DESCRIPTION: returns an array containing the valid
 *              deskew values for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char ** getJobProfileDeskews(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_deskews[which];
  return(GLOBAL_subsysConfig.jobProfileDeskews[which]);

} /* getJobProfileDeskews */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfileProjections
 *
 * DESCRIPTION: returns an array containing the valid
 *              projections for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char ** getJobProfileProjections(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_projections[which];
  return(GLOBAL_subsysConfig.jobProfileProjections[which]);

} /* getJobProfileProjections */

/*----------------------------------------------------------
 * NAME:
 *  getJobProfileTerrainCorrections
 *
 * DESCRIPTION: returns an array containing the valid
 *              terrain correction values for a particular job profile
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char ** getJobProfileTerrainCorrections(int which, int *howMany)
{
  *howMany = GLOBAL_subsysConfig.n_terrain_corrections[which];
  return(GLOBAL_subsysConfig.jobProfileTerrainCorrections[which]);

} /* getJobProfileTerrainCorrections */





/*----------------------------------------------------------
 * NAME:
 *  freeCPconfig()
 *
 * DESCRIPTION:
 *  free everything in the config structure
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void freeCPconfig()
{
  int i;

  doFree(GLOBAL_subsysConfig.configFilePtr);
  doFree(GLOBAL_subsysConfig.printCmd);
  doFree(GLOBAL_subsysConfig.logBrowserCmd);
  doFree(GLOBAL_subsysConfig.cal_params_dirPtr);
  doFree(GLOBAL_subsysConfig.CPscriptsDirPtr);
  doFree(GLOBAL_subsysConfig.cp_current_statefile);
  doFree(GLOBAL_subsysConfig.cp_saved_basename);
  doFree(GLOBAL_subsysConfig.cp_last_saved_statefile);
  doFree(GLOBAL_subsysConfig.cp_autoload_state);
  doFree(GLOBAL_subsysConfig.cp_err_logfile);
  doFree(GLOBAL_subsysConfig.cp_info_logfile);
  doFree(GLOBAL_subsysConfig.cp_debug_logfile);
  doFree(GLOBAL_subsysConfig.sps_err_logfile);
  doFree(GLOBAL_subsysConfig.sps_info_logfile);
  doFree(GLOBAL_subsysConfig.sps_debug_logfile);
  doFree(GLOBAL_subsysConfig.sps_retain_products);
  doFree(GLOBAL_subsysConfig.sps_J1_routing);
  doFree(GLOBAL_subsysConfig.GPRlogicalPtr);
  doFree(GLOBAL_subsysConfig.GPRpathPtr);
  doFree(GLOBAL_subsysConfig.GPRexeNamePtr);
  doFree(GLOBAL_subsysConfig.GPRtemplatePtr);
  doFree(GLOBAL_subsysConfig.GPRconfigFilePtr);
  doFree(GLOBAL_subsysConfig.QCexeNamePtr);
  doFree(GLOBAL_subsysConfig.QChostPtr);
  doFree(GLOBAL_subsysConfig.QCtemplatePtr);
  doFree(GLOBAL_subsysConfig.QCdisplayPtr);
  doFree(GLOBAL_subsysConfig.ScanQCpathPtr);
  doFree(GLOBAL_subsysConfig.ScanQCexeNamePtr);
  doFree(GLOBAL_subsysConfig.ScanQChostPtr);
  doFree(GLOBAL_subsysConfig.ScanQCtemplatePtr);
  doFree(GLOBAL_subsysConfig.ScanQCdisplayPtr);

  doFree(GLOBAL_subsysConfig.PreQCpathPtr);
  doFree(GLOBAL_subsysConfig.PreQCexeNamePtr);
  doFree(GLOBAL_subsysConfig.PreQChostPtr);
  doFree(GLOBAL_subsysConfig.PreQCtemplatePtr);

  for (i=0; i < GLOBAL_subsysConfig.RDSnum; i++) {
  doFree(GLOBAL_subsysConfig.RDSlogicalPtr[i]);
  doFree(GLOBAL_subsysConfig.RDSbackground[i]);
  doFree(GLOBAL_subsysConfig.RDSpathPtr[i]);
  doFree(GLOBAL_subsysConfig.RDSexeNamePtr[i]);
  doFree(GLOBAL_subsysConfig.RDShostPtr[i]);
  doFree(GLOBAL_subsysConfig.RDS_CP_hostNamePtr[i]);
  doFree(GLOBAL_subsysConfig.RDStemplatePtr[i]);
  doFree(GLOBAL_subsysConfig.RDSconfigFilePtr[i]);
  /* doFree(GLOBAL_subsysConfig.RDSsaveDirPtr[i]); */
  }

  for (i=0; i < GLOBAL_subsysConfig.SSP2num; i++) {
    doFree(GLOBAL_subsysConfig.SSP2logicalPtr[i]);
    doFree(GLOBAL_subsysConfig.SSP2background[i]);
    doFree(GLOBAL_subsysConfig.SSP2pathPtr[i]);
    doFree(GLOBAL_subsysConfig.SSP2exeNamePtr[i]);
    doFree(GLOBAL_subsysConfig.SSP2hostPtr[i]);
    doFree(GLOBAL_subsysConfig.SSP2_CP_hostNamePtr[i]);
    doFree(GLOBAL_subsysConfig.SSP2templatePtr[i]);
    doFree(GLOBAL_subsysConfig.SSP2configFilePtr[i]);
    /* doFree(GLOBAL_subsysConfig.SSP2saveDirPtr[i]); */
  }
  doFree(GLOBAL_subsysConfig.ASPlogicalPtr);
  doFree(GLOBAL_subsysConfig.ASPbackground);
  doFree(GLOBAL_subsysConfig.ASPpathPtr);
  doFree(GLOBAL_subsysConfig.ASPexeNamePtr);
  doFree(GLOBAL_subsysConfig.ASPhostPtr);
  doFree(GLOBAL_subsysConfig.ASP_CP_hostNamePtr);
  doFree(GLOBAL_subsysConfig.ASPtemplatePtr);
  doFree(GLOBAL_subsysConfig.ASPconfigFilePtr);
  /* doFree(GLOBAL_subsysConfig.ASPsaveDirPtr); */

  doFree(GLOBAL_subsysConfig.PPSlogicalPtr);
  doFree(GLOBAL_subsysConfig.PPSpathPtr);
  doFree(GLOBAL_subsysConfig.PPSexeNamePtr);
  doFree(GLOBAL_subsysConfig.PPShostPtr);
  doFree(GLOBAL_subsysConfig.PPS_CP_hostNamePtr);
  doFree(GLOBAL_subsysConfig.PPStemplatePtr);
  doFree(GLOBAL_subsysConfig.PPSconfigFilePtr);

  for (i=0; i < GLOBAL_subsysConfig.IMSnum; i++) {
     doFree(GLOBAL_subsysConfig.IMSlogicalPtr[i]); 
     doFree(GLOBAL_subsysConfig.IMSpathPtr[i]);
     doFree(GLOBAL_subsysConfig.IMSexeNamePtr[i]);
     doFree(GLOBAL_subsysConfig.IMShostPtr[i]);
     doFree(GLOBAL_subsysConfig.IMS_CP_hostNamePtr[i]);
     doFree(GLOBAL_subsysConfig.IMStemplatePtr[i]);
     doFree(GLOBAL_subsysConfig.IMSconfigFilePtr[i]);
  }

}




/*----------------------------------------------------------
 * NAME:
 *  getCPconfig()
 *
 *
 * DESCRIPTION:
 *  get the pointer to the whole structure for quick retrevial
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
subsysConfigType *getCPconfig()
{
 return(&GLOBAL_subsysConfig);
}

int isDCRSI(char *type)
{
  if (strcmp(type,"DCRSI") == 0) {
     printf("YES--- DCRSI\n");
     return(True);
  }
  else {
     printf("NO--- %s\n",type);
     return(False);
  }
}
/*----------------------------------------------------------
 * NAME:
 *  special_J1_routing
 *
 * DESCRIPTION:
 *  returns True if J1 jobs are to be routed specially to one
 *  subsystem for scanning; returns False if any subsystem can
 *  scan a J1 job
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int special_J1_routing()
{
  if (strcmp(GLOBAL_subsysConfig.sps_J1_routing, "SINGLE") == 0)
    return(True);
  else
    return(False);

} /* end special_J1_routing...................................*/

/*----------------------------------------------------------
 * NAME:
 *  retainProducts
 *
 * DESCRIPTION:
 *  returns True if products are to be retained after the final
 *  status message is sent to PPS; returns False if products 
 *  are to be deleted after status is sent to PPS
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int retainProducts()
{
  if (strcmp(GLOBAL_subsysConfig.sps_retain_products, "YES") == 0)
    return(True);
  else
    return(False);

} /* end retainProducts...................................*/

/*----------------------------------------------------------
 * NAME:
 *  autoLoadState
 *
 * DESCRIPTION:
 *  returns True if previous state file is to be loaded 
 *  automatically at startup; returns False if master queue
 *  is to come up empty
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int autoLoadState()
{
  if (strcmp(GLOBAL_subsysConfig.cp_autoload_state, "YES") == 0)
    return(True);
  else
    return(False);

} /* end autoLoadState...................................*/


/*----------------------------------------------------------
 * NAME:
 *  getPortID
 *
 * DESCRIPTION:
 *  an access function that returns the portID to be used
 *  throughout the system as an integer.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getPortID()
{
  return(GLOBAL_subsysConfig.portId);

} /* end getPortID........................................*/


/*----------------------------------------------------------
 * NAME:
 *  getPortIDString
 *
 *
 * DESCRIPTION:
 *  an access function that returns the portID to be used
 *  throughout the system as a string.
 *
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getPortIDString()
{
  sprintf(GLOBAL_portStr, "%d", GLOBAL_subsysConfig.portId);
  return(GLOBAL_portStr);

} /* end getPortIDString...................................*/

/*----------------------------------------------------------
 * NAME:
 *  getScanResultsDir
 *
 * DESCRIPTION:
 *  an access function that returns the scan results directory
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getScanResultsDir()
{
  return(GLOBAL_subsysConfig.scan_results_dirPtr);

} /* end getScanResultsDir...................................*/

/*----------------------------------------------------------
 * NAME:
 *  getCalParamsDir
 *
 * DESCRIPTION:
 *  an access function that returns the scan results directory
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getCalParamsDir()
{
  return(GLOBAL_subsysConfig.cal_params_dirPtr);

} /* end getCalParamsDir...................................*/






/*----------------------------------------------------------
 * NAME:
 *  getPrintCommand
 *
 * DESCRIPTION:
 *  an access function that returns the command to be used
 *  for printing screen dumps
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getPrintCommand()
{
  return(GLOBAL_subsysConfig.printCmd);

} /* end getPrintCommand...................................*/


/*----------------------------------------------------------
 * NAME:
 *  getLogBrowserCmd
 *
 * DESCRIPTION:
 *  an access function that returns the command to be used
 *  for bringing up the log browser application
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getLogBrowserCmd()
{
  return(GLOBAL_subsysConfig.logBrowserCmd);

} /* end getLogBrowserCmd...................................*/


/*----------------------------------------------------------
 * NAME:
 *  getSaveDir
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getSaveDir()
{
  return(GLOBAL_subsysConfig.CPsaveDirPtr);

} /* end getSaveDir */

/*----------------------------------------------------------
 * NAME:
 *  getRestoreDir
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getRestoreDir()
{
  return(GLOBAL_subsysConfig.CPrestoreDirPtr);

} /* end getRestoreDir */

/*----------------------------------------------------------
 * NAME:
 *  getSavedStateFile
 *
 * DESCRIPTION:
 *   return either the current cp saved state file or the last one saved
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getSavedStateFile(int which)
{
  if (which == LAST_STATE_FILE)
    return(GLOBAL_subsysConfig.cp_last_saved_statefile);
  else if (which == CURRENT_STATE_FILE) 
    return(GLOBAL_subsysConfig.cp_current_statefile);
  else if (which == CURRENT_STATE_BASENAME)
    return(GLOBAL_subsysConfig.cp_saved_basename);
  
  return(NULL);

} /* end getSavedStateFile */





/*----------------------------------------------------------
 * NAME:
 *  getGPRname
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getGPRname()
{
  return(GLOBAL_subsysConfig.GPRlogicalPtr);

} /* end getGPRname */

/*----------------------------------------------------------
 * NAME:
 *  getNextRDSname
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getNextRDSname(int thisRDS)
{
  int lastRDS = thisRDS+1;


  if (lastRDS >= GLOBAL_subsysConfig.RDSnum)
    lastRDS = 0;
  if (GLOBAL_subsysConfig.RDSnum)
    return(getRDSname(lastRDS));

  return(NULL);

} /* end getNextRDSname */


/*----------------------------------------------------------
 * NAME:
 *  getRDSname
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getRDSname(int whichRDS)
{
  if (whichRDS == -1) 
     return (NULL);
  return(GLOBAL_subsysConfig.RDSlogicalPtr[whichRDS]);

} /* end getRDSname */

/*----------------------------------------------------------
 * NAME:
 *  getRDSmediaType
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getRDSmediaType(int whichRDS)
{
  return(GLOBAL_subsysConfig.RDSmediaType[whichRDS]);

} /* end getRDSMediaType */


/*----------------------------------------------------------
 * NAME:
 *  getNumRDSs
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getNumRDSs()
{
  return(GLOBAL_subsysConfig.RDSnum);
} /* end getNumRDSs */

/*----------------------------------------------------------
 * NAME:
 *  getNumSSP2s
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getNumSSP2s()
{
  return(GLOBAL_subsysConfig.SSP2num);
} /* end getNumSSP2s */

/*----------------------------------------------------------
 * NAME:
 *  getNumIMSs
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getNumIMSs()
{
  return(GLOBAL_subsysConfig.IMSnum);
} /* end getNumIMSs */


/*----------------------------------------------------------
 * NAME:
 *  getNumGPRs
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getNumGPRs()
{
  return(GLOBAL_subsysConfig.GPRnum);
} /* end getNumGPRs */

/*----------------------------------------------------------
 * NAME:
 *  getNumQCs
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getNumQCs()
{
  return(GLOBAL_subsysConfig.QCnum);
} /* end getNumQCs */


/*----------------------------------------------------------
 * NAME:
 *  getNumScanQCs
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getNumScanQCs()
{
  return(GLOBAL_subsysConfig.ScanQCnum);
} /* end getNumScanQCs */

/*----------------------------------------------------------
 * NAME:
 *  getNumPreQCs
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getNumPreQCs()
{
  return(GLOBAL_subsysConfig.PreQCnum);
} /* end getNumPreQCs */

/*----------------------------------------------------------
 * NAME:
 *  getSSP2name
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getSSP2name(int i)
{
  return(GLOBAL_subsysConfig.SSP2logicalPtr[i]);

} /* end getSSP2name */

/*----------------------------------------------------------
 * NAME:
 *  getNextSSPname
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getNextSSPname(int thisSSP2)
{
  /* static int lastSSP2 = 0; */
  int lastSSP2 = thisSSP2+1;

/* if there is an ssp1, send to it; otherwise loop through the
   ssp2s in sequence */

  if (lastSSP2 >= GLOBAL_subsysConfig.SSP2num)
    lastSSP2 = 0;
  if (GLOBAL_subsysConfig.SSP2num) 
    return(getSSP2name(lastSSP2++));

  return(NULL);

} /* end getNextSSPname */


/*----------------------------------------------------------
 * NAME:
 *  getASPname
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getASPname()
{
  return(GLOBAL_subsysConfig.ASPlogicalPtr);

} /* end getASPname */

/*----------------------------------------------------------
 * NAME:
 *  getPPSname
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getPPSname()
{
  return(GLOBAL_subsysConfig.PPSlogicalPtr);

} /* end getPPSname */

/*----------------------------------------------------------
 * NAME:
 *  getPPSqueueSize
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getPPSqueueSize()
{
  return(GLOBAL_subsysConfig.PPSqueueSize);

} /* end getPPSqueueSize */

/*----------------------------------------------------------
 * NAME:
 *  setPPSqueueSize
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void setPPSqueueSize(int size)
{
  GLOBAL_subsysConfig.PPSqueueSize = size;

  return;

} /* end setPPSqueueSize */




/*----------------------------------------------------------
 * NAME:
 *  getIMSname
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getIMSname(int i)
{
  return(i < MAX_IMS ? GLOBAL_subsysConfig.IMSlogicalPtr[i] : NULL);

} /* end getIMSname */

/*----------------------------------------------------------
 * NAME:
 *  getIMSnameGivenCatalogStep
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getIMSnameGivenCatalogStep(int catalogStep)
{
  char *namePtr = NULL;
  
  switch (catalogStep) {
    case IMS_TYPE_GET_VERSION:
    case IMS_TYPE_GET_SCAN_RESULT:
    case IMS_TYPE_GET_CAL_PARAMS:
    case IMS_TYPE_STORE_SCAN_RESULT:
    case IMS_TYPE_STORE_SCAN_METADATA:
    case IMS_TYPE_CP_WORKING:
      namePtr = getIMSname(IMS_DEFAULT_EXE);
      break;
    case IMS_TYPE_STORE_CAL_PRODUCTS:
    case IMS_TYPE_STORE_PRODUCTS:
      namePtr = getIMSname(IMS_IMAGE_EXE);
    default:
      break;
  }

  return(namePtr);

} /* end getIMSnameGivenCatalogStep */




/*----------------------------------------------------------
 * NAME:
 *  getASPoutputDirName
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getASPoutputDirName(int whichDir)
{
  return(GLOBAL_subsysConfig.ASPoutput_data_dirPtr[whichDir]);

} /* end getASPoutputDirName */



/*----------------------------------------------------------
 * NAME:
 *  getSSP2inputDirName
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getSSP2inputDirName(int whichSSP, int whichDir)
{
  return(GLOBAL_subsysConfig.SSP2input_data_dirPtr[whichSSP][whichDir]);

} /* end getSSP2inputDirName */

/*----------------------------------------------------------
 * NAME:
 *  getSSP2outputDirName
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getSSP2outputDirName(int whichSSP, int whichDir)
{
  return(GLOBAL_subsysConfig.SSP2output_data_dirPtr[whichSSP][whichDir]);

} /* end getSSP2outputDirName */

/*----------------------------------------------------------
 * NAME:
 *  getRDSsaveDir
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getRDSsaveDir(int whichRDS)
{
  return(GLOBAL_subsysConfig.RDSsaveDirPtr[whichRDS]);

} /* end getRDSsaveDir */


/*----------------------------------------------------------
 * NAME:
 *  getSSP2saveDir
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getSSP2saveDir(int whichSSP)
{
  return(GLOBAL_subsysConfig.SSP2saveDirPtr[whichSSP]);

} /* end getSSP2saveDir */


/*----------------------------------------------------------
 * NAME:
 *  getASPsaveDir
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *getASPsaveDir()
{
  return(GLOBAL_subsysConfig.ASPsaveDirPtr);

} /* end getASPsaveDir */


/*----------------------------------------------------------
 * NAME:
 *  getStartTimeInterval
 *
 * DESCRIPTION:
 *  get the start time interval an convert it to microseconds
 *  for the Xt call
 *
 * NOTES:
 *  break this out separately from the above since it is used
 *  so offen
 *
 *---------------------------------------------------------*/
int getStartTimeInterval()
{
  return(GLOBAL_subsysConfig.startTimeInterval*1000);

} /* end get StartTimeInterval.............................*/

/*----------------------------------------------------------
 * NAME:
 *  getStopTimeInterval
 *
 * DESCRIPTION:
 *  get the stop time interval an convert it to microseconds
 *  for the Xt call
 *
 * NOTES:
 *  break this out separately from the above since it is used
 *  so offen
 *
 *---------------------------------------------------------*/
int getStopTimeInterval(char *dest)
{
   int stopTimeInterval;

   switch ( GetSubsystemCategoryGivenName(dest)) {
      case RDS_CATEGORY_ID:
         stopTimeInterval = GLOBAL_subsysConfig.RDSstopTimeInterval[getRDSnumGivenName(dest)];
         break;
      case SSP2_CATEGORY_ID:
         stopTimeInterval = GLOBAL_subsysConfig.SSP2stopTimeInterval[getSSP2numGivenName(dest)]; 
         break;
      case ASP_CATEGORY_ID:
         stopTimeInterval = GLOBAL_subsysConfig.ASPstopTimeInterval;
         break;
      case PPS_CATEGORY_ID:
         stopTimeInterval = GLOBAL_subsysConfig.PPSstopTimeInterval;
         break;
      case IMS_CATEGORY_ID:
         /* stopTimeInterval = GLOBAL_subsysConfig.IMSstopTimeInterval[getIMSnumGivenName(dest)];  */
         break;
   }
   return(stopTimeInterval*1000);

} /* end getStopTimeInterval...............................*/

/*----------------------------------------------------------
 * NAME:
 *  getResetTimeInterval
 *
 * DESCRIPTION:
 *  get the halt time interval an convert it to microseconds
 *  for the Xt call
 *
 * NOTES:
 *  break this out separately from the above since it is used
 *  so offen
 *
 *---------------------------------------------------------*/
int getResetTimeInterval(char *dest)
{
   int haltTimeInterval;

   switch ( GetSubsystemCategoryGivenName(dest)) {
      case RDS_CATEGORY_ID:
         haltTimeInterval = GLOBAL_subsysConfig.RDShaltTimeInterval[getRDSnumGivenName(dest)];
         break;
      case SSP2_CATEGORY_ID:
         haltTimeInterval = GLOBAL_subsysConfig.SSP2haltTimeInterval[getSSP2numGivenName(dest)]; 
         break;
      case ASP_CATEGORY_ID:
         haltTimeInterval = GLOBAL_subsysConfig.ASPhaltTimeInterval;
         break;
      case PPS_CATEGORY_ID:
         haltTimeInterval = GLOBAL_subsysConfig.PPShaltTimeInterval;
         break;
      case IMS_CATEGORY_ID:
/*         haltTimeInterval = GLOBAL_subsysConfig.IMShaltTimeInterval[getIMSnumGivenName(dest)];  */
         break;
   }
   return(haltTimeInterval*1000);

} /* end getResetTimeInterval...............................*/


/*----------------------------------------------------------
 * NAME:
 *  getHealthTimeInterval
 *
 * DESCRIPTION:
 *  get the health time interval an convert it to microseconds
 *  for the Xt call
 *
 * NOTES:
 *  break this out separately from the above since it is used
 *  so offen
 *
 *---------------------------------------------------------*/
int getHealthTimeInterval()
{
  return(GLOBAL_subsysConfig.healthTimeInterval*1000);
} /* end getHealthTimeInterval...............................*/


/*----------------------------------------------------------
 * NAME:
 *  getRequestSentTimeInterval
 *
 * DESCRIPTION:
 *  get the request sent time interval an convert it to microseconds
 *  for the Xt call
 *
 * NOTES:
 *  break this out separately from the above since it is used
 *  so offen
 *
 *---------------------------------------------------------*/
int getRequestSentTimeInterval()
{
  return(GLOBAL_subsysConfig.requestSentTimeInterval*1000);
} /* end getRequestSentTimeInterval...............................*/

/*----------------------------------------------------------
 * NAME:
 *  setupCPconfig
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int setupCPconfig(char *fileNamePtr)
{
 int fd, retval, ssp, ssp2Num, imsNum, rds,rdsNum,ims, jpt, jp, numJobProfiles;
 struct timeval tv;
 struct stat fs;
 char *filePtr, *baseStateFile, errstr[256], *p;
 static char timeStr[255];
 ODL  ODLconfig, odl, *bodyODL, ssp2ODL, imsODL, rdsODL, *jptODL, jpODL;
 int totalNin, totalNout, ii, n_in, n_out;
 int nr=0, nc, sf, i;
 double dretval;

  fd = open(fileNamePtr, 0);
  if (fd == -1) {
   printf("Cannot open Configuration file\n");
   exit(-1);
  }

  retval = fstat(fd, &fs);

  filePtr = (char *)malloc(fs.st_size);
  if (read(fd, filePtr, fs.st_size) != fs.st_size) {
   printf ("Cannot read %d\n",  fs.st_size);
   free(filePtr);
   close(fd);
   return(-1);
  }
  close(fd);

  if ((ODLconfig = StrToODL(filePtr, fs.st_size)) == NULL) {
   printf(CANNOT_PARSE_CONFIG);
   return(-1);
  } 
  free(filePtr);

  GLOBAL_subsysConfig.configFilePtr = (char *) doMalloc(strlen(fileNamePtr)+1);
  strcpy(GLOBAL_subsysConfig.configFilePtr, fileNamePtr);

                                                      /* get the port id */
  GLOBAL_subsysConfig.portId = ODLGetInt(ODLconfig, PORT_ID,  &retval);
                                            /* get the print screen command */
/*  GLOBAL_subsysConfig.printCmd = (char *) doMalloc(strlen(PRINT_COMMAND)+1); */
  GLOBAL_subsysConfig.printCmd =
             convertEnvFile(p=ODLGetString(ODLconfig, PRINT_COMMAND, &retval));
  setValidDir (GLOBAL_subsysConfig.printCmd, retval, DEF_PRINT_CMD );


/*  GLOBAL_subsysConfig.logBrowserCmd = (char *) doMalloc(strlen(LOG_BROWSER)+1); */
  GLOBAL_subsysConfig.logBrowserCmd =
    convertEnvFile(p=ODLGetString(ODLconfig, LOG_BROWSER, &retval));
  setValidDir (GLOBAL_subsysConfig.logBrowserCmd, retval, DEF_LOG_BROWSER_CMD );

  GLOBAL_subsysConfig.startTimeInterval = 
                     ODLGetInt(ODLconfig, START_TIME_INTERVAL,  &retval);
  GLOBAL_subsysConfig.healthTimeInterval = 
                     ODLGetInt(ODLconfig, HEALTH_TIME_INTERVAL, &retval);
  GLOBAL_subsysConfig.requestSentTimeInterval = 
                     ODLGetInt(ODLconfig, REQUEST_SENT_TIME_INTERVAL, &retval);


  /* get the root directories */

  GLOBAL_subsysConfig.cal_params_dirPtr           =
          convertEnvFile(p=ODLGetString(ODLconfig, CAL_PARAMS_DIR, &retval));
  setValidDir(GLOBAL_subsysConfig.cal_params_dirPtr , retval,  
              DEF_CAL_PARAMS_DIR);
    
  GLOBAL_subsysConfig.scan_results_dirPtr =
          convertEnvFile(p=ODLGetString(ODLconfig, SCAN_RESULTS_DIR, &retval));
  setValidDir(GLOBAL_subsysConfig.scan_results_dirPtr , retval, DEF_SCAN_DIR);
 

  /* get cp restore directory */
  GLOBAL_subsysConfig.CPrestoreDirPtr = 
      convertEnvFile(p=ODLGetString(ODLconfig, CP_RESTORE_DIR, &retval));
  setValidDir(GLOBAL_subsysConfig.CPrestoreDirPtr ,retval, DEF_SAVE_DIR);

  /* get cp save directory */
  GLOBAL_subsysConfig.CPsaveDirPtr = 
     convertEnvFile(p=ODLGetString(ODLconfig, CP_SAVE_DIR, &retval));
  setValidDir(GLOBAL_subsysConfig.CPsaveDirPtr , retval, DEF_SAVE_DIR);

  /* get cp scripts directory */
  GLOBAL_subsysConfig.CPscriptsDirPtr = 
     convertEnvFile(p=ODLGetString(ODLconfig, CP_SCRIPTS_DIR, &retval));
  setValidDir(GLOBAL_subsysConfig.CPscriptsDirPtr , retval, DEF_SCRIPTS_DIR);


  /*
   * get parameters CP saved state files
   */

  GLOBAL_subsysConfig.cp_saved_basename = 
      convertEnvFile(p=ODLGetString(ODLconfig, CP_SAVED_STATEFILE,  &retval));
  setValidDir(GLOBAL_subsysConfig.cp_saved_basename, retval, DEF_SAVED_FILE);

/* append the date/time to the end of the current state file name */
  gettimeofday(&tv);
  timeval_to_string(timeStr, &tv);
  timeStr[17]='\0';  /* whack off the small digits */


/* the 20 is just for testing -- can timeStr be short in some cases? */
  GLOBAL_subsysConfig.cp_current_statefile = 
    doMalloc(strlen(GLOBAL_subsysConfig.cp_saved_basename)+strlen(timeStr) +20);
  sprintf(GLOBAL_subsysConfig.cp_current_statefile, "%s.%s", 
          GLOBAL_subsysConfig.cp_saved_basename, timeStr+9); /* +9:remove day */

  setValidDir(GLOBAL_subsysConfig.cp_current_statefile , retval,DEF_SAVED_FILE);

  GLOBAL_subsysConfig.cp_last_saved_statefile = 
    convertEnvFile(p=ODLGetString(ODLconfig, CP_LAST_SAVED_STATEFILE,  &retval));
  setValidDir(GLOBAL_subsysConfig.cp_last_saved_statefile , retval,
                DEF_LAST_SAVED_FILE);

  GLOBAL_subsysConfig.cp_autoload_state =
      ODLGetString(ODLconfig, CP_AUTOLOAD_STATE, &retval);
  if (retval == -1) {
      GLOBAL_subsysConfig.cp_autoload_state = doMalloc(strlen("NO")+1);
      strcpy(GLOBAL_subsysConfig.cp_autoload_state, "NO");
  }



  /*
   * get syslog parameters
   */

  GLOBAL_subsysConfig.cp_err_logfile = 
          convertEnvFile(p=ODLGetString(ODLconfig, CP_ERR_LOGFILE,  &retval));
   setValidDir(GLOBAL_subsysConfig.cp_err_logfile, retval,DEF_CP_ERR_LOGFILE);

  GLOBAL_subsysConfig.cp_info_logfile = 
      convertEnvFile(p=ODLGetString(ODLconfig, CP_INFO_LOGFILE, &retval));
   setValidDir(GLOBAL_subsysConfig.cp_info_logfile, retval,DEF_CP_INFO_LOGFILE);
  GLOBAL_subsysConfig.cp_debug_logfile = 
      convertEnvFile(p=ODLGetString(ODLconfig, CP_DEBUG_LOGFILE, &retval));
   setValidDir(GLOBAL_subsysConfig.cp_debug_logfile , retval,
               DEF_CP_DEBUG_LOGFILE);

  GLOBAL_subsysConfig.sps_err_logfile = 
      convertEnvFile(p=ODLGetString(ODLconfig, SPS_ERR_LOGFILE, &retval));
  setValidDir(GLOBAL_subsysConfig.sps_err_logfile , retval, 
              DEF_SPS_ERR_LOGFILE);

  GLOBAL_subsysConfig.sps_info_logfile = 
      convertEnvFile(p=ODLGetString(ODLconfig, SPS_INFO_LOGFILE, &retval));
  setValidDir(GLOBAL_subsysConfig.sps_info_logfile , retval,
              DEF_SPS_INFO_LOGFILE);
  GLOBAL_subsysConfig.sps_debug_logfile = 
      convertEnvFile(p=ODLGetString(ODLconfig, SPS_DEBUG_LOGFILE, &retval));
   setValidDir(GLOBAL_subsysConfig.sps_debug_logfile , retval,
              DEF_SPS_DEBUG_LOGFILE);

  GLOBAL_subsysConfig.sps_retain_products = 
      ODLGetString(ODLconfig, SPS_RETAIN_PRODUCTS, &retval);
  if (retval == -1) {
      GLOBAL_subsysConfig.sps_retain_products = doMalloc(strlen("NO")+1);
      strcpy(GLOBAL_subsysConfig.sps_retain_products, "NO");
  }

  GLOBAL_subsysConfig.sps_J1_routing = 
      ODLGetString(ODLconfig, SPS_J1_ROUTING, &retval);
  if (retval == -1) {
      GLOBAL_subsysConfig.sps_J1_routing = doMalloc(strlen("SINGLE")+1);
      strcpy(GLOBAL_subsysConfig.sps_J1_routing, "SINGLE");
  }

  /* end get syslog parameters */

  /* GPR config info */
  GLOBAL_subsysConfig.GPRnum = ODLGetInt(ODLconfig, GPR_NUM, &retval);
  GLOBAL_subsysConfig.GPRlogicalPtr = ODLGetString(ODLconfig, GPR_LOGICAL, &retval);
  GLOBAL_subsysConfig.GPRpathPtr = convertEnvFile(p=ODLGetString(ODLconfig, GPR_PATH, &retval));
  GLOBAL_subsysConfig.GPRexeNamePtr = ODLGetString(ODLconfig, GPR_EXE_NAME, 
           &retval);
  GLOBAL_subsysConfig.GPRtemplatePtr = convertEnvFile(p=ODLGetString(ODLconfig,
         GPR_MSG_TEMP, &retval));
  if (retval == -1)
    GLOBAL_subsysConfig.GPRtemplatePtr = convertEnvFile(DEF_MSG_TEMPLATE_FILE);

  GLOBAL_subsysConfig.GPRconfigFilePtr = 
         convertEnvFile(p=ODLGetString(ODLconfig, GPR_CONFIG_FILE, &retval));
  if (retval == -1)
    GLOBAL_subsysConfig.GPRconfigFilePtr = DEFAULT_GPR_CONFIG_FILE;

  /* QC config info */
  GLOBAL_subsysConfig.QCnum = ODLGetInt(ODLconfig, QC_NUM, &retval);
  GLOBAL_subsysConfig.QCpathPtr = 
          convertEnvFile(p=ODLGetString(ODLconfig, QC_PATH, &retval));

  GLOBAL_subsysConfig.QChostPtr = ODLGetString(ODLconfig, QC_HOST, &retval);
  GLOBAL_subsysConfig.QCexeNamePtr = ODLGetString(ODLconfig, QC_EXE_NAME, 
            &retval);
  GLOBAL_subsysConfig.QCtemplatePtr = ODLGetString(ODLconfig, QC_MSG_TEMP, 
            &retval);
  if (retval == -1)
    GLOBAL_subsysConfig.QCtemplatePtr = NULL;

  if (strcmp(GLOBAL_subsysConfig.QCtemplatePtr, "") == 0)
    GLOBAL_subsysConfig.QCtemplatePtr = NULL;

  GLOBAL_subsysConfig.QCdisplayPtr = ODLGetString(ODLconfig, QC_DISPLAY, 
             &retval);
  if (retval == -1)
    GLOBAL_subsysConfig.QCdisplayPtr = NULL;

  if (strcmp(GLOBAL_subsysConfig.QCdisplayPtr, "") == 0)
    GLOBAL_subsysConfig.QCdisplayPtr = NULL;

  GLOBAL_subsysConfig.QCcomplexScaleFac = 
        ODLGetDouble(ODLconfig, QC_CX_SCALE_FAC, &retval);

  /* ScanQC config info */
  GLOBAL_subsysConfig.ScanQCnum = ODLGetInt(ODLconfig, SCAN_QC_NUM, &retval);
  GLOBAL_subsysConfig.ScanQCpathPtr =
          convertEnvFile(p=ODLGetString(ODLconfig, SCAN_QC_PATH, &retval));

  GLOBAL_subsysConfig.ScanQChostPtr = ODLGetString(ODLconfig, SCAN_QC_HOST, &retval);
  GLOBAL_subsysConfig.ScanQCexeNamePtr = ODLGetString(ODLconfig, SCAN_QC_EXE_NAME,
            &retval);
  GLOBAL_subsysConfig.ScanQCtemplatePtr = ODLGetString(ODLconfig, SCAN_QC_MSG_TEMP,
            &retval);
  if (retval == -1)
    GLOBAL_subsysConfig.ScanQCtemplatePtr = NULL;

  if (strcmp(GLOBAL_subsysConfig.ScanQCtemplatePtr, "") == 0)
    GLOBAL_subsysConfig.ScanQCtemplatePtr = NULL;

  GLOBAL_subsysConfig.ScanQCdisplayPtr = ODLGetString(ODLconfig, SCAN_QC_DISPLAY,
             &retval);
  if (retval == -1)
    GLOBAL_subsysConfig.ScanQCdisplayPtr = NULL;

  if (strcmp(GLOBAL_subsysConfig.QCdisplayPtr, "") == 0)
    GLOBAL_subsysConfig.QCdisplayPtr = NULL;

  /* PreQC config info */
  GLOBAL_subsysConfig.PreQCnum = ODLGetInt(ODLconfig, PRE_QC_NUM, &retval);
  GLOBAL_subsysConfig.PreQCpathPtr =
          convertEnvFile(p=ODLGetString(ODLconfig, PRE_QC_PATH, &retval));

  GLOBAL_subsysConfig.PreQChostPtr = ODLGetString(ODLconfig, PRE_QC_HOST, &retval);
  GLOBAL_subsysConfig.PreQCexeNamePtr = ODLGetString(ODLconfig, PRE_QC_EXE_NAME,
            &retval);
  GLOBAL_subsysConfig.PreQCtemplatePtr = ODLGetString(ODLconfig, PRE_QC_MSG_TEMP,
            &retval);
  if (retval == -1)
    GLOBAL_subsysConfig.PreQCtemplatePtr = NULL;

  if (strcmp(GLOBAL_subsysConfig.PreQCtemplatePtr, "") == 0)
    GLOBAL_subsysConfig.PreQCtemplatePtr = NULL;


/***********************************************************************/
/***************** this is all special because of multiple jobProfile objects */
/***********************************************************************/
  /* jobProfile config info */

  if ((odl = ODLparse(fileNamePtr, 0, errstr)) == NULL) {
      printf("can't open config file 2 %s\n", fileNamePtr);
      return(-1);
  }

  if ((bodyODL = (ODL*) Val(Lookup(odl, "CONFIG_FILE"))) == NULL) {
      printf("Can't find CONFIG_FILE object");
      return(-1);
  }

  GLOBAL_subsysConfig.numJobProfiles = ODLGetInt(odl,NUM_JOB_PROFILES,&retval);

  numJobProfiles = 0;
  for (jpt=0; bodyODL[jpt]!=NULL; ++jpt) {
    if (strcasecmp(Name(bodyODL[jpt]), "JOB_PROFILE_TABLE") ||
          (jptODL = (ODL*) Val(bodyODL[jpt])) == NULL) {
         continue;
    }

/* found jptable, now find jp elements */
  for (jp=0; jptODL[jp]!=NULL; ++jp) {
    if (strcasecmp(Name(jptODL[jp]), "JOB_PROFILE") ||
          (jpODL = (ODL*) Val(jptODL[jp])) == NULL) {
      continue;
    }

    GLOBAL_subsysConfig.n_platforms[numJobProfiles] = 
            ODLGetStringArray(jptODL[jp], "PLATFORM_LIST",
               &GLOBAL_subsysConfig.jobProfilePlatforms[numJobProfiles]);
    GLOBAL_subsysConfig.n_modes[numJobProfiles] = 
            ODLGetStringArray(jptODL[jp], "MODE_LIST",
               &GLOBAL_subsysConfig.jobProfileModes[numJobProfiles]) ;
    GLOBAL_subsysConfig.n_product_types[numJobProfiles] = 
            ODLGetStringArray(jptODL[jp], "PRODUCT_TYPE_LIST",
               &GLOBAL_subsysConfig.jobProfileProductTypes[numJobProfiles]) ;
    GLOBAL_subsysConfig.n_quicklook_flags[numJobProfiles] = 
            ODLGetStringArray(jptODL[jp], "QUICKLOOK_FLAG_LIST",
               &GLOBAL_subsysConfig.jobProfileQuicklookFlags[numJobProfiles]) ;
    GLOBAL_subsysConfig.n_compensation_flags[numJobProfiles] = 
            ODLGetStringArray(jptODL[jp], "COMPENSATION_FLAG_LIST",
          &GLOBAL_subsysConfig.jobProfileCompensationFlags[numJobProfiles]) ;

    GLOBAL_subsysConfig.n_frame_modes[numJobProfiles] = 
            ODLGetStringArray(jptODL[jp], "FRAME_MODE_LIST",
          &GLOBAL_subsysConfig.jobProfileFrameModes[numJobProfiles]) ;
    GLOBAL_subsysConfig.n_deskews[numJobProfiles] = 
            ODLGetStringArray(jptODL[jp], "DESKEW_LIST",
          &GLOBAL_subsysConfig.jobProfileDeskews[numJobProfiles]) ;
    GLOBAL_subsysConfig.n_projections[numJobProfiles] = 
            ODLGetStringArray(jptODL[jp], "PROJECTION_LIST",
          &GLOBAL_subsysConfig.jobProfileProjections[numJobProfiles]) ;
    GLOBAL_subsysConfig.n_terrain_corrections[numJobProfiles] = 
            ODLGetStringArray(jptODL[jp], "TERRAIN_CORRECTION_LIST",
          &GLOBAL_subsysConfig.jobProfileTerrainCorrections[numJobProfiles]) ;

    nr=1;
    nc = MAX_JOB_PIXEL_SPACINGS; /* have to give the size of the dest array */
    ODLGetArrayDouble(jptODL[jp], "PIXEL_SPACING_LIST",
       GLOBAL_subsysConfig.jobProfilePixelSpacings[numJobProfiles], /* retval */
       &nr, &nc );
    GLOBAL_subsysConfig.n_pixel_spacings[numJobProfiles] = nr*nc;

    nr=1;
    nc = MAX_JOB_SUBFRAME_IDS; /* have to give the size of the dest array */

#ifdef ZZZZZZZZZZZ
printf("num subframes %d (0x%x), subframe[%d] starts at 0x%x\n",
GLOBAL_subsysConfig.n_subframe_ids[numJobProfiles],
&GLOBAL_subsysConfig.n_subframe_ids[numJobProfiles], numJobProfiles,
GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles]
);

for (i=0; i < 10; i++) {
  GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles][i] = numJobProfiles*100+i;
/****
  printf("%d at 0x%x\n", 
GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles][i],
&GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles][i]);
****/
}
  printf("\n");
/*****
printIntArray("subframe ids", MAX_JOB_SUBFRAME_IDS,
               GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles]);

*********/

printf("top of subgrames 0x%x & 0x%x\n", 
GLOBAL_subsysConfig.jobProfileSubframeIds,
&GLOBAL_subsysConfig.jobProfileSubframeIds);
printf("top of subframes[1] 0x%x, & 0x%x\n", 
GLOBAL_subsysConfig.jobProfileSubframeIds[1], 
&GLOBAL_subsysConfig.jobProfileSubframeIds[1]);

printf("passing 0x%x to ODLGetArrayInt, & is 0x%x, val[0] %d val[1] %d\n", 
GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles],
&GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles],
GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles][0], 
GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles][1]);
#endif

    if (!ODLGetArrayInt(jptODL[jp], "SUBFRAME_ID_LIST",
      GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles], /* retval */
      &nr, &nc) ) {
     printf("ODLGetArrayInt error!\n");
     return(-1);
    }
    GLOBAL_subsysConfig.n_subframe_ids[numJobProfiles] = nr*nc;

#ifdef ZZZZZZZ
printf("after ODLGetArrayInt, is 0x%x, val[0] %d val[1] %d\n", 
&GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles],
GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles][0], 
GLOBAL_subsysConfig.jobProfileSubframeIds[numJobProfiles][1]);
#endif

  /* get profile processor name */
    GLOBAL_subsysConfig.jobProfileSubsystem[numJobProfiles] =
                     ODLGetString(jptODL[jp], "PRODUCT_CREATOR", &retval);
    if (retval == -1) {
      printfLLog(LOG_ERR, "cannot get job profile");
      exit(-1);
    }
    numJobProfiles++;
    } /* end processing of job profile object */

  }  /* end of job profile table body loop */

  if (GLOBAL_subsysConfig.numJobProfiles > numJobProfiles) {
    printf( CANNOT_GET_ALL_OBJECTS,  "JobProfiles", numJobProfiles);
    exit(-1);
  }
/***********************************************************************/
/***************** end of special processing for multiple job profile objects */
/***********************************************************************/

  /* RDS config info */
  GLOBAL_subsysConfig.RDSnum = ODLGetInt(ODLconfig, RDS_NUM, &retval);
  if (retval == -1) {
    printf( CANNOT_GET_OBJECT, "RDS");
    exit(-1);
  }
 if ((bodyODL = (ODL*) Val(Lookup(odl, "CONFIG_FILE"))) == NULL) {
      printf("CANT_FIND_FRAME_BODY");
      return(-1);
  }

  GLOBAL_subsysConfig.RDSnum = ODLGetInt(odl, RDS_NUM, &retval);
  rdsNum = 0;
  for (rds=0; bodyODL[rds]!=NULL; ++rds) {
    if (strcasecmp(Name(bodyODL[rds]), "RDS") ||
          (rdsODL = (ODL*) Val(bodyODL[rds])) == NULL) {
         continue;
    }

    GLOBAL_subsysConfig.RDScategory[rdsNum] = RDS_CATEGORY_ID;
    GLOBAL_subsysConfig.RDSlogicalPtr[rdsNum] = ODLGetString(bodyODL[rds], "LOGICAL",
                &retval);
    GLOBAL_subsysConfig.RDSpathPtr[rdsNum] = ODLGetString(bodyODL[rds], "PATH", &retval);
    GLOBAL_subsysConfig.RDSbackground[rdsNum] = ODLGetString(bodyODL[rds], "BACKGROUND", 
                &retval);
    GLOBAL_subsysConfig.RDSexeNamePtr[rdsNum] = ODLGetString(bodyODL[rds], "EXECUTABLE", 
                &retval);
    GLOBAL_subsysConfig.RDShostPtr[rdsNum] = ODLGetString(bodyODL[rds], "HOST", &retval);
    GLOBAL_subsysConfig.RDSmediaType[rdsNum] = ODLGetString(bodyODL[rds], "MEDIA_TYPE", &retval);
    GLOBAL_subsysConfig.RDSstopTimeInterval[rdsNum] = ODLGetInt(bodyODL[rds], 
                "STOP_TIME_INTERVAL",   &retval);
    GLOBAL_subsysConfig.RDShaltTimeInterval[rdsNum] = ODLGetInt(bodyODL[rds], 
                "HALT_TIME_INTERVAL",   &retval);
    GLOBAL_subsysConfig.RDSsaveDirPtr[rdsNum] = 
        convertEnvFile(ODLGetString(ODLconfig, RDS_SAVE_DIR, &retval));
    setValidDir(GLOBAL_subsysConfig.RDSsaveDirPtr[rdsNum] ,retval, DEF_RDS_SAVE_DIR);


  /* get RDS subnet host name */
  GLOBAL_subsysConfig.RDS_CP_hostNamePtr[rdsNum] = 
                     ODLGetString(ODLconfig, RDS_CP_HOSTNAME, &retval);
  if (retval == -1) {
   printf( CANNOT_GET_SUBNET_HOST, getRDSname(rdsNum));
   exit(-1);
  }

  if (strcmp(GLOBAL_subsysConfig.RDS_CP_hostNamePtr[rdsNum], "") == 0) {
   printf( CANNOT_GET_SUBNET_HOST, getRDSname(rdsNum));
   exit(-1);
  }

  GLOBAL_subsysConfig.RDStemplatePtr[rdsNum] = convertEnvFile(ODLGetString(ODLconfig, 
            RDS_MSG_TEMP, &retval));
  if (retval == -1)
    GLOBAL_subsysConfig.RDStemplatePtr[rdsNum] = NULL;

  if (strcmp(GLOBAL_subsysConfig.RDStemplatePtr[rdsNum], "") == 0)
    GLOBAL_subsysConfig.RDStemplatePtr[rdsNum] = NULL;

  GLOBAL_subsysConfig.RDSconfigFilePtr[rdsNum] = ODLGetString(bodyODL[rds],
      "CONFIG_FILE", &retval);
    if (retval == -1)
      GLOBAL_subsysConfig.RDSconfigFilePtr[rdsNum] = NULL;

    if (strcmp(GLOBAL_subsysConfig.RDSconfigFilePtr[rdsNum], "") == 0)
      GLOBAL_subsysConfig.RDSconfigFilePtr[rdsNum] = NULL;
 
    rdsNum++;
  }  /* end of rds body loop */
  if (GLOBAL_subsysConfig.RDSnum > rdsNum) {
    printf( CANNOT_GET_ALL_OBJECTS,  "RDS", rdsNum);
    exit(-1);
  }


/***********************************************************************/
/***************** this is all special because of multiple SSP2 objects */
/***********************************************************************/
  /* SSP2 config info */
  GLOBAL_subsysConfig.SSP2num = ODLGetInt(ODLconfig, SSP2_NUM, &retval);

/****
  if ((odl = ODLparse(fileNamePtr, 0, errstr)) == NULL) {
      printf("can't open config file 2 %s\n", fileNamePtr);
      return(-1);
  }
*/

  if ((bodyODL = (ODL*) Val(Lookup(odl, "CONFIG_FILE"))) == NULL) {
      printf("CANT_FIND_FRAME_BODY");
      return(-1);
  }

  GLOBAL_subsysConfig.SSP2num = ODLGetInt(odl, SSP2_NUM, &retval);
  ssp2Num = totalNin = totalNout = 0;  
  for (ssp=0; bodyODL[ssp]!=NULL; ++ssp) {
    if (strcasecmp(Name(bodyODL[ssp]), SSP_PP_OBJECT_NAME) ||
          (ssp2ODL = (ODL*) Val(bodyODL[ssp])) == NULL) {
         continue;
    }


    GLOBAL_subsysConfig.SSP2category[ssp2Num] = SSP2_CATEGORY_ID;
    GLOBAL_subsysConfig.SSP2logicalPtr[ssp2Num] = ODLGetString(bodyODL[ssp], 
             "LOGICAL", &retval);
    GLOBAL_subsysConfig.SSP2background[ssp2Num] = ODLGetString(bodyODL[ssp], 
              "BACKGROUND", &retval);
    GLOBAL_subsysConfig.SSP2pathPtr[ssp2Num] = ODLGetString(bodyODL[ssp], 
              "PATH", &retval);
    GLOBAL_subsysConfig.SSP2exeNamePtr[ssp2Num] = ODLGetString(bodyODL[ssp], 
              "EXECUTABLE", &retval);
    GLOBAL_subsysConfig.SSP2hostPtr[ssp2Num] = ODLGetString(bodyODL[ssp], 
              "HOST", &retval);
    GLOBAL_subsysConfig.SSP2stopTimeInterval[ssp2Num] = ODLGetInt(bodyODL[ssp],
              "STOP_TIME_INTERVAL",   &retval);
    GLOBAL_subsysConfig.SSP2haltTimeInterval[ssp2Num] = ODLGetInt(bodyODL[ssp],
               "HALT_TIME_INTERVAL",   &retval);
    GLOBAL_subsysConfig.SSP2saveDirPtr[ssp2Num] =  
      convertEnvFile(p=ODLGetString(ODLconfig, SSP2_SAVE_DIR, &retval));
    setValidDir(GLOBAL_subsysConfig.SSP2saveDirPtr[ssp2Num],
                  retval, DEF_SSP2_SAVE_DIR);

    n_in = ODLGetStringArray(bodyODL[ssp], "INPUT_BASEDIR",
               &GLOBAL_subsysConfig.SSP2input_data_dirPtr[ssp2Num]);
    n_out = ODLGetStringArray(bodyODL[ssp], "CEOS_OUTPUT_BASEDIR",
               &GLOBAL_subsysConfig.SSP2output_data_dirPtr[ssp2Num]) ;

/* remove environment variables from pathname */
  for (ii=0; ii < n_in; ii++) {
    GLOBAL_subsysConfig.SSP2input_data_dirPtr[ssp2Num][ii] =
        convertEnvFile(GLOBAL_subsysConfig.SSP2input_data_dirPtr[ssp2Num][ii]);
  }
  for (ii=0; ii < n_out; ii++) {
    GLOBAL_subsysConfig.SSP2output_data_dirPtr[ssp2Num][ii] =
        convertEnvFile(GLOBAL_subsysConfig.SSP2output_data_dirPtr[ssp2Num][ii]);
  }

    totalNin += n_in;
    totalNout += n_out;

  /* get SSP2's subnet hostname */
    GLOBAL_subsysConfig.SSP2_CP_hostNamePtr[ssp2Num] =
                     ODLGetString(bodyODL[ssp], "CP_HOSTNAME", &retval);
    if (retval == -1) {
      printf( CANNOT_GET_SUBNET_HOST, getSSP2name(ssp2Num));
      exit(-1);
    }

    if (strcmp(GLOBAL_subsysConfig.SSP2_CP_hostNamePtr[ssp2Num], "") == 0) {
      printf( CANNOT_GET_SUBNET_HOST, getSSP2name(ssp2Num));
      exit(-1);
    }

    GLOBAL_subsysConfig.SSP2templatePtr[ssp2Num] = 
         convertEnvFile(ODLGetString(bodyODL[ssp], "MSG_TEMP", &retval));
    if (retval == -1)
      GLOBAL_subsysConfig.SSP2templatePtr[ssp2Num] = NULL;
  
    if (strcmp(GLOBAL_subsysConfig.SSP2templatePtr[ssp2Num], "") == 0)
      GLOBAL_subsysConfig.SSP2templatePtr[ssp2Num] = NULL;

    GLOBAL_subsysConfig.SSP2configFilePtr[ssp2Num] = ODLGetString(bodyODL[ssp],
      "CONFIG_FILE", &retval);
    if (retval == -1)
      GLOBAL_subsysConfig.SSP2configFilePtr[ssp2Num] = NULL;

    if (strcmp(GLOBAL_subsysConfig.SSP2configFilePtr[ssp2Num], "") == 0)
      GLOBAL_subsysConfig.SSP2configFilePtr[ssp2Num] = NULL;
  
    ssp2Num++;
  }  /* end of ssp2 body loop */
  if (GLOBAL_subsysConfig.SSP2num > ssp2Num) {
    printf( CANNOT_GET_ALL_OBJECTS,  "SSP-2", ssp2Num);
    exit(-1);
  }
/***********************************************************************/
/***************** end of special processing for multiple SSP2 objects */
/***********************************************************************/

  /* ASP config info */
  GLOBAL_subsysConfig.ASPnum = ODLGetInt(ODLconfig, ASP_NUM, &retval);
  if (retval == -1) {
    printf( CANNOT_GET_OBJECT, "ASP");
    exit(-1);
  }
  GLOBAL_subsysConfig.ASPcategory = ASP_CATEGORY_ID;
  GLOBAL_subsysConfig.ASPlogicalPtr = ODLGetString(ODLconfig, ASP_LOGICAL,
              &retval);
  GLOBAL_subsysConfig.ASPbackground = ODLGetString(ODLconfig, ASP_BACKGROUND, 
             &retval);
  GLOBAL_subsysConfig.ASPpathPtr = ODLGetString(ODLconfig, ASP_PATH, &retval);
  GLOBAL_subsysConfig.ASPexeNamePtr = ODLGetString(ODLconfig, ASP_EXE_NAME, 
             &retval);
  GLOBAL_subsysConfig.ASPhostPtr = ODLGetString(ODLconfig, ASP_HOST, &retval);
  GLOBAL_subsysConfig.ASPstopTimeInterval = 
                     ODLGetInt(ODLconfig, ASP_STOP_TIME_INTERVAL,   &retval);
  GLOBAL_subsysConfig.ASPhaltTimeInterval = 
                     ODLGetInt(ODLconfig, ASP_HALT_TIME_INTERVAL,   &retval);
  n_out = ODLGetStringArray(ODLconfig, ASP_OUTPUT_DATA_DIR, 
               &GLOBAL_subsysConfig.ASPoutput_data_dirPtr) ;
  GLOBAL_subsysConfig.ASPsaveDirPtr = 
      convertEnvFile(p=ODLGetString(ODLconfig, ASP_SAVE_DIR, &retval));
  setValidDir(GLOBAL_subsysConfig.ASPsaveDirPtr ,retval, DEF_ASP_SAVE_DIR);

/* remove environment variables from pathname */
  for (ii=0; ii < n_out; ii++)
    GLOBAL_subsysConfig.ASPoutput_data_dirPtr[ii] =
        convertEnvFile(GLOBAL_subsysConfig.ASPoutput_data_dirPtr[ii]);

  /* get ASP's subnet hostname */
  GLOBAL_subsysConfig.ASP_CP_hostNamePtr = 
                     ODLGetString(ODLconfig, ASP_CP_HOSTNAME, &retval);
  if (retval == -1) {
   printf( CANNOT_GET_SUBNET_HOST, getASPname());
   exit(-1);
  }

  if (strcmp(GLOBAL_subsysConfig.ASP_CP_hostNamePtr, "") == 0) {
   printf( CANNOT_GET_SUBNET_HOST, getASPname());
   exit(-1);
  }

  GLOBAL_subsysConfig.ASPtemplatePtr = convertEnvFile(ODLGetString(ODLconfig, 
           ASP_MSG_TEMP, &retval));
  if (retval == -1)
    GLOBAL_subsysConfig.ASPtemplatePtr = NULL;

  if (strcmp(GLOBAL_subsysConfig.ASPtemplatePtr, "") == 0)
    GLOBAL_subsysConfig.ASPtemplatePtr = NULL;
  
  /* PPS config info */
  if ((GLOBAL_subsysConfig.PPSnum = 
           ODLGetInt(ODLconfig, PPS_NUM, &retval)) <= 0) {
    printf( CANNOT_GET_MAX_CONCURRENT, PPS_CATEGORY);
#ifndef ALLOW_ZERO_PPS_IMS
    exit(0);
#endif
  }
  
  GLOBAL_subsysConfig.PPScategory = PPS_CATEGORY_ID;
  GLOBAL_subsysConfig.PPSlogicalPtr = ODLGetString(ODLconfig, PPS_LOGICAL,
              &retval);
  GLOBAL_subsysConfig.PPSbackground = ODLGetString(ODLconfig, PPS_BACKGROUND, 
             &retval); 
  GLOBAL_subsysConfig.PPSpathPtr = ODLGetString(ODLconfig, PPS_PATH, &retval);
  GLOBAL_subsysConfig.PPSexeNamePtr = ODLGetString(ODLconfig, PPS_EXE_NAME, 
             &retval);
  GLOBAL_subsysConfig.PPShostPtr = ODLGetString(ODLconfig, PPS_HOST, &retval);
  GLOBAL_subsysConfig.PPSstopTimeInterval = 
                     ODLGetInt(ODLconfig, PPS_STOP_TIME_INTERVAL,   &retval);
  GLOBAL_subsysConfig.PPShaltTimeInterval = 
                     ODLGetInt(ODLconfig, PPS_HALT_TIME_INTERVAL,   &retval);
  GLOBAL_subsysConfig.PPSqueueSize = ODLGetInt(ODLconfig, PPS_MAX_PENDING, 
             &retval);

  /* get PPS's subnet hostname */
  GLOBAL_subsysConfig.PPS_CP_hostNamePtr = 
                     ODLGetString(ODLconfig, PPS_CP_HOSTNAME, &retval);
  if (retval == -1) {
   printf( CANNOT_GET_SUBNET_HOST, getPPSname());
   exit(-1);
  }

  if (strcmp(GLOBAL_subsysConfig.PPS_CP_hostNamePtr, "") == 0) {
   printf( CANNOT_GET_SUBNET_HOST, getPPSname());
   exit(-1);
  }

  GLOBAL_subsysConfig.PPStemplatePtr = convertEnvFile(ODLGetString(ODLconfig, 
              PPS_MSG_TEMP, &retval));
  if (retval == -1)
    GLOBAL_subsysConfig.PPStemplatePtr = NULL;

  if (strcmp(GLOBAL_subsysConfig.PPStemplatePtr, "") == 0)
    GLOBAL_subsysConfig.PPStemplatePtr = NULL;

  GLOBAL_subsysConfig.PPSconfigFilePtr =
    convertEnvFile(p=ODLGetString(ODLconfig, PPS_CONFIG_FILE, &retval));
  if (retval == -1)
    GLOBAL_subsysConfig.PPSconfigFilePtr = NULL;

  if (GLOBAL_subsysConfig.PPSconfigFilePtr == NULL)
    GLOBAL_subsysConfig.PPSconfigFilePtr = NULL;


  /* IMS config info */
  if ((GLOBAL_subsysConfig.IMSnum = 
           ODLGetInt(ODLconfig, IMS_NUM, &retval)) <= 0) {
    printf( CANNOT_GET_MAX_CONCURRENT, IMS_CATEGORY);
#ifndef ALLOW_ZERO_PPS_IMS
    exit(0);
#endif
  }

/***********************************************************************/
/***************** this is all special because of multiple IMS objects */
/***********************************************************************/
  /* IMS config info */
  GLOBAL_subsysConfig.IMSnum = ODLGetInt(ODLconfig, IMS_NUM, &retval);

/*****
  if ((odl = ODLparse(fileNamePtr, 0, errstr)) == NULL) {
      printf("can't open config file 3 %s\n", fileNamePtr);
      return(-1);
  }

*****/
  if ((bodyODL = (ODL*) Val(Lookup(odl, "CONFIG_FILE"))) == NULL) {
      printf("CANT_FIND_FRAME_BODY");
      return(-1);
  }

  GLOBAL_subsysConfig.IMSnum = ODLGetInt(odl, IMS_NUM, &retval);
  imsNum = totalNin = totalNout = 0;  
  for (ims=0; bodyODL[ims]!=NULL; ++ims) {
    if (strcasecmp(Name(bodyODL[ims]), "IMS") ||
          (imsODL = (ODL*) Val(bodyODL[ims])) == NULL) {
         continue;
    }


    GLOBAL_subsysConfig.IMScategory[imsNum] = IMS_CATEGORY_ID;
    GLOBAL_subsysConfig.IMSlogicalPtr[imsNum] = ODLGetString(bodyODL[ims], 
             "LOGICAL", &retval);
    GLOBAL_subsysConfig.IMSbackground[imsNum] = ODLGetString(bodyODL[ims], 
              "BACKGROUND", &retval);
    GLOBAL_subsysConfig.IMSpathPtr[imsNum] = ODLGetString(bodyODL[ims], 
              "PATH", &retval);
    GLOBAL_subsysConfig.IMSexeNamePtr[imsNum] = ODLGetString(bodyODL[ims], 
              "EXECUTABLE", &retval);
    GLOBAL_subsysConfig.IMShostPtr[imsNum] = ODLGetString(bodyODL[ims], 
              "HOST", &retval);
    GLOBAL_subsysConfig.IMSstopTimeInterval[imsNum] = ODLGetInt(bodyODL[ims],
              "STOP_TIME_INTERVAL",   &retval);
    GLOBAL_subsysConfig.IMShaltTimeInterval[imsNum] = ODLGetInt(bodyODL[ims],
               "HALT_TIME_INTERVAL",   &retval);



    totalNin += n_in;
    totalNout += n_out;

  /* get IMS's subnet hostname */
    GLOBAL_subsysConfig.IMS_CP_hostNamePtr[imsNum] =
                     ODLGetString(bodyODL[ims], "CP_HOSTNAME", &retval);
    if (retval == -1) {
      printf( CANNOT_GET_SUBNET_HOST, getIMSname(imsNum));
      exit(-1);
    }

    if (strcmp(GLOBAL_subsysConfig.IMS_CP_hostNamePtr[imsNum], "") == 0) {
      printf( CANNOT_GET_SUBNET_HOST, getIMSname(imsNum));
      exit(-1);
    }

    GLOBAL_subsysConfig.IMStemplatePtr[imsNum] = 
         convertEnvFile(ODLGetString(bodyODL[ims], "MSG_TEMP", &retval));
    if (retval == -1)
      GLOBAL_subsysConfig.IMStemplatePtr[imsNum] = NULL;
  
    if (strcmp(GLOBAL_subsysConfig.IMStemplatePtr[imsNum], "") == 0)
      GLOBAL_subsysConfig.IMStemplatePtr[imsNum] = NULL;

    GLOBAL_subsysConfig.IMSconfigFilePtr[imsNum] = ODLGetString(bodyODL[ims],
      "CONFIG_FILE", &retval);
    if (retval == -1)
      GLOBAL_subsysConfig.IMSconfigFilePtr[imsNum] = NULL;

    if (strcmp(GLOBAL_subsysConfig.IMSconfigFilePtr[imsNum], "") == 0)
      GLOBAL_subsysConfig.IMSconfigFilePtr[imsNum] = NULL;
  
    imsNum++;
  }  /* end of ims body loop */
  if (GLOBAL_subsysConfig.IMSnum > imsNum) {
    printf( CANNOT_GET_ALL_OBJECTS,  "IMS", imsNum);
    exit(-1);
  }
/***********************************************************************/
/***************** end of special processing for multiple IMS objects */
/***********************************************************************/
  
#ifdef DEBUG
printConfig(GLOBAL_subsysConfig);
#else
#ifdef PROFILE_DEBUG
printConfig(GLOBAL_subsysConfig);
#endif
#endif

  ODLFree(ODLconfig); 
  ODLFree(odl);

  return(0);

} /* end get configFile..*/

printConfig(subsysConfigType cfg)
{

  char *p;
  int i, j;
#ifdef DEBUG

  printf("CP \n");
  printf("\tconfigFilePtr %s\n", cfg.configFilePtr);
  printf("\tscan_results_dirPtr %s\n", cfg.scan_results_dirPtr);
  printf("\tcal_params_dirPtr %s\n", cfg.cal_params_dirPtr);
  printf("\tCPrestoreDirPtr %s\n", cfg.CPrestoreDirPtr);
  printf("\tCPsaveDirPtr %s\n", cfg.CPsaveDirPtr);
  printf("\tCPscriptsDirPtr %s\n", cfg.CPscriptsDirPtr);
  printf("\tcp_saved_basename %s\n", cfg.cp_saved_basename);
  printf("\tcp_current_statefile %s\n", cfg.cp_current_statefile);
  printf("\tcp_last_saved_statefile %s\n", cfg.cp_last_saved_statefile);
  printf("\tcp_autoload_state %s\n", cfg.cp_autoload_state);
  printf("\tcp_err_logfile %s\n", cfg.cp_err_logfile);
  printf("\tcp_info_logfile %s\n", cfg.cp_info_logfile);
  printf("\tcp_debug_logfile %s\n", cfg.cp_debug_logfile);
  printf("\tsps_err_logfile %s\n", cfg.sps_err_logfile);
  printf("\tsps_info_logfile %s\n", cfg.sps_info_logfile);
  printf("\tsps_debug_logfile %s\n", cfg.sps_debug_logfile);
  printf("\tsps_retain_products %s\n", cfg.sps_retain_products);
  printf("\tsps_J1_routing %s\n", cfg.sps_J1_routing);

  printf("\tprintCmd %s\n", cfg.printCmd);
  printf("\tlogBrowserCmd %s\n", cfg.logBrowserCmd);
  printf("\tportId %d\n", cfg.portId);
  printf("\tstartTimeInterval %d\n", cfg.startTimeInterval);
  printf("\thealthTimeInterval %d\n", cfg.healthTimeInterval);
  printf("\trequestSentTimeInterval %d\n", cfg.requestSentTimeInterval);

  printf("GPR \n");
  printf("\tGPRlogicalPtr %s\n", cfg.GPRlogicalPtr);
  printf("\tGPRpathPtr %s\n", cfg.GPRpathPtr);
  printf("\tGPRexeNamePtr %s\n", cfg.GPRexeNamePtr);
  printf("\tGPRtemplatePtr %s\n", cfg.GPRtemplatePtr);
  printf("\tGPRconfigFilePtr %s\n", cfg.GPRconfigFilePtr);

  printf("RDS \n");
  printf("\tRDSnum %d\n", cfg.RDSnum);
  printf("\tRDSlogicalPtr %s\n", cfg.RDSlogicalPtr);
  printf("\tRDSbackground %s\n", cfg.RDSbackground);
  printf("\tRDSpathPtr %s\n", cfg.RDSpathPtr);
  printf("\tRDSexeNamePtr %s\n", cfg.RDSexeNamePtr);
  printf("\tRDShostPtr %s\n", cfg.RDShostPtr);
  printf("\tRDS_CP_hostNamePtr %s\n", cfg.RDS_CP_hostNamePtr);
  printf("\tRDStemplatePtr %s\n", cfg.RDStemplatePtr);
  printf("\tRDSconfigFilePtr %s\n", cfg.RDSconfigFilePtr);
  printf("\tRDSsaveDirPtr %s\n", cfg.RDSsaveDirPtr);
  printf("\tRDSstopTimeInterval %d\n", cfg.RDSstopTimeInterval);
  printf("\tRDShaltTimeInterval %d\n", cfg.RDShaltTimeInterval);


  printf("SSP2 \n");
    printf("\tSSP2num %d\n", cfg.SSP2num);
  for (i=0; i < cfg.SSP2num; i++) {
    printf("\tSSP2logicalPtr[i] %s\n", cfg.SSP2logicalPtr[i]);
    printf("\tSSP2background[i] %s\n", cfg.SSP2background[i]);
    printf("\tSSP2pathPtr[i] %s\n", cfg.SSP2pathPtr[i]);
    printf("\tSSP2exeNamePtr[i] %s\n", cfg.SSP2exeNamePtr[i]);
    printf("\tSSP2hostPtr[i] %s\n", cfg.SSP2hostPtr[i]);
    printf("\tSSP2_CP_hostNamePtr[i] %s\n", cfg.SSP2_CP_hostNamePtr[i]);
    printf("\tSSP2templatePtr[i] %s\n", cfg.SSP2templatePtr[i]);
    printf("\tSSP2configFilePtr[i] %s\n", cfg.SSP2configFilePtr[i]);
    printf("\tSSP2saveDirPtr[i] %s\n", cfg.SSP2saveDirPtr[i]);
    printf("\tSSP2stopTimeInterval %d\n",
                     cfg.SSP2stopTimeInterval[i]);
    printf("\tSSP2haltTimeInterval %d\n", 
                     cfg.SSP2haltTimeInterval[i]);
printf("\n");
  }


  printf("ASP \n");
  printf("\tASPnum %d\n", cfg.ASPnum);
  printf("\tASPlogicalPtr %s\n", cfg.ASPlogicalPtr);
  printf("\tASPbackground %s\n", cfg.ASPbackground);
  printf("\tASPpathPtr %s\n", cfg.ASPpathPtr);
  printf("\tASPexeNamePtr %s\n", cfg.ASPexeNamePtr);
  printf("\tASPhostPtr %s\n", cfg.ASPhostPtr);
  printf("\tASP_CP_hostNamePtr %s\n", cfg.ASP_CP_hostNamePtr);
  printf("\tASPtemplatePtr %s\n", cfg.ASPtemplatePtr);
  printf("\tASPconfigFilePtr %s\n", cfg.ASPconfigFilePtr);
  printf("\tASPsaveDirPtr %s\n", cfg.ASPsaveDirPtr);
  printf("\tASPstopTimeInterval %d\n", cfg.ASPstopTimeInterval);
  printf("\tASPhaltTimeInterval %d\n", cfg.ASPhaltTimeInterval);

  printf("PPS \n");
  printf("\tPPSnum %d\n", cfg.PPSnum);
  printf("\tPPSqueueSize %d\n", cfg.PPSqueueSize);
  printf("\tPPSlogicalPtr %s\n", cfg.PPSlogicalPtr);
  printf("\tPPSpathPtr %s\n", cfg.PPSpathPtr);
  printf("\tPPSexeNamePtr %s\n", cfg.PPSexeNamePtr);
  printf("\tPPShostPtr %s\n", cfg.PPShostPtr);
  printf("\tPPS_CP_hostNamePtr %s\n", cfg.PPS_CP_hostNamePtr);
  printf("\tPPStemplatePtr %s\n", cfg.PPStemplatePtr);
  printf("\tPPSconfigFilePtr %s\n", cfg.PPSconfigFilePtr);
  printf("\tPPSstopTimeInterval %d\n", cfg.PPSstopTimeInterval);
  printf("\tPPShaltTimeInterval %d\n", cfg.PPShaltTimeInterval);

  printf("IMS \n");
    printf("\tIMSnum %d\n", cfg.IMSnum);
  for (i=0; i < cfg.IMSnum; i++) {
    printf("\tIMSlogicalPtr[i] %s\n", cfg.IMSlogicalPtr[i]);
    printf("\tIMSpathPtr[i] %s\n", cfg.IMSpathPtr[i]);
    printf("\tIMSexeNamePtr[i] %s\n", cfg.IMSexeNamePtr[i]);
    printf("\tIMShostPtr[i] %s\n", cfg.IMShostPtr[i]);
    printf("\tIMS_CP_hostNamePtr[i] %s\n", cfg.IMS_CP_hostNamePtr[i]);
    printf("\tIMStemplatePtr[i] %s\n", cfg.IMStemplatePtr[i]);
    printf("\tIMSconfigFilePtr[i] %s\n", cfg.IMSconfigFilePtr[
i]);
    printf("\tIMSstopTimeInterval %d\n",
                     cfg.IMSstopTimeInterval[i]);
    printf("\tIMShaltTimeInterval %d\n",
                     cfg.IMShaltTimeInterval[i]);
printf("\n");
  }



  printf("PreQCpathPtr = %s\n",cfg.PreQCpathPtr); 
  printf("PreQChostPtr = %s\n",cfg.PreQChostPtr); 
  printf("PreQCexeNamePtr = %s\n",cfg.PreQCexeNamePtr); 
  printf("PreQCtemplatePtr = %s\n",cfg.PreQCtemplatePtr); 

#endif

#ifdef PROFILE_DEBUG
  printf("JobProfile \n");
  printf("  numJobProfiles %d\n", cfg.numJobProfiles);

  for (i=0; i < cfg.numJobProfiles; i++) {
    printf("  jobProfileSubsystem[i] %s\n", cfg.jobProfileSubsystem[i]);

    printf("\tplatforms: ");
    for (j=0; cfg.jobProfilePlatforms[i][j] != NULL; j++) 
      printf("%s ", cfg.jobProfilePlatforms[i][j]);
    printf("\n");

    printf("\tmodes: " );
    for (j=0; cfg.jobProfileModes[i][j] != NULL; j++) 
      printf("%s ", cfg.jobProfileModes[i][j]);
    printf("\n");

    printf("\tproduct types: ");
    for (j=0; cfg.jobProfileProductTypes[i][j] != NULL; j++) 
      printf("%s ", cfg.jobProfileProductTypes[i][j]);
    printf("\n");

    printf("\tquicklook flags: ");
    for (j=0; cfg.jobProfileQuicklookFlags[i][j] != NULL; j++) 
      printf("%s ", cfg.jobProfileQuicklookFlags[i][j]);
    printf("\n");

    printf("\tcompensation flags: ");
    for (j=0; cfg.jobProfileCompensationFlags[i][j] != NULL; j++) 
      printf("%s ", cfg.jobProfileCompensationFlags[i][j]);
    printf("\n");

    printf("\tpixel spacings: ");
    for (j=0; j < GLOBAL_subsysConfig.n_pixel_spacings[i]; j++) {
      fflush(stdout);
      printf("%lf ", cfg.jobProfilePixelSpacings[i][j]);
      fflush(stdout);
    }
    printf("\n");

    printf("\tsubframe ids: ");
    for (j=0; j < GLOBAL_subsysConfig.n_subframe_ids[i]; j++)  {
      printf("%d ", cfg.jobProfileSubframeIds[i][j]);
    }
    printf("\n");

    printf("\tframe modes: " );
    for (j=0; cfg.jobProfileFrameModes[i][j] != NULL; j++) 
      printf("%s ", cfg.jobProfileFrameModes[i][j]);
    printf("\n");

    printf("\tdeskews: " );
    for (j=0; cfg.jobProfileDeskews[i][j] != NULL; j++) 
      printf("%s ", cfg.jobProfileDeskews[i][j]);
    printf("\n");

    printf("\tprojections: " );
    for (j=0; cfg.jobProfileProjections[i][j] != NULL; j++) 
      printf("%s ", cfg.jobProfileProjections[i][j]);
    printf("\n");

    printf("\tterrain corrections: " );
    for (j=0; cfg.jobProfileTerrainCorrections[i][j] != NULL; j++) 
      printf("%s ", cfg.jobProfileTerrainCorrections[i][j]);
    printf("\n");


  }

#endif


  return(0);
}
