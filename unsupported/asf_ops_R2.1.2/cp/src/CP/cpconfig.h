#ifndef _cpconfig_h__
#define _cpconfig_h__

static char sccsid_cpconfig_h[] = "@(#)cpconfig.h	4.40 97/01/03 15:52:27";

#include "validate.h"

/*----------------------------------------------------------
 * NAME:
 *  cpconfig.h
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
/* default values for incorrectly entered config file entries */
#define DEF_PRINT_CMD "$ASF/bin/xgrabsc -id %d | lpr"
#define DEF_LOG_BROWSER_CMD "$ASF/bin/log_browser"
#define DEF_SCRIPTS_DIR "$ASF/bin"
#define DEF_SAVE_DIR "$LOCAL/cp/save"
#define DEF_SAVED_FILE "$LOCAL/cp/state/CP.current.state"
#define DEF_LAST_SAVED_FILE "$LOCAL/cp/state/CP.last.state"

#define DEF_MSG_TEMPLATE_FILE "$LOCAL/sps/templates/msgTemplate.odl"

#define DEF_CP_ERR_LOGFILE "$LOCAL/cp/logs/cp-err.asflog"
#define DEF_CP_INFO_LOGFILE "$LOCAL/cp/logs/cp-info.asflog"
#define DEF_CP_DEBUG_LOGFILE "$LOCAL/cp/logs/cp-debug.asflog"
#define DEF_SPS_ERR_LOGFILE "$LOCAL/sps/logs/sps-err.asflog"
#define DEF_SPS_INFO_LOGFILE "$LOCAL/sps/logs/sps-info.asflog"
#define DEF_SPS_DEBUG_LOGFILE "$LOCAL/sps/logs/sps-debug.asflog"

#define DEF_CAL_PARAMS_DIR "$LOCAL/cp/cal_params"
#define DEF_SCAN_DIR "$LOCAL/cp/scan"
#define DEF_CATALOG_DIR "$LOCAL/cp/tmp"

#define DEF_INPUT_BASEDIR "$LOCAL/cp/tmp"
#define DEF_OUTPUT_BASEDIR "$LOCAL/cp/tmp"
#define DEF_TMP_DIR "$LOCAL/cp/tmp"

#define DEF_RDS_SAVE_DIR "$LOCAL/rds"

#define DEF_SSP2_SAVE_DIR "/home/tmpdisk"         /* chosen by quyen... */
#define DEF_ASP_SAVE_DIR "/data0/asp/R1BP/hold"   /* chosen by ???... */

/* these are used in calls to getSavedStateFile() */
#define CURRENT_STATE_FILE     0
#define LAST_STATE_FILE        1
#define CURRENT_STATE_BASENAME 2

/* begin type definitions */

#define MAX_IMS  2

#define MAX_RDS 2
#define MAX_ASP 1
#define MAX_PP 0

#define MAX_SSP2 3
#define MAX_INPUT_DIRS 5
#define MAX_OUTPUT_DIRS 5
#define MAX_TAPE_ID 50

#define MAX_SUBSYSTEMS MAX_RDS + MAX_ASP + MAX_PP + MAX_SSP2

#define MAX_JOB_PROFILES 60 /* we have 15 now... allow for future expansion */
#define MAX_JOB_PLATFORMS XtNumber(SPS_platforms)
#define MAX_JOB_MODES XtNumber(SPS_modes)
#define MAX_JOB_PRODUCT_TYPES XtNumber(SPS_productTypes)
#define MAX_JOB_QUICKLOOK_FLAGS XtNumber(SPS_quicklookFlags)
#define MAX_JOB_COMPENSATION_FLAGS XtNumber(SPS_compensationFlags)
#define MAX_JOB_PRODUCT_CREATORS XtNumber(SPS_productCreators)

#define MAX_JOB_PIXEL_SPACINGS 10
#define MAX_JOB_SUBFRAME_IDS 10 /* number of possible subframe ids.  this */
                                /* value seems kind of gamy to hardcode */
                                /* but right now -1 through 8 are valid */

typedef struct {
                char   *configFilePtr;
                int    portId; 
                char   *printCmd;
                char   *logBrowserCmd;

                int    startTimeInterval;
                int    healthTimeInterval;
                int    requestSentTimeInterval;

                char   *scan_results_dirPtr;
                char   *scan_catalog_dirPtr;
                char   *cal_params_dirPtr;

                char   *CPrestoreDirPtr;
                char   *CPsaveDirPtr;
                char   *CPscriptsDirPtr;

                char   *cp_saved_basename;
                char   *cp_current_statefile;
                char   *cp_last_saved_statefile;
                char   *cp_autoload_state;

                char   *cp_err_logfile;
                char   *cp_info_logfile;
                char   *cp_debug_logfile;

                char   *sps_err_logfile;
                char   *sps_info_logfile;
                char   *sps_debug_logfile;

                char   *sps_retain_products;
                char   *sps_J1_routing;

                int    GPRnum;

                char   *GPRlogicalPtr;
                char   *GPRpathPtr;
                char   *GPRexeNamePtr;
                char   *GPRhostPtr;
                char   *GPRtemplatePtr;
                char   *GPRconfigFilePtr;

                int    QCnum;

                char   *QCpathPtr;
                char   *QCexeNamePtr;
                char   *QChostPtr;
                char   *QCtemplatePtr;
                char   *QCdisplayPtr;
                double  QCcomplexScaleFac;

                int    ScanQCnum;

                char   *ScanQCpathPtr;
                char   *ScanQCexeNamePtr;
                char   *ScanQChostPtr;
                char   *ScanQCtemplatePtr;
                char   *ScanQCdisplayPtr;

                int    PreQCnum;

                char   *PreQCpathPtr;
                char   *PreQCexeNamePtr;
                char   *PreQChostPtr;
                char   *PreQCtemplatePtr;

      int    numJobProfiles;

      char   *jobProfileSubsystem[MAX_SUBSYSTEMS];
      int    n_platforms[MAX_JOB_PROFILES];
      char   **jobProfilePlatforms[MAX_JOB_PROFILES];
      int    n_modes[MAX_JOB_PROFILES];
      char   **jobProfileModes[MAX_JOB_PROFILES]; 
      int    n_product_types[MAX_JOB_PROFILES];
      char   **jobProfileProductTypes[MAX_JOB_PROFILES]; 
      int    n_quicklook_flags[MAX_JOB_PROFILES];
      char   **jobProfileQuicklookFlags[MAX_JOB_PROFILES]; 
      int    n_compensation_flags[MAX_JOB_PROFILES];
      char   **jobProfileCompensationFlags[MAX_JOB_PROFILES]; 

      int    n_frame_modes[MAX_JOB_PROFILES];
      char   **jobProfileFrameModes[MAX_JOB_PROFILES]; 
      int    n_deskews[MAX_JOB_PROFILES];
      char   **jobProfileDeskews[MAX_JOB_PROFILES]; 
      int    n_projections[MAX_JOB_PROFILES];
      char   **jobProfileProjections[MAX_JOB_PROFILES]; 
      int    n_terrain_corrections[MAX_JOB_PROFILES];
      char   **jobProfileTerrainCorrections[MAX_JOB_PROFILES]; 

      int    n_pixel_spacings[MAX_JOB_PROFILES];
      double jobProfilePixelSpacings[MAX_JOB_PROFILES][MAX_JOB_PIXEL_SPACINGS]; 

      int    n_subframe_ids[MAX_JOB_PROFILES];
      int  jobProfileSubframeIds[MAX_JOB_PROFILES][MAX_JOB_SUBFRAME_IDS]; 


                int    RDSnum;

                char   *RDSlogicalPtr[MAX_RDS];
                char   *RDSbackground[MAX_RDS];
                char   *RDSpathPtr[MAX_RDS];
                char   *RDSexeNamePtr[MAX_RDS];
                char   *RDShostPtr[MAX_RDS];
                char   *RDS_CP_hostNamePtr[MAX_RDS];
                char   *RDStemplatePtr[MAX_RDS];
                char   *RDSconfigFilePtr[MAX_RDS];
                char   *RDSsaveDirPtr[MAX_RDS];
                char   *RDSmediaType[MAX_RDS]; 
                int    RDSstopTimeInterval[MAX_RDS];
                int    RDShaltTimeInterval[MAX_RDS];
                int    RDScategory[MAX_RDS]; 

                int    SSP2num;

                char   *SSP2logicalPtr[MAX_SSP2];
                char   *SSP2background[MAX_SSP2];
                char   *SSP2pathPtr[MAX_SSP2];
                char   *SSP2exeNamePtr[MAX_SSP2];
                char   *SSP2hostPtr[MAX_SSP2];
                char   *SSP2_CP_hostNamePtr[MAX_SSP2];
                char   *SSP2templatePtr[MAX_SSP2];
                char   *SSP2configFilePtr[MAX_SSP2];
                int    SSP2stopTimeInterval[MAX_SSP2];
                int    SSP2haltTimeInterval[MAX_SSP2]; 
                int    SSP2category[MAX_SSP2]; 
                char   **SSP2input_data_dirPtr[MAX_SSP2];
                char   **SSP2output_data_dirPtr[MAX_SSP2]; 
                char   *SSP2saveDirPtr[MAX_SSP2];

                int    ASPnum;

                char   *ASPlogicalPtr;
                char   *ASPbackground;
                char   *ASPpathPtr;
                char   *ASPexeNamePtr;
                char   *ASPhostPtr;
                char   *ASP_CP_hostNamePtr;
                char   *ASPtemplatePtr;
                char   *ASPconfigFilePtr;
                int    ASPstopTimeInterval;
                int    ASPhaltTimeInterval; 
                int    ASPcategory;
                char   **ASPoutput_data_dirPtr;
                char   *ASPsaveDirPtr;

                int    PPSnum;

                char   *PPSlogicalPtr;
                char   *PPSbackground;
                char   *PPSpathPtr;
                char   *PPSexeNamePtr;
                char   *PPShostPtr;
                char   *PPS_CP_hostNamePtr;
                char   *PPStemplatePtr;
                char   *PPSconfigFilePtr;
                int    PPSstopTimeInterval;
                int    PPShaltTimeInterval; 
                int    PPScategory;
                int    PPSqueueSize; 

                int    IMSnum;

                char   *IMSlogicalPtr[MAX_IMS];
                char   *IMSbackground[MAX_IMS];
                char   *IMSpathPtr[MAX_IMS];
                char   *IMSexeNamePtr[MAX_IMS];
                char   *IMShostPtr[MAX_IMS];
                char   *IMS_CP_hostNamePtr[MAX_IMS];
                char   *IMStemplatePtr[MAX_IMS];
                char   *IMSconfigFilePtr[MAX_IMS];
                int    IMSstopTimeInterval[MAX_IMS];
                int    IMShaltTimeInterval[MAX_IMS];
                int    IMScategory[MAX_IMS];

} subsysConfigType;

typedef struct {
  char IDstr[50];
  int  queueIndx;
  int  useCount;
} RDSqueueIDtype;

/* begin prototypes */
subsysConfigType *getCPconfig();
int setupCPconfig(char *fileNamePtr);
char *getSavedStateFile(int last);
char *getSSP2inputDirName(int whichSSP, int whichDir);
char *getSSP2outputDirName(int whichSSP, int whichDir);
char *getASPoutputDirName(int whichDir);
char *getSaveDir();
char *getRestoreDir();
int get_CPsyslog_level();
int get_SPSsyslog_level();
int assignRDS(char *mediaIDPtr, char *mediaTypePtr);
char *getRDSmediaType(int whichRDS);
char *getRDSname(int whichRDS);
char *getNextSSPname();
char *getNextRDSname();
char *getSSP2name(int i);
int getNumIMSs();
int getNumGPRs();
int getNumQCs();
int getNumScanQCs();
int getNumSSP2s();
int getNumRDSs();
char *getGPRname();
char *getASPname();
char *getPPSname();
char *getIMSname();
char *getIMSnameGivenCatalogStep();
int getPPSqueueSize();
void setPPSqueueSize(int size);

char *getJobProfileSubsystem(int which);
char **getJobProfilePlatforms(int which, int *howMany);
char **getJobProfileModes(int which, int *howMany);
char **getJobProfileProductTypes(int which, int *howMany);
char **getJobProfileQuicklookFlags(int which, int *howMany);
char **getJobProfileCompensationFlags(int which, int *howMany);
char **getJobProfileFrameModes(int which, int *howMany);
char **getJobProfileDeskews(int which, int *howMany);
char **getJobProfileProjections(int which, int *howMany);
char **getJobProfileTerrainCorrections(int which, int *howMany);
int *getJobProfileSubframeIds(int which, int *howMany);
double *getJobProfilePixelSpacings(int which, int *howMany);

char *get_subnet_inetPtr(char *namePtr);
char *getPortIDString();
char *getPrintCommand();
char *getScanResultsDir();
char *getCalParamsDir();
int getResetTimeInterval(char *dest);
int getStopTimeInterval(char *dest);
int getHealthTimeInterval();
int getStartTimeInterval();
int getRequestSentTimeInterval();

char *getRDSsaveDir(int whichRDS);
char *getASPsaveDir();
char *getSSP2saveDir(int whichSSP);

int getPortID();

int special_J1_routing();
int retainProducts();

int autoLoadState();

/* end  prototypes */

/* config ODL names */
#define PORT_ID                     "CONFIG_FILE.PORT_ID" 
#define PRINT_COMMAND               "CONFIG_FILE.PRINT_COMMAND" 
#define LOG_BROWSER                 "CONFIG_FILE.LOG_BROWSER" 

#define START_TIME_INTERVAL         "CONFIG_FILE.SPS.START_TIME_INTERVAL" 
#define HEALTH_TIME_INTERVAL        "CONFIG_FILE.SPS.HEALTH_TIME_INTERVAL" 
#define REQUEST_SENT_TIME_INTERVAL  "CONFIG_FILE.SPS.REQUEST_SENT_TIME_INTERVAL" 
/* root file directories */

#define INPUT_DATA_DIR        "CONFIG_FILE.SPS.INPUT_BASEDIR"
#define OUTPUT_DATA_DIR        "CONFIG_FILE.SPS.CEOS_OUTPUT_BASEDIR"

#define CAL_PARAMS_DIR             "CONFIG_FILE.SPS.CAL_PARAMS_BASEDIR"
#define SCAN_RESULTS_DIR           "CONFIG_FILE.SPS.SCAN_RESULTS_BASEDIR" 
#define SCAN_CATALOG_DIR           "CONFIG_FILE.SPS.SCAN_CATALOG_BASEDIR" 

#define CP_RESTORE_DIR              "CONFIG_FILE.CP.RESTORE_DIR" 
#define CP_SAVE_DIR                 "CONFIG_FILE.CP.SAVE_DIR" 
#define CP_SCRIPTS_DIR              "CONFIG_FILE.CP.SCRIPTS_DIR" 

#define CP_SAVED_STATEFILE          "CONFIG_FILE.CP.SAVED_STATEFILE"
#define CP_LAST_SAVED_STATEFILE     "CONFIG_FILE.CP.LAST_SAVED_STATEFILE"
#define CP_AUTOLOAD_STATE           "CONFIG_FILE.CP.AUTOLOAD_STATE"

#define CP_ERR_LOGFILE              "CONFIG_FILE.CP.ERR_LOGFILE"
#define CP_INFO_LOGFILE             "CONFIG_FILE.CP.INFO_LOGFILE"
#define CP_DEBUG_LOGFILE            "CONFIG_FILE.CP.DEBUG_LOGFILE"
 
#define SPS_ERR_LOGFILE             "CONFIG_FILE.SPS.ERR_LOGFILE"
#define SPS_INFO_LOGFILE            "CONFIG_FILE.SPS.INFO_LOGFILE"
#define SPS_DEBUG_LOGFILE           "CONFIG_FILE.SPS.DEBUG_LOGFILE"
#define SPS_RETAIN_PRODUCTS         "CONFIG_FILE.SPS.RETAIN_PRODUCTS"
#define SPS_J1_ROUTING              "CONFIG_FILE.SPS.J1_ROUTING"

#define GPR_NUM                     "CONFIG_FILE.GPR.MAX_CONCURRENT" 
#define GPR_LOGICAL                 "CONFIG_FILE.GPR.LOGICAL"
#define GPR_EXE_NAME                "CONFIG_FILE.GPR.EXECUTABLE" 
#define GPR_PATH                    "CONFIG_FILE.GPR.PATH" 
#define GPR_HOST                    "CONFIG_FILE.GPR.HOST" 
#define GPR_MSG_TEMP                "CONFIG_FILE.GPR.MSG_TEMP" 
#define GPR_CONFIG_FILE             "CONFIG_FILE.GPR.CONFIG_FILE" 

#define QC_OBJECT                   "CONFIG_FILE.QC" 
#define QC_NUM                      "CONFIG_FILE.QC.MAX_CONCURRENT" 
#define QC_EXE_NAME                 "CONFIG_FILE.QC.EXECUTABLE" 
#define QC_PATH                     "CONFIG_FILE.QC.PATH" 
#define QC_HOST                     "CONFIG_FILE.QC.HOST" 
#define QC_MSG_TEMP                 "CONFIG_FILE.QC.MSG_TEMP" 
#define QC_DISPLAY                  "CONFIG_FILE.QC.DISPLAY" 
#define QC_CX_SCALE_FAC             "CONFIG_FILE.QC.COMPLEX_SCALE_FAC" 

#define SCAN_QC_OBJECT                   "CONFIG_FILE.SCAN_QC" 
#define SCAN_QC_NUM                      "CONFIG_FILE.SCAN_QC.MAX_CONCURRENT" 
#define SCAN_QC_EXE_NAME                 "CONFIG_FILE.SCAN_QC.EXECUTABLE" 
#define SCAN_QC_PATH                     "CONFIG_FILE.SCAN_QC.PATH" 
#define SCAN_QC_HOST                     "CONFIG_FILE.SCAN_QC.HOST" 
#define SCAN_QC_MSG_TEMP                 "CONFIG_FILE.SCAN_QC.MSG_TEMP" 
#define SCAN_QC_DISPLAY                  "CONFIG_FILE.SCAN_QC.DISPLAY" 

#define PRE_QC_OBJECT                   "CONFIG_FILE.PRE_QC" 
#define PRE_QC_NUM                      "CONFIG_FILE.PRE_QC.MAX_CONCURRENT"
#define PRE_QC_EXE_NAME                 "CONFIG_FILE.PRE_QC.EXECUTABLE"
#define PRE_QC_PATH                     "CONFIG_FILE.PRE_QC.PATH"
#define PRE_QC_HOST                     "CONFIG_FILE.PRE_QC.HOST"
#define PRE_QC_MSG_TEMP                 "CONFIG_FILE.PRE_QC.MSG_TEMP"

#define SUBSYS_TABLE "CONFIG_FILE.PROCESSOR_SELECTION_TABLE"
#define NUM_JOB_PROFILES "CONFIG_FILE.NUM_JOB_PROFILES"



#define RDS_OBJECT                  "CONFIG_FILE.RDS"
#define RDS_NUM                     "CONFIG_FILE.RDS_MAX_CONCURRENT"
#define RDS_LOGICAL                 "CONFIG_FILE.RDS.LOGICAL"
#define RDS_BACKGROUND              "CONFIG_FILE.RDS.BACKGROUND"
#define RDS_PATH                    "CONFIG_FILE.RDS.PATH"
#define RDS_EXE_NAME                "CONFIG_FILE.RDS.EXECUTABLE"
#define RDS_HOST                    "CONFIG_FILE.RDS.HOST"
#define RDS_CP_HOSTNAME             "CONFIG_FILE.RDS.CP_HOSTNAME" 
#define RDS_MSG_TEMP                "CONFIG_FILE.RDS.MSG_TEMP" 
#define RDS_CONFIG_FILE             "CONFIG_FILE.RDS.CONFIG_FILE"
#define RDS_HALT_TIME_INTERVAL      "CONFIG_FILE.RDS.HALT_TIME_INTERVAL" 
#define RDS_STOP_TIME_INTERVAL      "CONFIG_FILE.RDS.STOP_TIME_INTERVAL" 
#define RDS_SAVE_DIR                "CONFIG_FILE.RDS.SAVE_DIR" 
#define RDS_MEDIA_TYPE              "CONFIG_FILE.RDS.MEDIA_TYPE" 

#define SSP_PP_OBJECT_NAME "SSP_PP"

#define SSP2_OBJECT                 "CONFIG_FILE.SSP_PP"
#define SSP2_NUM                    "CONFIG_FILE.SSP_PP_MAX_CONCURRENT"
#define SSP2_LOGICAL                "CONFIG_FILE.SSP_PP.LOGICAL"
#define SSP2_BACKGROUND             "CONFIG_FILE.SSP_PP.BACKGROUND"
#define SSP2_PATH                   "CONFIG_FILE.SSP_PP.PATH"
#define SSP2_EXE_NAME               "CONFIG_FILE.SSP_PP.EXECUTABLE"
#define SSP2_HOST                   "CONFIG_FILE.SSP_PP.HOST"
#define SSP2_CP_HOSTNAME            "CONFIG_FILE.SSP_PP.CP_HOSTNAME" 
#define SSP2_MSG_TEMP               "CONFIG_FILE.SSP_PP.MSG_TEMP" 
#define SSP2_CONFIG_FILE            "CONFIG_FILE.SSP_PP.CONFIG_FILE"
#define SSP2_HALT_TIME_INTERVAL     "CONFIG_FILE.SSP_PP.HALT_TIME_INTERVAL" 
#define SSP2_STOP_TIME_INTERVAL     "CONFIG_FILE.SSP_PP.STOP_TIME_INTERVAL" 
#define SSP2_INPUT_DATA_DIR         "CONFIG_FILE.SSP_PP.INPUT_BASEDIR"
#define SSP2_OUTPUT_DATA_DIR        "CONFIG_FILE.SSP_PP.CEOS_OUTPUT_BASEDIR"
#define SSP2_SAVE_DIR                "CONFIG_FILE.SSP_PP.SAVE_DIR" 

#define ASP_OBJECT                  "CONFIG_FILE.ASP"
#define ASP_NUM                     "CONFIG_FILE.ASP.MAX_CONCURRENT"
#define ASP_LOGICAL                 "CONFIG_FILE.ASP.LOGICAL"
#define ASP_BACKGROUND              "CONFIG_FILE.ASP.BACKGROUND"
#define ASP_PATH                    "CONFIG_FILE.ASP.PATH"
#define ASP_EXE_NAME                "CONFIG_FILE.ASP.EXECUTABLE"
#define ASP_HOST                    "CONFIG_FILE.ASP.HOST"
#define ASP_CP_HOSTNAME             "CONFIG_FILE.ASP.CP_HOSTNAME" 
#define ASP_MSG_TEMP                "CONFIG_FILE.ASP.MSG_TEMP" 
#define ASP_HALT_TIME_INTERVAL      "CONFIG_FILE.ASP.HALT_TIME_INTERVAL" 
#define ASP_STOP_TIME_INTERVAL      "CONFIG_FILE.ASP.STOP_TIME_INTERVAL" 
#define ASP_OUTPUT_DATA_DIR         "CONFIG_FILE.ASP.CEOS_OUTPUT_BASEDIR"
#define ASP_SAVE_DIR                "CONFIG_FILE.ASP.SAVE_DIR" 

#define PPS_OBJECT                  "CONFIG_FILE.PPS"
#define PPS_NUM                     "CONFIG_FILE.PPS.MAX_CONCURRENT"
#define PPS_LOGICAL                 "CONFIG_FILE.PPS.LOGICAL"
#define PPS_BACKGROUND              "CONFIG_FILE.PPS.BACKGROUND"
#define PPS_PATH                    "CONFIG_FILE.PPS.PATH"
#define PPS_EXE_NAME                "CONFIG_FILE.PPS.EXECUTABLE"
#define PPS_HOST                    "CONFIG_FILE.PPS.HOST"
#define PPS_CP_HOSTNAME             "CONFIG_FILE.PPS.CP_HOSTNAME" 
#define PPS_MSG_TEMP                "CONFIG_FILE.PPS.MSG_TEMP" 
#define PPS_HALT_TIME_INTERVAL      "CONFIG_FILE.PPS.HALT_TIME_INTERVAL" 
#define PPS_STOP_TIME_INTERVAL      "CONFIG_FILE.PPS.STOP_TIME_INTERVAL" 
#define PPS_MAX_PENDING             "CONFIG_FILE.PPS.MAX_PENDING"
#define PPS_CONFIG_FILE             "CONFIG_FILE.PPS.CONFIG_FILE"

#define IMS_OBJECT                  "CONFIG_FILE.IMS"
#define IMS_NUM                     "CONFIG_FILE.IMS_MAX_CONCURRENT"
#define IMS_LOGICAL                 "CONFIG_FILE.IMS.LOGICAL"
#define IMS_BACKGROUND              "CONFIG_FILE.IMS.BACKGROUND"
#define IMS_PATH                    "CONFIG_FILE.IMS.PATH"
#define IMS_EXE_NAME                "CONFIG_FILE.IMS.EXECUTABLE"
#define IMS_HOST                    "CONFIG_FILE.IMS.HOST"
#define IMS_CP_HOSTNAME             "CONFIG_FILE.IMS.CP_HOSTNAME"
#define IMS_MSG_TEMP                "CONFIG_FILE.IMS.MSG_TEMP"
#define IMS_HALT_TIME_INTERVAL      "CONFIG_FILE.IMS.HALT_TIME_INTERVAL"
#define IMS_STOP_TIME_INTERVAL      "CONFIG_FILE.IMS.STOP_TIME_INTERVAL"
#define IMS_CONFIG_FILE             "CONFIG_FILE.IMS.CONFIG_FILE"




/* end config ODL names */

#endif   /* !_cpconfig_h__ */

