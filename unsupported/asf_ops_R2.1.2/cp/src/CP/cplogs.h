#ifndef _cplogs_h__
#define _cplogs_h__

static char sccsid_cplogs_h[] = "@(#)cplogs.h	1.77 97/02/20 12:58:37";

/*----------------------------------------------------------
 * NAME:
 *  cplogs.h
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/

/****************************************************************************

            LOG_INFO messages

****************************************************************************/

#define STARTING_CP     "Starting CP\n"
#define CP_READY        "CP Ready\n"
#define CP_VER_READY        "%s Ready\n"
#define STARTING_SUBSYS "Starting subsystem %s\n"
#define STARTING_GPR    "Starting GPR Utility\n"
#define ALREADY_RUNNING    "%s is already running\n"
#define MAX_RUNNING    "Maximum number of %s applications already running\n"

#define SENDING_SHUTDOWN "Shutting down %s\n"
#define SENDING_HALT     "Resetting %s\n"
#define HAS_SHUTDOWN     "%s has shutdown\n"
#define SENDING_SHUTDOWN_WHILE_PROC "Shutting down %s while processing job %d\n"
#define SENDING_HALT_WHILE_PROC "Resetting %s while processing job %d\n"
#define SENDING_SHUTDOWN_NOT_STARTED "Subsystem %s did not finish starting\n"
#define PAUSING_IDLE_SUBSYS "%s processing being paused immediately"
#define PAUSING_ACTIVE_SUBSYS \
       "%s processing being paused after job %d completes"

#define RCVD_JOB         "Received job %d"

#define SITE_NAME        "Site name = %s \n"
#define SENDING_JOB      "Sending %s job %d to %s \n"
#define SENDING_CLEANUP  "Sending cleanup request to %s \n"
#define JOB_ID           "Job ID = %d\n"
#define REQ_TYPE         "Request type = %s \n"
#define STATE_VEC_TYPE   "State vector type = %s \n"
#define INSTR_MODE       "Instrument mode = %s \n"
#define FRAME_ID         "Frame ID =  %d \n"

#define VALUE_OF_EQUAL   "Value of %s file = %s\n"
#define FILE_EQUAL       "%s file = %s\n"


#define SCAN_RESULTS     "Scan Results"   /* means same as FRAME_FILE */


#define RAW_DATA         "Raw Data"
#define ECHO_DATA        "Echo Data"
#define AUX_DATA         "Auxiliary Data"
#define REPLICA_DATA     "Replica Data"
#define EPHEM_DATA       "Ephemeris Data"
#define CALIBRATION      "Calibration"
#define CAL_PARAMS       "Calibration Params"
#define PMF_DATA         "PMF Data"
#define BURST_DATA       "Burst Offset Data"


#define CEOS_DATA        "CEOS Data"   /* means same as CEOS_IMAGE */
#define CEOS_IMAGE       "CEOS Image"  /* means same as CEOS_DATA */
#define IMAGE_FILE       "Image File"  /* is this different than CEOS_IMAGE? */

#define TRACK_GAIN       "Cross Track Gain Profile"
#define CEOS_LEADER      "CEOS Leader"
#define CEOS_TRAILER     "CEOS Trailer"

#define PROCESS_NOT_READY_TEXT    "Process %s is not ready"
#define READY_FOR_REQS_TEXT     "%s ready to accept requests"
#define SENT_ACK_TEXT "%s has acknowledged receipt of job %d"
#define SUBSYS_COMPLETED_HALT_TEXT "%s has reset"
#define OK_COMPLETED_PROCESSING_TEXT "%s completed job %d, status OK"
#define BAD_COMPLETED_PROCESSING_TEXT "%s completed job %d, error status %d"
#define PRE_QC_TEXT "%s job %d, ready for pre-qc image averaging"
#define READY_TO_QC_COMPLETED_PROCESSING_TEXT "%s job %d, ready to Q/C"
#define BAD_QC_COMPLETED_PROCESSING_TEXT \
                                  "%s Q/C completed job %d, error status %d"
#define AUTO_IMAGE_AVG "Auto-spawning Image Averager: %s jobid %d "
#define AUTO_SCAN_QC   "Auto-spawning Scan QC: %s jobid %d "
#define AUTO_QC        "Auto-spawning QC: %s jobid %d "

#define CP_GOODBYE         "Goodbye"

#define CHECKING_FILE_EXISTENCE "Checking existence of %s"
#define SUBSYS_FILES_INCOMPLETE \
          "Subsystem-created intermediate files are not complete"
#define PPS_REQUESTING_JOB "Requesting (%s,%s) job from %s\n"
#define PPS_NO_JOBS_OF_TYPE "No (%s, %s) jobs are available"
#define PPS_NO_JOBS_OF_ANY_TYPE "No jobs of any type available.  CP will wait a short period of time before requesting another."
#define PPS_JOB_GEN_ERROR  "%s could not generate job because: %s.\nThe subsystem is being reset, and will request no more jobs until readied."
#define PPS_FAILURE_PROMPT \
            "Please enter the reason that job %d could not be completed"
#define PPS_FAILURE_LOG "Job %d not completed because: %s" 

#define JOB_INPUT_DIR "Job %d: input directory selected %s"
#define JOB_OUTPUT_DIR "Job %d: output directory selected %s"

/****************************************************************************

            LOG_DEBUG messages

****************************************************************************/

#define READ_STARTED       "starting read thread processing, socket = %d"
#define ACCEPT_TH          "Created Accept thread, pid = %d\n"
#define THREAD_NOT_EXIST   "Thread %d %s thread %d does not exist\n"
#define THREAD_READ_ERR    "Thread read failed on socket %d: %s\n"
#define THREAD_RCVD_MSG    "Thread received msg_type = %s on socket %d\n"

#define FILE_NOT_EXIST   "File %s does not exist\n"

#define RCVD_SUBSYS_READY "Processing SUBSYSTEM_READY from %s, state %d\n"
#define HANDLING_NEW_PROC   "handling new process %s\n"

#define RCVD_SUBSYS_COMPLETE "Job %d completed, %s state %d"
#define RCVD_SUBSYS_STATUS "Processing SUBSYSTEM_STATUS from %s for job %d; subsys state %d"

#define DEB_ERROR_READING_SCAN_RESULTS \
         "Scan results: cannot get %s\n"
#define DEB_ERROR_GETTING_REQ "Getting %s message: cannot get %s\n"
#define DEB_ERROR_SETTING_MSG "Setting up %s message: failed on  %s\n"
#define DEB_ERROR_READING_MSG "Incoming message error: %s\n"
#define DEB_ERROR_BUILDING_DISPLAY_MSG "Building of display string failed at %s\n"

#define SENDING_HALT_SOCKET    "sending RESET to process: %s socket: %d\n"
#define STOP_ALL_MSG           "Stopping all processes and threads\n"

#define LIST_POS_ERROR     "%s display list item selection error -- %d\n"
#define LIST_SEL_ERROR     "No item selected for %s"

#define SOCKET_GOT_MSG     "socket %d got a message from %s"
#define CLOSING_SOCKET     "closing %s socket %d\n"
#define CLOSING_SOCKET_EXIT "Closing socket %d and exiting %s thread %d"
#define MAX_SOCKET_ERROR_TEXT "Maximum number of thread read failures"

#define UNRECOG_MSG_TYPE   "unrecognized message type %s"
#define UNRECOG_SUB_TYPE   "unrecognized message sub-type %s"

#define CANNOT_KILL_PID    "Cannot kill %s pid %d\n"
#define CANNOT_GET_PID_EL  "Cannot get PID element\n"

#define CREATE_BASE_LIST   "creating base list\n"

#define NOT_IN_MSTRQ       "Cannot find position in Master Queue\n"
#define MSTRQ_ADDING       "Master Queue: adding job %d at position %d\n"
#define MSTRQ_BAD_INDEX    "Bad index into Master Queue: %d\n"
#define MSTRQ_REQ_STAT     "Master Queue: handling job %d, state %s(%d)\n"
#define MSTRQ_CAUGHT       "Subsys will handle job %d, state %s(%d)\n"
#define ERROR_ADDING_MQ_ID "Cannot add job %d to Master Queue"
#define ERROR_ADDING_SQ_ID "Cannot add job %d to %s Subsystem Queue"
#define DUP_JOB_EXISTS "Received duplicate job %d -- discarding latest copy"
#define RETURNING_JOB "Job %d being sent back to %s.\n%s"

#define DOMAIN_CHECK_FAILED "Reason: Domain Check Failed on %s"
#define JOB_PROFILE_FAILED  "Reason: Job Profile Consistency Test Failed"


#define Q_ADDING           "%s queue: adding job %d at position %d\n"


#define SYSQ_INVALID_MODE   "Invalid operation requested of System Queue"
#define SYSQ_REMOVING       "removing element %d from %s sysque\n"
#define SYSQ_NO_REMOVE      "could not remove elem %d from %s sysque\n"

#define DID_NOT_HEALTH_ACK  "Did not receive health message for %s\n"
#define DID_NOT_REQ_ACK     "Did not receive ACK for %s job %d\n"
#define SENDING_ODL_TO       "%s sending out ODL request to %s\n"
#define REMOVING_ODL         "Master Queue: removing ODL from MSTRQ list\n"
#define PROCESS_GONE         "Process %s has terminated \n"

#define JOB_QUEUE_CHANGE     "Job %d moving from %s queue to %s queue"
#define JOB_STATUS_CHANGE    "Job %d moving from state %d to state %d"
#define JOB_INTERRUPTED      "Job %d interrupted by subsystem stop or reset\n"
#define SENDING_HEARTBEAT    "Sending heartbeat to %s on socket %d\n"

#define ENTER_SIG_CLD  "CP detected a child (pid %d) has terminated, exit status %d\n"
#define SIG_CLD_SHUTDOWN  "CP: %s has shutdown, pid %d\n"
#define ACCEPT_SIG_CLD "CP detected a thread has terminated (accept_sig_cld)\n"
#define ACCEPT_STATUS  "CP accept status = %d for pid %d\n"

#define ATTEMPT_BIND    "attempting bind %d on portID %d\n"
#define ATTEMPT_STOP    "attempting to stop process %s | pid %d | socket %d\n"
#define SOCKET_BOUND    "bound to socket %d on port %d\n"

#define SPAWNING        "spawning %s\n"
#define CHILD_SPAWNED   "Child process %s spawned\n"
#define ERROR_SPAWNING   "Cannot spawn process %s: file %s not found"
#define LAUNCHED        "launched %s, pid %d \n"
#define LAUNCH_ARGS     "argp[%d] = %s\n"

#define SPAWNING_QC_REQ "spawning %s for job %d\n"
#define QC_STATUS       "%s returned status = %d\n"

#define PERFORMING_QC   "Performing QC"
#define PERFORMING_SCAN_QC   "Performing Scan QC"
#define QC_LEADER_IMAGE "QC leader file = %s, image file = %s\n"
#define QC_SCAN_RESULTS "QC scan results file = %s\n"

#define LIST_POS_JOB_ID   "%s, list position = %d jobId = %d\n"
#define JOB_STATUS        "CP status of job %d = %d\n"
#define SIGCLD_QC_STATUS "%s completed: job %d to state %d\n"
#define JOB_STATUS_SUBSYS "%s Subsys Queue: handling job %d state %s(%d)"
#define DEFER_JOB_TAPE_PROMPT  \
                "Deferring job %d because prompting for tape mount on job %d"
#define DEFER_JOB_TAPE  \
                "Deferring job %d because %d is checking the tape label on %s"
#define DEFER_JOB_RUN  "Deferring job %d because %d is already running on %s"
#define DEFER_JOB_QC   "Deferring job %d to process QC completion of job %d"
#define DEFER_JOB_DONE "Deferring job %d to process completion of job %d"

#define CAPTURE_ACCEPT_THREAD "capturing the accept thread\n"
#define STOPPING_SUBSYS       "Stopping Subsystem %s\n"

#define LAUNCHED_TEXT		"%s launched"
#define TERMINATED_TEXT         "%s terminated normally"
#define SUBSYS_RESET_TEXT      "%s reset "
#define PLACED_ON_QUE_TEXT      "%s placed job %d on %s queue"
#define CP_RECEIVED_REQ_TEXT    "%s received request from %s"
#define CP_SENT_REQUEST_TEXT    "%s sent job %d to %s"

#define QC_TERMINATED_WITH_STATUS "%s has terminated with status %s, value %d"

#define STARTED_PROCESSING_TEXT   "%s started job %d"
#define CP_COMPLETED_TEXT         "%s completed cycle for job %d"

#define CREATING_DIR "Creating directory %s"

#define QUESTION_CHOICE  "%s chosen to question on %s job %d "
#define NOT_READY_FOR_EXIT "CP not ready for shutdown : %s in invalid state %d"

#define DOING_CLEANUP  "%s performing cleanup for job %d"

/****************************************************************************

            LOG_ERR messages

****************************************************************************/

#define NO_DISPLAY "Environment variable DISPLAY not defined: CP exiting."
#define BAD_DISPLAY "Can't open DISPLAY (%s): CP exiting.\n"
#define MALLOC_ERROR  "Error allocating memory when %s"
#define MKDIR_ERROR   "Error creating directory %s/%s/%d\n"

#define PRODUCT_NOT_EXIST "Couldn't find %s-created file %s"
#define JOB_NOT_ON_QUEUE "Job %d is not on %s queue, cannot perform %s"
#define CLEANUP_JOB_DELETED "Received cleanup error for deleted job %d"

#define RESTORE_FILE_BAD "The contents of restored file %s\n are incompatible with this version of the CP.\n\nThe state of jobs contained in this file is unreliable."
#define RESTORE_FILE_GONE "Unable to open restored file %s"

#define JOB_MOVE_INVALID "Job %d ineligible for inter-subsystem movement"
#define JOB_MOVE_BAD_STATE \
       "Job %d in invalid state (%s) inter-subsystem movement"
#define JOB_MOVE_BAD_DEST "No destination exists for %s job %d with mode %s"

#define JOB_PROFILE_MISMATCH  "Job profile inconsistency"
#define CAL_PARAM_MISMATCH  \
   "Processor incorrectly specified in Calibration Parameters File Name"
#define NO_CAL_PARAM_FILE \
    "Cannot get name of Primary Calibration Parameters File"
#define NEITHER_CAL_PARAMS_MATCHED \
     "Neither cal params file (%s or %s) matched processor %s"
#define CAL_PARAMS_MATCHED "Cal params file chosen: %s"


#define EXPECTED_PRODUCT_SIZE "Expected %s %s product size: %d blocks"
#define CHECKING_DISK_SPACE  "Checking available disk space on %s"
#define NO_DISK_SPACE        "Not enough disk space to create %s %s products"

#define CANNOT_OPEN_SCAN_RESULTS   "Cannot open scan results file %s"

#define CANNOT_DETERMINE           "Cannot determine %s\n"
#define CANNOT_FIND                "Cannot find %s\n"
#define CANNOT_LOCATE              "Cannot locate %s\n"
#define CANNOT_REMOVE              "Cannot remove %s\n"

#define CANNOT_SET                 "Cannot set %s\n"
#define CANNOT_SET_FILE            "Cannot set %s File\n"
#define CANNOT_SET_RECURSIVE       "Cannot set recursive for name list\n"

#define CANNOT_CHANGE_STATUS       "Cannot change %s status\n"
#define CANNOT_CHANGE_TO_RUN       "Cannot change %s state to running\n"
#define CANNOT_CHANGE_TO_STARTED   "Cannot change %s state to started\n"
#define CANNOT_CHANGE_TO_HALTED    "Cannot change %s state to reset\n"
#define CANNOT_CHANGE_MSTRQ_STATUS "Cannot change mstrq status\n"

#define CANNOT_SEND_HEARTBEAT      "Cannot send heartbeat"
#define CANNOT_SEND_MSG_ERRNO "Cannot send message to %s; %s"
#define CANNOT_SEND_MSG_SOCKET "Cannot send message to %s; socket communication failure"

#define CANNOT_GET          "Cannot get %s\n"
#define CANNOT_GET_COLOR    "Cannot convert color for Queue window background\n"
#define CANNOT_GET_NEW_ODL  "Cannot get new ODL PROD REQ msg for = %s\n"
#define CANNOT_GET_HOSTNAME  "Cannot get hostname\n"
#define CANNOT_GET_INET_ADDR "Cannot get internet address for %s\n"
#define CANNOT_GET_CONFIG_INET_ADDR  \
  "Cannot get internet address for %s hostname from configuration file\n"
#define CANNOT_GET_SUBNET_ADDR \
   "Cannot determine subnet internet address for subsystem\n"
#define CANNOT_GET_SUBNET_HOST \
   "Cannot get the %s subnet hostname from configuration file\n"
#define CANNOT_GET_PROCNAME_TO "Cannot get name of subsystem to %s\n"
#define CANNOT_GET_PID_TO      "Cannot get process ID of element to %s\n"
#define CANNOT_GET_MAX_CONCURRENT \
    "Invalid value for %s.MAX_CONCURRENT in the configuration file"
#define CANNOT_GET_OBJECT "Invalid or missing %s object in configuration file"
#define CANNOT_GET_ALL_OBJECTS  \
    "Incorrect number of %s objects (%d) in configuration file"

#define ERROR_NO_SSP "Cannot assign job to %s.\n No SSP subsystem is defined in configuration file."
#define ERROR_DEVICES_FULL "Cannot assign job to %s.\n No valid output directory specified,\n or all output devices are full"
#define ERROR_READING_SCAN_RESULTS "Scan results read error: %s: see debug log for details\n"
#define ERROR_READING_MSG "Incoming message error: see debug log for details\n"
#define ERROR_GETTING_REQ \
   "Error getting incoming message: see debug log for details\n"
#define ERROR_SETTING_MSG "Message setup error: see debug log for details\n"
#define ERROR_BUILDING_DISPLAY_MSG "Building of display string failed at %s\n"
#define old_ERROR_BUILDING_DISPLAY_MSG \
   "Display string build error:see debug log for details\n"
#define CP_DISPLAY_SETUP_ERR "Problem with %s in incoming message"

#define SRF_CONTENT_ERROR "Can't find %s in Scan Results File"

#define CANNOT_INIT_PTHREADS \
   "Cannot initialize pthreads, check write access to directory\n"

#define CANNOT_GET_CONFIG "Cannot get CP configuration file, exiting\n"
#define CANNOT_PARSE_CONFIG "Cannot parse CP configuration file\n"

#define SOCKET_OPEN_ERROR   "Socket open error\n"
#define SOCKET_ACCEPT_ERROR "Socket accept error %d\n"
#define SOCKET_LINGER_ERROR "Socket linger error %d\n"
#define CANNOT_FIND_SOCKET  "Cannot find socket for %s, socket = %d\n"
#define CANNOT_SET_SOCKET   "Cannot set socket for %s, socket = %d\n"

#define CANNOT_GET_MEMORY   "Memory allocation error"
#define CANNOT_SPAWN        "Cannot spawn subprocess %s due to %s"
/*#define EXEC_ERROR  "Error creating new process (exec) status %d errno# %d:\n" */
#define EXEC_ERROR  "%s process creation error: %s"

#define INVALID_SUBSYS_CATEGORY "Invalid subsystem category for %s: %d\n"

#define REQUEST_NOT_READY       "request not ready %s\n"
#define CANNOT_GET_PMF_FILE     "Cannot get PMF file name\n"
#define CANNOT_GET_QC_FILES     "Cannot get leader and image files for QC\n"
#define CANNOT_GET_QC_REQNO     "Cannot get QC request number\n"
#define CANNOT_PERFORM_QC       "Cannot perform QC on %s job %d"

#define UNEXPECTED_VALUE     "Unexpected value %d in %s\n"
#define IS_NOT_REG_FILE      "%s is not a regular file\n"

#define CATEGORY_EMPTY       "No subsystems in this category -- cannot %s\n"
#define POSITION_EMPTY       "No such entry in subsystem list -- cannot %s\n"

#define DIED_UNEXPECTEDLY_TEXT  "%s has shutdown unexpectedly.\nPlease check system log files for more information"

/* #define RCVD_UNEXPECTED_READY  "CP received unexpected SUBSYSTEM_READY message from %s.\nThis subsystem does not exist for the configuration of this instance\nof the CP and therefore must be terminated manually." 
*/
#define RCVD_UNEXPECTED_READY  \
     "CP received unexpected SUBSYSTEM_READY message from %s"
#define CONNECT_ATTEMPT "CP trying to connect to unknown %s subsystem\n"
#define CONNECT_ESTABLISHED  "CP received unsolicited SUBSYSTEM_READY message from %s.\nA connection has been successfully established with this subsystem."
#define RCVD_UNKNOWN_SOURCE "Received message from unknown subsystem: %s"

#define NO_PRODUCT_REQUEST_STATUS_TEXT   " Job in invalid state"

#define RCVD_INVALID_ACK   \
   "Job %d in invalid state (%d) to receive SUBSYSTEM_ACK message from %s"

#define SUBSYSTEM_DID_NOT_STAT_TEXT \
            "%s did not return status message for job %d"
#define SUBSYSTEM_DID_NOT_STAT_LAST_TEXT  \
         "%s did not return status message for last job"

#define SUBSYSTEM_DID_NOT_ACK_TEXT  "%s did not acknowledge receipt of job %d"
#define SUBSYSTEM_DID_NOT_ACK_LAST_TEXT  \
         "%s did not acknowledge receipt of last job"


/* errors we should never see */

#define X_MOTIF_ERROR         "X/Motif error: %s failed\n"
#endif

