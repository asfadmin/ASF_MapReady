/*==============================================================================
Filename:	semops.h

Description:
	Header file for semops module.  This file defines the semaphore
keys to be used for each of the semaphores used by FAIF.  It also includes
the structure definition for the semaphore table entry.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _SEMDEFS_
#define _SEMDEFS_

#define CSA_PORB_SEM      0   /* Pred Orbit semaphore */
#define CSA_DORB_SEM      1   /* Defv Orbit semaphore  */
#define CSA_RRQ_SEM       2   /* Recpt rqst (RR) semaphore */
#define CSA_RSH_SEM       3   /* Recpt sched (RS) semaphore */
#define CSA_CRQ_SEM       4   /* Cal rqst (CR) semaphore */
#define CSA_CSH_SEM       5   /* Cal sched (CS) semaphore */
#define CSA_SARPP_SEM     6   /* Proc params semaphore */
#define CSA_RRQ_M_SEM     7   /* McMdo RR semaphore */
#define CSA_RSH_M_SEM     8   /* McMdo RS semaphore */
#define CSA_CRQ_M_SEM     9   /* McMdo CR semaphore */
#define CSA_CSH_M_SEM    10   /* McMdo CS semaphore */
#define ESA_DIRMON_SEM   11   /* ESA dirmon semaphore */ 
#define NASDA_DIRMON_SEM 12   /* NASDA dirmon semaphore */ 
#define WALPS_DIRMON_SEM 13   /* Wallops dirmon semaphore */ 
#define SEND_ALL_SEM     15   /* Generic send program semaphore */
#define ADEOS_DIRMON_SEM 16   /* ADEOS dirmon semaphore */ 


#define SEMKEYBASE (0x4253484d)     /* Random base semkey */

/*
-- Semaphore keys
*/
#define CSA_PORB_KEY      (SEMKEYBASE + CSA_PORB_SEM)
#define CSA_DORB_KEY      (SEMKEYBASE + CSA_DORB_SEM)
#define CSA_RRQ_KEY       (SEMKEYBASE + CSA_RRQ_SEM)
#define CSA_RSH_KEY       (SEMKEYBASE + CSA_RSH_SEM)
#define CSA_CRQ_KEY       (SEMKEYBASE + CSA_CRQ_SEM)
#define CSA_CSH_KEY       (SEMKEYBASE + CSA_CSH_SEM)
#define CSA_SARPP_KEY     (SEMKEYBASE + CSA_SARPP_SEM)
#define CSA_RRQ_M_KEY     (SEMKEYBASE + CSA_RRQ_M_SEM) 
#define CSA_RSH_M_KEY     (SEMKEYBASE + CSA_RSH_M_SEM)
#define CSA_CRQ_M_KEY     (SEMKEYBASE + CSA_CRQ_M_SEM)
#define CSA_CSH_M_KEY     (SEMKEYBASE + CSA_CSH_M_SEM)
#define ESA_DIRMON_KEY    (SEMKEYBASE + ESA_DIRMON_SEM) 
#define NASDA_DIRMON_KEY  (SEMKEYBASE + NASDA_DIRMON_SEM) 
#define WALPS_DIRMON_KEY  (SEMKEYBASE + WALPS_DIRMON_SEM) 
#define SEND_ALL_KEY      (SEMKEYBASE + SEND_ALL_SEM) 
#define ADEOS_DIRMON_KEY  (SEMKEYBASE + ADEOS_DIRMON_SEM) 

#define SEMKEYCEIL (ADEOS_DIRMON_KEY)           /* highest semkey */
#define NUM_OF_SEMS (SEMKEYCEIL-SEMKEYBASE+1)   /* Num of sems */

#endif /* _SEMDEFS_ */

/* End of File */
