C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  create_nominal_orbit.for
C--
C--  Description:   
C-- 
C--  Notes:
C--
C-- ==========================================================================
      INTEGER FUNCTION create_nominal_orbit( p_progname,  dbproc,
     ?                       p_dbname, p_sybase_userid, p_password,
     ?                       p_sat, p_phase )
      CHARACTER *(100) p_progname
      INTEGER        dbproc
      CHARACTER *(100) p_dbname
      CHARACTER *(100) p_sybase_userid
      CHARACTER *(100) p_password
      CHARACTER *(10) p_sat
      CHARACTER *(10) p_phase

      character*100 SccsFileID
     -/'@(#)create_nominal_orbit.for	5.1 98/01/08 APS/ASF\0'/

C **************************************************************************** 
*  NAME: create_nominal_orbit
*  MODULE TYPE:  function 
*  PURPOSE:  to call the nominal orbit and state vector creation function.
*            this routine receives the satellite and phase parameters 
*            and then initializes the vector library for the appropriate 
*            stoic file.  Then, file permissions are checked.  
*            eventually, the real work is done by the function: ephmsvc1
*
*            This routine could and perhaps should be rewritten in C.  
*
*
*  INPUT PARAMETERS:
*  Type        Name               Definition
*  char        p_progname         argv[0] from calling main program.  
*  int         dbproc             sybase dbproc pointer.
*  char        p_dbname           name of sybase database.  
*  char        p_sybase_userid    sybase userid
*  char        p_password         sybase password
*  char        p_sat              satellite
*  char        p_phase            phase.  
*
*  NOTE:  passing character parameters to and from C is not recommended.  
*         we add in some safety when we move the character strings 
*         from p_phase to PHASE_NAME etc.  The 'p_' prefix means parameter.
*
*  SAT              char*2      name of satellite:  E1, J1, E2, RS, etc.  
*  PHASE_NAME       char*1      phase id:  A, B, C, D, etc.  
*  
*  OUTPUT PARAMETERS
*     NONE
*  Modification History:                                            
*  $DATE$      $REVISION$      $AUTHOR$
*********************************************************************/
      IMPLICIT NONE
      CHARACTER*99 PROGNAME
      CHARACTER*2 SAT
      CHARACTER*10 PHASE_NAME
      CHARACTER*150 STOICDIR, CWD
      INTEGER EPHMSVC1, rcode, status, system
      character*100 dbname, sybase_userid, password
      integer rcode, lastc, chdir, getcwd

C-    returned values from ECONST() in vector lib
      DOUBLE PRECISION days, secs, jd 
C-    file_presence_flag == TRUE if present.     */
      LOGICAL file_presence_flag 

      INTEGER econst
      external econst

      external aps_access       !$pragma c(aps_access)
      external f77_aps_fullpath !$pragma c(f77_aps_fullpath)

c---    STATEMENTS TO SUPPORT SYBASE CALLS TO DBLIB
      integer*4 dbproc

      INCLUDE 'APS_HOME:include/local/aps_log_msg.inc'

C--   set up the local variables from the input parameters.  
      PROGNAME = p_progname(1:LASTC(p_progname))
      SAT = p_sat(1:2) 
      PHASE_NAME = p_phase(1:1) 

      IF (SAT .EQ. ' ') GO TO 8001
      if(PHASE_NAME .EQ. ' ') go to  8001
C---    get password; it is required.  
C---    get sybase_userid if provided.  
      sybase_userid = p_sybase_userid(1:LASTC(p_sybase_userid))
      password = p_password(1:LASTC(p_password))
      if(sybase_userid .EQ. ' ') go to 8001
      dbname = p_dbname(1:LASTC(p_dbname))
 
      rcode = system('banner CREATE NOMINAL ORBIT')
      call sleep(2)

C---    Now let us use the sat and phase info to determine the name 
C---    of the stoicfile directory (STOICDIR) that we want to use:
      call tfname('APS_DATA/stoicfiles/' // SAT(1:2) // '_' 
     ?            // PHASE_NAME(1:1) // CHAR(0), STOICDIR)

C---    Now change to that directory:  
      rcode = chdir(STOICDIR(1:LASTC(STOICDIR)))
      if(rcode .ne. 0) then
          print *,'create_nominal_orbit:  '
          print *,'           Stoicfile directory not found for '
          print *,'           this satellite and phase.  '
          print *,'           Possible error in satellite or phase:'
          print *,'           Satellite = ', SAT(1:2)
          print *,'           Phase = ', PHASE_NAME(1:1)
          print *,'Expected stoicfile directory:'
          print *,STOICDIR(1:LASTC(STOICDIR))
          print *,'If the satellite and phase are OK, then'
          print *,'you need to obtain the stoicfile for the start time '
          print *,'of the phase, create the expected stoicfile '
          print *,'directory, and copy the stoicfile to the directory.'
          print *,'It is best to find one with a time bracket starting'
          print *,'just before the start of the phase.'

          CALL APS_LOG_MSG_FORTRAN(PROGNAME,  APS_CRITICAL,
     ?         'Stoicfile directory not found for satellite '// 
     ?         SAT(1:2) // ' phase ' // PHASE_NAME(1:1) // '.', 
     ?         DO_SYSLOG, DO_PRINT )

          go to 8000
      endif
C---    Now check for the existence of this stoicfile:
      call aps_access('stoicfile' // CHAR(0),'r',rcode)
      if(rcode .ne. 0) then

          CALL APS_LOG_MSG_FORTRAN(PROGNAME,  APS_CRITICAL,
     ?         'Stoicfile not readable in expected directory for '//
     ?         SAT(1:2) // ' phase ' // PHASE_NAME(1:1) // '.',
     ?         DO_SYSLOG, DO_PRINT )
 
          print *,'create_nominal_orbit:  '
          print *,'      Error;  stoicfile not readable in expected '
          print *,'      directory for satellite and phase.'
          print *,'      Satellite = ', SAT(1:2)
          print *,'      Phase = ', PHASE_NAME(1:1)
          rcode = getcwd(CWD)
          if(rcode .ne. 0) go to 8000
          print *,'Expected stoicfile directory: '
          print *, CWD(1:LASTC(CWD))
          print *,'Check for existence of stoicfile and readability.'      

          CALL APS_LOG_MSG_FORTRAN(PROGNAME,  APS_CRITICAL,
     ?         'Expected stoicfile directory is '//
     ?         CWD(1:LASTC(CWD)) // '.', DO_SYSLOG, DO_PRINT )
 
          go to 8000
      endif

      print *,'using STOICFILE in ', STOICDIR(1:LASTC(STOICDIR))


C-
C- run a vectory library routine to get the vector library to
C- initialize   itself.
C- this is to prevent the WARNING messages.
C- note:  in change_to_stoicdir, we check the file presence already.
C-
      rcode = econst(days, secs, jd, file_presence_flag ) 

      status = ephmsvc1(PROGNAME, dbproc, dbname, sybase_userid, 
     ?                  password, SAT, PHASE_NAME) 
      IF(status .EQ. 0) GO TO 9000

 8000 continue
c---    the function was not completed normally.  
      print *, ' '
      print *, 'create_nominal_orbit:  NOMINAL ORBIT AND ',
     ?                          'STATE VECTOR NOT CREATED.'
      create_nominal_orbit = APS_EXIT_ERROR
      return

 8001 continue
c---    usage error.  
      CALL APS_LOG_MSG_FORTRAN(PROGNAME,  APS_ERROR,
     ?   'Error in usage.', DO_SYSLOG, DO_PRINT )
      print *, 'usage:  create_nominal_orbit [-U sybase_userid] ', 
     ?                     '-P password sat phase'
      create_nominal_orbit = APS_EXIT_ERROR
      return

 9000 CONTINUE
      print *,'create_nominal_orbit:   Nominal orbit and ',
     ?                          'state vector were created.'        
      create_nominal_orbit = APS_EXIT_OK
      return 

      END
