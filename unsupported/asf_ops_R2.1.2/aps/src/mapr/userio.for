C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  userio.for
C--
C--  Description:   
C-- 
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C USERIO.QF
C
C   MAPPER I/O ROUTINES
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]USERIO.FOV  $
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 10/4/94  Nadia Adhami  Display segment index as a 4-digit number
C 4/24/95  Nadia Adhami ASK_SENSOR(SEN) -> (SAT,SEN)
C
C-----------------------------------------------------------------

C-----------------------------------------------------------------
C SUBROUTINE TOPMNU
C
C PURPOSE
C   DISPLAY THE INITIAL MENU FOR THE USER
C   PROMPT USER FOR DISPLAY TYPE
C
C INPUT
C OUTPUT
C   DISP        DISPLAY FLAG
C           1=WORLD MAP FILE / 2=GKS METAFILE
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE TOPMNU (DISP)

      character*100 SccsFileID
     -/'@(#)userio.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

      INTEGER DISP,IOS

 1000 CONTINUE

      WRITE (6,100)
      WRITE (6,101)
      WRITE (6,102)
      WRITE (6,103)
      WRITE (6,105) 

  100 FORMAT (/,' MAPPER')
  101 FORMAT (' 1) WORLD MAP')
  102 FORMAT (' 2) METAFILE MAP')
  103 FORMAT (' 0) QUIT')
  105 FORMAT (/,' Select Map : ',$)

      READ (5,*,IOSTAT = IOS) DISP   

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (DISP .LT. 0 .OR. DISP .GT. 2) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000                          
      END IF
      
 9999 CONTINUE
      RETURN                                  
      END


C-----------------------------------------------------------------
C SUBROUTINE FUNMNU
C
C PURPOSE
C   DISPLAY THE FUNCTION MENU AND PROMPT FOR A FUNCTION
C
C INPUT
C   PROJN       PROJECTION NUMBER
C
C OUTPUT
C   ANSW        ANSWER
C
C INTERNAL
C   YN      YES/NO
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE FUNMNU(PROJN,ANSW)

      IMPLICIT NONE

      INTEGER ANSW,IOS
      INTEGER PROJN
      INTEGER   LOCAL_DUMMY_VALUEHOLDER

      INCLUDE 'mapper_port.inc'
      INCLUDE 'mapr_db_extern.inc'

C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLES
      INTEGER*4 TEMP
      DATA TEMP /10/
       LLISTPTR =  LOC (TEMP) 
       LOCAL_DUMMY_VALUEHOLDER = LLIST
       PTRPTR = LOC (TEMP)
       LOCAL_DUMMY_VALUEHOLDER = PTR
       P2_DATA_RECORD = LOC (TEMP)
       DATA WHERE_CLAUSE /'ABC'/

C   INPUT A MAPPER FUNCTION
                                      
 1000 CONTINUE

      WRITE (6,100)
  100 FORMAT (/,' FUNCTIONS:')


C FOR SATELLITE VIEW - NO ZOOM

      IF (PROJN .EQ. 1) THEN

        WRITE (6,101)
        WRITE (6,102)
        WRITE (6,103)
        WRITE (6,104)
        WRITE (6,105)
        WRITE (6,199)
  101   FORMAT (' 1) CREATE OVERLAYS')
  102   FORMAT (' 2) MANAGE OVERLAYS')
  103   FORMAT (' 3) SAVE MAP')
  104   FORMAT (' 4) PLOT MAP')
  105   FORMAT (' 0) QUIT')
  199   FORMAT ('99) REFRESH DISPLAY')
      
        WRITE (6,110)
  110   FORMAT (/,' Function : ',$)

        READ (5,*,IOSTAT = IOS) ANSW

        IF (ANSW .EQ. 99) THEN
          GOTO 9999
        END IF

        IF (IOS .NE. 0 .OR. ANSW .LT. 0 .OR. ANSW .GT. 4) THEN
          CALL DISMSG('Error: Invalid entry.')
          GOTO 1000
        ENDIF

        IF (TESTMODE .GT. 0) THEN
          CALL SLEEP(TESTMODE)
        ENDIF

C INCREMENT ANSW TO ACCOUNT FOR MISSING FUNCTION
                       
        IF (ANSW .NE. 0) THEN
          ANSW = ANSW + 1
        END IF

C FOR ALL OTHER PROJECTIONS

      ELSE 

        WRITE (6,201)
  201   FORMAT (' 1) SET WINDOW')
        WRITE (6,202)
  202   FORMAT (' 2) CREATE OVERLAYS')
        WRITE (6,203)
  203   FORMAT (' 3) MANAGE OVERLAYS')
        WRITE (6,204)
  204   FORMAT (' 4) SAVE MAP')
        WRITE (6,205)
  205   FORMAT (' 5) PLOT MAP')
        WRITE (6,206)
  206   FORMAT (' 6) PRINT MAP')
        WRITE (6,209)
  209   FORMAT (' 9) EXIT MAPPER')
        WRITE (6,200)
  200   FORMAT (' 0) QUIT')
        WRITE (6,299)
  299   FORMAT ('99) REFRESH DISPLAY')

        WRITE (6,210)
  210   FORMAT (/,' Function : ',$)

        READ (5,*,IOSTAT = IOS) ANSW

        IF (TESTMODE .GT. 0) THEN
            CALL SLEEP(TESTMODE)
        ENDIF

        IF (ANSW .EQ. 99) THEN
          GOTO 9999
        END IF

        IF (IOS .NE. 0 ) THEN
          CALL DISMSG('Error: Invalid entry.')
          GOTO 1000
        END IF
           
C---    valid entries are 0-6 and also 9.  
        IF (ANSW .NE. 9) THEN
          IF (ANSW .LT. 0 .OR. ANSW .GT. 6) THEN
            CALL DISMSG('Error: Invalid entry.')
            GOTO 1000
          END IF
        END IF

      END IF

 9999 CONTINUE
      RETURN                                  
      END



C-----------------------------------------------------------------
C SUBROUTINE ASK_WRESC
C
C PURPOSE
C   ALLOW THE USER TO SELECT THE MAP RESOLUTION
C
C INPUT
C OUTPUT
C   RES     MAP RESOLUTION
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_WRES (RES)

      IMPLICIT NONE

      INTEGER RES

      INTEGER IOS

 1000 CONTINUE

      WRITE (6,100)
      WRITE (6,102)
      WRITE (6,103)
C      WRITE (6,104)
      WRITE (6,106)
  100 FORMAT (/,' RESOLUTION:')
  102 FORMAT (' 1) LOW ')
  103 FORMAT (' 2) MEDIUM ')
  104 FORMAT (' 3) HIGH ')
  106 FORMAT (/,' Resolution : ',$)
                      
      READ (5,*,IOSTAT = IOS) RES
                            
      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (RES .LT. 1 .OR. RES .GT. 2) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_WPROJ
C
C PURPOSE
C   ALLOW THE USER TO SELECT THE PROJECTION TO BE DISPLAYED
C   FOR POLAR STEREO, ASKS WHICH POLE ALSO
C
C INPUT
C OUTPUT
C   PROJN       PROJECTION NUMBER
C
C INTERNAL
C   IOS     I/O STATUS
C
C CALLS
C   ASK_WPOLAR
C-----------------------------------------------------------------
      SUBROUTINE ASK_WPROJ(PROJN) 

      IMPLICIT NONE

      INTEGER PROJN,IOS

 1000 CONTINUE

      WRITE (6,100)
      WRITE (6,102)
      WRITE (6,103)
      WRITE (6,104)
      WRITE (6,105)
      WRITE (6,106)
      WRITE (6,107)
  100 FORMAT (/,' PROJECTIONS:')
  102 FORMAT (' 1) GENERAL PERSPECTIVE')
  103 FORMAT (' 2) CYLINDRICAL EQUIDISTANT')
  104 FORMAT (' 3) MERCATOR')
  105 FORMAT (' 4) MILLER CYLINDRICAL')
  106 FORMAT (' 5) POLAR STEREOGRAPHIC')
  107 FORMAT (/,' Projection : ',$)
                                      
      READ (5,*,IOSTAT = IOS) PROJN
      
      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (PROJN .LT. 1 .OR. PROJN .GT. 5) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

C PROMPT FOR THE NORTH (PROJN=5) / SOUTH (PROJN=6) HEMISPHERE
      IF (PROJN .EQ. 5) THEN
        CALL ASK_WPOLAR(PROJN)
      END IF

 9999 CONTINUE
      RETURN
      END        

C-----------------------------------------------------------------
C SUBROUTINE SPC_MENU1
C
C PURPOSE
C   ALLOW USER TO SELECT SSCVG BY DARID OR SITENAME
C
C INPUT
C OUTPUT
C   OPT     OPTION FLAG
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE SPC_MENU1(OPT) 

      IMPLICIT NONE

      INTEGER OPT,IOS

 1000 CONTINUE

      WRITE (6,100)
      WRITE (6,102)
      WRITE (6,103)
      WRITE (6,104)
      WRITE (6,106)
  100 FORMAT (/,' SELECT SPECIFIC SITE CVRG BY :')
  102 FORMAT (' 1) DARID')
  103 FORMAT (' 2) SITENAME')
  104 FORMAT (' 0) QUIT')
  106 FORMAT (/,' Select by : ',$)
      
      READ (5,*,IOSTAT = IOS) OPT
                             
      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (OPT .LT. 0 .OR. OPT .GT. 2) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------
C SUBROUTINE SPC_MENU2
C
C PURPOSE
C   ALLOW USER TO SELECT HOW SSCV WILL BE DISPLAYED
C
C INPUT/OUTPUT
C   OPT     RETURN FLAG
C INTERNAL
C   IOS     I/O FLAG
C-----------------------------------------------------------------
      SUBROUTINE SPC_MENU2(OPT)

      IMPLICIT NONE

      INTEGER OPT,IOS

 1000 CONTINUE

      WRITE (6,101)
      WRITE (6,103)
      WRITE (6,104)
      WRITE (6,105)
      WRITE (6,109)
  101 FORMAT (/,' DISPLAY BY:')
  103 FORMAT (' 1) TIME')
  104 FORMAT (' 2) REV')
  105 FORMAT (' 0) QUIT')
  109 FORMAT (/,' Display by: ',$)

      READ (5,*,IOSTAT = IOS) OPT

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (OPT .LT. 0 .OR. OPT .GT. 2) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF
                           
 9999 CONTINUE
      RETURN
      END

                 
C-----------------------------------------------------------------
C SUBROUTINE SEGMNU
C
C PURPOSE
C   ALLOW USER TO SELECT HOW SEGS WILL BE DISPLAYED
C
C INPUT/OUTPUT
C   OPT     FLAG
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE SEGMNU(OPT)

      IMPLICIT NONE

      INTEGER OPT,IOS

 1000 CONTINUE

      WRITE (6,101)
      WRITE (6,103)
      WRITE (6,104)
      WRITE (6,105)
      WRITE (6,106)
      WRITE (6,107)
      WRITE (6,108)
      WRITE (6,109)
  101 FORMAT (/,' DISPLAY BY:')
  103 FORMAT (' 1) SEG ID')
  104 FORMAT (' 2) DAR ID')
  105 FORMAT (' 3) DTK ID')
  106 FORMAT (' 4) TIME')
  107 FORMAT (' 5) REV')
  108 FORMAT (' 0) QUIT')
  109 FORMAT (/,' Display by: ',$)

      READ (5,*,IOSTAT = IOS) OPT

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (OPT .LT. 0 .OR. OPT .GT. 5) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE DTKMNU
C
C PURPOSE
C   ALLOW USER TO SELECT HOW DTKS WILL BE DISPLAYED
C
C INPUT/OUTPUT
C   OPT     USER SELECTION
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE DTKMNU(OPT)

      IMPLICIT NONE

      INTEGER OPT,IOS

 1000 CONTINUE

      WRITE (6,101)
      WRITE (6,103)
      WRITE (6,104)
      WRITE (6,105)
      WRITE (6,106)
      WRITE (6,109)
  101 FORMAT (/,' DISPLAY BY:')
  103 FORMAT (' 1) DTK ID')
  104 FORMAT (' 2) TIME')
  105 FORMAT (' 3) REV')
  106 FORMAT (' 0) QUIT')
  109 FORMAT (/,' Display by: ',$)

      READ (5,*,IOSTAT = IOS) OPT
      
      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (OPT .LT. 0 .OR. OPT .GT. 3) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE   
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE CVGMNU
C
C PURPOSE
C   ALLOW USER TO SLEECT HOW CVRG WILL BE DISPLAYED
C
C INPUT/OUTPUT
C   OPT     RETURN FLAG
C INTERNAL
C   IOS     I/O FLAG
C-----------------------------------------------------------------
      SUBROUTINE CVGMNU(OPT)

      IMPLICIT NONE

      INTEGER OPT,IOS

 1000 CONTINUE

      WRITE (6,101)
      WRITE (6,103)
      WRITE (6,104)
      WRITE (6,105)
      WRITE (6,109)
  101 FORMAT (/,' DISPLAY BY:')
  103 FORMAT (' 1) TIME')
  104 FORMAT (' 2) REV')
  105 FORMAT (' 0) QUIT')
  109 FORMAT (/,' Display by: ',$)

      READ (5,*,IOSTAT = IOS) OPT

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (OPT .LT. 0 .OR. OPT .GT. 2) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF
                           
 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE CVGGTMNU
C
C PURPOSE
C   ASK USER WHAT SHOULD BE DISPLAYED: GT, SWATH, OR BOTH
C
C INPUT/OUTPUT
C   GTOPT       OPTION FLAG
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE CVGGTMNU(GTOPT)

      IMPLICIT NONE

      INTEGER GTOPT,IOS

 1000 CONTINUE

      WRITE (6,100)
      WRITE (6,101)
      WRITE (6,102)
      WRITE (6,103)
      WRITE (6,105)
  100 FORMAT (/,' DISPLAY:')
  101 FORMAT (' 1) GROUND TRACK')
  102 FORMAT (' 2) SENSOR SWATH')
  103 FORMAT (' 3) BOTH')
  105 FORMAT (/,' Display: ',$)

      READ (5,*,IOSTAT = IOS) GTOPT

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000         
      ELSE IF (GTOPT .LT. 0 .OR. GTOPT .GT. 3) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE CVGTXMNU
C
C PURPOSE
C   ASK USER WHAT TEXT SHOULD BE DISPLAYED ALONG CVRG
C
C INPUT/OUTPUT
C   TXOPT       OPTION FLAG
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE CVGTXMNU(TXOPT)

      IMPLICIT NONE

      INTEGER TXOPT,IOS

 1000 CONTINUE

      WRITE (6,100)
      WRITE (6,101)
      WRITE (6,102)
      WRITE (6,103)
      WRITE (6,104)
      WRITE (6,105)
  100 FORMAT (/,' DISPLAY TEXT:')
  101 FORMAT (' 1) SAT/SEN/REV')
  102 FORMAT (' 2) TIME')
  103 FORMAT (' 3) BOTH')
  104 FORMAT (' 4) NO TEXT')
  105 FORMAT (/,' Display text: ',$)

      READ (5,*,IOSTAT = IOS) TXOPT

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (TXOPT .LT. 0 .OR. TXOPT .GT. 4) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_WLON
C
C PURPOSE
C   ASK USER FOR LONGITUDE NUMBERING SCHEME
C
C INPUT/OUTPUT
C   LONNUM      OPTION FLAG 1=0TO360 2=-180TO180
C INTERNAL
C   IOS     I/O FLAG
C-----------------------------------------------------------------
      SUBROUTINE ASK_WLON(LONNUM)

      IMPLICIT NONE

      INTEGER LONNUM,IOS

 1000 CONTINUE

      WRITE (6,100)
      WRITE (6,101)
      WRITE (6,102)
  100 FORMAT (/,' 1) 0 to 360 ')
  101 FORMAT (' 2) -180 to 180 ')
  102 FORMAT (/,' LONGITUDE NUMBERING : ',$)

      READ (5,*,IOSTAT = IOS) LONNUM

      IF (IOS.NE.0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (LONNUM.LT.1 .OR. LONNUM.GT.2) THEN
        CALL DISMSG('Error: Invalid input.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END

                 
C-----------------------------------------------------------------
C SUBROUTINE ASK_WGRID
C
C PURPOSE
C   ASK IF USER WANTS GRID LINES DISPLAYED OR NOT
C
C INPUT/OUTPUT
C   GRIDLN      GRID LINE FLAG 1=ON 0=OFF
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_WGRID(GRIDLN)

      IMPLICIT NONE

      INTEGER GRIDLN,IOS

 1000 CONTINUE

      WRITE (6,150)
  150 FORMAT (/,' DISPLAY GRID LINES  [ 0 = off, 1 = on ] : ',$)

      READ (5,*,IOSTAT = IOS) GRIDLN

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (GRIDLN.LT.0 .OR. GRIDLN.GT.1) THEN
        CALL DISMSG('Error: Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_WPOLAR
C
C PURPOSE
C   ASK FOR NORTH OR SOUTH POLE
C
C INPUT/OUTPUT
C   PROJN       PROJECTION NUMBER
C
C INTERNAL
C   IOS     I/O STATUS
C   ANS     ANSWER
C-----------------------------------------------------------------
      SUBROUTINE ASK_WPOLAR(PROJN)

      IMPLICIT NONE

      INTEGER PROJN
      INTEGER IOS,ANS

 1000 CONTINUE
      
      WRITE (6,100)
      WRITE (6,101)
      WRITE (6,102)
      WRITE (6,103)
  100 FORMAT (/,1X,'HEMISPHERE:')
  101 FORMAT (1X,' 1) North')
  102 FORMAT (1X,' 2) South')
  103 FORMAT (1X,'Hemisphere: ',$)

      READ(5,110,IOSTAT=IOS) ANS
  110 FORMAT (I1)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (ANS .GT. 2 .OR. ANS .LT. 1) THEN
        CALL DISMSG('Error : Invalid input.')
        GO TO 1000
      END IF

      IF (ANS .EQ. 2) THEN
        PROJN = PROJN + 1
      END IF

 9999 CONTINUE
      RETURN
      END

                 
C-----------------------------------------------------------------
C SUBROUTINE ASK_WNSIZE
C
C PURPOSE
C   ASK THE USER FOR THE WINDOW SIZE
C
C INPUT/OUTPUT
C   WNSIZE      SIZE OF WINDOW
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_WNSIZE(WNSIZE)

      IMPLICIT NONE 

      INTEGER IOS
      REAL WNSIZE

 1000 CONTINUE

      WRITE (6,100)
  100 FORMAT (/,' WINDOW SIZE %  [ 1 to 100 ] :  ',$)

      READ (5,*,IOSTAT = IOS) WNSIZE

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (WNSIZE.LT.1.0 .OR. WNSIZE.GT.100.0) THEN
        CALL DISMSG('Error: Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END

               
C-----------------------------------------------------------------
C SUBROUTINE ASK_MFNAME
C
C INPUT:
C   DIR     DIRECTION 0 - INPUT / 1 - OUTPUT
C
C OUTPUT:
C   MWFIL       METAFILE FILENAME
C   MWHDR       METAFILE HEADER FILENAME
C   STATUS      RETURN STATUS
C
C INTERNAL:
C   IOS     I/O STATUS
C   I       COUNTER
C   FN      STRING COUNTER
C-----------------------------------------------------------------
      SUBROUTINE ASK_MFNAME (MWFIL,MWHDR,DIR,STAT)

      IMPLICIT NONE

C INPUT:
      INTEGER DIR
C OUTPUT:
      CHARACTER*(*) MWFIL,MWHDR
      INTEGER STAT
C INTERNAL:
      INTEGER IOS,FN
C FUNCTION
      integer SLEN

 1000 CONTINUE
              
      WRITE (6,100)
  100 FORMAT (/,' Map Window Filename (no extension) : ',$)

      READ (5,110,IOSTAT = IOS) MWFIL
  110 FORMAT (A)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000 
      END IF
         
C DETERMINE THE STRING LENGTH
      FN = SLEN(MWFIL)
      IF (FN .EQ. 0) THEN
        STAT = -1
        GO TO 9999
      END IF

C FORM METAFILE FILENAME

      MWFIL = MWFIL(1:FN)//'.GKS'

C FORM METAFILE HEADER FILENAME

      MWHDR = MWFIL(1:FN)//'.HDR'

C IF FOR READING, CHECK FOR EXISTENCE

      IF (DIR .EQ. 0) THEN

        CALL EXFNAM(MWFIL,IOS)

        IF (IOS .NE. 0) THEN
          CALL DISMSG('Error : Map Window File not found.')
          GO TO 1000
        END IF

        CALL EXFNAM(MWHDR,IOS)

        IF (IOS .NE. 0) THEN
          CALL DISMSG('Error : Map Window Header File not found.')
          GO TO 1000
        END IF

C IF FOR WRITING CHECK TO SEE IF YOU CAN OPEN THE FILE

      ELSE 

        CALL CHFNAM(MWFIL,IOS)
        IF (IOS .NE. 0) THEN
          CALL DISMSG('Error : Could not open Map Window File.')
          GO TO 1000
        END IF

        CALL CHFNAM(MWHDR,IOS)
        IF (IOS .NE. 0) THEN
          CALL DISMSG('Error : Could not open Map Window Header File.')
          GO TO 1000
        END IF

      END IF

      STAT = 0

 9999 CONTINUE
      RETURN
      END



C-----------------------------------------------------------------
C SUBROUTINE OVERMNU
C
C PURPOSE
C   DISPLAY OVERLAY MENU
C   ASK USER FOR SELECTION
C
C INPUT/OUTPUT
C   OLN     OVERLAY OPTION
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE OVERMNU(OLN)

      INCLUDE 'mapper_port.inc'

      IMPLICIT NONE

      INTEGER OLN,IOS

 1000 CONTINUE
      WRITE (6,100)
      WRITE (6,102)
      WRITE (6,103)
      WRITE (6,104)
      WRITE (6,105)
      WRITE (6,106)
      WRITE (6,107)
      WRITE (6,108)
      WRITE (6,199)
      WRITE (6,109)
  100 FORMAT (/,' CREATE OVERLAYS')
  102 FORMAT (' 1) SITES')
  103 FORMAT (' 2) DAR')
  104 FORMAT (' 3) SEG')
  105 FORMAT (' 4) DTK')
  106 FORMAT (' 5) COVERAGE')
  107 FORMAT (' 6) SPECIFIC SITE COVERAGE')
  108 FORMAT (' 0) QUIT')
  199 FORMAT ('99) REFRESH DISPLAY')
  109 FORMAT (/,' Overlay :',$)

      READ (5,*,IOSTAT = IOS) OLN

      IF (TESTMODE .GT. 0) THEN
          CALL SLEEP(TESTMODE)
      ENDIF

      IF (OLN .EQ. 99) THEN 
        GO TO 9999
      ENDIF

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (OLN .LT. 0 .OR. OLN .GT. 6) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF              
      
 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE MNGMNU
C
C PURPOSE
C   LIST THE MANAGE OVERLAYS MENU
C   ASK THE USER FOR AN OPTION
C
C OUTPUT
C   MGN     OPTION NUMBER
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE MNGMNU(MGN)

      IMPLICIT NONE

      INTEGER MGN,IOS

 1000  CONTINUE

       WRITE (6,101)
       WRITE (6,102)
       WRITE (6,103)
       WRITE (6,104)
       WRITE (6,105)
       WRITE (6,106)
       WRITE (6,107)
       WRITE (6,108)
       WRITE (6,109)
       WRITE (6,110)
  101  FORMAT (/,' MANAGE OVERLAYS')
  102  FORMAT (' 1) REMOVE')
  103  FORMAT (' 2) REPLACE')
  104  FORMAT (' 3) DELETE')
  105  FORMAT (' 4) REMOVE ALL')
  106  FORMAT (' 5) REFRESH DISPLAY')
  107  FORMAT (' 6) CREATE SEG FILE')
  108  FORMAT (' 7) CREATE SEG FILE AND LOAD INTO DATABASE')
  109  FORMAT (' 0) QUIT')
  110  FORMAT (/,' Selection : ',$)
                        
       READ (5,*,IOSTAT = IOS) MGN

       IF (IOS .NE. 0) THEN 
         CALL DISMSG('Error on input.')
         GO TO 1000
       ELSE IF (MGN .LT. 0 .OR. MGN .GT. 7) THEN
         CALL DISMSG('Error : Invalid entry.')
         GO TO 1000
       END IF

 9999 CONTINUE
      RETURN        
      END
                      

C-----------------------------------------------------------------
C SUBROUTINE ASK_COMM
C
C PURPOSE
C   ASK THE USER FOR A COMMENTS STRING FOR THE HEADER FILE
C
C OUTPUT
C   COMMNT      COMMENT STRING
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_COMM(COMMNT)

      IMPLICIT NONE

      CHARACTER*(*) COMMNT
      INTEGER IOS
                 
 1000 CONTINUE

      WRITE (6,100)
  100 FORMAT (/,' Comments : ',$)

      READ (5,110,IOSTAT = IOS) COMMNT
  110 FORMAT (A76)

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_SITENAME
C
C PURPOSE
C   ASK THE USER FOR A SITENAME
C
C OUTPUT
C   SITENAME    <
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_SITENAME(SITENAME)
      IMPLICIT NONE

      CHARACTER*(*) SITENAME
      INTEGER IOS

 1000 CONTINUE

      WRITE (6,100)
  100 FORMAT (/,' Site name : ',$)

      READ (5,110,IOSTAT = IOS) SITENAME
  110 FORMAT (A32)

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_SAT
C
C PURPOSE
C   PROMPT THE USER FOR A SAT IDENTIFIER
C
C OUTPUT
C   SAT     SATELLITE TWO LETTER IDENTIFIER (E1/J1/SS/RS)
C
C INTERNAL
C   IOS     I/O STATUS
C   SATNUM      OPTION NUMBER
C-----------------------------------------------------------------
      SUBROUTINE ASK_SAT(SAT)
      IMPLICIT NONE

C OUTPUT
      CHARACTER*(*) SAT

C INTERNAL
      CHARACTER*3  SATELLITE
      INTEGER STATUS, COUNT

      EXTERNAL ask_sat_c  !$pragma c(ask_sat_c)
      INTEGER  ask_sat_c
C-----------------------------------------------------------------

 1000 CONTINUE

      STATUS = ask_sat_c( SATELLITE, COUNT ) 

      IF (COUNT .EQ. 0) GOTO 9999

C--     SATELLITES WERE FOUND AND PRESENTED.   CHECK for TRUE USER RESPONSE
C--     IF NOT TRUE RESPONSE, PRESENT CHOICES AGAIN.  
      IF ( STATUS .NE. 1 ) GO TO 1000

C--     RESPONSE WAS TRUE; USE IT.  
      SAT(1:2) = SATELLITE(1:2)

 9999 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------
C SUBROUTINE ASK_SENSOR
C
C PURPOSE
C   PROMPT THE USER FOR SENSOR IDENTIFIER
C
C OUTPUT
C   SEN     SENSOR 3-LETTER IDENTIFIER (SAR/OPS)
C
C INTERNAL
C   IOS     I/O STATUS
C   SENNUM      OPTION NUMBER
C-----------------------------------------------------------------
      SUBROUTINE ASK_SENSOR(SAT , SEN)
      IMPLICIT NONE

C OUTPUT
      CHARACTER*(*) SAT , SEN

C INTERNAL
      CHARACTER*4 SENSOR

      EXTERNAL ask_sensor_c  !$pragma c(ask_sensor_c)
      INTEGER  ask_sensor_c
      INTEGER  STATUS, COUNT
 
C-----------------------------------------------------------------
 1000 CONTINUE

      STATUS = ask_sensor_c( SAT // char (0), SENSOR, COUNT ) 

      IF (COUNT .EQ. 0) GOTO 9999

C--     SENSORS WERE FOUND AND PRESENTED.   CHECK for TRUE USER RESPONSE
C--     IF NOT TRUE RESPONSE, PRESENT CHOICES AGAIN.  
      IF ( STATUS .NE. 1 ) GO TO 1000

C--     RESPONSE WAS TRUE; USE IT.  
      SEN(1:3) = SENSOR(1:3)

 9999 CONTINUE
      RETURN
      END
   

C-----------------------------------------------------------------
C SUBROUTINE ASK_REV
C
C PURPOSE
C   ASK THE USER FOR A REV NUMBER
C
C INPUT
C   PROM        PROMPT
C
C OUTPUT
C   REV     ORBIT REV NUMBER
C
C INTERNAL
C   IOS     I/O STATUS
C
C FUNCTION 
C   SLEN        RETURNS STRING LENGTH
C-----------------------------------------------------------------
      SUBROUTINE ASK_REV(REV,PROM)
      IMPLICIT NONE

      INTEGER REV
      CHARACTER*(*) PROM

      INTEGER IOS
      INTEGER SLEN

C FUNCTION

C-----------------------------------------------------------------
 1000 CONTINUE

      WRITE (6,100) PROM
  100 FORMAT (/,A<SLEN(PROM)>,$)

      READ (5,110,IOSTAT = IOS) REV
  110 FORMAT (I5)

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (REV .LT. 1 .OR. REV .GT. 99999) THEN
        CALL DISMSG('Error : Invalid Rev.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_ID
C
C PURPOSE
C   ASK THE USER FOR A DTKID NUMBER
C
C OUTPUT
C   ID      DATATAKE ID NUMBER
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_ID(ID)
      IMPLICIT NONE

      INTEGER ID,IOS

 1000 CONTINUE

      WRITE (6,100)
  100 FORMAT (/,' ID : ',$)
     
      READ (5,110,IOSTAT = IOS) ID
  110 FORMAT (I2)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (ID .LT. 1 .OR. ID .GT. 99) THEN
        CALL DISMSG('Error : Invalid DTK ID.')
        GO TO 1000
      END IF

  999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_NREV
C
C PURPOSE
C   ASK THE USER FOR A NUMBER OF ORBITS
C
C OUTPUT
C   NREV        NUMBER OF REVS 1-99
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_NREV(NREV)
      IMPLICIT NONE

      INTEGER NREV,IOS

 1000 CONTINUE

      WRITE (6,100)
  100 FORMAT (/,' Number of orbits to display [1-99] : ',$)
     
      READ (5,110,IOSTAT = IOS) NREV
  110 FORMAT (I2)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (NREV .LT. 1 .OR. NREV .GT. 99) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF
                 
 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE DARID
C
C PURPOSE
C   ASK THE USER FOR A DAR IDENTIFIER
C
C OUTPUT
C   DARID       DAR IDENTIFIER 1-9999999999
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_DARID(DARID)
      IMPLICIT NONE

      INTEGER IOS,DARID
      INTEGER DB_SYBINT_USE_APS_READER_DBPROC
      INTEGER NRECS

      INTEGER LASTC

      INCLUDE 'mapr_db_extern.inc'
C FOLLOWING ASSIGNMENTS REMOVE WARNING MESSAGE FOR UNUSED VARIABLES
      INTEGER*4 TEMP
      INTEGER*4 LOCAL_DUMMY_VALUEHOLDER 
      DATA TEMP /10/
         LLISTPTR =  LOC (TEMP) 
         LOCAL_DUMMY_VALUEHOLDER = LLIST
         PTRPTR = LOC (TEMP)
         LOCAL_DUMMY_VALUEHOLDER = PTR
         P2_DATA_RECORD = LOC (TEMP)
      DATA WHERE_CLAUSE /'ABC'/

      DB_SYBINT_USE_APS_READER_DBPROC = 1

 1000 CONTINUE
      WRITE (6,100)
  100 FORMAT (/,' DARID : ',$)

      READ (5,110,IOSTAT = IOS) DARID
  110 FORMAT (I10)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (DARID .LT. 0 .OR. DARID .GT. 999999999) THEN
        CALL DISMSG('Error : Invalid DAR ID.')
        GO TO 1000
      ELSE IF (DARID .EQ. 0 ) THEN
        CALL DISMSG('No DAR ID selected.')
        GO TO 9999
      END IF

C  VALIDATE THE DAR ID AGAINST THE DB
      WRITE(WHERE_CLAUSE, 1) DARID
    1 FORMAT('where darid = ',I)
      WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)
 
      NRECS = db_num_records( %VAL(DB_SYBINT_USE_APS_READER_DBPROC), 
     ?    'dar'//char(0), WHERE_CLAUSE )
      IF ( NRECS .GT. 0 ) GO TO 9999

C  DARID NOT FOUND.   
      CALL DISMSG(
     ?'DAR ID not found in dar relation: Try again or type 0 to end.')
      GO TO 1000

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_SEGID
C
C PURPOSE
C   ASK THE USER FOR A MPS SEGMENT IDENTIFIER 1-99999
C
C OUTPUT
C   SEGID       SEGMENT IDENTIFIER
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_SEGID(SEGID)
      IMPLICIT NONE

      INTEGER IOS,SEGID

 1000 CONTINUE

      WRITE (6,100)
  100 FORMAT (/,' SEGID : ',$)

      READ (5,110,IOSTAT = IOS) SEGID
  110 FORMAT (I5)
     
      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (SEGID .LT. 1 .OR. SEGID .GT. 99999) THEN
        CALL DISMSG('Error : Invalid SEG ID.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_DTKID
C
C PURPOSE
C   ASK THE USER FOR A DTKID SAT,SENSOR,REV,ID
C   ASSEMBLE AND OUTPUT A DTKID SS/S/RRRRR.XX
C
C OUTPUT
C   DTKID       DATATAKE IDENTIFIER
C INTERNAL
C   SAT,SEN     SAT AND SENSOR
C   REVTXT      ASCII REV
C   IDTXT       ASCII ID
C   IOS     I/O STATUS
C   I,N     COUNTERS
C   REV     REV NUMBER
C   ID      ID NUMBER
C   ILEN,RLEN   STRING LENGTHS
C-----------------------------------------------------------------
      SUBROUTINE ASK_DTKID(DTKID)
      IMPLICIT NONE

      CHARACTER*(*) DTKID

      CHARACTER*2 SAT
      CHARACTER*3 SEN
      CHARACTER*10 REVTXT,IDTXT                               

      INTEGER REV,ID,ILEN,RLEN


      CALL ASK_SAT(SAT)

      CALL ASK_SENSOR(SAT , SEN)

      CALL ASK_REV(REV,' REV [1 - 99999] : ')
                                       
      CALL ASK_ID(ID)

C TRANSLATE THE REV TO ASCII - RIGHT JUSTIFIED - ZERO FILLED

      ENCODE(5,'(I5.5)',REVTXT) REV

      CALL TRIM(REVTXT,5,RLEN)

C TRANSLATE THE DTKID TO ASCII - RIGHT JUSTIFIED - ZERO FILLED

      ENCODE(2,'(I2.2)',IDTXT) ID

      CALL TRIM(IDTXT,2,ILEN)

C ASSEMBLE THE COMPLETE DTKID SS/SSS/RRRRR.ID

      DTKID = SAT(1:2) // '/' // SEN(1:3) // '/' //
     1        REVTXT(1:5) // '.' // IDTXT(1:2)

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_TIME
C
C PURPOSE
C   ASK THE USER FOR AN ASF FORMAT TIME YYYY:DDD:HH:MM
C   SECONDS AND FRACTIONS ARE DEFAULTED TO 00.000
C
C INPUT
C   PROM        PROMPT
C OUTPUT
C   TIME        ASF TIME
C INTERNAL
C   IOS     I/O STATUS
C   STAT        RETURN STATUS
C FUNCTION
C   SLEN        RETURNS STRING LENGTH
C-----------------------------------------------------------------
      SUBROUTINE ASK_TIME(TIME,PROM)
      IMPLICIT NONE

      CHARACTER*(*) TIME,PROM

C LOCAL
      CHARACTER*20 PTIME
      INTEGER IOS,STAT
      INTEGER SLEN

C FUNCTION

      INCLUDE 'APS_HOME:include/local/timeconv.inc'

 1000 CONTINUE

       WRITE (6,100) PROM
  100  FORMAT (/,A<SLEN(PROM)>,$)

       READ (5,110,IOSTAT = IOS) PTIME
  110  FORMAT (A14)

       IF (IOS .NE. 0) THEN 

         CALL DISMSG('Error on input.')
         GO TO 1000

       ELSE 

         TIME = PTIME(1:14) // ':00.000'

         STAT = tc_validate_asf_datetime(TIME // char(0) )

         IF (STAT .NE. 1) THEN
           CALL DISMSG('Error : Invalid time.')
           GO TO 1000
         END IF

       END IF

 9999 CONTINUE
      RETURN
      END

                       
C-----------------------------------------------------------------
C SUBROUTINE ASK_FNAME
C
C PURPOSE
C   PROMPT THE USER FOR A FILENAME AND VERIFY THAT IT IS CORRECT
C
C INPUT:
C   DIR DIRECTION 0 - READ / 1 - WRITE
C
C OUTPUT:
C   FILE    FILENAME
C
C INTERNAL
C   IOS I/O STATUS
C   I   COUNTER
C   STAT    RETURN STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_FNAME (FILE,DIR)

      IMPLICIT NONE

C INPUT:
      INTEGER DIR
C OUTPUT:
      CHARACTER*(*) FILE
C INTERNAL:
      INTEGER IOS,STAT

 1000 CONTINUE

      WRITE (6,100)
  100 FORMAT (/,' Filename : ',$)

      READ (5,110,IOSTAT = IOS) FILE
  110 FORMAT (A57)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000 
      END IF

C IF FOR READING, CHECK IF FILE EXISTS

      IF (DIR .EQ. 0) THEN

        CALL EXFNAM(FILE,STAT)

        IF (STAT .NE. 0) THEN
          CALL DISMSG('Error : File not found.')
          GO TO 9999
        END IF

C IF FOR WRITING CHECK TO SEE IF YOU CAN OPEN THE FILE

      ELSE 

        CALL CHFNAM(FILE,STAT)

        IF (STAT .NE. 0) THEN
          CALL DISMSG('Error : Could not open file.')
          GO TO 1000
        END IF

      END IF

 9999 CONTINUE
      RETURN
      END



C-----------------------------------------------------------------
C SUBROUTINE ASK_DBFILE
C
C PURPOSE
C   ASK THE USER IF THE COVERAGE SHOULD BE READ FROM DB OR FILE
C
C INPUT/OUTPUT
C   DBFILE      RESPONSE 1=DB,2=FILE
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_DBFILE (DBFILE)

      IMPLICIT NONE

      INTEGER DBFILE
      INTEGER IOS

 1000 CONTINUE

      WRITE (6,100)
      WRITE (6,101)
      WRITE (6,102)
      WRITE (6,103)
  100 FORMAT (/,' COVERAGE DATA SOURCE:')
  101 FORMAT (' 1) DATABASE ')
  102 FORMAT (' 2) FILE ')
  103 FORMAT (/,' Coverage data source: ',$)
      
      READ (5,*,IOSTAT = IOS) DBFILE
                             
      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (DBFILE .LT. 1 .OR. DBFILE .GT. 2) THEN
        CALL DISMSG('Error: Invalid entry.')
        GO TO 1000
      END IF
  
 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_TXFREQ
C
C PURPOSE
C   ASK THE USER HOW OFTEN TEXT SHOULD BE PRINTED FOR COVERAGE
C   DISPLAY
C
C INPUT/OUTPUT
C   FREQ        FREQUENCY VALUE 1-99
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_TXFREQ(FREQ)
      IMPLICIT NONE

      INTEGER FREQ

      INTEGER IOS

 1000 CONTINUE

      WRITE (6,100) 
  100 FORMAT (/,' Text frequency [1-99] : ',$)

      READ (5,110,IOSTAT = IOS) FREQ
  110 FORMAT (I5)

      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (FREQ .LT. 1 .OR. FREQ .GT. 99) THEN
        CALL DISMSG('Error : Invalid Frequency.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE TOUPPER
C
C PURPOSE
C   CONVERT A STRING TO ALL UPPERCASE
C
C INPUT/OUTPUT
C   STRING      ASCII STRING TO CONVERT
C   LEN     STRING LENGTH
C
C INTERNAL
C   I       COUNTER
C   IASCII      ASCII CHARACTER NUMBER
C-----------------------------------------------------------------
      SUBROUTINE TOUPPER(STRING,LEN)
      IMPLICIT NONE

C INPUT
      CHARACTER*(*) STRING
      INTEGER LEN

C INTERNAL
      INTEGER I,IASCII

      DO 1000 I = 1,LEN

        IASCII = ICHAR(STRING(I:I))

        IF (IASCII .GE. 97 .AND. IASCII .LE. 122) THEN

          STRING(I:I) = CHAR(IASCII-32)

        END IF

 1000 CONTINUE

 9999 CONTINUE
      RETURN
      END

      
C-----------------------------------------------------------------
C SUBROUTINE CVGMASK
C
C PURPOSE
C   PROMPT THE USER IF DATA SHOULD BE DISPLAYED ONLY FOR 
C   THE ALASKA MASK OR FOR ENTIRE GLOBE
C
C OUTPUT
C   MASKOPT     FLAG INDICATING MASK OR GLOBAL
C           1=GLOBAL / 2=ASF MASK / 3=MCMURDO MASK
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE CVGMASK(MASKOPT)

      IMPLICIT NONE

      INTEGER MASKOPT
      INTEGER IOS

 1000 CONTINUE
      WRITE (6,100)
      WRITE (6,101)
      WRITE (6,102)
      WRITE (6,103)
      WRITE (6,104)
  100 FORMAT (/,' DISPLAY DATA:')
  101 FORMAT (' 1) GLOBALLY')
  102 FORMAT (' 2) WITHIN ASF MASK')
  103 FORMAT (' 3) WITHIN MCMURDO MASK')
  104 FORMAT (/,' Display data: ',$)

      READ (5,*,IOSTAT = IOS) MASKOPT
                           
      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (MASKOPT.LT.1 .OR. MASKOPT.GT.3) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE SHOW_SEGS
C
C PURPOSE
C   DISPLAY THE OVERLAYS LIST ON THE SCREEN
C
C INPUT
C   NSEG        TOTAL NUMBER OF OVERLAYS
C   CRSEGN      ARRAY OF OVERLAY STATUS
C   CRSEGT      ARRAY OF OVERLAY NAMES
C   CRDARID     ARRAY OF OVERLAY DARIDS
C   TEMP        ARRAY OF FLAGS TO INDICATE IF OVERLAY HAS
C           ALREADY BEEN WRITTEN TO FILE
C   DARID       DARID OF SSCVG RECORDS
C   
C OUTPUT
C   SEGNUM      SELECTED SSCVG OVERLAY
C
C INTERNAL
C   I       LOOP COUNTER
C   IOS     INPUT/OUTPUT STATUS
C-----------------------------------------------------------------
      SUBROUTINE SHOW_SEGS(NSEG,CRSEGN,CRSEGT,CRDARID,
     1                TEMP,DARID,SEGNUM)

      IMPLICIT NONE

C INPUT:
      INTEGER NSEG,CRSEGN(*),CRDARID(*)
      INTEGER TEMP(*),DARID
      CHARACTER*(*) CRSEGT(*)

C OUTPUT:
      INTEGER SEGNUM

C INTERNAL:
      INTEGER I,IOS
      INTEGER LINECT

 1000 CONTINUE
C--   use LINECT to display 20 lines per page.
      LINECT = 0

      WRITE (6,100)
  100 FORMAT(/,' SSC OVERLAYS')

C DISPLAY THE OVERLAY NAMES AND NUMBERS

      DO 2000 I = 1,NSEG

C DISPLAY IF ITS DARID MATCHES
C AND IF IT HAS NOT BEEN REMOVED OR DELETED FROM MAPPER SESSION
C AND IF IT HAS NOT BEEN WRITTEN TO THE FILE PREVIOUSLY
                      
         IF (CRDARID(I).EQ.DARID .AND. CRDARID(I).NE.0 .AND.
     1       CRSEGN(I).GT.0 .AND. TEMP(I).EQ.0) THEN

           WRITE (6,200) I,CRSEGT(I)
  200      FORMAT (' ',I4,') ',A20)
           LINECT = LINECT + 1

           IF (LINECT .GE. 20) THEN
             LINECT = 0
             WRITE (6,205)
  205        FORMAT (/,' Press <RETURN> to Continue ',$)
             READ (5,*)
             WRITE (6,210)
  210        FORMAT (/)
           END IF

         END IF

 2000  CONTINUE

       WRITE (6,300)
  300  FORMAT ('  0) QUIT')

       WRITE (6,310)
  310  FORMAT (/,' Selection : ',$)

C ASK FOR OVERLAY NUMBER
       READ (5,*,IOSTAT = IOS) SEGNUM
       IF (IOS .NE. 0) THEN
         CALL DISMSG('Error on input.')
         GO TO 1000
       ELSE IF (SEGNUM .EQ. 0) THEN
         GO TO 9999
       ELSE IF (SEGNUM .LT. 0 .OR. SEGNUM .GT. NSEG) THEN
         CALL DISMSG('Error: Invalid overlay number.')
         GO TO 1000
       ELSE IF (TEMP(SEGNUM) .NE. 0) THEN
         CALL DISMSG('Error: previously chosen, not on menu.')
         GO TO 1000
       ELSE IF (CRSEGN(SEGNUM).LE.0) THEN
         CALL DISMSG(
     ?       'Error: previously removed or deleted, not on menu.')
         GO TO 1000
       END IF


C CHECK THAT DARID MATCHES
       IF (CRDARID(SEGNUM) .NE. DARID) THEN
         CALL DISMSG('Error: Invalid overlay number.')
         GO TO 1000     
       END IF

 9999 CONTINUE
      RETURN
      END



C-----------------------------------------------------------------
C SUBROUTINE ASK_LATLON
C
C PURPOSE
C   PROMPT THE USER FOR A LAT OR LON VALUE
C
C INPUT
C   MSG     PROMPT MESSAGE
C   MIN,MAX     MINIMUM AND MAXIMUM VALUES
C
C OUTPUT
C   NUM     LAT OR LON VALUE
C
C INTERNAL
C   IOS     I/O STATUS
C   MSGLEN      MESSAGE LENGTH
C   MINLEN,MAXLEN   MIN/MAX STRING LENGTHS
C   MINA,MAXA   MIN,MAX VALUES CONVERTED TO ASCII
C   PROMPT      PROMPT STRING INCLUDING LIMITS
C
C FUNCTIONS
C   SLEN        RETURNS STRING LENGTH
C-----------------------------------------------------------------
      SUBROUTINE ASK_LATLON(NUM,MSG,MIN,MAX)

      IMPLICIT NONE

C INPUT
      CHARACTER*(*) MSG
      REAL MIN,MAX

C OUTPUT
      REAL NUM

C INTERNAL
      INTEGER IOS
      INTEGER MSGLEN,MINLEN,MAXLEN
      CHARACTER*10 MINA,MAXA
      CHARACTER*60 PROMPT

C FUNCTIONS
      INTEGER SLEN

      MSGLEN = SLEN(MSG)

C CONVERT THE LIMITS TO ASCII
      ENCODE(6,'(F6.1)',MINA) MIN
      ENCODE(6,'(F6.1)',MAXA) MAX
      CALL TRIM(MINA,6,MINLEN)
      CALL TRIM(MAXA,6,MAXLEN)

      PROMPT = ' ' // MSG(1:MSGLEN) // ' [' // MINA(1:MINLEN) //
     1         ' TO ' // MAXA(1:MAXLEN) // '] :'

 1000 CONTINUE

C ASK USER
      WRITE(6,100) PROMPT
  100 FORMAT (/,A<SLEN(PROMPT)>,$)

C GET USER RESPONSE
      READ(5,*,IOSTAT=IOS) NUM
                  
      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (NUM .LT. MIN .OR. NUM .GT. MAX) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF
    
 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_ZMVER
C
C PURPOSE
C   VERIFY THE ZOOM WINDOW LATS/LONS
C
C INPUT
C OUTPUT
C   ANS     USER RESPONSE
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_ZMVER(ANS)

      IMPLICIT NONE

C INPUT
C OUTPUT
      CHARACTER*(*) ANS

C INTERNAL
      INTEGER IOS

 1000 CONTINUE

      WRITE (6,110) 
  110 FORMAT (/,' Correct? [Y/N]  ',$)


      READ (5,200,IOSTAT = IOS) ANS
  200 FORMAT (A1)
        
      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000 
      END IF

      CALL TOUPPER(ANS,1)

      IF (ANS.NE.'Y' .AND. ANS.NE.'N') THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------
C SUBROUTINE ASK_PLOTDEV
C
C PURPOSE
C   PROMPT THE USER FOR THE DEVICE TO OUTPUT THE MAP TO
C
C OUTPUT
C   PLOTDEV     1=HP7550
C           2=LA75
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_PLOTDEV(PLOTDEV)

      IMPLICIT NONE

      INTEGER PLOTDEV
      INTEGER IOS

 1000 CONTINUE
      WRITE (6,100)
      WRITE (6,101)
      WRITE (6,102)
      WRITE (6,103)
  100 FORMAT (/,' PLOT DEVICE:')
  101 FORMAT (' 1) HP7550 PLOTTER')
  102 FORMAT (' 2) LA75 PRINTER')
  103 FORMAT (/,' Plot device: ',$)

      READ (5,200,IOSTAT = IOS) PLOTDEV
  200 FORMAT(I2)
                           
      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (PLOTDEV.LT.0 .OR. PLOTDEV.GT.2) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE ASK_ZOOMOPT
C
C PURPOSE
C   SELECT A ZOOM OPTION
C
C OUTPUT
C   ZMOPT       1=SET NEW WINDOW
C           2=RESET TO ORIGINAL
C           3=RESET TO MAX WINDOW
C           0=QUIT
C
C INTERNAL
C   IOS     I/O STATUS
C-----------------------------------------------------------------
      SUBROUTINE ASK_ZOOMOPT(ZMOPT)

      IMPLICIT NONE

      INTEGER ZMOPT
      INTEGER IOS

 1000 CONTINUE
      WRITE (6,100)
      WRITE (6,101)
      WRITE (6,102)
      WRITE (6,103)
      WRITE (6,104)
      WRITE (6,105)
      WRITE (6,106)
  100 FORMAT (/,' SET WINDOW DIMENSIONS TO:')
  101 FORMAT (' 1) NEW')
  102 FORMAT (' 2) ORIGINAL')
  103 FORMAT (' 3) FULL')
  104 FORMAT (' 4) PREVIOUS NEW WINDOW')
  105 FORMAT (' 0) QUIT')
  106 FORMAT (/,' SELECTION : ',$)

      READ (5,200,IOSTAT=IOS) ZMOPT
  200 FORMAT(I2)
                           
      IF (IOS .NE. 0) THEN 
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (ZMOPT.LT.0 .OR. ZMOPT.GT.4) THEN
        CALL DISMSG('Error : Invalid entry.')
        GO TO 1000
      END IF

 9999 CONTINUE
      RETURN
      END
