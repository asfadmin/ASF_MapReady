C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  get_sensor_list.for
C--
C--  Description:
C--
C--  Notes:
C--
C-- ==========================================================================
      character*100 SccsFileID
     -/'@(#)get_sensor_list.for	5.1 98/01/08 APS/ASF\0'/

       SUBROUTINE GET_SENSOR_LIST (SAT, LIST, LIST_SIZE, COUNT)

       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_satsensor.inc'

       CHARACTER*2   SAT
       CHARACTER*3   SEN
       CHARACTER*10  LIST (*)
       INTEGER       LIST_SIZE
       INTEGER       COUNT

       CHARACTER*2   cdummy2
       CHARACTER*15  cdummy
       INTEGER       idummy
       REAL          rdummy
       INTEGER       NRECS

       INTEGER       LASTC

C-----------------------------------------------------------------------

       COUNT = 0 

       WRITE(WHERE_CLAUSE, 1) SAT
    1  FORMAT('where sat = "',A,'" and cvrg_allowed = "Y"')

       WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)

       llistptr = db_get_records(%VAL(MAPR_DBPROC),
     ?   'satsensor'//char(0),
     ?   WHERE_CLAUSE, char(0),SATSENSOR_COLUMNS,%VAL(ALL_COLS))
C--        check for a db error.  do not handle here.  
       if ( llistptr .EQ. 0 )
     ?    GO TO 9999

       call get_no_elements(llist,NRECS)
 
       IF (NRECS .EQ. 0) THEN
         GO TO 9000
       ENDIF

       P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)
 
       DO 3000 WHILE (P2_DATA_RECORD .NE. 0)
 
             call get_satsensor_rec(%VAL(P2_DATA_RECORD),
     ?       cdummy2, SEN, idummy, cdummy,rdummy,rdummy)
 
             IF (COUNT .GT. LIST_SIZE) THEN
               CALL DISMSG('CAN NOT DISPLAY ALL SENSORS.')
               GO TO 9000
             END IF

             COUNT = COUNT + 1
 
             LIST (COUNT) = SEN //char(0)

             P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)
 
 3000  CONTINUE

 9000  CONTINUE
C---      free the linked list:  list + members, if any.  
       CALL db_ftn_free_llist(llist)

 9999  CONTINUE
       RETURN
       END

