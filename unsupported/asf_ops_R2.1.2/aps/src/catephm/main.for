C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	main.for
C--
C--  Description:	main routine to handle arguments for catephm.for
C--	
C--  Notes:
C--
C-- ==========================================================================

      PROGRAM MAIN 
      IMPLICIT NONE
	  character*200 filename, progname
      integer EUNIT, iargc, lastc, ierr
	  integer print_usage
      character*100 SccsFileID
     -/'@(#)main.for	5.1 98/01/08 APS/ASF\0'/

	  print_usage = 0
	  if( iargc() .lt. 1) then
		  print_usage = 1
	  else
		  call getarg(1, filename)
		  if(filename(1:5) .eq. '-help') print_usage = 1
	  endif
	  if( print_usage .ne. 0) then
		call getarg(0, progname)
		print*,'usage:  ',progname(1:lastc(progname)),' ephemeris_file'
		print*,' '
		print*,
     ?'This program formats a binary ephemeris file, created by'
        print*,'create_nominal_orbit, to standard output.  The epheme
     ?ris file contains '
        print*,'state vectors and other data at one minute intervals.'
		print*,'The coordinate system is Mean of Epoch Earth Equator Equ
     ?inox.'
		print*,'  '
		print*,'The elements of the file are described with the short na
     ?mes used in the '
		print*, 'formatted ASCII output:'
		print*, ' '
		print*,'The first element in the file is the epoch of the coordi
     ?nate system in real ','julian days.  Next are repeating groups of 
     ?1 character and 12 real numbers:    '
		print*,' 0. A node flag, (N or _), which identifies an ascending
     ? node.'
		print*,' 1-3.  X, the satellite position vector (xyz)  '
		print*,' 4-6.  V, the satellite velocity vector (xyz)  '
		print*,' 7. ET, the ephemeris time in real julian days for this 
     ?data.'
		print*,' 8. REV, the rev number for this data          '
		print*,' 9. LASC, the Longitude of ASCending node for the orbit 
     ?in the above ','    coordinate system.  ' 
		print*,'10-11. SS LAT/LON, the geocentric lat/lon of the subsatelli
     ?te point.'
		print*,'12. GHA, Greenwich Hour Angle: the angle from the X axis
     ? to the prime meridian.' 
		print*,' '
		stop
      endif
	  print *,'EPHEMERIS FILE NAME:  ', filename(1:lastc(filename))
      open(EUNIT,FILE=filename, err = 8001, iostat = ierr, 
     ?      STATUS = 'old', FORM = 'unformatted')
C---	POSITION TO START OF THE FILE.  
	  CALL CATEPHM(EUNIT, IERR)
      IF(IERR .NE. 0) GO TO 8888
C---	NORMAL END
      print *,'END OF FILE'
	  stop
 8001 continue
      call getarg(0, progname)
      print*,progname(1:lastc(progname)),
     ?          ':  ERROR opening file;  system error no. ', ierr
	  stop
 8888 continue
      call getarg(0, progname)
      print*,progname(1:lastc(progname)),
     ?          ':  ERROR reading file;  system error no. ', ierr
	  stop
	  end
