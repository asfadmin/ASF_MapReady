	subroutine get_gha(iyear,iday,ihour,min,sec,gha_val)     
c/*	subroutine get_gha(iyear,iday,ihour,min,sec,gha_val)  --------
c routine to call the function gha for C language. dtc,9/13/91
c*/
	double precision gha,gha_val
	gha_val = gha(iyear,iday,ihour,min,sec,istat)     
	return
	end
	function gha(iyear,iday,ihour,min,sec,istat)     
c/*	function gha(iyear,iday,ihour,min,sec,istat)  --------
c
c   R.E.CARANDE  August 1, 1988.
C		3/89   add istat
c
c function for getting mean Hour Angle of Greenwich at input date/time
c                                         
c   routine uses following equation to determine greenwich hour angle:
c
c	GHA = 99.6910 + 36000.7689(T) + .0004(T)(T) + UT
c
c   where constants are in degrees and T is the time of interest 
c   expressed in units of JULIAN CENTURIES since noon 1/1/1900.  
c   The julian time is determined
c   via a look-up table of Julian dates of the first day of the years
c   1975 - 2000.                                        
c   UT is the input universal time in units of degrees.
c     therefore the value for 'we' below should be 360/86400.
c
c	reference: SPACECRAFT ATTITUDE DETERMINATION AND CONTROL, 
c		   ed.by James R. Wertz, D.Reidel Pub Comp.1978,80,84,
c  		   appendix J.
c                              
c   input values are single precision            
c   all calculations done throughout program are double precision.
c   return value gha is double precision (in degrees). 
c                                    
c*/
	double precision     utc2gha, seconds	!
	double precision     gha
	double precision     jd(30)  		!provision for 30 years
	double precision     ta,tb,tc   	!equation constants
        double precision     day,fracday,jd1900,t,ut,we
c  correct 'we' value, 3/27/91, dtc.
	data we /4.16666666666667d-3/ ! 360.0/86400 deg/seconds
c	data we /4.17807462193866d-3/ ! rotrate of earth in degrees per sec
	data jd1900 /2415020.0d0/   !Julian day for noon 1/1/1900
	data ta /99.6910d0/, tb/36000.7689d0/, tc/0.0004d0/
    	data jd /42412.5d0, 42777.5d0, 43143.5d0, 43508.5d0, 43873.5d0,
     .		 44238.5d0, 44604.5d0, 44969.5d0, 45334.5d0, 45699.5d0, 
     .  	 46065.5d0, 46430.5d0, 46795.5d0, 47160.5d0, 47526.5d0,
     .		 47891.5d0, 48256.5d0, 48621.5d0, 48987.5d0, 49352.5d0, 
     .		 49717.5d0, 50082.5d0, 50448.5d0, 50813.5d0, 51178.5d0, 
     .           51543.5d0,     0.0d0,     0.0d0,     0.0d0,     0.0d0 /           
      character*80 sccsid
      data sccsid /'@(#)gha.f	1.3 96/04/09 22:51:50\0'/
c
c use a different scheme to calculate gha by using a set of
c apparent gha's gotten by the MPS from Groucho.  These values
c in the file ACS_GHA_FIXES are updated on a weekly basis.
c The error introduce by this scheme should be less than 50 
c meters compared to 500 meters by the formula below.
c
	seconds = sec
	gha = utc2gha(iyear,iday,ihour,min,seconds)
	istat = 0
	return
c 
c the remainder of the code is not used.
c
c  check input bounds
c
c	if (iyear.lt.1975.or.iyear.gt.2000) then !not in data base
c		write(6,*)'GHA: Year out of 1975:2000 range'
c		istat = -1
c		return       
c	end if
c
c   calculate todays date
c                                                 
c	fracday= dble(iday) + dble(ihour)/2.4D1 + dble(min)/1.440D3 
c     . + dble(sec)/8.64D4                          
c	index = iyear- 1974
c      	day = 2.4D6 + jd(index) + fracday    ! julian day of date
c	day = day - jd1900		     ! julian day relative to jd1900
c	t = day/3.6525D4	  	     ! julian cen. wrt jd1900
c	ut=we*(dble(ihour)*3600.d0 + dble(min)*60.d0 + dble(sec)) !in deg
c	gha = (ta + tb*t + tc*t*t+ ut)       ! gha in doub prec. degrees
c	gha =dmod(gha,360.d0)
c	istat = 0
c	return                                       
	end
