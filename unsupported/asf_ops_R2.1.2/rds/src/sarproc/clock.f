	subroutine clock (e1,e2,e3,e4,snr,prf,pbw,dbw,deltafd,istat) 
c/*     subroutine clock (e1,e2,e3,e4,snr,prf,pbw,dbw,deltafd,istat) -----
C
C	R.E. CARANDE MARCH 89.
C
c   Definition:  By energy, I mean amplitude - NOT POWER (sorry)
c
c   This routine calculates the error in the assumed fd used to make
c   images of four looks.   e1,e2,e3, and e4 represent the summed 
c   energy of the respective looks.     
c
c  Look 1 is the lowest frequency look (most recent time)
c  Look 4 is the highest frequency look (longest time)   
c                                                          
c	INPUT	 e1  summation of energy in look 1 (real*4)
c		 e2  summation of energy in look 2 (real*4)
c		 e3  summation of energy in look 3 (real*4)
c		 e4  summation of energy in look 4 (real*4)
c			(summation means adding up pixel values)
c		 snr = signal to noise ratio (power) in dB real*4
c		 prf = prf (Hz) real*4
c		 pbw = processor bandwidth (sum of 4 looks) real (Hz)
c		 dbw = Doppler bandwidth of antenna pattern (Hz) real
c		 istat = 0 non verbose, = 1 verbose, = -1 initialize
c	
c	RETURN	 deltafd = update required to the fd used.  
c		           That is,  fd(new) = fd(old) + deltafd
c		 istat = 0 successful completion, deltafd ok
c		       = <0, unsuccessful completion, deltafd not ok.
c		       = -2  more than 2 "c" or "e" matches made
C
c	ALGORITHM:	An ideal (sinc**2) antenna pattern of the 
c			appropriate 3dB bandwidth (dbw) is calculated.
c			This represents the noise-free pattern. The 
c			parameters e and c are calculated using 1024
c			different center frequencies:
c			
c			e = (E1 + E2 - E3 - E4) / ETOT
c			c = (E2 + E3 - E1 - E4) / ETOT
c
c			where ETOT = E1 + E2 + E3 + E4
c			and En are the summation of the energy in
c			each look's bandwidth, where the four looks are 
c			contiguous and centered at the particular center
c			frequency.  Once the 1024 values of e and c are
c			calculated for the noise free pattern, they can 
c			be used to compare to the e and c parameters 
c			calculated using the input (noisy) e1,e2,e3 and
c			e4 parameters, suitably modified to account for the
c			signal to noise, to estimate the imbalance in the
c			energies and the correct adjustment (deltafd) that
c			should be made.
c
c
c	NOTE:  A TEMPLATE IS CALCULATED IN ROUTINE MKCL_TABLE.  THIS
C		TEMPLATE, OR LOOK-UP TABLE REPRESENTS THE NOISE-FREE
C		VALUES FOR C AND E.  IT IS *ONLY* CALCULATED ON THE FIRST
C		CALL TO THIS ROUTINE.  SINCE IT IS A FUNCTION OF PRF,PBW
C		AND DBW,  IT IS ASSUMED THAT THESE PARAMETERS WILL NOT CHANGE
C		DURING A RUN IN WHICH THIS ROUTINE IS NOT INITIALIZED.  I.E.
C		IF THEY DO CHANGE, THE CALLING PROGRAM SHOULD BE RESTARTED
C		TO INSURE THE CORRECT NOISE FREE TABLE IS MADE.
C*/
	integer nbine(2),nbinc(2)
	integer diff,dmin,dmin2
	logical first,verbose,btwn 
	real etable(1024),ctable(1024)
	data first /.true./
      character*80 sccsid
      data sccsid /'@(#)clock.f	1.3 96/04/09 22:51:39\0'/
	if (istat .gt. 0) then
		verbose =.true.
	else
		verbose = .false.
	end if
	if (istat .lt. 0) then
		first = .true.
	end if
	if (first) then
		first = .false.
		if (verbose)write(6,*)'Making IDEAL look-up table...'
		call mkcl_table (prf,pbw,dbw,etable,ctable) !mk error table
		cmax=-10.
		cmin=+10.
		emax=-10.
		emin=+10.
		do i=1,1024  !find extreme table values
		if (ctable(i).gt.cmax)then
			icmax=i
			cmax=ctable(i)
		end if
		if (ctable(i).lt.cmin)then
			icmin=i
			cmin=ctable(i)
		endif
		if (etable(i).gt.emax)then
			iemax=i
			emax=etable(i)
		end if
		if (etable(i).lt.emin)then
			iemin=i
			emin=etable(i)
		endif
		end do                       
		if(verbose)then
			write(6,*)'look-up table extremes:'
		        write(6,*)'emin,max',etable(iemin),etable(iemax)
			write(6,*)'at elements',iemin,iemax
			write(6,*)'cmin,max',ctable(icmin),ctable(icmax)
		        write(6,*)'at elements',icmin,icmax
		end if
	end if
	if (istat .lt. 0) then
		return
	end if
c
c   calculate normalized antenna bandwidth (bw) and normalized proc. bw
c
	bw = dbw/prf  
	pbwnorm = pbw/prf                                    
c
c   calculate err and cerr terms, including noise correction:
c                                  
c	
	snratio = 10.**(snr/20.)
	etot = (e1+e2+e3+e4)/(1.+1./snratio) ! total energy corrected by noiseterm
	err = (e1+e2 - (e3+e4))/etot     ! left minus right       
 	cerr =(e2+e3- (e4+e1))/etot      ! center minus edges
	if (verbose) write(6,*)'snratio,etot,err,cerr = ',snratio,etot,
     .err,cerr
c
c   Use ideal noise free pattern to calculate error
c
	nbine(1) = -1  !initialize to nonsense
	nbine(2) = -1
	nbinc(1) = -1
	nbinc(2) = -1      
	iccnt = 0
	iecnt = 0
	do i = 1,1023   !find bins that match
	if (btwn (err,etable(i),etable(i+1))) then
		if (verbose)write(6,*) 'e match found:', 
     .i,err,etable(i),etable(i+1)
		iecnt = iecnt + 1
		if (iecnt .gt. 2) then  ! too many matches
			istat=-2
			write(6,*)'CLOCK:  more than 2 clutter lock 
     .matches found for e parameter'
		return
		end if
		nbine(iecnt) = i
	end if	  
        if (btwn (cerr,ctable(i),ctable(i+1))) then
		if (verbose)write(6,*) 'c match found:', 
     .i,cerr,ctable(i),ctable(i+1)
		iccnt = iccnt + 1
		if (iccnt .gt. 2) then  ! too many matches
			istat=-2         
			write(6,*)'CLOCK:  more than 2 clutter lock 
     .matches found for c parameter'
		return
		end if
		nbinc(iccnt) = i
	end if	         
	end do                                
	if (verbose) then                                              
		write(6,*)'e bin match ideal in bins',nbine(1),nbine(2)
	      	write(6,*)'c bin match ideal in bins',nbinc(1),nbinc(2)
	end if

	if (nbine(1).lt.0 .or. nbine(2).lt. 0) then    
		if (verbose) write(6,*)'CLOCK: eval beyond extremes...'
		if (err.gt.emax)nbine(1) = iemax
		if (err.lt.emin)nbine(1) = iemin
		nbine(2)=nbine(1)
	end if
	if (nbinc(1).lt.0 .or. nbinc(2).lt. 0) then
		if (verbose) write(6,*)'CLOCK: cval beyond extremes...'
		if (cerr.gt.cmax)nbinc(1) = icmax
		if (cerr.lt.cmin)nbinc(1) = icmin
		nbinc(2)=nbinc(1)
	end if
c
c  find out which bins agree best
c                           
	dmin = 10000
	dmin2 = 10000
	do i = 1,2
	    do j = 1,2
		diff = iabs(nbinc(i) - nbine(j))
		if (diff .lt. dmin) then
		    dmin2 = dmin
		    imin2 = imin
		    jmin2 = jmin
		    dmin = diff
		    imin = i
		    jmin = j
		else if (diff .lt. dmin2) then
		    dmin2 = diff
		    imin2 = i
		    jmin2 = j
		end if
	    end do
	end do
	deltafd = -prf*(nbine(jmin) - 512.)/1024.
	deltafd2 = -prf*(nbine(jmin2) - 512.)/1024.
	if ((cerr .ge. 0  .and. abs(deltafd) .gt. abs(deltafd2)) .or.
     .      (cerr .lt. 0 .and. abs(deltafd) .lt. abs(deltafd2))) then
		deltafd = deltafd2
		imin = imin2
		jmin = jmin2
	end if
	ebin = ( nbinc(imin)+nbine(jmin) )/2.

	if(verbose) write (6,*)'avg of best: ',ebin
	if(verbose) write(6,*)' delta fd = ',deltafd
	istat = 0
	return

	end         
	FUNCTION BTWN (X1,X2,X3)
c/*	FUNCTION BTWN (X1,X2,X3) ------------------
c
c LOGICAL FUNCTION BETWEEN
C
C	RETURNS TRUE VALUE IF X1 BETWEEN X2 AND X3
C*/
	LOGICAL BTWN     
	IF (X2 .LT. X3) THEN              
		IF (X1 .GT. X2  .AND. X1 .LE. X3) THEN
			BTWN=.TRUE.
		ELSE              
			BTWN=.FALSE.
		END IF
	ELSE
		IF (X1 .LE. X2  .AND. X1 .GT. X3) THEN
			BTWN = .TRUE.
		ELSE      
			BTWN = .FALSE.
		END IF
	END IF        
	RETURN
	END
			
