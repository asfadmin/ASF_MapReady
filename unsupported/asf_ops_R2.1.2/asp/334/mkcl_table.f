C Alaska SAR Processor (ASP) %W% %E% %U%
	subroutine mkcl_table (prf,pbw,dbw,err,cerr)
c/*	subroutine mkcl_table (prf,pbw,dbw,err,cerr)  ----------
c
c   subroutine to make clutterlock look up table. Assumes a 
c   sinc squared antenna pattern and simply calculates the 
c   e and c terms as a function of the doppler fd offset on a 
c   noise free pattern
c   
c	input:	prf = prf (hz)
c		pbw = processor bandwidth (full, sum of look bandwidths)
c		dbw = Doppler bandwidth of antenna pattern (3dB)
c
c	return	etable = 1024 real entries of "e" values as function
c			 of error from fd.
c		ctable = 1024 real entries of "c" values as function
c			 of error from fd.
c
c	the 512th entry is where the error from fd is 0.
c                                    
c*/
 	real ant(1024),err(1),cerr(1)
	bw=dbw/prf
	npow = 2   !sinc squared
	pbwnorm = pbw/prf
c
c   create antenna pattern
c       
	fstep=1./1024.                                    
 	do i=-511,512
	f=(i-1)*fstep
	t=2*1.389351*f/bw         
	ant(i+512)= sinc1(t)**npow                             
	end do                    
c
c	write(6,*)'plot of antenna pattern :'
c	call plotsc(x,ant,1024,01,-8.,-5.,'f',1,' ',1,
c     .'Antenna Pattern',15,x(1),x(1024),
c     .0.,1.,)
c
c   begin four look clutter lock error measure
c                                                           
	nbinlook=(pbwnorm/4.)*1024  !number of bins in look
	do i=0,1023
c 	if (mod(i,10).eq.1) write(6,*)'mkcl_table:',i
	icut1 = mod(i-2*nbinlook,1024)
	icut2 = mod(i-nbinlook,1024)
	icut3 = mod(i,1024)
	icut4 = mod(i+nbinlook,1024)
	if (icut1 .le. 0) icut1=1024+icut1
	if (icut2 .le. 0) icut2=1024+icut2
	if (icut3 .le. 0) icut3=1024+icut3
	if (icut4 .le. 0) icut4=1024+icut4
c
c sum energies
c              
	e1=0.
	e2=0.
	e3=0.
	e4=0.                                            
	etot=0.
	do j=icut1,icut1 + nbinlook-1
	  index = j
	  if (index.gt.1024) index=index-1024 
	  e1=e1+ant(index)
	end do
	do j=icut2,icut2 + nbinlook-1
	  index = j                     
	  if (index.gt.1024) index=index-1024 
	  e2=e2+ant(index)
	end do
	do j=icut3,icut3 + nbinlook-1
	  index = j
	  if (index.gt.1024) index=index-1024 
	  e3=e3+ant(index)
	end do
	do j=icut4,icut4 + nbinlook-1
	  index = j
	  if (index.gt.1024) index=index-1024 
	  e4=e4+ant(index)
	end do        
	do j=icut1,icut1 + 4*nbinlook-1
	  index = j
	  if (index.gt.1024) index=index-1024 
	  etot=etot+ant(index)
	end do        
                 
	err(i+1) = (e1+e2 - (e3+e4))/etot  ! left minus right       
	cerr(i+1)=(e2+e3- (e4+e1))/etot    ! center minus edge

	end do                                           
	return
	end
	function sinc1(x)
c/*	function sinc1(x)   ----------------
c
c*/
	if (x .eq.0.) then
		sinc1 = 1.
		return
	else
        	sinc1 = sin(x)/x
	return
	end if
	end
