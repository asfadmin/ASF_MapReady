	subroutine mkwvec (vec,h,pbw,prf,fd,nsize)
c/*	subroutine mkwvec (vec,h,pbw,prf,fd,nsize) ------------
c
c	AUTHOR: RICHARD E. CARANDE       DECEMBER 1988.
c
c   This subroutine generates a cos**2 weighting vector of the form:
c
c		vec(i)= h + (1-h)*cos(i)**2 
c                               
c   over the entire Processor Bandwidth (pbw).  Data outside pbw
c   is 0'd.  Vec is a real vector.
c   The weighting is centered on the center frequency fd, there are
c   nsize complex elements in vec.
c   Bandwidth of vec is pbw in steps of nsize/prf, bin 0 is dc, bin
c   nsize/2 =prf/2. - 1/prf,  bin nsize = prf - 1/prf 
c
c	INPUT:  h = pedestal height
c		pbw = bandwidth of output vec 
c		prf = sampling rate of time domain signal, or potential
c			bandwidth of output
c		fd = center frequency of vec, i.e. where cosine square
c			weighting function should be maximum.
c		nsize = number of points in vec
c
c	OUTPUT: vec = real vector containing the cosine on a pedestal 
c			function as described in above equation.
c
c*/
	real vec(nsize)
	data pi /3.141592654/
      character*80 sccsid
      data sccsid /'@(#)mkwvec.f	1.3 96/04/09 22:51:54\0'/
	a=(1.-h)   
	fstep=float(nsize)/prf
	xk=pi/pbw  
	nbins=int(pbw/prf*nsize + .5)
	nbin1=int((fd-pbw/2)*float(nsize)/prf + .5)+1
	write(6,*)'mkwvec: nbin1 and nbins = ',nbin1,nbins
c
c   Begin Loop
c
	do i=1,nsize
	nbin = nbin1 + (i-1)   ! bin  # of interest (not moduloed) 
	nbin = mod(nbin,nsize)
	if (nbin .le. 0) nbin =nbin + nsize
c
c see if in cosine bandwidth or not
c
	if (i .le. nbins) then  ! in band
	  f = fd - pbw/2. + (i)/fstep
	  vec(nbin) = h + a*(cos((f-fd)*xk)**2)
	else  !out of band
	  vec(nbin) = 0.
	end if
	end do
	return            
	end

