	subroutine getstat(data,max,x3db,pslr,islr,nbins,NSIZE)
c/*	subroutine getstat(data,max,x3db,pslr,islr,nbins,NSIZE) ------
c
c   subroutine that measures 3db width of peak at bin max of 
c   real array data (x3db units = bins), as well as the PSLR
c   and ISLR of the response in data.  ISLR is calculated out
c   to nbins to right of peak.
c
c	INPUT:	data real*4 dimensioned nsize.  represents the
c		data line of which a poit target measurement is 
c		to be made.
c		max  integer*4  element of data containing the
c			peak to be measured.
c		nbins	number of bins to right of bin max used
c			in the ISLR calculation.
c		nsize	size of input vector data
c	RETURN:	x3db real*4  3dB width of main lobe containing bin max
c			     in units of bins.
c		pslr real*4  ratio of first side lobe to main lobe level
c			     in dB 
c	 	islr real*4  ratio of energy in side lobes (integrated out
c			     nbins from max) to energy in main lobe.
c			     Uses Simpson integration that is circular
c			     at the end of the vector data. I.e., bin
c			     nsize is next to bin 1.  	In dB.
c
C   NSIZE IS THE SIZE OF THE DATA ARRA DATA(NSIZE)
C   
c*/
	real*4 data(1), ISLR
      character*80 sccsid
      data sccsid /'@(#)getstat.f	1.3 96/04/09 22:51:50\0'/
	call get3db(data,max,x3db,nsize) ! gets 3db point
c	write(6,*)'3db found=',x3db
c
c   calculate pslr
c          
c   first find position of null (bin nul) and side lobe (nsl)
	xlast=data(max)
	do i=max+1,max+nbins ! look for 1rst null within nbins
	xnew=data(i)
	if (xnew .gt. xlast) then !found null
		nul=i-1
		xlast=xnew
c		write(6,*)'null found=',nul
		goto 100
	else
		xlast=xnew
	end if
	end do
100	do i=nul+1,max+nbins ! look out to nbins+max for 1rst side lobe
	xnew=data(i)
	if (xnew .lt. xlast) then ! found side lobe position
		nsl=i-1                    
c		write(6,*)'sl found:',nsl,data(nsl-1),data(nsl)
c     .,data(nsl+1)    
c	        fit parabola to sl peak: 
c		call parabmax(data(nsl-1),data(nsl),data(nsl+1),xsl)
c		write(6,*)'fit maximum to sl=',xsl           
		pslr = 10.*log10 (data(nsl)/data(max)) 
c		write(6,*)'pslr = ',pslr
		goto 200
	else  ! continue searching
		xlast=xnew
	endif
	end do                   
200	call simpWRAP (data,max,max+nul,NSIZE,peak)  !integrate half of peak
	call simpWRAP (data,nul+1,max+nbins,NSIZE,sidelobes) ! integrate null to nbins
c	write(6,*)'peak and sls integ.=',peak,sidelobes
	islr=10.*log10(sidelobes/peak)                 
c	write(6,*)'islr=',islr
	return
 	end
