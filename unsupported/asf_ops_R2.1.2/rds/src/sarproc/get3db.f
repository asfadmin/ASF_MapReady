	subroutine get3db (array,max,x3dbwidth,nsize)
c/*     subroutine get3db (array,max,x3dbwidth,nsize)  -----------
c
c	AUTHOR R.E. Carande
c
C  array is not in db, it is linear
c 
c  this subroutine accepts a real array that has a peak in element max
c  x3dbwidth is calculated which is the 3db width of the peak in units
c  of bins (real)
c*/                           
	real array(1),x(100)
	data db3 /.5011872/ 
      character*80 sccsid
      data sccsid /'@(#)get3db.f	1.3 96/04/09 22:51:48\0'/
	do i=1,100
	x(i)=float(i)
	enddo
	xmax=array(max)
	arlast=xmax
	do i=max-1,max-nsize/2,-1 
	index=i
	if (index .le. 0) index=index+nsize
c	write(6,*)'i,index,arlast,array(index),xmax',i,index,arlast
c     .,array(index),xmax
	if (arlast .lt. array(index)) then ! hit bottom, no 3db point this lobe
		write(6,*) 'get3db: no left 3db point this lobe'
		x3dbwidth=0.
		return
	endif
	if (array(index)/xmax .lt. db3) goto 100
	arlast=array(index)
	end do
	write(6,*)'get3db: left 3db point not found'
	x3dbwidth=0.
	return
c
c  found left 3db area. now linear interpolate
c                         
100	index1=i+1
	if (index1.gt.nsize)index1=index1-nsize 
	if (index1.le. 0) index1=index1+nsize
	xleft=float(i)+abs((array(index)/xmax-db3)/
     .(array(index)/xmax-array(index1)/xmax))
c	write(6,*)i,array(i)/xmax,array(i+1)/xmax,xleft
c
c  get right 3db point
c                         
	arlast=xmax
	do i=max+1,max+nsize/2 
	index=i
c	write(6,*)'i,index,arlast,array(index),xmax',i,index,arlast
c     .,array(index),xmax
	if (index .gt. nsize) index=i-nsize
	if (arlast .lt. array(index)) then ! hit bottom, no 3db point this lobe
		write(6,*) 'get3db: no right 3db point this lobe'
		x3dbwidth=0.
		return
	endif
	if (array(index)/xmax .lt. db3) goto 200
	arlast=array(index)
	end do
	write(6,*)'get3db: right 3db point not found'
	x3dbwidth=0.
	return            
200	index1=i-1
	if (index1.le.0) index1=index1+nsize
	if (index1.gt.nsize)index1=index1-nsize
     	xright=float(i)-abs((array(index)/xmax-db3)/
     .(array(index)/xmax-array(index1)/xmax))
c	write(6,*)i,array(i)/xmax,array(i-1)/xmax,xright 
c	write(6,*)xleft,xright
c        write(6,*)' '
	x3dbwidth=xright-xleft	                            
c	write(6,105)(i,i=-5,5)
c	write(6,106)(array(i)/xmax,i=nsize-5,nsize),(array(i)/xmax,
c     .i=1,5)
c105	format(' ',11i6)
c106	format(' ',11f6.3) 
c	write(6,*)' '
	return
	end
