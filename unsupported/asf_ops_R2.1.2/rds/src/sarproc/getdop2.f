    	subroutine getdop2(xlam,statevec,gamma,a,fd,fdot) 
c/* 	subroutine getdop2(xlam,statevec,gamma,a,fd,fdot) -------
c
c    Getdop performs vector arithmetic to calculate doppler
c    parameters for orbiting SAR.
c
c	Input:  xlam = wavelength of radar (m,double precision)
c		statevec = x,y,z,xdot,ydot,zdot of SAR  (m,double precision)
c		gamma = look angle (radians,double precision)
c		a = azimuth anlge (radians,double precision)
c	
c	Return:	fd = Doppler center frequency
c		fdot = Doppler frequency rate
c
C    5/5/88:	range is determined iteratively by function calcr
c		instead of being input.
c                       
c	aug 88 made double precision 
c*/
	implicit double precision(a-h,o-z)
	double precision rt(3),rvec(3),rot(3,3),xsc(3),vsc(3),vt(3),vrel(3),
     .asc(3),at(3),arel(3),statevec(6)       
	double precision rcalc,getlook,dot
	common /relative/tr1,tr2,tr3,tv1,tv2,tv3,vv1,vv2,vv3,ra1,ra2,ra3
c   xsc + rvec = rt             
	common /earth/ rp,re
	common /transform/ rot
	data we/7.29211585d-5/,pi/3.141592654/
	data gme /3.986005d14/
      character*80 sccsid
      data sccsid /'@(#)getdop2.f	1.3 96/04/09 22:51:48\0'/
c	write(6,*)'rotation matrix:'
c	do i=1,3
c	write(6,*)( rot(i,j),j=1,3)
c	write(6,*)' '   
c 	end do
c
c   redefine parameters
c                        
	do i=1,3
	xsc(i)=statevec(i)
	vsc(i)=statevec(i+3)
 	end do
c
c   calculate x,y,z position
c            
 	rvec(1)= dsin(a)
	rvec(2)=-dsin(gamma)*dcos(a)
	rvec(3)=-dcos(gamma)*dcos(a)           
c   rvec points to target.  Translate into earth centered unit vector:
       	call matmul(rot,rvec,rt) 
c   put rt into correct rvec position
	range = calcr(xsc,gamma,a)
	rvec(1)=rt(1)*range
	rvec(2)=rt(2)*range                   
	rvec(3)=rt(3)*range
c	write(6,*)'rvec',rvec
	call vecadd(rvec,xsc,rt) ! rvec+statevec=rt 
c	write(6,*)'rt',rt
c
c  Now we have all three vectors in earth centered coordinates:
c     statevec = sar position
c     rvec = range vector from sar to target
c     rt = target position
c                                       
c	write(6,*) 'Rt (earth centered) =',rt    
c
c   calculate velocity vectors vt and vrel.
c
	vt(1)= -we*rt(2)    
	vt(2)= we*rt(1) 
	vt(3)= 0.        
c	write(6,*)'vt',vt
 	vrel(1)=vt(1)-vsc(1)
	vrel(2)=vt(2)-vsc(2)
	vrel(3)=vt(3)-vsc(3)
c
c  Calcuate accelerations of sar and target asc,at
c                                       
 	asc(1)=0.
	asc(2)=0.
c     sar acceleration toward earth center      -GxMe / h**2
	asc(3)=-gme/(xsc(1)*xsc(1)+xsc(2)*xsc(2)+xsc(3)*xsc(3))
	call matmul(rot,asc,asc) !put in e.c. coordinates
c	write(6,*)'asc (e.c.)',asc
c   calculate acceleration of target on earth surface:
	at(1)=-rt(1)*we**2
	at(2)=-rt(2)*we**2
        at(3)=0.
c	write(6,*)'at (e.c.)',at
 	arel(1)=at(1)-asc(1)
	arel(2)=at(2)-asc(2)
	arel(3)=at(3)-asc(3) 
c
c	write(6,*)'rvec,vrel,dotprod',rvec,vrel,dot(rvec,vrel)
c	write(6,*)'vrel**2',dot(vrel,vrel)  
c	write(6,*)'rvec,arel,dotprod =',rvec,arel,dot(rvec,arel) 
cc
c   calculate doppler parameters
c
	fd=-2./(xlam*range)*dot(rvec,vrel) 
	fdot=-2./(xlam*range)*(dot(vrel,vrel)+dot(rvec,arel))
c	write(6,*)'fd = ',fd,'       fdot = ',fdot
C	goto 10                 
c  
c  following assingments for common block
c
	tr1= rt(1)
	tr2=rt(2)
	tr3= rt(3)
 	tv1 =vt(1)
	tv2= vt(2)
	tv3=  vt(3)
	vv1=vrel(1)
     	vv2=vrel(2)
	vv3=vrel(3)
	ra1=arel(1)
	ra2=arel(2)
	ra3=arel(3)
	RETURN
	end
