c SccsId = @(#)COMMON_ONE_TWO.f	2.41 3/24/98 


	subroutine ac_to_ebf(c1,c2,x,y,z,lat,lat_d,lon,rlocal,
     *  alpha1,alpha2,alpha3)
c	--------------------------------------------------------------
C 	Abstract:
C	Converts the body fixed coordinates into the along/cross track
c	coordinates.
c	--------------------------------------------------------------
	implicit none
C*****WAYNE******
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
C*****WAYNE******
c	------------------
C	PASSING IN PARAMETERS
c	------------------
	real*8  c1,c2			!A/C coordinate
	real*8	rlocal

c	------------------
C	PASSING OUT PARAMETERS
c	------------------
	real*8	x,y,z			!position in ebf	

c	-------------
c	LOCAL VARIABLE
c	-------------
	real*8 qlat,qlon,lat,lon,lat_d

	call ac_to_qsph(c1,c2,rlocal,qlat,qlon)
	call qsph_to_ebf(qlat,qlon,rlocal,
     *			 x,y,z,lat,lat_d,lon,alpha1,alpha2,alpha3)

	return
	end

        function btoi(a)
	byte a
	integer btoi

	btoi = a
	if(btoi.lt.0) btoi = btoi + 256

	return
	end
	subroutine mats(a,x,n)
	implicit real (a-h,o-z)
	real a(n,n+1),x(n)

c  This routine does a gauss-jordan elimination and back substitution
c  to solve the matrix equation ax=b for x, where b is stored in
c  the (n+1)TH column of a

	mm=n+1
	   do 3 i=2,n
	ii=i-1
	   do 3 j=1,ii
	caij=abs(a(i,j))
      if(caij.eq.0) go to 3
      if(caij.gt.abs(a(j,j))) then
	r=a(j,j)/a(i,j)
	   do 1 k=1,mm
	b=a(j,k)
	a(j,k)=a(i,k)
 	a(i,k)=b
 1	continue
      else
	r=a(i,j)/a(j,j)	    
      endif
	jj=j+1
	   do 2 k=jj,mm
 2	a(i,k)=a(i,k)-r*a(j,k)
 3	continue
      if(abs(a(n,n)).lt.1.e-30) write(6,100)
	x(n)=a(n,mm)/a(n,n)
	   do 5 i=2,n
	jj=n-i+1
	ii=jj+1
	b=0.
	   do 4 k=ii,n
 4	b=b+a(jj,k)*x(k)
      if(abs(a(jj,jj)).lt.1.e-30) write(6,100)
	x(jj)=(a(jj,mm)-b)/a(jj,jj)
 5	continue
 100	format(3x,'things are getting singular in mats')

	return
	end
	subroutine read_disk(file_name,buff,bytes,dk_ptr,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4     istatus
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
	integer copen_r,ccreate,cwrite,cread,ch_fd
	integer n_bytes,bytes,dk_ptr
	character*60 file_name
	byte buff(*)
	integer prt_log_str, prt_log_rtn,dummy

	ch_fd=copen_r(file_name)
	if ( ch_fd .le. 0) then
           write(6,*)' open for read ERR',file_name
c          dummy = prt_log_str('SSP Disk open ERROR ')
c          dummy = prt_log_rtn(dummy)
           idummy=printflog(3,'Failed to open '//file_name//'&')
           idummy=printerr(file_name)
	   istatus=ierr_2
           return
	endif 

	n_bytes = cread(ch_fd,buff,bytes,dk_ptr)
	if ( n_bytes .ne. bytes) then 
           write(6,*)' read ERR',file_name
c          dummy = prt_log_str('SSP Disk Read ERROR ')
c           dummy = prt_log_rtn(dummy)
           idummy=printflog(3,'Failed to read from '//file_name//'&')
           idummy=printerr(file_name)
	   istatus=ierr_4
           return
	endif
	call cclose(ch_fd)

	return
	end
        subroutine ebf_to_ac(x,y,z,rlocal_mean,c1,c2,
     *  alpha1,alpha2,alpha3       )
c	--------------------------------------------------------------
C 	Abstract:
C	Converts the body fixed coordinates into the along/cross track
c	coordinates.
c	--------------------------------------------------------------
	implicit none
c	include 'proj_const.inc'
C*****WAYNE******
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
C*****WAYNE******
c	------------------
C	PASSING IN PARAMETERS
c	------------------
	real*8	x,y,z,rlocal_mean			!position in ebf	

c	------------------
C	PASSING OUT PARAMETERS
c	------------------
	real*8 c1, c2			!A/C coordinate

c	-------------
c	LOCAL VARIABLE
c	-------------
	real*8 r,qlat,qlon

	call ebf_to_qsph(x,y,z,qlat,qlon,alpha1,alpha2,alpha3,rlocal_mean)
	call qsph_to_ac(qlat,qlon,rlocal_mean,c1,c2)

	return
	end
	subroutine write_disk(file_name,buff,bytes,dk_ptr,istatus)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4     istatus
C*****WAYNE******
	integer copen_w,ccreate,cwrite,cread,ch_fd
	integer n_bytes,bytes,dk_ptr
	character*60 file_name
	byte buff(*)
	integer prt_log_str, prt_log_rtn,dummy

	stat=0
	ch_fd=copen_w(file_name)
	
	if ( ch_fd .le. 0) then
           write(6,*)' open for write ERR',file_name
c          dummy = prt_log_str('SSP Disk open ERROR ')
c          dummy = prt_log_rtn(dummy)
           idummy=printflog(3,'Failed to open '//file_name//'&')
           idummy=printerr(file_name)
	   istatus=ierr_2
           return
	endif 

	n_bytes = cwrite(ch_fd,buff,bytes,dk_ptr)

	if ( n_bytes .ne. bytes) then
           write(6,*)' write ERR',file_name
c         dummy = prt_log_str('SSP Disk Write ERROR ')
c         dummy = prt_log_rtn(dummy)
          idummy=printflog(3,'Failed to write to '//file_name//'&')
           idummy=printerr(file_name)
  	   istatus=ierr_3
           return
	endif

	call cclose(ch_fd)

	return
	end
     	subroutine ll_to_ebf(lat,lon,x,y,z)
	implicit real*8 (a-h,o-z)
	real*8 x,y,z,lat,lon
C*****WAYNE******
       include 'ssp2_const.inc'
C*****WAYNE******
c	include 'key_const.inc'

	r_local = re*rp/sqrt((rp*cosd(lat))**2+(re*sind(lat))**2)
	x = r_local * cosd(lon) * cosd(lat)
	y = r_local * sind(lon) * cosd(lat)
	z = r_local * sind(lat)

	return
	end
	SUBROUTINE LSF(X,Y,N,COEF,M,C)

C  THIS ROUTINE DOES A LEAST SQUARES FIT OF THE DATA Y(X) TO THE 
C  POLYNOMIAL COEF(1) + COEF(2)*X + COEF(3)*X**2 + ... + COEF(M)*X**(M-1)
C  if the data has a large constant term, subtract it off, since it
C  affects the accuracy of the result.

	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 X(N),Y(N),C(M,M+1),COEF(M)

	   DO 1 J=1,M
	COEF(J)=0.0
	   DO 1 I=1,M+1
 1	C(J,I)=0.0

	   DO 2 I=1,N
	X1=X(I)
	IF(X1.EQ.0.0) X1=1.D-50

C  SET UP N LEAST SQUARES EQUATIONS

	X1_JM1=1
	   DO 3 J=1,M
c	C(J,M+1)=C(J,M+1)+Y(I)*X1_JM1
	C(J,M+1)=C(J,M+1)+(Y(I)-Y(N/2))*X1_JM1   !Mod by M. Jin 4/22/94
	X1_JM1_KM1=X1_JM1
	X1_JM1=X1_JM1*X1
	   DO 3 K=1,M
	C(K,J)=C(K,J)+X1_JM1_KM1
	X1_JM1_KM1=X1_JM1_KM1*X1
 3	CONTINUE
 2	CONTINUE

C  SOLVE N EQUATIONS

	CALL DMATS(C,COEF,M)
	COEF(1) = COEF(1)+Y(N/2)	!Add by M. Jin 4/22/94

	RETURN
	END
	subroutine v_poly(x1,x0,coef,m,y)
	implicit real*8 (a-h,o-z)
	real*8 x0,x1,x,coef(m),y	

	x = x1-x0
	y = coef(m)
	do k = 1, m-1
	y = y*x+coef(m-k)
	end do

	return
	end
        function sinc(a,pi)
        real a,sinc
        real*8 pi

        if(a.eq.0) then
        sinc = 1.
        else
        b = a*pi
        sinc = sin(b)/b
        end if

        return
        end
	subroutine rotate_z(x,y,z,theta0,sign)

C	--------------------------------
C 	Abstract:
C		Performs coordinate rotation about the Z axis
C		by an angle of theta0.
C	--------------------------------
	implicit none

c	----------
c	PASSING INPUT/OUTPUT PARAMETERS
c	----------
	real*8 x, y, z		

c	----------
c	PASSING INPUT PARAMETERS
c	----------
	real*8 theta0
	real*4 sign

c	----------
c	LOCAL VARIABLES
c	----------
	real*8 theta
	real*8 x1, y1

	theta=theta0*sign
	x1=x*cosd(theta)+y*sind(theta)
	y1=-1*x*sind(theta)+y*cosd(theta)

c	z=z
	x=x1
	y=y1

	return
	end
	subroutine rotate_y(x,y,z,theta0,sign)

c	---------------------
C	Abstract:
C	Performs coordinate rotation about the Y axis by an angle of theta.
c	---------------------
	implicit none

c	----------
c	PASSING INPUT/OUTPUT PARAMETERS
c	----------
	real*8 x, y, z		

c	----------
c	PASSING INPUT PARAMETERS
c	----------
	real*8 theta0
	real*4 sign

c	----------
c	LOCAL VARIABLES
c	----------
	real*8 theta
	real*8 x1, z1

	theta=theta0*sign
	z1=z*cosd(theta)+x*sind(theta)
	x1=-1*z*sind(theta)+x*cosd(theta)

c	y=y
	x=x1
	z=z1

	return
	end
	subroutine ac_to_qsph(c1,c2,rlocal,qlat,qlon)

c	-------------------------------------------
C 	Abstract:
C    	   Converts the spacecraft position from the
C	   the projection coordinate into quasi spherical coordinate.
c	-------------------------------------------
	implicit none

c	------------------
C	PASSING IN PARAMETERS
c	------------------
	real*8 c1, c2		!Projection coordinates
	real*8 rlocal		!Earth radius

c	------------------
C	PASSING OUT PARAMETERS
c	------------------
	real*8 qlat, qlon	!Quasi-spherical angles lat. lon.

c	------------------
C	LOCAL VARIABLE
c	------------------
	real*8 pi

	pi=atan(1.d0)*4.d0

	qlat=-1*(c2/rlocal)*(180.d0/pi)	!-1 is right looking 
	qlon=c1/(rlocal*cosd(qlat))*(180.d0/pi)

	return
	end
	subroutine qsph_to_ac(qlat,qlon,rlocal,c1,c2)

c	-----------------------------------------------
c	Abstract:
C	   Converts the spacecraft position from the quasi
C	   spherical coordinates into the projection coordinates
c	-----------------------------------------------
	implicit none

c	----------------
C	PASSING IN PARAMETERS
c	----------------
	real*8	qlat			!Quasi-latitude
	real*8	qlon			!Quasi-longitude
	real*8	rlocal			!Earth radius

c	----------------
C	PASSING OUT PARAMETERS
c	----------------
	real*8	c1			!Projection coordinate c1
	real*8	c2			!Projection coordinate c2

c	----------------
C	LOCAL VARIABLES
c	----------------
	real*8 pi
	real*8 d_qlat

	pi = atan(1.d0)*4.d0

	c1 = (qlon)/180.d0*pi*rlocal*cosd(qlat)	!Oblique
	c2 = -1*qlat*rlocal*pi/180.d0		!-1 is right looking

	return
	end
	subroutine ebf_to_qsph(x,y,z,qlat,qlon,
     *  alpha1,alpha2,alpha3,rlocal_mean       )
c	--------------------------------------------------
c	Abstract:
C	   Converts the earth body fixed coordinates into the 
C	   quasi spherical coordinate. 
c	--------------------------------------------------
	implicit none
c	include 'proj_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       real*8     rlocal_mean
C*****WAYNE******
c	----------------
C	PASSING IN PARAMETERS
c	----------------
	real*8	x,y,z				!Sensor position	

c	----------------
C	PASSING OUT PARAMETERS
c	----------------
	real*8	qlat			!Quasi-latitude
	real*8	qlon			!Quasi-longitude

c	----------------
C	LOCAL VARIABLES
c	----------------
	real*8 x_t, y_t, z_t
	real*8 r

	x_t=x
	y_t=y
	z_t=z

	call rotate_z(x_t,y_t,z_t,alpha1,1.)
	call rotate_y(x_t,y_t,z_t,alpha2,1.)
	call rotate_z(x_t,y_t,z_t,alpha3,1.)

	qlat=asind(z_t/rlocal_mean)
	qlon=atan2d(y_t,x_t)

	return
	end
	subroutine ebf_to_ll(xt,yt,zt,lat,lat_d,lon)
	implicit real*8 (a-h, o-z)
	real*8 lat,lon,lat_d
C*****WAYNE******
       include 'ssp2_const.inc'
C*****WAYNE******
c	include 'key_const.inc'
	
        lat = asind(zt/sqrt(xt**2+yt**2+zt**2))
c	lat = atan2d(zt,sqrt(xt**2+yt**2))	
	lat_d = atand(tand(lat)/(1-ecc_e**2))
	lon = atan2d(yt,xt)
	r_local = re*rp/sqrt((rp*cosd(lat))**2+(re*sind(lat))**2)
	rr = sqrt(xt**2+yt**2+zt**2)

	return
	end
	subroutine dmats(a,x,n)
	implicit real*8 (a-h,o-z)
	real*8 a(n,n+1),x(n)

c  This routine does a gauss-jordan elimination and back substitution
c  to solve the matrix equation ax=b for x, where b is stored in
c  the (n+1)TH column of a

	mm=n+1
	   do 3 i=2,n
	ii=i-1
	   do 3 j=1,ii
	caij=abs(a(i,j))
      if(caij.eq.0) go to 3
      if(caij.gt.abs(a(j,j))) then
	r=a(j,j)/a(i,j)
	   do 1 k=1,mm
	b=a(j,k)
	a(j,k)=a(i,k)
 	a(i,k)=b
 1	continue
      else
	r=a(i,j)/a(j,j)	    
      endif
	jj=j+1
	   do 2 k=jj,mm
 2	a(i,k)=a(i,k)-r*a(j,k)
 3	continue
      if(abs(a(n,n)).lt.1.d-50) write(6,100)
	x(n)=a(n,mm)/a(n,n)
	   do 5 i=2,n
	jj=n-i+1
	ii=jj+1
	b=0.d0
	   do 4 k=ii,n
 4	b=b+a(jj,k)*x(k)
      if(abs(a(jj,jj)).lt.1.d-50) write(6,100)
	x(jj)=(a(jj,mm)-b)/a(jj,jj)
 5	continue
 100	format(3x,'things are getting singular in mats')

	return
	end
