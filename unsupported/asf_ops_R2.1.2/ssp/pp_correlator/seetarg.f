c SccsId = @(#)seetarg.f	2.41 3/24/98


	subroutine seetarg(h,xs,ys,zs,px,py,pz,xt,yt,zt,r_local)

        implicit none

        character*128 SccsId_seetarg
        data SccsId_seetarg
     +  /'@(#)PPseetarg.f:2.41'/


	include '/usr/lpp/ppe.poe/include/mpif.h'

	real*8 lat, lat_d, ierror

	real*8 ratio,re1,r,rp,ecc_e,rp1,con,rst,cc,a,b,re,zs,px,ys,h
	real*8 xs,py,zt,r_local,yt,pz,xt

	re = 6378144.d0
	rp =  6356755.d0
	ecc_e = 8.1827385e-2

	lat = atan2d(zs,sqrt(xs**2+ys**2))
	lat_d = atand(tand(lat)/(1-ecc_e**2))

	r = 1./sqrt(cosd(lat)**2/re**2+sind(lat)**2/rp**2)
	ratio = (r+h/cosd(lat-lat_d))/r
	re1 = re*ratio
	rp1 = rp*ratio
	a = (px**2+py**2)/re1**2+pz**2/rp1**2
	b = (2*px*xs+2*py*ys)/re1**2+2*pz*zs/rp1**2
	cc = (xs**2+ys**2)/re1**2+zs**2/rp1**2-1
	con =b**2-4*a*cc
	if(con.lt.0) then 
	   print *, 'cannot see a target. b**2-4*a*c ', con 
	   call mpi_abort(mpi_comm_world,22,ierror)
	   call exit(22)
	end if
	rst=(-b-sqrt(con))/(2*a)
	xt=xs+px*rst
	yt=ys+py*rst
	zt=zs+pz*rst
	r_local = sqrt(xt**2+yt**2+zt**2)
	
	return
	end




