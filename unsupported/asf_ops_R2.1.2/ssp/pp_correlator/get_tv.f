c SccsId = @(#)get_tv.f	2.41 3/24/98


	subroutine get_tv(lat,lon,lat_d,h,
     1			xt,yt,zt,vxt,vyt,vzt)

        implicit none

        character*128 SccsId_get_tv
        data SccsId_get_tv
     +  /'@(#)PPget_tv.f:2.41'/


	real*8 lat,lon,lat_d,h

        real*8 pi 
        real*8 rn
        real*8 ecc_e
        real*8 omega_e
        real*8 u2
        real*8 u3
        real*8 rxyt
        real*8 u1
        real*8 zt
        real*8 vxt
        real*8 xt
        real*8 yt
        real*8 re
        real*8 rp
        real*8 vyt
        real*8 vzt


	re = 6378144.d0
	rp =  6356755.d0
	ecc_e = 8.1827385e-2
        omega_e = 360.9856296d0 / (24.0d0 * 3600.0d0 )
	pi = 4.*atan(1.)
        omega_e = omega_e * pi / 180.

	rn = re/sqrt(1-(ecc_e*sind(lat_d))**2)
	xt = (rn+h)*cosd(lon)*cosd(lat_d)
	yt = (rn+h)*sind(lon)*cosd(lat_d)
	zt = (rn*(1-ecc_e**2)+h)*sind(lat_d)
	rxyt = sqrt(xt**2+yt**2)
	call cross(0.d0,0.d0,1.d0,xt,yt,zt,u1,u2,u3)
	vxt = omega_e*rxyt*u1
	vyt = omega_e*rxyt*u2
	vzt = omega_e*rxyt*u3
	
	return
	end
