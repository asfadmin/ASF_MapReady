c SccsId = @(#)inc_ang.f	2.41 3/24/98


	subroutine inc_ang(alkang,xs,ys,zs,xt,yt,zt,thi_inc)

        implicit none

        character*128 SccsId_inc_ang
        data SccsId_inc_ang
     +  /'@(#)PPinc_ang.f:2.41'/


	real*8 lat,lat_d
	
        real*8 rp
        real*8 re
        real*8 thi_inc
        real*8 rt
        real*8 rs
        real*8 ecc_e
        real*8 zt
        real*8 ys
        real*8 xs
        real*8 alkang
        real*8 yt
        real*8 xt
        real*8 zs

	re = 6378144.d0
	rp =  6356755.d0
	ecc_e = 8.1827385e-2

	rs = sqrt(xs**2+ys**2+zs**2)
	rt = sqrt(xt**2+yt**2+zt**2)
	thi_inc = asind((rs/rt)*sind(alkang))
	lat = atand(zt/sqrt(xt**2+yt**2))
	lat_d = atand(tand(lat)/(1-ecc_e**2))	
	thi_inc = thi_inc -(lat_d-lat)

	return
	end
