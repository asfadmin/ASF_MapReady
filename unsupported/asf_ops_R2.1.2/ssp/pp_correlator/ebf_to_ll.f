c SccsId = @(#)ebf_to_ll.f	2.41 3/24/98




	subroutine ebf_to_ll(xt,yt,zt,lat,lat_d,lon)

        implicit none

        character*128 SccsId_ebf_to_ll
        data SccsId_ebf_to_ll
     +  /'@(#)PPebf_to_ll.f:2.41'/


	real*8 lat,lon,lat_d

	real*8 ecc_e,rp,rr,r_local,yt,xt,re,zt

	re = 6378144.d0
	rp =  6356755.d0
	ecc_e = 8.1827385e-2
	
	lat = asind(zt/sqrt(xt**2+yt**2+zt**2))
	lat_d = atand(tand(lat)/(1-ecc_e**2))
	lon=atan2d(yt,xt)
	r_local = re*rp/sqrt((rp*cosd(lat))**2+(re*sind(lat))**2)
	rr = sqrt(xt**2+yt**2+zt**2)

	return
	end
