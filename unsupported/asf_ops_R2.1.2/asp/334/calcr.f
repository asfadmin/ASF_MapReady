C Alaska SAR Processor (ASP) %W% %E% %U%
	function calcr(xsc,gamma,asquint)
c/*	function calcr(xsc,gamma,asquint)   --------------
c
c  Dec 87
c
c	aug 88   updated to double precision
c
c  extracted from getlook.for to calcr.for
c  file july 88 and used by other routines
c  as means for calculating range.  
c
c  Originally internal function to getlook.  Calculate range from s/c to touch
c  point
c  defined by earth surface (ellipsoid) and SAR angles gamma and asquint
c  gamma and asquint are in radians. xsc(3) is s/c position in m.
c
c  Error condition:  d < 0 indicates rage vector does not intersect Earth.
c                    In this case, a value of 0 is returned for calcr.
c*/
	implicit double precision(a-h,o-z)
	double precision xsc(3),rvec(3),rt(3)
	common /earth/ rp,re
	common /transform/ s
	rvec(1)= dsin(asquint)
	rvec(2)=-dsin(gamma)*dcos(asquint)
	rvec(3)=-dcos(gamma)*dcos(asquint)           
c   rvec points to target.  Translate into earth centered unit vector:
       	call matmul(s,rvec,rt) 
c   put rt into correct rvec position
	rvec(1)=rt(1)
	rvec(2)=rt(2)                   
	rvec(3)=rt(3)
c 
c   calculate range to intercept earth ellipsoid
c                               
	re2=re**2                                   
	rp2=rp**2         
	a=(rvec(1)**2+rvec(2)**2)/re2 + rvec(3)**2/rp2
	b=2.*((xsc(1)*rvec(1) + xsc(2)*rvec(2))/re2 + xsc(3)*rvec(3)/rp2)
	c=(xsc(1)**2+xsc(2)**2)/re2 + xsc(3)**2/rp2 -1.
c  quadradic formula...save near range point (first Earth intersection).
	d=(b**2-4.*a*c) 
	if (d.lt.0) then ! range vector does not hit Earth surface.
		calcr=0.
		return 
	end if
	ans1=(-b + dsqrt(d))/(2.*a)
	ans2=(-b - dsqrt(d))/(2.*a)
	calcr=dmin1(ans1,ans2)
        return
	end
  
