C Alaska SAR Processor (ASP) %W% %E% %U%
 	subroutine mksl2gr (rscs,rslant0s,rearths,srincs,grincs,sr2gr)
c/*     subroutine mksl2gr (rscs,rslant0s,rearths,srincs,grincs,sr2gr) -----
c
c	SUBROTINE MKSL2GR       AUTHOR R.E.CARANDE   NOV 88
c
c   This subroutine constructs the slantrange to groundrange interpolation
c   vector.
c
c	Input:	    rsc = distance from Earth center to SAR in meters 
c			  (real*4)
c               rslant0 = slant range distance to first interpolation point.
c			  This may be a point different from the slant range
c			  to the first sampled point due to range migration.
c			  Units = meters. (real*4)
c		 rearth = radius of Earth at center swath (mid range) in meters.
c		          (real*4)
c		  srinc = slant range increment in meters (real*4). This
c			  determined by the SAR's A/D sampling rate.
c		  grinc = ground range increment in meters (real*4)
c			  For ASF = 12.5meters
c                          
c	Output:    sr2gr = 8k (real*4) vector that contains the interpolation 
c			  points for slant range to ground range conversion.  
c			  The first element is always 0, which means the first
c			  interpolation point is at rslant0.  This vector
c			  is in units of slant range bins.
c			 
c       Algorithm:  The local Earth surface (within image) is approximated
c		    by a sphere of radius rearth.  This radius may be the
c		    local Earth radius as determined by a higher order model,
c		    such as an ellipsoid.  (Deviations between the elliptical
c		    surface and the sphere, for the purpose of this resampling,
c		    are small.)  The following equations are used to determine
c		    the resampling vector sr2gr and are inverses of each other:
c             
c   		    slant range as a function of ground range:
c
c	rground(rslant) = rearth*acos( (1+(1+a)**2 - (rslant/rearth)**2)/
c			  (2*(1+a))  )
c
c		    ground range as a function of slant range:
c
c	rslant(rground) = rearth*sqrt(2(1+a)*(1-cos(rground/rearth) )+a*a)   
c
c		where a = (rsc-rearth)/rearth
c                                                     
c		    First the ground range at rslant0 is determined from
c		    the first equation above. This ground range, plus 
c		    the enumerated increment (grinc) is used to calculate
c		    the slant range to the interpolation points.  Finally
c		    the interpolation points are normalized to slant range
c		    bins by dividing by the slant range bin size, rsinc.
c
c	NOTE:  ALL INPUT VARIABLES ARE CONVERTED TO DOUBLE PRECISION
C	       AND ALL CALCULATIONS ARE PERFORMED IN DOUBLE PRECISION.
C	       THE FINAL SL2GR INTERPOLATION VECTOR IS CONVERTED TO SINGLE
C	       PRECISION.  IN OTHER WORDS, ALL VARIABLES PASSED TO AND FROM
C	       THIS SUBROUTINE ARE SINGLE PRECISION.
c                                                                     
c*/
	implicit double precision (a-h,o-z)
	real*4 sr2gr(8192),rscs,rslant0s,rearths,srincs,grincs
c
c  convert to double internal variables
	rsc=dble(rscs)
	rslant0 = dble(rslant0s)
	rearth = dble(rearths)
	srinc=dble(srincs)
	grinc=dble(grincs)

	a = (rsc-rearth)/rearth
c            
c   calculate g.r. to first point
c
	rg0 = rearth*dacos( (1.+(1.+a)**2 - 
     .(rslant0/rearth)**2)/(2.*(1.+a)) ) 
c
c   begin loop
c                                     
	do i=1,8192
	TEMP = rearth*dsqrt(2.*(1.+a)*(1.-dcos(((i-1)
     .*grinc+rg0)/rearth))+a*a)
	sr2gr(i) = SNGL((TEMP- rslant0)/srinc)
	end do                                                
	sr2gr(1)=( sr2gr(1) + sr2gr(2) ) -sr2gr(2)! make equal to zero
	return
	end              
                                             
