c SccsId = @(#)eme_kepler.f	2.41 3/24/98
c**********************************************************************
c*    Module Name:	eme_kepler.f
c	
c---------------------------------------------------------------------
c
c	9/5/94 A.Chu Modified from asp to ScanSAR 
c---------------------------------------------------------------------
	subroutine eme_kepler (rx, ry, rz, vx, vy, vz, kepler)

	implicit none

c	----------------
c	Abstract:
c               Converts the Cartesian State Vector to Kepler elements  
c	----------------
	
c	----------------
c	INPUT PARAMETERS PASSING
c	----------------
	real*8  rx, ry, rz  !Cartesian inertial x,y,z coordinate in KM.
	real*8  vx, vy, vz  !Cartesian inertial coordinates in KM/sec.

	real*8 tanfix	    !function call

c	----------------
c	OUTPUT PARAMETERS PASSING
c	----------------
	real*8   kepler(6)

c 		 the resultant Kepler transformation consisting of the
c		 six Kepler elements,namely, 
c		 'a'   : the semi-major axis in Km
c		 'e'   :the eccentricity
c		 'i'   :angle of inclination (deg)
c		 'Omega': longitude of ascending node (deg.)
c		 'w'   :argument of periapsis (deg.)
c		 'M'   : mean anomaly (deg.)

c	----------------
c	LOCAL VARIABLES
c	----------------
        real*8  V2, H, i, i_deg
        real*8  a, u, r, v, es, ec, ecc, E, M_deg
        real*8  w_deg, cu, su, somega, comega, omega
        real*8  pi, mu

        pi = 3.141592653589793d0
        mu = 3.9860045d+14

c	----------------
c       determine semi major axis 'a' 
c	----------------
    	r = sqrt ((rx * rx) + (ry * ry) + (rz * rz))
    	V2 = (vx * vx) + (vy * vy) + (vz * vz)
    	a = (mu * r) / ((2.0d0 * mu) - (r * V2))

c	----------------
c       determine eccentricity 'e' 
c	----------------
    	es = ((rx * vx) + (ry * vy) + (rz * vz)) / sqrt (mu * a)
    	ec = 1.0 - (r / a)
        ecc = sqrt ((es * es) + (ec * ec))

c	----------------
c       determine mean anomaly'M' 
c	----------------
    	E = 2.0d0 * tanfix ((ecc - ec), es)
        M_deg = (180.0d0 / pi) * (E - es)
    	if (M_deg .lt. 0.0)
     *	   M_deg = M_deg + 360.0d0

c	----------------
c       determine angle of inclination 'i' 
c	----------------
    	H = sqrt (mu * a * (1.0 - (ecc * ecc)))
    	i = acos (((rx * vy) - (ry * vx)) / H)
    	i_deg = i * (180.0d0 / pi)

c	----------------
c       determine omega 
c	----------------
    	somega = ((ry * vz) - (rz * vy)) / (sin (i) * H)
    	comega = ((rx * vz) - (rz * vx)) / (sin (i) * H)
    	omega = (180.0d0 / pi) * 2.0 * tanfix ((1.0 - comega), somega)
    	if (omega .lt. 0.0)
     *	   omega = omega + 360.0d0

c	----------------
c       determine w_deg 
c	----------------
    	su = rz / (r * sin (i))
    	cu = ((ry / r) * somega) + ((rx / r) * comega)
    	if (rz .eq. 0.0)
     *	   cu = 1.0
    	u = 2 * tanfix ((1.0 - cu), su)
    	v =2*atan(sqrt((1.0 +ecc) /(1.0 -ecc))* tan(E / 2.0d0))
    	w_deg = (180.0d0 / pi) * (u - v)
    	if (w_deg .lt. 0.0)
     *	   w_deg = w_deg + 360.0d0


    	kepler(1) = a
   	kepler(2) = ecc
    	kepler(3) = i_deg
    	kepler(4) = omega
    	kepler(5) = w_deg
    	kepler(6) = M_deg

	end



c -------------------------------------------------------
c  Function: tanfix(a,b)
c	This routine calculates the tangent of a/b, protecting
c	against b=0.
c -------------------------------------------------------
 

	real*8 function tanfix(a,b)
	
	implicit  none
    	real*8 a,b,pi

        pi = 3.141592653589793d0

        tanfix=atan2(a,b)
        if (b .eq. 0.d0) then
	    if (a .lt. 0.d0) tanfix=-pi/2.0d0
	    if (a .gt. 0.d0) tanfix=pi/2.0d0
	else if ( a .eq. 0.d0 .and. b .eq. 0.d0)  then
	    tanfix=0.0
        endif  
	
        return 
	end

