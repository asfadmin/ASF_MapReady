c SccsId = @(#)propag_init.f	2.41 3/24/98

c****************************************************************
c       Jet Propulsion Laboratory
c       Project: ASF RadarSAT
c       Task:    ScanSAR
c       Code:    Fortran 77
c------------------------------------------------------------------
c
c       8/31/94 A.Chu Modified for ScanSAR
c------------------------------------------------------------------
	subroutine propag_init(r_x,r_y,r_z,v_x,v_y,v_z,kepler)

c     *                         GE,RE,RATE,PM,ELLIP,RATM)
c	---------------------
c	Abstract:
c	        1. Convert state vector to Kepler coordinate
c	        2. Set up flags & coefficients.
c	---------------------
c	
C    ALL THE UNIT CONVERSIONS SHOULD BE MADE IN THIS DRIVER
C    PROGRAM SO THAT ALL SUBROUTINES WOULD BE CONSISTENT IN UNITS. 
C   

	implicit none

        character*128 SccsId_propag_init
        data SccsId_propag_init
     +  /'@(#)PPpropag_init.f:2.41'/


	real*8 gs, csrp, cdrag, gm, et, es, wt, altmax, sht, 
     +    scmass, areas, aread, dts, dtr, hlarge, hstart, 
     +    zero, c, en, em, abserr, relerr, s, rht, aj2, pm, 
     +    rate, rdens, ratm, ellip, tf, ti,  
     +    re, ge, tr

	integer neq, isrp, idens, iplot,
     +    idrag, inode, iprint, iorb, ires, m, l, iephem, imoon, 
     +    isun

	real*8 r_x,r_y,r_z,v_x,v_y,v_z
	real*8 kepler(6)
	integer ii,jj

      	real*8 ORB(6),Y(6),X(6)
      	real*8 TINT(2),TFIN(2),TREF(2)
      	COMMON/OPTION/L,M,IRES,ISUN,IMOON,IEPHEM,IDRAG,IDENS,ISRP,IORB
     1  ,IPRINT,INODE,IPLOT
      	COMMON/ATIME/TI,TF,TR
      	COMMON/PLTCON/GE,RE,RATE,PM,AJ2,ELLIP,RATM
      	COMMON/ATMCON/RDENS,RHT,SHT,ALTMAX,WT
      	COMMON/SPCCON/AREAD,AREAS,SCMASS,CDRAG,CSRP
      	COMMON/SUNCON/GS,ES(7),ET(7)
      	COMMON/MUNCON/GM,EM(7),EN(7)
      	COMMON/HARMON/C(41,41),S(41,41)
      	COMMON/accurcy_err/relerr,abserr
      	EXTERNAL DER
      	DATA NEQ/6/
      	DATA DTR,DTS/.1745329251994330D-1,8.64D4/
      	DATA ZERO/0.D0/
      	DATA HSTART,HLARGE/60.D0,1.D99/

	call eme_kepler (r_x,r_y,r_z,v_x,v_y,v_z,kepler)

	L=20	!Mean value, (2)
	M=20	!Mean value, (0)
	IRES=0	!1=Resonance effect in Tesserals,not affect in zonal term
	ISUN=0  !No Solar Gravity
	IMOON=0 !No Lunar gravity
	IEPHEM=1!Earth orbiting spacecraft using earth mean equator
	IDRAG=0 !No Air Drag
	IDENS=1	!Use static 1977 Earth Model
	ISRP=0	!No Solar Radiation Pressure
	IORB=0	!Orbit element are osculating value
	IPRINT=0!Print at constant step as specified by step,1=periapsis
	INODE=0 !No Print nodal crossing info
	IPLOT=0 !No plotting
	RELERR=1.d-12	!Relative accuracy of the integration
	ABSERR=1.d-12	!Absolute accuracy of the integration
        GE = 3.9860045D5 !EARTH gmass KM**3/SEC**2 (JPL DE118)
cc	GE=GE/1.d9	!KM**3/SEC**2
        RE= 6378.140D0 !FOR EARTH(KM)
        RATE= 4.178074216D-3 !FOR EARTH (deg/sec)
	PM=99.652865509d0 !Prime meridian relative to the inertial
			  !x-axis at tref (deg)
        ELLIP = .8182D-1  !Ellipticity of the reference ellipsoid to
			  !compute geodetic altitude
        RATM= RE+90 	  !Atmospheric blckage (KM)
        RDENS=0.d0
        RHT=0.d0
        SHT=0.d0
        ALTMAX=1.d0	  !Maximum altitude for Drag perturbation
        WT=1.d0		  !Weight factor to be applied the density
        AREAD=10.d-6	  !s/c area for Drag (km**2)
        AREAS=10.d-6	  !s/c area for solar wind (km**2)
        SCMASS=2000.d0    !Effective S/C mass for the length
			  !propagation (KG)
        CDRAG=2.d0	  !Drag coefficient
        CSRP=6.6d-3       !SRP Constant (kg/km-sec**2)
        GS=.13271244d12	  !Sun gmass
	ES(1)=0.d0        !Sun Ephemeris
	ES(2)=0.d0
	ES(3)=0.d0
	ES(4)=0.d0
	ES(5)=0.d0
	ES(6)=0.d0
	ES(7)=0.d0
        GM=.490279d4	  !Moon gmass
        EM(1)=0.d0
        EM(2)=0.d0
        EM(3)=0.d0
        EM(4)=0.d0
        EM(5)=0.d0
        EM(6)=0.d0
        EM(7)=0.d0
	do ii=1,20
	   do jj=1,20
	   c(ii,jj)=0.d0
	   s(ii,jj)=0.d0
	   enddo
	enddo
	C(3,1)=-.10826271d-2
	S(3,1)=-.10826271d-2
C

	return
	end



