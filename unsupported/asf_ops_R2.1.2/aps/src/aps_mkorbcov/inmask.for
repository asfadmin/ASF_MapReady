
*.
*   InMaskR - find out if an orbiter at rPlat in view of a ground station
*
*   Args
*	rPlat(3)	double	input	Cartesian position vector (km)
*	istat		integer	input	station number (from INITMASK)
*
*   Return
*	InMaskR		logical
*
*   History
*	 3/25/91 Use new IJKtoSEZ routines
*	12/ 1/88
*
*   Last modified
*	3/25/91 14:17
*..
	logical function InMaskR(rPlat,istat)

	include 'eosinclude:constants.inc'
	include 'eosinclude:station.inc'

      character*100 SccsFileID
     -/'@(#)inmask.for	5.1 98/01/08 APS/ASF\0'/
	double precision rplat(3)
	integer istat

	double precision rhoSEZ(3)
	double precision az,el

	double precision raz
	integer iaz,iaz1,iaz2
	double precision el1,el2,elInterp


	call IJKtoSEZ(tmStation(1,1,istat),rPlat,rStation(1,istat),
     .			rhoSEZ,az,el)

	az = az / rads
	el = el / rads

	if(el .lt. MinElev(istat)) then

	    InMaskR = .false.

c	    write(4,*) '           el:',el

	else if(el .ge. MaxElev(istat)) then

c	    write(4,*) '   el,MaxElev: ',el,MaxElev(istat)

	    InMaskR = .true.

	else if(el.ge.MinElev(istat)) then

	    if(az .lt. 0) az = az + 360.0d0

	    raz = (az/360.0d0) * AzSteps(istat) + 1.0
	    iaz = int(raz)
	    if(raz.eq.iaz) then
		iaz1 = iaz
		iaz2 = iaz
	    else
		iaz1 = iaz
		iaz2 = iaz + 1
	    end if

	    if(iaz2 .gt. AzSteps(istat)) iaz2 = 1

	    el1 = MaskElev(iaz1,istat)
	    el2 = MaskElev(iaz2,istat)
	    elInterp = (el2-el1)*(raz-iaz1) + el1

	    InMaskR = (el.ge.elInterp)

c	    write(4,*) '        az,el:',az,el
c	    write(4,*) 'iaz1,raz,iaz2:',iaz1,raz,iaz2
c	    write(4,*) '        elevs:',MaskElev(iaz1,istat),elInterp,
c     .						MaskElev(iaz2,istat)
	end if

c	if(InMaskR) then
c	    write(4,*) '  *** In Mask ***'
c	else
c	    write(4,*) '*** Not In Mask ***'
c	end if

	end


	logical function InMask(latd,lon,altd,istat)
** Args
	double precision latd,lon,altd
	integer istat

	double precision rPlat(3)

** Functions
	double precision InMaskR

	call SphereXYZ(latd,lon,altd,rPlat)
	InMask = InMaskR(rPlat,istat)

	end
