	subroutine eosmenu(filename,slen,plat,projection,lat1,lon1,lat2,lon2,instrument,duration,smin,ssec,yawang)
	character	projection
	double precision	lat1,lon1,lat2,lon2
	character*80	instrument,filename,duration,plat
	integer	smin,ssec,slen,yawang

	  character*100 SccsFileID
     -/'@(#)eosmenu.for	5.1 98/01/08 APS/ASF\0'/


*	program EosMenu_137
*.
*   EosMenu v1.37 - Instrument coverage display
*
*   History
*	06/05/91 1.37 
*	03/06/91 1.36 Use new masking in INSTRUMENTS.INC
*	06/11/90 1.35 Ask for platform data file
*	04/20/90 1.34 Reorganized menus, consolidated MenuInst* routines
*	04/17/90 1.33 Changed HardCopy, Annot*, Print* routines
*	12/03/89 1.32 Changed ReadPlat to ReadPlatFile/ReadPlatLine
*	12/03/89 1.31 Changed ReadInst to ReadInstFile/ReadInstLine
*	08/07/89 1.3  Converted most REAL variables to DOUBLE PRECISION finally
*	07/26/89 1.27 Added reading platform file
*	07/21/89 1.26 Fixup
*	04/12/89 1.25 Improved station masking
*	03/31/89 1.21 Added station masking
*	03/22/89 1.2  Added simul coverage display
*	03/08/89 1.1
*	10/28/88 1.0  Original
*
*	04/20/90 16:04
*..

** Includes
	include 'eosinclude:files.inc'
	include 'eosinclude:display.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:instruments.inc'
	include 'eosinclude:eossimul.inc'
	include 'eosinclude:constants.inc'	! aap..11/4/93

** Variables

* Menu
	integer MaxMenu
	parameter(MaxMenu=13)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
*	integer iMenu
	integer itemDisp,itemCBody,itemEpoch,itemMap
	integer itemPlat,ItemOrbit,itemInst,itemFly
	integer itemSimul,itemGStation,itemSpecial
	integer itemHelp,itemQuit
	parameter (itemDisp=1,itemCBody=2,itemEpoch=3,itemMap=4)
	parameter (itemPlat=5,itemOrbit=6,itemInst=7,itemFly=8)
	parameter (itemSimul=9,itemGStation=10)
	parameter (itemSpecial=11)
	parameter (itemHelp=MaxMenu-1,itemQuit=MaxMenu)
*	logical done

* Epoch
	double precision gmt

* Projection
	double precision arg(NUMPROJARG)

*	character YesNo

** Data
	data MenuI /'Display','Central Body','Epoch','Map',
     .		'Platform','Orbit','Instruments','Fly',
     .		'Simultaneous coverage','Ground stations',
     .		'Special','Help','Quit'/
	data MenuC /'D','C','E','M','P','O','I',
     .		'F','S','G','Z','?','Q'/

** Program

* Inits

*	write(*,'(/,a,/)') ' *** EosMenu 1.37 - Daren Casey 1991 ***'

	call InitFiles
*	call InitDisp

c..aap	call idhmstos(91,15,20,0.0d0,gmt)
	call idhmstos(i91,i15,i20,dzero,gmt)
c..aap	call InitEpoch(1995,gmt)
	call InitEpoch(i1995,gmt)
	call InitBody(EARTH)

*	arg(1) = 90.0d0
*	arg(2) = -180.0d0
*	arg(3) = -90.0d0
*	arg(4) = 180.0d0
*	call SetProj(CYL,arg)

	NumPlat = 0

c	write(*,'(/,a)') ' Reading platform data ...'
c	write(*,'(/,a)') ' Reading instrument data ...'
c	call ReadInstFile(' ')

*	ShowSimul = .false.

* Main loop

		call ReadPlatFile(plat)

		arg(1) = lat1
		arg(2) = lon1
		arg(3) = lat2
		arg(4) = lon2
		call GetProj(projection,arg)

		call MenuInst(7,instrument,slen)

		call MenuInst(2,instrument,slen)

		if(projection.eq.'N' .or. projection.eq.'S') then

			call MenuInst(3,instrument,yawang)
		end if

		call MenuFly(filename,duration,smin,ssec)


900	    continue

	end
