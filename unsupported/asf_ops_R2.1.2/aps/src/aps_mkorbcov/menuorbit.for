
*.
*   MenuOrbit - orbit menu for EosMenu
*
*	07/26/89 11:19
*..
	subroutine MenuOrbit

** Includes
	include 'eosinclude:instruments.inc'
	include 'eosinclude:orbit.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)menuorbit.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=3)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemNew,itemShow,itemHelp
	parameter (itemNew=1,itemShow=2)
	parameter (itemHelp=MaxMenu)
	logical done

	integer iplat,slen
	double precision hp,ha,Q

** Data
	data MenuI /'New orbit','Show orbit','Help'/
	data MenuC /'N','S','?'/

** Functions
	integer StringLen

	if(NumPlat.gt.0) then

	    done = .false.

	    do while(.not.done)

		call MenuInput('Orbit',MenuI,MenuC,MaxMenu,iMenu)

		if(iMenu.eq.itemNew) then

		    call GetPlatNum(iplat)
		    if(iplat.ne.0) then
			call GetElements1(a(iplat),e(iplat),i(iplat),
     .			 node0(iplat),omega0(iplat),m0(iplat),t0(iplat))
		    end if

		else if(iMenu.eq.itemShow) then

		    call GetPlatNum(iplat)
		    if(iplat.gt.0) then
			hp = a(iplat) * (1.0 - e(iplat)) - RBody
			hp = max(0.0d0,hp)
			ha = a(iplat) * (1.0 + e(iplat)) - RBody
			ha = max(0.0d0,ha)
			call FindQ(a(iplat),e(iplat),i(iplat),Q)
			slen = StringLen(PlatName(iplat))
			write(*,10) PlatName(iplat)(1:slen),iplat,
     .				a(iplat),node0(iplat)/rads,
     .				t0(iplat)/3600.0d0,
     .				e(iplat),omega0(iplat)/rads,hp,
     .				i(iplat)/rads,m0(iplat)/rads,ha,Q
10			format(	/,' Orbital elements for platform ',
     .				a,' (#',i1,')',/,
     .				/, t6,'a ',f9.3,' km',
     .				  t26,' node0 ',f9.4,' deg',
     .				  t47,'    t0 ',f9.3,' hr',
     .				/, t6,'e ',f9.7,
     .				  t26,'omega0 ',f9.4,' deg',
     .				  t47,'    hp ',f9.3,' km',
     .				/, t6,'i ',f9.4,' deg',
     .				  t26,'    m0 ',f9.4,' deg'
     .				  t47,'    ha ',f9.3,' km',
     .				/,t47,'     Q ',f9.4,' orbits/day')
			call WaitKey
		    end if

		else if(iMenu.eq.itemHelp) then

		    write(*,'(99(/,a))')
     .' The "New orbit" item is used to set the orbital elements',
     .'   for a platform.',
     .' The "Show orbit" item lists the orbital elements for a',
     .'   selected platform:',
     .' ',
     .'        a - semi-major axis',
     .'        e - eccentricity',
     .'        i - inclination',
     .'    node0 - longitude of ascending node',
     .'   omega0 - argument of periapsis',
     .'       m0 - mean anomaly',
     .'       t0 - time difference between these elements and the',
     .'              reference epoch',
     .'       hp - height at periapsis, or least altitude',
     .'       ha - height at apoapsis, or greatest altitude'
		    call WaitKey

		else if(iMenu.eq.0) then
		    done = .true.
		end if

	    end do

	else

	    write(*,'(2(/,a))')
     .' Please create at least one platform with the top-level',
     .'   menu PLATFORM before setting orbits'
	    call WaitKey

	end if

	end
