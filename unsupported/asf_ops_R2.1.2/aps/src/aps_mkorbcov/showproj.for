*.
*   ShowProj - show map projection
*
*	06/12/90
*
*	06/12/90 15:39
*..
	subroutine ShowProj

	 character*100 SccsFileID
     -/'@(#)showproj.for	5.1 98/01/08 APS/ASF\0'/
	include 'eosinclude:map.inc'

	integer i

	if(proj.lt.1) return

	write(*,'(/,2a)') ' Projection is ',ProjName(proj)

	if(proj.eq.CYL .or. proj.eq.MERC .or. proj.eq.MILL) then

	    write(*,'(a,2(f8.3,f9.3))')
     .			'   Window lat/lons',(ProjArg(i),i=1,4)

	else if(proj.eq.LAMB) then

	    write(*,'(a,2(f8.3,f9.3))')
     .			'   Window lat/lons',(ProjArg(i),i=1,4)
	    write(*,'(a,2f8.3)') '   Parallels',(ProjArg(i),i=5,6)

	else if(proj.eq.NPOLE .or. proj.eq.SPOLE) then

	    write(*,'(a,f8.3)') '   Outer latitude',ProjArg(1)

	else if(proj.eq.GLOBE) then

	    write(*,'(a,f8.3,f9.3)')
     .			'   Center lat/lon',(ProjArg(i),i=1,2)
	    write(*,'(a,f8.3)') '   Axis tilt',ProjArg(3)
	    write(*,'(a,f8.3)') '   Zoom',ProjArg(4)

	else

	    write(*,'()') ' No projection information available'

	end if

	end
