
*.
*   MapGrid - draw a lat/lon grid on the map
*
*   Args
*	indlatd,indlon	double	input	lat/lon spacing (deg)
*	col		integer	input	DN/line style to draw with
*
*   Last modified
*	02/10/89 13:20
*..
	subroutine MapGrid(indlatd,indlon,col)
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'	! aap..11/4/93
	double precision indlatd,indlon
	integer col

	  character*100 SccsFileID
     -/'@(#)mapgrid.for	5.1 98/01/08 APS/ASF\0'/
	character*50 comment

	double precision dlatd,dlon
	double precision latd,lon

	dlatd = indlatd
	dlon = indlon

	if(dlatd.eq.0.0 .and. dlon.eq.0.0) go to 999
	if(dlatd.ne.0.0 .and. dlon.eq.0.0) dlon = dlatd

	dlatd = abs(dlatd)
	dlon = abs(dlon)

	write(comment,'(a,f4.1,a,f5.1,a)') 'Grid (',dlatd,' x ',dlon,')'
	call GrafComment(comment)

	call UseCol(col)

	if(.not.RectProj) then

*	  Meridians
	    do lon=0.0,180.0d0-dlon,dlon
c..aap		call ArcSphere(0.0,lon,90.0d0)
		call ArcSphere(dzero,lon,d90)
	    end do

*	  Parallels
*	  Only do equator once!
c..aap	    call ArcSphere(90.0d0,0.0,90.0d0)
	    call ArcSphere(d90,dzero,d90)

*	  Northern hemisphere
	    if(proj.ne.SPOLE) then
		do latd=dlatd,90.0d0,dlatd
c..aap		    call ArcSphere(90.0d0,0.0,90.0d0-abs(latd))
		    call ArcSphere(d90,dzero,d90-abs(latd))
		end do
	    end if

*	  Southern hemisphere
	    if(proj.ne.NPOLE) then
		do latd=-90.0d0,-dlatd,dlatd
c..aap		    call ArcSphere(-90.0d0,0.0,90.0d0-abs(latd))
		    call ArcSphere(dneg90,dzero,d90-abs(latd))
		end do
	    end if

	else

	end if

999	continue
	end
