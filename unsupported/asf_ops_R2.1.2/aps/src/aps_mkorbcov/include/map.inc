     -/'@(#)map.inc	5.1 98/01/08 APS/ASF\0'/

*.
*   Map.inc - map projection variables
*
*   Commons
*	eos$map
*
*   History
*	07/30/91 Removed ProjName from common block
*	03/20/91 Changed north-/south pole names
*	08/02/89 Finally converted real to double
*	04/20/89 Changed x0,y0,xwrap from INTEGER*2 to double precision to avoid overflow
*	03/08/89
*
*   Last modified
*	08/02/89 15:59
*..

* Type of projection

	integer CYL,MERC,LAMB,MILL,NPOLE,SPOLE,GLOBE
	integer proj
	logical RectProj,MapCut,MapWrap

	parameter(CYL=1,MERC=2,LAMB=3,MILL=4,NPOLE=5,SPOLE=6,GLOBE=7)

	character*30 ProjName(7)
	data ProjName(CYL)	/'Cylindrical'/
	data ProjName(MERC)	/'Mercator'/
	data ProjName(LAMB)	/'Lambert Conformal Conic'/
	data ProjName(MILL)	/'Miller Cylindrical'/
	data ProjName(NPOLE)	/'North Pole'/	! Orthographic'/
	data ProjName(SPOLE)	/'South Pole'/	! Orthographic'/
	data ProjName(GLOBE)	/'Globe (Orthographic)'/

* Scaling

	double precision scalex,scaley,scaler	! x, y, radial scale factors
	double precision winx(2),winy(2)	! map boundary coordinates
	double precision widex,widey,winr	! map width, height, radius
	double precision x0,y0			! projection origin
	double precision xwrap			! compensates for dateline
	double precision lolat			! polar projection mininum lat
	double precision tilt,lat0,lon0		! globe orientation

* Lambert variables

	double precision rho0,SinParallel0,cLambert

* Proj args

	integer NUMPROJARG
	parameter (NUMPROJARG=6)
	double precision ProjArg(NUMPROJARG)

* Commons

	common /eos$map/ proj,RectProj,MapCut,MapWrap,scalex,scaley,
     .				scaler,winx,winy,widex,widey,winr,x0,y0,
     .				xwrap,lolat,tilt,lat0,lon0,rho0,
     .				SinParallel0,cLambert,ProjArg
