     -/'@(#)cbody.inc	5.1 98/01/08 APS/ASF\0'/

*.
*   CBody.inc - central body variables
*
*	08/02/89 15:37
*..
	integer CentralBody,EARTH,MOON,MARS,VENUS,BODIES
	parameter(EARTH=1,MOON=2,VENUS=3,MARS=4,BODIES=4)

	double precision RBody,RBodies(BODIES)
	double precision mu,mus(BODIES)
	double precision flat,flats(BODIES)
	double precision RotRate,RotRates(BODIES)
	double precision j2,j2s(BODIES)
	double precision j3,j3s(BODIES)
	double precision AxisTilt,AxisTilts(BODIES)
	double precision SecPerYear,SecsPerYear(BODIES)
	character*20 NameBody,NameBodies(BODIES)

	data RBodies(EARTH)	/6378.144d0/
	data mus(EARTH)		/3.9860045d5/
	data flats(EARTH)	/0.00335233d0/
	data RotRates(EARTH)	/7.292115856d-5/
	data j2s(EARTH)		/0.10826270d-02/
	data j3s(EARTH)		/-0.25364140d-05/
	data AxisTilts(EARTH)	/23.45d0/
	data SecsPerYear(EARTH)	/31556925.9747d0/
	data NameBodies(EARTH)	/'Earth'/

	data RBodies(MOON)	/1738.0d0/
	data mus(MOON)		/4902.8d0/
	data flats(MOON)	/0.0d0/
	data RotRates(MOON)	/0.0d0/
	data j2s(MOON)		/0.0d0/
	data j3s(MOON)		/0.0d0/
	data AxisTilts(MOON)	/6.5d0/
	data SecsPerYear(MOON)	/2419200.0d0/
	data NameBodies(MOON)	/'Moon'/

	data RBodies(VENUS)	/6052.0d0/
	data mus(VENUS)		/3.2485877d5/
	data flats(VENUS)	/0.0d0/
	data RotRates(VENUS)	/-2.992677d-7/
	data j2s(VENUS)		/.452072282177133526D-05/
	data j3s(VENUS)		/-.134207959331458283D-05/
	data AxisTilts(VENUS)	/32.0d0/
	data SecsPerYear(VENUS)	/19394640.0d0/
	data NameBodies(VENUS)	/'Venus'/

	data RBodies(MARS)	/3393.4d0/
	data mus(MARS)		/4.2828287d4/
	data flats(MARS)	/0.004898d0/
	data RotRates(MARS)	/7.0881912d-5/
	data j2s(MARS)		/.196045446D-02/
	data j3s(MARS)		/.314492574D-04/
	data AxisTilts(MARS)	/23.9833d0/
	data SecsPerYear(MARS)	/59355046.08d0/
	data NameBodies(MARS)	/'Mars'/

	common /eos$CBody/ RBody,mu,flat,RotRate,j2,j3,
     .				AxisTilt,SecPerYear,NameBody,CentralBody
