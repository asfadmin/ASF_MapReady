*.
*   Orbit.inc - orbital elements common block for EosMenu
*
*	Need Instruments.inc for value of MAXPLAT
*
*	07/26/89 Original
*
*	07/26/89 11:04
*..
	double precision a(MAXPLAT),e(MAXPLAT),i(MAXPLAT)
	double precision node0(MAXPLAT),omega0(MAXPLAT),m0(MAXPLAT),t0(MAXPLAT)
	double precision tstart(MAXPLAT),tend(MAXPLAT)

	common /eos$orbit/ a,e,i,node0,omega0,m0,t0,tstart,tend
