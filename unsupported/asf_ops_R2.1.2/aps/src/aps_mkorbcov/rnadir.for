
*.
*   RNadir - compute earth radius at latitude latc (rad)
*
*   Args
*	latc			double	input	geocentric latitude (rad)
*	RNadir			double	return	radius of earth (km)
*
*   Assumes
*	Central body has been set
*
*	10/05/88 15:49
*..
	double precision function RNadir(latc)
	include 'eosinclude:cbody.inc'
	double precision latc

	  character*100 SccsFileID
     -/'@(#)rnadir.for	5.1 98/01/08 APS/ASF\0'/
	RNadir = RBody*(1.0-flat)
     .			/ sqrt((1.0-flat)**2*cos(latc)**2+sin(latc)**2)

	end
