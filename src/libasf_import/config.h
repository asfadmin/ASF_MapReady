/*Config.h-- parameters, etc. that could concievably change.
This file is #included by decoder.h, which is in turn #included
almost everywhere in the level-zero utilities.
*/

/*Sync codes:
	static unsigned char RSAT_syncCode[4]={0x1a,0xcf,0xfc,0x1d};
	static unsigned char RSAT_auxSyncCode[4]={0x35,0x2E,0xF8,0x53};
	static unsigned char JRS_syncCode[7]={0xFF,0xCC,0xFF,0x0F,0x0f,0x30,0x00};
	static unsigned char ERS_syncCode[3]={0xFA,0xF3,0x20};
*/


#define speedOfLight 299792458.0 /*Speed of light in vacuum, m/s */

/*Planet constants*/
#define er_equator 6378137.0000 /*Equatorial Radius (m) WGS-84*/
#define er_polar (er_equator*(1-1.0/298.257223563)) /*Polar Radius (m) WGS-84*/
#define ecc2 (1-(er_polar*er_polar/(er_equator*er_equator)))


/**************************************
Satellite hardcoded parameter-setting macros.
These macros are used by the decoder_* files' *_init() routines.
*/
#define CONF_ERS_fields(s) \
	s->nPulseInAir=9;            /*Number of pulses in the air at one time.*/ \
	s->nSamp=5616;               /*Number of samples in a line of data.*/ \
	s->I_BIAS=s->Q_BIAS=15.5;    /*DC bias for I and Q channels.*/ \
	s->estDop=0.0;               /*Estimated doppler (PRF).*/ \
	s->zeroDopSteered=1;         /*Steered to zero doppler?*/ \
	s->fs=1.8962468e+07;         /*Range sampling frequency, Hz*/ \
	s->nValid=4800;              /*# of range samples for ARDOP to use.*/ \
	s->nLooks=5;                 /*# of looks for ARDOP to use.*/ \
	s->azres=8.0;                /*Azimuth resolution for ARDOP (m).*/ \
	s->slope=4.191375e+11;       /*chirp slope, Hz/sec.*/ \
	s->pulsedur=3.710000E-05;    /*chirp length, in sec.*/ \
	s->frequency=speedOfLight/0.056565; /*radar wavelength, in m.*/

#define CONF_JRS_fields(s) \
	s->nPulseInAir=7;            /*Number of pulses in the air at one time.*/ \
	s->nSamp=samplesPerFrame;    /*Number of samples in a line of data.*/ \
	s->I_BIAS=127.845;s->Q_BIAS=127.557;/*DC bias for I and Q channels->*/ \
	s->estDop=0.0;               /*Estimated doppler (PRF).*/ \
	s->zeroDopSteered=1;         /*Steered to zero doppler?*/ \
	s->fs=17.076e+06;            /*Range sampling frequency, Hz*/ \
	s->nValid=5400;              /*# of range samples for ARDOP to use.*/ \
	s->nLooks=3;                 /*# of looks for ARDOP to use.*/ \
	s->azres=18.0;               /*Azimuth resolution for ARDOP (m).*/ \
	s->slope=-4.285714e+11;      /*chirp slope, Hz/sec.*/ \
	s->pulsedur=3.50000E-05;     /*chirp length, in sec.*/ \
	s->frequency=1.274086E09;   /*speedOfLight/0.23512941;*/ /*radar wavelength, in m.*/ \

#define CONF_RSAT_fields(s) 	/*Fewer hardcoded fields for Radarsat-- more options*/ \
	s->I_BIAS=s->Q_BIAS=7.5; 	/*DC bias for I and Q channels.*/ \
	s->pulsedur=42E-06; 		/*chirp length, in sec.*/ \
	s->zeroDopSteered=0;		/*Steered to zero doppler? (no)*/ \
	s->frequency=5.300432E09; /*speedOfLight/0.056565;*/ /*radar wavelength, in m.*/

// Need some ALOS numbers
#define CONF_ALOS_fields(s) \
	s->nPulseInAir=7;            /*Number of pulses in the air at one time.*/ \
	s->nSamp=6144;               /*Number of samples in a line of data.*/ \
	s->I_BIAS=s->Q_BIAS=15.5;    /*DC bias for I and Q channels->*/ \
	s->estDop=0.0;               /*Estimated doppler (PRF).*/ \
	s->zeroDopSteered=0;         /*Steered to zero doppler?*/ \
	s->fs=17.076e+06;            /*Range sampling frequency, Hz*/ \
	s->nValid=5040;              /*# of range samples for ARDOP to use.*/ \
	s->nLooks=2;                 /*# of looks for ARDOP to use.*/ \
	s->azres=18.0;               /*Azimuth resolution for ARDOP (m).*/ \
	s->slope=-4.285714e+11;      /*chirp slope, Hz/sec.*/ \
	s->pulsedur=27E-06;     /*chirp length, in sec.*/ \
	s->frequency=speedOfLight/0.2360571; /*radar wavelength, in m.*/ \
//        s->prf=2159827.2138229; \

/***********************************************
Satellite auxiliary parameters, used by aux_* routines.
*/
#define CONF_ERS_timeBase 210.94E-9 /*ERS-series basic clock duration (s)*/
#define CONF_JRS_prfCodeList {1505.8,1530.1,1555.2,1581.1,1606.0,-99,-99,-99}
#define CONF_RSAT_beamNameList {"   ", /*Table of beam names,indexed by beam number*/\
		"ST1","ST2","ST3","ST4","ST5","ST6","ST7", \
		"WD1","WD2","WD3","WD2_Recorded", \
		"EX1","EX2","EH4","EX4", \
		"FN1","FN2","FN3","FN4","FN5" \
		}
#define CONF_RSAT_pulsesInAirList {0, /*Table of # of pulses in air,indexed by beam number*/\
		7,7,8,8,8,9,9, /*Standard beams*/\
		7,8,9,8, /*Wide beams*/\
		10,10,11,11, /*Experimental beams (may change)*/\
		8,8,9,9,9 /*Fine beams*/\
		}
#define CONF_RSAT_SMO 129.2683e6 /*Satellite Master Oscillator frequency (Hz)*/
/*RADARSAT range chirp slopes (Hz/sec)*/
#define CONF_RSAT_slopeFast -7.214e11
#define CONF_RSAT_slopeMed -4.1619e11
#define CONF_RSAT_slopeSlow  -2.7931e11



/*******************************************
Satellite Range Pulse delay factors:
these values are subtracted from the naive
range gate delay in aux_*.  Values in seconds.
These parameters were estimated using the Delta
Junction corner reflector array.
*/
#define CONF_ERS1_rangePulseDelay 7.8e-6
#define CONF_ERS2_rangePulseDelay 6.4e-6

/* #define CONF_JRS_rangePulseDelay 16.75e-6 */
#define CONF_JRS_rangePulseDelay 13.75e-6

#define CONF_RSAT_rangePulseDelay 4.04e-6


/*******************************************
Satellite maximum clock values:

*/
#define CONF_ERS_maxClockValue	4294967295

