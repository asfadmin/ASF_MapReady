/****************************************************************
FUNCTION NAME:

SYNTAX:

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"
#include "sarout.h"
#include "asf_meta.h"
#include "string.h"

#define E1      1               /* ERS-1      */
#define E2      2               /* ERS-2      */
#define J1      3               /* JERS-1     */
#define R1      4               /* RADARSAT-1 */

void addSensorFields(int mode, char *sensor,char *beammode, ceosLeader *data);
void addSensorFields(int mode, char *sensor,char *beammode, ceosLeader *data)
 {
   int platform;
   int n;

   switch (sensor[0])
    {
      case 'E': if (sensor[3] == '1') platform = E1; else platform = E2; break;
      case 'J': platform = J1; break;
      case 'R': platform = R1;
    }

   if (platform == E1)
    {
	strcpy(data->dssr.mission_id,"ERS-1");
	strcpy(data->facdr.platform,"ERS-1");
	strcpy(data->dssr.sensor_id,"ERS-1-C -     -VV");
	strcpy(data->facdr.sensmode,"ERS-1-C -     -VV");
	data->dssr.frequency = 5.3;
	data->dssr.rng_length = 37.1;
	data->dssr.rng_samp_rate = 18.96;
	data->dssr.chn_bits = data->facdr.bitssamp = 5;
	data->dssr.ele_sight = data->dssr.mech_sight = 20.3;
	data->dssr.elev_beam = 5.4;
	data->dssr.azi_beam = 0.2852;
	data->dssr.bnd_azi = 1260.0;
	data->dssr.bnd_rng = 15.55;
	data->dssr.sat_clkinc = 3906249;
	data->dssr.phas_coef[2] = 0.419137;
	n = 9;
    }
   else if (platform == E2)
    {
	strcpy(data->dssr.mission_id,"ERS-2");
	strcpy(data->facdr.platform,"ERS-2");
	strcpy(data->dssr.sensor_id,"ERS-2-C -     -VV");
	strcpy(data->facdr.sensmode,"ERS-2-C -     -VV");
	data->dssr.frequency = 5.3;
	data->dssr.rng_length = 37.1;
	data->dssr.rng_samp_rate = 18.96;
	data->dssr.chn_bits = data->facdr.bitssamp = 5;
	data->dssr.ele_sight = data->dssr.mech_sight = 20.3;
	data->dssr.elev_beam = 5.4;
	data->dssr.azi_beam = 0.2852;
	data->dssr.bnd_azi = 1260.0;
	data->dssr.bnd_rng = 15.55;
	data->dssr.sat_clkinc = 3906249;
	data->dssr.phas_coef[2] = 0.419137;
	n = 9;
    }
   else if (platform == J1)
    {
	strcpy(data->dssr.mission_id,"JERS-1");
	strcpy(data->facdr.platform,"JERS-1"); 
	strcpy(data->dssr.sensor_id,"JERS-1-L -    -HH");
	strcpy(data->facdr.sensmode,"JERS-1-L -    -HH");
	data->dssr.frequency = 1.275; 
	data->dssr.rng_length = 35.0;
	data->dssr.rng_samp_rate = 17.076;
	data->dssr.chn_bits = data->facdr.bitssamp = 3;
	data->dssr.ele_sight = data->dssr.mech_sight = 35.1;
	data->dssr.elev_beam = 5.4;
	data->dssr.azi_beam = 0.994574;
	data->dssr.bnd_azi = 1110.0;
	data->dssr.bnd_rng = 15.0;
	data->dssr.sat_clkinc = 2118;
	n = 7;
    }
   else if (platform == R1)
    {
	strcpy(data->dssr.mission_id,"RSAT-1");
	strcpy(data->facdr.platform,"RSAT-1");
	strcpy(data->dssr.sensor_id,"RSAT-1-C -    -HH");
	strcpy(data->facdr.sensmode,"RSAT-1-C -    -HH");
	data->dssr.frequency = 5.3;
	data->dssr.rng_length = 42;
	data->dssr.chn_bits = data->facdr.bitssamp = 4;
        data->dssr.elev_beam = 5.4; 
	data->dssr.sat_clkinc = 976;

	if (strncmp(beammode,"ST1",3)==0)
	  {
	    data->dssr.rng_samp_rate = 18.46;
	    data->dssr.ele_sight = 20.366;
	    data->dssr.mech_sight = 29.8;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 17.48;
	    strcpy(data->dssr.beam1,"ST1");
	    n = 7;
	  }
	else if (strncmp(beammode,"ST2",3)==0)
	  {
	    data->dssr.rng_samp_rate = 18.46;
	    data->dssr.ele_sight = 24.2;
	    data->dssr.mech_sight = 29.8;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 17.48;
	    strcpy(data->dssr.beam1,"ST2");
 	    n = 7;
	  }
	else if (strncmp(beammode,"ST3",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight =  -99.0;
	    data->dssr.mech_sight = 29.8;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"ST3");
	    n = 8;
	  }
	else if (strncmp(beammode,"ST4",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = 31.875;
	    data->dssr.mech_sight = 29.8;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"ST4");
	    n = 8;
	  }
	else if (strncmp(beammode,"ST5",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = 34.064;
	    data->dssr.mech_sight = 29.8;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"ST5");
	    n = 8;
	  }
	else if (strncmp(beammode,"ST6",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = 38.20718;
	    data->dssr.mech_sight = 29.8;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"ST6");
	    n = 9;
	  }
	else if (strncmp(beammode,"ST7",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = 40.438;
	    data->dssr.mech_sight = 29.8;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"ST7");
	    n = 9;
	  }
	else if (strncmp(beammode,"WD1",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"WD1");
	    n = 7;
	  }
	else if (strncmp(beammode,"WD2",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"WD2");
	    n = 8;
	  }
	else if (strncmp(beammode,"WD3",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"WD3");
	    n = 9;
	  }
	else if (strncmp(beammode,"FN1",3)==0)
	  {
	    data->dssr.rng_samp_rate = 32.31;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 30.002;
	    strcpy(data->dssr.beam1,"FN1");
	    n = 8;
	  }
	else if (strncmp(beammode,"FN2",3)==0)
	  {
	    data->dssr.rng_samp_rate = 32.31;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 30.002;
	    strcpy(data->dssr.beam1,"FN2");
	    n = 8;
	  }
	else if (strncmp(beammode,"FN3",3)==0)
	  {
	    data->dssr.rng_samp_rate = 32.31;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 30.002;
	    strcpy(data->dssr.beam1,"FN3");
	    n = 9;
	  }
	else if (strncmp(beammode,"FN4",3)==0)
	  {
	    data->dssr.rng_samp_rate = 32.31;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 30.002;
	    strcpy(data->dssr.beam1,"FN4");
	    n = 9;
	  }
	else if (strncmp(beammode,"FN5",3)==0)
	  {
	    data->dssr.rng_samp_rate = 32.31;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 30.002;
	    strcpy(data->dssr.beam1,"FN5");
	    n = 9;
	  }
	else if (strncmp(beammode,"EL1",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"EL1");
	  }
	else if (strncmp(beammode,"EH1",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"EH1");
            n = 10;
	  }
	else if (strncmp(beammode,"EH2",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"EH2");
            n = 10;
	  }
	else if (strncmp(beammode,"EH3",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"EH3");
            n = 10;
	  }
	else if (strncmp(beammode,"EH4",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"EH4");
            n = 11;
	  }
	else if (strncmp(beammode,"EH5",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"EH5");
            n = 11;
	  }
	else if (strncmp(beammode,"EH6",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.92;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.2852;
	    data->dssr.bnd_azi = 960.0;
	    data->dssr.bnd_rng = 11.78;
	    strcpy(data->dssr.beam1,"EH6");
            n = 11;
	  }
	else if (strncmp(beammode,"SWA",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.927;
	    data->dssr.ele_sight = -7.17;
	    data->dssr.mech_sight = 29.4;
	    data->dssr.azi_beam = 0.288;
	    data->dssr.bnd_azi = 899.3; 
	    data->dssr.bnd_rng = 11.731;
	    data->dssr.no_beams = 4;
	    strcpy(data->dssr.beam1,"WD1");
	    strcpy(data->dssr.beam2,"WD2");
	    strcpy(data->dssr.beam3,"WD3");
	    strcpy(data->dssr.beam4,"ST7");
	    data->dssr.prf1 = 1284.0;
	    data->dssr.prf2 = 1323.0;
	    data->dssr.prf3 = 1222.0;
	    data->dssr.prf4 = 1265.0;

 	    data->ppdr.alt_poserr = 65.3199997;
	    data->ppdr.crt_poserr = 12.9399996;
	    data->ppdr.rad_poserr = 12.1700001;
	    data->ppdr.alt_velerr = 0.0123300;
	    data->ppdr.crt_velerr = 0.0137900;
    	    data->ppdr.rad_velerr = 0.0596200;	
	    n = 7;
	  }
	else if (strncmp(beammode,"SWB",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.927;
	    data->dssr.ele_sight = -7.17;
	    data->dssr.mech_sight = 29.4;
	    data->dssr.azi_beam = 0.288;
	    data->dssr.bnd_azi = 901.4525757;
	    data->dssr.bnd_rng = 11.731;
	    data->dssr.no_beams = 4;
	    strcpy(data->dssr.beam1,"WD1");
	    strcpy(data->dssr.beam2,"WD2");
	    strcpy(data->dssr.beam3,"ST5");
	    strcpy(data->dssr.beam4,"ST6");
	    data->dssr.prf1 = 1287.0;
	    data->dssr.prf2 = 1325.0;
	    data->dssr.prf3 = 1287.0;
	    data->dssr.prf4 = 1329.0;

 	    data->ppdr.alt_poserr = 65.3199997;
	    data->ppdr.crt_poserr = 12.9399996;
	    data->ppdr.rad_poserr = 12.1700001;
	    data->ppdr.alt_velerr = 0.0123300;
	    data->ppdr.crt_velerr = 0.0137900;
    	    data->ppdr.rad_velerr = 0.0596200;	
	    n = 7;
	  }
	else if (strncmp(beammode,"SNA",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.927;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.288;
	    data->dssr.bnd_azi = 939.0;
	    data->dssr.bnd_rng = 11.731;
	    data->dssr.no_beams = 2;
	    strcpy(data->dssr.beam1,"WD1");
	    strcpy(data->dssr.beam2,"WD2");
	    data->dssr.prf1 = 1287.0;
	    data->dssr.prf2 = 1325.0;	
	    n = 7;
	  }
	else if (strncmp(beammode,"SNB",3)==0)
	  {
	    data->dssr.rng_samp_rate = 12.927;
	    data->dssr.ele_sight = data->dssr.mech_sight = -99.0;
	    data->dssr.azi_beam = 0.288;
	    data->dssr.bnd_azi = 939.0;
	    data->dssr.bnd_rng = 11.731;
	    data->dssr.no_beams = 3;
	    strcpy(data->dssr.beam1,"WD2");
	    strcpy(data->dssr.beam2,"ST5");
	    strcpy(data->dssr.beam3,"ST6");
	    data->dssr.prf1 = 1325.0;
	    data->dssr.prf2 = 1287.0;
	    data->dssr.prf3 = 1329.0;	
	    n = 8;
	  }
     } /* End of RSAT-1 section */
   if (mode == CEOS_CCSD)
    { 
      data->dssr.rng_gate1 =
      data->facdr.rangegd  = data->facdr.datawin+1.0E6*(n/data->facdr.prfreq);
    }

  }
