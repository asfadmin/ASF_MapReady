#ifndef _L_DSS_H
#define _L_DSS_H

static char sccsid_l_dss_h[] = 
    "@(#)l_dss.h	1.2 96/04/09 20:29:08";

/**********************************************************************/
/* leader file data set summary record structure 'l_dss.h'            */
/**********************************************************************/

typedef   struct {

unsigned long int    seq_number; 	/*Sequence Number*/
unsigned      char   rec1_code; 	/*1-st Record sub-type code*/
unsigned      char   rec_type;	 	/*Record Type code*/
unsigned      char   rec2_code; 	/*2-nd Record sub-type code*/
unsigned      char   rec3_code; 	/*3-rd Record sub-type code*/
unsigned long int    rec_len;	 	/*Record length*/

char	dss007[4];	/*Record sequence number (starts at 1)*/
char	dss008[4];	/*SAR channel indicator*/
char	dss009[16];	/*Scene identifier*/
char	dss010[32];	/*Scene designator*/
char	dss011[32];	/*Input scene center time*/
			/*YYYYMMDDhhmmssttt$$$$$$$$$$$$$ where:*/
			/*YYYY = year*/
			/*MM = month */
			/*DD = day */
			/*hh = hours (00 to 23) */
			/*mm = minutes (00 to 59)*/
			/*ss = seconds (00 to 59)*/
			/*ttt = milliseconds (000 to 999)*/
char	dss012[16];	/*spare*/
char	dss013[16];	/*Processed scene center geodetic latitude*/
char	dss014[16];	/*Processed scene center geodetic longitude*/
char	dss015[16];	/*Processed Scene Center true heading(deg.)*/
char	dss016[16];	/*Ellipsoid designator*/
char	dss017[16];	/*Ellipsoid semimajor axis (km)	- {Re}*/
char	dss018[16];	/*Ellipsoid semiminor axis (km)*/
char	dss019[16];	/*Gravitational parameter - {mu = G*Me}*/
char	dss020[16];	/*spare*/
char	dss021[16];	/*Ellipsoid J2 parameter*/
char	dss022[16];	/*Ellipsoid J3 parameter*/
char	dss023[16];	/*Ellipsoid J4 parameter*/
char	dss024[16];	/*Average terrain height(km)*/
char	dss025[16];	/*Scene center line number*/
char	dss026[16];	/*Scene center pixel number*/
char	dss027[16];	/*Processed scene length (km)*/
char	dss028[16];	/*Processed scene width (km)*/
char	dss029[8];	/*spare*/
char	dss030[8];	/*spare*/
char	dss031[4];	/*Number of SAR channels*/
char	dss032[4];	/*spare*/
char	dss033[16];	/*Sensor platform mission identifier*/
char	dss034[32];	/*Sensor ID: and mode of operation*/
			/*<AAAAAA-BB-CCDD-EF$$$$$$$$$$$$$$$>, where:*/	
			/*AAAAAA= six characters sensor ID*/
			/*BB = SAR band (eg: X$, L$, C$, KU, KA, etc:)*/
			/*CC = code for resolution mode*/
			/*DD = code for imaging mode*/
			/*E = transmit polarization (H or V)*/
			/*F = receiver polarization (H or V)*/
char	dss035[8];	/*Orbit number*/
char	dss036[8];	/*Sensor Platform geodetic Lattitude)*/
char	dss037[8];	/*Sensor Platform geodetic Longitude*/
char	dss038[8];	/*Sensor Platform Heading(deg.)*/
char	dss039[8];	/*Sensor clock angle*/
char	dss040[8];	/*Incidence angle at scene center*/
char	dss041[8];	/*radar frequency (GHz) */
char	dss042[16];	/*Radar wavelength (meters)*/
char	dss043[2];	/*Motion compensation indicator*/
			/*"00" = no compensation*/
			/*"01" = on board compensation*/
			/*"10" = in processor compensation*/
			/*"11" = both on board and in processor*/
char	dss044[16];	/*Range pulse code specifier*/
char	dss045[16];	/*Range pulse amplitude coefficient #1(Hz)*/
char	dss046[16];	/*Range pulse amplitude coefficient #2(Hz/sec)*/
char	dss047[16];	/*Range pulse amplitude coefficient #3*/
char	dss048[16];	/*Range pulse amplitude coefficient #4*/
char	dss049[16];	/*Range pulse amplitude coefficient #5*/
char	dss050[16];	/*Range pulse phase coefficient #1(rad)*/
char	dss051[16];	/*Range pulse phase coefficient #2(rads./sec)*/
char	dss052[16];	/*Range pulse phase coefficient #3(rads./sec)*/
char	dss053[16];	/*Range pulse phase coefficient #4*/
char	dss054[16];	/*Range pulse phase coefficient #5*/
char	dss055[8];	/*Down linked data chirp extraction index*/
char	dss056[8];	/*spare*/
char	dss057[16];	/*Sampling rate (MHz)*/
char	dss058[16];	/*Range gate at early edge(usec)*/
char	dss059[16];	/*Range pulse length (usec)*/
char	dss060[4];	/*Base band conversion flag (YES$/NOT$)*/
char	dss061[4];	/*Range compressed flag (YES$/NOT$)*/
char	dss062[16];	/*Receiver gain for like polarized(dB)*/
char	dss063[16];	/*Receiver gain for cross polarized(dB)*/
char	dss064[8];	/*Quantization in bits per channel*/
char	dss065[12];	/*Quantizer  descriptor (eg:"UNIFORM$I,Q$")*/
char	dss066[16];	/*DC Bias for I-component*/
char	dss067[16];	/*DC Bias for Q-component*/
char	dss068[16];	/*Gain imbalance for I & Q*/
char	dss069[16];	/*spare*/
char	dss070[16];	/*spare*/
char	dss071[16];	/*Antenna electronic boresight relative to */
			/*platform vertical axis at the start of */
			/*the image (degrees)*/
char	dss072[16];	/*Antenna mechanical boresight relative to*/
			/*platform vertical axis at the start of */
			/*the image, positive to the right,negative */
			/*to the left (degrees)*/
char	dss073[4];	/*Echo tracker-on/off designator"ON$$","OFF$"*/
char	dss074[16];	/*NominaI PRF (Hz)*/
char	dss075[16];	/*Effective two-way antenna elevation 3dB */
			/*beam width at boresight (degrees)*/
char	dss076[16];	/*Effective two-way antenna azimuth 3dB */
			/*beam width at electronic boresight (degrees)*/

	/*Sensor specific parameters*/
char	dss077[16];	/*Satellite encoded binary time code)*/
char	dss078[32];	/*Satellite clock time<YYYYMMDDhhmmssttt$$$.$>*/
char	dss079[8];	/*Satellite clock increment (nano-secs))*/
char	dss080[8];	/*spare*/

	/*General processing parameters*/
char	dss081[16];	/*Processing facility identifier*/
char	dss082[8];	/*Processing system identifier*/
char	dss083[8];	/*Processing version identifier*/
char	dss084[16];	/*Processing facility process code*/
char	dss085[16];	/*Product level code (Ref.#2)*/
char	dss086[32];	/*Product type specifier (Ref.#2)*/
char	dss087[32];	/*Processing algorithm identifier*/
char	dss088[16];	/*Nominal effective number of Azimuth looks*/
char	dss089[16];	/*Nominal effective number of Range looks*/
char	dss090[16];	/*Bandwidth per look in Azimuth ( Hz )*/
char	dss091[16];	/*Bandwidth per look in Range ( Hz )*/
char	dss092[16];	/*Total processor bandwidth in Azimuth*/
char	dss093[16];	/*Total processor bandwidth in Range*/
char	dss094[32];	/*Weighing function designator in Azimuth*/
char	dss095[32];	/*Weighing function designator in Range*/
char	dss096[16];	/*Data input source (eg:HDDT identifier)*/
char	dss097[16];	/*Nominal resolution equal to 3dB point*/
			/*in ground range (meter)*/
char	dss098[16];	/*Nominal resolution in Azimuth (meter)*/
char	dss099[16];	/*Constant radiometric parameter (Bias)*/
char	dss100[16];	/*Linear radiometric parameter (Gain)*/
char	dss101[16];	/*Along track Doppler frequency constant(Hz)*/
char	dss102[16];	/*Along track Dopplr frq linear term(Hz/pixel)*/
char	dss103[16];	/*Along track Doppler frequency quadratic term*/
			/*at early edge of the image (Hz/pixel/pixel)*/
char	dss104[16];	/*spare*/
char	dss105[16];	/*Cross track Doppler frequency constant term*/
			/*at early edge of the image (Hz)*/
char	dss106[16];	/*Cross track Doppler frequency linear term*/
			/*at early edge of the image (Hz/pixel)*/
char	dss107[16];	/*Cross track Doppler frequency quadratic term*/
			/*at early edge of the image (Hz/pixeI/pixel)*/
char	dss108[8];	/*Time direction indicator along pixel directn*/
char	dss109[8];	/*Time direction indicator along line directn*/
char	dss110[16];	/*Along track Doppler freq rate const. Hz/sec)*/
char	dss111[16];	/*Along track Doppler freq rate linear term*/
char	dss112[16];	/*Along track Doppler freq rate quadratic term*/
char	dss113[16];	/*spare*/
char	dss114[16];	/*Cross track Doppler freq rate constant term*/
char	dss115[16];	/*Cross track Doppler freq rate linear term*/
char	dss116[16];	/*Cross track Doppler freq rate quadratic term*/
char	dss117[16];	/*spare*/
char	dss118[8];	/*Line content indicator*/
char	dss119[4];	/*Clutter lock applied flag ( "YES$"/"NOT$" )*/
char	dss120[4];	/*Autofocussing applied flag ( "YES$"/"NOT$" )*/
char	dss121[16];	/*Line spacing (meters)*/
char	dss122[16];	/*Pixel spacing (meters)*/
char	dss123[16];	/*Processor range compression designator*/
char	dss124[16];	/*spares*/
char	dss125[16];	/*spares*/

	/*Sensor specific local use segment*/
char	dss126[120];	/*spares*/

	/*Processor specific local use segment*/
char	dss127[120];	/*spares*/

	/*Image annotation fields*/
char	dss128[8];	/*Number of Annotation points (0)*/
}L_DSS_FILE,*L_DSS_PTR;


#endif /* ! _L_DSS_H */
