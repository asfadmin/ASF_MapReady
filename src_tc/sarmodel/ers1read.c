#include "asf.h"
/********************************************************************
NAME:               ERS1READ.C

PURPOSE:  Reads necessary data from ERS1 file header

PROGRAM HISTORY:
VERSION         DATE   AUTHOR	  PURPOSE:
-------         ----   ------	  ------------------------------------------
  1.0           5/92   C. Taylor
  2.0		1/96   T. Logan	  Modified to use standard metadata handlers
  2.1		11/98  T. Logan   Modified for use of JERS data
*********************************************************************/
#include "asf.h"


#include "constants.h"
#include "ers1read.h"
#include "ceos.h"


/* Routine to read ERS1 file header */
ERS1  ers1read(char  *hdname)
{
  ERS1   ers;          		/* define ers1 structure for sarmodel   */
  struct VFDRECV ofdr;		/* Define value of facility data record */
  struct dataset_sum_rec dssr;	/* define data set summary record       */
  struct pos_data_rec ppdr;	/* define platform position data record */
  int    itmp1,itmp2;
  double dtmp1;

/* Information read from the data set summary record
 --------------------------------------------------*/
if (get_dssr(hdname,&dssr)<0)
 { printf("Failure reading dataset summary record from %s\n",hdname); exit(1); }
ers.wavelength = dssr.wave_length;

/* Information read from the facility related data record
 -------------------------------------------------------*/
if (get_facdr(hdname,&ofdr)<0)
 { printf("Failure reading facility data record from %s\n",hdname); exit(1); }

ers.latdc =  ofdr.imgclat*RPD;
ers.longc = (360.0-ofdr.imgclon)*RPD;
if (ers.longc>PIE2) ers.longc-=PIE2;
ers.azlook   = ofdr.nlooksaz;
ers.prf      = ofdr.prfreq;
ers.swathvel = ofdr.swathvel;
ers.elevc    = 1000.0*ofdr.avgterht;
ers.slant0   = 1000.0*ofdr.sltrngfp;
ers.slength  = 1000.0*ofdr.swazim;
if (strncmp(ofdr.grndslnt,"GROUN",5) == 0) ers.GROUND_RANGE = 1;
else  ers.GROUND_RANGE = 0;

if (ers.GROUND_RANGE)
 {
   ers.grrpix   = ofdr.rapixspc;
   ers.grapix   = ofdr.azpixspc;
   ers.ngrrpix  = (int)ofdr.apixels;
   ers.ngrapix  = (int)ofdr.alines;
   ers.srrpix   = -999.0;
   ers.srapix   = -999.0;
   ers.nsrapix  = -999.0;
 }
else /* SLANT RANGE */
 {
   ers.grrpix   = -999.0;
   ers.grapix   = -999.0;
   ers.ngrrpix  = -999.0;
   ers.ngrapix  = -999.0;
   ers.srrpix   = ofdr.rapixspc/sin(ofdr.incedang*RPD);
   ers.srapix   = ofdr.azpixspc;
   ers.nsrapix  = (int)ofdr.alines;
 }

/* Information read from the platform position data record
 --------------------------------------------------------*/
if (get_ppdr(hdname,&ppdr)<0) {
  printf("Failure reading platform position data record from %s\n",hdname);
  exit(1);
}

/* Copy timing information */
ers.timeref[1] = (double)ppdr.year;
ers.timeref[4] = (double)ppdr.month;
ers.timeref[5] = (double)ppdr.day;
ers.timeref[2] = (double)ppdr.gmt_day;
ers.deltimeref = ppdr.data_int;
ers.timeref[3] = ppdr.gmt_sec+ers.deltimeref;
itmp1 = (int)(ers.timeref[3]/3600);
itmp2 = (int)(ers.timeref[3]/60);
ers.timeref[6] = (double)itmp1;
ers.timeref[7] = (double)itmp2-60*(double)itmp1;
ers.timeref[8] = ers.timeref[3]-(double)(itmp1*3600)-(ers.timeref[7]*60);

/* Copy state vector information */
ers.satints[1] = ppdr.pos_vec[0][0]*1000.0;
ers.satints[2] = ppdr.pos_vec[0][1]*1000.0;
ers.satints[3] = ppdr.pos_vec[0][2]*1000.0;
ers.satints[4] = 1000.0*sqrt(ppdr.pos_vec[0][0]*ppdr.pos_vec[0][0]+
   ppdr.pos_vec[0][1]*ppdr.pos_vec[0][1]+ppdr.pos_vec[0][2]*ppdr.pos_vec[0][2]);
ers.satints[5] = ppdr.pos_vec[0][3];
ers.satints[6] = ppdr.pos_vec[0][4];
ers.satints[7] = ppdr.pos_vec[0][5];
ers.satints[8] = sqrt(ppdr.pos_vec[0][3]*ppdr.pos_vec[0][3]+
   ppdr.pos_vec[0][4]*ppdr.pos_vec[0][4]+ppdr.pos_vec[0][5]*ppdr.pos_vec[0][5]);
ers.satintc[1] = ppdr.pos_vec[1][0]*1000.0;
ers.satintc[2] = ppdr.pos_vec[1][1]*1000.0;
ers.satintc[3] = ppdr.pos_vec[1][2]*1000.0;
ers.satintc[4] = 1000.0*sqrt(ppdr.pos_vec[1][0]*ppdr.pos_vec[1][0]+
   ppdr.pos_vec[1][1]*ppdr.pos_vec[1][1]+ppdr.pos_vec[1][2]*ppdr.pos_vec[1][2]);
ers.satintc[5] = ppdr.pos_vec[1][3];
ers.satintc[6] = ppdr.pos_vec[1][4];
ers.satintc[7] = ppdr.pos_vec[1][5];
ers.satintc[8] = sqrt(ppdr.pos_vec[1][3]*ppdr.pos_vec[1][3]+
   ppdr.pos_vec[1][4]*ppdr.pos_vec[1][4]+ppdr.pos_vec[1][5]*ppdr.pos_vec[1][5]);
ers.satinte[1] = ppdr.pos_vec[2][0]*1000.0;
ers.satinte[2] = ppdr.pos_vec[2][1]*1000.0;
ers.satinte[3] = ppdr.pos_vec[2][2]*1000.0;
ers.satinte[4] = 1000.0*sqrt(ppdr.pos_vec[2][0]*ppdr.pos_vec[2][0]+
   ppdr.pos_vec[2][1]*ppdr.pos_vec[2][1]+ppdr.pos_vec[2][2]*ppdr.pos_vec[2][2]);
ers.satinte[5] = ppdr.pos_vec[2][3];
ers.satinte[6] = ppdr.pos_vec[2][4];
ers.satinte[7] = ppdr.pos_vec[2][5];
ers.satinte[8] = sqrt(ppdr.pos_vec[2][3]*ppdr.pos_vec[2][3]+
   ppdr.pos_vec[2][4]*ppdr.pos_vec[2][4]+ppdr.pos_vec[2][5]*ppdr.pos_vec[2][5]);

/* Additional Parameter calculations 
 -------------------------------------------------------*/
ers.times = -ers.deltimeref;
ers.timec = 0.0;
ers.timee = ers.deltimeref;
ers.strtrc = ers.satints[4];
ers.centrc = ers.satintc[4];
ers.endrc = ers.satinte[4];
dtmp1 = ers.satints[4]*ers.satints[4]*ers.satints[4];
ers.satints[13] = sqrt(MU/dtmp1);
dtmp1 = ers.satintc[4]*ers.satintc[4]*ers.satintc[4];
ers.satintc[13] = sqrt(MU/dtmp1);
dtmp1 = ers.satinte[4]*ers.satinte[4]*ers.satinte[4];
ers.satinte[13] = sqrt(MU/dtmp1);
ers.rawstrt = 0.0;

/* Print out values of the ers1 structure
 ---------------------------------------*/
/*
printf("ers.latdc:\t%16.7e\n",ers.latdc);
printf("ers.longc:\t%16.7e\n",ers.longc);
printf("ers.elevc:\t%16.7e\n",ers.elevc);
printf("ers.swathvel:\t%17.7e\n",ers.swathvel);
printf("ers.slant0:\t%17.7e\n",ers.slant0);
printf("ers.slength:\t%16.7e\n",ers.slength);
printf("ers.azlook:\t%16.7f\n",ers.azlook);
printf("ers.srapix:\t%16.7f\n",ers.srapix);
printf("ers.nsrapix:\t%16.7f\n",ers.nsrapix);
printf("ers.srrpix:\t%16.7f\n",ers.srrpix);
printf("ers.rawstrt:\t%16.7f\n",ers.rawstrt);
printf("ers.grapix:\t%16.7f\n",ers.grapix);
printf("ers.ngrapix:\t%16.7f\n",ers.ngrapix);
printf("ers.grrpix:\t%16.7f\n",ers.grrpix);
printf("ers.ngrrpix:\t%16.7f\n",ers.ngrrpix);
printf("ers.timeref[1]:\t%16.7e\n",ers.timeref[1]);
printf("ers.timeref[2]:\t%16.7e\n",ers.timeref[2]);
printf("ers.timeref[3]:\t%22.15f\n",ers.timeref[3]);
printf("ers.timeref[4]:\t%16.7e\n",ers.timeref[4]);
printf("ers.timeref[5]:\t%16.7e\n",ers.timeref[5]);
printf("ers.timeref[6]:\t%16.7e\n",ers.timeref[6]);
printf("ers.timeref[7]:\t%16.7e\n",ers.timeref[7]);
printf("ers.timeref[8]:\t%22.15f\n",ers.timeref[8]);
printf("ers.deltimeref:\t%22.15f\n",ers.deltimeref);
printf("ers.satints[1]:\t%22.15f\n",ers.satints[1]);
printf("ers.satints[2]:\t%22.15f\n",ers.satints[2]);
printf("ers.satints[3]:\t%22.15f\n",ers.satints[3]);
printf("ers.satints[4]:\t%22.15f\n",ers.satints[4]);
printf("ers.satints[5]:\t%22.15f\n",ers.satints[5]);
printf("ers.satints[6]:\t%22.15f\n",ers.satints[6]);
printf("ers.satints[7]:\t%22.15f\n",ers.satints[7]);
printf("ers.satints[8]:\t%22.15f\n",ers.satints[8]);
printf("ers.satints[9]:\t%22.15f\n",ers.satints[9]);
printf("ers.satints[10]:\t%22.15f\n",ers.satints[10]);
printf("ers.satints[11]:\t%22.15f\n",ers.satints[11]);
printf("ers.satints[13]:\t%22.15f\n",ers.satints[13]);
printf("ers.satintc[1]:\t%22.15f\n",ers.satintc[1]);
printf("ers.satintc[2]:\t%22.15f\n",ers.satintc[2]);
printf("ers.satintc[3]:\t%22.15f\n",ers.satintc[3]);
printf("ers.satintc[4]:\t%22.15f\n",ers.satintc[4]);
printf("ers.satintc[5]:\t%22.15f\n",ers.satintc[5]);
printf("ers.satintc[6]:\t%22.15f\n",ers.satintc[6]);
printf("ers.satintc[7]:\t%22.15f\n",ers.satintc[7]);
printf("ers.satintc[8]:\t%22.15f\n",ers.satintc[8]);
printf("ers.satintc[9]:\t%22.15f\n",ers.satintc[9]);
printf("ers.satintc[10]:\t%22.15f\n",ers.satintc[10]);
printf("ers.satintc[11]:\t%22.15f\n",ers.satintc[11]);
printf("ers.satintc[12]:\t%22.15f\n",ers.satintc[12]);
printf("ers.satinte[1]:\t%22.15f\n",ers.satinte[1]);
printf("ers.satinte[2]:\t%22.15f\n",ers.satinte[2]);
printf("ers.satinte[3]:\t%22.15f\n",ers.satinte[3]);
printf("ers.satinte[4]:\t%22.15f\n",ers.satinte[4]);
printf("ers.satinte[5]:\t%22.15f\n",ers.satinte[5]);
printf("ers.satinte[6]:\t%22.15f\n",ers.satinte[6]);
printf("ers.satinte[7]:\t%22.15f\n",ers.satinte[7]);
printf("ers.satinte[8]:\t%22.15f\n",ers.satinte[8]);
printf("ers.satinte[9]:\t%22.15f\n",ers.satinte[9]);
printf("ers.satinte[10]:\t%22.15f\n",ers.satinte[10]);
printf("ers.satinte[11]:\t%22.15f\n",ers.satinte[11]);
printf("ers.satinte[12]:\t%22.15f\n",ers.satinte[12]);
printf("ers.times:\t%16.7f\n",ers.times);
printf("ers.timec:\t%16.7f\n",ers.timec);
printf("ers.timee:\t%16.7f\n",ers.timee);
printf("ers.strtrc:\t%16.7f\n",ers.strtrc);
printf("ers.centrc:\t%16.7f\n",ers.centrc);
printf("ers.endrc:\t%16.7f\n",ers.endrc);
*/
return(ers);
}
