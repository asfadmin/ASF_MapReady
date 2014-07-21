/********************************************************************
NAME:   print_convert.c

PURPOSE: prints floats and integers stored in the VFDRECV structure ofdr

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           9/93   T. Logan (ASF)
  1.1           9/96   T. Logan (ASF)    Modified to interpret era of record

*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_facdr(struct VFDRECV *ofdr, int era)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n********** begin of Facility Related Data record ***********\n\n");
  add(&ret, "Data take id = \t%s\n", ofdr->dataid);
  add(&ret, "SPS image identifier = \t%s\n", ofdr->imageid);
  add(&ret, "UTC year of image correlation = \t%s\n", ofdr->coryear);
  add(&ret, "UTC time of image correlation = \t%s\n", ofdr->cortime);
  add(&ret, "Name of site covered = \t%s\n", ofdr->sitename);
  add(&ret, "UTC year at the image center = \t%s\n", ofdr->imgyear);
  add(&ret, "UTC time at the image center = \t%s\n", ofdr->imgtime);
  add(&ret, "Latitude of the image center = \t%.7f\n", ofdr->imgclat);
  add(&ret, "Longitude of the image center = \t%.7f\n", ofdr->imgclon);
  add(&ret, "Lat at start of image frame in near swath = \t%.7f\n", 
	  ofdr->nearslat);
  add(&ret, "Long at start of image frame in near swath = \t%.7f\n", 
	  ofdr->nearslon);
  add(&ret, "Lat at end of image frame in near swath = \t%.7f\n", ofdr->nearelat);
  add(&ret, "Long at end of image frame in near swath = \t%.7f\n", 
	  ofdr->nearelon);
  add(&ret, "Lat at start of image frame in far swath = \t%.7f\n", ofdr->farslat);
  add(&ret, "Long at start of image frame in far swath = \t%.7f\n", 
	  ofdr->farslon);
  add(&ret, "Lat at end of image frame in far swath = \t%.7f\n", ofdr->farelat);
  add(&ret, "Long at end of image frame in far swath = \t%.7f\n", ofdr->farelon);
  add(&ret, "Actual swath width (km) in azimuth direction = \t%.7f\n", 
	  ofdr->swazim);
  add(&ret, "Actual swath width (km) in range direction = \t%.7f\n", 
	  ofdr->swrange);
  add(&ret, "Actual (no filler) number of pixels per line = \t%i\n", 
	  ofdr->npixels);
  add(&ret, "Actual (no filler) number of image lines = \t%i\n", ofdr->nlines);
  add(&ret, "Total (with filler) number of pixels per line = \t%i\n", 
	  ofdr->apixels);
  add(&ret, "Total (with filler) number of image lines = \t%i\n", ofdr->alines);
  add(&ret, "Identification label of the media written to = \t%s\n", 
	  ofdr->mediaid);
  add(&ret, "Location on the DCRSI where data begins = \t%s\n", ofdr->sblock);
  add(&ret, "Location on the DCRSI where data ends = \t%s\n", ofdr->eblock);
  add(&ret, "Name of platform for the transmitting sensor = \t%s\n", 
	  ofdr->platform);
  add(&ret, "Sensor and mode of operation = \t%s\n", ofdr->sensmode);
  add(&ret, "Pulse repitition frequency (prf) = \t%.7f\n", ofdr->prfreq);
  add(&ret, "SAR antenna look angle = \t%.7f\n", ofdr->antlook);
  add(&ret, "Data rate = \t%.7f\n", ofdr->datarate);
  add(&ret, "Data window position = \t%.7f\n", ofdr->datawin);
  add(&ret, "Range gate delay = \t%.7f\n", ofdr->rangegd);
  add(&ret, "Track angle to True North = \t%.7f\n", ofdr->trackang);
  add(&ret, "Flag indicating whether the pass is asc/desc = \t%s\n", 
	  ofdr->ascdesc);
  add(&ret, "Altitude of the spacecraft at image center = \t%.7f\n", ofdr->scalt);
  add(&ret, "Spacecraft X-position at the image center = \t%.15f\n", 
	  ofdr->scxpos);
  add(&ret, "Spacecraft Y-position at the image center = \t%.15f\n", 
	  ofdr->scypos);
  add(&ret, "Spacecraft Z-position at the image center = \t%.15f\n", 
	  ofdr->sczpos);
  add(&ret, "Spacecraft X-velocity at the image center = \t%.15f\n", 
	  ofdr->scxvel);
  add(&ret, "Spacecraft Y-velocity at the image center = \t%.15f\n",
	  ofdr->scyvel);
  add(&ret, "Spacecraft Z-velocity at the image center = \t%.15f\n", 
	  ofdr->sczvel);
  add(&ret, "Spacecraft roll at the image center = \t%.14g\n", ofdr->scroll);
  add(&ret, "Spacecraft yaw at the image center = \t%.14g\n", ofdr->scyaw);
  add(&ret, "Spacecraft pitch at the image center = \t%.14g\n", ofdr->scpitch);
  add(&ret, "Quality flag for the spacecraft roll = \t%i\n", ofdr->qroll);
  add(&ret, "Quality flag for the spacecraft yaw = \t%i\n", ofdr->qyaw);
  add(&ret, "Quality flag for the spacecraft pitch = \t%i\n", ofdr->qpitch);
  add(&ret, "Spacecraft roll rate at the image center = \t%.14g\n", ofdr->rroll);
  add(&ret, "Spacecraft yaw rate at the image center = \t%.14g\n", ofdr->ryaw);
  add(&ret, "Spacecraft pitch rate at the image center = \t%.14g\n", 
	  ofdr->rpitch);
  add(&ret, "Quality flag for the spacecraft roll rate = \t%i\n", ofdr->rqroll);
  add(&ret, "Quality flag for the spacecraft yaw rate = \t%i\n", ofdr->rqyaw);
  add(&ret, "Quality flag for the spacecraft pitch rate = \t%i\n", ofdr->rqpitch);
  add(&ret, "Radius of the earth at nadir = \t%.7f\n", ofdr->eradnadr);
  add(&ret, "Radius of the earth at image center = \t%.7f\n", ofdr->eradcntr);
  add(&ret, "Incidence angle at the center of the image = \t%.7f\n", 
	  ofdr->incedang);
  add(&ret, "Version of the ASP = \t%s\n", ofdr->procvers);
  add(&ret, "Image processing type identifier = \t%s\n", ofdr->imgproct);
  add(&ret, "Type of Ephemeris used identifier = \t%s\n", ofdr->ephemert);
  add(&ret, "Effective number of looks in azimuth = \t%.7f\n", ofdr->nlooksaz);
  add(&ret, "Effective number of looks in range = \t%.7f\n", ofdr->nlooksra);
  add(&ret, "Weighting pedestal height in azimuth = \t%.7f\n", ofdr->weightaz);
  add(&ret, "Weighting pedestal height in range = \t%.7f\n", ofdr->weightra);
  add(&ret, "Look energy normalization flag = \t%s\n", ofdr->normener);
  add(&ret, "Processing induced distortions in azimuth = \t%.7f\n", 
	  ofdr->indistaz);
  add(&ret, "Processing induced distortions in range = \t%.7f\n", ofdr->indistra);
  add(&ret, "Receiver gain = \t%.7f\n", ofdr->recgain);
  add(&ret, "Swath velocity = \t%.7f\n", ofdr->swathvel);
  add(&ret, "Squint angle = \t%.7f\n", ofdr->squintan);
  add(&ret, "Ave. terrain height above Geoid at center = \t%.7f\n", 
	  ofdr->avgterht);
  add(&ret, "Processor gain = \t%s\n", ofdr->procgain);
  add(&ret, "Flag indicating if Doppler Skew was removed = \t%s\n", 
	  ofdr->deskewf);
  add(&ret, "Ground range / slant range flag = \t%s\n", ofdr->grndslnt);
  add(&ret, "Slant range to the first image pixel = \t%.7f\n", ofdr->sltrngfp);
  add(&ret, "Slant range to the last image pixel = \t%.7f\n", ofdr->sltrnglp);
  add(&ret, "Start sample of signal data range line = \t%i\n", ofdr->strtsamp);
  add(&ret, "Flag indicating whether clutterlock was used = \t%s\n", 
	  ofdr->clttrlkf);
  add(&ret, "Doppler frequency at the near range = \t%.7f\n", ofdr->dpplrfrq);
  add(&ret, "Doppler frequency slope = \t%.7f\n", ofdr->dpplrslp);
  add(&ret, "Doppler frequency quadratic term = \t%.7f\n", ofdr->dpplrqdr);
  add(&ret, "Flag indicating whether autofocus was used = \t%s\n", 
	  ofdr->autfocsf);
  add(&ret, "Doppler frequency rate at the near range = \t%.7f\n", 
	  ofdr->dpplrrat);
  add(&ret, "Doppler frequency rate slope = \t%.7f\n", ofdr->dpratslp);
  add(&ret, "Doppler frequency rate quadratic term = \t%.7f\n", ofdr->dpratqdr);
  add(&ret, "Nominal image resolution in azimuth = \t%.7f\n", ofdr->imresaz);
  add(&ret, "Nominal image resolution in range = \t%.7f\n", ofdr->imresra);
  add(&ret, "Pixel spacing in azimuth = \t%.7f\n", ofdr->azpixspc);
  add(&ret, "Pixel spacing in range = \t%.7f\n", ofdr->rapixspc);
  add(&ret, "On-board range compression flag = \t%s\n", ofdr->rngcompf);
  add(&ret, "Bits per sample of the SAR signal data = \t%i\n", ofdr->bitssamp);
  add(&ret, "Calibrator estimate = \t%.7f\n", ofdr->calestim);
  add(&ret, "Data transfer bit error rate = \t%.7f\n", ofdr->biterrrt);
  add(&ret, "Signal to noise ratio = \t%.7f\n", ofdr->sigtonoi);
  add(&ret, "Estimated noise floor = \t%.7f\n", ofdr->estnoifl);
  add(&ret, "Radiometric resolution = \t%.7f\n", ofdr->radiores);
  add(&ret, "Num. of saturated points determined from hist = \t%i\n",
	  ofdr->nsatpnts);
  add(&ret, "Flag to indicate whether image is within spec = \t%s\n", 
	  ofdr->inspecf);
  if (era) {
    add(&ret, "Chirp replica AGC value\t\t%.7f\n",ofdr->repl_agc);
    add(&ret, "Temp of RCVR LNA\t\t%.7f\n",ofdr->temp_rx_lna);
    add(&ret, "Temp of RCVR subsystem\t\t%.7f\n",ofdr->temp_rx_sub);
    add(&ret, "Temp of RCVR protector\t\t%.7f\n",ofdr->temp_rx_prot);
    add(&ret, "Temp of calib system\t\t%.7f\n",ofdr->temp_cal_sys);
    add(&ret, "RCVR AGC value\t\t\t%.7f\n",ofdr->rx_agc);
    add(&ret, "Pre CAL1 avg power\t\t%.7f\n",ofdr->pre_cal1_pow);
    add(&ret, "Pre CAL2 avg power\t\t%.7f\n",ofdr->pre_cal2_pow);
    add(&ret, "Post CAL1 avg power\t\t%.7f\n",ofdr->post_cal1_pow);
    add(&ret, "Post CAL2 avg power\t\t%.7f\n",ofdr->post_cal2_pow);
    add(&ret, "Replica avg power\t\t%.7f\n",ofdr->repl_pow);
    add(&ret, "Est scansar roll angle\t\t%.7f\n",ofdr->ssar_roll_ang);
  }
  add(&ret, "Comment field for documenting image anomalies = \t%s\n", 
	  ofdr->comment);
  add(&ret, "********** end of Facility Related Data record ****************\n\n");
  return ret;
}

void prn_facdr(FILE *fp, struct VFDRECV *ofdr, int era)
{
    char *rec = sprn_facdr(ofdr, era);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
