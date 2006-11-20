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

void prn_facdr(FILE *fp, struct VFDRECV *ofdr, int era)
{
  fprintf(fp, "\n********** begin of Facility Related Data record ***********\n\n");
  fprintf(fp, "Data take id = \t%s\n", ofdr->dataid);
  fprintf(fp, "SPS image identifier = \t%s\n", ofdr->imageid);
  fprintf(fp, "UTC year of image correlation = \t%s\n", ofdr->coryear);
  fprintf(fp, "UTC time of image correlation = \t%s\n", ofdr->cortime);
  fprintf(fp, "Name of site covered = \t%s\n", ofdr->sitename);
  fprintf(fp, "UTC year at the image center = \t%s\n", ofdr->imgyear);
  fprintf(fp, "UTC time at the image center = \t%s\n", ofdr->imgtime);
  fprintf(fp, "Latitude of the image center = \t%.7f\n", ofdr->imgclat);
  fprintf(fp, "Longitude of the image center = \t%.7f\n", ofdr->imgclon);
  fprintf(fp, "Lat at start of image frame in near swath = \t%.7f\n", 
	  ofdr->nearslat);
  fprintf(fp, "Long at start of image frame in near swath = \t%.7f\n", 
	  ofdr->nearslon);
  fprintf(fp, "Lat at end of image frame in near swath = \t%.7f\n", ofdr->nearelat);
  fprintf(fp, "Long at end of image frame in near swath = \t%.7f\n", 
	  ofdr->nearelon);
  fprintf(fp, "Lat at start of image frame in far swath = \t%.7f\n", ofdr->farslat);
  fprintf(fp, "Long at start of image frame in far swath = \t%.7f\n", 
	  ofdr->farslon);
  fprintf(fp, "Lat at end of image frame in far swath = \t%.7f\n", ofdr->farelat);
  fprintf(fp, "Long at end of image frame in far swath = \t%.7f\n", ofdr->farelon);
  fprintf(fp, "Actual swath width (km) in azimuth direction = \t%.7f\n", 
	  ofdr->swazim);
  fprintf(fp, "Actual swath width (km) in range direction = \t%.7f\n", 
	  ofdr->swrange);
  fprintf(fp, "Actual (no filler) number of pixels per line = \t%i\n", 
	  ofdr->npixels);
  fprintf(fp, "Actual (no filler) number of image lines = \t%i\n", ofdr->nlines);
  fprintf(fp, "Total (with filler) number of pixels per line = \t%i\n", 
	  ofdr->apixels);
  fprintf(fp, "Total (with filler) number of image lines = \t%i\n", ofdr->alines);
  fprintf(fp, "Identification label of the media written to = \t%s\n", 
	  ofdr->mediaid);
  fprintf(fp, "Location on the DCRSI where data begins = \t%s\n", ofdr->sblock);
  fprintf(fp, "Location on the DCRSI where data ends = \t%s\n", ofdr->eblock);
  fprintf(fp, "Name of platform for the transmitting sensor = \t%s\n", 
	  ofdr->platform);
  fprintf(fp, "Sensor and mode of operation = \t%s\n", ofdr->sensmode);
  fprintf(fp, "Pulse repitition frequency (prf) = \t%.7f\n", ofdr->prfreq);
  fprintf(fp, "SAR antenna look angle = \t%.7f\n", ofdr->antlook);
  fprintf(fp, "Data rate = \t%.7f\n", ofdr->datarate);
  fprintf(fp, "Data window position = \t%.7f\n", ofdr->datawin);
  fprintf(fp, "Range gate delay = \t%.7f\n", ofdr->rangegd);
  fprintf(fp, "Track angle to True North = \t%.7f\n", ofdr->trackang);
  fprintf(fp, "Flag indicating whether the pass is asc/desc = \t%s\n", 
	  ofdr->ascdesc);
  fprintf(fp, "Altitude of the spacecraft at image center = \t%.7f\n", ofdr->scalt);
  fprintf(fp, "Spacecraft X-position at the image center = \t%.15f\n", 
	  ofdr->scxpos);
  fprintf(fp, "Spacecraft Y-position at the image center = \t%.15f\n", 
	  ofdr->scypos);
  fprintf(fp, "Spacecraft Z-position at the image center = \t%.15f\n", 
	  ofdr->sczpos);
  fprintf(fp, "Spacecraft X-velocity at the image center = \t%.15f\n", 
	  ofdr->scxvel);
  fprintf(fp, "Spacecraft Y-velocity at the image center = \t%.15f\n",
	  ofdr->scyvel);
  fprintf(fp, "Spacecraft Z-velocity at the image center = \t%.15f\n", 
	  ofdr->sczvel);
  fprintf(fp, "Spacecraft roll at the image center = \t%.14g\n", ofdr->scroll);
  fprintf(fp, "Spacecraft yaw at the image center = \t%.14g\n", ofdr->scyaw);
  fprintf(fp, "Spacecraft pitch at the image center = \t%.14g\n", ofdr->scpitch);
  fprintf(fp, "Quality flag for the spacecraft roll = \t%i\n", ofdr->qroll);
  fprintf(fp, "Quality flag for the spacecraft yaw = \t%i\n", ofdr->qyaw);
  fprintf(fp, "Quality flag for the spacecraft pitch = \t%i\n", ofdr->qpitch);
  fprintf(fp, "Spacecraft roll rate at the image center = \t%.14g\n", ofdr->rroll);
  fprintf(fp, "Spacecraft yaw rate at the image center = \t%.14g\n", ofdr->ryaw);
  fprintf(fp, "Spacecraft pitch rate at the image center = \t%.14g\n", 
	  ofdr->rpitch);
  fprintf(fp, "Quality flag for the spacecraft roll rate = \t%i\n", ofdr->rqroll);
  fprintf(fp, "Quality flag for the spacecraft yaw rate = \t%i\n", ofdr->rqyaw);
  fprintf(fp, "Quality flag for the spacecraft pitch rate = \t%i\n", ofdr->rqpitch);
  fprintf(fp, "Radius of the earth at nadir = \t%.7f\n", ofdr->eradnadr);
  fprintf(fp, "Radius of the earth at image center = \t%.7f\n", ofdr->eradcntr);
  fprintf(fp, "Incidence angle at the center of the image = \t%.7f\n", 
	  ofdr->incedang);
  fprintf(fp, "Version of the ASP = \t%s\n", ofdr->procvers);
  fprintf(fp, "Image processing type identifier = \t%s\n", ofdr->imgproct);
  fprintf(fp, "Type of Ephemeris used identifier = \t%s\n", ofdr->ephemert);
  fprintf(fp, "Effective number of looks in azimuth = \t%.7f\n", ofdr->nlooksaz);
  fprintf(fp, "Effective number of looks in range = \t%.7f\n", ofdr->nlooksra);
  fprintf(fp, "Weighting pedestal height in azimuth = \t%.7f\n", ofdr->weightaz);
  fprintf(fp, "Weighting pedestal height in range = \t%.7f\n", ofdr->weightra);
  fprintf(fp, "Look energy normalization flag = \t%s\n", ofdr->normener);
  fprintf(fp, "Processing induced distortions in azimuth = \t%.7f\n", 
	  ofdr->indistaz);
  fprintf(fp, "Processing induced distortions in range = \t%.7f\n", ofdr->indistra);
  fprintf(fp, "Receiver gain = \t%.7f\n", ofdr->recgain);
  fprintf(fp, "Swath velocity = \t%.7f\n", ofdr->swathvel);
  fprintf(fp, "Squint angle = \t%.7f\n", ofdr->squintan);
  fprintf(fp, "Ave. terrain height above Geoid at center = \t%.7f\n", 
	  ofdr->avgterht);
  fprintf(fp, "Processor gain = \t%s\n", ofdr->procgain);
  fprintf(fp, "Flag indicating if Doppler Skew was removed = \t%s\n", 
	  ofdr->deskewf);
  fprintf(fp, "Ground range / slant range flag = \t%s\n", ofdr->grndslnt);
  fprintf(fp, "Slant range to the first image pixel = \t%.7f\n", ofdr->sltrngfp);
  fprintf(fp, "Slant range to the last image pixel = \t%.7f\n", ofdr->sltrnglp);
  fprintf(fp, "Start sample of signal data range line = \t%i\n", ofdr->strtsamp);
  fprintf(fp, "Flag indicating whether clutterlock was used = \t%s\n", 
	  ofdr->clttrlkf);
  fprintf(fp, "Doppler frequency at the near range = \t%.7f\n", ofdr->dpplrfrq);
  fprintf(fp, "Doppler frequency slope = \t%.7f\n", ofdr->dpplrslp);
  fprintf(fp, "Doppler frequency quadratic term = \t%.7f\n", ofdr->dpplrqdr);
  fprintf(fp, "Flag indicating whether autofocus was used = \t%s\n", 
	  ofdr->autfocsf);
  fprintf(fp, "Doppler frequency rate at the near range = \t%.7f\n", 
	  ofdr->dpplrrat);
  fprintf(fp, "Doppler frequency rate slope = \t%.7f\n", ofdr->dpratslp);
  fprintf(fp, "Doppler frequency rate quadratic term = \t%.7f\n", ofdr->dpratqdr);
  fprintf(fp, "Nominal image resolution in azimuth = \t%.7f\n", ofdr->imresaz);
  fprintf(fp, "Nominal image resolution in range = \t%.7f\n", ofdr->imresra);
  fprintf(fp, "Pixel spacing in azimuth = \t%.7f\n", ofdr->azpixspc);
  fprintf(fp, "Pixel spacing in range = \t%.7f\n", ofdr->rapixspc);
  fprintf(fp, "On-board range compression flag = \t%s\n", ofdr->rngcompf);
  fprintf(fp, "Bits per sample of the SAR signal data = \t%i\n", ofdr->bitssamp);
  fprintf(fp, "Calibrator estimate = \t%.7f\n", ofdr->calestim);
  fprintf(fp, "Data transfer bit error rate = \t%.7f\n", ofdr->biterrrt);
  fprintf(fp, "Signal to noise ratio = \t%.7f\n", ofdr->sigtonoi);
  fprintf(fp, "Estimated noise floor = \t%.7f\n", ofdr->estnoifl);
  fprintf(fp, "Radiometric resolution = \t%.7f\n", ofdr->radiores);
  fprintf(fp, "Num. of saturated points determined from hist = \t%i\n",
	  ofdr->nsatpnts);
  fprintf(fp, "Flag to indicate whether image is within spec = \t%s\n", 
	  ofdr->inspecf);
  if (era) {
    fprintf(fp, "Chirp replica AGC value\t\t%.7f\n",ofdr->repl_agc);
    fprintf(fp, "Temp of RCVR LNA\t\t%.7f\n",ofdr->temp_rx_lna);
    fprintf(fp, "Temp of RCVR subsystem\t\t%.7f\n",ofdr->temp_rx_sub);
    fprintf(fp, "Temp of RCVR protector\t\t%.7f\n",ofdr->temp_rx_prot);
    fprintf(fp, "Temp of calib system\t\t%.7f\n",ofdr->temp_cal_sys);
    fprintf(fp, "RCVR AGC value\t\t\t%.7f\n",ofdr->rx_agc);
    fprintf(fp, "Pre CAL1 avg power\t\t%.7f\n",ofdr->pre_cal1_pow);
    fprintf(fp, "Pre CAL2 avg power\t\t%.7f\n",ofdr->pre_cal2_pow);
    fprintf(fp, "Post CAL1 avg power\t\t%.7f\n",ofdr->post_cal1_pow);
    fprintf(fp, "Post CAL2 avg power\t\t%.7f\n",ofdr->post_cal2_pow);
    fprintf(fp, "Replica avg power\t\t%.7f\n",ofdr->repl_pow);
    fprintf(fp, "Est scansar roll angle\t\t%.7f\n",ofdr->ssar_roll_ang);
  }
  fprintf(fp, "Comment field for documenting image anomalies = \t%s\n", 
	  ofdr->comment);
  fprintf(fp, "********** end of Facility Related Data record ****************\n\n");
  return;
}
