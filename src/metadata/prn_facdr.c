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

void prn_facdr(struct VFDRECV *ofdr, int era)
{
printf("\n********** begin of Facility Related Data record **************\n\n");
printf("Data take id = \t%s\n", ofdr->dataid);
printf("SPS image identifier = \t%s\n", ofdr->imageid);
printf("UTC year of image correlation = \t%s\n", ofdr->coryear);
printf("UTC time of image correlation = \t%s\n", ofdr->cortime);
printf("Name of site covered = \t%s\n", ofdr->sitename);
printf("UTC year at the image center = \t%s\n", ofdr->imgyear);
printf("UTC time at the image center = \t%s\n", ofdr->imgtime);
printf("Latitude of the image center = \t%.7f\n", ofdr->imgclat);
printf("Longitude of the image center = \t%.7f\n", ofdr->imgclon);
printf("Lat at start of image frame in near swath = \t%.7f\n", ofdr->nearslat);
printf("Long at start of image frame in near swath = \t%.7f\n", ofdr->nearslon);
printf("Lat at end of image frame in near swath = \t%.7f\n", ofdr->nearelat);
printf("Long at end of image frame in near swath = \t%.7f\n", ofdr->nearelon);
printf("Lat at start of image frame in far swath = \t%.7f\n", ofdr->farslat);
printf("Long at start of image frame in far swath = \t%.7f\n", ofdr->farslon);
printf("Lat at end of image frame in far swath = \t%.7f\n", ofdr->farelat);
printf("Long at end of image frame in far swath = \t%.7f\n", ofdr->farelon);
printf("Actual swath width (km) in azimuth direction = \t%.7f\n", ofdr->swazim);
printf("Actual swath width (km) in range direction = \t%.7f\n", ofdr->swrange);
printf("Actual (no filler) number of pixels per line = \t%i\n", ofdr->npixels);
printf("Actual (no filler) number of image lines = \t%i\n", ofdr->nlines);
printf("Total (with filler) number of pixels per line = \t%i\n", ofdr->apixels);
printf("Total (with filler) number of image lines = \t%i\n", ofdr->alines);
printf("Identification label of the media written to = \t%s\n", ofdr->mediaid);
printf("Location on the DCRSI where data begins = \t%s\n", ofdr->sblock);
printf("Location on the DCRSI where data ends = \t%s\n", ofdr->eblock);
printf("Name of platform for the transmitting sensor = \t%s\n", ofdr->platform);
printf("Sensor and mode of operation = \t%s\n", ofdr->sensmode);
printf("Pulse repitition frequency (prf) = \t%.7f\n", ofdr->prfreq);
printf("SAR antenna look angle = \t%.7f\n", ofdr->antlook);
printf("Data rate = \t%.7f\n", ofdr->datarate);
printf("Data window position = \t%.7f\n", ofdr->datawin);
printf("Range gate delay = \t%.7f\n", ofdr->rangegd);
printf("Track angle to True North = \t%.7f\n", ofdr->trackang);
printf("Flag indicating whether the pass is asc/desc = \t%s\n", ofdr->ascdesc);
printf("Altitude of the spacecraft at image center = \t%.7f\n", ofdr->scalt);
printf("Spacecraft X-position at the image center = \t%.15f\n", ofdr->scxpos);
printf("Spacecraft Y-position at the image center = \t%.15f\n", ofdr->scypos);
printf("Spacecraft Z-position at the image center = \t%.15f\n", ofdr->sczpos);
printf("Spacecraft X-velocity at the image center = \t%.15f\n", ofdr->scxvel);
printf("Spacecraft Y-velocity at the image center = \t%.15f\n", ofdr->scyvel);
printf("Spacecraft Z-velocity at the image center = \t%.15f\n", ofdr->sczvel);
printf("Spacecraft roll at the image center = \t%.14g\n", ofdr->scroll);
printf("Spacecraft yaw at the image center = \t%.14g\n", ofdr->scyaw);
printf("Spacecraft pitch at the image center = \t%.14g\n", ofdr->scpitch);
printf("Quality flag for the spacecraft roll = \t%i\n", ofdr->qroll);
printf("Quality flag for the spacecraft yaw = \t%i\n", ofdr->qyaw);
printf("Quality flag for the spacecraft pitch = \t%i\n", ofdr->qpitch);
printf("Spacecraft roll rate at the image center = \t%.14g\n", ofdr->rroll);
printf("Spacecraft yaw rate at the image center = \t%.14g\n", ofdr->ryaw);
printf("Spacecraft pitch rate at the image center = \t%.14g\n", ofdr->rpitch);
printf("Quality flag for the spacecraft roll rate = \t%i\n", ofdr->rqroll);
printf("Quality flag for the spacecraft yaw rate = \t%i\n", ofdr->rqyaw);
printf("Quality flag for the spacecraft pitch rate = \t%i\n", ofdr->rqpitch);
printf("Radius of the earth at nadir = \t%.7f\n", ofdr->eradnadr);
printf("Radius of the earth at image center = \t%.7f\n", ofdr->eradcntr);
printf("Incidence angle at the center of the image = \t%.7f\n", ofdr->incedang);
printf("Version of the ASP = \t%s\n", ofdr->procvers);
printf("Image processing type identifier = \t%s\n", ofdr->imgproct);
printf("Type of Ephemeris used identifier = \t%s\n", ofdr->ephemert);
printf("Effective number of looks in azimuth = \t%.7f\n", ofdr->nlooksaz);
printf("Effective number of looks in range = \t%.7f\n", ofdr->nlooksra);
printf("Weighting pedestal height in azimuth = \t%.7f\n", ofdr->weightaz);
printf("Weighting pedestal height in range = \t%.7f\n", ofdr->weightra);
printf("Look energy normalization flag = \t%s\n", ofdr->normener);
printf("Processing induced distortions in azimuth = \t%.7f\n", ofdr->indistaz);
printf("Processing induced distortions in range = \t%.7f\n", ofdr->indistra);
printf("Receiver gain = \t%.7f\n", ofdr->recgain);
printf("Swath velocity = \t%.7f\n", ofdr->swathvel);
printf("Squint angle = \t%.7f\n", ofdr->squintan);
printf("Ave. terrain height above Geoid at center = \t%.7f\n", ofdr->avgterht);
printf("Processor gain = \t%s\n", ofdr->procgain);
printf("Flag indicating if Doppler Skew was removed = \t%s\n", ofdr->deskewf);
printf("Ground range / slant range flag = \t%s\n", ofdr->grndslnt);
printf("Slant range to the first image pixel = \t%.7f\n", ofdr->sltrngfp);
printf("Slant range to the last image pixel = \t%.7f\n", ofdr->sltrnglp);
printf("Start sample of signal data range line = \t%i\n", ofdr->strtsamp);
printf("Flag indicating whether clutterlock was used = \t%s\n", ofdr->clttrlkf);
printf("Doppler frequency at the near range = \t%.7f\n", ofdr->dpplrfrq);
printf("Doppler frequency slope = \t%.7f\n", ofdr->dpplrslp);
printf("Doppler frequency quadratic term = \t%.7f\n", ofdr->dpplrqdr);
printf("Flag indicating whether autofocus was used = \t%s\n", ofdr->autfocsf);
printf("Doppler frequency rate at the near range = \t%.7f\n", ofdr->dpplrrat);
printf("Doppler frequency rate slope = \t%.7f\n", ofdr->dpratslp);
printf("Doppler frequency rate quadratic term = \t%.7f\n", ofdr->dpratqdr);
printf("Nominal image resolution in azimuth = \t%.7f\n", ofdr->imresaz);
printf("Nominal image resolution in range = \t%.7f\n", ofdr->imresra);
printf("Pixel spacing in azimuth = \t%.7f\n", ofdr->azpixspc);
printf("Pixel spacing in range = \t%.7f\n", ofdr->rapixspc);
printf("On-board range compression flag = \t%s\n", ofdr->rngcompf);
printf("Bits per sample of the SAR signal data = \t%i\n", ofdr->bitssamp);
printf("Calibrator estimate = \t%.7f\n", ofdr->calestim);
printf("Data transfer bit error rate = \t%.7f\n", ofdr->biterrrt);
printf("Signal to noise ratio = \t%.7f\n", ofdr->sigtonoi);
printf("Estimated noise floor = \t%.7f\n", ofdr->estnoifl);
printf("Radiometric resolution = \t%.7f\n", ofdr->radiores);
printf("Num. of saturated points determined from hist = \t%i\n",ofdr->nsatpnts);
printf("Flag to indicate whether image is within spec = \t%s\n", ofdr->inspecf);
if (era)
 {
  printf("Chirp replica AGC value\t\t%.7f\n",ofdr->repl_agc);
  printf("Temp of RCVR LNA\t\t%.7f\n",ofdr->temp_rx_lna);
  printf("Temp of RCVR subsystem\t\t%.7f\n",ofdr->temp_rx_sub);
  printf("Temp of RCVR protector\t\t%.7f\n",ofdr->temp_rx_prot);
  printf("Temp of calib system\t\t%.7f\n",ofdr->temp_cal_sys);
  printf("RCVR AGC value\t\t\t%.7f\n",ofdr->rx_agc);
  printf("Pre CAL1 avg power\t\t%.7f\n",ofdr->pre_cal1_pow);
  printf("Pre CAL2 avg power\t\t%.7f\n",ofdr->pre_cal2_pow);
  printf("Post CAL1 avg power\t\t%.7f\n",ofdr->post_cal1_pow);
  printf("Post CAL2 avg power\t\t%.7f\n",ofdr->post_cal2_pow);
  printf("Replica avg power\t\t%.7f\n",ofdr->repl_pow);
  printf("Est scansar roll angle\t\t%.7f\n",ofdr->ssar_roll_ang);
 }
printf("Comment field for documenting image anomalies = \t%s\n", ofdr->comment);
printf("********** end of Facility Related Data record ****************\n\n");
return;
}
