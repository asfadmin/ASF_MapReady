/********************************************************************
FUNCTION NAME:   display_pdr.c

SYNOPSIS: display_pdr <filename> [start] <cnt>

DESCRIPTION:
 reads ASF groundstation .D file & displays values in RADARSAT era
 192 byte "Processed Data Record" leader structure.
 <cnt> leaders are displayed.
 [start] is the first leader to dump; default is 1

RETURN VALUE:	0 indicates failure, 1 indicates success

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERSION     DATE   AUTHOR
    ---------------------------------------------------------------
      1.0       4/98   T. Logan (ASF) Original Creation 

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

*********************************************************************/
/****************************************************************************
*                                                                           *
*   DSIPLAY_PDR - displays processed data records from ASF files.           *
*   Copyright (C) 1997  Alaska SAR Facility                                 *
*                                                                           *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.                                     *
*                                                                           *
*   This program is distributed in the hope that it will be useful,         *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).                                  *
*                                                                           *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software             *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*                                                                           *
*   ASF APD Contacts:                                                       *
*       Special Projects Manager - Rick Guritz  rguritz@asf.alaska.edu      *
*       Lead Software Engineer - Tom Logan      tlogan@asf.alaska.edu       *
*                                                                           *
*       Alaska SAR Facility                     APD Web Site:               *
*       Geophysical Institute                   www.asf.alaska.edu/apd      *
*       University of Alaska Fairbanks                                      *
*       P.O. Box 757320                                                     *
*       Fairbanks, AK 99775-7320                                            *
*                                                                           *
****************************************************************************/
#include "asf.h"
#include "asf_endian.h"
#include "ceos.h"

#define VERS	1.0

void prn_pdr(struct RHEADER *r)
{
  printf("\n");
/*
  printf("RECORD Number                     :\t%i\n",r->recnum);
  printf("Sub-Type1                         :\t%i\n",(int)r->rectyp[0]);
  printf("Record Type                       :\t%i\n",(int)r->rectyp[1]);
  printf("Sub-Type2                         :\t%i\n",(int)r->rectyp[2]);
  printf("Sub-Type3                         :\t%i\n",(int)r->rectyp[3]);
  printf("Record Length                     :\t%i\n",r->recsiz);
*/
  printf("Image Data Line #                 :\t%i\n",r->line_num);
  printf("Image Data Record Index           :\t%i\n",r->rec_num);
  printf("Left fill pixel count             :\t%i\n",r->n_left_pixel);
  printf("Data pixel count                  :\t%i\n",r->n_data_pixel);
  printf("Right fill pixel count            :\t%i\n",r->n_right_pixel);
  printf("Sensor parameter update flag      :\t%i\n",r->sensor_updf);
  printf("Acquisition year                  :\t%i\n",r->acq_year);
  printf("Aquisition day of year            :\t%i\n",r->acq_day);
  printf("Acquisition msecs of day          :\t%i\n",r->acq_msec);
  printf("SAR channel indicator             :\t%i\n",r->sar_cib);
  printf("SAR channel code                  :\t%i\n",r->sar_chan_code);
  printf("Transmitted polarization          :\t%i\n",r->tran_polar);
  printf("Received polarization             :\t%i\n",r->recv_polar);
  printf("Pulse repetition freq, Hz         :\t%f\n",r->prf_bin);
  printf("Slant range first pixel, m        :\t%f\n",r->sr_first);
  printf("Slant range mid-pixel, m          :\t%f\n",r->sr_mid);
  printf("Slant range last pixel, m         :\t%f\n",r->sr_last);
  printf("First pixel Doppler centroid, Hz  :\t%f\n",r->fdc_first);
  printf("Mid-pixel Doppler centroid, Hz    :\t%f\n",r->fdc_mid);
  printf("Last pixel Doppler centroid, Hz   :\t%f\n",r->fdc_last);
  printf("First pixel azimuth FM rate, Hz   :\t%f\n",r->ka_first);
  printf("Mid-pixel azimuth FM rate, Hz     :\t%f\n",r->ka_mid);
  printf("Last pixel azimuth FM rate, Hz    :\t%f\n",r->ka_last);
  printf("Nadir look angle, 10**-6 deg      :\t%f\n",r->nadir_ang);
  printf("Azimuth squint angle, 10**-6 deg  :\t%f\n",r->squint_ang);
  printf("Geographic ref. param update flag :\t%i\n",r->geo_updf);
  printf("First pixel latitude              :\t%i\n",r->lat_first);
  printf("Mid-pixel latitude                :\t%i\n",r->lat_mid);
  printf("Last pixel latitude               :\t%i\n",r->lat_last);
  printf("First pixel longitude             :\t%i\n",r->long_first);
  printf("Mid pixel longitude               :\t%i\n",r->long_mid);
  printf("Last pixel longitude              :\t%i\n",r->long_last);
  printf("Northing of first pixel, m        :\t%i\n",r->north_first);
  printf("Northing of last pixel, m         :\t%i\n",r->north_last);
  printf("Easting of first pixel, m         :\t%i\n",r->east_first);
  printf("Easting of last pixel, m          :\t%i\n",r->east_last);
  printf("Line heading                      :\t%i\n",r->heading);
  printf("\n");
}

int main(int argc, char **argv)
{
  FILE 	*fp;
  char  name[256];
  char 	buff[25600];
  int	length, /* era,*/ cnt;
  struct  RHEADER  bufhdr;
  struct  HEADER   hdr;
  char  filename[256];
  int   start_rec, current_rec;
  

  if (argc != 3 && argc !=4)
    {
      printf("\nUsage: %s file [start] cnt\n",argv[0]);
      printf("\tfile\tASF Groundstation Product Name\n");
      printf("\tcnt\tNumber of Processed Data Leaders Records to Dump\n");
      printf("\tstart\tOptional number of first Leader to dump {default 1}\n");
      printf("\nDisplay_pdr displays processed data records to screen.\n");
      printf("Version %.2f, ASF SAR TOOLS\n\n",VERS);
      exit(1);
    }

  strcpy(filename,argv[1]);
  /* era = */ set_era(filename,name,0);
  
  fp = FOPEN(name,"r");

  if (argc == 4) { start_rec = atoi(argv[2]); cnt = atoi(argv[3]); }
  else { start_rec = 1; cnt = atoi(argv[2]); }
  
  printf("Reading %i leader records starting at number %i\n",cnt,start_rec);

  current_rec = 0;
  while (current_rec < start_rec+cnt)
   {
   
     if (fread (&hdr, sizeof(struct HEADER), 1, fp) != 1)
      { fprintf(stderr," End of file detected.\n"); return(-1); }
     
     if (fread (&bufhdr, sizeof(struct RHEADER), 1, fp) != 1)
      { fprintf(stderr," End of file detected.\n"); return(-1); }
     
     if (current_rec >= start_rec) prn_pdr(&bufhdr);

     length = bigInt32(hdr.recsiz) -
	 (sizeof(struct RHEADER) + sizeof(struct HEADER));
     if((fread(buff, length, 1, fp)) != 1) 
      { fprintf(stderr," Error reading data portion of record.\n"); return(-1);}

     current_rec++;
   }	
 fclose(fp);
 exit(0);
}




