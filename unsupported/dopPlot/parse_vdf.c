/***************************************************************************
      PROLOG-- Part of dump_multi_volume
        NAME:  get_names -- extracts file names, record sizes, &
 	       volume info from a Volume Description File (VDF)   
      SYNTAX:  get_names filename
  PARAMETERS:  filename -- name of VDF file to extract names from.	 
 DESCRIPTION:  This program takes the name of a VDF file as input.
	       First, it extracts the volume number, then the total
	       number of volumes.  Then, it scans through the input
	       file extracting all file ids and max record sizes
	       stored therein.  The results are all reported to
	       stdout.
RETURN VALUE:  0 = successful; 2=usage; 3=file not found; 4=can't read file
      AUTHOR:  Shusun Li, GI at UAF, Ak.
	       Modified by Tom Logan 8/93	
       	       Modified by Tom Logan 9/96   - Changed to read new CEOS files
               07/18/97 - Bump exit codes up by 1 to distinguish between
                     a usage call and command not found error- D.Corbett

EXTERNAL VARIABLES:
        argv[0]         command name
        argv[1]         file name
INTERNAL VARIABLES:
        buf             temporary buffer for file name
	buf1	  	temporary buffer for var formatting & cponversion	
	im_id	  	string to concatenate file name & file type		
	tp  		temporary pointer to file structure 
  	i, k		loop counters	
        fp              file pointer (from fopen)
        fopen           external command to open file
***************************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

This software discloses material protectable under copyright 
laws of the United States. Permission is hereby granted to 
use, reproduce, and prepare derivative works for noncommercial 
purposes at no charge, provided that this original copyright 
notice, and disclaimer are retained and any changes are 
clearly documented. Any entity desiring permission to 
incorporate this software or a work based on the software 
into a product for sale must contact the University of 
Alaska Technology Corporation.


This software was authored by:

RICK GURITZ      rguritz@images.alaska    (907)474-7886
Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed 
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
http://www.images.alaska.edu

NEITHER THE UNIVERSITY OF ALASKA NOR ANY SUBUNIT THEREOF, 
NOR ANY OF THEIR EMPLOYEES MAKES ANY WARRANTY, EXPRESS 
OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR 
RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR 
USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR 
PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
NOT INFRINGE PRIVATELY OWNED RIGHTS.
LICENSING INQUIRES MAY BE DIRECTED TO THE UNIVERSITY 
OF ALASKA TECHNOLOGY DEVELOPMENT CORPORATION AT (907)451-0718.
************************************************************/
#include	<stdio.h>
#include	<math.h>
#include	"VDF.h"

int getint(const char *buf, int len)
{
	int val;
	char bufz[255];
	strncpy(bufz,buf,len);
	bufz[len]=0;
	sscanf(bufz,"%i",&val);
	return val;
}
char * getstr(const char *buf, int len)
{
	char *bufz=(char *)malloc(255*sizeof(char));
	strncpy(bufz,buf,len);
	bufz[len]=0;
	return bufz;
}
main(argc, argv)
int	argc;
char	**argv;
{
	int record_number;
      volume_descriptor_record  vdf;
      file_pointer_record	  fpr;
      FILE	*fp;

      if (argc != 2)
       	{ fprintf(stderr,"Usage: %s vdf\n"
       	"  This program prints out the contents of\n"
       	"a VDF file.\n"
       	"ASF SAR Tools, 1997.  Version 0.0\n", argv[0]); exit(2);}

      if ((fp = fopen(argv[1], "rb")) == NULL)
       { fprintf (stderr, "Can't open file %s\n", argv[1]); exit (3); }

      /* Read the Volume Descriptor Record */
      if (fread ((void *)&vdf,sizeof(volume_descriptor_record),1, fp) == 0)
       { fprintf (stderr, "Error *** Can't Read Volume Descriptor Record\n");
	 exit (4);
       }

      /* get this volume number */
      printf("volume %d of %d",getint(vdf.this_volume,2),getint(vdf.total_volumes,2)); 
      printf("\tNum FPRs: %i\n\tNum Records: %i\n",getint(vdf.numFPR,4),getint(vdf.numRecords,4));
      

      /* read a group of file_pointer_records at a time from the file */
      record_number=1;
      while (0!=fread ((void *)&fpr,sizeof(file_pointer_record),1, fp))
      {
		printf("\n\nRecord #%i:\n",record_number);
		printf("\t1-st Record Subtype1: %i\n",(int)fpr.subType1);
		printf("\tSer_num=%i\n",getint(fpr.ser_num,4));
		printf("\tImage ID='%s'\n",getstr(fpr.i_id,16));
		printf("\tImage Type='%s'\n",getstr(fpr.f_type,28));
		printf("\tf_code='%s'\n",getstr(fpr.f_code,4));
		printf("\tN_Lines=%i\n",getint(fpr.n_line,8));
		printf("\tLine_size=%i\n",getint(fpr.max_rec_len,8));
		printf("\tPhys_start=%i\n",getint(fpr.phys_vol_start,2));
		record_number++;
      } /* while(fread) */
      return 0;
}	
