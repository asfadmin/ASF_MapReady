/********************************************************************
NAME:    get_rectypes.c

SYNOPSIS: get_rectypes sarfile.ext 

DESCRIPTION: Reads an ASF groundstation file and prints the
             names of all of the records found therein

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           4/96   T. Logan (ASF) 
*********************************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ceos.h"

main(argc, argv)
int	argc;
char	**argv;
{
  FILE 	 *fp;
  char 	 buff[20000];
  int	 il, itype, length;
  struct HEADER	bufhdr;

  if (argc != 2) {fprintf(stderr,"Usage : %s filename.ext \n",argv[0]);exit(1);}
  if ((fp = fopen(argv[1], "r")) == NULL)
   { fprintf (stderr, "Unable to open file %s\n", argv[1]); exit (0); }
  while (1)
   {
     if (fread(&bufhdr,sizeof(bufhdr),1,fp)!=1)
       { fprintf(stderr," End of file detected.\n"); exit(0); }
     itype = bufhdr.rectyp[1];
     switch (itype)
      {
	case (10): printf("Data Set Summary Record.\n"); break;
	case (11): printf("Data Record.\n"); break;	
	case (20): printf("Map Projection Data Record.\n"); break;
	case (30): printf("Platform Position Data Record.\n"); break;
	case (40): printf("Attitude Data Record.\n"); break;
	case (50): printf("Radiometric Data Record.\n"); break;
	case (51): printf("Radiometric Compensation Record.\n"); break;
        case (60): printf("Data Quality Summary Record.\n"); break;
	case (70): printf("Data Histograms Record.\n"); break;
	case (80): printf("Range Spectra Record.\n"); break;
	case (90): printf("Digital Elevation Model Descriptor Rec.\n"); break;
	case (100): printf("Radar Parameter Data Update Record.\n"); break;
	case (110): printf("Annotation Data Record.\n"); break;
	case (120): printf("Detailed Processing Parameters Record.\n"); break;
	case (130): printf("Calibration Data Record.\n"); break;
	case (140): printf("Ground Control Points Record.\n"); break;
        case (192): printf("File Descriptor Record.\n"); break; 
        case (200): printf("Facility Related Data Record.\n"); break;
        case (201): printf("GPS Metadata Record.\n"); break;
	case (210): printf("Facility Related Data Record (RADARSAT).\n"); break;
	default: printf("Not Valid Record Type: %d\n",itype);
      }
     length = bufhdr.recsiz - 12;
     if((fseek(fp, length, 1)) != 0) 
      { fprintf(stderr," Error reading data portion of record.\n"); exit(0); }
      printf("\tLength=%i, rectyp[0]=%i, rectyp[1]=%i, rectyp[2]=%i, rectyp[3]=%i\n",length,
      	(int)bufhdr.rectyp[0],
      	(int)bufhdr.rectyp[1],
      	(int)bufhdr.rectyp[2],
      	(int)bufhdr.rectyp[3]);
   }	
}
