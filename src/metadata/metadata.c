/********************************************************************
NAME:    metadata.c -- MAIN PROGRAM TO READ/TEST CEOS METADATA

SYNOPSIS:     metadata [-f] rectypes infile
 Where, the rectypes are:
		u  		Data Set Summary Record
		m  		Map Projection Data Record
		l  		Platform Position Data Record
		a  		Attitude Data Record
		r  		Radiometric Data Record 
		o  		Radiometric Compensation Record
		q  		Data Quality Summary Record
		p		Processed Data Histogram Record 
		h  		Signal Data Histograms Record
		s  		Range Spectra Record
		e  		Digital Elevation Model Descriptor Record 
		n  		Annotation Data Record
		d  		Detailed Parameter Processing Record 
		c  		Calibration Data Record 
		g  		Ground Control Points Record
		f  		Facility Related Data Record
		i  		Image File Descriptor Record	
		b		Leader File Descriptor Record
		
	infile                  the base name of the SAR image 
	-f                      specifies writing the data to an output file

 NOTE: NOT ALL OF THESE OPTIONS ARE INCLUDED IN THIS RELEASE. SEE USAGE.

DESCRIPTION:
	Reads infile.ext, where ext is one of L, D, ldr, trl, tlr, or dat
        to find the record type specified.  Once found, converts the data
        to a structure of values and prints the values out, either to the
        screen or to file as specified by the -f switch.

        This program handles both old ASF format image triplet (leader,
        data, trailer) and new ASF format pair (leader, data) files.
        New format files are "looked for" first.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           3/96   T. Logan (ASF) 
  1.01          7/96   M. Shindle (ASF) - Corrected minor bug
  2.00	        8/96   T. Logan (ASF)   - Expanded to access RADARSAT data
  2.01	       12/96   T. Logan (ASF)   - Added check_cal() call
  2.02          7/97   D. Corbett       - Don't print mpdr if get_mpdr erred
  2.03          3/98   O. Lawlor (ASF)  - Moved prn_ceos types over to metadata,
                                          which is the only program that ever
					  uses them.
  2.04		6/99   M. Ayers (ASF)   - Added prn_fdr to print file descriptor
					  records.
  2.05		9/01   S. Watts  	- Added case statements to display
					  Signal Data Histogram Records.
  2.3           3/02   P. Denny         - Update command line arguments

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

*********************************************************************/
/****************************************************************************
*								            *
*   Metadata retrieves ceos structures from SAR metadata		    *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ceos.h"
#include "asf.h"
#include "metadisplay.h"
#define VERSION 2.3

int main(int argc, char **argv)
{
  char  dataName[256], leaderName[256];
  int	j, itype=-99, nrecs;
  int   reqrec, era;
  int   fileFlag=0;
  extern int optind;            /* argv index of the next argument */
  extern char *optarg;          /* current argv[] */
  int c;                        /* option letter from getopt() */

  struct VFDRECV         *facdr;
  struct VRADDR          *raddr;
  struct IOF_VFDR        *vfdr;
  struct VMPDREC         *mpdrec;
  struct FDR		 *fdr;
  struct dataset_sum_rec *dssr;
  struct pos_data_rec    *ppdr;
  struct att_data_rec    *atdr;
/*struct radi_comp_rec   *rcr;*/
  struct data_hist_rec   *dhr;
  struct rng_spec_rec    *rsr;
  struct qual_sum_rec    *dqsr;
  

  /* process command line */
  while ((c=getopt(argc,argv,"f")) != EOF)
  {
    switch (c)
    {
      case 'f':
	fileFlag=1;
	break;
      default:
	printf("\n\nInvalid Option: %s\n",argv[optind-1]);
	usage(argv[0]);
	break;	
     }
  }

  if ((argc-optind) != 2)
  {
    if ((argc-optind) > 2) printf("\nToo many inputs.\n");
    if ((argc-optind) < 2) printf("\nToo few inputs.\n");
    usage(argv[0]);
  }

  if (fileFlag) write_to_file(argv[0],argv[optind],argv[optind+1]);

  era = 1;  /* Used to use set_era() to find if data was pre or post RADARSAT
             * Now we don't support pre-RADARSAT, so era is always 1 (post) */

  get_ceos_names(argv[optind+1], dataName, leaderName);
 
  nrecs = (int) strlen(argv[optind]);
  for (j = 0; j < nrecs; j++)
  {
    switch(argv[optind][j]) {
      case ('u') :  reqrec =  10; break;
      case ('m') :  reqrec =  20; break;
      case ('l') :  reqrec =  30; break;
      case ('a') :  reqrec =  40; break;
      case ('r') :  reqrec =  50; break;
      case ('q') :  reqrec =  60; break;
      case ('p') :  reqrec =  70; break;
      case ('h') :  reqrec =  71; break;
      case ('s') :  reqrec =  80; break;
      case ('i') :  reqrec = 192; break;
      case ('f') :  reqrec = 200; break;
      case ('b') :  reqrec = 300; break;
   /* case (???) :  reqrec =  11; break; */
   /* case ('o') :  reqrec =  51; break; */
   /* case ('e') :  reqrec =  90; break; */
   /* case ('p') :  reqrec = 100; break; *//*p is now being used by another*/
   /* case ('n') :  reqrec = 110; break; */
   /* case ('d') :  reqrec = 120; break; */
   /* case ('c') :  reqrec = 130; break; */
   /* case ('g') :  reqrec = 140; break; */
   /* case (???) :  reqrec = 201; break; */
   /* case (???) :  reqrec = 210; break; */
      default: printf("Not a valid record type: %s\n",argv[optind]); reqrec = -1;
    }

    switch (reqrec) {
     case (10): dssr = (struct dataset_sum_rec *) malloc(sizeof(struct dataset_sum_rec));
		get_dssr(leaderName,dssr); prn_dssr(dssr,era); free(dssr);
		break;
     case (20):	mpdrec = (struct VMPDREC  *) malloc (sizeof(struct VMPDREC));
                if ( get_mpdr(leaderName,mpdrec) >= 0 ) { prn_mpdr(mpdrec); }
		else printf("\nNo Map Projection Data Record Found\n");
                free(mpdrec);
		break;
     case (30):	ppdr=(struct pos_data_rec*)malloc(sizeof(struct pos_data_rec));
		get_ppdr(leaderName,ppdr); prn_ppdr(ppdr); free(ppdr);
		break;
     case (40):	atdr=(struct att_data_rec*)malloc(sizeof(struct att_data_rec));
		get_atdr(leaderName,atdr); prn_atdr(atdr); free(atdr);
		break;
     case (50):	raddr = (struct VRADDR  *) malloc (sizeof(struct VRADDR));
		get_raddr(leaderName,raddr); prn_raddr(raddr); free(raddr);
		break;
     case (60):	dqsr=(struct qual_sum_rec*)malloc(sizeof(struct qual_sum_rec));
		if (get_dqsr(leaderName,dqsr) >= 0) { prn_dqsr(dqsr,era); }
		else printf("\nNo Data Quality Summary Record Found\n");
		free(dqsr);
		break;
     case (70):	dhr=(struct data_hist_rec*)malloc(sizeof(struct data_hist_rec));
		if (get_dhr(leaderName,dhr) >= 0) { prn_dhr(dhr); }
		else printf("\nNo Processed Data Histograms Record Found\n");
		free(dhr);
		break;
     case (71): dhr=(struct data_hist_rec*)malloc(sizeof(struct data_hist_rec));
		if (get_sdhr(leaderName,dhr) >= 0) { prn_dhr(dhr); }
		else printf("\nNo Signal Data Histograms Record Found\n");
		free(dhr); 
		break;
     case (80):	rsr=(struct rng_spec_rec*) malloc (sizeof(struct rng_spec_rec));
                if (get_rsr(leaderName,rsr) >= 0) { prn_rsr(rsr); }
		else printf("\nNo Range Spectra Record Found\n");
		free(rsr); 
		break;
     case (192): vfdr=(struct IOF_VFDR *) malloc (sizeof(struct IOF_VFDR));
                 get_ifiledr(dataName,vfdr);prn_ifiledr(vfdr);free(vfdr);break;
     case (200): facdr = (struct VFDRECV  *) malloc (sizeof(struct VFDRECV));
                 if (get_asf_facdr(leaderName,facdr) >= 0) { prn_facdr(facdr,era); }
		 else printf("\nNo Facility Related Data Record Found\n");
		 free(facdr);
		 break;
     case (300): fdr = (struct FDR *) malloc (sizeof(struct FDR));
     		 get_fdr(leaderName,fdr); prn_fdr(fdr); free(fdr);
     		 break;

/********  THESE ARE THE RECORDS THAT I FOUND, BUT THEY WERE BLANK  ***
 *** case (51):	 printf("Radiometric Compensation Record.\n");	    ***
 ***		 printf("This record is not used by ASF\n"); break; ***
 *** case (120): printf("Detailed Processing Parameters Record.\n");***
 ***		 printf("This record is not used by ASF\n"); break; ***
 *** case (130): printf("Calibration Data Record.\n"); 		    ***
 ***		 printf("This record is not used by ASF\n"); break; ***/

/*** I HAVEN'T BEEN ABLE TO FIND THESE RECORDS IN SAR METADATA FILES YET ***
 *** case (11):  printf("Data Record.\n");				 ***
 ***		 printf("\n...UNDER CONSTRUCTION...\n\n"); break;	 ***
 *** case (90):	 printf("Digital Elevation Model Descriptor Rec.\n");    ***
 ***             printf("\n...UNDER CONSTRUCTION...\n\n"); break;        ***	
 *** case (100): printf("Radar Parameter Data Update Record.\n");        ***
 ***             printf("\n...UNDER CONSTRUCTION...\n\n"); break;        ***
 *** case (110): printf("Annotation Data Record.\n");                    ***
 ***             printf("\n...UNDER CONSTRUCTION...\n\n"); break;        *** 
 *** case (140): printf("Ground Control Points Record.\n");              ***
 ***		 printf("\n...UNDER CONSTRUCTION...\n\n"); break;        *** 
 *** case (201): printf("GPS Metadata Record.\n"); 			 ***
 ***		 printf("\n...UNDER CONSTRUCTION...\n\n"); break;        ***
 *** case (210): printf("Facility Related Data Record (RADARSAT).\n"):	 ***
 ***		 printf("\n\n...UNDER CONSTRUCTION...\n\n"); break;      ***/

     default:    printf("Not Valid Record Type: %d\n",itype); 
    }  /* End SWITCH reqrec */
  } /* End FOR j < nrecs */
  return 0;
}

void usage(char *name)
{
   fprintf(stderr,"\n");
   fprintf(stderr,"USAGE:\n");
   fprintf(stderr,"  %s [-f] rectypes infile\n",name);
   fprintf(stderr,"\n");
   fprintf(stderr,"OPTIONS:\n");
   fprintf(stderr,"  -f        Write the records to an output file\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"REQUIRED ARGUMENTS:\n");
   fprintf(stderr,"  rectypes:\n");
   fprintf(stderr,"     u      Data Set Summary Record\n");
   fprintf(stderr,"     m      Map Projection Data Record\n");
   fprintf(stderr,"     l      Platform Position Data Record\n");
   fprintf(stderr,"     a      Attitude Data Record\n");
   fprintf(stderr,"     r      Radiometric Data Record\n");
   fprintf(stderr,"     q      Data Quality Summary Record\n");
   fprintf(stderr,"     p      Processed Data Histograms Record\n");
   fprintf(stderr,"     h      Signal Data Histograms Record\n");
   fprintf(stderr,"     s      Range Spectra Record\n");
   /*
   fprintf(stderr,"     o      Radiometric Compensation Record\n");
   fprintf(stderr,"     e      Digital Elevation Model Descriptor Record\n");
   fprintf(stderr,"     p      Radar Parameter Data Update Record\n");
   fprintf(stderr,"     n      Annotation Data Record\n");
   fprintf(stderr,"     d      Detailed Parameter Processing Record\n");
   fprintf(stderr,"     c      Calibration Data Record\n");
   fprintf(stderr,"     g      Ground Control Points Record\n");
   */
   fprintf(stderr,"     f      Facility Related Data Record\n");
   fprintf(stderr,"     i      Image File Descriptor Record\n");
   fprintf(stderr,"     b      Leader File Descriptor Record\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"  infile    The base name of the SAR image\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"DESCRIPTION:\n");
   fprintf(stderr,"  Metadata retrieves ceos structures from SAR metadata\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"Version %.2f,  ASF SAR TOOLS\n\n",VERSION);
   exit (1);
}


void write_to_file(char *exe, char *rectypes, char *infile)
{
  int i,j, nrecs, reqrec;
  char cmd[256];
  char name[256],outfile[256];

  nrecs = (int) strlen(rectypes);
  for (j = 0; j < nrecs; j++)
  {
    strcpy(name,infile);
    strtok(name,".");
    i=(int)strlen(name);
    name[i]='\0';
    strcpy(outfile,name);
    switch(rectypes[j]) {
      case ('u') : reqrec =  10; strcat(outfile,".dssr");    break;
      case ('m') : reqrec =  20; strcat(outfile,".mpdr");    break;
      case ('l') : reqrec =  30; strcat(outfile,".ppdr");    break;
      case ('a') : reqrec =  40; strcat(outfile,".atdr");    break;
      case ('r') : reqrec =  50; strcat(outfile,".raddr");   break;
      case ('q') : reqrec =  60; strcat(outfile,".dqsr");    break;
      case ('p') : reqrec =  70; strcat(outfile,".pdhr");    break;
      case ('h') : reqrec =  71; strcat(outfile,".sdhr");    break;
      case ('s') : reqrec =  80; strcat(outfile,".rsr");     break;
      case ('i') : reqrec = 192; strcat(outfile,".ifiledr"); break;
      case ('f') : reqrec = 200; strcat(outfile,".facdr");   break;
      case ('b') : reqrec = 300; strcat(outfile,".fdr");     break;  
   /* case ('o') : reqrec =  51; strcat(outfile,".rcr");     break; */
   /* case ('e') : reqrec =  90; strcat(outfile,".demdr");   break; */
   /* case ('p') : reqrec = 100; strcat(outfile,".rpdur");   break; */
   /* case ('n') : reqrec = 110; strcat(outfile,".andr");    break; */
   /* case ('d') : reqrec = 120; strcat(outfile,".dppr");    break; */
   /* case ('c') : reqrec = 130; strcat(outfile,".cdr");     break; */ 
   /* case ('g') : reqrec = 140; strcat(outfile,".gcpr");    break; */
      default: printf("Invalid record type: %c\n",rectypes[j]); reqrec=-1;
    }
    if (reqrec > 0)
    {
      sprintf(cmd,"%s %c %s > %s\n",exe,rectypes[j],name,outfile);
      if (system(cmd) != 0) {printf("Error calling metadata\n"); exit(0);}
    }
  }
  exit(0);
} 
