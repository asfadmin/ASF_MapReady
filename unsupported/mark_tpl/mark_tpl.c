/******************************************************************************
NAME: mark_tpl - Annotate a LAS image with markers at tie point locations

SYNOPSIS:  mark_tpl [-c size | -i marker] <t|t2|i> <infile> <tplfile> <outfile>

DESCRIPTION:
  At each location (seedfile), a marker (cross) is combined with the input
  file (infile) to create the output file (outfile).  This program will work
  with either terrain correction tie point files, specified by "t," or
  interferometry seed point files (specified by "i").  The marker image used 
  to annotate locations can be provided by the user or the default cross of
  the specified size can be used.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    concatm		Concatenates LAS images based on user specified 
			starting lines and samples.

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    infile.ddr		Metadata (image size)
    infile.img		Input image data - used by concatm
    cfile.ddr		Metadata (cross size)
    cfile.img		Input marker image data - used by concatm

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    3/97   T. Logan     Develop ability to visualize the spatial
				placement of a set of tie point locations.
    2.0     4/97   T. Logan     Allow input of ifm seed points or terrain
				correction tie point files.  Added automatic
				creation of cross file.
    2.1     5/97   T. Logan     Makes multiple concatm calls as needed
    2.1     7/97   D.Corbett    add version number to usage
    2.2     7/98   O. Lawlor    Uninitialized data bug fix.
    2.3     2/99   T. Logan     Allowed use of second set of terrcorr tpl points
    2.4     4/01   T. Logan     CLA for size of cross
    2.5     3/02   P. Denny	Update CLAs

HARDWARE/SOFTWARE LIMITATIONS:
	The input, output, and cross files are all LAS 6.0 single-banded 
	byte images only.  Only a limited number of locations can be used
	per run.

ALGORITHM DESCRIPTION:
  Determine the size of the input (output) file
  Determine the size and ULH corner offsets for cross marker
  for each tie point in seed file
    calculate start line and start sample for cross placement
  execute a single concatm call to create the output image 

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   mark_tpl - Annotate a LAS image with markers at tie point locations     *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/

#include "asf.h"
#include "ddr.h"
#define  TC  1
#define  TC2 2
#define  IFM 3
#define  VERSION 2.5

void usage(char *name);
int make_cross (char *name, int size);
 
int main(int argc,char **argv)
{
  FILE   *fps;
  char   ifile[256], sfile[256], ofile[256], cfile[256];
  char   temp[256], temp2[256];
  int    temp_created=0;
  char   tfile[16]="marked_temp";
  struct DDR in_ddr,cross_ddr;
  int    inl, ins;		/* Input file # lines & # samples       */
  int    cnl, cns, cxm, cym;    /* Cross #line,#samp & middle samp,line */
  float  x, y, tmp1, tmp2;
  int    /*i,*/ ulx, uly, mode;
  char   c[256][80];		/* Array of locations for annotation */
  char   cmd[256];
  int    ntpl, /*base_len,*/ len, cur_tpl;
  char   pre_cmd[255];
  int    cross_flag=0;		/* flag to see if user specified cross */
  extern int optind;            /* argv index of the next argument */
  extern char *optarg;          /* current argv[] */
  int    opt;                     /* option letter from getopt() */

  /* Parse command line arguments
     ---------------------------*/
  while ((opt=getopt(argc,argv,"c:i:")) != EOF)
  {
	if (cross_flag)
	{
	    printf("\nCannot use both -c and -i at the same time\n");
	    usage(argv[0]);
	}
	switch (opt) {
	    case 'c':
		strcpy(cfile,"cross"); 
		make_cross(cfile,atoi(optarg));
		cross_flag=1;
		break;			
	    case 'i':
		strcpy(cfile,optarg);
		cross_flag=1;
		break;
	    default:
 		usage(argv[0]);
		break;	
	}
  }

  if ((argc-optind) != 4)
  {
	if ((argc-optind) > 4) printf("\nToo many arguments.\n");
	if ((argc-optind) < 4) printf("\nToo few arguments.\n");
	usage(argv[0]);
  }
  if (strcmp(argv[optind],"t") == 0) mode = TC;
  else if (strcmp(argv[optind],"t2") == 0) mode = TC2;
  else if (strcmp(argv[optind],"i") == 0) mode = IFM;
  else {printf("\nUnrecognized tie point file type: %s\n",argv[optind]); usage(argv[0]);}

  strcpy(ifile,argv[optind+1]);
  strcpy(sfile,argv[optind+2]);
  strcpy(ofile,argv[optind+3]);

  if (!cross_flag) { strcpy(cfile,"cross"); make_cross(cfile,25); }

  printf("\nAnnotating %s with marker %s to create %s",ifile,cfile,ofile);
  if (mode==IFM) printf(" (mode = IFM)\n");
  else if (mode==TC)  printf(" (mode = TC)\n");
  else printf(" (mode = TC2)\n");


  /* Determine the size of input/output files */
  fps = FOPEN(sfile,"r");
  
  /*i = */ c_getddr(ifile, &in_ddr);
  inl = in_ddr.nl;
  ins = in_ddr.ns;

  /* Determine the size of the cross -- calculate the center point */
  /*i = */c_getddr(cfile, &cross_ddr);
  cnl = cross_ddr.nl;	 	
  cns = cross_ddr.ns;
  cxm = (cns-1) / 2; 
  cym = (cnl-1) / 2;

  printf(" Input Size = %i, %i  Marker Size = %i, %i\n",inl,ins,cnl,cns);
  printf(" Reading tie point locations from %s\n",sfile);
  /* printf("           LINE SAMP          LINE SAMP\n"); */
  ntpl = 0;
  if (mode == IFM)
   while (fscanf(fps,"%f %f %f\n",&x,&y,&tmp1) != EOF)
    {
       x=(x-in_ddr.master_sample)/in_ddr.sample_inc;
       y=(y-in_ddr.master_line)/in_ddr.line_inc;
       ulx = (int) x - cxm; uly = (int) y - cym;
       sprintf(c[ntpl],"%s %i %i ",cfile,uly,ulx);
       /* printf("  seed %2i: %4.0f %4.0f - ", ntpl+1, y, x);
          printf(" cross: %4i %4i\n"uly,ulx); */
       ntpl++;
    }
  else if (mode == TC)
   while (fscanf(fps,"%f %f %f %f\n",&y, &x, &tmp1, &tmp2) != EOF)
    {
       ulx = (int) x - cxm; uly = (int) y - cym;
       sprintf(c[ntpl],"%s %i %i ",cfile,uly,ulx);
       /* printf("  seed %2i: %4.0f %4.0f - ",ntpl+1, y, x);
	  printf(" cross: %4i %4i\n",uly,ulx); */
       ntpl++;
    }
  else  /* mode == TC2 */
   while (fscanf(fps,"%f %f %f %f\n",&tmp1, &tmp2, &y, &x) != EOF)
    {
       ulx = (int) x - cxm; uly = (int) y - cym;
       sprintf(c[ntpl],"%s %i %i ",cfile,uly,ulx);
       /* printf("  seed %2i: %4.0f %4.0f - ",ntpl+1, y, x);
	  printf(" cross: %4i %4i\n",uly,ulx); */
       ntpl++;
    }
   

  sprintf(pre_cmd,"concatm %s %i %i ",ofile, inl, ins);
  /*base_len = strlen(pre_cmd);*/
  cur_tpl = 0;

  /* Break the command up into 256 or less characters per system call */
  while (cur_tpl < ntpl)
    {
      sprintf(cmd,"%s %s 1 1 ",pre_cmd,ifile);
      len = strlen(cmd);
      while (cur_tpl < ntpl && (len+strlen(c[cur_tpl]))< 256)
	{
	  strcat(cmd,c[cur_tpl]);
	  len += strlen(c[cur_tpl]);
	  cur_tpl++;
        }
      printf("\n EXECUTING concat manual:\n\n");
      printf("  %s\n",cmd);
      system(cmd);

      if (cur_tpl < ntpl)  /* We aren't done yet! */
        {
      	  printf("Not all tie points copied; moving to temp. file.\n");
          temp_created=1;
	  sprintf(temp,"%s.img",ofile);
	  sprintf(temp2,"%s.img",tfile);
	  printf("Renaming %s to %s\n",temp, temp2);
	  rename(temp,temp2);
	  sprintf(temp,"%s.ddr",ofile);
	  sprintf(temp2,"%s.ddr",tfile);
	  printf("Renaming %s to %s\n",temp, temp2);
	  rename(temp,temp2);
	  strcpy(ifile,tfile);
        }
    }

  /* Remove any temporary files that we created */
  if (temp_created) 
    {
      printf("Removing temp files...\n");
      strcat(strcpy(temp,tfile),".img");
      unlink(temp);
      strcat(strcpy(temp,tfile),".ddr");
      unlink(temp);
    }

  /* Clean Up the Cross file */
  if (argc != 6) 
    {
      strcat(strcpy(temp,cfile),".img");
      if (remove(temp)==-1) perror("Can't remove cross");
      strcat(strcpy(temp,cfile),".ddr");
      if (remove(temp)==-1) perror("Can't remove cross");
    }

  /* Copy input ddr to output ddr to save the pixel size */
  strcat(ofile,".ddr");
  strcat(ifile,".ddr");
  sprintf(temp,"cp -r %s %s\n",ifile,ofile);
  system(temp);

  printf("MARK_TPL COMPLETED: ADDED %i MARKERS\n\n",cur_tpl);

  exit(1);
}

void usage(char *name)
 {
  printf("\nUSAGE:\n");
  printf("   %s  [-c size | -i marker] <mode> <infile> <tplfile> <outfile>\n",name);
  printf("\n");
  printf("REQUIRED ARGUMENTS:\n");
  printf("  <mode>    - specify the input tie point file type\n");
  printf("              t  = terrain correction tie point file\n");
  printf("              t2 = terrain correction tie point file (2nd column)\n");
  printf("              i  = interferometry seed point file\n");
  printf("  <infile>  - LAS byte image to be marked\n");
  printf("  <tplfile> - input tie point file of the form:\n");
  printf("              t = tie point file (line, samp, line, samp)\n");
  printf("              i = elevation file (samp, line, ht)\n");
  printf("  <outfile> - LAS byte image with marker at given locations\n");
  printf("\n");
  printf("OPTIONS:\n");
  printf("  -c size   - Use a default cross of given size (should be an odd number)\n");
  printf("  -i marker - LAS byte image used as the marker.\n");
  printf("                If neither -c or -i option is used, defaults to\n"
	 "		  cross of size 25\n\n"
  	 "mark_tpl -- Annotate a LAS image with markers at tie point locations");
  printf("\nVersion %0.2f, ASF SAR TOOLS\n\n",VERSION);
  exit(1);
 }
