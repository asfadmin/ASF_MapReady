/****************************************************************
NAME:  elev

SYNOPSIS:  

  elev [-log <file>] [-quiet] <phase> <base> <meta> <outfile> <seed_file>

DESCRIPTION:

       Elev calculates the elevation at each integrated pixel  in
       an unwrapped phase file.

       The  user also needs to specify a seed point file, identi-
       cal to the format used by tandem_ifm.

       The output file is a float file of the same dimensions  as
       the  input file.  Each value will be a height in meters or
       0 if the pixel is non-integrated.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    .1      10/96        Original Creation
    .2       4/97        Properly handles windowed images - T. Logan
    1.0	     6/97        Gets image size from ddr - O. Lawlor
    1.2      10/97       Eliminated unwrapping mask parameter - O. Lawlor
    1.3      11/97       Changed CLA's to accept list of seed points- O. Lawlor
    2.0      6/98        Re-derived equation; updated for new ceos. O. Lawlor
    2.1      6/00	 Modified to handle files larger than 2GB -D. Koster
    2.2	     7/01	 Added log file switch - R. Gens

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   elev  -  produce  an  elevation file from a deramped igram phase file.  *
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
#include "asf_meta.h"

#define VERSION                2.2

/* local function declaration */
void usage(char *name);

int main(int argc, char **argv)
{
	int x, y, i, optind=1;
	float percent=5.0;
	double xScale,yScale;
	int ss,sl;

	double k;
	double *phase2elevBase,*sinFlat,*cosFlat;
	baseline base;
	meta_parameters *meta;
	char *datafile, *basefile, *outfile,*seedfile;
	FILE *fdata, *fout,*fseed;
	float *f_uwp,*f_elev;
	char *ceos;
	int nrows,ncols/*,arows,acols*/;
	double delta_phase,delta_height;
	double seed_phase,seed_height;
	struct DDR ddr,newddr;

	if (argc==1 || argc>9) usage(argv[0]);

	logflag=quietflag=0;

	/* handle options */
        for (i=1; i<argc; i++) {
          if (strncmp(argv[i], "-log", 4)==0) {
	    sprintf(logFile, "%s", argv[i+1]);
	    fLog = FOPEN(logFile, "a");
	    logflag=1;
            i+=1;
	    optind+=2;
          }
	  else if (strncmp(argv[i], "-quiet", 6)==0) {
	    quietflag=1;
	    optind+=1;
	  }
	  else if (strncmp(argv[i], "-", 1)==0) {
	    sprintf(errbuf, "%s is not a valid option!\n", argv[i]);
	    printErr(errbuf);
	  }
	}

	/* handle required input */
	i=optind;
	datafile = argv[i];
	basefile = argv[i+1];
	ceos=argv[i+2];
	outfile = argv[i+3];
	seedfile= argv[i+4];

	system("date");
	printf("Program: elev\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: elev\n\n");
	}

	/* Get input scene size and windowing info*/
	c_getddr(datafile, &ddr);
	ss = ddr.master_sample - 1;
	sl = ddr.master_line - 1;
	xScale=ddr.sample_inc;
	yScale=ddr.line_inc;
	nrows=ddr.nl;
	ncols=ddr.ns;

	/*Copy DDR fields over.*/
	newddr=ddr;
	newddr.dtype=4;
	newddr.nbands=1;
	c_putddr(outfile,&newddr);

	/* allocate space for vectors and matricies*/
	f_uwp = (float *)MALLOC(sizeof(float)*ncols);
	f_elev =(float *)MALLOC(sizeof(float)*ncols);

	/* Read in values from CEOS */
	meta=meta_init(ceos);
	k=meta_get_k(meta);/*Wavenumber K.*/

	/* read in baseline values*/
	base=read_baseline(basefile);

	/* open data file & get seed phase*/
/*	printf("opening unwrapped phase file %s...\n",datafile);*/
	fdata = fopenImage(datafile, "rb");
	fseed=FOPEN(seedfile,"r");
	/*Use least-squares fit to determine
	  	the optimal seed_phase and seed_height.*/
	{
		double x,xSum=0,xSqrSum=0,hSum=0,hxSum=0,pxSum=0,pxSqrSum=0;
		double a,b,c,d,e,f,det;
		int npts=0;
		while (1)
		{
			float seed_x,seed_y,height,phase;
			int seek_x,seek_y;
			/*Read in each seed point*/
			if (3!=fscanf(fseed,"%f%f%f",&seed_x,&seed_y,&height))
				break;/*Break out when no more points.*/
			seek_x=(int)((seed_x-ss)/xScale);
			seek_y=(int)((seed_y-sl)/yScale);
			FSEEK64(fdata,sizeof(float)*(seek_y*ncols+seek_x),0);
			fread(&phase,sizeof(float),1,fdata);
			if (phase==0)
				continue;/*Escher couldn't unwrap this tie point.*/

			/*Calculate that seed point's impact on fit.*/
			x=meta_phase_rate(meta,base,seed_y,seed_x);
			xSum+=x;
			xSqrSum+=x*x;
			hSum+=height;
			hxSum+=height*x;
			pxSum+=phase*x;
			pxSqrSum+=phase*x*x;
			npts++;
		}
		if (!quietflag) printf("   Read %d seed points\n",npts);
		/*The least-squares fit above leaves us with a matrix equation
			  [ a  b ]   [ seed_phase  ]   [ e ]
			  [      ] * [             ] = [   ]
			  [ c  d ]   [ seed_height ]   [ f ]
			  
			  which has the solution
			  
			  [ d  -b ]   [ e ]    1    [ seed_phase  ]
			  [       ] * [   ] * --- = [             ]
			  [ -c  a ]   [ f ]   det   [ seed_height ]
			  */
		a=-xSqrSum;
		b=xSum;
		c=-xSum;
		d=npts;
		e=hxSum-pxSqrSum;
		f=hSum-pxSum;
		det=a*d-b*c;
		seed_phase=(e*d-f*b)/det;
		seed_height=(e*(-c)+f*a)/det;
	}

	FSEEK64(fdata,0,0);
	if (!quietflag) printf("   Seed Phase: %f\n   Elevation: %f\n",seed_phase,seed_height);

	/* calculate the sine of the incidence angle across cols*/
	sinFlat = (double *)MALLOC(sizeof(double)*ncols);
	cosFlat = (double *)MALLOC(sizeof(double)*ncols);
	phase2elevBase = (double *)MALLOC(sizeof(double)*ncols);
	for (x=0;x<ncols;x++)
	{
		int img_x=x*xScale+ss;
		double incid=meta_incid(meta,0,img_x);
		double flat=meta_flat(meta,0,img_x);
		sinFlat[x]=sin(flat);
		cosFlat[x]=cos(flat);
		phase2elevBase[x]=meta_get_slant(meta,0,img_x)*sin(incid)/(2.0*k);
	}

	/* open other files*/
	fout = fopenImage(outfile,"wb");

	/* loop through each row & calculate height*/
	/*Note:
		To make this faster, we don't call 
	delta_height=delta_phase * meta_phase_rate(ceos,base,y*yScale+sl,x*xScale+ss).
		Instead, we use the annoying temporary arrays
	allocated above to calculate the same thing, quicker.
	*/
	for (y=0;y<nrows;y++) {
		double Bn_y,Bp_y;
		/* read in data */
		FREAD(f_uwp,sizeof(float),ncols,fdata);
		
		/* calculate baseline for this row*/
		meta_interp_baseline(meta,base,y*yScale+sl,&Bn_y,&Bp_y);

		/* step through each pixel in row*/
		for (x=0;x<ncols;x++) {
			if (f_uwp[x]!=0.0) {
				delta_phase = (double)f_uwp[x] - seed_phase;
				delta_height=delta_phase * phase2elevBase[x]/(-Bp_y*sinFlat[x]-Bn_y*cosFlat[x]);
				f_elev[x] = delta_height+seed_height;
			}
			else 
				f_elev[x] = 0.0;
		}
		FWRITE(f_elev,sizeof(float),ncols,fout);
		if ((y*100/nrows)>percent) {
		  printf("   Completed %3.0f percent\n", percent);
		  percent+=5.0;
		}
	}
	printf("   Completed 100 percent\n\n   Wrote %lld bytes of data\n\n", (long long)(nrows*ncols*4));
	if (logflag) {
	  sprintf(logbuf, "   Wrote %lld bytes of data\n\n", (long long)(nrows*ncols*4));
	  printLog(logbuf);
	}

	/* free memory & scram*/
	FREE(f_uwp);
	FREE(f_elev);
	FCLOSE(fout);
	FCLOSE(fdata);
	return(0);
}

void usage(char *name)
{
	printf("\n");
	printf("Usage: %s [-log <file>] [-quiet] <phase> <base> <meta> <outfile> <seed_file>\n",name);
	printf("\n");
	printf("    -log:   Option to have output written to a log <file>.\n"
	       "    -quiet: Option to have output surpressed to essential.\n");
	printf("    phase:  unwrapped phase file (.phase and .ddr)\n");
	printf("    base:   file containing baseline params. used to unwrap\n");
	printf("            format:  Bn_c   dBn   Bp_c   dBp \n");
	printf("    meta:   the name of the file that contains the \n");
	printf("            metadata for image 1 of the interferogram pair.\n");
	printf("    outfile: output file containing elevations.\n");
	printf("    seed_file: tandem_ifm style seed file.\n");
	printf("\n");
	printf("elev: generate DEM from unwrapped phase\n");
	printf("Version: %.2f, ASF SAR TOOLS\n\n",VERSION);
	exit(1);
}
