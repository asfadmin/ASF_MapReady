/******************************************************************
NAME:	    maskgen.c -	generates a series of mask images for a swath given a 
						raw state vector and then concatenates them into one

SYNOPSIS:   maskgen <inRawSVfile> <inDEMfile> <outBaseName> [-l]
					-l		option to leave behind temporary files

DESCRIPTION:


EXTERNAL ASSOCIATES:
	NAME:				USAGE:
	--------------------------------------------------------
	propagate			Propagate one ERS1 raw state vector forward
	prop2ll				Generate a list of lat/lon coordinates for near 
						and far edges of a nominal ERS1 swath given the
						list of propagated state vectors
	ll2proj				Convert the list of lat/lon coordinates to the projection used.
	projfit				Find coefficients to model the swath edges
	demclip				Modified demclip program which clips the DEM and
						setting the area outside the swath to zero
	sarsim				Modified sarsim to generate the separate mask images
						using only a .ssv file and a DEM
	concat				concatenates the mask images to make a big one

FILE REFERENCES:
	---------------------------------------------------------
    	inRSVfile:
		x(km)  y(km)  z(km)  x'(m/s)  y'(m/s)  z'(m/s)
		year, julian day, gmtSec
		delta time

	outBaseName: 	Output base name for clipped DEM files and their
					corresponding state vector files
		
		Temporary base files:
		base.coefs
		base01.img base02.img ...
		base01.ddr base02.ddr ...
		base01.ssv base02.ssv ...
		base01.cnr base03.cnr ...
		base01_mask.img base02_mask.img ...
		base01_mask.ddr base02_mask.ddr ...			
				
		Output files:
		base_mask.img base_mask.ddr

	 -->SSV file format (.ssv):
		x(m)	y(m)	z(m)	z'(m/s)   y'(m/s)   z'(m/s)
		...(x3 for start, center, and end scene)
		year, julian day, gmtSec
		delta time
		
	Other temporary files:
		propInVec.1 propOutVec.1 LatLon.1 PROJ.1 trash.img
		
PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:         PURPOSE
    --------------------------------------------------------------
    1.0      8/00   M. Jessop	    Create a mask of shadows and layover
				    errors in a given orbit for a given
				    UTM projected DEM.
    2.0	    11/00   J. Badgley	    Allow the use of any projection type.
    
******************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the
University of Alaska Technology Corporation, below.

Legalese:
                 COPYRIGHT NOTIFICATION
(C) COPYRIGHT 2000 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

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
MIKE JESSOP
under the supervision of
RICK GURITZ      rguritz@images.alaska    (907)474-7886
Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195
Any questions or comments on the software may be directed
to one of the authors: Rick Guritz, and Tom Logan; or to
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


#include "asf.h"
#include "dateUtil.h"


void execute(char *cmd)
{
	printf("%s",cmd);
	fflush(stdin);
	if (system(cmd)!=0) { printf("Command execution failed:  %s\n",cmd); exit(1); }
}

int execute_butdontabort(char *cmd)
{
	printf("%s",cmd);
	fflush(stdin);
	if (system(cmd)==0) return(0); else return(1);
}


void findextreme(v1,v2,v3,v4,extreme,flag)
int 	flag;
double	v1,v2,v3,v4,
		*extreme;
{
	int 	i;
	double	vals[5];
	
	vals[1]=v1;vals[2]=v2;vals[3]=v3;vals[4]=v4;
	if (flag==-1) {
		*extreme=999999999;
		for (i=1; i<=4; i++) { if (vals[i]<*extreme) *extreme=vals[i]; }
	}
	else {
		*extreme=-999999999;
		for (i=1; i<=4; i++) { if (vals[i]>*extreme) *extreme=vals[i]; }
	}
}

main (int argc, char *argv[]) {
    int	i, j,
    	pnum,			/* number of vectors to propagate ahead */
    	numpts,			/* number of points within lat/lon constraints */
    	start,end,
    	index1, index2,	/* index positions for starting and ending */
    	step,			/* step forward (+1) or backward (-1) depending on ascflag */
    	flag=0,	
    	abort_flg=0,	/* when demclip or sarsim fails */
    	leavetmp_flg=0,	/* leave behind temporary files if the option is given */
    	ascflag;		/* ascending/descending pass flag */
    
    char 	cmd[255], inVec[255], outVec[255], llfile[255], basename[255],
    		RSVfile[255], projfile[255], DEMimg[255], DEMddr[255],
    		cfsfile[255], tmpfname[255], starttime[30];
    double	rsvdeltime,
    		timeref[4],			/* timeref of raw state vector */
    		rsv[7],				/* The raw state vector for propagating */
    		deltimeref,			/* Delta time between state vectors */
    		hilat, lolat,		/* Nominal high and low latitude constraints */
    		toffset[50],		/* Time offset from scene start */
    		lat[3][50],			/* List of lat and lon values for near and */
    		lon[3][50],			/*   far edge of swath at each ssv point */
    		minlat, maxlat,		/* Extreme coordinates for DEM corners */
    		minlon, maxlon,
    		ssv[7][50],			/* List of propagated state vectors */
    		tmp;				/* Temporary value holder */
	
	julian_date jld;
	ymd_date	ymd;
	
	FILE *fpi, *fpo;
	
	/* Initialize */
	deltimeref = 60.0;
	hilat = 71.5;
	lolat = 53.0;
	
	/* Test for proper arguments */
    if (argc != 4 && argc != 5) { 
    	printf("Usage: %s <inRSVfile> <inDEMfile> <outBaseName> [-l]\n",argv[0]);
    	printf("\t-l\toption to leave behind temporary files\n");
    	exit(1); }
    
    if (argc==5) leavetmp_flg=1;
    
    /* Record start time */
    sprintf(cmd,"date > start_time.temp\n"); fflush(stdin); system(cmd);
    fpi = fopen("start_time.temp","r"); fread(starttime,28,1,fpi); fclose(fpi);
    starttime[28]='\0';
    sprintf(cmd, "rm -f start_time.temp\n"); fflush(stdin); system(cmd);
    
    strcat(strcpy(RSVfile, argv[1]), ".rsv");
    strcpy(DEMimg, argv[2]);
    strcat(strcpy(DEMddr, argv[2]), ".ddr");
    strcpy(basename, argv[3]);
    strcat(strcpy(cfsfile,basename),".coefs");
    strcpy(inVec, "propInVec.1");
	strcpy(outVec, "propOutVec.1");
	strcpy(llfile, "LatLon.1");
	strcpy(projfile, "PROJ.1");
    
    /* Open input file and read the first state vector and the timeref info */
    fpi = fopen(RSVfile, "r");
    if ( fpi == (FILE *) NULL) {
        printf("\n*** couldn't open data file: %s\n", RSVfile);
        exit(1); }
    for (i=1; i<=6; i++) {
    	fscanf(fpi, "%lf", &tmp);
    	rsv[i]=tmp; }
    for (i=1; i<=3; i++) { fscanf(fpi, "%lf", &timeref[i]); }
    fscanf(fpi,"%lf",&rsvdeltime);
    fclose(fpi);
    
    pnum = (int) (2*rsvdeltime / deltimeref);
    
    /* Find month and day from julian day (required for propagate) */
    jld.year = (int) timeref[1];
    jld.jd = (int) timeref[2];
    date_jd2ymd(&jld,&ymd);    
    
    /*	LONG LIST GENERATION for finding COEFs
    	Write the inVec file for propagate and run */
    fpo = fopen(inVec,"w");
    for (i=1; i<=6; i++) { fprintf(fpo, "%lf\n", rsv[i]); }
    fprintf(fpo,"%d\n%d\n%d\n",ymd.year,ymd.month,ymd.day);
    fprintf(fpo,"%lf\n",timeref[3]);
    fprintf(fpo,"%d\n%d\n%d\n",ymd.year,ymd.month,ymd.day);
    fprintf(fpo,"%lf\n",timeref[3]+pnum*deltimeref);
    fprintf(fpo,"%d\n",(int)(2*rsvdeltime));
    fclose(fpo);
    sprintf(cmd,"propagate %s %s\n",inVec,outVec); execute(cmd);
    
    /* Run prop2ll, ll2proj, and projfit to generate the base.coefs file*/
    sprintf(cmd,"prop2ll %s %s %s\n",RSVfile,outVec,llfile); execute(cmd);
    sprintf(cmd,"ll2proj %s %s %s\n",DEMimg,llfile,projfile); execute(cmd);
    sprintf(cmd,"projfit %s %s\n",projfile,cfsfile);execute(cmd);        
    
    /*	SHORT LIST GENERATION for clipping 
    	Write the inVec file for propagate and run */
    fpo = fopen(inVec,"w");
    for (i=1; i<=6; i++) 
	{ fprintf(fpo, "%lf\n", rsv[i]); }
    fprintf(fpo,"%d\n%d\n%d\n",ymd.year,ymd.month,ymd.day);
    fprintf(fpo,"%lf\n",timeref[3]);
    fprintf(fpo,"%d\n%d\n%d\n",ymd.year,ymd.month,ymd.day);
    fprintf(fpo,"%lf\n",timeref[3]+pnum*deltimeref);
    fprintf(fpo,"%d\n", pnum);
    fclose(fpo);
    sprintf(cmd,"propagate %s %s\n",inVec,outVec); execute(cmd);
    
    /* Run prop2ll to convert list to lat/lon */
    sprintf(cmd,"prop2ll %s %s %s\n",RSVfile,outVec,llfile); execute(cmd);
    
    /* Read state vectors and lat/lon positions from files and into arrays */
    fpi = fopen(outVec,"r");
    for (i=1; i<=pnum+1; i++) {
    	fscanf(fpi,"%lf",&tmp);
    	for (j=1; j<=6; j++) {
    		fscanf(fpi,"%lf",&tmp);
    		ssv[j][i] = tmp*1000; }
    }
    fclose(fpi);
    fpi = fopen(llfile,"r");
    for (j=1; j<=3; j++) { fscanf(fpi,"%lf",&tmp); }
    for (i=1; i<=pnum+1; i++) {
    	fscanf(fpi,"%lf",&toffset[i]);
    	fscanf(fpi,"%lf %lf %lf %lf", &lat[1][i], &lon[1][i], &lat[2][i], &lon[2][i]);
    }
    fclose(fpi);
    
    /* Find out index points where AK starts and ends using nominal hilat and lolat */
    ascflag = -1;
    if (lat[1][1]>hilat) ascflag=0;		/* descending pass */
    if (lat[1][1]<lolat) ascflag=1;		/* ascending pass  */
    
    printf("ascflag %d\n",ascflag);		

    if (ascflag) pnum--;
    /* ?? for some reason propagate sometimes writes one too many points to
    	file and sometimes it doesn't, which messes up everything when the pass
    	is ascending because this program looks at the last state vector first
    	for those passes.  So the statement above is a temp fix
    			-->> this could be fixed in prop2ll, which reads the
    				 output from propagate, but I am out of time!! */
    
    if (ascflag==-1) {
    	printf("ERROR: rsv file doesn't cover latitudes: %2.1lf to %2.1lf\n",lolat,hilat);
    	printf("It covers from %2.1lf to %2.1lf\n",lat[1][1],lat[1][pnum+1]);
    	exit(1); }
    if (!ascflag)	{ start=1; end = pnum+2; step=1; }
    		else	{ start=pnum+1; end = 0; step=-1; }
    for (i=start; i!=end; i=i+step) {
    	if ((flag==0) && (lat[1][i]<hilat)) { index1 = i-step; flag=1; }
    	if ((flag==1) && (lat[1][i]<lolat)) { index2 = i; flag=2; }    	
    	printf("*** lat, lon, flag: %lf %lf %d\n",lat[1][i],lon[1][i],flag);
    }
    if (!flag==2) {
    	printf("ERROR: rsv file doesn't cover AK latitudes: %2.1lf to %2.1lf\n",lolat,hilat);
    	printf("It covers from %2.1lf to %2.1lf\n",lat[1][1],lat[1][pnum+1]);
    	exit(1); }
    if (index1>index2) { tmp=index1; index1=index2; index2=tmp; }
    if (index1<1) index1=1;				/* checking bounds */
    if (index2>pnum+1) index2=pnum+1;
    
    numpts = index2-index1+1;			

    for (i=1; i<=numpts; i++) {
    	for (j=1; j<=6; j++) { ssv[j][i] = ssv[j][i+index1-1]; }
    	for (j=1; j<=2; j++) {
    		lat[j][i] = lat[j][i+index1-1];
    		lon[j][i] = lon[j][i+index1-1];
    	}
    	toffset[i] = toffset[i+index1-1];
    }
    
    if (numpts<3) { printf("ERROR: not enough points... check rsv file\n"); exit(1); }
    if ((numpts%2)==0) { numpts--; flag=1; } else flag=0;   
    	/*		flag is used now to tell whether there
    			is an even or an odd number of points
    		-- if it is even it will generate the last mask overlapping half
    	  	   of the one right before it rather than exceeding the bounds
    	*/
    
    /* Determine number of images that will be generated */
    for (i=-1,j=0; i<numpts-2;j++) { i=i+2; if ((flag) && (i==numpts-2)) i--; }
    printf("\nNUMBER OF MASK IMAGES TO GENERATE OVER ALASKA: %d\n\n", j);
	if (flag) printf(">>NOTE: overlapping will occur with the last two\n\n");
	
    /* Loop and generate masks */
    for (i=-1,j=1; i<numpts-2;)
    {
    	abort_flg=0;
    	i=i+2;
    	
    	/* Write lat/lon corner files for clipping */
    	findextreme(lat[1][i],lat[2][i],lat[1][i+2],lat[2][i+2],&minlat,-1);
    	findextreme(lat[1][i],lat[2][i],lat[1][i+2],lat[2][i+2],&maxlat,1);
    	findextreme(lon[1][i],lon[2][i],lon[1][i+2],lon[2][i+2],&minlon,-1);
    	findextreme(lon[1][i],lon[2][i],lon[1][i+2],lon[2][i+2],&maxlon,1);
    	    	
    	fpo = fopen(basename,"w");
    	fprintf(fpo,"%lf %lf %lf %lf\n",maxlat,minlon,minlat,maxlon);
    	fclose(fpo);
    	sprintf(cmd,"mv %s %s_%02d.cnr\n",basename,basename,j); execute(cmd);
    	
    	/* Write ssv files */
    	fpo = fopen(basename,"w");
    	for (index1=i;index1<=i+2;index1++) {
    		for (index2=1;index2<=6;index2++)
    			{ fprintf(fpo,"%lf ",ssv[index2][index1]); }
    		fprintf (fpo,"\n");
    	}
    	fprintf(fpo,"%lf %lf %lf\n",timeref[1],timeref[2],timeref[3]+toffset[i+1]);
    	fprintf(fpo,"%lf\n",deltimeref);
    	fclose(fpo);
    	sprintf(cmd,"mv %s %s_%02d.ssv\n",basename,basename,j); execute(cmd);
    	
    	/* Use modified demclip to clip out each area */
    	sprintf(cmd,"demclip %s %s_%02d.cnr %s.coefs %s_%02d u\n",DEMimg,basename,j,basename,basename,j);
    	if (execute_butdontabort(cmd)!=0)  abort_flg=1;
    	
    	/* Use sarsim to generate mask for each clipped area */
    	/* Don't run if demclip failed */
    	if (!abort_flg) {
    		sprintf(cmd,"sarsim %s_%02d %s_%02d trash -ks\n",basename,j,basename,j);
    		if (execute_butdontabort(cmd)!=0)  abort_flg=1;
    	}
		
    	/* Remove clipped DEM */
    	if (!leavetmp_flg) {
    		sprintf(cmd,"rm -f %s_%02d.*\n",basename,j);
			execute(cmd); }
			
		if (!abort_flg) j++;	/* step to the next file unless demclip or sarsim aborted */
		
    	if ((flag) && (i==numpts-2)) i--;  /* back-up one to generate the last mask */
    }    	
	
	/* Clean up temporary files */
	sprintf(cmd,"rm -f trash.img\n"); fflush(stdin); system(cmd);
	
	if (!leavetmp_flg) {
		sprintf(cmd,"rm -f propInVec.1 propOutVec.1 LatLon.1 PROJ.1 %s.coefs\n",basename);
		execute(cmd);
	}
	
	printf ("\nNUMBER OF MASKS SUCCESSFULLY GENERATED: %d\n\n",j-1);
	if (j<=1)	{printf("ERROR: no coverage,  check input rsv file\n"); exit(1);}
	
	/* Concatenate to make a final mask */
	sprintf(cmd,"concat %s_mask",basename);
	for (i=1; i<j; i++) {
		sprintf(tmpfname," %s_%02d_mask",basename,i);
		strcat(cmd,tmpfname); }
	strcat(cmd,"\n");
	if (execute_butdontabort(cmd)==0) {
		/* leave these behind if concat failed */
		sprintf(cmd,"rm -f %s_??_mask.img %s_??_mask.ddr\n",basename,basename);
		execute(cmd); }
		
	/* Report start time and end time, then finished */
	printf("%s\n",starttime);
	sprintf(cmd,"date\n"); fflush(stdin); system(cmd);
	printf("%s sucessfully completed!\n\n",argv[0]); 
    return(0);
}
