/*****************************************************************************
NAME: dop_prf: Doppler PRF Determination Software

SYNOPSIS: dop_prf: Same parameters as ARDOP.

	Dop_prf attempts to determine the doppler ambiguity
by "sub-aperture correlation"-- it processes the positive
half of the azimuth reference function; the negative half;
then correlates the two along range.  

	If the two images are identical, the doppler is correct.
If the two images are offset by some fraction of a pixel, the
doppler is incorrect.  The direction of the offset tells
you the direction the doppler is off; the magnitude of the
offset tells you how many PRF it is off.


EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    ifile.D		 Input CCSD data file
    ifile.L		 Input CCSD leader file
    ofile.cpx		 Output float complex image file
    ofile.in		 Parameter file used to create ofile
    ofile.amp		 Multilooked float amplitude file 

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0	    1996   T. Logan     Initial C implementation from H. Zebker roi.f
    0.1	    2/97   T. Logan     Added Doppler Estimation Subroutine Call
    0.2     2/97   T. Logan     Added read_dopplr & read_offsets
    1.0	    1/98   O. Lawlor	Added ability to process RADARSAT data
    1.1	    5/98   O. Lawlor	Generalized I/O, slight cleaning.
    2.0	    8/98   O. Lawlor	Globals now confined to ardop_setup.c.
    				Can read image info from .fmt, .replica.

HARDWARE/SOFTWARE LIMITATIONS:
    This program requires large amounts of memory to run.  The main
    buffer is 
	      size(trans) = n_az * n_range * sizeof(complexFloat)

              where  n_az = number of lines in a patch (azimuth samples)
		     n_range = number of lines in the azimuth (range samples) 

    n_az is defined is ardop_def.h as 4096, and a full swath of ERS CCSD
    data includes 5616 range samples, so size(trans) = 176 Mbytes.  When 
    this is combined with the rest of the storage requirements for the
    program, 200+ Mbytes are needed.

    Because of the 1000+ azimuth lines of overhead per patch, it is best
    NOT to decrease the defined value of n_az.  Rather, one should decrease
    n_range by processing fewer range bins at a time.  Regardless, n_az should
    always be a power of 2 because the FFTs operate best that way.

ALGORITHM DESCRIPTION:
    Deal with setting all of the parameters
    For each patch
	read the patch and range compress (rciq)
	transform via fft
	perform range migration (rmpatch)
	perform azimuth compression (acpatch)
	transpose the patch and write it in azimuth lines to output

ALGORITHM REFERENCES:
    This program and all subroutines were converted from Fortran programs
    donated by Howard Zebker, and extensively modified.

BUGS:

*****************************************************************************/
#include "asf.h"
#include "ardop_defs.h"
/********
findPeak:
	Given a 1-D array of floats, finds the maximum value and
interpolates to return the floating-point index of the peak.
*/
double dop_findPeak(float *arr,int start,int len)
{
	int i;
	int bestIndex=-1;
	float bestVal=-100000000;
	double a,b,c,d;
	double ret;
	for (i=start;i<start+len;i++)
		if (bestVal<arr[i])
		{
			bestVal=arr[i];
			bestIndex=i;
		}
	if (bestIndex==-1) {
	  sprintf(errbuf, "   ERROR: Logic error in findPeak-- Array has no maximum!\n");
	  printErr(errbuf);
	}
	a=arr[bestIndex-1];
	b=arr[bestIndex];
	c=arr[bestIndex+1];
	d=4*((a+c)/2-b);
	if (d!=0)
		ret=(a-c)/d;
	else ret=0;
	return bestIndex+ret;
}
/*******
amp_corr:
	Given two patches, p1 containing the echos from
ahead of beam center, p2 containing echos from behind beam
center; performs an amplitude cross-correlation in range
and returns the range offset, in pixels, between
these two images.
*/
double amp_corr(patch *p1,patch *p2,file *f)
{
	int clip=20;/*Remove this from the far-range end of the images*/
	int overlap=30;/*Overlap is the maximum shift we ever expect.*/
	FILE *fout;
	int x,y;
	int fftLen=smallestPow2(p1->n_range);
	float scale=1.0/fftLen;
	complexFloat *amp1,*amp2;
	float *corr;
	amp1=(complexFloat *)MALLOC(sizeof(complexFloat)*fftLen);
	amp2=(complexFloat *)MALLOC(sizeof(complexFloat)*fftLen);
	corr=(float *)MALLOC(sizeof(float)*(fftLen+overlap));
	if (!quietflag) printf("   Performing amplitude correlation.\n");
	for (x=0;x<fftLen+overlap;x++)
		corr[x]=0.0;
	cfft1d(fftLen,NULL,0);
	p1->n_range-=clip;
	p2->n_range-=clip;
	for (y=f->firstOutputLine;y<f->firstOutputLine+f->n_az_valid;y++)
	{
		for (x=0;x<overlap;x++)
		{
			amp1[x]=Czero();
			amp2[x].r=Cabs(p2->trans[x*p2->n_az+y])*scale;
			amp2[x].i=0.0;
		}
		for (;x<p1->n_range-overlap;x++)
		{
			amp1[x].r=Cabs(p1->trans[x*p1->n_az+y])*scale;
			amp1[x].i=0.0;
			amp2[x].r=Cabs(p2->trans[x*p2->n_az+y])*scale;
			amp2[x].i=0.0;
		}
		for (;x<p1->n_range;x++)
		{
			amp1[x]=Czero();
			amp2[x].r=Cabs(p2->trans[x*p2->n_az+y])*scale;
			amp2[x].i=0.0;
		}
		for (;x<fftLen;x++)
			amp1[x]=amp2[x]=Czero();
		cfft1d(fftLen,amp1,-1);
		cfft1d(fftLen,amp2,-1);
		for (x=0;x<fftLen;x++)
			amp2[x]=Cmul(amp2[x],Cconj(amp1[x]));
		cfft1d(fftLen,amp2,1);
		for (x=0;x<fftLen;x++)
			corr[x]+=Cabs(amp2[x]);
	}
	fout=FOPEN(f->out_cpx,"w");
	for (x=-overlap;x<overlap;x++)
	{
		corr[x+fftLen]=corr[(x+fftLen)%fftLen];
		if (corr[x+fftLen]<10000000000000000000000.0)
		/*Is a good value-- print it out*/
			fprintf(fout,"%d %f\n",x,corr[x+fftLen]);
		else/*Must be an infinity or NaN*/
		{
			printf("   WARNING!! Found infinity at offset %d!\n",x);
			if (logflag) {
			  sprintf(logbuf,"   WARNING!! Found infinity at offset %d!\n",x);
			  printLog(logbuf);
			}
			corr[x+fftLen]=0.0;
		}
	}
	FCLOSE(fout);
	return dop_findPeak(corr,fftLen-overlap,2*overlap)-fftLen;
}

extern int ac_direction;

int main (int argc, char *argv [])
{
/*Structures: these are passed to the sub-routines which need them.*/
	patch *p1,*p2;
	satellite *s;
	rangeRef *r;
	getRec *signalGetRec;
	file *f;
/*Variables.*/
	int n_az,n_range;/*Region to be processed.*/
	int patchNo;/*Loop counter.*/
	double pixShift,dopDel,old_dop;
	int refLen,lineToBeRead;
	int saved_NLA;
	struct ARDOP_PARAMS params,old_params;
	meta_parameters *meta;
	
	StartWatch();
	printf("   Doppler Determination:\n");
	if (logflag) printLog("   Doppler Determination:\n");
	if (!parse_cla(argc,argv,&params,&meta)) 
	  printErr("   ERROR: Usage: dop_prf as ardop\n");
	
/*Set the initial doppler parameters.*/
	printf("   Processing to doppler at %f prf...\n",params.fd);
	if (logflag) {
	  sprintf(logbuf,"   Processing to doppler at %f prf...\n",params.fd);
	  printLog(logbuf);
	}
	read_params(params.in1,&old_params);/*Read in user's old parameters*/
	
/*Set params so we're processing at least 300 samples and 500 lines.*/
	refLen=params.fs*params.pulsedur;
	params.nla=smallestPow2(300+refLen)-refLen;
	params.na_valid=500;
	ardop_setup(&params,meta,&n_az,&n_range,&s,&r,&f,&signalGetRec);
	
	if (!quietflag) {
	  printf("   Processing a %d az by %d range patch...\n",n_az,n_range);
	  printf("   Of the %d azimuth lines, only %d are valid.\n",n_az,f->n_az_valid);
	}
	
/*
Create "patch"es of data.
*/
	p1=newPatch(n_az,n_range);
	p2=newPatch(n_az,n_range);

/*Process both patches.*/
	lineToBeRead = f->firstLineToProcess;
	
	ac_direction=-1;
	setPatchLoc(p1,s,f->skipFile,f->skipSamp,lineToBeRead);/*Update patch parameters for location.*/
	processPatch(p1,signalGetRec,r,s);/*SAR Process patch.*/

	ac_direction=1;
	setPatchLoc(p2,s,f->skipFile,f->skipSamp,lineToBeRead);/*Update patch parameters for location.*/
	processPatch(p2,signalGetRec,r,s);/*SAR Process patch.*/

/*Do range amplitude correlation.*/
	pixShift=amp_corr(p1,p2,f);
	StopWatch();
	
	dopDel=2*pixShift*f->rngpix/(s->wavl*s->refPerRange/2*(p1->slantToFirst+p1->slantPer*p1->n_range/2));
	printf("   The image's doppler is off by %.2f pixels, or %.2f PRF.\n",
		pixShift,dopDel);
	if (logflag) {
	  sprintf(logbuf,"   The image's doppler is off by %.2f pixels, or %.2f PRF.\n",
		pixShift,dopDel);
	  printLog(logbuf);
	}

	/*dopDel-=0.2; <-- Constant frequency offset?*/
	dopDel=floor(dopDel+0.5);

	printf("   The doppler coefficients should be:\n"
	"   %.10f %.10f %.10f\n\n",s->orig_fd-dopDel,s->orig_fdd,s->orig_fddd);
	printf("   These coefficients have been written into '%s'\n",appendExt(params.in1,".in"));
	if (logflag) {
	  sprintf(logbuf,"   The doppler coefficients should be:\n"
	          "   %.10f %.10f %.10f\n\n",s->orig_fd-dopDel,s->orig_fdd,s->orig_fddd);
	  printLog(logbuf);
	  sprintf(logbuf,"   These coefficients have been written into '%s'\n",appendExt(params.in1,".in"));
	  printLog(logbuf);
	}

	old_params.fd=s->orig_fd-dopDel;
	old_params.fdd=s->orig_fdd;
	old_params.fddd=s->orig_fddd;

	print_params(params.in1,&old_params,"dop_prf");

	destroyPatch(p1);
	destroyPatch(p2);
	
	return(0);
}
