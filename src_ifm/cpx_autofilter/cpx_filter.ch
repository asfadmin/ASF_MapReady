/*
    NAME: cpx_filter

    SYNOPSIS: cpx_filter <complex image input> <complex image output> <frequency parameter file>


    DESCRIPTION:
	This program filters the complex image data in an attempt to improve coherence in interferograms
	Complex data that is processed at a doppler different that the optimal doppler will exhibit a decreased
	coherence in interferometry.  This coherence can be improved by filtering the data in azimuth and 
	retaining only the part of the two dataset's spectra that overlap.  The two datasets are then time-domain
	demodulated to baseband by the mean of their two doppler centroids.  Another program 'cpx_spectrum' generates
	an azimuthal spectrum of the data.  
	
	The frequency parameter file indicates which frequencies should be filtered.  This can be generated using the program
	gen_filt_params.  The format of the file is as follows.
	
	frequency shift (modulation)
	loFreq1 hiFreq1
	loFreq2 hiFreq2

	All specifications are in Hz and each line is ended with a carriage return.

	
    AUTHOR:
	M. Ayers 5-2000
*/

#include <math.h>
#include "asf.h"
#include "aisp_defs.h"
#include "ddr.h"

#define block_size 2000
#define toss_size block_size/100
#define VERSION 1.1

main(int argc,char **argv)
{
	
/* Define all the variables that we'll be using */
	
	int lines,samps,fftLen,start_line;		/* Number of image lines, samples and the FFT length */
	int x,y,i,k,f_lo1,f_lo2,f_hi1,f_hi2;		/* Counters and the filter frequency indicies */
	int chunk_size,chunk_int;			/* Size of the current datablock, temporary value */
	int last_chunk;					/* Size of the last chunk */
	struct DDR inDDR,outDDR;			/* DDR structures to figure out info about the images */
	char *infile, *freqFile, *outfile;		/* Input filename */
	float filtStart[2], filtEnd[2],df,stop;		/* Filter stop, start, delta and counter variables */
	FCMPLX *inBuf,*outBuf;				/* Input/Output Image Buffers */
	FCMPLX *fftBuf, *tossBuf;			/* FFT Buffer for the image */
	float *ampBuf,*phsBuf;				/*Amplitude and Phase Buffers */
	float *time_vector,A,B,shift;			/* Time vector and frequency modulation shift variables */
	float chunk_float;				/* Temporary value */
	FILE *inF, *freqF, *outF1;			/* Input and Output file pointers */
	float cur_time, f_s;				/* Current time to increment the time vector by */

/* Usage is shown if the user doesn't give 3 arguements */
	if(argc!=4)
	{
		printf("\n%s <.cpx in> <.cpx out> <frequency parameter file>\n\n",argv[0]);
		printf("     This program filters the complex image data in an attempt to\n");
		printf("improve coherence in interferograms.  Complex data that is processed\n");
		printf("at a doppler different that the optimal doppler will exhibit a\n");
		printf("decreased coherence in interferometry.  This coherence can be improved\n");
		printf("by filtering the data in azimuth and retaining only the part of the two\n");
		printf("dataset's spectra that overlap.  The two datasets are then time-domain\n");
		printf(" demodulated to baseband by the mean of their two doppler centroids.\n");
		printf("     Another program 'cpx_spectrum' generates an azimuthal spectrum of\n");
		printf("the data.  ");
		printf("The frequency parameter file indicates which frequencies should be\n");
		printf("filtered.  This can be generated using the program gen_filt_params.\n");
		printf("     The format of the file is as follows.sampling frequency\n\n");
		printf("frequency shift (modulation)\n");
		printf("loFreq1 hiFreq1\nloFreq2 hiFreq2\n\n");
		printf("All specifications are in Hz and each line is ended with a carriage return.\n");
		printf("Version %.2f ASF IFSAR tools 2000\n", VERSION);
		exit(0);
	}
	
	StartWatch();

/* Get the filename and filter start and end frequencies from the command line */ 
	infile=argv[1];
	c_getddr(infile,&inDDR);
	if(findExt(infile)==NULL)
		infile=appendExt(argv[1],".cpx");
	outfile=argv[2];
	freqFile=argv[3];

/* Open the frequency parameter file and read the parameters */

	if((freqF=FOPEN(freqFile,"r"))==NULL)
        {
                printf("Frequency Parameter File %s could not be Opened!\n",freqFile);
                exit(0);
        }
	fscanf(freqF,"%f\n",&f_s);
	fscanf(freqF,"%f\n",&shift);
	fscanf(freqF,"%f %f\n", &filtStart[0], &filtEnd[0]);
	fscanf(freqF,"%f %f\n", &filtStart[1], &filtEnd[1]);

	
	printf("\n\n\n-+-+-+-+-+-+-+-+-+-+-+-+- PROCESSING -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-\n\n");
	printf("Input file is %s, Output file is %s.cpx, Parameter file is %s\n",infile,outfile,freqFile);
	printf("Filtering from frequencies %.2f to %.2f Hz and %.2f to %.2f in Azimuth\n",filtStart[0],filtEnd[0],filtStart[1],filtEnd[1]);
	printf("The sampling frequency is %f Hz\n",f_s);
	printf("Shifting the spectrum by %.2f Hz\n",shift);

/* Get the number of lines and samples from the input DDR */
	lines=inDDR.nl;
	samps=inDDR.ns;
	
	chunk_size=block_size;
	chunk_float=(float)lines/chunk_size;
	chunk_int=lines/chunk_size;
	last_chunk=(int)(((chunk_float-(float)chunk_int)*(float)block_size+0.5)+(2*toss_size));
	printf("Chunk Size is set to %d, the last chunk is %d lines\n",chunk_size,last_chunk);
		
/* Compute the FFT length based on the number of lines. Must be a power of 2 */
	i=(log10(chunk_size)/log10(2))+1;
	fftLen=pow(2,i);

	printf("FFT Length is %d\n",fftLen);
	cfft1d(fftLen,NULL,0);

	printf("The Input Image has %d lines and %d samples\n",lines,samps);
	printf("\n-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-\n\n"); 
	printf("Allocating Memory\n");

/* Allocate the memory for all the buffers */
          
        inBuf   = (FCMPLX *)MALLOC(sizeof(FCMPLX)*samps*fftLen);
        outBuf  = (FCMPLX *)MALLOC(sizeof(FCMPLX)*samps*fftLen);
	fftBuf  = (FCMPLX *)MALLOC(sizeof(FCMPLX)*fftLen);
	tossBuf = (FCMPLX *)MALLOC(sizeof(FCMPLX)*samps*toss_size*2);
	ampBuf  = (float *)MALLOC(sizeof(float)*fftLen);
        phsBuf  = (float *)MALLOC(sizeof(float)*fftLen);
	time_vector = (float *)MALLOC(sizeof(float)*lines);
	
/* Open the Complex Image File */
	
	if((inF=FOPEN(infile,"rb"))==NULL)
	{	
		printf("Complex Image file %s could not be opened\n",infile);
		exit(0);
	}
	
        strcat(outfile,".cpx"); 

	if((outF1=FOPEN(outfile,"wb"))==NULL)
        {
                printf("Unable to write output %s\n",outfile);
                exit(0);
        }
	
	outDDR=inDDR;
        outDDR.nl=lines;
        outDDR.ns=samps; 
        outDDR.dtype=4;
        outDDR.nbands=1;
        c_putddr(outfile,&outDDR);
        printf("DDR file created\n");
                        


/* Find the filter frequency index values */
	 
	df=f_s/fftLen;
                
	stop=0;
	i=0;
	while(stop<filtStart[0])
	{
		f_lo1=i;
		stop=df*i;
		i++;
	}
        
	stop=0;
        i=0;
        while(stop<filtStart[1])
        {
                f_lo2=i;
                stop=df*i;
                i++;
        }

	i=fftLen;
	stop=df*i;  
	while(stop>filtEnd[0])
	{
		f_hi1=i;
		stop=df*i;   
		i--;
	}

	i=fftLen;
        stop=df*i;
        while(stop>filtEnd[1])
        {
                f_hi2=i;
                stop=df*i;
                i--;
        }
	
/* Zero out all the arrays and begin processing data */

	cur_time=0;

	for(i=0;i<fftLen;i++)
	{
		ampBuf[i]=0;
		phsBuf[i]=0;
	}
	
	for(k=0;k<chunk_int+1;k++)
	{
		printf("\nProcessing Chunk %d of %d\n",k,lines/chunk_size);
		if(k==0)
			start_line=0;
		else
			start_line=(k*chunk_size)-(2*toss_size);
		
		if(k==chunk_int)
			chunk_size=last_chunk;
		
		cur_time=start_line*(1/f_s);
/*		printf("Current time is %f seconds\n",cur_time);	*/
	
	/* Read in the data chunk */
		printf("Reading %d Lines Starting at Line %d\n",chunk_size,start_line);
		FSEEK64(inF,sizeof(FCMPLX)*samps*start_line,SEEK_SET);
		FREAD(inBuf,sizeof(FCMPLX)*samps*chunk_size,1,inF);

	/* Process the each column */
	
		printf("Performing the FFT and Filtering Operations\n");
		for(x=0;x<samps;x++) 
		{
			if(x%1000 == 0)  printf("Processing Column %d\n",x);

			for(y=0;y<fftLen;y++)
			{
				fftBuf[y].r=0;
				fftBuf[y].i=0;
			}
			
			for(y=0;y<chunk_size;y++)
			{
				fftBuf[y].r=inBuf[y*samps+x].r;
				fftBuf[y].i=inBuf[y*samps+x].i;
			}

			cfft1d(fftLen,fftBuf,-1);

			for (i=0;i<fftLen;i++) 
			{
				ampBuf[i]=sqrt(fftBuf[i].r*fftBuf[i].r+fftBuf[i].i*fftBuf[i].i);	

				if(fftBuf[i].i!=0.0 || fftBuf[i].r!=0.0)
					phsBuf[i]=atan2(fftBuf[i].i,fftBuf[i].r);
				else
					phsBuf[i]=0;

				if(((i>f_lo1)&&(i<f_hi1)) || ((i>f_lo2) && (i<f_hi2)))
				{
					ampBuf[i]=0;
					phsBuf[i]=0;
				}

				fftBuf[i].r=ampBuf[i]*cos(phsBuf[i]);
				fftBuf[i].i=ampBuf[i]*sin(phsBuf[i]);	
			}
		
			cfft1d(fftLen,fftBuf,1);
			
			for(i=0;i<chunk_size;i++)
			{	
				outBuf[i*samps+x].r=fftBuf[i].r;
				outBuf[i*samps+x].i=fftBuf[i].i;
			}	
		
		}
	
	printf("Finished the FFT and Filtering Operations\n");


        /* Perform the time-domain frequency shift */ 
		if(shift != 0.0)
		{
		for(i=0;i<chunk_size;i++)
			time_vector[i]=cur_time+(1/f_s)*i;


                        printf("\nPerforming time-domain frequency shift of %.2f Hz\n",shift);
                        for(y=0;y<chunk_size;y++)
                        {
                                for(x=0;x<samps;x++)
                                {
                                        A=outBuf[y*samps+x].r;
                                        B=outBuf[y*samps+x].i;
					outBuf[y*samps+x].r=A*cos(2*pi*shift*time_vector[y])-B*sin(2*pi*shift*time_vector[y]);
					outBuf[y*samps+x].i=B*cos(2*pi*shift*time_vector[y])+A*sin(2*pi*shift*time_vector[y]);
                                }
                        }
                }


	/* Write out the data file */



		if(k==0)
		{
			for(x=0;x<2*toss_size*samps;x++);
			{
				tossBuf[x].r=0.0;
				tossBuf[x].i=0.0;
			}
			printf("Writing out the initial data\n");
			FWRITE(tossBuf,sizeof(FCMPLX)*samps*2*toss_size,1,outF1);
		}
		printf("Writing the output lines %d to %d in file %s\n",start_line,start_line+chunk_size-toss_size,outfile);
		FWRITE(outBuf+(samps*toss_size),sizeof(FCMPLX)*samps*(chunk_size-toss_size),1,outF1);

		if(k==chunk_int)
		{
			printf("Writing out the last data\n");
			FWRITE(tossBuf,sizeof(FCMPLX)*samps*2*toss_size,1,outF1);
		}
	}

	FCLOSE(outF1);
	printf("\n");
	StopWatch();
	return 0;
}

