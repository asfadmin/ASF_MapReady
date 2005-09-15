/*
    NAME: pcpx_filter

    SYNOPSIS: pcpx_filter <complex image input> <complex image output> <frequency parameter file>


    DESCRIPTION:
	This is a parallel implementation of the cpx_filter algorithm.

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
#include <mpi.h>
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
	int num_prcsrs, prcsr_rank, mpi_offset;		/* MPI Processor count and rank variables, MPI counter offset*/
	int x,y,i,k,f_lo1,f_lo2,f_hi1,f_hi2;		/* Counters and the filter frequency indicies */
	int chunk_size,chunk_int;			/* Size of the current datablock, temporary value */
	int last_chunk;					/* Size of the last chunk */
	long long in_offset, out_offset;		/* MPI Input and Output byte offsets */
	struct DDR inDDR,outDDR;			/* DDR structures to figure out info about the images */
	char *infile, *freqFile, *outfile;		/* Input filename */
	float filtStart[2], filtEnd[2],df,stop;		/* Filter stop, start, delta and counter variables */
	complexFloat *inBuf,*outBuf;				/* Input/Output Image Buffers */
	complexFloat *fftBuf;					/* FFT Buffer for the image */
	float *ampBuf,*phsBuf;				/*Amplitude and Phase Buffers */
	float *time_vector,A,B,shift;			/* Time vector and frequency modulation shift variables */
	float chunk_float;				/* Temporary value */
	FILE *inF, *freqF, *outF1;			/* Input and Output file pointers */
	float cur_time, f_s;				/* Current time to increment the time vector by */


	MPI_Init(&argc,&argv);
        MPI_Comm_size(MPI_COMM_WORLD,&num_prcsrs);
        MPI_Comm_rank(MPI_COMM_WORLD,&prcsr_rank);
        
        if(prcsr_rank==0) printf("\nMPI Functions initialized, using %d processors\n",num_prcsrs);

/* Usage is shown if the user doesn't give 3 arguements */
	
	if(argc!=4)
		MPI_Finalize();

	if((argc!=4) && (prcsr_rank==0))
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
		printf("     The format of the file is as follows.\n\nfrequency shift (modulation)\n");
		printf("loFreq1 hiFreq1\nloFreq2 hiFreq2\n\n");
		printf("All specifications are in Hz and each line is ended with a carriage return.\n");
		printf("Version %.2f ASF IFSAR tools 2000\n", VERSION);
		exit(1);
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
                exit(1);
        }

	fscanf(freqF,"%f\n",&f_s);
	fscanf(freqF,"%f\n",&shift);
	fscanf(freqF,"%f %f\n", &filtStart[0], &filtEnd[0]);
	fscanf(freqF,"%f %f\n", &filtStart[1], &filtEnd[1]);

	if(prcsr_rank==0)
	{
	printf("\n\n-+-+-+-+-+-+-+-+-+-+-+-+- PROCESSING -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-\n\n");
	printf("The input file is %s, the output file is %s.cpx, the parameter file is %s\n",infile,outfile,freqFile);
	printf("The sampling frequency is %f Hz\n",f_s);
	printf("Filtering from frequencies %.2f to %.2f Hz and %.2f to %.2f in Azimuth\n",filtStart[0],filtEnd[0],filtStart[1],filtEnd[1]);
	printf("Shifting the spectrum by %.2f Hz\n",shift);
	}

/* Get the number of lines and samples from the input DDR */
	lines=inDDR.nl;
	samps=inDDR.ns;
	
	chunk_size=block_size;
	chunk_float=(float)lines/chunk_size;
	chunk_int=lines/chunk_size;
	last_chunk=(int)((chunk_float-(float)chunk_int)*chunk_size+0.5);

	if(prcsr_rank==0) printf("Chunk Size is set to %d, the last chunk is %d lines\n",chunk_size,last_chunk);
		
/* Compute the FFT length based on the number of lines. Must be a power of 2 */
	i=(log10(chunk_size)/log10(2))+1;
	fftLen=pow(2,i);

	if(prcsr_rank==0) 
	{
	printf("FFT Length is %d\n",fftLen);
	printf("The Input Image has %d lines and %d samples\n",lines,samps);
	printf("\n-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-\n\n"); 
	printf("Allocating Memory\n");
        }

	cfft1d(fftLen,NULL,0);

/* Allocate the memory for all the buffers */

        inBuf=(complexFloat *)MALLOC(sizeof(complexFloat)*samps*fftLen);
        outBuf=(complexFloat *)MALLOC(sizeof(complexFloat)*samps*fftLen);
        fftBuf=(complexFloat *)MALLOC(sizeof(complexFloat)*fftLen);
        ampBuf=(float *)MALLOC(sizeof(float)*fftLen);
        phsBuf=(float *)MALLOC(sizeof(float)*fftLen);  
        time_vector=(float *)MALLOC(sizeof(float)*lines);
	
/* Open the Complex Image Files */
	
	if((inF=FOPEN(infile,"rb"))==NULL)
	{	
		printf("Complex Image file %s could not be opened\n",infile);
		exit(0);
	}
	
	strcat(outfile,".cpx"); 
	
	if((outF1=FOPEN(outfile,"w+b"))==NULL)
        {
                printf("Unable to open output file %s\n",outfile);
                exit(0);
        }

/* Create the output complex image .ddr file */

	outDDR=inDDR;
        outDDR.nl=chunk_int*(block_size-2*toss_size)+(last_chunk-2*toss_size);
        outDDR.ns=samps; 
        outDDR.dtype=4;
        outDDR.nbands=1;
        c_putddr(outfile,&outDDR);
	if(prcsr_rank==0) printf("DDR file created\n");


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
	
	if(prcsr_rank==0) printf("Waiting for all the processors to catch up\n");
	MPI_Barrier(MPI_COMM_WORLD);

/* Zero out all the arrays and begin processing data */

	cur_time=0;

	for(i=0;i<fftLen;i++)
	{
		ampBuf[i]=0;
		phsBuf[i]=0;
	}
	
/* Start processing */
	for(k=0;k<(chunk_int/num_prcsrs)+1;k++)
	{
		mpi_offset=k*num_prcsrs+prcsr_rank;

		if(prcsr_rank==0)
			printf("\nProcessing Chunks %d through %d of %d\n",mpi_offset,mpi_offset+(num_prcsrs-1),chunk_int);
		

		/* Starting Here we only use enough processors to fill all the chunks */	        

                if(mpi_offset<chunk_int+1)
                {
			if(k==0 && prcsr_rank==0)
				start_line=0;
			else
				start_line=mpi_offset*chunk_size-2*toss_size;
			
                        in_offset=samps*sizeof(complexFloat)*start_line;

			if(mpi_offset==chunk_int)
			{
				printf("Processing the last chunk\n");
				chunk_size=last_chunk;
			}
		
			cur_time=start_line*(1/f_s);
			
		/* Read in the data chunk */
	
			FSEEK64(inF,in_offset,SEEK_SET);
			FREAD(inBuf,sizeof(complexFloat)*samps*chunk_size,1,inF);
		/* Process the each column */
	
			if(prcsr_rank==0)
				printf("Performing the FFT and Filtering Operations\n");
			for(x=0;x<samps;x++) 
			{

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
	


	        /* Perform the time-domain frequency shift */ 
			if(shift != 0.0)
			{
			for(i=0;i<chunk_size;i++)
				time_vector[i]=cur_time+(1/f_s)*i;


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


		/* Write out the data files */
	
			if(prcsr_rank==0)
				printf("Writing out data for chunks %d through %d\n",mpi_offset,mpi_offset+(num_prcsrs-1));

			if(k==0 && prcsr_rank==0)
				out_offset=0;
			else
				out_offset=(mpi_offset*(block_size-2*toss_size))*sizeof(complexFloat)*samps;
		
			FSEEK64(outF1,out_offset,SEEK_SET);
			FWRITE(outBuf+(samps*toss_size),sizeof(complexFloat)*samps*(chunk_size-toss_size),1,outF1);
			
		} /* End of the processing using the active processors */
	
		else printf("Not using processor %d\n",prcsr_rank);

		MPI_Barrier(MPI_COMM_WORLD);
	}

	if(prcsr_rank==0)
	{
		printf("Filtering of Data Complete\n");
		StopWatch();
	}
	FCLOSE(outF1);
	MPI_Finalize();
	return 0;
}

