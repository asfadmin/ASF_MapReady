/*
Specan Processor implementation file.

This file contains specan_file, which
runs on the master processor to SAR process
an entire file full of signal data to
create an entire output amplitude file.

Orion Lawlor, ASF 11/98.
*/
#include "asf.h"
#include "aisp_defs.h"
#include "specan.h"

#include "my_mpi.h"
#if MPI
#define status(mess) printf mess

/*Slave receive (complex) and send(amp)*/
FCMPLX *slave_recv(specan_patch *s)
{
	FCMPLX *in;
	status(("Slave %d receiving from master\n",get_my_rank()));
	MPI_Recv(s,sizeof(specan_patch),MPI_BYTE,0,0,comm,&mpi_stat);
	in=(FCMPLX *)malloc(sizeof(FCMPLX)*s->iWid*s->iHt);
	MPI_Recv(in,2*s->iWid*s->iHt,MPI_FLOAT,0,0,comm,&mpi_stat);
	return in;
}
void slave_send(specan_patch *s,float *amp_out)
{
	status(("Slave %d sending to master\n",get_my_rank()));
	MPI_Send(amp_out,s->oWid*s->oHt,MPI_FLOAT,0,0,comm);
}

/*slave_process is called by my_mpi when the master calls us*/
void slave_process(int command)
{
        specan_patch patch_storage;
        specan_patch *s=&patch_storage;
        FCMPLX *in=slave_recv(s);
        float *amp_out=(float *)malloc(sizeof(float)*s->oWid*s->oHt);
        status(("Slave %d processing patch\n",get_my_rank()));
        specan_process_patch(s,in,amp_out);
        slave_send(s,amp_out);
        free(amp_out);
        free(in);
}

/*Master send (complex) and receive (amp)*/
void master_send(specan_patch *s,FCMPLX *in,int dest)
{
	int slaveRank=dest+first_slave;
	send_slave_command(dest,SLAVE_PROCESS_BEGIN);
	status(("Master sending to slave %d\n",slaveRank));
	MPI_Send(s,sizeof(specan_patch),MPI_BYTE,slaveRank,0,comm);
	MPI_Send(in,2*s->iWid*s->iHt,MPI_FLOAT,slaveRank,0,comm);
}

void master_recv(specan_patch *s,float *amp_out,int src)
{
	int slaveRank=src+first_slave;
	status(("Master waiting on slave %d\n",slaveRank));
	MPI_Recv(amp_out,s->oWid*s->oHt,MPI_FLOAT,slaveRank,0,comm,&mpi_stat);
}


#endif
/*Specan_file SAR processes an entire file from
input to output.  It only runs on the master (0).
*/
void specan_file(getRec *inFile,int NnLooks,FILE *outFile, 
        specan_struct *in_rng,specan_struct *in_az,
        int *out_lines,int *out_samples)
{
	FCMPLX *in;
	float *amp_out;
	specan_patch patch_storage;
	specan_patch *s=&patch_storage;
	
	int yPatch,nyPatch;
	
	s->az=*in_az;
	s->rng=*in_rng;
	s->nxPatch=inFile->nSamples/s->rng.iNum;
	nyPatch=inFile->nLines/s->az.iNum;
	s->iWid=inFile->nSamples;
	s->iHt=s->az.fftLen;
	s->oWid=s->nxPatch*s->rng.oNum;
	s->oHt=s->az.oNum;
	s->nLooks=NnLooks;
	s->outSeq=(int)(s->rng.oNum/s->nLooks);
	s->inSeq=s->outSeq*s->rng.oSamp/s->rng.iSamp;
	
	*out_samples=s->oWid;
	*out_lines=nyPatch*s->oHt;
	printf("Input is %lli lines by %lli samples\n"
		"Output will be %d lines by %d samples\n",
		inFile->nLines,inFile->nSamples,
		*out_lines,*out_samples);

	init_patch(s);
	
	/*Allocate buffers*/
	in=(FCMPLX *)malloc(sizeof(FCMPLX)*s->iWid*s->iHt);
	amp_out=(float *)malloc(sizeof(float)*s->oWid*s->oHt);

/*Loop through the patches of input data.*/
	for (yPatch=0;yPatch<nyPatch;yPatch++)
	{/*Process a patch of data.*/
#  if !MPI
		read_patch(s,inFile,in,yPatch);
		specan_process_patch(s,in,amp_out);
		write_patch(s,amp_out,outFile);
	}
#  else /*using MPI*/
		/*Distribute patches round-robin for processing*/
		read_patch(s,inFile,in,yPatch);
		master_send(s,in,yPatch%num_slaves);
		if (yPatch>=num_slaves-1)
		{
		/*Pick the patches back up as they finish*/ 
			master_recv(s,amp_out,(yPatch+1)%num_slaves);
			write_patch(s,amp_out,outFile);
		}
	}

/*cleanup loop-- collect all outstanding patches from slaves.*/
	for (yPatch=nyPatch;yPatch<nyPatch+num_slaves-1;yPatch++)
	{
		master_recv(s,amp_out,(yPatch+1)%num_slaves);
		write_patch(s,amp_out,outFile);
	}
#  endif /*MPI*/

	free(in);
	free(amp_out);
}

