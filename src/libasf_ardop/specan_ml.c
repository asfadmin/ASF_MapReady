/*
Specan Processor implementation file.

These routines do range multilooking for the SPECAN
algorithm.

Orion Lawlor, ASF 1/99.
*/
#include "asf.h"
#include "ardop_defs.h"
#include "specan.h"
#include "specan_ml.h"


/*Create a specan_ml struct of the given size*/
specan_ml *specan_ml_init(int wid,int ht)
{
	int x;
	specan_ml *ml=(specan_ml *)MALLOC(sizeof(specan_ml));
	ml->wid=wid;
	ml->ht=ht;
	ml->s=(lookset *)MALLOC(sizeof(lookset)*wid);
	/*At the start, there are no looks in any column*/
	for (x=0;x<ml->wid;x++)
	{
		int y;
		float *looks=(float *)malloc(sizeof(float)*ht);
		for (y=0;y<ht;y++)
			looks[y]=0.0;/*Zero out image array*/
		ml->s[x].looks=looks;
		ml->s[x].nlooks=0;
	}
	return ml;
}


/*Add another radiometric look to the buffer.
DestX may be arbitrary (not necessarily in bounds).
Input buffer is one column of data, at least ht long.
*/
void specan_ml_look(specan_ml *ml,int destX,complexFloat *in)
{
	float *looks;
	int y;
	
	/*Check bounds-- discard out-of-bounds lines*/
	if ((destX<0)||(destX>=ml->wid))
		return;/*Out-of-bounds*/
	
	/*Else, column is in bounds.  Make a new float buffer,
	  and add it to the correct lookset.*/
	ml->s[destX].nlooks++;
	looks=ml->s[destX].looks;
	
	/*Add power of complex image to this look*/
	for (y=0;y<ml->ht;y++)
		looks[y]+=in[y].real*in[y].real+in[y].imag*in[y].imag;
}


/*Write the wid x ht floating-point output block to 
(row-by-row) dest,
multiplying each line by one coordinate of scallop,
which must be at least ht long.*/
void specan_ml_out(specan_ml *ml,float *scallop,float *dest)
{
	int x;
	register int /*look,*/y;
	int wid=ml->wid,ht=ml->ht;
	
	/*For each output column*/
	for (x=0;x<wid;x++)
	{
		int nlooks=ml->s[x].nlooks;
		float *looks=ml->s[x].looks;
		float scale=1.0/nlooks;
		if (nlooks==0)
		/*No data- write zeros*/
			scale=0.0;
		/*Loop for each output row*/
		for (y=0;y<ht;y++)
			dest[wid*y+x]=scallop[y]*sqrt(scale*looks[y]);
	}
}


/*Free a specan_ml struct*/
void specan_ml_free(specan_ml *ml)
{
	int i;
	for (i=0;i<ml->wid;i++)
		free(ml->s[i].looks);
	free(ml->s);
	ml->wid=ml->ht=-1;
	free(ml);
}

