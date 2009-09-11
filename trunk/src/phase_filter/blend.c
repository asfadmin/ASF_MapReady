/*******************************************************
blend Routines:
	These blend the complex numbers stored in
the given arrays either vertically or horizontally by
the given weighting array.
	They're here in a separate file, because they're
so ugly.

Orion Sky Lawlor, ASF-STEP, 7/98
*/

/***********************************************
blendHoriz, blendVert:
	These blend the complex numbers stored in
the given arrays either vertically or horizontally by
the given weighting array.

left,right,top, and bottom are [dx,dy] complex input arrays.
weight is [ox,oy] float input weighting array.
outBuf is [ns,oy] float output phase array.
*/
#include "asf.h"


#include "filter.h"

void blendHoriz(complex *left,complex *right,float *weight,float *outBuf)
{
	register int x,y;
	register float blend_r,blend_i;
	register float *out,*wL,*wR;
	register complex *cL,*cR;
	for (y=0;y<oy;y++)
	{
		cL=&left [y*dx];
		cR=&right[y*dx];
		
		wL=&weight[(oy-1)*ox+ox-1];
		wR=&weight[(oy-1)*ox+0   ];
		
		out=&outBuf[y*ns];
		for (x=0;x<ox;x++)
		{
			blend_r=(*wL  ) * cL->r + (*wR  ) * cR->r;
			blend_i=(*wL--) * cL->i + (*wR++) * cR->i;
			*out++=atan2(blend_i,blend_r);
			cL++,cR++;
		}
	}
}

	
void blendVert(complex *top,complex *bottom,float *weight,float *outBuf)
{
	register int x,y;
	register float blend_r,blend_i;
	register float *out,*wT,*wB;
	register complex *cT,*cB;
	for (y=0;y<oy;y++)
	{
		cT=&top   [(y+oy)*dx];
		cB=&bottom[     y*dx];
		
		wT=&weight[(oy-1-y)*ox +ox-1];
		wB=&weight[       y*ox +ox-1];
		
		out=&outBuf[y*ns];
		for (x=0;x<ox;x++)
		{
			blend_r=(*wT) * cT->r + (*wB) * cB->r;
			blend_i=(*wT) * cT->i + (*wB) * cB->i;
			*out++=atan2(blend_i,blend_r);
			cT++,cB++;
		}
	}
}

			
/*******************************************************
blendData:
	A routine internal to las_filter.  Given two rows
of chunks of data, blendData weights the two, converts
to polar form, and fills the given output buffer.

If last_chunks is NULL, this is the first line in the file.
If chunks is NULL, this is the last line in the file.

Chunks,last_chunks: a [ns/ox] array of [dx,dy] complex chunks.
weight: a [ox,oy] array of bilinear blending weights.  Strongest weight is at 0,0.
outBuf: a [ns,oy] array of output floats.
*/
void blendData(complex **chunks,complex **last_chunks,
	float *weight,float *outBuf)
{
	int nChunkX=ns/ox-1;
	int chunkX,x,y;
	register float *wTL,*wTR,
	               *wBL,*wBR;
	register complex *cTL,*cTR,
	                 *cBL,*cBR;
	register float *out,blend_r,blend_i;
/*Figure out the possibility we're dealing with:*/
	if (chunks!=NULL && last_chunks!=NULL)
	{
	/*Left border:top-to-bottom 2-way blend*/
		chunkX=0;
		blendVert(&last_chunks[chunkX][0],&chunks[chunkX][0],
			weight,&outBuf[chunkX*ox]);
		
	/*Interior lines: 4-way blend.*/
		/*Blending:
			last_chunks[chunkX-1] last_chunks[chunkX]
			     chunks[chunkX-1] chunks[chunkX]
		*/
		for (chunkX=1;chunkX<nChunkX;chunkX++)
		for (y=0;y<oy;y++)
		{
			cTL=&last_chunks[chunkX-1][(y+oy)*dx    +ox];
			cTR=&last_chunks[chunkX]  [(y+oy)*dx    +0];
			cBL=&chunks[chunkX-1][y*dx+ox];
			cBR=&chunks[chunkX]  [y*dx+0];
			
			wTL=&weight[(oy-1-y)*ox +ox-1];
			wTR=&weight[(oy-1-y)*ox +0];
			wBL=&weight[y*ox+ox-1];
			wBR=&weight[y*ox+0];
			
			out=&outBuf[y*ns+chunkX*ox];
			for (x=0;x<ox;x++)
			{
				blend_r=(*wTL  ) * cTL->r + (*wTR  ) * cTR->r +
					(*wBL  ) * cBL->r + (*wBR  ) * cBR->r;
				blend_i=(*wTL--) * cTL->i + (*wTR++) * cTR->i +
					(*wBL--) * cBL->i + (*wBR++) * cBR->i;
				*out++=atan2(blend_i,blend_r);
				cTL++,cTR++,cBL++,cBR++;
			}
		}
		
	/*Right border: top-to-bottom 2-way blend*/
		blendVert(&last_chunks[chunkX-1][ox],&chunks[chunkX-1][ox],
			weight,&outBuf[chunkX*ox]);
	} else 
	{
	
		if (chunks!=NULL)
		/*Top Border: left-to-right 2-way blend*/
		for (chunkX=1;chunkX<nChunkX;chunkX++)
			blendHoriz(&chunks[chunkX-1][ox],&chunks[chunkX][0],
				weight,&outBuf[chunkX*ox]);
		
		else if (last_chunks!=NULL)
		/*Bottom Border: left-to-right 2-way blend*/
		for (chunkX=1;chunkX<nChunkX;chunkX++)
			blendHoriz(&last_chunks[chunkX-1][oy*dx+ox],&last_chunks[chunkX][oy*dx+0],
				weight,&outBuf[chunkX*ox]);
		
		/*Now just copy over image corners.*/
		for (y=0;y<oy;y++)
		{
			if (chunks!=NULL)/*Top line*/
				cBL=&chunks[chunkX-1][y*dx+ox],
				cBR=&chunks[0]  [y*dx+0];
			else /*Bottom line.*/
				cBL=&last_chunks[chunkX-1][(y+oy)*dx    +ox],
				cBR=&last_chunks[0]  [(y+oy)*dx    +0];
			out=&outBuf[y*ns+0*ox];
			for (x=0;x<ox;x++)
			{
				*out++=atan2(cBR->i,cBR->r);
				cBR++;
			}
			out=&outBuf[y*ns+nChunkX*ox];
			for (x=0;x<ox;x++)
			{
				*out++=atan2(cBL->i,cBL->r);
				cBL++;
			}
		}
		
	}
}
