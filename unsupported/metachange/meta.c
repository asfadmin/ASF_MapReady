/******************************************************
Program Name: Meta


Purpose: Modify ASF metadata files, in a general-purpose way.


Change History:
Version:	Programmer:	Date:		Purpose:
1.0		O.Lawlor	2/97		For RAMS project,
						needed to convert
						metadata to Slant range.

Details/External References:
        I created routines to read in and write out most of the 
metadata structures defined in CEOS.h.  I've linked these 
routines into a program called "meta".  Designed to be general-
purpose, meta takes a parameter which tells it what to do with 
the metadata.
        If you pass the "-s" parameter (for Slant range), the program 
will convert the metadata into a form appropriate for slant range
images.  More fields can easily be modified, if necessary, at a 
later date.

The important files are:

        meta.c-- main procedure, and process_### procedures (which change
                the metadata fields.

        parse_ldr.c-- procedure which opens up and steps through the
                chunks of an ASF .L file.

        code_ldr.c-- a set of procedures for encoding and decoding
                the metadata structures.  There is only one routine,
                Code_###, for each structure type.  Whether the
                conversion is from ASCII or to ASCII depends
                on the "dir" enumerated parameter.       

        gr2sr_vec.c-- (in asf_tools/src/gr2sr/) procedure to create
                an array which maps slant range samples to ground 
                range samples.

        gr2ml_vec.c-- (in asf_tools/src/gr2sr/) procedure to create
                an array mapping slant range lines to ground range lines.



Syntax: meta [-s|-n] <infilename> <outfilename>
	-s converts the image metadata to Slant range.
	-n does Nothing to the image metadata (that is, copy).	
******************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ceos.h"
#include "ddr.h"
float version=0.82;

typedef struct {
	char *infilename;
	FILE *outFile;
	int era;
	char option;/*command line option: -n or -s*/
	/*fields for slant range conversion (-s)*/
		long outNL,outNS,inNL,inNS;
		float scaleSample,scaleLine;
		float *sr2gr,*ml2gr;
		struct dataset_sum_rec *dssr;
		struct DDR *ddr;
} metaWriteRecord;

void init_slant(metaWriteRecord *w)
{
	float	pi = 3.141592654,nlooks=5.0;
	int      i;
	char infile[240];
	w->dssr=(struct dataset_sum_rec *)malloc(sizeof(struct dataset_sum_rec));
	w->ddr=(struct DDR *)malloc(sizeof(struct DDR));
	strcpy(infile,w->infilename);
	strcat(infile,".img");
	get_dssr(w->infilename,w->dssr);
	c_getddr(infile,w->ddr);
	w->inNL=w->ddr->nl;w->inNS=w->ddr->ns;
	w->sr2gr=(float *)malloc(sizeof(float)*8192);
	w->ml2gr=(float *)malloc(sizeof(float)*8192);
	gr2sr_vec(infile,w->sr2gr);
	gr2ml_vec(infile,&nlooks,w->ml2gr);
	for (i=8191; i>=0; i--) 
		if ((int)w->sr2gr[i] > w->inNS) 
			w->outNS = i;
	for (i=8191; i>=0; i--) 
		if ((int)w->ml2gr[i] > w->inNL) 
			w->outNL = i;
	w->scaleSample=((double)w->outNS)/w->inNS;
	w->scaleLine=((double)w->outNL)/w->inNL;
	printf("Now rescaling metadata: \n\
Input size: %ld samples by %ld lines\n\
Output size: %ld samples by %ld lines.\n\
Sample scale=%f, line scale=%f.\n",w->inNS,w->inNL,w->outNS,w->outNL,w->scaleSample,w->scaleLine);
}
void process_dssr(unsigned char *buf,metaWriteRecord *w);
void process_raddr(unsigned char *buf,metaWriteRecord *w);
void process_dqsr(unsigned char *buf,metaWriteRecord *w);
void process_dhr(unsigned char *buf,metaWriteRecord *w);
void process_facdr(unsigned char *buf,metaWriteRecord *w);

void chunk_copier(struct HEADER *bufHeader,unsigned char *buf,int era,void *passThisAlong)
{
	metaWriteRecord *w=(metaWriteRecord *)passThisAlong;
	int type=(int)bufHeader->rectyp[1];
	buf-=12;
	w->era=era;
	printf("Now processing ");
	switch(type)
	{
		case 10: 
			printf("a DSSR.\n");
			process_dssr(buf,w);
			break;
		case 50:
			printf("a RADDR.\n");
			process_raddr(buf,w);
			break;
		case 60: 
			printf("a DQSR.\n");
			process_dqsr(buf,w);
			break;
		case 70: 
			printf("a DHR.\n");
			process_dhr(buf,w);
			break;
		case 200:
		case 210: 
			printf("a FACdr.\n");
			process_facdr(buf,w);
			break;
		default: printf("an unrecognized ASF LDR record type: %ld\n",type);break;
	}
	buf+=12;
	fwrite(bufHeader,1,12,w->outFile);
	fwrite(buf,1,bufHeader->recsiz-12,w->outFile);
}
void print_usage(void);
int main(int argc, char **argv)
{
	if (argc==4)
	{
		char *cmdOption=argv[1],*infilename=argv[2],outfilename[260];
		metaWriteRecord w;
		strcat(strcpy(outfilename,argv[3]),".L");
		w.infilename=infilename;
		w.outFile=fopen(outfilename,"w+");
		if (!w.outFile) 
			{fprintf(stderr,"Can't create output file %s\n",outfilename);return (0);}
		if ((cmdOption[0]!='-')||
		    (!(cmdOption[1]=='n'||cmdOption[1]=='s')))
		    	print_usage();
		w.option=cmdOption[1];
		if (w.option=='s')
			init_slant(&w);
		parse_ldr(infilename,&w,chunk_copier);
		fclose(w.outFile);
		return 1;
	} else print_usage();
}
void print_usage(void)
{
	printf("\n\nUsage: meta [-s|-n] <inSARfilename> <outSARfilename>\n\
\n\t-s\tconvert metadata to Slant range\
\n\t-n\tdo Nothing to metadata (copy)\
\n\n\tVersion:%.2f\n\n",version);
	exit(0);
}
/*************** Record Processors ***************/
typedef enum {toASCII,fromASCII} codingDirection;
void Code_DSSR(unsigned char *buf,struct dataset_sum_rec * q,int era,codingDirection dir);
void Code_RADDR(unsigned char *buf, struct VRADDR* q,codingDirection dir);
void Code_DQS(unsigned char *buf, struct qual_sum_rec* q, int era,codingDirection dir);
void Code_DHR(unsigned char *buf,struct data_hist_rec* q,codingDirection dir);
void Code_FACDR(unsigned char *buf,struct VFDRECV *ofdr,int era,codingDirection dir);

void  replaceGwSandSwR(char *string)
{
	int i;
	for (i=0;string[i];i++)
		if (string[i]=='G') 
			string[i]='S';
		else if (string[i]=='S') 
			string[i]='R';
}
void process_dssr(unsigned char *buf,metaWriteRecord *w)
{
	struct dataset_sum_rec s;
	Code_DSSR(buf,&s,w->era,fromASCII);
	/*Modify data set summary record here*/
	if (w->option=='s')
	{/*Convert to Slant range*/
		replaceGwSandSwR(s.product_id);/*Change the product id*/
		replaceGwSandSwR(s.fac_code);/*Change the facility code*/
		strcpy(s.product_type,"RAMS TEST");/********This will change with RADARSAT data.*/
		s.n_azilok=5.0;/********This will change with RADARSAT data, too.*/
		s.sc_lin*=w->scaleLine;
		s.sc_pix*=w->scaleSample;
		s.crt_dopcen[1]/=w->scaleSample;
		s.crt_dopcen[2]/=(w->scaleSample*w->scaleSample);
		s.crt_rate[1]/=w->scaleSample;
		s.crt_rate[2]/=(w->scaleSample*w->scaleSample);
		s.line_spacing/=w->scaleLine;
		s.pixel_spacing/=w->scaleSample;
		s.pixel_spacing*=fabs(sin(w->dssr->incident_ang*M_PI/180.0));
		s.azi_res=1.5*s.line_spacing;/*azimuth resolution is 1.5 times line spacing*/
	}
	/*prn_dssr(&s,w->era);*/
	Code_DSSR(buf,&s,w->era,toASCII);
}
void process_raddr(unsigned char *buf,metaWriteRecord *w)
{
	struct VRADDR r;
	buf+=12;
	Code_RADDR(buf,&r,fromASCII);
	/*Modify radiometric data record here*/
	if (w->option=='s')
	{/*Convert to Slant range*/
		int i,carefulOutNS=0;
		double newNoise[256];
		r.a[0]*=256.0*256.0;/*Scale noise values by 65,536*/
		r.a[1]/=65536.0;/*Scale (samples squared + noise) by 1/65,536*/
		/*Now, we need to remap the noise(range) to the new
		range values.*/
		for (i=8191; i>=0; i--) 
		if (w->sr2gr[i] > (float)w->inNS) 
			carefulOutNS = i;
		for (i=0;i<256;i++)
		{
			float srVal=((double)i)/255*(carefulOutNS-1);
			double grVal=w->sr2gr[(int)srVal];
			int grInt=(int)grVal,grIndex=((double)((int)grVal))/w->inNS*255;
			float grUpper=grVal-grInt;
			float grLower=1.0-grUpper;
			newNoise[i]=r.noise[grIndex]*grLower+r.noise[grIndex+1]*grUpper;
		}
		for (i=0;i<256;i++)
			r.noise[i]=newNoise[i];
	}
	/*prn_raddr(&r,w->era);*/
	Code_RADDR(buf,&r,toASCII);
}
void process_dqsr(unsigned char *buf,metaWriteRecord *w)
{
	struct qual_sum_rec q;
	Code_DQS(buf, &q, w->era,fromASCII);
	/*Modify data set quality record here*/
	if (w->option=='s')
	{/*Slant range convert*/
		/*(we don't have much to do here)*/
	}
	/*prn_dqsr(&q,w->era);*/
	Code_DQS(buf, &q, w->era,toASCII);
}
void process_dhr(unsigned char *buf,metaWriteRecord *w)
{
	struct data_hist_rec h;
	struct hist_dset *d;
	Code_DHR(buf,&h,fromASCII);
	/*Modify data histogram record here*/
	d=h.data;
	while (d)
	{/*For each dataset:*/
		if (w->option=='s')
		{/*Slant range converter*/
			d->ns_lin=w->outNL;
			d->ns_pix=w->outNS;
			d->max_smp=65535.0;
			d->ns_pix*=w->scaleSample;
			d->ns_lin*=w->scaleLine;
			d->ngrp_lin*=w->scaleLine;
			d->ngrp_pix*=w->scaleSample;
			d->nsamp_lin*=w->scaleLine;
			d->nsamp_pix*=w->scaleLine;
			d->mean_smp*=256.0;/*We're scaling the data 
			by 256, so we have to
			scale the histogram, too.*/
			d->std_smp*=65536.0;
		}
		d=d->next;
	}
	/*prn_dhr(&h);*/
	Code_DHR(buf,&h,toASCII);
}
void process_facdr(unsigned char *buf,metaWriteRecord *w)
{
	struct VFDRECV f;
	Code_FACDR(buf, &f,w->era,fromASCII);
	/*Modify facility-related data record here*/
	if (w->option=='s')
	{/*Slant range converter*/
		replaceGwSandSwR(f.imageid); /*Replace some letters in the image name*/
		replaceGwSandSwR(f.sitename); /*Replace some letters in the site name*/
		strcpy(f.grndslnt,"SLANT");/*Image is slant range, now*/
		strcpy(f.deskewf,"NOT");/*Image is not deskewed*/
		f.dpplrslp/=w->scaleSample;/*divide slope terms by scale*/
		f.dpplrqdr/=(w->scaleSample*w->scaleSample);/*and quadratic terms by scale*scale*/
		f.dpratslp/=w->scaleSample;
		f.dpratqdr/=(w->scaleSample*w->scaleSample);
		f.npixels*=w->scaleSample;
		f.nlines*=w->scaleLine;
		f.apixels*=w->scaleSample;
		f.alines*=w->scaleLine;
		f.nlooksaz=5.00;/*****************This will change with RADARSAT */
		f.rapixspc/=w->scaleSample;
		f.rapixspc*=fabs(sin(w->dssr->incident_ang*M_PI/180.0));
		f.azpixspc/=w->scaleLine;
		f.imresaz=1.5*f.azpixspc;/*azimuth resolution is 1.5*pixel spacing*/
	}
	/*prn_facdr(&f);*/
	Code_FACDR(buf, &f,w->era,toASCII);
}
