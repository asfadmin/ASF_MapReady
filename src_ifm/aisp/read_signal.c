/*******************************************************************
Read_signal:
	A package designed to read in various types of signal data,
thus allowing a SAR processor to stay relatively independant
of the specifics of the signal files.

	Currently works with both ASF CCSD data, ASF JPL
RDS RADARSAT Engineering files, and raw data.

	Main external entry points:

getRec * fillOutGetRec(char file[]);
void getSignalLine(getRec *r,long long lineNo,FCMPLX *destArr,int readStart,int readLen);

*/
#include "asf.h"
#include <unistd.h>
#include "aisp_defs.h"
#include "read_signal.h"
#include "ceos.h"

/****************************************
getSignalFormat:
	Reads the number of good bytes, number of bytes per line,
I and Q DC offset, and i/q flip parameters from 
".fmt" input file.
*/
#define FILL(A,B,C)	if (fgets((A),(B),(C))!=NULL) 

void getSignalFormat(char *baseName,long long bytesInFile,getRec *r)
{
	char name[255];
	char buf[80];
	FILE *fp;
	create_name(name,baseName,".fmt");
	fp=FOPEN(name,"r");
	FILL(buf,80,fp) sscanf(buf,"%i%i", &r->lineSize, &r->header);
	FILL(buf,80,fp) sscanf(buf,"%f%f", &r->dcOffsetI, &r->dcOffsetQ);
	FILL(buf,80,fp) sscanf(buf,"%c", &r->flipIQ); 
	
	r->nLines=bytesInFile/r->lineSize;/*Automatically figure out number of lines in file.*/
	r->nSamples=(r->lineSize-r->header)/r->sampleSize;/*Ditto for samples*/
	
	fgets(buf,80,fp);/*Skip comment line*/
	if (NULL==fgets(buf,80,fp)) 
		return; /*If no extra data, bail!*/
	else /*File has information about window shifts and AGC compensation*/
	{/*Read infomation about each line of the file*/
		float firstWindow;/*First Window Position shift*/
		int lastLine;float lastAGC,lastWindow;
		int keepGoing=1;
		
		r->lines=(signalLineRec *)MALLOC(r->nLines*sizeof(signalLineRec));
		
		/*Read first line's info.*/
		sscanf(buf,"%d%g%g",&lastLine,&lastWindow,&lastAGC);
		firstWindow=lastWindow;
		
		while (keepGoing)
		{
			int line,thisLine;
			if (NULL==fgets(buf,80,fp))
			{/*Hit EOF*/
				keepGoing=0;/*Don't keep going*/
				thisLine=r->nLines;/*Write to last line*/
			} else
				sscanf(buf,"%d",&thisLine);
			
			/*Write old values until this line*/
			for (line=lastLine;line<thisLine;line++)
			{
				r->lines[line].shiftBy=(int)(lastWindow-firstWindow);
				r->lines[line].scaleBy=lastAGC;
			}
			/*Read new values*/
			sscanf(buf,"%d%g%g",&lastLine,&lastWindow,&lastAGC);			
		}
	}
	fclose(fp);
}

/******************************************************************************
fillOutGetRec:
	Given a file name, returns a "getRec", which you can pass to
getSignalLine to fetch a line of signal data.
*/
getRec * fillOutGetRec(char file[])
{
	getRec *r=(getRec *)MALLOC (sizeof(getRec));
	char name_ASF[255],name_RAW[255];
	FILE *fp_ASF, *fp_RAW;
	int skip_second_if=0;	

	create_name(name_ASF,file,".D");
	/* Lower case fopen used to control if statement */
	fp_ASF=fopen(name_ASF,"rb");
	
	create_name(name_RAW,file,".raw");
	/* Lower case fopen used to control if statement */
	fp_RAW=fopen(name_RAW,"rb");
	
	r->lines=NULL;
	if (fp_ASF!=NULL)
	{
  		struct        IOF_VFDR ifiledr;
		r->flipIQ='y';
  		r->sampleSize=2;
		r->fp_in=fp_ASF;
		get_ifiledr(file,&ifiledr);
		if(!quietflag)printf("Looking for %s...\n",name_ASF);

	/**Make sure input file is a CCSD file and not a CEOS**/
	     if (strcmp(ifiledr.formcode,"CI*2")!=0)
		{
		printf("  %s is not a CCSD file.\n", name_ASF);
		if (!quietflag)printf("Looking for %s...\n",name_RAW);
		}
	     else
		{ 
		if (!quietflag) 
		  printf("   Found ASF CCSD file '%s'...\n",name_ASF);
		skip_second_if++;
		r->lineSize=ifiledr.reclen;
		r->nSamples=ifiledr.datgroup;
		FSEEK64(r->fp_in,0,SEEK_END);
		r->nLines=FTELL64(r->fp_in)/r->lineSize;/*Automatically figure out number of lines in file.*/
		r->header=ifiledr.reclen+(ifiledr.reclen - ifiledr.sardata);
		r->dcOffsetI=r->dcOffsetQ=15.5;
		}
	} 
	if ((fp_RAW!=NULL)&&(!skip_second_if))
	{
		long long bytesInFile;
		
		if (!quietflag) printf("   Found Raw, manual file '%s'...\n",name_RAW);
		
		r->header=0;/*Assume a zero-byte header*/
		r->dcOffsetI=r->dcOffsetQ=15.5;
		r->flipIQ='n';
		r->sampleSize=2;
		r->fp_in=fp_RAW;
		FSEEK64(r->fp_in,0,SEEK_END);
		bytesInFile=FTELL64(r->fp_in);
		
		getSignalFormat(file,bytesInFile,r);
	} 
	r->inputArr=(unsigned char *)MALLOC(r->sampleSize*r->nSamples);
	return r;
}
/****************************************
getSignalLine:
	Fetches and unpacks a single line of signal data
into the given array.
*/
void getSignalLine(const getRec *r,long long lineNo,FCMPLX *destArr,int readStart,int readLen)
{
	int x;
	int left,leftClip,rightClip;
	int windowShift=0;
	float agcScale=1.0;
	FCMPLX czero=Czero();
	
/*If the line is out of bounds, return zeros.*/
	if ((lineNo>=r->nLines)||(lineNo<0))
	{
		for (x=0;x<readLen;x++)
			destArr[x]=czero;
		return;
	}
/*Fetch window shift and AGC comp. if possible*/
	if (r->lines!=NULL)
	{
		windowShift=r->lines[lineNo].shiftBy;
		agcScale=r->lines[lineNo].scaleBy;
	}
	
/*Compute which part of the line we'll read in.*/
	leftClip=left=readStart-windowShift;
	if (leftClip<0) {leftClip=0; /*left=0;*/}
	rightClip=left+readLen;
	if (rightClip>r->nSamples) rightClip=r->nSamples;
	
/*Read line of raw signal data.*/
	FSEEK64(r->fp_in,r->header+lineNo*r->lineSize+leftClip*r->sampleSize,0);
	if (rightClip-leftClip!=
	    fread(r->inputArr,r->sampleSize,rightClip-leftClip,r->fp_in))
		{
		 sprintf(errbuf,"   ERROR: Problem reading signal data file on line %lld!\n",lineNo);
/*		 fprintf(stderr,"Read length = %d, Left = %d Readstart = %d\n",readLen, left,readStart);
		 fprintf(stderr,"Lines = %lld Samples = %lld, rightClip = %d, leftClip = %d window = %d\n"
			 ,r->nLines, r->nSamples, rightClip, leftClip, windowShift); */
		 printErr(errbuf);
		}
	leftClip-=left;
	rightClip-=left;

/*Fill the left side with zeros.*/
	for (x=0;x<leftClip;x++)
		destArr[x]=czero;
	
/*Unpack the read-in data into destArr:*/
	if (r->flipIQ=='y')
		/*is CCSD data (one byte Q, next byte I)*/
		for (x=leftClip;x<rightClip;x++)
		{
			int index=2*(x-leftClip);
			destArr[x].r=agcScale*(r->inputArr[index+1]-r->dcOffsetQ);
			destArr[x].i=agcScale*(r->inputArr[index]-r->dcOffsetI);
		}
	else /*if (r->flipIQ=='n')*/
		/*is Raw data (one byte I, next byte Q)*/
		for (x=leftClip;x<rightClip;x++)
		{
			int index=2*(x-leftClip);
			destArr[x].r=agcScale*(r->inputArr[index]-r->dcOffsetI);
			destArr[x].i=agcScale*(r->inputArr[index+1]-r->dcOffsetQ);
		}
	
/*Fill the right side with zeros.*/
	for (x=rightClip;x<readLen;x++)
		destArr[x]=czero;
}
/**************************************
freeGetRec:
	Disposes of a getRec structure.
*/
void freeGetRec(getRec *r)
{
	fclose(r->fp_in);
	if (r->lines!=NULL)
		FREE((void *)r->lines);
	free((void *)r->inputArr);
	free((void *)r);
}

/***************************************
fetchReferenceFunction:
	Reads in and returns a range reference function.
*/
void fetchReferenceFunction(char *fname,FCMPLX *ref,int refLen)
{
	int i;
	FILE *in;
	char name_REPLICA[255],line[255];
	create_name(name_REPLICA,fname,".replica");
	in=FOPEN(name_REPLICA,"rb");
	
	if (!quietflag) printf("   Reading pulse replica from '%s'.\n",name_REPLICA);
	fgets(line,255,in);
	for (i=0;i<refLen;i++)
	{
		fgets(line,255,in);
		sscanf(line,"%f%f",&(ref[i].r),&(ref[i].i));
	}
	FCLOSE(in);
}
