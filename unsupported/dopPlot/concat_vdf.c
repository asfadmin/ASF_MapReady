/***************************************************************************
      PROLOG-- Part of create_tape
        NAME:  concat_vdf -- extracts file names, record sizes, &
 	       volume info from a Volume Description File (VDF).
 	       Allows multiple input VDFs.  
      SYNTAX:  concat_vdf filename
  PARAMETERS:  filename -- name of VDF file to extract names from.	 
 DESCRIPTION:  This program takes the name of a VDF file as input.
	       First, it extracts the volume number, then the total
	       number of volumes.  Then, it scans through the input
	       file extracting all file ids and max record sizes
	       stored therein.  The results are all reported to
	       stdout.
RETURN VALUE:  0 = successful; 2=usage; 3=file not found; 4=can't read file
      AUTHOR:  Shusun Li, GI at UAF, Ak.
	       Modified by Tom Logan 8/93	
       	       Modified by Tom Logan 9/96   - Changed to read new CEOS files
               07/18/97 - Bump exit codes up by 1 to distinguish between
                     a usage call and command not found error- D.Corbett
               7/1/97 - converted from get_names to concat_vdf. O.Lawlor
         1.1   10/29/97- added ability to take names from a file. O.Lawlor
         1.2   3/98- Check for duplicate names in VDFs-- remove them.  O. Lawlor
         1.3   4/98- Check for duplicate PRODUCT names-- remove them.(Duh!)  O. Lawlor
         1.4   6/98- Only write the greatest version number, for duplicates.  O. Lawlor

***************************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

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

RICK GURITZ      rguritz@images.alaska    (907)474-7886
Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed 
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
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
#include	<stdio.h>
#include	<math.h>
#include <string.h>
#include	"VDF.h"

/*Read a signed integer from the given ASCII buffer.*/
int getint(const char *buf, int len)
{
	int val;
	char bufz[255];
	strncpy(bufz,buf,len);
	bufz[len]=0;
	sscanf(bufz,"%i",&val);
	return val;
}
/*Write a signed integer to the given ASCII buffer.*/
void putint(int putThis,char *buf, int len)
{
	int i,lenz;
	char bufz[200];
	sprintf(bufz,"%i",putThis);
	lenz=strlen(bufz);
	if (lenz>len) lenz=len;
	for (i=0;i<lenz;i++)
		buf[i]=bufz[i];
	for (;i<len;i++)
		buf[i]=' ';
}
/*Read a null-terminated string from given space-filled ASCII buffer*/
char * getstr(const char *buf, int len)
{
	int i;
	char *bufz=(char *)malloc(255*sizeof(char));
	strncpy(bufz,buf,len);
	bufz[len]=0;
	for (i=len-1;i>=0;i--)
		if ((bufz[i]==' ')||(bufz[i]=='\t')||(bufz[i]=='\n'))
			bufz[i]=0;
		else 
			break;
	return bufz;
}
/*Extract the portion of a name containing the path to the file--
	e.g. getPathname("/tmp/stuff/bob.txt")=="/tmp/stuff/"
*/
char *getPathname(const char *in)
{
	int len,i;
	char *out=(char *)malloc(255*sizeof(char));
	len=strlen(in);
	for (i=len-1;(i>=0)&&(in[i]!='/');i--)
		len--;
	for (i=0;i<len;i++)
		out[i]=in[i];
	out[len]=0;
	return out;
}
/*1== file exists; 0==error, file does not exist.*/
int fileExists(const char *fname,const char *errorMessage)
{
	FILE *f;
	f=fopen(fname,"r");
	if (NULL==f)
		{fprintf(stderr,"create_tape error:\n\t");fprintf(stderr,errorMessage,fname);return 0;}
	fclose(f);
	return 1;
}

typedef struct {
	char name[255];
	char pathName[255];
	int blockSize;
	file_pointer_record fpr;
	void *next;
} productFileRec;

static productFileRec *listHead=NULL;

int num_filePointers=0,num_textRecords=0;

/*Insert given productFileRec into the list, making sure none other has the same name.*/
void insertIntoList(productFileRec *n)
{
	/*Search file list for new name.*/
	productFileRec *p=listHead,*prev=NULL;
	while (p!=NULL)
	{
		/*If we find the "new" name already in the list...*/
		if ((0==strncmp(p->name,n->name,13)) && 
			(p->name[17]==n->name[17]))
		{
			/*If we're newer than the current occupant, remove them; insert us.*/
			if (strcmp(n->name,p->name)>0)
			{
				fprintf(stderr,"%s is a duplicate file (replacing by %s)!\n",p->pathName,n->name);
				if (prev)
					prev->next=(void *)n;
				n->next=p->next;
			}
			else fprintf(stderr,"%s is a duplicate file (already as %s)!\n",n->pathName,p->name);
			return;
		}
		prev=p;
		p=(productFileRec *)p->next;
	}
	/*If it's not in the list, add it.*/
	n->next=(void *)listHead;
	listHead=n;
	num_filePointers++;
}


main(argc, argv)
int	argc;
char	**argv;
{
	int argNo;
	volume_descriptor_record invdf,outvdf;
	FILE	*fpin,*fpout,*vdfFile=NULL;
	char fpin_name[255];

	if (argc < 3)
		{ fprintf(stderr,"Usage: %s outVDF [ -vdfFile | vdf1 [ vdf2 [...] ] ]\n"
		"  outVDF-- output VDF file name.\n"
		"  -vdfFile-- an optional text file containing\n"
		"       the names of the source VDF files.\n"
		"  vdf1..vdfN-- the names (including paths) of input VDF files.\n"
		" \n"
		"    This program concatenates several VDF (CEOS Volume Descriptor File)\n"
		"records into one big VDF file.  A VDF is the first thing on a CEOS-compatible\n"
		"computer tape, which is a sort of table of contents of the tape.\n"
		"ASF SAR Tools, 1997.  Version 1.3\n", argv[0]); exit(1);}
/*Get the first file name (possibly from argv, possibly from a file.*/
	if (argv[2][0]=='-')
	{
		char *vdfFileName=&argv[2][1];
		vdfFile=fopen(vdfFileName,"r");
		if (vdfFile)
			fprintf(stderr,"Reading vdf file names from %s...\n",vdfFileName);
		else {fprintf(stderr,"Couldn't open vdf names file %s.\n",vdfFileName);exit(1);}
		fscanf(vdfFile,"%s",fpin_name);
	} else strcpy(fpin_name,argv[2]);
	
	if ((fpin = fopen(fpin_name, "rb")) == NULL)
		{ fprintf (stderr, "Can't open input VDF file %s for reading\n", fpin_name); exit (1); }
	fread((void *)&outvdf,sizeof(volume_descriptor_record),1,fpin);
	fclose(fpin);
	
	argNo=2;
/*Read and chain product names from VDF's.*/
	while (1)
	{
		char *pathname;
		productFileRec *p=(productFileRec *)malloc(sizeof(productFileRec));
		pathname=getPathname(fpin_name);
		if ((fpin = fopen(fpin_name, "rb")) == NULL)
			{ fprintf (stderr, "Can't open input VDF file %s for reading\n", fpin_name); exit (1); }
		fread((void *)&invdf,sizeof(volume_descriptor_record),1,fpin);
		
		while (1==fread ((void *)&p->fpr,sizeof(file_pointer_record),1, fpin))
		{
			char *fileCode,*productName;
			int blockSize;
			switch((subType1Code)p->fpr.subType1)
			{
			case filePointer:
				strcpy(p->pathName,pathname);
				strcpy(p->name,getstr(p->fpr.i_id,16));
				fileCode=getstr(p->fpr.f_code,4);
				if ((0==strcmp(fileCode,"SARL"))||(0==strcmp(fileCode,"SARL")))
					strcat(p->name,".L");
				else if (0==strcmp(fileCode,"IMOP"))
					strcat(p->name,".D");
				else
				{
					fprintf(stderr,"Fatal Error: unknown image type code '%s'\n",fileCode);
					exit(1);
				}
				strcat(p->pathName,p->name);
				p->blockSize=getint(p->fpr.max_rec_len,8);
				if (!fileExists(p->pathName,"File %s specified by VDF does not exist.\n"))
					{fprintf(stderr,"\tVDF file name=%s\n",fpin_name);exit(1);}
				insertIntoList(p);
				p=(productFileRec *)malloc(sizeof(productFileRec));
				break;
			case textRecord:
				/*We don't have to do much with text records.  They're pretty useless.*/
				break;
			default:
				fprintf(stderr,"Fatal Error: unknown CEOS type %i encountered in VDF %s.\n",(int)p->fpr.subType1,argv[argNo]);
				exit(1);
			}
		}
		fclose(fpin);
		
		/* Get the next VDF name*/
		if (vdfFile)
			{ if (1!=fscanf(vdfFile,"%s",fpin_name)) break; }
		else 
		{
			if (++argNo>=argc) break;
			strcpy(fpin_name,argv[argNo]);
		}
	}
/*Write VDF Header.*/
	fpout=fopen(argv[1],"wb");
	if (fpout==NULL)
		{ fprintf (stderr, "Can't open output VDF file %s for writing\n", argv[1]); exit (1); }
	putint(num_filePointers,outvdf.numFPR,4);
	putint(1+num_filePointers+num_textRecords,outvdf.numRecords,4);
	fwrite((void *)&outvdf,sizeof(volume_descriptor_record),1,fpout);
/*Write/print chained file descriptors.*/
	num_filePointers=1;
	while (listHead!=NULL)
	{
		listHead->fpr.record_num=num_filePointers;
		putint(num_filePointers,listHead->fpr.ser_num,4);
		fwrite((void *)&listHead->fpr,sizeof(file_pointer_record),1,fpout);
		printf("%s\t%i\n",listHead->pathName,listHead->blockSize);
		listHead=(productFileRec *)listHead->next;
		num_filePointers++;
	}
	fclose(fpout);
	return 0;
}


