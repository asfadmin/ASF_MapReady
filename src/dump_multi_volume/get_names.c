/******************************************************************************
NAME:  get_names extracts file names, record sizes, &
 	       volume info from a Volume Description File (VDF).

SYNOPSIS:  get_names <filename.vdf>
		<filename> -- name of VDF file to extract names from.

DESCRIPTION:  
   
 	Part of dump_multi_volume
		
		This program takes the name of a VDF file as input.
	        First, it extracts the volume number, then the total
	        number of volumes.  Then, it scans through the input
	        file extracting all file ids and max record sizes
	        stored therein.  The results are all reported to
	        stdout.
	 RETURN VALUE:  
	 0 = successful; 2=usage; 3=file not found; 4=can't read file

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0		    Shusun Li,   GI at UAF, Ak.
	   8/93     Tom Logan 	 Modified
       	   9/96     Tom Logan    Changed to read new CEOS files
           06/18/97 D.Corbett    Bump exit codes up by 1 to distinguish between
                     	         a usage call and command not found error- 
    3.0    7/1/97   O.Lawlor     total rewrite-- now checks correctly for
               		         text records. 

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   get_names extracts file names, record sizes, and			    *
* 	       volume info from a Volume Description File (VDF).            *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/

#include	"asf.h"

#include	"VDF.h"

int getint(const char *buf, int len)
{
	int val;
	char bufz[255];
	strncpy(bufz,buf,len);
	bufz[len]=0;
	sscanf(bufz,"%i",&val);
	return val;
}
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
char * getstr(const char *buf, int len)
{
	int i;
	char *bufz=(char *)MALLOC(255*sizeof(char));
	strncpy(bufz,buf,len);
	bufz[len]=0;
	for (i=len-1;i>=0;i--)
		if ((bufz[i]==' ')||(bufz[i]=='\t')||(bufz[i]=='\n'))
			bufz[i]=0;
		else 
			break;
	return bufz;
}
char *getBasename(const char *in)
{
	int len,i;
	char *out=(char *)MALLOC(255*sizeof(char));
	len=strlen(in);
	for (i=len-1;(i>=0)&&(in[i]!='/');i--)
		len--;
	for (i=0;i<len;i++)
		out[i]=in[i];
	out[len]=0;
	return out;
}

#define VERSION 3.0

int main(argc, argv)
int	argc;
char	**argv;
{
	int argNo;
	volume_descriptor_record invdf;
	file_pointer_record	 fpr;
	FILE	*fpin;

	if (argc < 2)
		{ fprintf(stderr,"\nUsage: %s <vdf1>\n"
		"\t<vdf1>   Volume Description File (VDF)"
		"\n\nExtracts the SAR image names from the given\n"
		"CEOS VDF File.  Called by dump_multi_volume.\n"
		"Version %.2f, ASF SAR TOOLS\n\n", argv[0], VERSION); exit(2);}
	
	argNo=1;
	while (argNo<argc)
	{
	char *basename;
	basename=getBasename(argv[argNo]);
 	fpin = FOPEN(argv[argNo], "rb");
	FREAD((void *)&invdf,sizeof(volume_descriptor_record),1,fpin);

printf("volume\t%i\tof\t%i\n",getint(invdf.this_volume,2),getint(invdf.total_volumes,2));
	while (0!=FREAD ((void *)&fpr,sizeof(file_pointer_record),1, fpin))
		{
		char outFileName[255],*fileCode;
		int blockSize;
		switch((subType1Code)fpr.subType1)
		   {
		   case filePointer:
			strcpy(outFileName,basename);
			strcat(outFileName,getstr(fpr.i_id,16));
			fileCode=getstr(fpr.f_code,4);
			if (0==strcmp(fileCode,"SARL"))
				strcat(outFileName,".L");
			else if (0==strcmp(fileCode,"SART"))
				strcat(outFileName,".trl");
			else if (0==strcmp(fileCode,"IMOP"))
				strcat(outFileName,".D");
			else
			{
			fprintf(stderr,"GetNames Warning: unknown image type code '%s'.\n",fileCode);
		   	}
		   blockSize=getint(fpr.max_rec_len,8);
					printf("%s\t%i\t%i\n",outFileName,blockSize,getint(fpr.phys_vol_start,2));
		   break;
		   case textRecord:
		   /*We don't have to do much with text records, except pass over them.*/
		   break;
		   default:
	           fprintf(stderr,"GetNames Warning: unknown CEOS type %i encountered in VDF %s.\n",(int)fpr.subType1,argv[argNo]);
					
	   	   }
		}
	FCLOSE(fpin);
      	argNo++;
        }
        return 0;
}


