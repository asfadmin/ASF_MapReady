/******************************************************************************
NAME:	check_name_of -- determines the "era" of an image file, by checking
	the metadata, then renames the file appropriately.

SYNOPSIS:  check_name_of <filename>
		<filename> -- name of SAR file whose name to check

DESCRIPTION:  check_name_of reads the CEOS imagery options file to determine the
	number of header bytes.  If this number is zero, then the image must be
	pre-RADARSAT era data and will be changed to end in .dat and .ldr.  
	If this number of prefix bytes is not zero, then the image is
	post-RADARSAT  era data and will be changed to end with the extensions
	.D and .L.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:        PURPOSE:
    ---------------------------------------------------------------
    1.0	  08/12/97   Orion Lawlor

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   check_name_of -- determines the "era" of an image file, by checking     *
*		     the metadata, then renames the file appropriately.     *
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
            
#include "asf.h"
#include "ceos.h"

#define VERSION 1.0

/*extern int get_myifiledr(char *filename, struct IOF_VFDR *vfdr);*/
  
void reExtension(char *basename,char *oldExtension, char *newExtension)
{
	char oldname[255],newname[255];
	
	strcpy(oldname,basename);
	strcat(oldname,oldExtension);
	
	strcpy(newname,basename);
	strcat(newname,newExtension);
	
	rename(oldname,newname);
}
int main(int argc,char *argv[])
{
	int i;
	char basename[255];
	struct IOF_VFDR *iof=(struct IOF_VFDR *)MALLOC(sizeof(struct IOF_VFDR));
	if (argc!=2 || (argv[1][0]=='-'))
	{
		printf("\nUsage: check_name_of <filename>\n"
			"   <filename>   image file whose name to check.\n"
			"\n"
			"This program checks the metadata of\n"
			"an image to determine if it is pre-RADARSAT-era\n"
			"CEOS or post-RADARSAT-era CEOS, then renames the\n"
			"file appropriately.\n\n"
			"Version %.2f, ASF SAR TOOLS\n\n",VERSION);
		return(2);
	}
/*Copy over filename.*/
	strcpy(basename,argv[1]);
/*Strip off extension.*/
	for (i=strlen(basename)-1;i>0;i--)
		if (basename[i]=='.')
		{
			basename[i]=0;
			break;
		}
/*Find out the true era of the file, by checking the imagery options file.*/
	if (extExists(basename,".dat")||extExists(basename,".D"))
	{
		get_ifiledr(basename,iof);
		if (iof->predata==0&&iof->sufdata==0)
		{/*Old Data- rename appropriately.*/
			/*printf("Found OLD format data %s.\n",basename);*/
			reExtension(basename,".L",".ldr");
			reExtension(basename,".D",".dat");
		} else {
		/*New Data- rename appropriately.*/
			/*printf("Found NEW format data %s.\n",basename);*/
			reExtension(basename,".ldr",".L");
			reExtension(basename,".dat",".D");
		}
	}
	return (0);
}
