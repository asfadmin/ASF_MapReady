/**
  C++ utility routines.
  
  This file is #included by plugin.h; don't include it alone.

  by Orion Sky Lawlor, olawlor@acm.org, 2006/05/11 (ASF)
*/
#include <stdio.h>
#include "asf/plugin.h"

ASF_COREDLL std::string appendExt(const char *name,const char *newExt)
{
	std::string ret;
	const char *ext=findExt(name);
	if (ext) ret=std::string(name,ext-name); /* clip off old extension */
	else /* no old extension */ ret=name;
	
	if (newExt==NULL) return ret; /* <- Silly! */
	
	return ret+newExt;
}

ASF_COREDLL int extExists(const char *name,const char *newExt)
{
	int exists=fileExists(appendExt(name,newExt).c_str());
	return exists;
}

ASF_COREDLL int fileExists(const char *name)
{
	FILE *f=fopen(name,"rb");
	if (f==NULL)
		return 0;
	fclose(f);
	return 1;
}

ASF_COREDLL const char *findExt(const char *name)
{
	int i;
	i=strlen(name)-1;/*Start at end of name.*/
	while ((i>0) && (name[i]!='.') && (name[i]!='/') && (name[i]!='\\'))
		i--;/*Work backwards until we hit a directory separator or extension separator.*/
	
	if ((i>0)&&(name[i]=='.'))
	/*We found an extension!*/
		return &name[i];
	else
	/*We couldn't find an extension.*/
		return NULL;
}

ASF_COREDLL FILE *fopenImage(const char *fName,const char *access)
{
	int forWriting=0;
	FILE *fRet=NULL;
	std::string n=fName;
	
        /* Check to make sure that the (bone-headed) user didn't ask
           us to open a .ddr image or .meta file. */
	const char *ext=findExt(n.c_str());
	if (NULL!=ext)
	{
	  if ( (0==strcmp(ext,".ddr")) || (0==strcmp(ext,".meta")))
	    n=appendExt(n.c_str(),NULL);/*Clip off stupid extention-- will append .img later*/
	  else if ( ((0==strcmp(ext,".D"))||(0==strcmp(ext,".L"))) && access[0]=='w')
	    n=appendExt(n.c_str(),NULL);/*Clip off stupid extention-- will append .img later*/
	}

/*Find the file's actual name, with the correct extension.*/
	if (access[0]=='w')
	{/*We're opening for writing-- we have to be conservative.*/
		forWriting=1;
		if (NULL==findExt((char *)fName))/*If there is no extension,*/
			n=appendExt(n.c_str(),".img");/*Append .img*/
		/*else There was an extension, so just use it. */
	} 
	else
	{/*We're opening for reading-- search many extensions for the image.*/
		static const char *extTable[]={".img",".dem",".ht",".coh",NULL};
		int extNo=0;
		while (1)
		{
			if (fileExists(n.c_str()))
			{/*We found a file with the given basename and extension!*/
				break;/*Once we find one, we're done.*/
			}
			/* else no such file--try another common extension */
			const char *ext=extTable[extNo++];
			if (ext==NULL) break; /* no sign of the file */
			n=appendExt(n.c_str(),ext);
		}
	}
/*Try to open this name.*/
	fRet=fopen(n.c_str(),access);
	if (fRet!=NULL)
	{/*We've sucessfully opened the file.*/
		return fRet;/*Return the file pointer.*/
	}
/*An error occured-- tell the user and quit.*/
	fprintf(stderr,
			"********************** ERROR! ***********************\n"
			"An error occured trying to open for %s the\n"
			"file named '%s'.\n\n",forWriting?"write or create":"reading",
			fName);
	if (forWriting)
		fprintf(stderr,
			"This could be because you don't have write permissions\n"
			"in this directory, because your disk quota is full, or \n"
			"because your disk is full.\n");
	else /*for reading*/
		fprintf(stderr,
				"This could be because the file doesn't exist in this\n"
				"directory, or you don't have read permissions for the\n"
				"file.  I even searched for common image extensions.\n");
	exit(102);
	return NULL;
}
