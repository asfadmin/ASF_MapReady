#include "asf.h"
#include "decoder.h"
#include "ceos.h"
#include "asf_endian.h"


void linkFlag(short, char*);

/*********************************
openCeos:
	Open the given CEOS image, seek to
the beginning of the file, and return a file pointer
*/
FILE *openCeos(const char *fName)
{
	FILE *ret=FOPEN(fName,"rb");
	FSEEK64(ret,0,0);/*Seek to beginning of file*/
	getNextCeosLine(ret);/*Skip over first line of file*/
	return ret;
}

/*********************************
getNextCeosLine:
	Reads the next entire CEOS record from
the given file.  Returns pointer into static
buffer.
*/
signalType *getNextCeosLine(FILE *f)
{
	static int headerLen=12,totHeaderLen=192;
	static signalType buffer[100000];/*Input Buffer*/
	struct HEADER head;
	int length;
	if (headerLen!=fread(&head,1,headerLen,f))
	{
		printf("Finished with input file!\n");
		linkFlag(2,NULL);
		StopWatch();
		exit(1);
	}
	length=bigInt32(head.recsiz);
	FREAD(&buffer,1,length-12,f);
	return &buffer[totHeaderLen-headerLen];
}

/*********************************
linkFlag:
	Used to make & remove .L & .D links to
	 .ldr & .raw  extensions for compatibility
*/
void linkFlag(short link, char *inName)
{
	char temp[1280];
	char old_leader_name[1024], new_leader_name[1024];
	char new_data_name[1024];
	static short leader=0;
	static short data=0;
	static char file[1024];
	
	if (inName != NULL)
	{
		strcpy(file,inName);
	}
	switch (link) {
	 case 0: /*HACK: link .ldr file over to .L file-- keeps get_facdr happy*/
	 	create_name(old_leader_name, file, ".ldr");
	 	create_name(new_leader_name, file, ".L");
		sprintf(temp,"ln -s %s %s",old_leader_name,new_leader_name);
		system(temp);
		leader=1;
		break;
	 case 1: /*HACK: link .raw (or whatever) over to .D file-- keeps get_iof happy*/
	 	create_name(new_data_name, file, ".D");
		sprintf(temp,"ln -s %s %s",file,new_data_name);
		system(temp);
		data=1;
		break;
	 case 2: /* Remove ugly hack */
		if (leader)
		{
	 		create_name(new_leader_name, file, ".L");
			unlink(new_leader_name);
			leader=0;
		}
		if (data)
		{
	 		create_name(new_data_name, file, ".D");
			unlink(new_data_name);
			data=0;
		}
		break;
	}		
}
