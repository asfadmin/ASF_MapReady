/*Write_dlt:
	A program to create an ASF-format product tape, given
a slew of order directories.

Calls:
	ls to create a list of product VDFs.
	concat_vdf to assemble the order directories' VDFs
	write_tape to copy all the files to tape
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ceos.h"

#include "config.h"

/*Prelimary Routines:*/

/*Execute the given command, checking to make sure it worked.*/
void execute(char cmd[]) { 
	int errorCode=0;
	printf("\n$$$$$$$$###### Write_dlt #####$$$$$$$$$\n"
		"Executing: %s\n\n",cmd); 
	errorCode=system(cmd);
	if (0!=errorCode)
	{
		fprintf(stderr,
	"****  Fatal Error in write_dlt!\n"
	"****  Command:\n"
	"**** '%s' returned error code %i!\n",cmd,errorCode);
		exit(errorCode);
	}
}


/*
Ask the user the specified question (message).
	If the user responds affirmative, return 1.
	if the user responds negative, return 0.
	if the user wants to quit, do.
*/
int areYouSure(char *message)
{
	char input[255];
	printf("\n%s\n"
	"(y\\n\\q) >",message);
	fgets(input,255,stdin);
	if (toupper(input[0])=='Y')
		return 1;
	else if (toupper(input[0])=='Q')
	{
		printf("Exiting at user request.\n");
		exit(1);
	}
	else if (toupper(input[0])=='N')
		return 0;
	else 
	{
		printf("Unrecognized response:%s\n",input);
		return areYouSure(message);
	}
}

/*rmFiles: removes each file listed in "input".*/
void rmFiles(char *inputFile)
{
	char cmd[255],buf[255],lastDir[255]="";
	FILE *in=fopen(inputFile,"r");
	
	/*For each line of input...*/
	while (NULL!=fgets(buf,255,in))
	{
		int i;
		char orderDir[255];
		
		/*Extract out the file name (first part, before space).*/
		sscanf(buf,"%s",orderDir);

		/*extract out the order directory path*/
		i=strlen(orderDir);
		while (orderDir[i]!='/') 
			i--;
		orderDir[i]=0;
	
		/*Remove this order directory, if it isn't already gone.*/
		if (0!=strcmp(lastDir,orderDir))
		{
			printf("Removing directory '%s'\n",orderDir);
			sprintf(cmd,"/bin/rm -r %s ",orderDir);
			system(cmd);
			strcpy(lastDir,orderDir);
		}
	}
	fclose(in);
}

int main(int argc, char *argv[])
{
	int tapeNo,argNo;
	char command[1024],tmp[256];
	char *tapeLoadMessage;

/*Check the input arguments.*/
	if (argc<2)
	{
		printf("Usage: write_8mm <path...>\n"
	"  <path...> is a set of directories containing\n"
	"VDF files and products.\n"
	"\n"
	"  Write_8mm searches the given path for VDF files,\n"
	"concatenates these files using concat_vdf, then writes\n"
	"all the files it finds to the hardcoded device '%s'.\n"
	"Version 1.2, ASF STEP Tools.\n\n",TAPE_DEVICE);
		exit(1);
	}
/*Files used:
file_list: raw output of concat_vdf
tape_files: file_list, munged for the tape contents listing.
mail_files: file_list, munged for the informatory e-mail.
tape_header: contains date, tape-writer, tape #.
mail_header: contains 
*/
	system("rm vdf_list tape_files tape_header mail_files file_list > /dev/null 2>&1");

/*Find all the files to write.*/
	
	printf("Making a list of products...\n");
	
	sprintf(command,"ls ");
	for (argNo=1;argNo<argc;argNo++)
	{
		sprintf(tmp,"%s/*.VDF ",argv[argNo]);
		strcat(command,tmp);
	}
	strcat(command," > vdf_list");
	execute(command);
	
/*Create the list of tape files.*/
	printf("Creating a list the tape contents...\n");
	execute("concat_vdf output.VDF -vdf_list > file_list");
	
/*Get the tape number from the user.*/
	printf("Please enter the number of this 8mm tape:");
	fgets(command,256,stdin);
	if (1>sscanf(command,"%d",&tapeNo))
		{printf("That is not a number.\n");exit(1);}
	
/*Add a header and massage the list of tape contents */
	sprintf(command,"echo \"# ASF Groundstation 8mm Tape #%d.\" > tape_header",tapeNo);
	system(command);
	system("echo \"#\" >> tape_header");
	system("echo \"#  Written on\" >> tape_header");
	system("date >> tape_header");
	system("echo \"#\" >> tape_header");
	system("echo \"#  By ASF operator\" >> tape_header");
	system("who am i >> tape_header");
	system("echo \"#\" >> tape_header");
	system("echo \"# 8mm Tape Contents (file-by-file):\" >> tape_header" );
	system("echo \"# Format: order number/product number.ext\" >> tape_header");
	system("cat tape_header > tape_files");
	
/*Write as many tapes as the user wants.*/
	tapeLoadMessage="Please load the 8mm tape drive.\nWould you like to write the first tape?";
	while (areYouSure(tapeLoadMessage))
	{
		printf("Writing product tape #%d to device %s...\n",tapeNo,TAPE_DEVICE);
		sprintf(command,"create_tape %s -vdf_list",TAPE_DEVICE);
		execute(command);
		printf("Tape #%d was written sucessfully!\n",tapeNo);
		if (areYouSure("Would you like to print a listing of the tape contents?"))
			system("lp tape_files");
		tapeLoadMessage="Please unload and reload the 8mm drive.\n"
			"Would you like to write another tape?";
	}
	
	printf("\nDone writing tapes...\n");
	
	if (areYouSure("Would you like to e-mail the tape contents to Martha Mason et. al?"))
	{
		system("echo \"Subject: ASF RGPS 8mm Tape Written\" > mail_files");
		system("echo \"To: " EMAIL_RECIPIENTS " \" > mail_files");
		system("echo \"Automated e-mail notification follows:\" >> mail_files");
		system("cat mail_files tape_header file_list | mail " EMAIL_RECIPIENTS );
	}

/*Optionally remove all written files.*/
	if (areYouSure("Would you like to remove the files we just wrote out?"))
		rmFiles("file_list");

	system("rm vdf_list tape_header mail_files file_list > /dev/null 2>&1");
	printf("A list of files written is in 'tape_files'\n");
	return 0;
}
