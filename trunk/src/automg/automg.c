/******************************************************************
NAME:	    autocc -- automates mask generation to do many orbits at a time

SYNOPSIS:   autocc <inDATfile> <DEMfile> <[asc][desc]> [-w]

DESCRIPTION:


FILE REFERENCES:
	---------------------------------------------------------
    inDATfile:
    	list of orbit numbers

AUTHOR, DATE:	Mike Jessop, 8/03/00
    
******************************************************************/

#include "asf.h"

int orbitnum;	/* needs to be global */

int execute(char *cmd)
{
	printf("%s",cmd);
	fflush(stdin);
	if (system(cmd)==0) return(0); else return(1);
}

main (int argc, char *argv[]) {
    int		orbitnum;
    char 	inFile[255], errFile[255], ascdesc[5],
    		orbit[255], cmd[255],cmd_logoption[20],
    		DEM[255];
    
	FILE *fpi, *fpo;
	
		
	/* Test for proper arguments */
    if (argc != 4 && argc != 5) { 
    	printf("Usage: %s <inDATfile> <DEMfile> <[asc][desc]> [-w]\n",argv[0]);
    	printf("\t-w\twrite maskgen output to log files\n");
    	exit(1); }
        
    strcpy(inFile, argv[1]);
    strcpy(DEM,argv[2]);
    strcpy(ascdesc,argv[3]);
    strcpy(errFile, "errorlog");
    strcpy(cmd_logoption, "");
    
    /* Open input and output files */
    fpi = fopen(inFile, "r");
    if ( fpi == (FILE *) NULL) {
        printf("\n*** couldn't open data file: %s\n", inFile);
        exit(1); }
    fpo = fopen(errFile,"w");
    
    while (!feof(fpi))
    {
		fscanf(fpi, "%d", &orbitnum);
		if (orbitnum<1000||orbitnum>99999) exit(1); /*just to make sure*/
		sprintf(orbit,"%d",orbitnum);
		if (argc==5) sprintf(cmd_logoption,"> mg_%d.log",orbitnum);
        sprintf(cmd,"maskgen %s/%s %s %s/masks/%s %s\n",
        	ascdesc,orbit,DEM,ascdesc,orbit,cmd_logoption);
        if (execute(cmd)!=0) fprintf(fpo,"%s",cmd);
    }
    fclose(fpi);
    fclose(fpo);
    return(0);
}
