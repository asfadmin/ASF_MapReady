/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#define MAXCOPY 50

#define PATHLEN 80
char    ROOT_PATH[PATHLEN] = ".";
char    PROC_PATH[PATHLEN] = "proc";
char    JOBS_PATH[PATHLEN] = "jobs";
char    DATA_PATH[PATHLEN] = "/data0/asp";
char    SCRIPT_PATH[PATHLEN];
char    FULL_IMG_PATH[PATHLEN];
int	vbose = 0;

#define PASS 0
#define FAIL -1

int xcnt; ccnt, ocnt;

main( argc, argv )
  int argc;
  char **argv;
{
	char dname[80];
	char *asp_path = NULL;
	int i;

        for ( i = 1; i < argc; i++ ){
                if ( !strcmp(argv[i],"-path") )
                        asp_path = argv[i+1];
        }

	if ( asp_path == NULL ){
		printf("Usage: %s -path pathnames\n", argv[0]);
		exit(FAIL);
	}
	if ( op_get_path(asp_path) == FAIL ){
		printf("Missing pathnames files.\n");
		exit(FAIL);
	}
	count_fullres(DATA_PATH);
	sprintf(dname,"%sCPX",FULL_IMG_PATH);
	printf("%d complex available; %d in use\n", 
		count_files(dname), xcnt);
	sprintf(dname,"%sCSD",FULL_IMG_PATH);
	printf("%d CCSD available; %d in use\n", 
		count_files(dname), ccnt);
	sprintf(dname,"%sRPR",FULL_IMG_PATH);
	printf("%d full-res available; %d in use\n", 
		count_files(dname), ocnt);
}
int count_files(dname)
	char *dname;
{
	DIR *dirp;
	struct dirent *dp;
	char fname[4];
	int i, len, fcount=0;

	fcount = 0;
	dirp = opendir(dname);
	for (dp = readdir(dirp);dp != NULL; dp = readdir(dirp)){
		for ( i = 1; i <= MAXCOPY; i++ ){
			sprintf(fname,"%d",i);
			if ( (strlen(fname) == dp->d_namlen) && 
				(!strcmp(fname, dp->d_name)) ) {
				fcount++;
			}
		}
	}	
	closedir(dirp);
	return(fcount);
}
int count_fullres(dname)
	char *dname;
{
	DIR *dirp, *jobdirp;
	struct dirent *dp, *jp;
	char fname[132];

	xcnt = ccnt = ocnt = 0;
	dirp = opendir(dname);
	for (dp = readdir(dirp);dp != NULL; dp = readdir(dirp)){
	   if ( dp->d_name[0] == '.' ) continue;
	   sprintf(fname,"%s/%s",dname,dp->d_name);
	   printf("%s: ",dp->d_name);
	   if ( (jobdirp = opendir(fname)) != NULL ){
		for ( jp = readdir(jobdirp); jp != NULL;
			jp = readdir(jobdirp)){
		   if ( jp->d_name[0] == '.' ) continue;
		   sprintf(fname,"%s/%s/%s/full_image",
				dname,dp->d_name,jp->d_name);
		   if ( access(fname,F_OK) == 0  ){
		   	printf("%s ",jp->d_name);
			if ( dp->d_name[8] == 'x' ) xcnt++;
			else if ( dp->d_name[8] == 'c' ) ccnt++;
			else ocnt++;
		   }
		}
		closedir(jobdirp);
	   }
	   printf("\n");
	}	
	closedir(dirp);
	return;
}
