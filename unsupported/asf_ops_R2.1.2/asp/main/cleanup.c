/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* cleanup.c  
*/

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>

int asp_hardware_status;
int vbose = 0;		/* 1 = print progress messages */
int rmjob = 0;		/* 1 = remove job completely */
int debug = 0;		

typedef struct{
        char    jobname[15];    /* derived job name */
} RQST_RCD, *RQST_PTR;

/* job request table */

#define job_max 200
RQST_PTR job_list[job_max];
int job_count = 0;
#define	PATHLEN 80

char 	cur_dir[PATHLEN];
char	ROOT_PATH[PATHLEN] = ".";
char	PROC_PATH[PATHLEN] = "proc";
char	JOBS_PATH[PATHLEN] = "jobs";
char	DATA_PATH[PATHLEN] = "/data0/asp";
char	SCRIPT_PATH[PATHLEN];
char	FULL_IMG_PATH[PATHLEN];

#define PASS 0
#define FAIL -1

/* main (argc,argv) ----------------------------------------------------
	This is the top level routine and main menu of the asp program.
*/

main (argc,argv)
	int argc;
	char *argv[];
{
	RQST_PTR rp;
	char cmd[200];
	int i,k, job_id;
	char *asp_path = NULL;
	
	printf("ASP cleanup Utility\n\n");

	for ( i = 1; i < argc; i++ ){
		if ( !strcmp(argv[i],"-path") )
			asp_path = argv[i+1];
		else if ( !strcmp(argv[i],"-remove") )
			rmjob = 1;
		else if ( !strcmp(argv[i],"-vbose") )
			vbose = 1;
		else if ( !strcmp(argv[i],"-debug") )
			debug = 1;
	}
	if ( asp_path == NULL ){
		printf("Usage: %s -path pathnames\n", argv[0]);
		exit(FAIL);
	}

    /* ---------------------------- */

	if ( op_get_path(asp_path) == FAIL ){
		printf("Missing pathnames files.\n"); 
		exit(FAIL);
	}

    /* first get the job list from jobs directory */

	if ( chdir(ROOT_PATH) == FAIL ){
		printf("Cannot work at directory %s\n",ROOT_PATH);
		exit(FAIL);
	}
	do {

	   if( get_job_list() == FAIL) {
		printf("Failed to get any jobs.\n");
		exit(FAIL);
	   }
	   chdir( ROOT_PATH );
	   if ( (job_id = select_job()) < 0 ) {
		exit();
	   }

/* cleanup full-res image on ALPHA */
/* remove job directory on ALPHA */

	   if ( !debug ) cleanup_data(job_list[job_id]->jobname);
	   chdir(ROOT_PATH);

/*    remove the job request from the jobs directory   */

	   if ( rmjob ){ 
	        sprintf(cmd,"/bin/rm -f %s%s*",JOBS_PATH,job_list[job_id]->jobname);
	        if ( vbose ) printf("%s",cmd);
	        if ( !debug ) system(cmd);
	   } else {
	     sprintf(cmd,"\nWould you like to remove job file %s",
			job_list[job_id]->jobname);
	     if ( op_answer(cmd) == PASS ){
	        sprintf(cmd,"/bin/rm -f %s%s*",JOBS_PATH,job_list[job_id]->jobname);
	        if ( vbose ) printf("%s",cmd);
	        if ( !debug ) system(cmd);
	     }
	   }
	} while ( job_id >= 0 );

	exit();
}

/* get_job_list() ------------------------------------------------------
	This routine reads all the currently active job requests,
	and stores them in the job list data structure.
*/

get_job_list()
{
	RQST_PTR rp;
	RQST_RCD req;
	struct dirent *dp;
	DIR *dfp, *opendir();
	int i, j;
	int ans = PASS;
	static char filename[80];
	char t[80],s[80],*strcpy();

    /* read jobs from the jobs directory */
	if (vbose) printf("get_job_list %d...\n",job_count);
	if ( job_count > 0 ) 
		for ( i = 0; i < job_count; i++ )	
			free(job_list[i]);
	job_count = 0;
	chdir(JOBS_PATH);
	if ((dfp = opendir(".")) == NULL) {
	    printf("Cannot find jobs directory\n");
	    return (FAIL);
	}
	while ((dp = readdir(dfp)) != NULL) {
	    if (dp->d_name[0] == '.')
		continue;
	    if (strcmp(dp->d_name,"status") == 0)
		continue;
	    if (strstr(dp->d_name,"odl") != NULL)
		continue;
	    if (find_job(dp->d_name) == FAIL) {
		    if (job_count >= job_max) {
			printf("too many jobs\n");
			ans = FAIL;
			break;
		    }
		    strcpy(req.jobname,dp->d_name);
		    rp = (RQST_PTR) malloc(sizeof(RQST_RCD));
		    if (rp == NULL) {
			printf("Out of memory while reading jobs\n");
			ans = FAIL;
			break;
		    }
		    *rp = req;
		    job_list[job_count++] = rp;
	    }  /* if find_job */
	}  /* while */
	closedir(dfp);

	return (ans);
}

/* find_job(id) --------------------------------------------------------
	This routine finds the given job id in the job table, and
	returns the index to the job.  If no job is found, the
	routine returns FAIL.
*/

find_job(id)
	char *id;
{
	int i;

	for (i = 0; i < job_count; i++)
	    if (strcmp(job_list[i]->jobname,id) == 0) return (i);
	return (FAIL);
}

/* display_job_list(opt) -----------------------------------------------
	This routine displays a list of the currently pending job
	requests.  If opt = -1, the list starts at the beginning.
	If opt = -2, the list is redisplayed at its previous location.
	Otherwise, the next page of jobs is displayed.
*/

display_job_list(opt)
	int opt;
{
	RQST_PTR rp;
	int lines, cols, hline, last_item, nlines;
	static int first_item = 1;
	int i, j, k;
	static char system_hdr[] = "ASP cleanup Utility";

	static char hd[] = "##   JOBNAME ";
	static char tab[] = "          ";

	termsize (&lines, &cols);
	nlines = lines - 6;
	switch (opt) {
	    case -1:
		first_item = 1;
		break;
	    case -2:
		break;
	    default:
		first_item += nlines;
		if (first_item > job_count)
		    first_item = 1;
		break;
	}
	last_item = first_item + nlines - 1;
	if (last_item > job_count)
	    last_item = job_count;
    /* display system header */
	for (k = 0; k < ((cols - strlen(system_hdr)) / 2); k++)
	    printf (" ");
	printf("%s\n",system_hdr);
	hline = (lines - job_count - 2) / 2 + 1;
    /* display the list */
	for (i = 2, j = first_item; i < lines; i++) {
	    if (i == hline) {
	    /* print header */
		printf("%s%s",tab,hd);
	    }
	    if ((j <= last_item) && (i > hline + 1)) {
		rp = job_list[j-1];
		printf("%s%2d. %15.15s",tab,j,rp->jobname);
		j++;
	    }
	    printf("\n");
	}
}
/* select_job () -------------------------------------------------------
	This routine displays the job list menu, and lets the user
	select a job by number.  The index of the job in the job list
	is returned.  If the user enters zero, the routine returns
	FAIL.
*/


select_job()
{
	int i;
	char t[80];
	
	i = -1;
	while (i < 1) {
	    display_job_list(i);
	    printf("Enter selection or -cr- for more (0 to exit): ");
	    gets(t);
	    i = atoi(t);
	    if (i < 1 || i > job_count) {
		if (t[0] == '0')
		    return (FAIL);
		if (t[0] == '\0') {
		    i = -3;
		}
		else {
		    printf("\07\n");
		    i = -2;
		}
	    }
	}  /* while */
	return (i - 1);
}
