/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <unistd.h>
#include <dirent.h>

#define FAIL -1
#define PASS 0
#define MAXCOPY 50

extern char vbose;
extern char ROOT_PATH[];
extern char PROC_PATH[];
extern char JOBS_PATH[];
extern char DEF_PATH[];
extern char JOB_PATH[];
extern char DATA_PATH[];
extern char SCRIPT_PATH[];
extern char FULL_IMG_PATH[];
extern int toupper_sw;

static struct dlist {
        char name[15];
} dirlist[MAXCOPY];
static char destdir[80];
static int dir_count;

/* get_jobs() ------------------------------------------------------
*/

get_jobs()
{
	struct dirent *dp;
	DIR *dfp;
	int count;

	if ( chdir(destdir) == FAIL ) return(FAIL);
	if ((dfp = opendir(".")) == NULL) {
	    printf("Cannot find jobs directory\n");
	    return (-1);
	}
	count = 0;
	while ((dp = readdir(dfp)) != NULL) {
	    if (dp->d_name[0] != 'E' && dp->d_name[0] != 'J' &&
		dp->d_name[0] != 'T' && dp->d_name[0] != 'I')
		continue;
	    strcpy(dirlist[count++].name,dp->d_name);
	}  /* while */
	closedir(dfp);
	return (count);
}
void resume_fulfile( jobname )
	char *jobname;
{
	int img_count;
	int icount, count;
	char iname[40], fname[132], mvfname[132];
	char mvdir[132];
	int i, j, k;

	sprintf( destdir, "%s%s", DATA_PATH, jobname );
	if (vbose) printf("Working on job %s\n",destdir);
	dir_count = img_count = get_jobs();
	if (vbose) printf("There are %d I directories\n",dir_count);
	if ( dir_count <= 0 ) return;
	if ( jobname[8] == 'x' )
		sprintf(mvdir,"%sCPX",FULL_IMG_PATH);
	else if ( jobname[8] == 'c' )
		sprintf(mvdir,"%sCSD",FULL_IMG_PATH);
	else
		sprintf(mvdir,"%sRPR",FULL_IMG_PATH);
	for ( k = 0; k < dir_count; k++ ){ 
		strcpy(iname,dirlist[k].name);
		sprintf(fname,"%s/full_image",iname);
		if (vbose) printf("Working on %s\n",iname);
		if ( access(fname,F_OK) >= 0 ) 
			for ( i = 1; i <= MAXCOPY; i++ ){
			      sprintf(mvfname,"%s/%d",mvdir,i);
			      if ( access(mvfname,F_OK) < 0 ){ 
				printf("resume %s to %s\n", fname, mvfname);
				rename(fname,mvfname);
				break;
			      }
			}
	}
}
cleanup_data(jobname)
	char *jobname;
{
	char s[80];

	resume_fulfile( jobname );
	sprintf(s,"rm -r %s%s\n",DATA_PATH, jobname);
	system(s);
	return;
}
save_data(jobname,savedir)
	char *jobname, *savedir;
{
	char jobdir[132], s[132];

	resume_fulfile( jobname );
	sprintf(jobdir,"%s%s",DATA_PATH, jobname);
	sprintf(s,"mv %s%s* %s\n",JOBS_PATH, jobname, jobdir);
	system(s);
	sprintf(s,"mv %s %s\n",jobdir, savedir);
	system(s);
	return;
}
/* op_get_path (filename) -------------------------------------------
	This routine reads the ASP pathname file, and stores the
	data in global path  variables.
*/

op_get_path (filename)
	char *filename;
{
	char t[100];
	static char *ptab[] = { "ROOT_PATH", 
				"PROC_PATH",
				"JOBS_PATH",
				"DATA_PATH",
                                "SCRIPT_PATH",
                                "FULL_IMG_PATH",
				};
	int ptablen = sizeof (ptab) / sizeof (char *);
        int fields_in = 0;
        int i, n, ibit;
	int ans = PASS;

    /* open the file */
	if (open_file(filename,"") == 0)
	    return (PASS);
    /* set up parser parameters */
	toupper_sw = 0;
	set_separators("=");

    /* read the file & store the parameters */
	for (n = next_token(t); n > 0 && strcmp(t,"END.") != 0;
		n = next_token(t)) {
	/* decode the parameter ID */
	    for (i = 0; i < ptablen; i++) {
		if (strcmp(t,ptab[i]) == 0)
		    break;
	    }
	/* read and store the parameter's value */
	    switch (i) {
		case 0:		/* ROOT_PATH */
		    next_token (t);
		    sprintf(ROOT_PATH,"%s/",t);
		    break;
		case 1:		/* PROC_PATH */
		    next_token (t);
		    sprintf(PROC_PATH,"%s/",t);
		    break;
		case 2:		/* JOBS_PATH */
		    next_token (t);
		    sprintf(JOBS_PATH,"%s/",t);
		    break;
		case 3:		/* DATA_PATH */
		    next_token (t);
		    sprintf(DATA_PATH,"%s/",t);
		    break;
                case 4:         /* SCRIPT_PATH */
                    next_token (t);
                    sprintf(SCRIPT_PATH,"%s/",t);
                    break;
                case 5:         /* FULL_IMG_PATH */
                    next_token (t);
                    sprintf(FULL_IMG_PATH,"%s/",t);
                    break;
		default:		/* unrecognizeable parameter */
		    if (vbose) printf("unrecognizeable parameter: %s\n",t);
		    ans = FAIL;
	    }  /* switch */
	/* discard anything left on the input line */
	    flush_line();
	/* check for duplicate fields */
	    if (i < ptablen) {
		ibit = 1 << i;
		if (fields_in & ibit)
		    if (vbose) printf("duplicate parameter %s in %s file\n",
				ptab[i],filename);
		else
		    fields_in |= ibit;
	    }
	}  /* for */
	close_file();
	return (ans);
}
