/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* asp.c -- top level of the ASP operational software
			February 1989

	This program contains the top level menus and startup
	procedures by which the Alaska SAR Processor is operated.
*/

#include <sys/types.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <syslog.h>

#include <procfil.h>
#include <scene_file.h>
#include <ndir.h>
#include <aspdecl.h>
#include <procdec.h>
#include <procpar.h>
#include <asp_msg.h>

extern int toupper_sw;		/* 1 = parser converts lowercase to
					uppercase letters */

char ROOT_PATH[PATHLEN];
char PROC_PATH[PATHLEN];
char JOBS_PATH[PATHLEN];
char DEF_PATH[PATHLEN];
char JOB_PATH[PATHLEN];
char DATA_PATH[PATHLEN];
char SCRIPT_PATH[PATHLEN];
char FULL_IMG_PATH[PATHLEN];

#define TOK_SEP "\t "          /* Tab and space are token seperators */

int asp_hardware_status = PASS; /* PASS = asp hardware is OK */
static char *system_hdr = "ALASKA SAR PROCESSOR OPERATING SYSTEM";

char ASP_VERSION[8];

/* job request table */
#define job_max 100
RQST_PTR job_list[job_max];
int job_count = 0;

/* data from the current job request file */
int Cur_job;                    /* current job # selected */
RQST_PTR Cur_Rqst;		/* current request */
SV sv1, sv2;
TC_FILE tcf;

/* parameter files */
DPP_FILE dpf;			/* default processing parameters file */
SP_FILE spf;			/* sensor parameters file */
EM_FILE emf;			/* Earth model file */
RTF_FILE rtf;			/* range transfer function file */

/* data blocks built during preprocessing */
TAPE_SEG_PTR seg_list = NULL;	    /* tape segment list */
TAPE_SEG_PTR save_seg_list = NULL;  /* copy of tape segment list */
PREAM_FILE_PTR pream_list = NULL;   /* preamble block list */
int pream_max = 0;		    /* largest numbered preamble */
POSTAM_FILE_PTR postam_list = NULL; /* postamble block list */
int postam_max = 0;		    /* largest numbered postamble */
SCENE_FILE sf;			    /* scene file */

enum { LOW_RES, FULL_RES, BOTH_IMAGE };
int Res_type = BOTH_IMAGE;
static char *Res_type_txt[] = { "low-res", "full-res", "both" };
int st_fmt = 0;
char job_name[100];

double stv[1200];		/* one-minute statevectors array */
int nstv = 0;			/* number of statevectors */
GMT stv_gmt;			/* time of the first statevector */

int ptype_sw = 1;		/* 1 = using new pre/postamble codes */
				/* 0 = using old pre/postamble codes */

/* ASP configuration parameters */
int vbose = 0;		/* 1 = print progress messages */
int sw_dcrsi_in = 1;	/* 1 = use input dcrsi */
int sw_noise_meas = 1;	/* 1 = perform noise measurement */
int sw_rep_meas = 1;	/* 1 = perform replica measure */
int sw_snr_meas = 1;	/* 1 = perform SNR measurement */
int sw_clutter_lock = 1; /* 1 = perform clutter lock */
int sw_ambig = 0;	/* 1 = perform ambiguity determ. */
int sw_auto_focus = 0;	/* 1 = perform auto-focus */
int sw_auto_scale = 1;	/* 1 = adjust fft scaling automatically */
int sw_pp_only = 0;	/* 1 = perform preproc only */

/* SCSI NEXUS information */

int nfd = 0;
int scsi_fd = 0;
u_char bus_id = 1;
u_char targ_id = 0;
u_char targ_lun = 0;  

void report_asp_status();
void asp_exit();
void asp_signals();
void abnormal_exit();

/* Commands */

char host[16] = "newasp";
char *asp_path = "pathnames";
int asphw = 1;
int aspsyslog = 1;
int aspcleanup = 1;
int chkrds = 0;
int cp_flag = 0;
int migra_flag = 0;
int do_scan = 0;
int ucs_product = 0;	    /* add flag for uncompensated product */
char chk_media_id[10] = "NO";
int  Write_CP_Error_Status = FAIL;
float gaintbl[900];
char scan_result_filename[50] = " ";
char cal_params_filename[50] = " ";
char scan_result_version[20] = " ";
char cal_status[50] = " ";
char cal_comment[300] = " ";
double noise_fct;
double linear_conv_fct;
double offset_conv_fct;
double islr_rng, pslr_rng;
double range_res, azimuth_res;
double range_ambig, azimuth_ambig;
double ori_error, dist_skew;
double crosst_scale, alongt_scale;
double crosst_locerr, alongt_locerr;
double rel_radio_acc;
int stop_fd, stop_len; /* added to stopit */
char stop_cmd[132];
char asp_logfile[50];
char frame_mode[20] = " ";      /* amm */

/* ASP-CP 2.1, 11/21/96 */
char media_type[10] = "DCRSI"; /* "DCRSI"/"ID-1" */
char data_direction[10] = "FORWARD"; /* "FORWARD"/"REVERSE" */
int  experimental_beam = 0; /* not used by ASP */
char compensation_flag[5] = "YES"; /* YES/NO (ignored for CCSD) */
char quicklook_flag[5] = "YES"; /*YES/NO(YES only for RAMP & STANDARD)*/
int use_rds_scan = 0;
int rds_center_format;
int max_agc;

#ifdef CP_SW
void AspDisableKill() { AsfDisableKill(); }
void AspEnableKill()  { AsfEnableKill(); }
int  AspReadScanResults ()
		{ return(ReadScanResults(&seg_list,&pream_list)); }
int  AspReadCalFile ()
		{ return(ReadCalFile(gaintbl)); }
#else
void AspDisableKill() {}
void AspEnableKill()  {}
int  AspReadScanResults() { return(PASS); }
int  AspReadCalFile() {}
int  GetODLFrameTable() {}
float GetPreCal1Pow() { return(0.); }
float GetPreCal2Pow() { return(0.); }
float GetPostCal1Pow() { return(0.); }
float GetPostCal2Pow() { return(0.); }
#endif

/* ---------------------------------------------------------------- */

/* main (argc,argv) ----------------------------------------------------
	This is the top level routine and main menu of the asp program.
*/

main (argc,argv)
	int argc;
	char *argv[];
{
	int asphelp = 0;

	char *getenv();
	char string[132], *syslog_id;
	int i;

	setlinebuf(stdout);
	setlinebuf(stderr);

	for ( i = 1; i < argc; i++ ){
		if ( !strcmp(argv[i],"-asphw") ) asphw = 0;
		else if ( !strcmp(argv[i],"-asphelp") ) asphelp = 1;
		else if ( !strcmp(argv[i],"-aspsyslog") ) aspsyslog = 0;
		else if ( !strcmp(argv[i],"-aspcleanup") ) aspcleanup = 0;
                else if ( !strcmp(argv[i],"-chkrds") ) chkrds = 1;
		else if ( !strcmp(argv[i],"-asppath") )
			asp_path = argv[i+1];
                else if ( !strcmp(argv[i],"-asplog") )
                        strcpy(asp_logfile,argv[i+1]);
	}
	if ( asphelp ){
		printf("%s [ -asphelp ", argv[0]);
		printf(" -asppath pathfile ]");
		printf("\n");
		exit(0);
	}
	if ( op_get_path(asp_path) == FAIL )
		 puts(asp_messages[PATHNAMES_FILE_ERR]);
	strcpy(asp_error_msg,asp_messages[SEE_LOGFILE]);
	memset(ASP_VERSION," ",sizeof(ASP_VERSION));
	if ( aspsyslog ){
	   strcpy(string, "ASP");
	   syslog_id = (char *) malloc(strlen(string)+1);
	   strcpy(syslog_id,string);
	   openlog(syslog_id,LOG_PID|LOG_CONS|LOG_NDELAY|LOG_ERR,LOG_LOCAL4);
	}
#ifdef MIGRA_SW
	migra_flag = 1;
#endif

#ifdef CP_SW
	cp_flag = 1;
	report_asp_status(ASP_CONNECT_CP);
	if ( aspsyslog ) closelog();
	if ( activate_cp(argc,argv) == FAIL ) asp_exit(ACTIVATE_CP_ERR);
#else
	asp_init();
        ops_mode();
	if ( aspsyslog ) closelog();
#endif

	if (migra_flag) printf("migra flag is on\n");	
}
#ifdef CP_SW
/* process_cp_job() ----------------------------------------------------------
*/

int process_cp_job()
{
	int status = PASS;
	char cmd[132];
	int i;

	if ( process_job() == FAIL ) {
		status = FAIL;
		chdir(ROOT_PATH);
        	if ( scsi_fd > 0 ) {
                  close(scsi_fd);
                  scsi_fd = 0;
                }
/*SONY*/
             if (!strcmp( media_type, "DCRSI" ) ) {
/*stopit*/
          	for (i=0; i<3; i++){
            	   if ((stop_fd = open( "/dev/tty01", O_WRONLY ))==-1){
              		printf("stopit: Cannot open /dev/tty01\n");
              		break;
            	   }
            	   strcpy(stop_cmd,"SL;MD;UL;");
            	   stop_len = strlen(stop_cmd);
            	   write(stop_fd,stop_cmd,stop_len);
            	   close( stop_fd );
		   sleep(3);
          	} /*stopit*/
              }

		report_asp_status(ASP_IDLE);
		return (status);
	}
	else {
   	   report_asp_status(ASP_DELIVER);
   	   if ( do_scan ) {
   		SaveFrameResults(spf.side, Cur_Rqst, seg_list);
		status = WriteScanToCP(seg_list);
   		if ( status == FAIL )
   			asp_msg(LOG_ERR,asp_messages[WRITE_SCAN_FILE_ERR]);
   	   } else {
		chdir(JOB_PATH);
		chdir(seg_list->ppb_list->img_name);
   		if ( RcpImageFile() == FAIL ) status = FAIL;
   		if ( RcpLdrFile() == FAIL ) status = FAIL;
   		if ( WritePMFToCP(seg_list) == FAIL ) status = FAIL;
   	   }
	}
	chdir(ROOT_PATH);
       	if ( scsi_fd > 0 ) {
           close(scsi_fd);
           scsi_fd = 0;
        }

/*SONY*/
      if (!strcmp( media_type, "DCRSI" ) ) {
/*stopit*/
        for (i=0; i<3; i++){
          if (( stop_fd = open( "/dev/tty01", O_WRONLY ) ) == -1){
              printf("stopit2: Cannot open /dev/tty01\n");
              break;
          }
          strcpy(stop_cmd,"SL;MD;UL;");
          stop_len = strlen(stop_cmd);
          write(stop_fd,stop_cmd,stop_len);
          close( stop_fd );
	  sleep(3);
	  
        } /*stopit*/
      }
                                     
	if ( aspcleanup && (status == PASS) ) {
		cleanup_data(Cur_Rqst->jobname);
		sprintf(cmd,"/bin/rm -f %s%s*",JOBS_PATH,Cur_Rqst->jobname);
		system(cmd);
	}
	report_asp_status(ASP_IDLE);
	return(status);
}
#endif
/* ops_mode() ----------------------------------------------------------
	This routine displays the operations mode menu, and interprets
	and performs the user's choices from that menu.
*/

ops_mode()
{
	get_job_list();
	do { 
		Cur_job = select_job();
		if (Cur_job == FAIL) return;
		Cur_Rqst = job_list[Cur_job++];
		
		asp_msg(LOG_DEBUG,"Processing Job %d",Cur_job);
		process_job();
        	if ( Cur_job > 0 && Cur_Rqst->id[9] != NULL )
               		if ( Cur_Rqst->id[9] == 'P' )
                                strcpy(Cur_Rqst->type,"PBK");
		write_job_status();
		chdir(ROOT_PATH);
                if ( scsi_fd > 0 ) close(scsi_fd);
		report_asp_status(ASP_IDLE);
	} while ( Cur_job > 0 );
	return(PASS);
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
	chdir(JOBS_PATH);
	if ((dfp = opendir(".")) == NULL) {
	    asp_msg(LOG_DEBUG,"Cannot find jobs directory");
	    return (FAIL);
	}
	while ((dp = readdir(dfp)) != NULL) {
	    if (dp->d_name[0] == '.')
		continue;
	    if (strcmp(dp->d_name,"status") == 0)
		continue;
	    if (find_job(dp->d_name) == FAIL) {
		if(rc_get_rqst(dp->d_name,&req,0) == PASS) {
		    if (job_count >= job_max) {
			asp_msg(LOG_DEBUG,"too many jobs");
			ans = FAIL;
			break;
		    }
		    req.status = 0;
		    rp = (RQST_PTR) malloc(sizeof(RQST_RCD));
		    if (rp == NULL) {
			asp_msg(LOG_DEBUG,"Out of memory while reading jobs");
			ans = FAIL;
			break;
		    }
		    *rp = req;
		    job_list[job_count++] = rp;
		}  /* if rc_get_rqst */
	    }  /* if find_job */
	}  /* while */
	closedir(dfp);

    /* read job status file */
	set_separators(" ");
	toupper_sw = 0;
	if (open_file("status","")) {
	    i = 0;
	    while (next_token(t) != 0) {
		next_token(s);
		if ((j = find_job(t)) != FAIL) {
		    job_list[j]->status = atoi(s);
		    move_job(j,i);
		    i++;
		}  /* if j */
	    }  /* while */
	    close_file();
	}  /* if ans */
	return (ans);
}


/* write_job_status() --------------------------------------------------
	This routine writes the job status file.
*/

write_job_status()
{
	FILE *fp;
	RQST_PTR rp;
	int i;
	char filename[80];
	char *strcpy();

	strcat (strcpy(filename,JOBS_PATH),"status");
	if ((fp = fopen(filename,"w")) == NULL) {
	    asp_msg(LOG_DEBUG,"cannot open output status file");
	    return;
	}
	for (i = 0; i < job_count; i++) {
	    rp = job_list[i];
	    if (rp->status == 0)
		rp->status = 1;
	    fprintf(fp,"%s %d\n",rp->jobname,rp->status);
	}
	fclose(fp);
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

/* move_job(from, to) --------------------------------------------------
	This routine moves a job pointer from the position "from" to
	the position "to" in the job list.
*/

move_job (from, to)
	int from, to;
{
	RQST_PTR rp;
	int i, inc;

    /* check for unworkable initial conditions */
	if (from < 0 || from >= job_count)
	    return;
	if (to < 0 || to >= job_count)
	    return;
	if (from == to)
	    return;
    /* determine direction of increment */
	inc = (from < to) ? 1 : -1;
    /* save pointer to item being moved */
	rp = job_list[from];
    /* shift intervening pointers by 1 */
	for (i = from; i != to; i += inc)
	    job_list[i] = job_list[i+inc];
    /* store item being moved in its new location */
	job_list[to] = rp;
}

/* process_job() -------------------------------------------------------
	This routine lets the user select a job, and then performs all
	processing functions on the job:
		pre-processing
		processing
		delivering job to the Post Processor
		deleting the job request
*/

int process_job()
{
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	RQST_PTR rp;
	int i;
	static char string[132];
	int status;
	
	chdir(JOBS_PATH);
	rp = Cur_Rqst;
	if ( rc_get_rqst(rp->jobname,rp,1) != PASS ) return(FAIL);

	if ( !strcmp(rp->type,"CPX") || !strcmp(rp->type,"CSD") )
		Res_type = FULL_RES;
	else if ( !strcmp(rp->type,"STD") || !strcmp(rp->type,"QLK") )
		Res_type = LOW_RES;
	else {
#ifdef CP_SW
		Res_type = GetResType();
#else
		printf("Type 0, 1, 2 ");
		printf("for low-res, full-res, or both respectively\n");
		scanf("%d",&Res_type);
		if ( Res_type < LOW_RES || Res_type > BOTH_IMAGE ) 
			Res_type = BOTH_IMAGE;
#endif
	}
	if ( vbose ){
		printf("ASP shall produce %s image(s)\n",
			Res_type_txt[Res_type]);
	}

	if ( !strcmp(rp->type,"PBK") ) strcpy(rp->type,"STD");
	sprintf(JOB_PATH,"%s%s",DATA_PATH,rp->jobname);
	op_mkdir( JOB_PATH );
	if ( vbose ) printf("%s is created.\n", JOB_PATH );

        strcat(strcpy(string,ROOT_PATH),"config.asp");
        p_get_cfg_file(string);
	make_def_path(rp->type,rp->take_id,DEF_PATH);

        if ( asphw ){
		AspDisableKill();
		if ( Res_type != LOW_RES ) connect_scsi();
		AspEnableKill();
		if ( vbose ) printf("Verbose mode ON\n");
	} /* asphw if */

/* Set tape block size */

        if( strcmp( media_type, "DCRSI" ) ) TAPE_BLOCK_LEN = ID1_BLOCK_LEN;
        else TAPE_BLOCK_LEN = DCRSi_BLOCK_LEN;

/* Preprocessing */

	status = preproc(rp);

        if ( status != PASS ){
           if(rp->status < 1) rp->status = 1;
           return(FAIL);
        }

	if ( cp_flag && do_scan ) return(PASS);

/* Processing */

	AspDisableKill();
	status = proc(rp);
	AspEnableKill();
	if ( status != PASS ) return(FAIL);

	if ( sw_pp_only ){
		if ( rp->status < 2 ) rp->status = 2;
	} else {
		if ( rp->status < 3 ) rp->status = 3;
	}	
        return(PASS);
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
	static char hd[] = "##  JOB ID NUMBER    TYPE  TAPEID   TIME      STATUS";
	static char tab[] = "    ";
	static char *stattab[] = { "New",
				   "Unprocessed",
				   "Preproc. Done",
				   "ProcDone, NotDelivered",
				   "ProcDone, NotDeliveredAgain",
				   " ",
				   " ",
				   " ",
				   "Delivered",
				   "Deleted",
				   };

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
		printf("%s%2d. %15.15s  %s   %s  ",tab,
		    j,rp->id,rp->type,rp->tape_id);
		printf("%.4d:%.3d:%.2d  ",rp->start.yr,rp->start.day,
		    rp->start.hr);
		printf(stattab[rp->status]);
		j++;
	    }
	    printf("\n");
	}
}
void report_asp_status( int code )
{
	asp_status = code;
	asp_msg( LOG_INFO, "ASP state: %s",asp_state_msg[asp_status] );
	
}
/* asp_exit( code ) ----------------------------------------------------
*/
void asp_exit( int code )
{
        if ( scsi_fd > 0 ) close(scsi_fd);
	if ( cleanup_shm() == FAIL )
		asp_msg(LOG_ERR,asp_messages[UNABLE_CLENAUP_SHM]);
	asp_msg(LOG_ERR,asp_messages[code]);
        exit();
}
/* abnormal_exit( code ) ----------------------------------------------------
      This routine clears all jobs before exit
*/
void abnormal_exit( code )
int code;
{
    char *sigmsg[] = { 	"", "hangup", "interrupt", "quit", 
			"illegal instruction",
                       	"trace trap", "abort process", "EMT instruction",
                      	"floating point exception", "kill",
                       	"bus error", "segmentation violation",
                       	"bad argument to system call",
                       	"write on a pipe with no one to read it",
                       	"alarm clock timeout",
                       	"software termination signal",
                       	"urgent contition on I/O channel",
                       	"stop", "interactive stop", "continue",
                       	"sent to parent on child stop or exit",
                       	"background read attempted from control terminal",
                       	"background write attempted to control terminal",
                      	"I/O possible", "cpu time limit exceeded",
                       	"file size limit exceeded",
                       	"virtual time alarm", "profiling time alarm",
                       	"window size changed", "information request",
                       	"ASP_SERVER quited",
			"user defined signal 2",
                       	NULL };
	char msg[132];

        if ( scsi_fd > 0 ) close(scsi_fd);
	if ( cleanup_shm() == FAIL )
		asp_msg(LOG_ERR,asp_messages[UNABLE_CLENAUP_SHM]);
	asp_msg(LOG_DEBUG,"ASP encountered termination signal.");
	asp_msg(LOG_ERR,sigmsg[code]);
	asp_msg(LOG_DEBUG,"ASP was in the state of %s",asp_state_msg[asp_status]);
        exit();
}
/* connect_scsi() -------------------------------------------------------
        This routine connect to scsi on outboard
*/
int connect_scsi()
{
	int scsi_ofd;

	report_asp_status(ASP_CONNECT_SCSI);
	if (( scsi_fd = open( "/dev/tty00", O_RDONLY ) ) == -1 ){
		asp_exit(SCSI_ERR);
	}
	sleep(10);
	init_cam(); 		/* Initialize CAM */
	setup_out_init();	/* Initialize SCSI registers 6/8/94 */
}
void asp_signals()
{
/* intercept various termination signals */

	if ( !cp_flag ){
	        signal( SIGHUP, abnormal_exit );
	        signal( SIGINT, abnormal_exit );
	        signal( SIGTERM, abnormal_exit );
	        signal( SIGQUIT, abnormal_exit );
		signal( SIGSTOP, abnormal_exit );
		signal( SIGKILL, abnormal_exit );
	}
	signal( SIGPIPE, abnormal_exit );
	signal( SIGUSR1, abnormal_exit );

	signal( SIGSEGV, abnormal_exit );
	signal( SIGFPE, abnormal_exit );
	signal( SIGBUS, abnormal_exit );
}
int asp_init()
{
	asp_signals();
	strcpy( host, getenv("HOST"));

	if ( op_get_path(asp_path) == FAIL ){
		asp_msg(LOG_ERR,asp_messages[PATHNAMES_FILE_ERR]);
		return(FAIL);
	}
	chdir(ROOT_PATH);
        if (p_get_version(asp_logfile,ASP_VERSION) == FAIL) {
        	asp_msg(LOG_ERR,asp_messages[LOGFILE_ERR]);
		return(FAIL);
        } else asp_msg(LOG_INFO,"ASP Version %s",ASP_VERSION);

/* Get configuration file */

	chdir(ROOT_PATH);
        p_get_cfg_file("config.asp");

/* Initialization of correlator hardware */

	report_asp_status(ASP_INIT);
	if ( asphw ){
		AspDisableKill();
		if ( cleanup_shm() == FAIL ){
		   asp_msg(LOG_ERR,asp_messages[UNABLE_CLENAUP_SHM]);
		   return(FAIL);
		}
		initmbx(0x80000000);
		pup();
		AspEnableKill();
	}
	tape_rdflds();

} /* asp_init */
