/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* preproc.c - This module performs the ASP pre-processing
   for all type of processing requests.
*/


#include <stdio.h>
#include <syslog.h>
#include <string.h>
#include <aspdecl.h>  /* ASP processor data & mapping constants */
#include <procfil.h>  /* Preprocessing file and data definitions */
#include <procdec.h>  /* Processing software declaration file */


/* ASP configuration parameters */
extern int vbose;               /* 1 = print progress messages */
extern int sw_dcrsi_in;         /* 1 = use input dcrsi */
extern int sw_clutter_lock;
extern int cp_flag;
extern int do_scan;
extern RQST_PTR Cur_Rqst;       /* current request */

/* parameter files */
extern DPP_FILE dpf;            /* default processing parameters file */
extern SP_FILE spf;             /* sensor parameters file */
extern int migra_flag;
/*SONY*/
extern char media_type[];

int pre_done;          /* 1 = do not repeat preprocessing */
int do_binary;         /* 1 = rebuild binary files when pre_done = 1 */
int use_prev_scan = 0;
int no_repeat_proc;

/* preproc (rqst) ------------------------------------------------------
	This routine performs preprocessing on the given job request.
*/

preproc(rqst)
	RQST_PTR    rqst;
{
	int ans;
        int tape_type;

	Cur_Rqst = rqst;
	do_binary = pre_done = use_prev_scan = 0;
	no_repeat_proc = 0;
	if (!cp_flag)  /* Pre-proc has been done */
	{
/*SONY*/
          printf("Type 0, 1 ");
          printf("for DCRSi or SONY \n");
          scanf("%d",&tape_type);
          if (tape_type == 0) {
            printf("DCRSi tape drive will be actived\n");
            strcpy (media_type, "DCRSI");
          }
          else {
                printf("SONY tape drive will be actived\n");
                strcpy (media_type, "ID-1");
               }

    	  /* Check whether pre-processing has already been done */
	  printf("\n-- Checking for previous pre-processing --\n");
	  printf("   on request ID %s\n",Cur_Rqst->id);
	  ans = pp_ver_preproc();
	  if ( ans == PASS ) /* found current preprocdata */
	  {
	    printf("\n\nProcessing done or failed in previous run!.\n");
	    ans = op_answer("Repeat processing from beginning?");
	    if (ans == FAIL) { /* Operator wishes to exit */
		pre_done = 1;
		if ( vbose ) {
		    ans = op_answer("Do you want to rebuild binary files?");
		    if ( ans == PASS ) do_binary = 1;
		}
		no_repeat_proc = 1;

/*		Comment out by CV 3/8/96: fix miscalculate fmt_ratio & 
		   reading the correct sensor_params.* file  
		if ( proc_init() == FAIL ){
		   printf("Unable to initialize processing\n");
		   return(FAIL);
		} else   return (PASS);
*/
	    }
	  } else if ( ans == FAIL ){ /* found archived preprodata */
	    if (Cur_Rqst->id[9] == 'P'){
	    	pre_done = 1;
	    	do_binary = 1;
	    	sw_clutter_lock = 0;
	    }
	    use_prev_scan = 1;
	  } else {		     /* found no preprocdata */
	    if (Cur_Rqst->id[9] == 'P') return(FAIL);
	    printf("...preprocessing not previously done\n");
	  }
	}

    /* check for input recorder available */
	if (sw_dcrsi_in == 0) {
	    printf("Cannot preprocess data: config.asp says no input DCRSI\n");
	    return (FAIL);
	}

    /* initialize preprocessing */
	if (pp_init() == FAIL)
	{
	    asp_msg(LOG_DEBUG,"Unable to initialize pre-processing\n");
	    return(FAIL);
	}
	if (migra_flag) {
	    if (get_oldscanresult() == FAIL){
	    	asp_msg(LOG_ERR,"Error in getting the old scan_result file\n");
		return(FAIL);
	    }
	}

    /* get the one-minute statevectors */
	chdir(JOB_PATH);
	asp_msg(LOG_DEBUG,"Making statevectors file");
	if (p_rw_sv_file(0,"statevectors") == FAIL)
	    make_statevectors();

   /* preform scanning */

	chdir(JOB_PATH);
	if ( scan_take() == FAIL ) return(FAIL);
	if ( no_repeat_proc ) return (PASS);

        p_rw_pp_file(1,"pp_regions1",0);
        p_rw_pream_file(1,"preambles");
        p_rw_postam_file(1,"postambles");

	if ( cp_flag && do_scan ) return(PASS);

    /* perform (satellite-specific) preprocessing */

	spf.pbw = dpf.bw_az4;
	if (vbose)
            printf("PBW to be used for main processing = %g\n", spf.pbw);

	return (build_proc_files());
}
