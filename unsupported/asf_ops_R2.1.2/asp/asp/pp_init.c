/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* pp_init.c - This module initializes the ASP pre-processing
   for all type of processing requests.
*/

#include <procfil.h>  /* processing data file declarations */
#include <procdec.h>  /* Processing software declaration file */
#include <stdio.h>
#include <syslog.h>
#include <string.h>
#include <errno.h>
#include <asp_msg.h>


#define BLKLEN 9               /* Size of block addr string */
#define DFILES "dfiles.lst"    /* File name of default file list */
#define PPFILES "ppfiles.lst"  /* File name of preproc result file */
#define PPFILES0 "ppfiles0.lst"  /* File name of preproc0 result file */

#define BLKSIZE 4356           /* Size of DCRSi data block */

#define TOK_SEP "\t "          /* Tab and space are token seperators */

char    Save_Addr[BLKLEN];   /* Current block address of tape */
static char *ARCH_PATH = "/da1/preprocdata/";
extern char DATA_PATH[PATHLEN];

extern	int vbose;		/* switch for verbose */
extern  toupper_sw;		/* 1 = convert input to upper case */

extern RQST_PTR    Cur_Rqst;   /* Info for current request */
char    q[80];                 /* Question to operator */

extern DPP_FILE dpf;		/* default processing params file */
extern SP_FILE spf;		/* sensor parameters file */
extern EM_FILE emf;		/* earth model file */
extern RTF_FILE rtf;		/* range transfer function file */
extern TAPE_SEG_PTR seg_list;	/* tape segment list */

typedef struct path_st {	/* chain of path names */
  struct path_st *nxt_path;	/* pointer to next path */
  char path[PATHLEN];		/* path names */
  char type;			/* proc type 's' or 'q' */
} PATH_ST, *PATH_PTR;

extern int asphw;
extern int cp_flag;
extern char frame_mode[];	/* amm */
int ans;
extern char media_type[];

/* pp_ver_preproc()------------------------------
	The existence of the processing directory and
	pre-processing result files is checked.  If they
	exist, then preprocessing has already been
	performed and the routine returns PASS.
*/

int pp_ver_preproc()
{
    char   proc_dir[PATHLEN];  /* Holds process directory name */

    /* Generate process directory name */

/* strcpy(proc_dir,PROC_PATH);            /* Copy working path */
/* strcat(proc_dir,Cur_Rqst->jobname);    /* Copy asp ID */

    strcpy(proc_dir,JOB_PATH);

    /* Verify that processing directory exists */
    if (op_finddir(proc_dir) == FAIL){
	return(ABORT);

/* For R1B, ASP will not check for previous processing results
	if ( pp_findprev_pp() == FAIL ) return(ABORT);
	return(FAIL); 
*/
    }

    if (pp_ver_ppfiles(proc_dir,2) == PASS) /* check for pp_regions */
        return(PASS);
    else {
    	return(ABORT);		/* Check for pp_regions0 at scan routine */
    }

/*
    if (pp_ver_ppfiles(proc_dir,2) == PASS) 
        return(PASS);
    else {
      if (pp_ver_ppfiles(proc_dir,0) == FAIL) 
	if ( pp_findprev_pp() == FAIL ) return(ABORT);
	else return(FAIL);
      else return(ABORT);
    }
*/
}


/* pp_ver_ppfiles(p_path,mode)------------------------------
	check to see if the scan 1 preprocessing files
	exist in the directory specified by "p_path".
	"mode" specifies whether to search for pp_regions0 (=0)
	or pp_regions (=2).
*/

int pp_ver_ppfiles(p_path,mode)
char p_path[PATHLEN];
int mode;
{
    char   proc_dir[PATHLEN];  /* Holds process directory name */
    char   listfile[PATHLEN];  /* Pre-proc list file name */
    char   pprocfile[PATHLEN]; /* Generic pre-proc file name */
    int    end;                /* Used for loop control */
    char   strg_tok[FIELDLEN]; /* Holds tokens read from file */


    /* Find pre-proc list file */
    strcpy(listfile,PROC_PATH);
    if (mode == 0)
      strcat(listfile,PPFILES0);
    else
      strcat(listfile,PPFILES);
    if (open_file(listfile,"") == 0)  /* Function returns 0 as false */
    {
	printf("\nPre-processing verification ERROR.");
	printf("\nUnable to find pre-proc list file.\n");
	return(ABORT);
    }

    /* Get ready to read list file by setting up parser */
    set_separators(TOK_SEP);  /* Set token separators */
    toupper_sw = 0;           /* Conversion to upper case */

    strcpy(proc_dir,p_path); 
    strcat(proc_dir,"/");  /* To be used for file names */

    /* Read pre-processing result file names and find files */
    if (next_head(strg_tok) == 0)   /* Nothing in file */
    {
	close_file();
	printf("\nPre-processing verification ERROR.");
	printf("\nUnable to read pre-proc list file.\n");
	return(ABORT);
    }
    end = 1;      /* Initialize for loop */
    while (end != 0)              /* While not eof */
    {
        if (strchr(strg_tok,'!') != NULL)  /* Token is a comment */
        {
            flush_line();
	    end = next_token(strg_tok);
        }
        else
	{
	    strcpy(pprocfile,proc_dir);
	    strcat(pprocfile,strg_tok);  /* Save file name */
            while (((end = next_token(strg_tok)) != 1) && (end != 0))
            {
		if (strcmp(strg_tok,Cur_Rqst->type) == PASS)
		{
		    if (op_findfile(pprocfile) == FAIL)
		    {
			close_file();
			return(FAIL);  /* Pre-proc file not there */
		    }
                }
            }
        }
    }
    close_file();
    return(PASS);
}



/* pp_ver_dfiles() -----------------------------------------------------
	This routine verifies that the default files listed in the 
	file DFILES exist.  The routine returns PASS if all files 
	are present and FAIL if they are not.
*/

int pp_ver_dfiles()
{
	char  dfile[PATHLEN];     /* Generic default file name */
	char  strg_tok[FIELDLEN]; /* Holds tokens read from file */
	int   status;             /* Set to FAIL if a file is missing */

	if (vbose) printf("frame_mode=%s\n",frame_mode);
    /* Find default list file */
	if (open_file(DEF_PATH,DFILES) == 0)
	{
	    printf("\nDefault file verification ERROR.");
	    printf("\nUnable to find default list file.\n");
	    return(FAIL);
	}

    /* Get ready to read list file by setting up parser */
	set_separators(TOK_SEP);  /* Set token separators */
	toupper_sw = 0;           /* No conversion to upper case */

    /* Read pre-processing default file names and find each file */
	status = PASS;
	while (next_token(strg_tok) != 0)   /* While not eof */
	{
	    if (strg_tok[0] != '!')   /* ! signifies a comment line */
	    {
		strcpy(dfile,DEF_PATH);
		strcat(dfile,strg_tok);  /* Save file name */
		if (op_findfile(dfile) == FAIL)
		{
		    status = FAIL; /* Verification fails */
		    printf("Default file missing:  %s\n",dfile);
			
		} 
	    }
	    flush_line();   /* Flush description (or comment) */
	}
	close_file();

    /* if default files OK, read in the ones we need */
	if (status == PASS) {
	    sprintf(dfile,"%sdf_proc_params",DEF_PATH);
	    if (p_get_dpp_file(dfile,&dpf) != PASS)
		return (FAIL);
            if (!strcmp(frame_mode,"ANARCTIC"))  /* for amm */
                 sprintf(dfile,"%ssensor_params.L",DEF_PATH);
            else
	         sprintf(dfile,"%ssensor_params",DEF_PATH);
	    if (p_get_sp_file(dfile,&spf) != PASS)
		return (FAIL);
	    sprintf(dfile,"%sdf_earth_mod",DEF_PATH);
	    if (p_get_em_file(dfile,&emf) != PASS)
		return (FAIL);
	    sprintf(dfile,"%srf_ra_file",DEF_PATH);
	    if (p_get_rtf_file(dfile,&rtf) != PASS)
		return (FAIL);
            if (!strncmp(Cur_Rqst->take_id,"E",1)){
                sprintf(dfile,"%sdwp.base",DEF_PATH);
                if (p_get_dwp_file(dfile) != PASS){
                    return (FAIL);
                }
            }
	}
	return (status);
}



/* pp_list_dfiles() ----------------------------------------------------
	This routine lists the default files in DFILES.
*/

void pp_list_dfiles()
{
	char   strg_tok[FIELDLEN]; /* Holds tokens read from file */

    /* Find default list file */
	if (open_file(DEF_PATH,DFILES) == 0)
	{
	    printf("\nUnable to find default list file.\n");
	    return;
	}    
	printf("\nDefault files are located in \n     %s directory.\n",DEF_PATH);
    /* Get ready to read list file by setting up parser */
	set_separators(TOK_SEP);  /* Set token separators */
	toupper_sw = 0;           /* No conversion to upper case */

    /* Read pre-processing result file names and list files */
	while (next_token(strg_tok) != 0)    /* While not eof */
	{
	    if (strg_tok[0] != '!')  /* Comment line */
		printf("%s\n",strg_tok);  /* Print file name */
	    flush_line();   /* Flush description (or comment) */
	}
	close_file();
	return;
}



/* pp_loadDCRSi(rcdr) --------------------------------------------------
	This routine initializes the DCRSi connection and mounts the 
	tape in the recorder.  PASS or FAIL is returned.
*/

int pp_loadDCRSi(rcdr)
	int rcdr;
{
	if ( asphw ){
		report_asp_status(ASP_LOAD_DCRSI);
		AspDisableKill();
	        if ((dcu_setup() == PASS)  /* Configure recorders */
	            && (dcu_connect(rcdr) == PASS)
	            && (tape_mnt(rcdr,Cur_Rqst->tape_id) == PASS))
	                    return(PASS);
		AspEnableKill();
		asp_msg(LOG_ERR,asp_messages[DCRSI_LOAD_ERR]);
	        return(FAIL);
	} return(PASS);
}

/* pp_loadSONY(rcdr) --------------------------------------------------
	This routine initializes the SONY connection and mounts the 
	tape in the recorder.  PASS or FAIL is returned.
*/

int pp_loadSONY(rcdr)
	int rcdr;
{
	if ( asphw ){
		report_asp_status(ASP_LOAD_SONY);
		AspDisableKill();

                if (dc_get_config() == FAIL){
                 printf("cann't not config\n");
                 exit(1);
                }


                if (sony_comm_init(rcdr) == PASS)
                  printf("sony_comm_init is PASS\n");
                else
                  printf("sony_comm_init is not PASS\n");
       
/*
	        if ((sony_setup(rcdr) == PASS)  
	            && (sony_tape_mnt(rcdr,Cur_Rqst->tape_id) == PASS))
	                    return(PASS);
*/
	        if (sony_setup(rcdr) == PASS)  
	                    return(PASS);
		AspEnableKill();
		asp_msg(LOG_ERR,asp_messages[SONY_LOAD_ERR]);
	        return(FAIL);
	} return(PASS);
}
/* pp_gen_procdir() -----------------------------------------
	A directory name is derived from JOB_PATH and the 
	directory is created if it does not already exist.  The 
	processing request record and other records related to the 
	request are copies to the directory.
*/

int pp_gen_procdir()
{
    /* Verify that processing directory exists */
	if (op_finddir(JOB_PATH) == FAIL)
	    if (op_mkdir(JOB_PATH) == FAIL)
		return(FAIL);
	return(PASS);
}



/* pp_init() -----------------------------------------------------------
	This routine checks all external resources necessary to
	preprocess the current request.  These are:
		ASP processor hardware (health flag)
		default processing parameter files
		DCRSi recorder availability
		correct cassette tape loaded
	If the resources check out OK, the default parameter files are
	read in, and the routine returns PASS.  Otherwise, the routine
	returns FAIL.
*/

int pp_init()
{
	int status;

    /* Verify health of processing hardware */
    report_asp_status(ASP_VERIFY_DEF);
    if (vbose) printf("\n--Verifying Processor Diagnostic Status --\n");
    if (op_get_diagstat() == FAIL)
	{
	printf("\n\nConfidence test failure; pre-processing aborted.\n");
	printf("Run hardware diagnostics.\n");
	return(FAIL);
	}
    else if (vbose) printf("...processor healthy\n");

    /* construct the default path */
    make_def_path(Cur_Rqst->type,Cur_Rqst->take_id,DEF_PATH);

    /* Verify existence of default files */
    if (vbose) printf("\n-- Verifying  existence of default files --\n");
    if (pp_ver_dfiles() == FAIL)
	{
	printf("\nUnable to verify default files; pre-processing aborted.\n");
	pp_list_dfiles();
	return(FAIL);
	}
    else if (vbose) printf("...default files OK\n");

/* SONY tape */
    if( strcmp( media_type, "DCRSI" ) ) {
    /* Connect SONY and load tape */
      while (pp_loadSONY(IN) == FAIL)
      {
	   if (asphw) sony_comm_close(IN);
	   return(FAIL);
      }
      if (vbose) printf("...SONY tape loaded\n");
    }
    else {
    /* Connect DCRSi and load tape */
      while (pp_loadDCRSi(IN) == FAIL)
      {
	   if (asphw) dc_comm_close(IN);
	   return(FAIL);
      }
      if (vbose) printf("...DCRSI tape loaded\n");
    }

    /* Generate process directory */
    if (vbose) printf("\n-- Creating process directory --\n");
    if (pp_gen_procdir() == FAIL)
    {
	printf("\n\nUnable to create process directory.\n");
      if( strcmp( media_type, "DCRSI" ) ) {
	if (asphw) sony_comm_close(IN);
      }
      else {
        if (asphw) dc_comm_close(IN);
      } 

	return(FAIL);
    }
    if (vbose) printf("...directory created\n");

    printf("\n** Pre-processing initialization complete. **\n");

    return (PASS);

}


/* proc_init() ------------------------------------------------------
	This routine checks all external resources necessary to
	process the current request when no preprocessing is 
	performed and pp_init is not called.  These are:
		ASP processor hardware (health flag)
		default processing parameter files
		DCRSi recorder availability
		correct cassette tape loaded
	If the resources check out OK, the default parameter files are
	read in, and the routine returns PASS.  Otherwise, the routine
	returns FAIL.
*/

int proc_init()
{
	int status;

    /* Verify health of processing hardware */
    report_asp_status(ASP_VERIFY_DEF);
    if (vbose) printf("\n--Verifying Processor Diagnostic Status --\n");
    if (op_get_diagstat() == FAIL)
	{
	printf("\n\nConfidence test failure; processing aborted.\n");
	printf("Run hardware diagnostics.\n");
	return(FAIL);
	}
    else if (vbose) printf("...processor healthy\n");

    /* construct the default path */
    make_def_path(Cur_Rqst->type,Cur_Rqst->take_id,DEF_PATH);

    /* Verify existence of default files */
    if (vbose) printf("\n-- Verifying  existence of default files --\n");
    if (pp_ver_dfiles() == FAIL)
	{
	printf("\nUnable to verify default files; processing aborted.\n");
	pp_list_dfiles();
	return(FAIL);
	}
    else if (vbose) printf("...default files OK\n");

    /* Connect DCRSi and load tape */
    while (pp_loadDCRSi(IN) == FAIL)
    {
	   if (asphw) dc_comm_close(IN);
	   return(FAIL);
    }
    if (vbose) printf("...tape loaded\n");

    printf("\n** Processing initialization complete. **\n");

    return (PASS);
}
/* pp_findprev_pp()------------------------------
	Find the existence of the processing directory for 
	the same datatake and verify pre-processing result 
	files.  If they exist, then ask operator if Okay to
	use the scan 1 from the one which has been done.
	If so, copy preambles, postambles, and pp_regions0.
	Also copy statevectors only if from a standard job.

	Modification: 8/11/92,dtc - the archive path must 
	have a sub directories which must identify the 
	satellite and the coarse grouping (thousands of revs)
	since there is a limit of the number of directories
	in a directory.

	Modification: 2/19/93,mc - if "p-type" job, we only interest
	in pp_regions.  Extra cut and paste should be done before
	moving pp_regions to JOB_DIR.
*/

int pp_findprev_pp()
{
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	FILE *op;
	int statev, mc, i;
	int id_match = 0;
	char filename[80];
	char cmd[130];
	char rev_id[10],t[80];
	char dir_path[PATHLEN];
	char alt_path[PATHLEN];
	char image_id[20];
	PATH_PTR ptptr, pt_first, pt_last;

	char q[80];

	pt_first = NULL;
	pt_last = NULL;
	strncpy(rev_id,Cur_Rqst->jobname,8);
	rev_id[8] = '\0';

	/* check if the datatake has been processed elsewhere.
	   Check in PROC_PATH first for standard, then quick-look;
	   then check in ARCH_PATH for standard, then quick-look.  */

	/* the path name for the archive must include the Satellite
	   id ("E","J",or"R") plus the upper two digits of the rev
	   (tens and units of thousands) */
	statev = 1;
	mc = 0;

/*
	for ( strcpy(alt_path,PROC_PATH); mc < 2; 
*/
	strcpy(alt_path,DATA_PATH);
	for ( strcat(alt_path,"proc/"); mc < 2; 
		strcpy(alt_path,ARCH_PATH) ) {
	  mc++;
	  if (mc > 1) {
	  	strncat(alt_path,rev_id,3);
	  	strcat(alt_path,"/");
	  }
	  sprintf(cmd,"cd %s;ls -dr %ss* %sq* %sr*> temp.lst\n",
		alt_path,rev_id,rev_id,rev_id);
	  system(cmd);
	  if(open_file(alt_path,"temp.lst") == 0)
	    close_file();
	  else {
	    /* set_separators(" /"); */
	    toupper_sw = 0; /* do not convert cases */
	    while (next_token(t) != 0) {
	      if((t[9] != '*') &&
	         (t[8] == 's' || t[8] == 'r' || t[8] == 'q')) { 
						/* found something */
		strcpy(dir_path,alt_path);
		strcat(dir_path,t);
		if(strcmp(dir_path,JOB_PATH) != 0) {
		  if((ptptr = (PATH_PTR) malloc(sizeof(PATH_ST))) == NULL) {
		    printf("memory allocation failed in pp_findprev_pp\n");
		    return(FAIL);
		  }
		  if(pt_first == NULL) pt_first = ptptr;
		  ptptr->nxt_path = NULL;
		  strcpy(ptptr->path,dir_path);
		  ptptr->type = t[8];
		  if(pt_last != NULL) pt_last->nxt_path = ptptr;
		  pt_last = ptptr;
	        }
	      } /* if(t[8] == 's'... */
	    } /* while(next_token... */
	    close_file();
	  } /* if(open_file... */
	} /* for(strcpy... */
	for (ptptr=pt_first; ptptr != NULL; ptptr=ptptr->nxt_path) {
	    strcpy(dir_path,ptptr->path);
	    if (Cur_Rqst->id[9] == 'P') { /* p-type job, search for pps */
	    	if((strcmp(dir_path,JOB_PATH) != 0) &&
		    (pp_ver_ppfiles(dir_path,2) != FAIL)) {
		    /* search IMAGE_ID here */
		    id_match = 0;
	  	    sprintf(cmd,"cd %s; grep IMG_NAME %s/pp_regions > temp.lst\n",
				alt_path, dir_path);
	  	    system(cmd);
		    printf("P job, id=%s\n", Cur_Rqst->site);
	  	    if(open_file(alt_path,"temp.lst") == 0)
	    	      close_file();
	  	    else {
	    	    /* set_separators(" /"); */
	    	      toupper_sw = 0; /* do not convert cases */
	    	      while (next_token(t) != 0) {
/* ??? to be fixed    	if(strcmp(t, Cur_Rqst->site) == 0) { */
			for (i=0; i<8; i++) {
			  if (t[i+1] != Cur_Rqst->site[i]) break;
			}
			if(i == 8) {
			  id_match = 1;
			  break;
			}
		      }
		      close_file();
		    }
		    if (id_match == 1) {
		      printf("Found matched image_id\n");
	  	      chdir(dir_path);
		      p_rw_pp_file(0,"pp_regions",1);
		    /* open the pp_regions to be */
		      strcpy(filename, "pps_tobe");
	    	      if ((op = fopen(filename,"w")) == NULL) {
			printf("Cannot open output file %s\n",filename);
			return (FAIL);
	    	      }
	    	      for (sp=seg_list; sp!=NULL && id_match==1; sp=sp->nxt_seg) {
			for (pp=sp->ppb_list; pp!=NULL; pp=pp->nxt_ppb) {
		    	  if (Cur_Rqst->id[9] == 'P' &&
/* ??? to be fixed	      strcmp(Cur_Rqst->site,pp->img_name)==0) { */
			      Cur_Rqst->site[0] == pp->img_name[1] &&
			      Cur_Rqst->site[1] == pp->img_name[2] &&
			      Cur_Rqst->site[2] == pp->img_name[3] &&
			      Cur_Rqst->site[3] == pp->img_name[4] &&
			      Cur_Rqst->site[4] == pp->img_name[5] &&
			      Cur_Rqst->site[5] == pp->img_name[6] &&
			      Cur_Rqst->site[6] == pp->img_name[7]) {
			    /* assign new image ID */
			    get_image_id("STD", image_id);
			    strcpy(pp->img_name, image_id);
			    fprintf(op,"%s\n","TAPE_SEGMENT");
			    p_rw_seg_block(1,sp,filename,op);
		    	    fprintf(op,"%s\n","PP_REGION");
		    	    p_rw_pp_block(1,pp,filename,op);
			    id_match = 0;
			    break;
		    	  }
			}
		      }
		      fprintf(op,"END.\n");
		      fclose(op);
		      if(ptptr->type == 'q') statev = 0;
		      mc = 9;  /* found pp data */
		      break;
		    }
	    	}
	    }
	    else {
	    	if((strcmp(dir_path,JOB_PATH) != 0) &&
		    (pp_ver_ppfiles(dir_path,0) != FAIL)) {
		    if(ptptr->type == 'q') statev = 0;
		    mc = 9;  /* found pp data */
		    break;
	    	}
	    }
	} /* for ptptr... */
    /* free up the memory */
	for (ptptr=pt_first; ptptr != NULL; ptptr=pt_last->nxt_path) {
		pt_last = ptptr;
		free(ptptr);
	}

	if (mc != 9) {
	  if (vbose) printf("...no preprocessing done anywhere\n");
	  if (Cur_Rqst->id[9] == 'P') {
	      printf("Warning - PBK job without pre-done pp_regions\n");
	      printf("Please re-submit as RPR job.\n");
	      return(FAIL);
	  }
	  return(FAIL);;
	}

	if (Cur_Rqst->id[9] == 'P') {


	    printf( "IMAGE ID searching completed ...\n" );
	    sprintf(q,"...%s has the following preproc data:\n",dir_path);
	    printf( q );
	    sprintf(cmd,"ls %s\n",dir_path);
	    system( cmd );
	    /*
	    ans = op_answer( "Do you wish to use the pps_tobe from it?" );
	    if (ans == FAIL) return(FAIL);
	    */
	    if(pp_gen_procdir() == FAIL)
	        return(FAIL);
	    sprintf(cmd,"cp %s/preambles %s\n",dir_path,JOB_PATH);
	    system(cmd);
	    sprintf(cmd,"cp %s/postambles %s\n",dir_path,JOB_PATH);
	    system(cmd);
	    sprintf(cmd,"cp %s/pp_regions0 %s\n",dir_path,JOB_PATH);
	    system(cmd);
	    sprintf(cmd,"cp %s/pp_regions1 %s\n",dir_path,JOB_PATH);
	    system(cmd);
	    sprintf(cmd,"cp %s/pps_tobe %s/pp_regions\n",dir_path,JOB_PATH);
	    system(cmd);
	    if (statev == 1) {
	      sprintf(cmd,"cp %s/statevectors %s\n",dir_path,JOB_PATH);
	      system(cmd);
	    }
	    return(PASS);
	}
	else {

	    printf( "This rev has been previously processed.\n" );
	    sprintf( q, "...%s has the following preproc data:\n",dir_path);
	    printf( q );
	    sprintf(cmd,"ls %s\n",dir_path);
	    system( cmd );

	    if ( !cp_flag ){
		ans = op_answer( "Do you wish to use the scan results from it" );
	    	if ( ans == FAIL) return(FAIL);
	    }
	    if(pp_gen_procdir() == FAIL) return(FAIL);
	    sprintf(cmd,"cp %s/preambles %s\n",dir_path,JOB_PATH);
	    system(cmd);
	    sprintf(cmd,"cp %s/postambles %s\n",dir_path,JOB_PATH);
	    system(cmd);
	    sprintf(cmd,"cp %s/pp_regions0 %s\n",dir_path,JOB_PATH);
	    system(cmd);
	    if (statev == 1) {
	      sprintf(cmd,"cp %s/statevectors %s\n",dir_path,JOB_PATH);
	      system(cmd);
	    }
	    return(PASS);
	}
}
/* get_oldscanresult()------------------------------
	Find the existence of the processing directory for 
	the same datatake and verify pre-processing result 
	files.  If they exist, copy preambles, postambles,
	pp_regions0, and statevectors.

*/

int get_oldscanresult()
{
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
        int statev, mc, i, temp;
        char filename[80];
        char cmd[130];
        char rev_id[10],t[80], r_id[10];
        char dir_path[PATHLEN], d_path[PATHLEN];
        char alt_path[PATHLEN], job_path[PATHLEN];
        PATH_PTR ptptr,pt_first,pt_last;
        char q[80];
	int found;

	found = 0;
        pt_first = NULL;
        pt_last = NULL;
        strncpy(rev_id,Cur_Rqst->jobname,8);
        rev_id[8] = '\0';

	if (!strncmp(Cur_Rqst->take_id,"E2",2)) {
	    strcpy(rev_id,strtok(rev_id,"E"));
	    sprintf(r_id,"%s%s", "T", rev_id);
	    printf("r_id=%s\n",r_id);
	    strcpy(rev_id,r_id);
	}
	strcpy (alt_path,ARCH_PATH);
	strncat (alt_path,rev_id,3);
	strcat (alt_path,"/");
        sprintf(cmd,"cd %s;ls -dr %ss* > temp.lst\n", alt_path,rev_id);
        if (system(cmd) != 0) return (FAIL);
	if(open_file(alt_path,"temp.lst") == 0) {
            close_file();
	    return (FAIL);
        } else {
            /* set_separators(" /"); */
            toupper_sw = 0; /* do not convert cases */
            while (next_token(t) != 0) {
	      printf("Looking old scan_result file in /da1/preprocdata\n");
              if((t[9] != '*') && (t[8] =='s')) { /* found something */
                strcpy(dir_path,alt_path);
                strcat(dir_path,t);
		strncpy(d_path,t,6);
		d_path[6] = '\0';
		strncpy(job_path,Cur_Rqst->jobname,6);
		job_path[6] = '\0';
		if (vbose)printf("dir_path=%s,JOB_PATH=%s\n",dir_path,JOB_PATH);
		printf("d_path=%s, job_path=%s\n",d_path,job_path);
		if (strncmp(Cur_Rqst->take_id,"E2",2)) {
                   if(strcmp(d_path,job_path) != 0) {
                      if((ptptr = (PATH_PTR) malloc(sizeof(PATH_ST))) == NULL) {
                         printf("memory allocation failed in get_oldscan_result\n");
		         return (FAIL);
                      }
                      if(pt_first == NULL) pt_first = ptptr;
                      ptptr->nxt_path = NULL;
                      strcpy(ptptr->path,dir_path);
                      ptptr->type = t[8];
                      if(pt_last != NULL) pt_last->nxt_path = ptptr;
                      pt_last = ptptr;
                   }
		   else {
		      printf("Found it\n");
		      found = 1;
		      break;
		   }
		} /* if not E2 */
	        else {
		   found = 1;
		   break;
		}
              } /* if(t[8] == 's'... */
            } /* while(next_token... */
            close_file();
        } /* if(open_file... */
	if ( found == 0 ) {
           for (ptptr=pt_first; ptptr != NULL; ptptr=pt_last->nxt_path) {
                pt_last = ptptr;
                free(ptptr);
	   }
	   return (FAIL);
	}
        if(pp_gen_procdir() == FAIL) {
           for (ptptr=pt_first; ptptr != NULL; ptptr=pt_last->nxt_path) {
                pt_last = ptptr;
                free(ptptr);
	   }
	   return(FAIL);
	}
	printf("dir_path=%s\n", dir_path);
	sprintf(cmd,"cp %s/preambles %s\n",dir_path,JOB_PATH);
	if (system(cmd) != 0) {
           for (ptptr=pt_first; ptptr != NULL; ptptr=pt_last->nxt_path) {
                pt_last = ptptr;
                free(ptptr);
	   }
	   printf("Error in getting preambles file\n"); 
	   return (FAIL);
	}
        sprintf(cmd,"cp %s/postambles %s\n",dir_path,JOB_PATH);
        system(cmd);
        sprintf(cmd,"cp %s/pp_regions0 %s\n",dir_path,JOB_PATH);
	if (system(cmd) != 0) {
           for (ptptr=pt_first; ptptr != NULL; ptptr=pt_last->nxt_path) {
                pt_last = ptptr;
                free(ptptr);
	   }
	   printf("Error in getting pp_regions0 file\n"); 
	   return (FAIL);
	}
/* Comment out suggested by Dave Cuddy 
        sprintf(cmd,"cp %s/statevectors %s\n",dir_path,JOB_PATH);
        system(cmd);
*/
    /* free up the memory */
        for (ptptr=pt_first; ptptr != NULL; ptptr=pt_last->nxt_path) {
                pt_last = ptptr;
                free(ptptr);
        }
                                      
        return(PASS);
}
