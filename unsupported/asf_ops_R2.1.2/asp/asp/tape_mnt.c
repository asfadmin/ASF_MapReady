/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* tape_mnt.c - This module contains tape_mnt and tape_umnt
    routines for finding the current position of a tape
    and returning the tape to that position.
    When a tape is loaded into the DCRSi,
    tape_mnt should be called before any commands which
    change tape position are used.  tape_umnt should be
    called before a tape is removed from the DCRSi.
    Routines are also included to maintain the DCRSi
    status file which keeps information about mounted
    tapes.
    4/22/91,dtc, mod -- AR1653.  change the call to tape_wrflds.
*/

#include <stdio.h>
#include <string.h>
#include <procfil.h>
#include <procdec.h>
#include <sony.h>

#define MOUNTED 1
#define UNMOUNTED 0
#define STAT_FILE "DCRSi.status"  /* Mnt stat structure stored */

#define BLK_FT 6984               /* Blocks of data per ft, approx */
#define SRCH_TIMES 5              /* Number of tries to find */
				  /*  tape position */
#define FT_INCR 2                 /* Number of feet to rewind */
				  /*  for each search try */
#define BLK_INCR 1000             /* Number of blocks to increment */
				  /*  tape position forward when */
				  /*  searching */
#define BLANK_MAX 30              /* Max number of feet of invalid */
				  /*  data at end of take  */
#define BLANK_MIN 4               /* Increment for searching for */
				  /*  end of invalid data, minimum */
				  /*  of two ft due to user log */
				  /*  tape interval */
#define BLKSIZE 2178              /* Size of blk in 16 bit words */


extern RQST_PTR Cur_Rqst;	  /* pointer to current job request */
extern int cp_flag;
extern int    toupper_sw;
extern int    alarm;		  /* 1 = timer has gone off */
extern char chk_media_id[];
extern int vbose;
extern int Write_CP_Error_Status;

char   *hds_tab[] = { "IN:", "OUT:", "STD:", "QLK:" };
char   *flds_tab[] = { "TAPE_ID",
		       "START_BLK",
		       "CUR_BLK",
		       "STATE"
                       };


static struct rcdrs {

    char tape_id[TAPEIDLEN];
    int start_blk;           /* Tape returns to pos when unmounted */
    int cur_blk;             /* Most recent seek position */
    int state;               /* Tape is mounted or unmounted */
} stat[4];                   /* Mnt status for IN and OUT rcdrs,
				followed by status for normal and
				QLK request output tapes */

static FILE    *fptr;

/* ASP-CP 2.1, 11/21/96 */
char inlog[104];

/* tape_find_blk(rcdr)------------------------------------
	This routine finds the current block location of
	the tape in the specified recorder and puts
	this value in the stat structure.  If the current
	location had not been recorded, the routine
	searches back to find the closest location with
	a recorded block address.  DCRSi communication
	must have been previously established and a
	tape loaded.
*/

int tape_find_blk(rcdr)
int    rcdr;
{
    STAT_RPT   st;
    LOC_RPT    loc;
    short int    rpi_dat[BLKSIZE];
    int   feet, act_feet, maxblks, i, j, drivesel, rev;
    int   backup, cur_block, end_block, err, wait_for_ft;
    int   try = 1;
    int   bypass = 1;

    if (bypass)
	return (PASS);
    printf("Determining initial position of cassette\n");
    if (dc_res_ft(rcdr) == FAIL)  /* Reset footage counter */
    {
	printf("DCRSi not responding to footage counter reset.\n");
	return(FAIL);
    }

    end_block = Cur_Rqst->end_blk;

    while (try <= SRCH_TIMES)  /* Try to find pos SRCH_TIMES */
    {
        feet = 10000 - (FT_INCR * try);  /* Footage to move to */
	try++;
	backup = 0;

        if (dc_mv_ft(rcdr,feet) == PASS)
        {
	    st.trgt_cmd = -1;
	    for (i = 0; i < 30; i++) {
		dc_clr_resp(rcdr,&st,&loc,&err,OFF);
		if ((st.trgt_cmd == st.cur_cmd) && (st.cur_cmd == 0))
		    break;
		if (i == 29) {
		    printf("Input tape did not reach desired footage\n");
		    return (FAIL);
		}
	    }
	    act_feet = 10000 - feet;  /* Num of feet actually moved */
	/*
	    printf("backed up %d feet\n",act_feet);
	*/
	    /* Number of blocks from present pos to start pos */
	    maxblks = (act_feet + 2) * BLK_FT;

	    /* Set up RPI board in MC5600 read mode */
	    reset_rpi(1);
drivesel=0;
rev=0;
            setup_rpi(0,2,0,0,1,1,drivesel,rev);
	    play_dis(1);

	    /* Start play-back at current position */
	    if (dc_play(rcdr,0,NULL,0,NULL) == PASS)
	    {
		reset_rpi(0);
		play_dis(0);
		/* Read five blocks of data */
		for (i=0; i<200; i++)
		{
		    if ((j = get_rpi_blk(rpi_dat)) != BLKSIZE)
		    {
			printf("read failed, %d words read\n",j);
/*			if (abob()) exit (1); */
			/* Full block not read */
		        backup = 1;
			break;
		    }
		}
		if (!backup)  /* Blocks were read */
		{
		    /* Read start blk */
		/* set_alarm(2);
		    pause(); */
		    sleep(2); /*clw avoid to use timer signal*/
		    cur_block = dc_get_addr(rcdr);
		    printf("recorder is at block %d\n",cur_block);
                    stat[rcdr].start_blk = cur_block;
		    /* If address not found, back up */
		    if (cur_block < 1) 
			backup = 1;
		    else {
			if (cur_block < end_block &&
				(cur_block > (end_block - maxblks)))
			    stat[rcdr].start_blk = end_block;
			else
			    stat[rcdr].start_blk = cur_block + 
				(act_feet * BLK_FT);
			printf("Tape starting position = %d\n",
				stat[rcdr].start_blk);
			return (PASS);
		    }
		}
		dc_stop(rcdr);  /* Stop play back */

	    }  /* if dc_play */
	}  /* if dc_mv_ft */
    }  /* while try */
    /* could not find a block number */
    dc_mv_ft(rcdr,0);   /* Move back to start footage */
    return(FAIL);
}



/* tape_rdflds()--------------------------------------------
	This routine reads the tape status file if it
	exists and puts the information into the status
	structure.  If the file does not exist, the
	structure is initialized to the unmouted state.
*/

int tape_rdflds()
{
    int    i, j;
    char   str[80];
    int    val;

    if (open_file(PROC_PATH,STAT_FILE) == 0)
    {
	/* If file not there initialize structures */
	strcpy(stat[IN].tape_id,"xxxxxx");
	stat[IN].start_blk = 0;
	stat[IN].cur_blk = 0;
	stat[IN].state = UNMOUNTED;
	strcpy(stat[OUT].tape_id,"xxxxxx");
	stat[OUT].start_blk = 0;
	stat[OUT].cur_blk = 0;
	stat[OUT].state = UNMOUNTED;
    }
    else /* Read status into structures */
    {
	toupper_sw = 0;  /* Do not convert file contents to upper */
	set_separators("= ");
	for (i = 0; i < 4; i++)
	{
            if (find_token(hds_tab[i]) == 1) /* Structure is present */
	    {
		for (next_token(str);strcmp(str,";");next_token(str))
		{
		    for (j = 0; j < 4; j++)
			if (strcmp(flds_tab[j],str) == 0)
			    break;
		    switch (j) {
			case 0:  /* Tape ID */
			    next_token(stat[i].tape_id);
			    break;
			case 1:  /* Starting block */
			    get_int(&stat[i].start_blk);
			    break;
			case 2:  /* Current block */
			    get_int(&stat[i].cur_blk);
			    break;
			case 3:  /* State */
			    next_token(str);
			    stat[i].state = (strcmp(str,"MOUNTED") == 0);
			    break; 
			default:
			    printf("unrecognized entry: %s\n",
				    str);
			    break;
		    }  /* switch */
		}  /* for next_token */
            }  /* if */
	}  /* for i */

        close_file();
    }
    return(PASS);
}



/* tape_wrflds(rcdr)--------------------------------------------
	This routine translates the tape status structures
	to readable character strings and writes them to
	a file.  The file must already be open for
	writing.
	4/22/91,dtc, mod. -- add rcdr as argument so that
	STD or QLK fields in the output are updated only
	if the rcdr is the OUT recorder.  AR1653.
*/

int tape_wrflds(rcdr)
int	rcdr;
{
    int    i;
    char   filename[PATHLEN];

    if (rcdr == OUT) {
      if (strcmp(Cur_Rqst->type,"QLK") == 0)
	stat[3] = stat[OUT];
      else
	stat[2] = stat[OUT];
    }
    strcpy(filename,PROC_PATH);
    strcat(filename,STAT_FILE);
    if ((fptr = fopen(filename,"w")) == NULL) {
	printf("cannot open tape status file\n");
	return (FAIL);
    }
    for (i = 0; i < 4; i++)
    {
        fprintf(fptr,"%s\n",hds_tab[i]);
        fprintf(fptr,"\t%s = %s\n",flds_tab[0],stat[i].tape_id);
        fprintf(fptr,"\t%s = %d\n",flds_tab[1],stat[i].start_blk);
        fprintf(fptr,"\t%s = %d\n",flds_tab[2],stat[i].cur_blk);
        fprintf(fptr,"\t%s = %s\n",flds_tab[3],
		stat[i].state ? "MOUNTED" : "UNMOUNTED");
	fprintf(fptr,"\t;\n");
    }
    fclose(fptr);
    return (PASS);
}



/* tape_updt_blk(rcdr,block)--------------------------------
	This routine reads the tape status file, and if
	a tape is mounted in the specified recorder, the
	current block field is updated and the new
	information is written to the file.  PASS is
	returned.  This routine does not move the tape.
*/

int tape_updt_blk(rcdr,block)
int    rcdr;
int    block;
{
    /* Able to read status file and tape is mounted */
    if ((tape_rdflds() == PASS) && (stat[rcdr].state == MOUNTED))
    {
        stat[rcdr].cur_blk = block;  /* Update file */
        tape_wrflds(rcdr);
	return(PASS);
    }
    else printf("Tape not mounted in DCRSi %s\n",hds_tab[rcdr]);
    return(FAIL);
}



/* tape_mv_blk(rcdr,block)----------------------------------
	This routine moves the tape to the location
	specified by block.  It assumes that the current
	mount status is already available in the stat
	structure.  If the move is successful, it updates
	the structure and writes it to a file.  If the
	move is not successful, the tape is returned to the
	old position.  The routine returns PASS or FAIL.
*/

int tape_mv_blk(rcdr,block)
int    rcdr;
int    block;
{
    if (stat[rcdr].state == MOUNTED)
        if (dc_mv_addr(rcdr,block) == PASS)
        {
	    stat[rcdr].cur_blk = block;
	    tape_wrflds(rcdr);
	    return(PASS);
        }
        else
	{
	    printf("Unable to move tape to block %d.\n",block);
	    if (stat[rcdr].cur_blk < 0)
		printf("Unable to return to previous position.\n");
	    else if (dc_mv_addr(rcdr,stat[rcdr].cur_blk) == FAIL)
	    {
		printf("Unable to return to previous position.\n");
		stat[rcdr].cur_blk = -1;
		tape_wrflds(rcdr);
	    }
	}
    else
	printf("Tape is not mounted.\n");
    return(FAIL);
}



/* tape_cmp_id(tape,take,log)-------------------------------
	This routine decodes the log data into tape and
	take fields and compares them to the arguments.
	If the 'take' argument is null, it is not
	compared.  PASS or FAIL is returned.
*/

/*added rcdr, by GWU, 8/8/96*/
int tape_cmp_id(rcdr, tape,take,log)
int     rcdr;
char    *tape, *take;
char    *log;
{
/* ASP-CP 2.1, 11/21/96*/
/*
    char inlog[104];
*/
    char inlog_tmp[104];
    char *WSptr;
    char tape_tmp[10];

    printf("Looking for tape %s",tape);
    if (take != NULL)
	printf(", take %s",take);
    printf("\n");
    printf("Log data says: %s\n",log);
  
    if (strlen(log) <= 7){
      strcpy(inlog,log);
      strcpy(inlog_tmp,log);
    }
    else {
           if (!strncasecmp(log, "WS", 2)){
             strncpy(inlog, log, 6);
             strncpy(inlog_tmp, log, 6);
             inlog[6] = '\0';
             inlog_tmp[6] = '\0';
           }
           else {
                  strncpy(inlog, log, 12);
                  strncpy(inlog_tmp, log, 12);
                  inlog[13] = '\0';
                  inlog_tmp[13] = '\0';
                }
          }


  if (strlen(inlog) > 7){
      if (strcmp(inlog,tape)){
        printf("*tape_cmp_id:inlog=%s\n", inlog);
	return (FAIL);
      }
  }
  else {
          strncpy(tape_tmp, tape+2, 2); /*WS or AS */
          strcpy(tape_tmp+2, tape+8); /*xxxx*/
          if (strncmp(inlog,tape_tmp, 6)){
            printf("*tape_cmp_id:inlog=%s\n", inlog);
	    return (FAIL);
          }
       }

    printf("*tape_cmp_id:inlog=%s\n", inlog);

/* added by GWU, 8/8/96*/
    strcpy(stat[rcdr].tape_id, inlog);
/*
    inlog[20] = '\0';
    if (take != NULL && strcmp(inlog+7,take))
        return (FAIL);
*/
    return (PASS);
}



/* tape_ver_id(rcdr,tape,take)------------------------------
	This routine reads the usr data log from the tape
	and verifies the tape id and the take id.  If the
	argument 'take' is null, only tape is verified.
	The user log data will only be available if at
	least 12000 blocks have just been read from tape.
	The routine has 3 return values:
		PASS = tape (and take if any) match OK
		FAIL = could not make the test
		ABORT = tape or take do not match
*/

int tape_ver_id(rcdr,tape,take)
int    rcdr;
char   *tape, *take;
{
    char   log[104];

    if (tape_rdflds() == FAIL)  /* cannot read status file */
     {
        printf("tape_ver_id: tape_rdflds FAIL\n");
	return(FAIL);
     }    
    
    if (dc_get_log(rcdr,log) != PASS) /* cannot get log data */
      {
        printf("tape_ver_id: dc_get_log FAIL\n");
	return (FAIL);
      }

    if (log[0] == ';')  /* log data is empty */
      {
        printf("tape_ver_id: log[0] ==';' FAIL\n");
	return(FAIL);   /*   (may be trying prematurely) */
       }

/*added rcdr, by GWU, 8/8/96*/
    if (tape_cmp_id(rcdr, tape,take,log) == PASS)
	return(PASS);
    else {
           printf("Tape number %s is not loaded.\n",tape);
           printf("Tape_media_id is not matched !!\n");
           return(FAIL);
         } 

}



/* tape_mnt(rcdr,tape)--------------------------------------
	This routine reads the tape status file if it
	exists.  If a tape is not already mounted in the
	specified recorder, the routine finds the current
	position of the tape in the recorder
	and stores it in the start block field of
	the status structure.  DCRSi communication
	must have been previously initialized.
	The tape id and state fields are also updated.
	and the structure is written to a file.
	FAIL is returned if a tape is already mounted
	and the user chooses not to override or if
	the start block cannot be found.  This routine
	moves the tape.
*/

int tape_mnt(rcdr,tape)
int    rcdr;
char   *tape;      /* Tape id */
{
    char    q[80];
    int     status;
    int blk_num;

    tape_rdflds();

/* resume tape confirmation 3/6/95 */


    if (!strcmp(chk_media_id,"YES")) {

      blk_num = Cur_Rqst->start_blk + 100000;
/*
      if (start_byte_play(Cur_Rqst->start_blk + blk_num) == FAIL)
*/
      if (blk_num > Cur_Rqst->end_blk) blk_num = Cur_Rqst->end_blk - 10;
      if (start_byte_play(blk_num) == FAIL)
        return(FAIL);

      if((status = tape_ver_id(rcdr, tape, NULL)) != PASS)
       {
            Write_CP_Error_Status = -2;
            return(FAIL);
        }

      dc_stop(IN);
/*
      dc_comm_close(IN);

      if (dcu_setup() != PASS)
         return(FAIL);
      if (dcu_connect(IN)!= PASS)
         return(FAIL);
*/

    } /*"YES"*/

    if (stat[rcdr].state == MOUNTED){ /* A tape is already mounted */
	if (strcmp(stat[rcdr].tape_id,tape) == 0) {
    		if (vbose) printf("tape_id=%s\n",stat[rcdr].tape_id);
		return(PASS);
	} else {
    	   sprintf(q, "Tape number %s should be inserted in DCRSi %s\n",
		tape,hds_tab[rcdr]);
    	   /*printf(q);
	   if (!strcmp(chk_media_id,"YES")) return (ABORT);*/
	}
    }

    if ( !cp_flag )
    if (op_answer("Is tape inserted") == FAIL) return(FAIL);

/* comment out for R1a by clw 10-11-94 
    if (stat[rcdr].state == MOUNTED) 
    {
	if (strcmp(stat[rcdr].tape_id,tape) == 0)
	    return(PASS);
	sprintf(q,"Tape number %s is already mounted in DCRSi %s\n",
		stat[rcdr].tape_id, hds_tab[rcdr]);
	printf(q);
	if (op_answer("Do you want to unmount this tape") == PASS)
	    tape_umnt(rcdr);
	
	strcpy(q,"Do you still want to mount tape number ");
	strcat(q,tape);

	if (op_answer(q) == FAIL) return(FAIL);

	strcpy(q,"Is tape inserted in DCRSi ");
	strcat(q,hds_tab[rcdr]);
	if (op_answer(q) == FAIL) return(FAIL);
    }
    else
    {
	sprintf(q, "Tape number %s should be inserted in DCRSi %s\n",
		tape,hds_tab[rcdr]);
	printf(q);
	if (op_answer("Is tape inserted") == FAIL) return(FAIL);
    }
*/

    while (dc_load(rcdr) == FAIL)
    {
	sprintf(q,"Unable to load tape number %s on DCRSi %s\n",
		tape,hds_tab[rcdr]);
	printf(q);
	/* comment out for R1a by clw 10-11-94 
	if (op_answer("Do you want to try again") == FAIL) 
	*/
	return(FAIL);
    }

    while (tape_find_blk(rcdr) == FAIL)  /* Unable to find pos */
    {
	printf("Unable to find current position of tape.\n");
	printf("Tape has not been mounted and may be removed.\n");
	dc_unload(rcdr);
	/* comment out for R1a by clw 10-11-94
	if (op_answer("Do you want to try again") == FAIL) 
	*/
	return(FAIL);
    }

    stat[rcdr].cur_blk = stat[rcdr].start_blk;
    strcpy(stat[rcdr].tape_id,tape);
    stat[rcdr].state = MOUNTED;
    tape_wrflds(rcdr);     /* Write new status to file */
    return(PASS);
}



/* tape_umnt(rcdr) -----------------------------------------------------
	This routine returns the tape (IN only) to the position
	stored in the start block field of the status
	structure, and then unloads the tape.  The status
	is changed to "unmounted".  The operator is informed
	that the tape may be removed.

	'rcdr' specifies the recorder to unmount.  If rcdr is -1, the
	routine unmounts whichever tape is mounted.  If both tapes are
	active, the routine asks which tape to unmount.
	4/22/91,dtc, mod-- part of AR1653.
	Add option to unmount neither tape if both are mounted.
*/

tape_umnt(rcdr)
int    rcdr;
{
    char    q[80],q1[80];
    int     either;

    either = (rcdr < 0);

/* get tape status file, if available */
    if (tape_rdflds() != PASS) {
	if (either)
	    printf("No tape is mounted\n");
	return;
    }

/* if rcdr is -1, determine which tape to unmount */
/* comment out for R1a by clw 10-11-94 
    if (either) {
	if (stat[IN].state == UNMOUNTED)
	    rcdr = OUT;
	else if (stat[OUT].state == UNMOUNTED)
	    rcdr = IN;
	else {
    	    strcpy(q,"Unmount the INPUT tape");
    	    strcpy(q1,"Unmount the OUTPUT tape");


	    if (op_answer(q) == PASS)
		rcdr = IN;
	    else {
		if (op_answer(q1) == PASS) rcdr = OUT;
		else return;
	    }
	}
    }
*/

    if (stat[rcdr].state != MOUNTED) {
	if (either)
	    printf("No tape is mounted\n");
	return;
    }

/* Specified tape is mounted */
    printf("Unmounting tape number %s.\n",stat[rcdr].tape_id);
/*
    if (rcdr == IN && dc_mv_addr(rcdr,stat[rcdr].start_blk) != PASS)
    {
	printf("\07** Warning: Tape cannot be returned to original ");
	printf("position.\n");
    }
*/

/* Update status file */
    dc_unload(rcdr);
    printf("Tape may be removed from DCRSi %s\n",hds_tab[rcdr]);
    stat[rcdr].state = UNMOUNTED;
    tape_wrflds(rcdr);
}



/* tape_mnt_stat(rcdr,mntd,cur_blk)---------------------------
	This routine reads the status file, if it exists, and
	returns the mount status and current position of
	the specified rcdr.  If the file does not exist,
	the routine returns an unmounted status and FAIL.
*/

int tape_mnt_stat(rcdr,mntd,cur_blk)
int    rcdr;
int    *mntd, *cur_blk;
{
    if (tape_rdflds() == PASS)
    {
	*mntd = stat[rcdr].state;
	*cur_blk = stat[rcdr].cur_blk;
	return(PASS);
    }

    *mntd = UNMOUNTED;
    stat[0].state = UNMOUNTED;
    stat[1].state = UNMOUNTED;
    tape_wrflds(rcdr);
    return(FAIL);
}


/* get_output_tape(id,block) -------------------------------------------
	This routine determines which output tape and position will be
	used to record the next output image, and returns the tape ID
	and starting block number.  If this cannot be done, the
	routine returns FAIL.
*/

get_output_tape(id,block)
	char *id;
	int *block;
{
	char s[80];
	int seq,rcdr2;
	int rcdr = OUT;

	if (strcmp(Cur_Rqst->type,"QLK") == 0)
	    rcdr2 = 3;
	else
	    rcdr2 = 2;
	if (strcmp(stat[rcdr].tape_id,stat[rcdr2].tape_id) != 0) {
	    dc_unload(rcdr);
	    stat[rcdr] = stat[rcdr2];
	    stat[rcdr].state = UNMOUNTED;
	}
	if (stat[rcdr].cur_blk > 10000000) {
	    if (stat[rcdr].state == MOUNTED) {
		dc_unload(rcdr);
		printf("\nNo more room on DCRSi rcdr:, tape %s\n",
			stat[rcdr].tape_id);
	    }
	    seq = atoi(&stat[rcdr].tape_id[2]);
	    sprintf(&stat[rcdr].tape_id[2],"%4.4d",seq+1);
	    stat[rcdr].start_blk = stat[rcdr].cur_blk = 100000;
	    stat[rcdr].state = UNMOUNTED;
	}
	if (stat[rcdr].state != MOUNTED) {
	    sprintf(s,"Please mount tape %s on DCRSi rcdr:\n",
		stat[rcdr].tape_id);
	    printf(s);		
	    sprintf(s," ...Press Enter when ready");
	    gets(s);
	    dc_load(rcdr);
	    stat[rcdr].state = MOUNTED;
	    tape_wrflds(rcdr);
	}
	strcpy(id,stat[rcdr].tape_id);
	*block = stat[rcdr].cur_blk;
	return (PASS);
}



int sony_tape_mnt(rcdr,tape)
int    rcdr;
char   *tape;      /* Tape id */
{
 
    unsigned char return_str[260];
    int  data_len;
    int  timeout;

    if (!strcmp(chk_media_id,"YES")) {

      printf("Looking for tape %s\n",tape);

      if (sony_send_cmd(rcdr, Extend_user_sense, NULL) == FAIL) {
        printf("sony_tape_mnt: can not get Extend_user_sense\n");
        return(FAIL);
      }
      if (sony_get_resp(rcdr, Extend_user_sense_ACK, return_str, &data_len, timeout) == PASS) {
        return_str[data_len] = '\0';
        printf("Extend user data: %s\n", return_str);
      
        if (strstr(return_str,tape) != NULL)
         return(PASS);
        else {

           printf("Tape_media_id is not matched !!\n");
           Write_CP_Error_Status = -2;
           return(FAIL);
         }
      }
      else {
        printf(" Can not get Extend user data sense\n");
        return(FAIL);
      }
}
    return(PASS);
} /*sony_tape_mnt*/
