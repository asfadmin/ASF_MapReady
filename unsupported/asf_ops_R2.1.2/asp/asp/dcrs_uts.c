/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* dcrs_uts.c - This module contains operating and diagnostic
	routines for the Ampex DCRSi Tape Recorder.
*/

#include <sys/ioctl.h>
#include <termio.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <procfil.h>
#include <procdec.h>
#include <sony.h>


#define LENGTH 132         /* Field length for commands and response */
#define STATLEN 30         /* Status field lenght */
#define TOK_SEP "\t ="     /* Token separators used for parser */


#define DCRSI_FILE "DCRSi.config"       /* Contains DCRSi info */
#define IN_COMM  "inDCRSi_comm"        /* Info in DCRSi file */
#define OUT_COMM "outDCRSi_comm"
#define IN_ID  "inDCRSi_id"
#define OUT_ID "outDCRSi_id"

#define BEG "B"                   /* Record or play beginning, */
#define CUR "C"                   /*  current or end tape */
#define END "E"                   /*  location.           */
                                  /* DCRSi commands */
#define FAULT "DE"                /* Display faults */
#define LOAD "LO"                 /* Load tape */
#define UNLOAD "UL"               /* Unload tape */
#define PAR_DAT "PD"              /* Use parallel data */
#define RCD_INH "RI"              /* Inhibit recording */
#define RCD_EN "RE"               /* Enable recording */
#define REWIND "RW"
#define FF "FF"                   /* Fast forward */
#define PLAY "PL"
#define RECORD "RC"
#define STOP "SL"                 /* Low priority stop */
#define EN_ECC "EE"               /* Enable error correction */
#define DIS_ECC "ED"              /* Disable error correction */
#define EN_MOTOR "ME"             /* Enable scanner */
#define DIS_MOTOR "MD"            /* Disable scanner */
#define IDENTIFY "DI"             /* Display ID command */
#define RES_FEET "FR"             /* Reset foot counter */
#define RCD_TEST "RT"             /* Record test */
#define PLAY_TEST "PT"            /* Play test */
#define GET_BER "DB"              /* Display bit error rate */
#define GET_FEET "DF"             /* Display footage */
#define GET_LOC  "SE; DL"         /* Display addr and time */
#define MOVE_FT "FI F"            /* Move tape to footage location */
#define MOVE_ADDR "FI A"          /* Move tape to address location */
#define RESET "RS"                /* System reset and initialize */
#define GET_STAT "DS"             /* Display status */
#define GET_AUX "DA"              /* Display aux status */
#define GET_DIP "DD"              /* Display dip switch settings */
#define GET_LOG "DU"		  /* Display user log data */
#define SET_LOG "LS"		  /* Set user log data */

typedef struct {
    char   id[STATLEN];
    int    term;
    int    infile;   /* Receive communication file descriptor */
    int    outfile;  /* Transmit communication file descriptor */
} RCDR_STAT;

typedef struct {
    int   code;
    char    *msg;
} DCRSI_FAULT;

static DCRSI_FAULT    er_tbl[] = {
	215,"Machine is record only",
	223,"Tape at EOT; can not execute command",
	224,"Record enable cassette tab missing",
	227,"Record enable mode has not been set",
	243,"Tape at BOT; can not execute command",
	252,"Tape is moving",
	253,"Cassette is already unloaded",
	260,"Cassette will not load",
	261,"Cassette not in place",
	262,"Cassette already loaded",
	290,"Search location not found",
	502,"Lost block due to system overhead",
	550,"Lost block in record due to system overhead",
	601,"Stop address invalid or non-existent",
	602,"Stop time code invalid or non_existent",
	603,"RECORD task aborted by request or on error",
	604,"PLAY task aborted by request or on error",
	605,"STOP task aborted on error",
	606,"Unable to set record mode",
	607,"Unable to set play mode",
	608,"Failed to enable writes",
	609,"Failed to disable writes",
	610,"Failed to enable reads",
	611,"Failed to disable reads",
	612,"Unable to write bit error rate command register",
	613,"Unable to reset buffer",
	614,"Unable to write buffer command register",
        680,"Low temperature fault-DCRSi temperature below 5 deg C",
	681,"Over-temperature fault-DCRSi temperature above 40 deg C",
	700,"Bit error rate control logic failed on internal test",
	701,"Bit error rate exceeds 10 exp -3 limit",
	709,"Temperature fault on servo",
	910,"Temperature fault on buffer",
	911,"Temperature fault on aux 1",
	912,"Temperature fault on aux 2",
	833,"Serial command too long",
	834,"Serial command invalid",
	838,"Command not implemented",
	839,"Invalid number of arguments in command",
	850,"Tract/write mode not selected",
	851,"Write channel not selected",
	852,"Invalid command parameter",
	908,"Broken tape (no idler tach in 2 sec)",
	918,"Record inhibited while in record",
	920,"Transport over-temperature",
	921,"Transport pressure fault",
};

/* Used for status display */
char   on_off[2][4] = {"OFF","ON"};

char   cmd_mode[7][14] = {
	"STOPPED",
	"UNLOADED",
	"PLAYBACK",
	"RECORD",
	"FAST FORWARD",
	"REWIND",
	"SEARCH"
};

char    io_sel[3][13] = {
	"Parallel I/O",
	"Serial I/O",
	"Custom I/O"
};

int     rs_sel[2] = {232,422};

char    parity[2][5] = {
	"ODD",
	"EVEN"
};

char    en_dis[2][9] = {"disabled","enabled"};


static int    tblsize = sizeof(er_tbl)/sizeof(DCRSI_FAULT);
RCDR_STAT     Rcdr[2];       /* Input and output rcdr status */
extern int    toupper_sw;
extern char host[16];
extern int vbose;

/* The following are status variables */
static char   Aux_Stat[2][STATLEN]; /* Current rcdr aux status */
static char   Cmd_Stat[2][STATLEN]; /* Current rcdr cmd status */
static char   Dip_Stat[2][STATLEN]; /* Current dip switch status */
static char   Id[2][STATLEN];       /* Current DCRSi id */
static char   Ber[2][STATLEN];      /* Current bit error rate */
static int    Feet[2];              /* Current tape location */
              /* Beginning, current and end refer to the */
	      /*  operation executing or just finished */
static char   Cur_Addr[2][STATLEN]; /* Current tape address */
static char   Beg_Addr[2][STATLEN]; /* Beginning tape address */
static char   End_Addr[2][STATLEN]; /* Ending tape address */
static int    Cur_Err[2];           /* Latest error received */



/* dc_make_config()--------------------------------------------
	This routine creates a DCRSi configuration file.
*/

int dc_make_config()
{
    FILE    *fptr;
    char    strg[LENGTH];     /* Holds string to write to file */
    char    filename[PATHLEN];  /* DCRSi config file */
    int     len;

    strcpy(filename,PROC_PATH);  /* Generate file name */
    strcat(filename,DCRSI_FILE);

    if ((fptr = fopen(filename,"w")) == NULL)
    {
        printf("Unable to create DCRSi config file\n");
        return(FAIL);
    }
    len = sprintf(strg,"\ninDCRSi_comm=%d",Rcdr[IN].term);
    if (fwrite(strg,len,1,fptr) == 1)
    {
        len = sprintf(strg,"\ninDCRSi_id=%s",Rcdr[IN].id);
        if (fwrite(strg,len,1,fptr) == 1)
	{
            len = sprintf(strg,"\noutDCRSi_comm=%d",Rcdr[OUT].term);
            if (fwrite(strg,len,1,fptr) == 1)
	    {
                len = sprintf(strg,"\noutDCRSi_id=%s",Rcdr[OUT].id);
                if (fwrite(strg,len,1,fptr) == 1)
		{
                    fclose(fptr);
                    return(PASS);
		}
            }
	}
    }
    printf("Unable to create DCRSi config file\n");
    return(FAIL);
}



/* dc_get_config()--------------------------------------------
	This routine gets the assigns DCRSi and terminal
	numbers for both recorders.  These default assignments
	are contained in DCRSI_FILE and may be changed by
	the operator.  If DCRSI_FILE is not found in the current
	working directory, the routine looks in /usr/local/asp.
*/

int dc_get_config()
{
    char    strg[LENGTH];     /* Holds string read from file */
    char    filename[PATHLEN];  /* DCRSi config file */
/* this is changed because commented version does not work */
/*  if (open_file(DCRSI_FILE,"") == 0) /* Function returns 0 as fail */
    if (open_file(ROOT_PATH,DCRSI_FILE) == 0) 
    {
	if (open_file("/usr/local/asp/",DCRSI_FILE) == 0)
	{
	    printf("DCRSi configuration file not available.\n");
	    return(FAIL);
	}
    }

    /* Get ready to read config file by setting up parser */
    set_separators(TOK_SEP);  /* Set token separators */
    toupper_sw = 0;           /* No conversion to upper case */

    if (find_token(IN_COMM) != 0)  /* Found */
        if (get_int(&Rcdr[IN].term) != 0) /* Found */
            if (find_token(OUT_COMM) != 0) /* Found */
                if (get_int(&Rcdr[OUT].term) != 0) /* Found */
		    if (find_token(IN_ID) != 0)
			if (next_token(Rcdr[IN].id) != 0)
			    if (find_token(OUT_ID) != 0)
				if (next_token(Rcdr[OUT].id) != 0)
				{
				    close_file();
				    /* All values assigned */
				    return(PASS);
                                }
    printf("Unable to read DCRSi configuration file.\n");
    close_file();
    return(FAIL);
}



/* dc_set_inrcdr(id,term)----------------------------------
	This routine allows user to change DCRSi id and
	terminal number for the input DCRSi.
*/

void dc_set_inrcdr(id,term)
char   *id;        /* DCRSi id number */
int    term;      /* tty terminal number */
{
    strcpy(Rcdr[IN].id,id);
    Rcdr[IN].term = term;
}



/* dc_set_outrcdr(id,term)--------------------------------
	This routine allows user to change DCRSi id and
	terminal number for the input DCRSi.
*/
void dc_set_outrcdr(id,term)
char   *id;        /* DCRSi id number */
int    term;      /* tty terminal number */
{
    strcpy(Rcdr[OUT].id,id);
    Rcdr[OUT].term = term;
}
 


/* dc_clr_stat(rcdr)----------------------------------------
	This routine clears all the status variables and
	is called before each command.
*/

void dc_clr_stat(rcdr)
int    rcdr;
{
    *Aux_Stat[rcdr] = NULL;
    *Cmd_Stat[rcdr] = NULL;
    *Id[rcdr] = NULL;
    *Cur_Addr[rcdr] = NULL;
    *Beg_Addr[rcdr] = NULL;
    *End_Addr[rcdr] = NULL;
    Feet[rcdr] = -1;
    Cur_Err[rcdr] = -1;
}



/* dc_comm_init(rcdr)------------------------------------
	This routine sets up the tty terminal for
	communication with the DCRSi.  The routine
	assigns the file descriptor number to the
	variable in the rcdr structure.  It returns
	PASS if successful.

	Modified 7/26/90. Ben Charny
	Includes setting ports that talk to DCRSi
	to have even parity, because MicroVAX
	cannot be changed to no parity on RS-232.
*/

int dc_comm_init(rcdr)
int    rcdr;          /* Input or output recorder code */
{
    char    dcrfile[20];
    struct termio new_setup;

    sprintf(dcrfile,"/dev/tty%2.2d",Rcdr[rcdr].term);
/*
    printf("Attempting to open/read-only %s\n",dcrfile);
*/
    if (Rcdr[rcdr].infile < 3) {
      Rcdr[rcdr].infile = open(dcrfile,O_RDONLY);
/*
      printf("Rcdr.infile %d\n",Rcdr[rcdr].infile);
*/

/* new code starts here */
      ioctl (Rcdr[rcdr].infile, TCGETA, &new_setup); /* get original state */
      if ( !strcmp(host,"newasp") ) /* if not smart */
      		new_setup.c_cflag |= PARENB; /* enable parity */
      new_setup.c_cflag &= ~PARODD; /* parity even */
      ioctl (Rcdr[rcdr].infile, TCSETA, &new_setup); /* set new state */
/* new code ends here */

      if (Rcdr[rcdr].infile < 0)
      {
	perror("dc_comm_init:  Unable to Open for READONLY\n");
	return(FAIL);
      }
      Rcdr[rcdr].outfile = open(dcrfile,O_WRONLY);
/*
printf("Rcdr.outfile %d\n",Rcdr[rcdr].outfile);
*/
      if (Rcdr[rcdr].outfile < 0)
      {
	perror("dc_comm_init:  Unable to Open for WRITEONLY\n");
	return(FAIL);
      }
    /* Initialize port */
      if (open_dcrsi(rcdr,Rcdr[rcdr].infile) != PASS) {
	printf("FAIL in open_dcrsi\n");
	return(FAIL);
      }
      dc_clr_stat(rcdr);
    }
    else {
      printf("DCRSi for %d already opened on channel %d.\n",
	      rcdr, Rcdr[rcdr].infile);
    }
    return(PASS);    /* It'll work */
}



/* dc_comm_close(rcdr)-----------------------------------
	This routine closes communication with the
	DCRSi.
*/

void dc_comm_close(rcdr)
int    rcdr;          /* Input or output recorder code */
{
  if(Rcdr[rcdr].infile < 3) {
    printf("DCRSi %d not opened to any channel\n",rcdr);
  }
  else {
    close_dcrsi(rcdr);
    close(Rcdr[rcdr].infile);
    close(Rcdr[rcdr].outfile);
    Rcdr[rcdr].infile = 0;
  }
}



/* dc_fault_decode(code)------------------------------------
	This routine searches through the DCRSi error table
	er_tbl, and prints the message appropiate for
	the error code.
*/

int dc_fault_decode(code)
int    code;       /* Fault code received from DCRSi */
{
    int    i;

    printf("DCRSi error encountered, FAULT CODE:  %d\n",code);
    /* Search table for code */
    for (i = 0; i < tblsize; i++) {
	if (code == er_tbl[i].code) {
	    if ( code == 261 ){
	    	printf("%s\n",er_tbl[i].msg);
                return(FAIL);
            }
	    else {  
	    	printf("%s\n",er_tbl[i].msg);
		if ( code == 290) return(FAIL);
	    }
	    break;
	    
	}
    }
}



/* dc_send_cmd(rcdr,cmd)---------------------------------
	This routine sends a command to the DCRSi
	communication file pointed to by fileptr.
	It returns PASS if it is able to write
	the command.  It does not wait for a response.
*/

int dc_send_cmd(rcdr,cmd)
int     rcdr;         /* Input or output recorder code */
char    *cmd;
{
    int    len;

    if (vbose) printf("DCRSI command:  %s\n",cmd);
    len = strlen(cmd);
    if (write(Rcdr[rcdr].outfile,cmd,len) < 0)
    {
	printf("Unable to transmit DCRSi command:  %s\n",cmd);
	return(FAIL);
    }
    if (write(Rcdr[rcdr].outfile,";",1) != 1)
	return(FAIL);
    return(PASS);
}



/* dc_filter(resp)-----------------------------------------------
	This routine filters non-ascii characters from the
	response string.
*/

void dc_filter(resp)
char    *resp;
{
    char    *strptr;

    for (strptr=resp; *strptr; strptr++)  /* Filter non-ascii */
    {
	if ((*strptr & 0xff) <= 0x7f) /* If ascii, save character */
	    *resp++ = *strptr;
    }
    *resp = '\0';
}




/* dc_get_prompt(rcdr,time,rcvd,flag)---------------------------
	This routine is not yet implemented and may not be
	needed.
	This routine collects responses from the DCRSi until the
	prompt and 'flag' are received or a period of 'time'
	passes without a response.  'flag' is a character string
	for a DCRSi display command.  If it is NULL, the routine
	waits for the prompt only.
	When the prompt and flag are received, it returns
	PASS.  It returns FAIL after a time-out.
	Other responses received before the
	prompt are assiged to the appropriate status variables
	and collected in rcvd.
*/

int dc_get_prompt(rcdr,time,rcvd,flag)
int    rcdr;
int    time;
char   *rcvd;
char   *flag;
{
    char    resp[LENGTH];    /* Individual response from DCRSi */
    char    code[STATLEN];   /* Error code from DCRSi */
    int     i, count;
    char    *hold;

    *rcvd = '\0';
    count = 0;
    while (dc_wait_for_str(rcdr,resp,LENGTH,time) == 0)  /* Get resp */
    {
	count++;
	dc_filter(resp);  /* Get rid of non-ascii */
     /* printf("%d:  %s\n",count,resp);*/
	i = 0;
	if (strchr(resp,'*') != NULL)  /* Prompt received */
	{
	    if (flag == NULL)
		return(PASS);     /* Only prompt required */
	    strcat(rcvd,"*");
	    if (resp[0] == '*')     /* First character is prompt */
	        i++;                /* Incr past prompt */
	}
        if (resp[i] == 'D')  /* A display command received */
	{
	    i++;
	    switch(resp[i])
	    {
		case 'Z':   /* Display revision */
			    strcat(rcvd,"Z");
			    break;
                case 'I':   /* Display ID */
			    strcpy(Id[rcdr],strtok(resp,"I"));
			    strcpy(Id[rcdr],strtok('\0'," "));
			    strcat(rcvd,"I");
			    break;
		case 'F':   /* Display Footage */
			    hold = strtok(resp,"F ");
			    Feet[rcdr] = atoi(strtok('\0'," "));
			    strcat(rcvd,"F");
			    break;
		case 'L':   /* Display Location on tape */
			    strcat(rcvd,"L");
			    /* If Current address */
			    if (strchr(resp,'C') != NULL)
			    {
				strcat(rcvd,"C;");
				strcpy(Cur_Addr[rcdr],strtok(resp,"C"));
				strcpy(Cur_Addr[rcdr],strtok('\0',"C-, "));
			    }
			    /* If Beginning address */
			    else if (strchr(resp,'B') != NULL)
			    {
				strcat(rcvd,"B;");
				strcpy(Beg_Addr[rcdr],strtok(resp,"B"));
				strcpy(Beg_Addr[rcdr],strtok('\0',"B-, "));
			    }
			    /* If Ending address */
			    else if (strchr(resp,'E') != NULL)
			    {
				strcat(rcvd,"E;");
				strcpy(End_Addr[rcdr],strtok(resp,"E"));
				strcpy(End_Addr[rcdr],strtok('\0',"E-, "));
			    }
			    break;
		case 'A':   /* Display Aux Status */
	                    strcpy(Aux_Stat[rcdr],strtok(resp,"A "));
			    strcpy(Aux_Stat[rcdr],strtok('\0'," "));
			    strcat(rcvd,"A");
			    break;
		case 'S':   /* Display Command Status */
		            strcpy(Cmd_Stat[rcdr],strtok(resp,"S "));
			    strcpy(Cmd_Stat[rcdr],strtok('\0'," "));
			    strcat(rcvd,"S");
			    break;
		case 'D':   /* Display dip switches */
		            strcpy(Dip_Stat[rcdr],strtok(resp,"DD "));
			    strcpy(Dip_Stat[rcdr],strtok('\0'," "));
			    strcat(rcvd,"D");
			    break;
		case 'B':   /* Display bit error rate */
		            strcpy(Ber[rcdr],strtok(resp,"B "));
			    strcpy(Ber[rcdr],strtok('\0'," "));
			    strcat(rcvd,"B");
			    break;
		case 'E':   /* Display Fault Code */
		            strcpy(code,strtok(resp,"E "));
			    strcpy(code,strtok('\0'," "));
			    strcat(rcvd,"E");
			    strcat(rcvd,code);
			    Cur_Err[rcdr] = atoi(code);
			    if (dc_fault_decode(atoi(code)) == FAIL){
				printf("Fail in dc_get_prompt\n");
			/*	return (atoi(code)); */
				return (FAIL);
			    }
            }
        }
	if ((strchr(rcvd,'*') != NULL) && (op_strstr(rcvd,flag) != NULL))
	    return(PASS);
    }
    return(FAIL);
}



/* dc_get_resp(rcdr,time,rcvd)----------------------------------
	This routine collects responses from the DCRSi.  If a
	period of 'time' passes without a response, it passes
	the collected responses as a character pointer.
	Responses are also assigned to the appropriate status
	variables.  This is altered to give back the whole string.
*/

int dc_get_resp(rcdr,time,rcvd)
int    rcdr;
int    time;
char   *rcvd;
{
    char    resp[LENGTH];    /* Individual response from DCRSi */
    char    code[STATLEN];   /* Error code from DCRSi */
    int     i, count, len, j;
    char    *hold;

    *rcvd = '\0';
    count = 0;
    while (dc_wait_for_str(rcdr,resp,LENGTH,time) == 0) {  /* Get resp */
	count++;
	dc_filter(resp);
	strcat(rcvd,resp);   
	i = 0;
	if (strchr(resp,'*') != NULL) {  /* Prompt received */
       /*   strcat(rcvd,"*");  */
	    if (resp[0] == '*')     /* First character is prompt */
	        i++;                /* Incr past prompt */
	}
        if (resp[i] == 'D') {  /* A display command received */
	    i++;
	    switch(resp[i]) {
		case 'Z':   /* Display revision */
			/*   strcat(rcvd,"Z");  */
			    break;
                case 'I':   /* Display ID */
			    strcpy(Id[rcdr],strtok(resp,"I"));
			    strcpy(Id[rcdr],strtok('\0'," "));
			  /*strcat(rcvd,"I"); */
			    break;
		case 'F':   /* Display Footage */
			    hold = strtok(resp,"F ");
			    Feet[rcdr] = atoi(strtok('\0'," "));
			  /*strcat(rcvd,"F");*/
			    break;
		case 'L':   /* Display Location on tape */
			  /*strcat(rcvd,"L");*/
			    /* If Current address */
			    if (strchr(resp,'C') != NULL) {
			/*strcat(rcvd,"LC;");*/
				strcpy(Cur_Addr[rcdr],strtok(resp,"C"));
				strcpy(Cur_Addr[rcdr],strtok('\0',"C-, "));
			    }
			    /* If Beginning address */
			    else if (strchr(resp,'B') != NULL) {
			/* strcat(rcvd,"LB;"); */
				strcpy(Beg_Addr[rcdr],strtok(resp,"B"));
				strcpy(Beg_Addr[rcdr],strtok('\0',"B-, "));
			    }
			    /* If Ending address */
			    else if (strchr(resp,'E') != NULL)
			    {
			/*strcat(rcvd,"LE;"); */
				strcpy(End_Addr[rcdr],strtok(resp,"E"));
				strcpy(End_Addr[rcdr],strtok('\0',"E-, "));
			    }
			    break;
		case 'A':   /* Display Aux Status */
	                    strcpy(Aux_Stat[rcdr],strtok(resp,"A "));
			    strcpy(Aux_Stat[rcdr],strtok('\0'," "));
		/*	    strcat(rcvd,"A"); */
			    break;
		case 'S':   /* Display Command Status */
		            strcpy(Cmd_Stat[rcdr],strtok(resp,"S "));
			    strcpy(Cmd_Stat[rcdr],strtok('\0'," "));
		/*	    strcat(rcvd,"S"); */
			    break;
		case 'D':   /* Display dip switches */
		            strcpy(Dip_Stat[rcdr],strtok(resp,"DD "));
			    strcpy(Dip_Stat[rcdr],strtok('\0'," "));
		/*	    strcat(rcvd,"D"); */
			    break;
		case 'B':   /* Display bit error rate */
		            strcpy(Ber[rcdr],strtok(resp,"B "));
			    strcpy(Ber[rcdr],strtok('\0'," "));
		/*	    strcat(rcvd,"B"); */
			    break;
		case 'E':   /* Display Fault Code */
		            strcpy(code,strtok(resp,"E "));
			    strcpy(code,strtok('\0'," "));
		/*	    strcat(rcvd,"E"); */
		/*		    strcat(rcvd,code); */
			    Cur_Err[rcdr] = atoi(code);
			    if (dc_fault_decode(atoi(code)) == FAIL){
				printf("Fail in dc_get_resp\n");
				return (atoi(code));
			    }
            } /* switch */
        } /* if */
    } /* while */
}



/* dc_report_stat(rcdr,status,visual)-----------------------
	This routine decodes the status contained in
	Cmd_Stat[rcdr] and returns the results in the
	status structure.  FAIL is returned if the
	status is not available.  If visual is on, the
	results are printed on the screen.
	The calling routine must pass the address of
	'status'.
*/

int dc_report_stat(rcdr,status,visual)
int    rcdr;
STAT_RPT    *status;
int    visual;         /* Print status on screen if on */
{
    int    stat_code;

    if (*Cmd_Stat[rcdr] == NULL)
    {
	if (visual == ON)
	    printf("DCRSi has not reported status.\n");
	return(FAIL);
    }

    sscanf(Cmd_Stat[rcdr],"%4x",&stat_code);  /* Convert to hex */

    /* Decode bits */
    status->data_rdy = (stat_code >> 15) & 0x1;
    status->lub_rdy = (stat_code >> 14) & 0x1;
    status->lub_rcd = (stat_code >> 13) & 0x1;
    status->eot = (stat_code >> 9) & 0x1;
    status->bot = (stat_code >> 8) & 0x1;
    status->trgt_cmd = (stat_code >> 4) & 0x7;
    status->cur_cmd = stat_code & 0x7;

    if (visual == ON)  /* Display status */
    {
	printf("DCRSi %s status:\n\n",Rcdr[rcdr].id);
	printf("%40s%20s\n","Data ready status",on_off[status->data_rdy]);
	printf("%40s%20s\n","Longitudinal user block ready",on_off[status->lub_rdy]);
	printf("%40s%20s\n","Longitudinal user block recorded",on_off[status->lub_rcd]);
        printf("%40s%20s\n","At end of tape",on_off[status->eot]);
	printf("%40s%20s\n","At beginning of tape",on_off[status->bot]);
	printf("%40s%20s\n","Target command mode",cmd_mode[status->trgt_cmd]);
	printf("%40s%20s\n","Current command mode",cmd_mode[status->cur_cmd]);
    }
    return(PASS);
}



/* dc_report_mode(rcdr,mode,visual)-----------------------
	This routine decodes the status contained in
	Aux_Stat[rcdr] and Dip_Stat[rcdr] 
	and returns the results in the
	mode structure.  FAIL is returned if the
	status is not available.  If visual is on, the
	results are printed on the screen.
	The calling routine must pass the address of 'mode'.
*/

int dc_report_mode(rcdr,mode,visual)
int    rcdr;
MODE_RPT    *mode;
int    visual;         /* Print status on screen if on */
{
    int    stat_code;

    if (*Aux_Stat[rcdr] == NULL)
    {
	if (visual == ON)
	    printf("DCRSi has not reported auxiliary status.\n");
	return(FAIL);
    }
    if (*Dip_Stat[rcdr] == NULL)
    {
	if (visual == ON)
	    printf("DCRSi has not reported dip switch settings.\n");
	return(FAIL);
    }

    sscanf(Aux_Stat[rcdr],"%4x",&stat_code);  /* Convert to hex */

    /* Decode bits */
    mode->rcd_en = (stat_code >> 15) & 0x1;
    mode->fifo_fault = (stat_code >> 14) & 0x1;
    mode->io_sel = (stat_code >> 8) & 0x3;
    mode->buf_norm = (stat_code >> 4) & 0x1;
    mode->scanner_en = (stat_code >> 3) & 0x1;
    mode->ecc_en = (stat_code >> 1) & 0x1;
    mode->ecc_pres = stat_code & 0x1;

    sscanf(Dip_Stat[rcdr],"%4x",&stat_code);  /* Convert to hex */

    /* Decode bits */
    mode->ser_io_addr = (stat_code >> 12) & 0xf;
    mode->maint_mode = (stat_code >> 8) & 0x1;
    mode->RS_sel = (stat_code >> 6) & 0x1;
    mode->baud_rate = (1 << ((stat_code >> 3) & 0x7)) * 300;
    mode->even_odd_par = (stat_code >> 2) & 0x1;
    mode->par_en = (stat_code >> 1) & 0x1;
    mode->unit_code = stat_code & 0x1;

    if (visual == ON) /* Display aux status */
    {
	printf("DCRSi %s mode:\n\n",Rcdr[rcdr].id);
	printf("%40s%20s\n","Record enabled",en_dis[mode->rcd_en]);
	printf("%40s%20s\n","FIFO buffer test fault",on_off[mode->fifo_fault]);
	printf("%40s%20s\n","Data I/O selection",io_sel[mode->io_sel]);
	printf("%40s%20s\n","Buffer mode normal",on_off[mode->buf_norm]);
	printf("%40s%20s\n","Scanner enabled",en_dis[mode->scanner_en]);
        printf("%40s%20s\n","ECC decoder enabled",en_dis[mode->ecc_en]);
        printf("%40s%20s\n","ECC decoder present",on_off[mode->ecc_pres]);
	printf("%40s%20d\n","Serial interface address",mode->ser_io_addr);
	printf("%40s%20s\n","Maintenance mode select",on_off[mode->maint_mode]);
	printf("%40s%20d\n","RS-422/232C select",rs_sel[mode->RS_sel]);
	printf("%40s%20d\n","Baud rate select",mode->baud_rate);
	printf("%40s%20s\n","Even/odd patrity",parity[mode->even_odd_par]);
        printf("%40s%20s\n","Parity enable",en_dis[mode->par_en]);
	printf("%40s%20d\n","7/8 unit code",mode->unit_code + 7);
    }
    return(PASS);
}



/* dc_report_loc(rcdr,loc)---------------------------------------
	This routine reports tape location information:
	addresses, and footage.
*/

void dc_report_loc(rcdr,loc,visual)
int    rcdr;
LOC_RPT    *loc;
{
    if (*Cur_Addr[rcdr] != NULL)
    {
	loc->cur_addr = atoi(Cur_Addr[rcdr]);
    }
    else loc->cur_addr = -1;
    if (*Beg_Addr[rcdr] != NULL)
	loc->beg_addr = atoi(Beg_Addr[rcdr]);
    else loc->beg_addr = -1;
    if (*End_Addr[rcdr] != NULL)
	loc->end_addr = atoi(End_Addr[rcdr]);
    else loc->end_addr = -1;
    loc->feet = Feet[rcdr];
    
    if (visual == ON)
    {
	printf("%40s%20d\n","Beginning address",loc->beg_addr);
	printf("%40s%20d\n","Current address",loc->cur_addr);
	printf("%40s%20d\n","Ending address",loc->end_addr);
	printf("%40s%20d\n","Footage",loc->feet);
    }
}
    




/**********************************************************/
/* The following routines are the low level DCRSi
   command functions.
*/



/* dc_clr_resp(rcdr,status,loc,err,visual)------------------
	This routine call get resp to clear response buffer
        and set available status information.  It will report
	addresses, footage and status.
*/

void dc_clr_resp(rcdr,status,loc,err,visual)
int    rcdr;
STAT_RPT    *status;
LOC_RPT     *loc;
int    *err;
{
    char    rcvd[LENGTH];  /* Response received */
    dc_get_resp(rcdr,1,rcvd);
    dc_report_stat(rcdr,status,visual);
    if (visual == ON)
	printf("\n");
    dc_report_loc(rcdr,loc,visual);
    *err = Cur_Err[rcdr];
    if (visual == ON) {
	printf("%40s%20d\n","Error code",*err);
	printf("Note:  Not available is -1.\n\n");
    }
}



/* dc_get_stat(rcdr,status,visual)-----------------------
	This routine commands the DCRSi to display status,
	decodes the status and displays it if visual is
	ON.  It returns FAIL if the status and the
	prompt are not returned.  The status is returned
	is the structure 'status'.
	The calling routine must pass the address of 'status'.
*/

int dc_get_stat(rcdr,status,visual)
int    rcdr;
STAT_RPT    *status;
int    visual;
{
    char rcvd[LENGTH];

    dc_send_cmd(rcdr,GET_STAT);
    if (dc_get_prompt(rcdr,5,rcvd,"S") == FAIL)
    {
	if (visual == ON)
	    printf("DCRSi did not respond to status request.\n");
	return(FAIL);
    }
    if (dc_report_stat(rcdr,status,visual) == FAIL)
	return(FAIL);
    return(PASS);
}



/* dc_get_mode(rcdr,mode,visual)------------------------------
	This routine commands the DCRSi to display the
	aux status and dip switch settings.   It decodes
	the results and if visual is ON, prints them on the
	screen.  It returns FAIL if the prompt and requested
	status are not returned by the DCRSi.  The information
	is returned in the structure 'mode'.
	The calling routine must pass the address of 'mode'.
*/

int dc_get_mode(rcdr,mode,visual)
int    rcdr;
MODE_RPT    *mode;
int    visual;
{
    char    rcvd[LENGTH];       /* Responses received */
    int     code;               /* Fault codes */

    dc_send_cmd(rcdr,GET_AUX);
    if (dc_get_prompt(rcdr,5,rcvd,"A") == FAIL)
    {
	if (visual == ON)
            printf("DCRSi did not respond to auxiliary status request.\n");
        return(FAIL);
    }
    dc_send_cmd(rcdr,GET_DIP);
    if (dc_get_prompt(rcdr,5,rcvd,"D") == FAIL)
    {
	if (visual == ON)
            printf("DCRSi did not respond to auxiliary status request.\n");
        return(FAIL);
    }
    if (dc_report_mode(rcdr,mode,visual) == FAIL)
	return(FAIL);
    return(PASS);
}



/* dc_reset(rcdr,id)-------------------------------------------
	This routine resets the recorder.  It passes back
	the recorder ID string if it is successful.  It calls 
	dc_get_resp and returns PASS if prompt, recorder ID 
	and version are received.  Otherwise, it returns FAIL.
*/

int dc_reset(rcdr,id)
int    rcdr;
char   *id;
{
    char    rcvd[1000];  /* Response received */

    dc_clr_stat(rcdr);          /* Clear status */
/*
printf("DC_RESET sending initial termination\n");
    dc_send_cmd(rcdr,";");
    dc_get_resp(rcdr,3,rcvd);   
printf("DC_RESET got initial termination response (please ignore):\n");
printf("%s\n", rcvd );
*/
    dc_send_cmd(rcdr,RESET);
    dc_get_resp(rcdr,3,rcvd);   

    if ((strchr(rcvd,'*') == NULL) || (strchr(rcvd,'I') == NULL) || (strchr(rcvd,'Z') == NULL))
    {
    /* try a second time, just for luck */
	dc_clr_stat(rcdr);          /* Clear status */
	dc_send_cmd(rcdr,RESET);
	dc_get_resp(rcdr,5,rcvd);   /* Get response */
	if ((strchr(rcvd,'*') == NULL) || (strchr(rcvd,'I') == NULL) || (strchr(rcvd,'Z') == NULL))
	{
	    *id = '\0';
	    if ( strcmp(host,"newasp") ) return (PASS);
	    else return(FAIL);  /* No response */
	}
    }
    strcpy(id,Id[rcdr]);        /* Get id stored by dc_get_resp */
    return (PASS);
}



/* dc_identify(rcdr,id)-------------------------------------
	This routine sends the identify command and
	passes the id as a pointer.  It calls dc_get_resp
	and returns PASS if prompt and recorder ID are
	received.  Otherwise, it returns FAIL.
*/

int dc_identify(rcdr,id)
int    rcdr;
char   *id;
{
    char    rcvd[LENGTH];       /* Responses received */
    int     code;               /* Fault codes */

    /* delaying statement */

    printf("dc_identify:rcdr %d\n",rcdr);
    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,IDENTIFY);
    if (dc_get_prompt(rcdr,5,rcvd,"I") == FAIL)
    {
	*id = '\0';
	printf("FAIL in dc_identify\n");
        return(FAIL);
    }
    else
    {
	strcpy(id,Id[rcdr]);   /* Get id stored by dc_get_resp */
        return(PASS);
    }
}



/* dc_get_ft(rcdr)------------------------------------------
	This routine sends the display footage command.
	dc_get_resp is called and PASS is returned if the
	prompt and footage is received.  Otherwise, FAIL
	is returned.
*/

int dc_get_ft(rcdr)
int    rcdr;
{
    char   rcvd[LENGTH];    /* Responses received */
    
    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,GET_FEET);
    if (dc_get_prompt(rcdr,5,rcvd,"F") == FAIL)
	return(FAIL);
    return(Feet[rcdr]);
}



/* dc_rewind(rcdr)------------------------------------------
	This routine sends the rewind command.  dc_get_resp
	is called and PASS is returned if the prompt is
	received and error 261 is not received.  Since
	rewinding can take some time, dc_clr_resp must be
	called later to purge the response buffer and the
	status variables must be checked to determine if
	rewind was successful.
*/

int dc_rewind(rcdr)
int    rcdr;         /* Input or output recorder */
{
    char    rcvd[LENGTH];       /* Responses received */

    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,REWIND);
    dc_get_resp(rcdr,5,rcvd);  /* Collect responses */
    if ((strchr(rcvd,'*') == NULL) || (op_strstr(rcvd,"E261") != NULL))
	return(FAIL);      /* Prompt not received */
    return(PASS);
}



/* dc_ff(rcdr)----------------------------------------------
	This routine sends the fast forward command.  dc_get_resp
	is called and PASS is returned if the prompt is
	received and error 261 is not received.  Since
	fast forwarding can take some time, dc_clr_resp must be
	called later to purge the response buffer and the
	status variables must be checked to determine if
	rewind was successful.
*/

int dc_ff(rcdr)
int    rcdr;         /* Input or output recorder */
{
    char    rcvd[LENGTH];       /* Responses received */

    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,FF);
    dc_get_resp(rcdr,5,rcvd);  /* Collect responses */
    if ((strchr(rcvd,'*') == NULL) || (op_strstr(rcvd,"E261") != NULL))
	return(FAIL);      /* Prompt not received */
    return(PASS);
}



/* dc_load(rcdr)--------------------------------------------
	This routine loads the tape in the recorder and
	returns PASS if the prompt was received and error
	261 was not received.  dc_get_resp is called to
	receive DCRSi responses.
*/

int dc_load(rcdr)
int    rcdr;
{
    char    rcvd[LENGTH];       /* Responses received */

    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,LOAD);
    dc_get_resp(rcdr,5,rcvd);  /* Collect responses */
     
    if ((strchr(rcvd,'*') == NULL) || (op_strstr(rcvd,"E261") != NULL))
	return(FAIL);      /* Prompt not received */
    return(PASS);
}



/* dc_unload(rcdr)------------------------------------------
	This routine unloads the tape in the recorder and
	returns PASS if the prompt is received.  dc_get_resp
	is called to receive DCRSi responses.
*/

int dc_unload(rcdr)
int    rcdr;
{
    char    rcvd[LENGTH];       /* Responses received */

    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,UNLOAD);
    dc_get_resp(rcdr,5,rcvd);  /* Collect responses */
     
    if (strchr(rcvd,'*') == NULL)
	return(FAIL);      /* Prompt not received */
    return(PASS);
}



/* dc_en_motor(rcdr,en)-------------------------------------
	This routine enables or disables the scanner motor
	and returns PASS if the prompt is received.
	dc_get_resp is called to received DCRSi responses.
*/

int dc_en_motor(rcdr,en)
int    rcdr;
int    en;
{
    char    rcvd[LENGTH];       /* Responses received */

    dc_clr_stat(rcdr);          /* Clear status */
    if (en == ON)
        dc_send_cmd(rcdr,EN_MOTOR);
    else
	dc_send_cmd(rcdr,DIS_MOTOR);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
        return(FAIL);
    return(PASS);
}



/* dc_en_ecc(rcdr,en)-------------------------------------
	This routine enables or disables the error
	correction coding and returns if the prompt
	is received.  dc_get_resp is called to receive
	DCRSi responses.
*/

int dc_en_ecc(rcdr,en)
int    rcdr;
int    en;
{
    char    rcvd[LENGTH];       /* Responses received */

    dc_clr_stat(rcdr);          /* Clear status */
    if (en == ON)
        dc_send_cmd(rcdr,EN_ECC);
    else
	dc_send_cmd(rcdr,DIS_ECC);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
        return(FAIL);
    return(PASS);
}



/* dc_par_data(rcdr)--------------------------------------
	This routine seclects parallel data IO
	and returns PASS if the prompt is received.
	dc_get_resp is called to receive the DCRSi
	responses.
*/

int dc_par_data(rcdr)
int    rcdr;
{
    char    rcvd[LENGTH];       /* Responses received */

    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,PAR_DAT);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	return(FAIL);      /* Prompt not received */
    return(PASS);
}



/* dc_en_rcd(rcdr,en)-----------------------------------
	This routine enables or disables recording and
	returns PASS if the prompt is received.
	dc_get_resp is called to receive DCRSi
	responses.
*/

int dc_en_rcd(rcdr,en)
int    rcdr;
int    en;
{
    char    rcvd[LENGTH];       /* Responses received */

    dc_clr_stat(rcdr);          /* Clear status */
    if (en == ON)
	dc_send_cmd(rcdr,RCD_EN);
    else
        dc_send_cmd(rcdr,RCD_INH);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	return(FAIL);      /* Prompt not received */
    return(PASS);
}



/* dc_res_ft(rcdr)---------------------------------------------
	This routine resets the DCRSi foot counter.
	PASS is returned if the prompt is received.
	dc_get_resp is called to receive DCRSi responses.
*/

int dc_res_ft(rcdr)
int    rcdr;
{
    char    rcvd[LENGTH];    /* Responses received */

    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,RES_FEET);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	return(FAIL);
    return(PASS);
}



/* dc_mv_ft(rcdr,addr)---------------------------------------
	This routine moves the tape the number of feet
	specified by addr.  It returns PASS if the prompt is
	received, but the DCRS may not be finished executing
	the search.  dc_clr_resp must
	be called sometime later to purge the respons buffer
	and the status variable must be checked to determine
	if the move was successful.
*/

int dc_mv_ft(rcdr,addr)
int    rcdr;
int    addr;
{
    char    cmd[LENGTH];
    char    rcvd[LENGTH];    /* Responses received */
    char    feet[LENGTH];

    dc_clr_stat(rcdr);          /* Clear status */
    sprintf(feet,"%d",addr);   /* Convert addr to char */
    strcpy(cmd,MOVE_FT);   /* Generate command */
    strcat(cmd,feet);

    dc_send_cmd(rcdr,cmd);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL) /* Only need prompt */
	return(FAIL);
    return(PASS);
}



/* dc_mv_addr(rcdr,addr)-------------------------------------
	This routine moves the tape to the address
	specified by addr.  It returns PASS if the prompt is
	received, but the DCRS may not be finished executing
	the search.  dc_clr_resp must
	be called sometime later to purge the respons buffer
	and the status variable must be checked to determine
	if the move was successful.
*/

int dc_mv_addr(rcdr,addr)
int    rcdr;
int    addr;
{
    char    cmd[LENGTH];
    char    rcvd[LENGTH];    /* Responses received */
    char    blk[LENGTH];

    dc_clr_stat(rcdr);          /* Clear status */
    sprintf(blk,"%d",addr);   /* Convert addr to char */
    strcpy(cmd,MOVE_ADDR);   /* Generate command */
    strcat(cmd,blk);

    dc_send_cmd(rcdr,cmd);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL) /* Only need prompt */
	return(NULL);
    return(PASS);
}



/* dc_stop(rcdr)--------------------------------------------------
	This routine stops record or play and returns PASS
	if the prompt is received.  dc_get_resp is called to
	receive responses from the DCRSi.  dc_clr_resp and
	dc_stat should be called to retrieve the address or
	check for errors if the command fails.
*/

int dc_stop(rcdr)
int    rcdr;
{
    char    rcvd[LENGTH];   /* Collected responses */

    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,STOP);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL) 
	return(FAIL);
    return(PASS);
}



/* dc_play(rcdr,start,start_form,stop,stop_form)------------------
	This routine causes the DCRSi to play from start to stop.
	Start or stop may be NULL character strings.  The
	routine returns PASS if the prompt is received and
	error 290 is not received.  Since it may require some
	time for the start to be found, dc_clr_resp should be
	called some time later to purge the response buffer
	and dc_report_stat to retrieve addresses and errors.
	The status variable should be checked to determine if
	the command was successful.
*/

int dc_play(rcdr,start,start_form,stop,stop_form)
int    rcdr, start, stop;
char   *start_form, *stop_form;
{
    char    rcvd[LENGTH];
    char    cmd[LENGTH];
    char    addr[LENGTH];

    dc_clr_stat(rcdr);          /* Clear status */
    strcpy(cmd,PLAY);    /* Generate command */
    if ((*start_form != NULL) || (*stop_form != NULL))
        strcat(cmd," ");
    if (*start_form != NULL)  /* Start address received */
    {
        strcat(cmd,start_form);
        sprintf(addr,"%d",start);
        strcat(cmd,addr);
    }
    if (*stop_form != NULL)   /* Stop address received */
    {
        strcat(cmd,",");
	strcat(cmd,stop_form);
	sprintf(addr,"%d",stop);
        strcat(cmd,addr);
    }

    dc_send_cmd(rcdr,cmd);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	return(FAIL);
    
    if (op_strstr(rcvd,"E290") != NULL)
	return(FAIL);
    if ((*start_form == NULL) && (*stop_form == NULL)) {
	/* set_alarm(3);
	pause(); */
	sleep(3);  /* clw avoid to use timer signal */
    }
    return(PASS);
}



/* dc_play_test(rcdr,start,start_form,stop,stop_form)-------------
	This routine causes the DCRSi to play test data from
	start to stop.  Start or stop may be NULL character
	strings.  The routine returns PASS if the prompt
	is received and error 290 is not received.  Since it
	may require some time for the start to be found,
	dc_clr_resp should be called some time later to purge
	the response buffer and dc_report_stat to retrieve
	addresses and errors.  The status variable should be
	checked to determine if the command was successful.
*/

int dc_play_test(rcdr,start,start_form,stop,stop_form)
int    rcdr, start, stop;
char   *start_form, *stop_form;
{
    char    rcvd[LENGTH];
    char    cmd[LENGTH];
    char    addr[LENGTH];

    dc_clr_stat(rcdr);          /* Clear status */
    strcpy(cmd,PLAY_TEST);    /* Generate command */
    if ((*start_form != NULL) || (*stop_form != NULL))
        strcat(cmd," ");
    if (*start_form != NULL)  /* Start address received */
    {
        strcat(cmd,start_form);
        sprintf(addr,"%d",start);
        strcat(cmd,addr);
    }
    if (*stop_form != NULL)   /* Stop address received */
    {
        strcat(cmd,",");
	strcat(cmd,stop_form);
	sprintf(addr,"%d",stop);
        strcat(cmd,addr);
    }

    dc_send_cmd(rcdr,cmd);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	return(FAIL);
    
    if (op_strstr(rcvd,"E290") != NULL)
	return(FAIL);
    return(PASS);
}



/* dc_record(rcdr,start,start_form,stop,stop_form)----------------
	This routine causes the DCRSi to write from start to stop.
	Start or stop may be NULL character strings.  The
	routine returns PASS if the prompt is received and
	error 290 is not received.  Since it may require some
	time for the start to be found, dc_clr_resp should be
	called some time later to purge the response buffer
	and dc_report_stat to retrieve addresses and errors.
	The status variable should be checked to determine if
	the command was successful.
*/

int dc_record(rcdr,start,start_form,stop,stop_form)
int    rcdr, start, stop;
char   *start_form, *stop_form;
{
    char    rcvd[LENGTH];
    char    cmd[LENGTH];
    char    addr[LENGTH];

    dc_clr_stat(rcdr);          /* Clear status */
    strcpy(cmd,RECORD);        /* Generate command */
    if ((*start_form != NULL) || (*stop_form != NULL))
        strcat(cmd," ");
    if (*start_form != NULL)  /* Start address received */
    {
        strcat(cmd,start_form);
        sprintf(addr,"%d",start);
        strcat(cmd,addr);
    }
    if (*stop_form != NULL)   /* Stop address received */
    {
        strcat(cmd,",");
	strcat(cmd,stop_form);
	sprintf(addr,"%d",stop);
        strcat(cmd,addr);
    }

    dc_send_cmd(rcdr,cmd);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	return(FAIL);
    
    if (op_strstr(rcvd,"E290") != NULL)
	return(FAIL);
    return(PASS);
}



/* dc_rcd_test(rcdr,start,start_form,stop,stop_form)-------------
	This routine causes the DCRSi to write test data from
	start to stop.  Start or stop may be NULL character
	strings.  The routine returns PASS if the prompt is
	received and error 290 is not received.  Since it may
	require some time for the start to be found, dc_clr_resp
	should be called some time later to purge the response
	buffer and dc_report_stat to retrieve addresses and
	errors.  The status should be checked to determine if
	the command was successful.
*/

int dc_rcd_test(rcdr,start,start_form,stop,stop_form)
int    rcdr, start, stop;
char   *start_form, *stop_form;
{
    char    rcvd[LENGTH];
    char    cmd[LENGTH];
    char    addr[LENGTH];

    dc_clr_stat(rcdr);          /* Clear status */
    strcpy(cmd,RCD_TEST);      /* Generate command */
    if ((*start_form != NULL) || (*stop_form != NULL))
        strcat(cmd," ");
    if (*start_form != NULL)  /* Start address received */
    {
        strcat(cmd,start_form);
        sprintf(addr,"%d",start);
        strcat(cmd,addr);
    }
    if (*stop_form != NULL)   /* Stop address received */
    {
        strcat(cmd,",");
	strcat(cmd,stop_form);
	sprintf(addr,"%d",stop);
        strcat(cmd,addr);
    }

    dc_send_cmd(rcdr,cmd);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	return(FAIL);
    
    if (op_strstr(rcvd,"E290") != NULL)
	return(FAIL);
    return(PASS);
}



/* dc_get_addr(rcdr)-----------------------------------------------
	This routine asks for the current tape address to be
	displayed.  It returns when the prompt and the commmand
	response is received.  Either FAIL or the current
	address is returned.
*/

int dc_get_addr(rcdr)
int    rcdr;
{
    char    rcvd[LENGTH];   /* Responses received */
    char    cmd[LENGTH];

    dc_clr_stat(rcdr);          /* Clear status */
    dc_send_cmd(rcdr,"SE");
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	return(FAIL);
    dc_send_cmd(rcdr,"DL");   
/*  dc_get_resp(rcdr,5,rcvd);  */
    if (dc_get_prompt(rcdr,5,rcvd,"LC;") == FAIL)
	return(FAIL);
    return(atoi(Cur_Addr[rcdr]));
}



/* dc_inc_addr(rcdr,inc,retcode)-------------------------------------
	This routine gets the tape address every inc seconds and
	returns when the address specified by retcode is received
	or time-out occures.
	retcode may be BEG, CUR, END or NULL.  The minumun inc
	is 2 seconds, otherwise dc_get_resp will never return
	when the response buffer is being purged.
	If retcode is NULL, the routine returns as soon as the
	prompt is received.
	Either the address specified by retcode or FAIL is
	returned.
*/

int dc_inc_addr(rcdr,inc,retcode)
int    rcdr;
int    inc;       /* Time between displays in secondes, min 2 */
char   *retcode;  /* Which address, BEG, CUR, or END to pass in addr */
{
    char    cmd[LENGTH];
    char    resp[LENGTH];   /* Individual response from DCRSi */
    char    rcvd[LENGTH];   /* Collected responses from DCRSi */
    char    code[STATLEN];  /* Fault code */
    char    strg[LENGTH];
    int     time;           /* Time to wait for a response */
    int     i;

    *rcvd = '\0';
    strcpy(cmd,GET_LOC);   /* Generate command */
    strcat(cmd," ");
    sprintf(strg,"%d",inc);   /* Convert to characters */
    strcat(cmd,strg);

    dc_send_cmd(rcdr,cmd);

    /* Wait inc + 1 for responses */
    time = inc + 1;
    strcpy(cmd,"L");
    strcat(cmd,retcode);
    strcat(cmd,";");

    if (dc_get_prompt(rcdr,time,rcvd,cmd) == FAIL)
	return(FAIL);
    if (*retcode == NULL)
	return(PASS);
    if (strcmp(retcode,BEG) == 0)
	return(atoi(Beg_Addr[rcdr]));
    if (strcmp(retcode,END) == 0)
	return(atoi(End_Addr[rcdr]));
    return(atoi(Cur_Addr[rcdr]));
}



/* dc_inc_addr_off(rcdr)-------------------------------------
	This routine turns off the incremental location
	display.  It returns PASS if the prompt is received.
	dc_get_resp is called to receive DCRSi responses.
*/

int dc_inc_addr_off(rcdr)
int    rcdr;
{
    char    rcvd[LENGTH];   /* Responses received */
    char    cmd[LENGTH];

    strcpy(cmd,GET_LOC);   /* Generate command */
    strcat(cmd," 0");
    dc_send_cmd(rcdr,cmd);
    if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	return(FAIL);
    return(PASS);
}



/* dc_stat_beg_addr(rcdr)-----------------------------------
	This routine purges the response buffer and returns
	the latest Beg_Addr setting.  User must have previously
	given a command which caused the DCRSi to display this
	address, in order for this value to be meaningful.
	The value of the beginning address status is returned.
*/

int dc_stat_beg_addr(rcdr)
int    rcdr;
{
    char    rcvd[LENGTH];

    /* Purge response buffer */
    dc_get_resp(rcdr,1,rcvd);

    return(atoi(Beg_Addr[rcdr]));   /* Copy current address */
}



/* dc_stat_cur_addr(rcdr)-----------------------------------
	This routine purges the response buffer and returns
	the latest Cur_Addr setting.  User must have previously
	given a command which caused the DCRSi to display this
	address, in order for this value to be meaningful.
	The value of the current address status is returned.
*/

int dc_stat_cur_addr(rcdr)
int    rcdr;
{
    char    rcvd[LENGTH];

    /* Purge response buffer */
    dc_get_resp(rcdr,1,rcvd);

    return(atoi(Cur_Addr[rcdr]));   /* Copy current address */
}



/* dc_get_ber(rcdr,ber)-----------------------------------
	This routine gets the bit error rate from the DCRSi
	BER reader during playback.  The result is not
	meaningful unless a PR sequence has previously been
	recorded.
*/

int dc_get_ber(rcdr,ber)
int    rcdr;
double *ber;
{
    char    rcvd[LENGTH];
    char    strg[LENGTH];

    dc_send_cmd(rcdr,GET_BER);
    if (dc_get_prompt(rcdr,5,rcvd,"B") == FAIL)
	return(FAIL);
    strg[0] = Ber[rcdr][1];
    strg[1] = Ber[rcdr][2];
    strg[2] = 'e';
    strg[3] = Ber[rcdr][0];
    strg[4] = NULL;
    *ber = atof(strg);

    return(PASS);
}



/* dc_get_log(rcdr,log)-----------------------------------
	This routine gets the user log data from the DCRSi.
*/

int dc_get_log(rcdr,log)
int    rcdr;
char   *log;
{
    char    rcvd[LENGTH];

    dc_send_cmd(rcdr,GET_LOG);
    dc_get_resp(rcdr,5,rcvd);
    if (strlen(rcvd) > 5)
	strcpy(log,&rcvd[5]);
    else {
	log[0] = '\0';
	return(FAIL);
    }
    return(PASS);
}



/* dc_set_log(rcdr,log) ------------------------------------------------
	This routine sets the user log data to be written to the DCRSi.
*/

int dc_set_log(rcdr,log)
int    rcdr;
char   *log;
{
    char    rcvd[LENGTH];
    char    cmd[120];

    sprintf(cmd,"%s %s",SET_LOG,log);
    dc_send_cmd(rcdr,cmd);
    return (dc_get_prompt(rcdr,5,rcvd,NULL));
}




/************************************************************/
/* The following routines use the low level command
   functions.
*/

/* dc_setup()-----------------------------------------------
	This routine does what is required to configure
	the DCRSi's for use without the ASP processing
	initialization routines.  It asks the user for
	any missing terminal information.  The
	dc_set_inrcdr, dc_set_outrcdr and dc_get_config
	routines need not be called if this routine is
	used.  It returns PASS or FAIL.
*/

int dc_setup()
{
    if (dc_get_config() == FAIL)
    {
	printf("\nInput IN RCDR id ('?' if unknown)-> ");
	scanf("%s",Rcdr[IN].id);
	printf("\nInput IN RCDR terminal number-> ");
	scanf("%d",&Rcdr[IN].term);
	printf("\nInput OUT RCDR id ('?' if unknown)-> ");
	scanf("%s",Rcdr[OUT].id);
	printf("\nInput OUT RCDR terminal number-> ");
	scanf("%d",&Rcdr[OUT].term);
        if (dc_make_config() == PASS)
	    return(PASS);
        else
	    return(FAIL);
    }
    else
	return(PASS);
}



/* dc_init(rcdr)--------------------------------------------
	(Uses low level command routines.)
	This routine verifies the connection of the DCRSi
	specified by rcdr, to the tty port specified by
	rcdr.  The recorder is initialized with recording
	inhibited and parallel data, scanner motor and
	ECC enabled.  If successful, the routine returns
	PASS.
*/

int dc_init(rcdr)
int    rcdr;        /* Input or output recorder */
{
    char    resp[LENGTH];

    /* First try to identify the DCRSi */
    if (dc_identify(rcdr,resp) == PASS)  /* Response received */
    {
	printf("Response from ID:  %s\n",resp);
	if (strcmp(Rcdr[rcdr].id,resp) != 0) /* Not the correct rcdr */
	{
	    printf("Could not connect to DCRSi %s\n",Rcdr[rcdr].id);
	    printf("DCRSi %s is connected.\n",resp);
	    return(FAIL);
        }
    }
    else
        printf("No response to IDENTIFY\n");

    /* Reseting recorder */
    if (dc_reset(rcdr,resp) == PASS)
    {
        printf("ID after reset:  %s\n",resp);
	if (strcmp(Rcdr[rcdr].id,resp) == 0) /* Correct rcdr */
	{
	    if (dc_en_motor(rcdr,ON) == PASS)   /* Motor enabled */
		if (dc_en_ecc(rcdr,ON)  == PASS)  /* Enable ECC */
		    if (dc_par_data(rcdr) == PASS) /* Use par data */
			if (dc_en_rcd(rcdr,OFF) == PASS) /* Disable rec */
	                    return(PASS);
	    return(FAIL);
        }
	else
	{
	    printf("Could not connect to DCRSi %s\n",Rcdr[rcdr].id);
	    printf("DCRSi %s is connected.\n",resp);
	}
    }
    return(FAIL);
}


/* dc_stopit(rcdr) -----------------------------------------------------
	This routine stops the given recorder from doing whatever it's
	doing.
*/

dc_stopit(rcdr)
	int rcdr;
{
	char rcvd[LENGTH];

	dc_send_cmd(rcdr,STOP);
	if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	{
	   /* set_alarm(3);
	   pause(); */
	   sleep(3);
	}
	dc_send_cmd(rcdr,DIS_MOTOR);
	if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	{
	   /* set_alarm(3);
	   pause(); */
	   sleep(3);
	}
	dc_send_cmd(rcdr,UNLOAD);
	if (dc_get_prompt(rcdr,5,rcvd,NULL) == FAIL)
	{
	   /* set_alarm(3);
	   pause(); */
	   sleep(3);
	}
	/*set_alarm(0);*/
}




/****************************************************************************
 The following contains the routines for the SONY tape recorder.
*****************************************************************************/


/*
  Command and control for sony recorders
*/


int string_to_bytes(char *hexstrings, unsigned char *buf){
  int bytes=0;
  char *hex_byte;
  char *dup_string=strdup(hexstrings);  /* need duplicate because strtok writes to string */
  char *h_string=dup_string;
  while(hex_byte=strtok(h_string," ")){
    h_string=NULL;
    buf[bytes++]=strtol(hex_byte,(char **)NULL,16);
  }
  free(dup_string);
  return bytes;
}/*string_to_bytes*/


/* compute sony check sum and copy cmd to output buffer */
int sony_cksum(unsigned char *cmd, int cmd_bytes){
  int check_sum=0;
  int i;
  
  for(i=0;i<cmd_bytes;i++){
    check_sum+=(unsigned char)cmd[i];
  }
  return check_sum & 0x00ff ;
} /*sony_cksum*/



int sony_send_cmd(int rcdr, char *hex_cmd, int *blk_id) {

    unsigned char cmd_buf[260];
    int  cmd_bytes;
    int i;
    
    cmd_bytes = string_to_bytes(hex_cmd, cmd_buf);

    if (blk_id != NULL) {
      cmd_buf[2] = (0x03);
      cmd_buf[3]=(*blk_id&0xff);
      cmd_buf[4]=(*blk_id>>8)&0xff;
      cmd_buf[5]=(*blk_id>>16)&0xff;
      cmd_bytes += 4;
    }

    cmd_buf[cmd_bytes] = sony_cksum(cmd_buf, cmd_bytes);

    if (vbose){
      printf("SONY command:");
      for (i=0; i<cmd_bytes+1; i++) 
        printf(" %02x", 0xff&(int)cmd_buf[i]);
      printf("\n");
    }

    if (write(Rcdr[rcdr].outfile, cmd_buf, cmd_bytes+1) < 0) {
     
       printf("Unable to transmit SONY command:");
       for (i=0; i<cmd_bytes+1; i++)
        printf(" %x", cmd_buf[i]);
       printf("\n");
       return(FAIL);
    }
 sleep(5);
    
    return(PASS);
}/*sony_send_cmd*/



/* sony_comm_init(rcdr)------------------------------------
	This routine sets up the tty terminal for
	communication with the SONY.  The routine
	assigns the file descriptor number to the
	variable in the rcdr structure.  It returns
	PASS if successful.
*/

int sony_comm_init(rcdr)
int    rcdr;          /* Input or output recorder code */
{
    char    dcrfile[20];
    struct termio new_setup;
  


    sprintf(dcrfile,"/dev/tty%2.2d",Rcdr[rcdr].term);

/*
    printf("Attempting to open/read-only %s\n",dcrfile);
*/

/*
      printf("before :Rcdr.infile %d\n",Rcdr[rcdr].infile);
*/
    if (Rcdr[rcdr].infile < 3) {

/*
      Rcdr[rcdr].infile = open(dcrfile,O_RDONLY);
*/

      Rcdr[rcdr].infile = open(dcrfile,O_RDWR);

/*
      printf("after : Rcdr.infile %d\n",Rcdr[rcdr].infile);
*/


/* new code starts here */

/*
      ioctl (Rcdr[rcdr].infile, TCGETA, &new_setup); 
      if ( !strcmp(host,"newasp") ) 
                new_setup.c_cflag |= PARENB; 
      new_setup.c_cflag &= ~PARODD; 
      ioctl (Rcdr[rcdr].infile, TCSETA, &new_setup); 
*/

      if (Rcdr[rcdr].infile < 0)
      {
	perror("sony_comm_init:  Unable to Open for READONLY\n");
	return(FAIL);
      }

/*
      Rcdr[rcdr].outfile = open(dcrfile,O_WRONLY);
*/

      Rcdr[rcdr].outfile = Rcdr[rcdr].infile;


/*
printf("Rcdr.outfile %d\n",Rcdr[rcdr].outfile);
*/

      if (Rcdr[rcdr].outfile < 0)
      {
	perror("sony_comm_init:  Unable to Open for WRITEONLY\n");
	return(FAIL);
      }
    /* Initialize port */
      if (open_sony(rcdr,Rcdr[rcdr].infile) != PASS) {
	printf("FAIL in open_sony\n");
	return(FAIL);
      }
      sony_clr_stat(rcdr);
    }
    else {
      printf("SONY for %d already opened on channel %d.\n",
	      rcdr, Rcdr[rcdr].infile);
    }
    return(PASS);    /* It'll work */
} /*sony_comm_init*/



/* sony_comm_close(rcdr)-----------------------------------
	This routine closes communication with the
	SONY.
*/

void sony_comm_close(rcdr)
int    rcdr;          /* Input or output recorder code */
{
  if(Rcdr[rcdr].infile < 3) {
    printf("SONY %d not opened to any channel\n",rcdr);
  }
  else {
    close_sony(rcdr);
    close(Rcdr[rcdr].infile);
    close(Rcdr[rcdr].outfile);
    Rcdr[rcdr].infile = 0;
  }
}/*sony_comm_close*/




/* sony_clr_stat(rcdr)----------------------------------------
	This routine clears all the status variables and
	is called before each command.
*/

sony_clr_stat(rcdr)
int    rcdr;
{
    *Aux_Stat[rcdr] = NULL;
    *Cmd_Stat[rcdr] = NULL;
    *Id[rcdr] = NULL;
    *Cur_Addr[rcdr] = NULL;
    *Beg_Addr[rcdr] = NULL;
    *End_Addr[rcdr] = NULL;
    Feet[rcdr] = -1;
    Cur_Err[rcdr] = -1;
} /*sony_clr_stat*/



/* int sony_get_resp(rcdr, expect_str, return_str, data_len, timeout)---
This routine collects responses from SONY. If  a period of 'time' passes
without a response, it'll return FAIL.
"expect_str" is the string pointer which point to the string which is
expected to be respopnsed. "data_len"  is the data length of return
string. "return_str" is the buffer location which contain the exact
string responsed.   
*/


int sony_get_resp(rcdr, expect_str, return_str, data_len, timeout) 
  int  rcdr;
  char *expect_str;
  unsigned char  *return_str;
  int  *data_len;
  int  timeout;
{
  int str_len;
 
  int expect_bytes;
  unsigned char expect_buf[260];

  int i;
  expect_bytes = string_to_bytes(expect_str, expect_buf);

/*
if (vbose){
      printf("expect str");
      for (i=0; i<expect_bytes; i++)
        printf(" %02x", 0xff&(int)expect_buf[i]);
      printf("\n");
    }
*/

   str_len = 0;
   *data_len = 0;

   if ((sony_wait_for_str(rcdr, expect_buf, return_str, &str_len, timeout)) == FAIL){
     printf("sony_get_resp: FAIL to get response.\n");
     return(FAIL);
   }
   if (str_len > 3) {  /*with data*/
     *data_len = return_str[2] & 0xff;
   }
   
   return(PASS);

}/* sony_get_resp */


int sony_init(int rcdr)
{
 unsigned char return_str[260];
 int data_len, timeout;

 data_len =0;
 timeout = 5; /*5 seconds*/

 if (sony_send_cmd(rcdr, V_STOP, NULL) == FAIL) {
   printf("sony_init: can not get V_STOP\n");
   return(FAIL);
 }

 if (sony_get_resp(rcdr, V_STOP_ACK, return_str, &data_len, timeout) == FAIL){
    printf(" Cann't get V_STOP_ACK\n");
    return(FAIL);
 }

 if (sony_send_cmd(rcdr, VRB_init, NULL) == FAIL) {
   printf("sony_init: can not get VRB_initialize\n");
   return(FAIL);
 }

 if (sony_get_resp(rcdr, VRB_init_ACK, return_str, &data_len, timeout) == FAIL){
    printf(" Cann't get VRB initialize ACK\n");
    return(FAIL);
 }


 if (sony_send_cmd(rcdr, "48 13 04 ff 00 83 00", NULL) == FAIL) {
   printf("sony_init: can not send V_data_mode_set\n");
   return(FAIL);
 }

 if (sony_get_resp(rcdr, "00 02", return_str, &data_len, timeout) == FAIL){
    printf(" Cann't get V_data_mode_set_ACK\n");
    return(FAIL);
 }

/*
 if (sony_send_cmd(rcdr, V_data_mode_sense, NULL) == FAIL) {
   printf("sony_init: can not send V_data_mode_sense\n");
   return(FAIL);
 }

 if (sony_get_resp(rcdr, V_data_mode_sense_ACK, return_str, &data_len, timeout) == FAIL){
    printf(" Cann't get V_data_mode_sense_ACK\n");
    return(FAIL);
 }
*/

 return (PASS);

}/* sony_init*/


int sony_identify(int rcdr)
{
 unsigned char return_str[260];
 int data_len, timeout;

 data_len =0;
 timeout = 5; /*5 seconds*/



 if (sony_send_cmd(rcdr, VRB_machine_sense, NULL) == FAIL) {
   printf("sony_identify: can not get VRB_machine_sense\n");
   return(FAIL);
 }
 
 if (sony_get_resp(rcdr, VRB_machine_sense_ACK, return_str, &data_len, timeout) == PASS) {

    return_str[data_len] = '\0';
    printf("VRB machine sense\n");
    printf("VRB machine type:%s\n", return_str);

 }
 else {
    printf(" Can not get VRB machine sense\n");
    return(FAIL);
 }


 return(PASS);

/*
 if (sony_send_cmd(rcdr, MACHINE_TYPE_SENSE, NULL) == FAIL) {
   printf("sony_identify: can not get machine type sense\n");
   return(FAIL);
 }
 
 if (sony_get_resp(rcdr, MACHINE_TYPE_SENSE_ACK, return_str, &data_len, timeout) == PASS) {
    return_str[data_len] = '\0';
    printf("SONY machine type sense\n");
    printf("SONY machine type:%s\n", return_str);
 }
 else {
    printf(" Can not get SONY machine sense\n");
    return(FAIL);
 }

 return(PASS);
*/

}/*sony_identify*/

int sony_setup(int rcdr)
{
 unsigned char return_str[260];
 int data_len, timeout;

 data_len =0;
 timeout = 5; /*5 seconds*/


 if (sony_init(rcdr) == FAIL) {
   printf("sony_setup: can not get VRB_initialize\n");
   return(FAIL);
 }

/*
 if (sony_identify(rcdr) == FAIL) {
   printf("sony_setup: can not do sony_identify\n");
   return(FAIL);
 }
*/

 return(PASS);

}/*sony_setup*/


/* sony_stop(rcdr)--------------------------------------------------
	This routine stops record or play and returns PASS
	if the prompt is received.  sony_get_resp is called to
	receive responses from the SONY.  sony_clr_stat is called to
        clear status.
*/

int sony_stop(rcdr)
int    rcdr;
{
    unsigned char return_str[260];
    int data_len, timeout;
    int success, try;

    data_len = 0;
    timeout = 10; /*20 seconds*/
    success =0;
    try = 0;

while ((success == 0) && (try < timeout)) {
 if (sony_send_cmd(rcdr, V_STOP, NULL) == FAIL) {
   printf("sony_stop: cann't send command V_STOP\n");
   return(FAIL);
 }

 if (sony_get_resp(rcdr, V_STOP_ACK, return_str, &data_len, timeout)
== FAIL){
    printf(" Can not get V_STOP ACK\n");
    sleep(2);
 }
 else success =1;

 try += 1;

}/*while*/

if ((try == timeout) || (success == 0)) return(FAIL);

    dc_clr_stat(rcdr);          /* Clear status */

    return(PASS);

}/*sony_stop*/




int sony_playback (rcdr, start_block, end_block)
	int     rcdr, start_block, end_block;
{
	int     i;

        unsigned char return_str[260];
        int data_len, timeout, complete, playblock;

        data_len =0;
        timeout = 5; /*5 seconds*/

    /* Get the recorder ready to play  */


    /* build and send play command */

        if (sony_send_cmd(rcdr, V_preroll_ID, &start_block) == FAIL) {
           printf("sony_playback: cann't send command V_preroll_ID\n");
           return(FAIL);
        }
        printf ("SONY finding starting address (%d). . .\n\n", start_block);

        if (sony_get_resp(rcdr, V_preroll_ID_ACK, return_str, &data_len, timeout) == FAIL){
            printf("sony_playback: Cann't get V_preroll_ID ACK\n");
            return(FAIL);
        }

    /* Wait until the SONY has found the address.  Do this by checking
       VRB_status_sense_ACK. Timeout after 400 seconds */

        complete = 0;
        data_len =0;
        for (i=0; i<400; i++) {
          if (sony_send_cmd(rcdr, VRB_status_sense, NULL) == FAIL) {
            printf("sony_playback: cann't get VRB_status_sense\n");
            return(FAIL);
          }
          if (sony_get_resp(rcdr, VRB_status_sense_ACK, return_str, &data_len, timeout) == PASS) {

            if ((return_str[3] == 0x10) && ((return_str[4] &= 0x30) == 0x30)){
/*
printf("  return :%02x\n", 0xff&(int)return_str[0]);
printf("  return :%02x\n", 0xff&(int)return_str[1]);
printf("  return :%02x\n", 0xff&(int)return_str[2]);
printf("  return :%02x\n", 0xff&(int)return_str[3]);
printf("  return :%02x\n", 0xff&(int)return_str[4]);
printf("  return :%02x\n", 0xff&(int)return_str[5]);
*/
              printf("V_preroll_ID completed\n");
              complete =1;
              break;
            }
            if (((return_str[7] &= 0x80) == 0x80) && (return_str[3] == 0x10)){
              printf("End of tape \n");
              printf("sony_playback: cann't search ID\n");
              sony_stop(rcdr);
              return(FAIL);
            }
              
          }
          sleep(1);
        }

  
        if (complete == 0){
           printf("Time out \n");
           printf("sony_playback: Cann't get VRB machine sense\n");
           sony_stop(rcdr);
           return(FAIL);
        }

printf("tape is playing back, please wait. \n");

/* Send out playback command */
        if (sony_send_cmd(rcdr, V_FWD, NULL) == FAIL) {
           printf("sony_playback: cann't send command V_FWD\n");
           return(FAIL);
        }
        if (sony_get_resp(rcdr, V_FWD_ACK, return_str, &data_len, timeout) == FAIL){
            printf("sony_playback: Cann't get V_FWD ACK\n");
            return(FAIL);
        }


    /* Confirms that the present status is V-FWD */

        complete = 0;
        data_len =0;
        for (i=0; i<400; i++) {
          if (sony_send_cmd(rcdr, VRB_status_sense, NULL) == FAIL) {
            printf("sony_playback: cann't get VRB_status_sense\n");
            return(FAIL);
          }
          if (sony_get_resp(rcdr, VRB_status_sense_ACK, return_str, &data_len, timeout) == PASS) {

            if ((return_str[3] == 0x40) && ((return_str[4] &= 0x20) == 0x20)){
              complete =1;
/*
printf("  return :%02x\n", 0xff&(int)return_str[0]);
printf("  return :%02x\n", 0xff&(int)return_str[1]);
printf("  return :%02x\n", 0xff&(int)return_str[2]);
printf("  return :%02x\n", 0xff&(int)return_str[3]);
printf("  return :%02x\n", 0xff&(int)return_str[4]);
printf("  return :%02x\n", 0xff&(int)return_str[5]);
*/
              printf("V_FWD is in ready mode\n");
              break;
            }
          }
          sleep(1);
        }
           

        if (complete == 0){
           printf("Time out \n");
           printf("sony_playback: Cann't get VRB machine sense\n");
printf("  return :%02x\n", 0xff&(int)return_str[0]);
printf("  return :%02x\n", 0xff&(int)return_str[1]);
printf("  return :%02x\n", 0xff&(int)return_str[2]);
printf("  return :%02x\n", 0xff&(int)return_str[3]);
printf("  return :%02x\n", 0xff&(int)return_str[4]);
printf("  return :%02x\n", 0xff&(int)return_str[5]);
           sony_stop(rcdr);
           return(FAIL);
        }


/* Send out STOP_ID command */
/*
         end_block = start_block+10000;
*/
        if (sony_send_cmd(rcdr, V_STOP_ID, &end_block) == FAIL) {
           printf("sony_playback: cann't send command V_STOP\n");
           sony_stop(rcdr);
           return(FAIL);
        }
       if (sony_get_resp(rcdr, V_STOP_ID_ACK, return_str, &data_len, timeout) ==
 FAIL){
            printf("sony_playblock: Cann't get V_STOP_ID ACK\n");
            sony_stop(rcdr);
            return(FAIL);
        }


        return (PASS);
}/*sony_playback*/

int sony_get_addr(rcdr)
  int rcdr;
{
   unsigned char return_str[260];
   int data_len, timeout;
   int blk_id, blk_id_1, blk_id_2, blk_id_3;

   data_len = 0;
   timeout = 5;

/*
   if (sony_send_cmd(rcdr, "20 03", NULL) == FAIL) {
     printf("sony_get_addr: can not send Stop\n");
     return(FAIL);
   }

   if (sony_get_resp(rcdr, "00 02", return_str, &data_len, timeout) == FAIL){
    printf(" Cann't get Stop ACK\n");
    return(FAIL);
   }
*/
   sony_stop(rcdr);

   if (sony_send_cmd(rcdr, ID_COUNTER_SENSE, NULL) == FAIL) {
           printf("sony_get_addr: cann't send command ID_COUNTER_SENSE\n");
           return(FAIL);
   }

   if (sony_get_resp(rcdr, ID_COUNTER_SENSE_ACK, return_str, &data_len, timeout) == FAIL){
           printf("sony_get_addr: Cann't get ID_COUNTER_SENSE_ACK\n");
           return(FAIL);
   }
   blk_id_1 = return_str[3];
   blk_id_2 = return_str[4];
   blk_id_3 = return_str[5];
   blk_id = (blk_id_3 << 16) + (blk_id_2 << 8) + blk_id_1;

printf("sony_get_addr: blkno = %d\n", blk_id);


   return (blk_id);
   

} /* sony_get_addr */


int sony_playback_REV (rcdr, start_block, end_block)
	int     rcdr, start_block, end_block;
{
	int     i;

        unsigned char return_str[260];
        int data_len, timeout, complete, playblock;

        data_len =0;
        timeout = 5; /*5 seconds*/

    /* Get the recorder ready to play  */

    /* build and send play command */

        if (sony_send_cmd(rcdr, V_preroll_ID, &start_block) == FAIL) {
           printf("sony_playback_REV: cann't send command V_preroll_ID\n");
           return(FAIL);
        }
        printf ("SONY finding starting address (%d). . .\n\n", start_block);

        if (sony_get_resp(rcdr, V_preroll_ID_ACK, return_str, &data_len, timeout) == FAIL){
            printf("sony_playback_REV: Cann't get V_preroll_ID ACK\n");
            return(FAIL);
        }

    /* Wait until the SONY has found the address.  Do this by checking
       VRB_status_sense_ACK. Timeout after 400 seconds */

        complete = 0;
        data_len =0;
        for (i=0; i<400; i++) {
          if (sony_send_cmd(rcdr, VRB_status_sense, NULL) == FAIL) {
            printf("sony_playback_REV: cann't get VRB_status_sense\n");
            return(FAIL);
          }
          if (sony_get_resp(rcdr, VRB_status_sense_ACK, return_str, &data_len, timeout) == PASS) {

            if ((return_str[3] == 0x10) && (return_str[4] == 0x30)){
/*
printf("  return :%02x\n", 0xff&(int)return_str[0]);
printf("  return :%02x\n", 0xff&(int)return_str[1]);
printf("  return :%02x\n", 0xff&(int)return_str[2]);
printf("  return :%02x\n", 0xff&(int)return_str[3]);
printf("  return :%02x\n", 0xff&(int)return_str[4]);
printf("  return :%02x\n", 0xff&(int)return_str[5]);
*/
              printf("V_preroll_ID completed\n");
              complete =1;
              break;
            }
            if (((return_str[7] &= 0x80) == 0x80) && (return_str[3] == 0x10)){
              printf("End of tape \n");
              printf("sony_playback_REV: cann't search ID\n");
              sony_stop(rcdr);
              return(FAIL);
            }
              
          }
          sleep(1);
        }

  
        if (complete == 0){
           printf("Time out \n");
           printf("sony_playback_REV: Cann't get VRB machine sense\n");
           sony_stop(rcdr);
           return(FAIL);
        }

printf("tape is REV playing back, please wait. \n");

/* Send out playback command */
        if (sony_send_cmd(rcdr, V_REV, NULL) == FAIL) {
           printf("sony_playback_REV: cann't send command V_REV\n");
           return(FAIL);
        }
        if (sony_get_resp(rcdr, V_REV_ACK, return_str, &data_len, timeout) == FAIL){
            printf("sony_playback_REV: Cann't get V_REV ACK\n");
            return(FAIL);
        }


    /* Confirms that the present status is V-REV */

        complete = 0;
        data_len =0;
        for (i=0; i<400; i++) {
          if (sony_send_cmd(rcdr, VRB_status_sense, NULL) == FAIL) {
            printf("sony_playback_REV: cann't get VRB_status_sense\n");
            return(FAIL);
          }
          if (sony_get_resp(rcdr, VRB_status_sense_ACK, return_str, &data_len, timeout) == PASS) {

            if ((return_str[3] == 0x20) && (return_str[4] == 0x20)){
              complete =1;
/*
printf("  return :%02x\n", 0xff&(int)return_str[0]);
printf("  return :%02x\n", 0xff&(int)return_str[1]);
printf("  return :%02x\n", 0xff&(int)return_str[2]);
printf("  return :%02x\n", 0xff&(int)return_str[3]);
printf("  return :%02x\n", 0xff&(int)return_str[4]);
printf("  return :%02x\n", 0xff&(int)return_str[5]);
*/
              printf("V_REV is in ready mode\n");
              break;
            }
          }
          sleep(1);
        }
           

        if (complete == 0){
           printf("Time out \n");
           printf("sony_playback_REV: Cann't get VRB machine sense\n");
printf("  return :%02x\n", 0xff&(int)return_str[0]);
printf("  return :%02x\n", 0xff&(int)return_str[1]);
printf("  return :%02x\n", 0xff&(int)return_str[2]);
printf("  return :%02x\n", 0xff&(int)return_str[3]);
printf("  return :%02x\n", 0xff&(int)return_str[4]);
printf("  return :%02x\n", 0xff&(int)return_str[5]);
           sony_stop(rcdr);
           return(FAIL);
        }


/* Send out STOP_ID command */

/*
        end_block = start_block+10000;
*/
        if (sony_send_cmd(rcdr, V_STOP_ID, &end_block) == FAIL) {
           printf("sony_playback_REV: cann't send command V_STOP\n");
           sony_stop(rcdr);
           return(FAIL);
        }
       if (sony_get_resp(rcdr, V_STOP_ID_ACK, return_str, &data_len, timeout) ==
 FAIL){
            printf("sony_playblock_REV: Cann't get V_STOP_ID ACK\n");
            sony_stop(rcdr);
            return(FAIL);
        }

        return (PASS);

}/*sony_playback_REV*/
