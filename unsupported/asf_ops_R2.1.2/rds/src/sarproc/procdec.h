#ifndef _PROCDEC_H
#define _PROCDEC_H

static char sccsid_procdec_h[] = 
    "@(#)procdec.h	1.3 96/04/09 20:40:51";

/* procdec.h - ASP Processing Software Declaration File
*/


#define IDLEN 30       /* Length of AOS proc request ID */
#define TYPELEN 4      /* Length of AOS proc request record type */
#define ASPIDLEN 15    /* Length of ASP proc request ID plus NULL */
#define TAPEIDLEN 7    /* Length of tape id field */
#define ON  1
#define OFF 0
#define FMODE 0744      /* File access mode (in octal) */
#define FMODE_MASK 022  /* File access mask, owner write privilege */
			/*  only, others may read */

#ifndef PASS
#define PASS   0
#define FAIL  -1
#define ABORT -2        /* System error needing expert attention */
#endif

        /* Interrupt identification for interrupt handling routines */

#define PATHLEN 80      /* Max length of file names including path */
#define FIELDLEN 80     /* Field length for character strings */

        /* Names for required files */
#define TTDL "ttdl.asp" /* ASP version of Things To Do List */
#define IN 0            /* Input and output DCRS and keyboard codes */
#define OUT 1           /*  for use with DCRS operating, diagnostic */
#define KEY_BD 2        /*  and interrupt handling routines */

	/* DCRSi Definitions */
#define STOP_STAT 0     /* Command status */
#define UNLOAD_STAT 1
#define PLAY_STAT 2
#define RECORD_STAT 3
#define FASTFOR_STAT 4
#define REWIND_STAT 5
#define SEARCH_STAT 6

#define PAR_IO 0        /* IO selection */
#define SER_IO 1
#define CUSTOM_IO 2

        /* Satellite Definitions */
#define E_ERS "E1"
#define J_ERS "J1"
#define RADARSAT "RS"
#define E_CODE 0xfaf320     /* E_ERS sync code */
#define J_CODE 0x00000000   /* J_ERS sync code ??? */
#define J_CODE1 0xaa99aa5a  /* J_ERS interleaved sync code - part 1 */
#define J_CODE2 0x5a655550  /* J_ERS interleaved sync code - part 2 */
#define J_INV_CODE1 0x556655a5  /* J_ERS inv sync code - part 1 */
#define J_INV_CODE2 0xa59aaaaf  /* J_ERS inv sync code - part 2 */
#define R_CODE 0xfaf320     /* RADARSAT sync code */
#define E_CODE_BITS 24      /* Number of bits in sync code */
#define J_CODE_BITS 30      /* Number of bits in sync code */
#define R_CODE_BITS 24      /* Number of bits in sync code */
#define E_CODE_SEP 256      /* Code repetition */
#define J_CODE_SEP 000      /* Code repetition ??? */
#define R_CODE_SEP 256      /* Code repetition */


typedef struct {    /* Weekly Operations Schedule header record */
	char    id[IDLEN];
	char    type[4];
	char    year[5];
	char    time[17];
	char    records[9];
	char    fill[100];
} WOS_HDR;

typedef struct {    /* WOS activity record */
	char    take_id[14];
	char    act_id[7];
	char    start_yr[5];
	char    start_time[17];
	char    end_yr[5];
	char    end_time[17];
	char    trans_id[3];
	char    site[33];
	char    fill[37];
} WOS_ACT;


/* DCRSi status structures */
/* 1 = ON or TRUE, 0 = OFF or FALSE */

typedef struct { /* Status constantly being updated */
	int    data_rdy;   /* Data ready status */
	int    lub_rdy;    /* Longitudinal user block ready */
	int    lub_rcd;    /* Longitudinal user block recorded */
	int    eot;        /* End of tape */
	int    bot;        /* Beginning of tape */
	int    trgt_cmd;   /* Target command mode */
	int    cur_cmd;    /* Current command mode */
} STAT_RPT;  /* Status report */

typedef struct { /* Initialization mode, changes infrequently */
	int    rcd_en;     /* Record enabled */
	int    fifo_fault; /* FIFO buffer test fault */
	int    io_sel;     /* Data IO selection */
	int    buf_norm;   /* Buffer mode normal */
	int    scanner_en; /* Scanner enabled */
	int    ecc_en;     /* ECC decoder enabled */
	int    ecc_pres;   /* ECC decoder present */
	int    ser_io_addr;   /* Serial interface address */
	int    maint_mode; /* Maintenance mode select */
	int    RS_sel;     /* RS-422/232C select */
	int    baud_rate;  /* Baud rate selected */
	int    even_odd_par;  /* On = even parity */
	int    par_en;     /* Parity enable */
	int    unit_code;  /* 7 or 8 unit code */
} MODE_RPT;  /* Mode report */

typedef struct {
	int    cur_addr;
	int    beg_addr;
	int    end_addr;
	int    feet;
} LOC_RPT;   /* Tape location report */

#endif /* ! _PROCDEC_H */
