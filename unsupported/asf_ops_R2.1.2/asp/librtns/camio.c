/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  CAM_IO routines  */

/* Include files needed for these routines. */
 
#include <stdio.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/uio.h>
#include <strings.h>
#include <ctype.h>
#include <math.h>

#include <cam_defs.h>

UAGT_CAM_CCB ua_ccb;		/* uagt structure */ 
CCB_SCSIIO ccb;			/* CCB for SCSI SEND/RECEIVE operation */ 
UAGT_CAM_CCB ua_ccb_rd;		/* uagt structure */ 
CCB_SCSIIO ccb_rd;		/* CCB for READ operation */ 
UAGT_CAM_CCB ua_ccb_rs;		/* uagt structure */ 
CCB_RESETDEV ccb_rs;		/* CCB for Reset/SIM release operation */ 

#define SEG_SIZE 4096*1024	/*size of single OUT board buffer*/
#define MAX_DATA 2048*1024
#define SEND_OPCODE 0x0a
#define RECEIVE_OPCODE 0x08
#define READ_10_OPCODE 0x28
#define SENSE_LEN 128		/* max sense length from device */ 
u_char sense[ SENSE_LEN ]; 

int cam_fd;			/* file descriptor for the CAM open */ 
 
extern u_char bus_id, targ_id, targ_lun, j_c;	/* SCSI nexus information */ 

extern void print_ccb_status();
extern void clear_mem(); 
extern void swap_short_store();
extern void swap_long_store();
 
/*  init_cam()---------------------------------------------
	routine to initialize CAM structures and open the driver to
	attach to the device, and send a device reset signal
*/
int init_cam()
{
/* Open the User Agent driver and report any errors. */
 
    if ((cam_fd = open("/dev/cam", O_RDWR, 0)) < 0 ){
	perror("Error on CAM UAgt Open");
	exit(1);
    }
 
/* Preset some stuff in the CAM header for the SEND/RECEIVE CCBs */
 
    ccb.cam_ch.my_addr = (struct ccb_header *)&ccb;	/* "Its" address */ 
    ccb.cam_ch.cam_func_code = XPT_SCSI_IO;		/* the opcode */
    ccb.cam_ch.cam_ccb_len = sizeof(CCB_SCSIIO);	/* Size of SCSIIO CCB */
    ccb.cam_ch.cam_path_id = bus_id;			/* selected bus */ 
    ccb.cam_ch.cam_target_id = targ_id;			/* selected target */
    ccb.cam_ch.cam_target_lun = targ_lun;		/* selected lun */
    ccb.cam_sense_ptr = &sense[0];		/* Autosense data area */
    ccb.cam_sense_len = SENSE_LEN;		/* Autosense data length */
 
/* Set up the fields for the User Agent Ioctl call. */
 
    ua_ccb.uagt_ccb = (CCB_HEADER *)&ccb;	/* where the CCB is */ 
    ua_ccb.uagt_ccblen = sizeof(CCB_SCSIIO);	/* Size of SCSIIO CCB */ 
    ua_ccb.uagt_snsbuf = &sense[0];		/* Autosense data area */ 
    ua_ccb.uagt_snslen = SENSE_LEN;		/* Autosense data length */
    ua_ccb.uagt_cdb    = (CDB_UN *)NULL;	/* CDB is in the CCB */ 
    ua_ccb.uagt_cdblen = 0;			/* CDB is in the CCB */

/* Preset some stuff in the CAM header for the READ CCBs */
 
    ccb_rd.cam_ch.my_addr = (struct ccb_header *)&ccb_rd;
    ccb_rd.cam_ch.cam_ccb_len = sizeof(CCB_SCSIIO);/* Size of SCSIIO CCB */
    ccb_rd.cam_ch.cam_path_id = bus_id;		/* selected bus */
    ccb_rd.cam_ch.cam_target_id = targ_id;	/* selected target */
    ccb_rd.cam_ch.cam_target_lun = targ_lun;	/* selected lun */
    ccb_rd.cam_ch.cam_flags = CAM_DIR_IN;	/* data direction */
    ccb_rd.cam_ch.cam_func_code = XPT_SCSI_IO;	/* the opcode */
    ccb_rd.cam_sense_ptr = &sense[0];		/* Autosense data */
    ccb_rd.cam_sense_len = SENSE_LEN;		/* Autosense buffer length */
 
/* Set up the fields for the User Agent Ioctl call. */
 
    ua_ccb_rd.uagt_ccb = (CCB_HEADER *)&ccb_rd;
    ua_ccb_rd.uagt_ccblen = sizeof(CCB_SCSIIO);	/* Size of SCSIIO CCB */ 
    ua_ccb_rd.uagt_snsbuf = &sense[0];		/* Autosense data */
    ua_ccb_rd.uagt_snslen = SENSE_LEN;		/* Autosense buffer length */
    ua_ccb_rd.uagt_cdb    = (CDB_UN *)NULL;	/* CDB is in the CCB */
    ua_ccb_rd.uagt_cdblen = 0;			/* CDB is in the CCB */
 
/* Set up the CAM header for the XPT_RESET_DEV function. */
 
    ccb_rs.cam_ch.my_addr = (struct ccb_header *)&ccb_rs;
    ccb_rs.cam_ch.cam_ccb_len = sizeof(CCB_RELSIM);	/* a RELSIM CCB */
    ccb_rs.cam_ch.cam_path_id = bus_id;		/* selected bus */
    ccb_rs.cam_ch.cam_target_id = targ_id;	/* selected target */
    ccb_rs.cam_ch.cam_target_lun = targ_lun;	/* selected lun */
    ccb_rs.cam_ch.cam_flags = CAM_DIR_NONE;
 
/* Set up the fields for the User Agent Ioctl call. */
 
    ua_ccb_rs.uagt_ccb = (CCB_HEADER *)&ccb_rs;
    ua_ccb_rs.uagt_ccblen = sizeof(CCB_RELSIM);
    ua_ccb_rs.uagt_buffer = (u_char *)NULL;	/* no data */
    ua_ccb_rs.uagt_buflen = 0;
    ua_ccb_rs.uagt_snsbuf = (u_char *)NULL;	/* no sense buffer */
    ua_ccb_rs.uagt_snslen = 0;
    ua_ccb_rs.uagt_cdb    = (CDB_UN *)NULL;	/* CDB is in the CCB */
    ua_ccb_rs.uagt_cdblen = 0;			/* CDB is in the CCB */
 
/*
    cam_reset();
    sleep( 2 );
*/
    return( 1 );
}

/*  cam_reset()----------------------------------
    Routine to reset a jammed device on a SCSI bus.
*/
int cam_reset()
{
/* Set up the CCB for an XPT_REL_SIMQ request. */
 
    ccb_rs.cam_ch.cam_func_code = XPT_REL_SIMQ;		/* the opcode */

/*  Try to release the SIM queue  */

    if( ioctl( cam_fd, UAGT_CAM_IO, (caddr_t) &ua_ccb_rs) < 0 ){
	printf("Error on CAM UAgt release SIM Queue ioctl");
	close( cam_fd );
	exit( -1 );
    }
    print_ccb_status("CAM UAgt Release SIM queue IOCTL",
	&(ccb_rs.cam_ch) );	/*  report the errors  */

/* Set up the CCB for an XPT_RESET_DEV request. */

    ccb_rs.cam_ch.cam_func_code = XPT_RESET_DEV;	/* the opcode */
 
/*  Try to reset the device  */

    if( ioctl( cam_fd, UAGT_CAM_IO, (caddr_t) &ua_ccb_rs) < 0 ){
	printf("Error on CAM UAgt reset device ioctl");
	close( cam_fd );
	exit( -1 );
    }
    print_ccb_status("CAM UAgt Reset Device IOCTL",
	&(ccb_rs.cam_ch) );	/*  report the errors  */
    return(1);
}

/*  sb( adr, bytes, regs )---------------------------------------------
	routine to send bytes in regs through SCSI CAM driver at adr.
	the CCB and UAGT control blocks must be already set up with
	the appropriate information such as SCSI bus, target, and LUN
*/
int sb( adr, bytes, regs )
int adr, bytes;
unsigned char *regs;
{
/*  define CDB for SCSI SEND (6 byte) command  */

    typedef struct {
	u_char opcode;		/*  this is 0x08  */
	u_char		:5,	/*  Reserved  */
		lun	:3;	/*  LUN  */
	u_char h_length;	/*  MSB of length */
	u_char m_length;	/*  mid byte of length */
	u_char l_length;	/*  LSB of length */
	u_char control;		/*  control byte  */
    } SEND_CDB;
    SEND_CDB *send;		/* define the SEND CDB  */

printf("SB address=%d, count=%d)\n", adr, bytes);
/*
printf("SCSI nexus set to: bus %d, target %d, LUN %d\n\n",
			bus_id, targ_id, targ_lun); 
*/

/* The needed CAM flags are: CAM_DIR_OUT - The data will go to the target. */
 
    ccb.cam_ch.cam_flags = CAM_DIR_OUT;
 
/* Set up the rest of the CCB for the SEND command. */
 
    ccb.cam_data_ptr = (u_char *)regs;		/* where the data is */ 
    ccb.cam_dxfer_len = bytes;			/* how much data */
    ccb.cam_timeout = 10;			/* use timeout of 10Sec */
    ccb.cam_cdb_len = sizeof( SEND_CDB );	/* how many bytes for send */ 
 
/* Use a local pointer to access the fields in the SEND CDB. */
 
    send = (SEND_CDB *)&ccb.cam_cdb_io.cam_cdb_bytes[0]; 
    clear_mem(send,sizeof(SEND_CDB));		/* clear all bits in CDB */ 
    send->opcode = SEND_OPCODE;			/* SEND command */
    send->lun = targ_lun;			/* lun on target */
    send->h_length = (u_char) (bytes>>16) & 255;/* MSB of length */
    send->m_length = (u_char) (bytes>>8) & 255;	/* mid byte of length */
    send->l_length = (u_char) bytes & 255;	/* LSB of length */
    send->control = (u_char) adr & 255;		/* read address */
 
/* Set up the fields for the User Agent Ioctl call. */
 
    ua_ccb.uagt_buffer = regs;			/* where the data is */ 
    ua_ccb.uagt_buflen = bytes;			/* how much data */ 
 
/* Send the CCB to the CAM subsystem via the User Agent driver, and report
   any errors. */
 
    if( ioctl(cam_fd, UAGT_CAM_IO, (caddr_t)&ua_ccb) < 0 ){
	perror("\nSB Error on CAM UAgt Ioctl to Send data line");
	return(-1);
    }
 
    if (ccb.cam_ch.cam_status != CAM_REQ_CMP){
	printf("SB \n");
	print_ccb_status("CAM UAgt send data line Ioctl error",
	  &(ccb.cam_ch) );			/* report the error values */
	printf("  cam_scsi_status = 0x%.2X\n", ccb.cam_scsi_status);
	return(-1);
    }
    return(1);
}

/*  rc( adr, bytes, regs )----------------------------------
    Routine to send the SCSI receive command to get a number of bytes.
*/
int rc( adr, bytes, regs )
int adr, bytes;
u_char *regs;
{
/*  define CDB for SCSI RECEIVE (6 byte) command  */

    typedef struct {
	u_char opcode;		/*  this is 0x08  */
	u_char		:5,	/*  Reserved  */
		lun	:3;	/*  LUN  */
	u_char h_length;	/*  MSB of length */
	u_char m_length;	/*  mid byte of length */
	u_char l_length;	/*  LSB of length */
	u_char control;		/*  control byte  */
    } RECEIVE_CDB;
    RECEIVE_CDB *receive;		/* define the RECEIVE CDB  */

/*
printf("RC address=%d, bytes=%d\n", adr, bytes);
printf("SCSI nexus set to: bus %d, target %d, LUN %d\n\n",
			bus_id, targ_id, targ_lun); 
*/

/* The needed CAM flags are: CAM_DIR_IN - The data will come from the target. */
 
    ccb.cam_ch.cam_flags = CAM_DIR_IN;
 
/* Set up the rest of the CCB for the RECEIVE command. */
 
    ccb.cam_data_ptr = (u_char *)regs;		/* where the data goes */ 
    ccb.cam_dxfer_len = bytes;			/* how much data */
    ccb.cam_timeout = 10;			/* use timeout of 10Sec */
    ccb.cam_cdb_len = sizeof( RECEIVE_CDB );	/* Size of RECEIVE CCB */ 

/* Use a local pointer to access the fields in the RECEIVE CDB. */
 
    receive = (RECEIVE_CDB *)&ccb.cam_cdb_io.cam_cdb_bytes[0]; 
    clear_mem(receive,sizeof(RECEIVE_CDB));	/* clear all bits in CDB */ 
    receive->opcode = RECEIVE_OPCODE;		/* RECEIVE command */
    receive->h_length = (u_char) (bytes>>16) & 255;/* MSB of length */
    receive->m_length = (u_char) (bytes>>8) & 255;/* mid byte of length */
    receive->l_length = (u_char) bytes & 255;	/* LSB of length */
    receive->control = (u_char) adr & 255;	/* read address */
 
/* Set up the fields for the User Agent Ioctl call. */
 
    ua_ccb.uagt_buffer = regs;			/* where the data goes */ 
    ua_ccb.uagt_buflen = bytes;			/* how much data */ 
 
/* Send the CCB to the CAM subsystem via the User Agent driver, and report
   any errors. */
 
    if( ioctl(cam_fd, UAGT_CAM_IO, (caddr_t)&ua_ccb) < 0 ){
	perror("\nRC Error on CAM UAgt Ioctl to Read data line");
	return(-1);
    }
 
/* If the CCB completed successfully then print out the data receive,
   if not report the error. */
 
    if (ccb.cam_ch.cam_status != CAM_REQ_CMP){
	printf("RC \n");
	print_ccb_status("CAM UAgt Read data line Ioctl",
	  &(ccb.cam_ch) );			/* report the error values */
	printf("RC cam_scsi_status = 0x%.2X\n", ccb.cam_scsi_status);
	return(-1);
    }
/*
    else {
	printf(" RC Data line received successfully:\n");
	for( i=0; i<bytes; i++ ) printf("%x ", regs[i] );
	printf("\n");
    }
*/
    return(1);
}

/*  rdb( lbn, count, buf, vf, over )----------------------------------
    routine to read data into buffer from SCSI device.  Each read will
    transfer a maximum of MAX_DATA bytes until all data has been transferred

    Inputs:

	lbn:  first logical block to start reading from
	count:  Number of blocks to read (512 byte blocks)
	buf:  buffer to read data into (must be at least as big as count*512)
	vf:  SCSI vendor flag options, set to 0 for normal operations.
		bit 0-1: 0,no FREEZEWR;1,freeze A;2,freezeb
		bit 2: WRITEONCE, not used
		bit 3: 1 means wait for START
		bit 4: 1 means last read
	over:  pointer to flag which will be set to true if buffer overflowed.
*/
int rdb( lbn, count, buf, vf, over )
int lbn, count, vf, *over;
u_char *buf;
{
/*  define CDB for SCSI READ (10 byte) command  */

    typedef struct {
	u_char opcode;		/*  this is 0x08  */
	u_char reladr :1,	/*  Relative address bit  (Wait For START)* */
		      :2,	/*  Reserved  */
		fua   :1,	/*  Force Unit Access  WRITEONCE*/
		dpo   :1,	/*  Defer Processing  RDLAST* */
		lun   :3;	/*  LUN  */
	u_char addr_3;		/*  Ignored  */
	u_char addr_2;		/*  Ignored  */
	u_char addr_1;		/*  MSB of block address  */
	u_char addr_0;		/*  LSB of block address  */
	u_char        :8;	/*  Reserved  */
	u_char cnt_h;		/*  Number of blocks to transfer  */
	u_char cnt_l;		/*  Number of blocks to transfer  */
	u_char link :1,		/*  Options byte  */
		flag :1,
		     :4,
		vendor :2;	/*  00 - no FREEZEWR
				    01 - freeze on A
				    10 - freeze on B		*/
    } READ_10_CDB;
    READ_10_CDB *read_10;

    u_char ReceiveData[1024];
    u_char  dummy[3];
    int i, n, seg;
 
    int total_bytes, size, blocks;	/* number of bytes to read  */

    blocks = MAX_DATA >> 9;		/*  blocks per transfer  */
    size = blocks << 9;			/*  bytes per transfer size  */

printf("RDB lbn=%d, count=%d, size=%d, BUS=%d TARGET=%d LUN=%d\n",
	lbn, count, size, ccb_rd.cam_ch.cam_path_id, 
	ccb_rd.cam_ch.cam_target_id, ccb_rd.cam_ch.cam_target_lun );
 
/* Set up the rest of the CCB for the READ command. */
 
    ccb_rd.cam_data_ptr = (u_char *)buf;	/* where the data goes */ 
    ccb_rd.cam_dxfer_len = size;			/* how much data */
    ccb_rd.cam_cdb_len = sizeof( READ_10_CDB );	/* bytes in read CDB */ 

/*  set the READ CDB specific stuff  */
 
    read_10 = (READ_10_CDB *)&ccb_rd.cam_cdb_io.cam_cdb_bytes[0];
    clear_mem(read_10,sizeof(READ_10_CDB));     /* clear all bits in CDB */
    read_10->opcode = READ_10_OPCODE;		/* READ command */
    read_10->fua=1;			/*deassert WRITEONCE* for safety */
    if( (vf & 8) !=0 ) read_10->reladr = 0;     /* Wait for START* */
    else read_10->reladr = 1;			/* or don't */
    if( (vf & 0x10) !=0 ) read_10->dpo = 0;     /* Last read,hope you know */
    else read_10->dpo = 1;			/* what or not you're doing */
    read_10->lun = targ_lun;			/* Target LUN */
    read_10->addr_3 = (u_char) ( (lbn>>24) & 0xff );	/* high address bits */
    read_10->addr_2 = (u_char) ( (lbn>>16) & 0xff );	/* mid2 address bits */
    read_10->addr_1 = (u_char) ( (lbn>>8) & 0xff );	/* mid1 address bits */
    read_10->addr_0 = (u_char) ( lbn & 0xff );		/* low address bits */
    read_10->cnt_h = (u_char)((blocks >> 8) & 255);	/* High block count */
    read_10->cnt_l = (u_char)(blocks & 255);	/* Low block count */
    read_10->link = 0;				/* no link */
    read_10->flag = 0;				/* no flag */
    read_10->vendor = vf & 3;			/* vendor code */
 
/* Set up the fields for the User Agent Ioctl call. */
 
    ua_ccb_rd.uagt_buffer = buf;		/* where the data goes */ 
    ua_ccb_rd.uagt_buflen = size;		/* how much data to transfer */ 
    *over = 0;					/*  clear overflow flag  */

/*  start transfer loop  */

    while( count>0 ) {
	for( seg=0;(seg<SEG_SIZE)&&(count>0); seg+=MAX_DATA ) {

	    ccb_rd.cam_timeout = 320;	/* timeout of 320 Sec for DCRSi */
    	    if( blocks>count ) {
	/* non-standard sized last transfer, adjust sizes */

		blocks=count;	/*get last, short transfer*/
		size = blocks << 9;	/*  bytes per transfer size  */
		ccb_rd.cam_dxfer_len = size;	/* how much data */
		ua_ccb_rd.uagt_buflen = size;	/* how much data to transfer */ 
		read_10->cnt_h = (u_char)((blocks>>8) & 255);	/* High count */
		read_10->cnt_l = (u_char)(blocks & 255);	/* Low count */
	    }
	    if((seg+size)>=SEG_SIZE)read_10->dpo=0;/*last time,assert RDLAST* */

	/*  do the read  */

	    if( ioctl(cam_fd, UAGT_CAM_IO, (caddr_t)&ua_ccb_rd) < 0 ) {
		perror("\nRDB Error on CAM UAgt Ioctl to Read data line");
		return(-1);
	    }

/* If the CCB did not completed successfully report the error. */

	    if (ccb_rd.cam_ch.cam_status != CAM_REQ_CMP) {
		printf("RDB CCB_RD.CAM_CH.CAM_STATUS error:%d\n",
			ccb_rd.cam_ch.cam_status );
		print_ccb_status("CAM UAgt Read data line Ioctl",
			&(ccb_rd.cam_ch) );	/* report the error values */
		printf("RDB cam_scsi_status=0x%.2X\n", ccb_rd.cam_scsi_status);
		return(-1);
	    }
	    read_10->reladr = 1;	/* Do not wait for START* again*/
	    ua_ccb_rd.uagt_buffer += size;	/* where the data goes */ 
	    ccb_rd.cam_data_ptr += size;	/* where the data goes */ 
	    count -= blocks;			/*reduce count by last ioctl*/
	    lbn += blocks;
	    read_10->addr_3 = (u_char) ( (lbn>>24) & 0xff );
	    read_10->addr_2 = (u_char) ( (lbn>>16) & 0xff );
	    read_10->addr_1 = (u_char) ( (lbn>>8) & 0xff );
	    read_10->addr_0 = (u_char) ( lbn & 0xff );
	}		/*  finished getting a segment  */

	read_10->dpo = 1;	/*deassert RDLAST for next buffer*/

/*  receive some status data from SCSI controller and do quick checks
	Register 6 in ReceiveData[6] contains
	bit 7: START*
	bit 6: READY*
	bit 5: OVERFLOW*
	bit 4: AORB
	bit 3..0: ADDRIN<19..16>
*/
	rc( 0, 8, ReceiveData );
/*
	printf("RDB OUTBD SCSI registers: ");
	for( i=0; i<7; i++ ) printf(" %02x", ReceiveData[i] );
	printf("\n");
*/
	if( ( ReceiveData[6]&0x20 )==0 )
	{	printf("**** RDB Overflow detected ****\n");
		*over = 1;
		return( -1 );
		}
	}	/*  end transfer loop  */
   return(1);
}

/*  rdf( file, lbn, count, buf, vf, over )----------------------------------
    routine to read data into buffer from SCSI device.  Each read will
    transfer a maximum of MAX_DATA bytes until all data has been transferred

    Inputs:

	filename:  name of file to write data into
	lbn:  first logical block to start reading from 
	count:  Number of blocks to read (512 byte blocks)
	buf:  buffer to read data into (must be at least as big as count*512)
	vf:  SCSI vendor flag options, set to 0 for normal operations.
	over:  pointer to flag which will be set to true if buffer overflowed.
*/
int rdf( filename, lbn, count, buf, vf, over )
char *filename;
int lbn, count, vf, *over;
u_char *buf;
{
    int rdb();
    int status, ofd, size, blocks;

/* Open the output file and report any errors. */

    if ((ofd = open(filename, O_RDWR|O_CREAT, 0644)) < 0 ){
	perror("Error on Output File Open");
	exit(1);
    }

/*  start transfer loop  */

    size = 4096*1024;	/*  bytes per transfer  */
    blocks = size>>9;	/*  blocks per transfer  */
    while( count>0 ){
	if( blocks>count ){
	    blocks = count;
	    size = blocks<<9;
	}
/*
printf("RDF Current COUNT=%d, SIZE=%d, BLOCKS=%d\n", count, size, blocks );
*/
/*  Read the data from the OUTBOARD using RDB  */

	if( (status=rdb( lbn, blocks, buf, vf, over )) != 1 ){
	    printf("RDF error from RDB: %d\n", status );
	    printf("RDF current COUNT=%d, SIZE=%d, BLOCKS=%d\n",
		count, size, blocks );
	    close( ofd );
	    return( -1 );
	}

/*  write data to output file  */

	if( write( ofd, buf, size ) ){
	    perror("RDF file write error:");
	    printf("RDF current COUNT=%d, SIZE=%d, BLOCKS=%d\n",
		count, size, blocks );
	    close( ofd );
	    return( -1 );
	}
	count -= blocks;
	vf = 0;
    }	/*  end transfer loop  */
    close( ofd );
    return(1);
}

/* ---------------------------------------------------------------------- */
/* Local routines and data structure to report in text and Hex form the 
returned CAM status. */

struct cam_statustable {
	u_char	cam_status;
	caddr_t	status_msg;
} cam_statustable[] = {
    {	CAM_REQ_INPROG,		"CCB request is in progress"		},
    {	CAM_REQ_CMP ,		"CCB request completed w/out error"	},
    {	CAM_REQ_ABORTED,	"CCB request aborted by the host"	},
    {	CAM_UA_ABORT,		"Unable to Abort CCB request"		},
    {	CAM_REQ_CMP_ERR,	"CCB request completed with an err"	},
    {	CAM_BUSY,		"CAM subsystem is busy"			},
    {	CAM_REQ_INVALID,	"CCB request is invalid"		},
    {	CAM_PATH_INVALID,	"Bus ID supplied is invalid"	        },
    {	CAM_DEV_NOT_THERE,	"Device not installed/there"		},
    {	CAM_UA_TERMIO,		"Unable to Terminate I/O CCB req"	},
    {	CAM_SEL_TIMEOUT,	"Target selection timeout"		},
    {	CAM_CMD_TIMEOUT,	"Command timeout"			},
    {	CAM_MSG_REJECT_REC,	"Reject received"			},
    {	CAM_SCSI_BUS_RESET,	"Bus reset sent/received"		},
    {	CAM_UNCOR_PARITY,	"Parity error occured"			},
    {	CAM_AUTOSENSE_FAIL,	"Request sense cmd fail"		},
    {	CAM_NO_HBA,		"No HBA detected Error"			},
    {	CAM_DATA_RUN_ERR,	"Overrun/underrun error"		},
    {	CAM_UNEXP_BUSFREE,	"BUS free"				},
    {	CAM_SEQUENCE_FAIL,	"Bus phase sequence failure"		},
    {	CAM_CCB_LEN_ERR,	"CCB length supplied is inadaquate"	},
    {	CAM_PROVIDE_FAIL,	"To provide requ. capability"		},
    {	CAM_BDR_SENT,		"A SCSI BDR msg was sent to target"	},
    {	CAM_REQ_TERMIO,		"CCB request terminated by the host"	},
    {	CAM_LUN_INVALID,	"LUN supplied is invalid"		},
    {	CAM_TID_INVALID,	"Target ID supplied is invalid"		},
    {	CAM_FUNC_NOTAVAIL,	"Requested function is not available"	},
    {	CAM_NO_NEXUS,		"Nexus is not established"		},
    {	CAM_IID_INVALID,	"The initiator ID is invalid"		},
    {	CAM_CDB_RECVD,		"The SCSI CDB has been received"	},
    {	CAM_SCSI_BUSY,		"SCSI bus busy"				}
};
int cam_statusentrys = sizeof(cam_statustable) / sizeof(cam_statustable[0]);

char * camstatus( cam_status )
register u_char cam_status;
{
	register struct cam_statustable *cst = cam_statustable;
	register entrys;

	for( entrys = 0; entrys < cam_statusentrys; cst++ ) {
		if( cst->cam_status == cam_status ) {
			return( cst->status_msg );
		}
	}
	return( "Unknown CAM Status" );
}
 
void print_ccb_status(id_string,cp)
char *id_string;
CCB_HEADER *cp;
{
        register i;
 
        printf("Status from %s\n",id_string);
        printf(" cam_status = 0x%.2X   (%s%s%s)\n", cp->cam_status,
        ((cp->cam_status & CAM_AUTOSNS_VALID) ? "AutoSns Valid-" : "" ),
        ((cp->cam_status & CAM_SIM_QFRZN) ? "SIM Q Frozen-" : "" ),
        camstatus( cp->cam_status & CAM_STATUS_MASK ));
 
        if (cp->cam_status & CAM_AUTOSNS_VALID) {
                printf("AutoSense Data (in hex):\n ");
                for( i=0; i < SENSE_LEN; i++)
                        printf("%.2X ", sense[i]);
                printf("\n" );
        }
        fflush(stdout);
}
 
void clear_mem(bp,n)		/* Clear n bytes of memory beginning at bp */ 
u_char *bp;
int	n;
{
	register i;
	register u_char *ptr;
	for(i=0, ptr=bp; i<n; i++, ptr++) *ptr = 0;
}
 
void swap_short_store(bp,val)	/* Store short into byte-reversed storage */ 
u_char *bp;
u_short val;
{
	u_short temp;
	register u_char *ptr;
	ptr = bp;			/* Copy pointer */
	*(bp++) = (u_char)(val>>8);	/* Store high byte first */
	*bp     = (u_char)val;		/* Then store low byte */
}
 
void swap_long_store(bp,val)	/* Store long into byte-reversed storage */ 
u_char *bp;
u_long val;
{
	*(bp++) = (u_char)(val>>24);	/* Store high byte first */
	*(bp++) = (u_char)(val>>16);
	*(bp++) = (u_char)(val>>8);
	*bp     = (u_char)val;		/* Store low byte last */
}
