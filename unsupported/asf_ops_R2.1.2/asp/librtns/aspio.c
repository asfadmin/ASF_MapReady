/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  ASPIO - support routines for accessing the ASP hardware through the
    BIT-3 VME adaptor on the Turbochannel
*/

#define RESP_PAGE_PTR (0x1008>> 2)
#define SYNC_PAGE_PTR (0x100c>> 2)

#include        <sys/file.h>
#include        <sys/types.h>
#include        <sys/stat.h>
#include        <stdio.h>
#include        <unistd.h>
#include        <errno.h>
extern int errno;

#include        <btio.h>

/*  define some of the stuff in aspdecl.h that are needed here  */

union {		/* memory-mapped i/o space */
	unsigned long i;
	unsigned int *u;
	unsigned short int *w;
	unsigned char *b;
} mb, mb_sync;
#define DATA_START (0x1000 >> 2)
#define RESP_LENGTH (0x1004 >> 2)
#define PAGE_SIZE 0x80000

#define MAX_VME_LEN 8192 
#define START_ADDR 0x80000000
int asp_vme_chan;
u_char *asp_mb_buf, *mb_sync_buf;
bt_status_t status_flags;

/** Local function prototypes **/

int bit3_perror();
int asp_read(), asp_write(), initmbx(), set_vme_param();

/**********************************************************************
**                                                                   **
**  initmbx - open device special file for VME bus adaptor driver    **
**  Input:  base_address - starting address to map into              **
**  Synopsis:                                                        **
**	BIT3 VME adaptor puts different VME components and address   **
**	ranges onto different logical units (/dev/btt*).             **
**	The ASP control bus has been mapped into 0x80000000, the     **
**	Remote Extended VME RAM area, so is used here by default.    **
**	Also, the default physical unit is 0, the first ( and        **
**	usually the only) BIT3 adaptor installed in the system.      **
**	This produces a logical device of /dev/btt128 by default.    **
**                                                                   **
**********************************************************************/

int initmbx( vme_base_addr )
unsigned int vme_base_addr;
{
    int unit = 0;			/* physical unit 0 by default */
    int type = BT_AXSRR;		/* Access RRAM by default */
    u_long remote_address = START_ADDR;	/* Access starts here */
    int syspagesize, getpagesize();
    char devname[64];

/*  call the VME_INIT program to make sure unit is properly initialized */

    sprintf( devname, "vme_init -u %d -a %x\n", unit, vme_base_addr );
    system ( devname );

/*  Use starting address to determine accecss type  */

    remote_address = (u_long) vme_base_addr;
    if( remote_address > (u_long) INT_MAX ) type = BT_AXSRE;
    else type = BT_AXSRR;

/*  Build name of special file  */

    sprintf( devname, "/dev/btt%d", unit + ( type<<BT_AXS_SHFT ) );
    printf("INITMBX opening logical device %s...\n", devname );
    if( ( asp_vme_chan = open( devname, O_RDWR ) ) < 0 ){
	perror("VME_OPEN error");
	printf("Return code=%d, errno=%d\n", asp_vme_chan, errno );
	return( -1 );
    }
printf("...VME adapter device opened, channel=%d\n", asp_vme_chan );

/*  Allocate 1 MB of memory for local copy of MB registers and actual
    MB arrays.  mb.w[0:2047] is registers and mb.w[2048:524287] is
    memory for operations that move data between memory and ASP buffers */

    syspagesize = getpagesize();
    if( ( asp_mb_buf = (u_char *)malloc( syspagesize+(PAGE_SIZE<<1) ) )==0 ){
	perror("INITMBX malloc error");
	printf("Returned address=%x, errno=%d\n", asp_mb_buf, errno );
	return( -1 );
    }
printf("MB array allocated at:%8x\n", asp_mb_buf );
    mb.b = &asp_mb_buf[syspagesize-1];
    mb.i &= ~(syspagesize-1);
printf("MB array page aligned to:%8x\n", mb.w );

    mb.u[DATA_START] = 0x80000000;
    mb.u[RESP_LENGTH] = 0x80000000;
/*
printf("Setting Multibus switch to use VME board...\n");
    asp_read( 0, &mb.w[0], 2 );
    mb.w[0] |= 0x10;
    asp_write( 0, &mb.w[0], 2 );
*/

/*  allocate memory for accessing SYNC codes  */

    if( ( mb_sync_buf = (u_char *)malloc( syspagesize+(PAGE_SIZE<<1) ) )==0 ){
	perror("INITMBX malloc error");
	printf("Returned address=%x, errno=%d\n", mb_sync_buf, errno );
	return( -1 );
    }
printf("MB_SYNC array allocated at:%8x\n", mb_sync_buf );
    mb_sync.b = &mb_sync_buf[syspagesize-1];
    mb_sync.i &= ~(syspagesize-1);
printf("MB_SYNC array page aligned to:%8x\n", mb_sync.w );

/*  initialize response and sync page pointers  */

    mb.u[RESP_PAGE_PTR] = -1;
    mb.u[SYNC_PAGE_PTR] = -1;

    return( 1 );
}

/*  qx() - routine to deallocate mb array  */
qx(){
/*
printf("Resetting Multibus switch to use MULTIBUS...\n");
	asp_read( 0, &mb.w[0], 2 );
	mb.w[0] &= 0x0f;
	asp_write( 0, &mb.w[0], 2 );
*/
    set_vme_param( 7, 2 );	/* transfer mode to 2 byte */
    set_vme_param( 12, 0 );	/* byte swap mode to none */
    free( asp_mb_buf );
    free( mb_sync_buf );
}

/*******************************************************/
/*  SET_VME_PARAM - set BIT-3 VME adaptor parameters   */
/*******************************************************/

int set_vme_param( param, value )
int param, value;
{
    bt_param_t bit3_params;

    bit3_params.param = param;
    bit3_params.value = value;
    if( ( ioctl(asp_vme_chan, BIOC_PARAM, &bit3_params) ) < 0 ){
	perror("BIOC_PARAM IOCTL error");
	printf("SET_VME_PARAM BIT3 error, parameter=%d value=%d\n",
		param, value );
	return( -1 );
    }
    if (bit3_perror(asp_vme_chan)) {
	printf("SET_VME_PARAM BIT3 error, parameter=%d value=%d\n",
		param, value );
	return(-1);
    }
    return( 1 );
}

/*******************************************************/
/*  ASP_WRITE - write data to a specified ASP address  */
/*******************************************************/

int asp_write( asp_addr, buf, inbytes )
int inbytes;
unsigned int asp_addr;
char *buf;
{
    int xfer_bytes, len_xfrd, bytes;

/*
unsigned short int *wbuf;
wbuf = (unsigned short int *) buf;
printf("ASP_WRITE address=%06x(%d), bytes=%d(%06x)",
asp_addr, asp_addr, inbytes, inbytes );
if( inbytes>2 ) printf(" first");
printf(" value=%04x\n", *wbuf&0xffff );
*/

/*  Clear any status errors on the Adaptor  */

    (void) ioctl(asp_vme_chan, BIOC_CLR_STATUS, &status_flags);
    if (bit3_perror(asp_vme_chan)) {
	printf("ASP_WRITE BIT3 error\n");
        return(-1);
    }

/*  Position to requested remote address  */

    if (lseek(asp_vme_chan, (u_long) asp_addr, SEEK_SET) == -1) {
        perror("ASP_WRITE lseek error");
	printf("Return code:%d, errno=%d\n", len_xfrd, errno );
        return(errno);
    }

/*  Check for non page aligned starting address and fix up if needed */

    bytes = MAX_VME_LEN - ( asp_addr & (MAX_VME_LEN-1) );
    if( bytes>0 ){
	if( bytes>inbytes ) bytes = inbytes;
	if( ( len_xfrd=write( asp_vme_chan, buf, bytes ) ) != bytes ){
	    perror("ASP_WRITE initial write error");
	    printf("Bytes attempted=%d, transferred=%d, remaining=%d\n", 
		bytes, len_xfrd, bytes );
	    printf("Input address=%x(%d), bytes=%d\n", 
		asp_addr, asp_addr, inbytes );
	    bit3_perror( asp_vme_chan );
	    return( -1 );
	}
	buf += bytes;
    }

/*  Do the remaining transfer MAX_VME_LEN bytes at a time  */

    bytes = inbytes-bytes;
    while( bytes>0 ){
	if( bytes> MAX_VME_LEN ) xfer_bytes = MAX_VME_LEN;
	else xfer_bytes = bytes;
	if( ( len_xfrd=write( asp_vme_chan, buf, xfer_bytes ) ) <= 0 ){
	    perror("ASP_WRITE error");
	    printf("Bytes attempted=%d, transferred=%d, remaining=%d\n", 
		xfer_bytes, len_xfrd, bytes );
	    printf("Input address=%x(%d), bytes=%d\n", 
		asp_addr, asp_addr, inbytes );
	    bit3_perror( asp_vme_chan );
	    return( -1 );
	}
	buf += len_xfrd;
	bytes -= len_xfrd;
    }
    return( 1 );
}

/*******************************************************/
/*  ASP_READ - read data from a specified ASP address  */
/*******************************************************/

int asp_read( asp_addr, buf, inbytes )
int inbytes;
unsigned int asp_addr;
char *buf;
{
    int xfer_bytes, len_xfrd, bytes;

/*
printf("ASP_READ address=%06x(%d) bytes=%d(%06x) buffer=%x\n",
asp_addr, asp_addr, inbytes, inbytes, buf );
*/

/*  Clear any status errors on the Adaptor  */

    (void) ioctl(asp_vme_chan, BIOC_CLR_STATUS, &status_flags);
    if (bit3_perror(asp_vme_chan)) {
	printf("ASP_READ BIT3 error\n");
        return(-1);
    }

/*  Position to requested remote address  */

    if (lseek(asp_vme_chan, (u_long) asp_addr, SEEK_SET) == -1) {
        perror("ASP_READ lseek error");
	printf("Return code:%d, errno=%d\n", len_xfrd, errno );
        return(errno);
    }

/*  Check for non page aligned starting address and fix up if needed */

    bytes = MAX_VME_LEN - ( asp_addr & (MAX_VME_LEN-1) );
    if( bytes>0 ){
	if( bytes>inbytes ) bytes = inbytes;
	if( ( len_xfrd=read( asp_vme_chan, buf, bytes ) ) != bytes ){
	    perror("ASP_READ initial read error");
	    printf("Bytes attempted=%d, transferred=%d, remaining=%d\n", 
		bytes, len_xfrd, bytes );
	    printf("Input address=%x(%d), bytes=%d\n", 
		asp_addr, asp_addr, inbytes );
	    bit3_perror( asp_vme_chan );
	    return( -1 );
	}
	buf += bytes;
    }

/*  Do the remaining transfer MAX_VME_LEN bytes at a time  */

    bytes = inbytes-bytes;
    while( bytes>0 ){
	if( bytes> MAX_VME_LEN ) xfer_bytes = MAX_VME_LEN;
	else xfer_bytes = bytes;
	if( ( len_xfrd=read( asp_vme_chan, buf, xfer_bytes ) ) <= 0 ){
	    perror("ASP_READ error");
	    printf("Bytes attempted=%d, transferred=%d, remaining=%d\n", 
		xfer_bytes, len_xfrd, bytes );
	    printf("Input address=%x(%d), bytes=%d\n", 
		asp_addr, asp_addr, inbytes );
	    bit3_perror( asp_vme_chan );
	    return( -1 );
	}
	buf += len_xfrd;
	bytes -= len_xfrd;
    }
    return( 1 );
}

/*****************************************************************************
**
**      Function:   bit3_perror()
**
**      Purpose:    Calls the BIOC_STATUS ioctl() command and checks
**                  the local status register for errors.  Prints a
**                  message to stderr if any errors are detected.
**
**      Args:       chan            File channel device was opened on.
**
**      Returns:    0               No errors.
**                  -1               If status errors.
**
*****************************************************************************/

int     bit3_perror(chan)
int     chan;
{
    bt_status_t         data;

    if (ioctl(chan, BIOC_STATUS, &data)) {
        printf("BIOC_STATUS returned errno = %d.\n", errno);
        return(-1);
    }

    if (data &= BT_STATUS_MASK) {
        fprintf(stderr, "Status error 0x%02X:\n", data>>BT_INTR_ERR_SHFT);
        if (data & BT_INTR_POWER) {
            fprintf(stderr, "\tRemote chassis off or cable disconnected.\n");
        } else {
            if (data & BT_INTR_PARITY)
                fprintf(stderr, "\tInterface parity error.\n");
            if (data & BT_INTR_REMBUS)
                fprintf(stderr, "\tRemote bus error.\n");
            if (data & BT_INTR_TIMEOUT)
                fprintf(stderr, "\tInterface timeout.\n");
        }
        return (-1);
    }
    return (0);
}
