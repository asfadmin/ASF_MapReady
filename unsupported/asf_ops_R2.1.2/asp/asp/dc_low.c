/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* dc_low.c -- read a string or integer value in so many seconds */

#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>
#include <termio.h>
#include <sys/time.h>
#include <procfil.h>
#include <procdec.h>
#include <sony.h>

#define KEY_PORT 2		/* Number of ports handles */
 /* This is the only thing to change if    */
 /*  more ports are required               */
#define USED 0
#define UNUSED -1


struct termio   keyload[KEY_PORT];/* holds key info */

extern int  alarm_count;
extern int  alarm;
static char dc_wait_seps[80] = ";*\n";
static int  dev_no[KEY_PORT] = { UNUSED, UNUSED };


/* dc_wait_for_int(id, value, timeout) -----------------------------
	This routine reads an integer value from the port, and
	returns that value in 'value'.  If the value has not been
	entered in 'timeout' seconds, the routine returns -1, else
	the routine returns 0.
*/

dc_wait_for_int (id, value, timeout)
int     id, *value, timeout;
{
    int     i;
    char    t[17];

    i = dc_wait_for_str (id, t, 15, timeout);
    *value = atoi (t);
    return (i);
}

/* dc_wait_for_str(id, t, len, timeout) --------------------------
	This routine reads a string of characters from the port,
	terminated by a separator character, and places the string in
	t.  A maximum of 'len' characters are read.  The routine allows
	'timeout' seconds to begin receiving characters, and another
	'timeout' seconds from receipt of the first character to 
	receipt of the expected separator.  All characters read,
	including the separator if any, are placed in t.  The routine
	returns 0 if a separator was read, or -1 if not.
*/

dc_wait_for_str (id, t, len, timeout)
char    t[];
int     id, len, timeout;
{
    char    c = 'x';
    int     i = 0;
    char   *strchr ();
    int	   timer;

    timer = timeout*1000000;		/* clw 8/25/95 removed alarm */
    while (strchr (dc_wait_seps, c) == NULL)
    {
	dc_get_byte(id,&c);
	if (c == '\0') {
	    if (timer<=0) {
		t[i] = '\0';
		return (-1);
	    }
	    c = 'x';
	} else if (i < len) {
	    t[i++] = c;
	} else {
	    printf ("dc_wait_for_str: INSUFFICIENT BUFFER SIZE\n");
	    return (-1);
	}
	usleep(1000); timer -= 1000;
    }				/* while */
    t[i] = '\0';
    return (0);
}


/* int dc_get_byte(id,c)------------------------------------------
	This routine returns the next available character in c.  If no
	character is returned, c returns as '\0'.
*/

int     dc_get_byte (id, c)
int     id;		/* DCRSi id */
char   *c;
{
    int     i;

    i = read (dev_no[id], c, 1);
    if (i > 0) /* if data byte available */
	*c = toupper (*c);
    else /* if no data or if blocking time-out */
	*c = '\0';
    return (0);
}


/* open_dcrsi(id, descript) --------------------------------------------
	This routine sets up a previously opened tty port for use as a
	DCRSi recorder command port.  The port is set to interrupt on
	every byte.  The routine returns PASS or FAIL.
	    id = the recorder id (IN or OUT)
	    descript = the open tty file descriptor
*/

int     open_dcrsi (id, descript)
int     id;			/* Id for one of the interrupts */
int     descript;		/* File descriptor from open() function */
{
    struct termio   newkeyload;	/* new keyboard load  */
    struct itimerval    timer, oldtimer;

 /* Assign new dev_no */
    if (id >= KEY_PORT)		/* Invalid id */
	return (FAIL);
    if (dev_no[id] == descript)
	return (PASS);		/* Return, already being serviced */
    else
	dev_no[id] = descript;

    if ((ioctl (dev_no[id], TCGETA, &keyload[id])) == -1)
    {
	perror ("open_d: unable to do ioctl TCGETA\n");
	exit (1);
    }
	newkeyload = keyload[id];
/*
	newkeyload.c_iflag = newkeyload.c_iflag | IXON | IXOFF;
	newkeyload.c_cflag = newkeyload.c_cflag | CLOCAL;
	newkeyload.c_lflag = newkeyload.c_lflag & ~ICANON;
*/
	newkeyload.c_iflag = newkeyload.c_iflag | IGNCR | IXOFF;
	newkeyload.c_oflag = 0;
	newkeyload.c_cflag = newkeyload.c_cflag | CLOCAL;
	newkeyload.c_lflag = 0;
	newkeyload.c_cc[4] = 1;	/* set min charrs to 1 */
	newkeyload.c_cc[5] = 2;	/* set timeout to 0.2 secs */
	if ((ioctl (dev_no[id], TCSETAW, &newkeyload)) == -1)
	{
	    perror ("open_d: unable to do ioctl TCSETAW");
	    exit (1);
	}
/* clw 8/25/95 resumed this section of code */
    if (fcntl (dev_no[id], F_SETFL, FNDELAY) == -1)
    {
	perror ("open_k: unable to set fcntl flags\n");
	return (FAIL);	/* clw return instead of exit */
    }
/* end */
    return (PASS);
}

/* close_dcrsi(id) -------------------------------------------------
	This routine returns the port settings to their original
	state, allowing normal port i/o.
*/

close_dcrsi (id)
int    id;
{
    int     device;
    if (dev_no[id] == UNUSED)
	return;
    device = dev_no[id];
    dev_no[id] = UNUSED;	/* Set to unused state */

    if ((ioctl (device, TCSETAW, &keyload[id])) == -1)
    {				/* set new parms  */
	printf("close_dcrsi TCSETAW error, ID=%d, DEVICE=%d\n",
		id, device );
	perror ("close_dcrsi: unable to do ioctl\n");/* say error */
	exit (1);		/* get out */
    }
}



/* open_sony(id, descript) --------------------------------------------
	This routine sets up a previously opened tty port for use as a
	SONY recorder command port.  The port is set to interrupt on
	every byte.  The routine returns PASS or FAIL.
	    id = the recorder id (IN or OUT)
	    descript = the open tty file descriptor
*/

int     open_sony (id, descript)
int     id;			/* Id for one of the interrupts */
int     descript;		/* File descriptor from open() function */
{
    struct termio   newkeyload;	/* new keyboard load  */
    struct itimerval    timer, oldtimer;

 /* Assign new dev_no */
    if (id >= KEY_PORT)		/* Invalid id */
	return (FAIL);
    if (dev_no[id] == descript)
	return (PASS);		/* Return, already being serviced */
    else
	dev_no[id] = descript;

/*
printf("dc_low: dev_no = %d\n", dev_no[id]);
*/

    if ((ioctl (dev_no[id], TCGETA, &keyload[id])) == -1)
    {
	perror ("open_d: unable to do ioctl TCGETA\n");
	exit (1);
    }
	newkeyload = keyload[id];

	newkeyload.c_iflag = IGNBRK | IGNCR | IXOFF;
	newkeyload.c_oflag = 0;
	newkeyload.c_cflag = B9600 | CS8 | CREAD | CLOCAL | PARENB;
	newkeyload.c_lflag = 0;
	newkeyload.c_cc[4] = 3;	/* set min charrs to 1 */
	newkeyload.c_cc[5] = 10;	/* set timeout to 0.2 secs */
	if ((ioctl (dev_no[id], TCSETAW, &newkeyload)) == -1)
	{
	    perror ("open_d: unable to do ioctl TCSETAW");
	    exit (1);
	}
/* clw 8/25/95 resumed this section of code */
    if (fcntl (dev_no[id], F_SETFL, FNDELAY) == -1)
    {
	perror ("open_k: unable to set fcntl flags\n");
	return (FAIL);	/* clw return instead of exit */
    }
/* end */
    return (PASS);
} /* open_sony */


/* close_sony(id) -------------------------------------------------
	This routine returns the port settings to their original
	state, allowing normal port i/o.
*/

close_sony (id)
int    id;
{
    int     device;
    if (dev_no[id] == UNUSED)
	return;
    device = dev_no[id];
    dev_no[id] = UNUSED;	/* Set to unused state */

    if ((ioctl (device, TCSETAW, &keyload[id])) == -1)
    {				/* set new parms  */
	printf("close_sony TCSETAW error, ID=%d, DEVICE=%d\n",
		id, device );
	perror ("close_sony: unable to do ioctl\n");/* say error */
	exit (1);		/* get out */
    }
} /* close_sony */



/*sony_wait_for_str(id, expect_str, return_str, str_len, timeout)-----
  This routine reads the data string from the port and put the data
  string in the return_str with the string length in str_len. The routine
  allows 'timeout' seconds for reciving the data otherwise will return
  failure.
*/

int sony_wait_for_str(id, expect_str, return_str, str_len, timeout)
  int  id;
  unsigned char *expect_str, *return_str;
  int  *str_len;
  int  timeout;
{
   int timer;
   unsigned char match_value[2];
   int match;

   int i;
   *str_len = 0;
   match = 0;
   timer = timeout*1000000;


/*
     for (i=0; i<2; i++)
        printf("  expect_str :%02x", 0xff&(int)expect_str[i]);

     printf("\n");
*/

     usleep(20000);

     *str_len = read (dev_no[id], return_str, 256);
/*
     printf("str_len = %d\n", *str_len);

     for (i=0; i<*str_len; i++)
        printf("  return_str :%02x\n", 0xff&(int)return_str[i]);
     printf("\n");
*/

     if (*str_len >= 2) {
        match_value[0] = expect_str[0] ^ return_str[0];
        match_value[1] = expect_str[1] ^ return_str[1];
        if ((match_value[0] == 0) && (match_value[1] == 0)){
          match =1;
          return(PASS);
        }
        else {
              printf("Cann't get expect ACK\n");
              return(FAIL);
             }            
     }
     else {
           printf("Cann't get expect ACK (str_len = %d)\n", *str_len);
           return(FAIL);
          }




}/* sony_wait_for_str */
