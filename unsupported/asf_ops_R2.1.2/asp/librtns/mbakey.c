/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbakey.c -- keyboard routines

	This program is a set of routines provided by Masscomp,
	which supports reading single keystrokes from the user's
	keyboard.  The routines have been modified to use no-wait
	i/o and disable input editing.
*/

/* taken from "key.c" unsupported masscomp example program
   alan jarboe masscomp service (214)239-5686
*/

#include <fcntl.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <signal.h>
#include <termio.h>
#include <ctype.h>

static struct termio   keyload;	/* holds key info    */
reset_kbd ();			/* restores kybd set */

int lowcase_sw = 0;		/* 1 = lowercase input is OK */

static kbd_open = 0;		/* 1 = keyboard is open */

int no_break = 1;		/* 0 = ctrl-C was entered */

/* get_byte(c) ---------------------------------------------------------
	This routine returns the next available character in c.  If no
	character is returned, c returns as '\0'.
*/
get_byte(c)
char *c;
{
	if (read(0, c, 1) > 0) {
	    if (lowcase_sw == 0) *c = toupper (*c);
	}
	else *c = '\0';
	return (0);
}

/* open_keyboard () ---------------------------------------------------
	This routine sets the keyboard in no-wait i/o mode, and
	disables input editing ("cooked" mode).
*/
open_keyboard() 
{
	struct termio   newkeyload;		/* new keyboard params */

    /* get old keyboard params */
	if ((ioctl (0, TCGETA, &keyload)) == -1) {
	    perror ("open_keyboard: ");	/* could not do it */
	    exit (1);
	}
    /* set up new keyboard params */
	newkeyload = keyload;			/* init to old params */
	newkeyload.c_lflag = newkeyload.c_lflag & ~(ICANON);
	newkeyload.c_cc[4] = 1;			/* set min chars to 1 */
	newkeyload.c_cc[5] = 2;			/* set timeout 0.2sec */
    /* set the keyboard to the new params */
	if ((ioctl (0, TCSETAW, &newkeyload)) == -1)
	{
	    perror ("open_keyboard: unable to do ioctl");
	    exit (1);
	}
    /* set ctrl-C exit handler routine */
	if (((long) signal (SIGINT, reset_kbd)) == -1) {
	    perror ("main: unable to set signal trap");
	    exit (1);
	}
    /* set keyboard in no-wait mode */
	if (fcntl (0, F_SETFL, FNDELAY) == -1) {
	    perror ("open_k: unable to set fcntl flags\n");
	    exit (1);
	}
    /* set flag saying keyboard is open */
	kbd_open = 1;
}

/* close_keyboard() -----------------------------------------------
	This routine returns the keyboard settings to their original
	state, allowing normal keyboard i/o.
*/
close_keyboard()
{
	if ((ioctl (0, TCSETAW, &keyload)) == -1) {
	    perror ("close_k: unable to do ioctl");
	    exit (1);
	}
	if (fcntl (0, F_SETFL, 0) == -1) {
	    perror ("close_k: unable to set fcntl flags");
	    exit (1);
	}
	kbd_open = 0;
}

/* reset_kbd () ---------------------------------------------------
	This routine resets the keyboard if ctrl-C is typed or
	an error occurs.
*/
reset_kbd () 
{
	close_keyboard();
	exit (0);
}

/* mba_disp_int() ------------------------------------------------------
	This routine flags a ctrl-C interrupt when it occurs.
*/
mba_disp_int()
{
	no_break = 0;
    /* if the keyboard is open, reset the signal trap to the
       keyboard ctrl-C handler */
	if (kbd_open) signal (SIGINT, reset_kbd);
}


/* set_break () --------------------------------------------------------
	This routine sets up the ctrl-C signal to return to our handler
	above.
*/
set_break ()
{
	signal (SIGINT, mba_disp_int);
	no_break = 1;
}
