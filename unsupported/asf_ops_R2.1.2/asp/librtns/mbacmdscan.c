/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbacmdscan.c -- parse 5 arguments from the command line

	This routine parses the first 5 arguments found on the
	command line of the calling program.  The five arguments
	are:
		<clock_speed> <stop_on_error> <repetitions> 
		<random_count> <random_start>

	<clock_speed> = test clock frequency:  0 = 10 MHz
					       1 = 5 MHz
					       2 = 2.5 MHz
	<stop_on_error> = 1 means drop into bobtalk if an error
			  is encountered.
			= 0 means keep going.
	<repetitions> = number of times to repeat each test.
	<random_count> = number of different random stimulus tests to 
			 run
	<random_start> = start with the n'th random stimulus

	The value of each argument is placed in a global variable
	of the same name.  If any argument is not present, a default
	value is used.  The defaults are:

		clock_speed:	10MHz
		stop_on_error:	1 (stop)
		repetitions:	1
		random_count:   1
		random_start:   1
*/


#include <aspdecl.h>

int stop_on_error;
int clock_speed;
int repetitions;
int random_count;
int random_start;
int use_bob = 1;

/* command_scan (argc, argv) -------------------------------------------
	This routine parses the first 5 arguments found on the command
	line of the calling program.  The five arguments are:
		<clock_speed> <stop_on_error> <repetitions> 
		<random_count> <random_start>
*/
command_scan (argc, argv)
int argc;
char *argv[];
{
    stop_on_error = 1;
    clock_speed = 0;
    repetitions = 1;
    random_count = 1;
    random_start = 1;
    if (argc > 1) clock_speed = atoi(argv[1]) & 0x3;
    if (argc > 2) stop_on_error = atoi(argv[2]) & 0x1;
    if (argc > 3) repetitions = atoi(argv[3]);
    if (argc > 4) random_count = atoi(argv[4]);
    if (argc > 5) random_start = atoi(argv[5]);
    if (repetitions <= 0) repetitions = 1;
    switch (clock_speed) {
	case 0 :
			printf ("Clock = 10MHz ");
			break;
	case 1 :
			printf ("Clock = 5MHz ");
			break;
	case 2 :
			printf ("Clock = 2.5MHz ");
			break;
	case 3 :
			printf ("Clock = External ");
			break;
    }
    printf ("  Stop_on_error=%d Reps=%d RCount=%d RStart=%d\n",
	stop_on_error, repetitions, random_count, random_start);
}
