/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* dc_usr.c - This module contains the high level DCRSi
	routines which utilize the low level routines
	in the dcrs_uts.c module.  Routine in the
	tape_mnt.c module are also used.  Routines is this
	module are prefixed with dcu_ .
*/

#include <stdio.h>
#include <string.h>
#include <procfil.h>
#include <procdec.h>


/* dcu_setup() ---------------------------------------------------------
	This routine does what is required to configure
	the DCRSi's for use with the ASP processing
	initialization routines.  op_get_root must have
	been previously called.  If the configuration
	file is not available the routine asks the user
	for any missing terminal information.  The
	dc_set_inrcdr, dc_set_outrcdr and dc_get_config
	routines need not be called if this routine is
	used.  It returns PASS or FAIL.
*/

int dcu_setup()
{
    char    q[30];
    char    id[30];
    int     term;

    if (dc_get_config() == FAIL)
    {
	printf("\nEnter the input DCRSi recorder id ('?' if unknown)-> ");
	gets(id);
	printf("\nEnter the input DCRSi tty port number-> ");
	gets(q);
	while ((term = atoi(q)) < 8 || term > 15)
	{
	    printf("\nInvalid port number (must be 8 thru 15).\n");
	    printf("\nEnter the input DCRSi port number ('q' to quit) -> ");
	    gets(q);
	    if (strchr(q,'q') != NULL)
		return(FAIL);
	}

	dc_set_inrcdr(id,term);
	printf("\nEnter the output DCRSi recorder id ('?' if unknown)-> ");
	gets(id);
	printf("\nEnter the output DCRSi tty port number-> ");
	gets(q);
	while ((term = atoi(q)) < 8 || term > 15)
	{
	    printf("\nInvalid port number (must be 8 thru 15).\n");
	    printf("\nEnter the output DCRSi port number ('q' to quit) -> ");
	    gets(q);
	    if (strchr(q,'q') != NULL)
		return(FAIL);
	}
	dc_set_outrcdr(id,term);
        if (dc_make_config() == PASS)
	    return(PASS);
	printf("Unable to configure DCRSi's.\n");
	return(FAIL);
    }
    else
	return(PASS);
}



/* dcu_connect(rcdr) ---------------------------------------------------
	This routine connects the DCRSi specified by rcdr to the tty 
	port specified by rcdr.  The recorder is initialized with 
	recording inhibited and parallel data, scanner motor and ECC 
	enabled.  If successful, the routine returns PASS.  dcu_setup 
	or dc_get_config must have been called prior to calling this 
	routine.
*/

int dcu_connect(rcdr)
int    rcdr;        /* Input or output recorder */
{
    char    resp[1000];

    /* delaying statement */
    printf("dcu_connect: rcdr %d\n", rcdr);

    if (dc_comm_init(rcdr) == PASS)

        /* Resetting recorder */
        if ((dc_reset(rcdr,resp) == PASS) & 
		(dc_identify(rcdr,resp) == PASS))
        {
	    printf("DCRSi %s is connected.\n",resp);
/*
	    if (dc_en_motor(rcdr,ON) == PASS) 
		if (dc_en_ecc(rcdr,ON) == PASS) 
		   if (dc_par_data(rcdr) == PASS)
			if (dc_en_rcd(rcdr,OFF) == PASS)
			   return(PASS);
	    dc_comm_close(rcdr);
*/
	    if (  (dc_en_motor(rcdr,ON) == PASS)
		& (dc_en_ecc(rcdr,ON) == PASS)
		& (dc_par_data(rcdr) == PASS)
		& (dc_en_rcd(rcdr,OFF) == PASS)  )
	        return(PASS);
            else
		dc_comm_close(rcdr);
        }
	else
	    dc_comm_close(rcdr);

    printf("Could not connect to DCRSi");
    return(FAIL);
}
