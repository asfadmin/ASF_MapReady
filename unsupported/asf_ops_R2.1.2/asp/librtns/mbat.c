/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbat.c - trigger routine */

#include  <stdio.h>
#include  <aspdecl.h>

extern int no_break;		/* 0 = ctrl-C has been entered */
extern int alarm;		/* 1 = timer alarm went off */
extern int vbose;

/* t() ---------------------------------------------------
	This routine arms the Alaska SAR exec module, and then
	waits for the exec to signal that the response has
	been written.  The routine returns the address of 
	the Response buffer when trigger occurred.
*/
int t()
{	
    int    freezeaddr, save, count;
/*
printf("T() Trigger...reseting STIM buffer...\n");
*/
    count = 0;
    mb.u[RESP_PAGE_PTR] = -1;
    mb.u[SYNC_PAGE_PTR] = -1;
    /* Stimulus buffer in run mode normal, reset */
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    mb.w[RLOC_STIM] = (mb.w[RLOC_STIM] & 0xfff0) | 0x4;
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
/*
printf("T() Trigger...RESP buffer in run mode normal...\n");
*/
    /* Response buffer in run mode normal */
    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    mb.w[RLOC_RESP] = (mb.w[RLOC_RESP] & 0xfff8) | 0x4;
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
/*
printf("T() Trigger...Arm RESP buffer...\n");
*/
    /* Arm response buffer */
    mb.w[RLOC_RESP + 4] = 0;
    asp_write( (RLOC_RESP+4)<<1, &mb.w[RLOC_RESP+4], 2 );
/*
printf("T() Trigger...Remove reset from STIM...\n");
*/
    /* Remove reset */
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    mb.w[RLOC_STIM] |= 0x8;
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );

    /* Set break routine */
    set_break();
/*
printf("T() Trigger...Waiting for reset to reach RESP...\n");
*/
    /* Wait until removed reset reaches resp board (disarmed) */
    asp_read( (RLOC_RESP+4)<<1, &mb.w[RLOC_RESP+4], 2 );
    while ((((save = mb.w[RLOC_RESP + 4]) & 0x1) == 0x1) && no_break){
	usleep( 5000 );
	asp_read( (RLOC_RESP+4)<<1, &mb.w[RLOC_RESP+4], 2 );
    }
/*
printf("T() Trigger...Waiting for end of write...\n");
*/
    /* Wait for the end of write */
    asp_read( (RLOC_RESP+3)<<1, &mb.w[RLOC_RESP+3], 2 );
    while (((mb.w[RLOC_RESP + 3] & 0xffff) == 0xfffe) && no_break &&
      (count < 2000)){
	usleep( 5000 );
	asp_read( (RLOC_RESP+3)<<1, &mb.w[RLOC_RESP+3], 2 );
	count += 1;
    }
    if (count >= 2000) { 
	if (vbose) printf("forced exit\n");
    }
/*
printf("T() Trigger...Put RESP into MB access mode...\n");
*/
    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    mb.w[RLOC_RESP] &= 0xfff8;
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );

    /* Get response buffer address */
    asp_read( (RLOC_RESP+1)<<1, &mb.w[RLOC_RESP+1], 4 );
    freezeaddr = mb.w[RLOC_RESP + 2] << 16;
    freezeaddr |= mb.w[RLOC_RESP + 1] & 0xffff;
/*
printf("T() Trigger...Freeze address: %08x\n", freezeaddr );
*/

/*
#ifdef DEBUG
    printf("trigger done, freezeaddr=%x\n",freezeaddr);
#endif
*/
    if (no_break) return(freezeaddr);
    else return(-1);
}

/* tarm() ---------------------------------------------------
	This routine is a partial derivative of the t() trigger routine
	above.  It only arms the exec module, but does not wait
	for response to be written.
*/
int tarm()
{	
    int    save;

    mb.u[RESP_PAGE_PTR] = -1;
    mb.u[SYNC_PAGE_PTR] = -1;
    /* Stimulus buffer in run mode normal, reset */
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    mb.w[RLOC_STIM] = (mb.w[RLOC_STIM] & 0xfff0) | 0x4;
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );

    /* Response buffer in run mode normal */
    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    mb.w[RLOC_RESP] = (mb.w[RLOC_RESP] & 0xfff8) | 0x4;
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );

    /* Arm response buffer */
    mb.w[RLOC_RESP + 4] = 0;
    asp_write( (RLOC_RESP+4)<<1, &mb.w[RLOC_RESP+4], 2 );

    /* Remove reset */
    asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
    mb.w[RLOC_STIM] |= 0x8;
    asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );

    /* Set break routine */
    set_break();

    /* Wait until removed reset reaches resp board (disarmed) */
    asp_read( (RLOC_RESP+4)<<1, &mb.w[RLOC_RESP+4], 2 );
    while ((((save = mb.w[RLOC_RESP + 4]) & 0x1) == 0x1) && no_break){
	usleep( 5000 );
	asp_read( (RLOC_RESP+4)<<1, &mb.w[RLOC_RESP+4], 2 );
    }

#ifdef DEBUG
    printf("trigger arm done\n");
#endif
    if (no_break) return;
    else {
	printf ("tarm fail: reset did not reach resp board\n");
	return(-1);
    }
}

/* tdone() ---------------------------------------------------
	This routine is a partial derivative of the t() trigger routine
	above.  It waits for the exec to signal that the response has
	been written.  The routine returns the address of 
	the Response buffer when trigger occurred.
*/
int tdone()
{	
    int    freezeaddr;
    int	   timeout = 0;

    mb.u[RESP_PAGE_PTR] = -1;
    mb.u[SYNC_PAGE_PTR] = -1;
    /* Set break routine */
    set_break();

    /* Wait for the end of write */
    asp_read( (RLOC_RESP+3)<<1, &mb.w[RLOC_RESP+3], 2 );
    while (((mb.w[RLOC_RESP + 3] & 0xffff) == 0xfffe) && no_break){
	usleep( 5000 );
/*	printf("%x; ",mb.w[RLOC_RESP+3]); */
	asp_read( (RLOC_RESP+3)<<1, &mb.w[RLOC_RESP+3], 2 );
/*	printf("VUU TEST OK HERE, nobreak=%d, %x\n",no_break,mb.w[RLOC_RESP+3]);
*/
    }
    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    mb.w[RLOC_RESP] &= 0xfff8;  /* Resp in mb access mode */
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );

    /* Get response buffer address */
    asp_read( (RLOC_RESP+1)<<1, &mb.w[RLOC_RESP+1], 4 );
    freezeaddr = mb.w[RLOC_RESP + 2] << 16;
    freezeaddr |= mb.w[RLOC_RESP + 1] & 0xffff;

/*
#ifdef DEBUG
    printf("trigger done, freezeaddr=%x\n",freezeaddr);
#endif
*/
    if (no_break) return(freezeaddr);
    else {
	printf ("tdone fail: no end of write\n");
	return(-1);
    }
}

/* tddone() ---------------------------------------------------
	This routine is a partial derivative of the t() trigger routine
	above.  It waits for the exec to signal that the response has
	been written.  The routine returns the address of 
	the Response buffer when trigger occurred.
	This routine differs from tdone in that it tests the RPI
	board sync dropout flag, and quits if the flag is set.
*/

int tddone()
{	
    int    freezeaddr;
    int    save;
    int timeout;

    mb.u[RESP_PAGE_PTR] = -1;
    mb.u[SYNC_PAGE_PTR] = -1;
    /* Set break routine */
    set_break();

    /* Wait for the end of write or timeout */
/*    set_alarm(60);	*/
    timeout = 0;
    asp_read( (RLOC_RESP+3)<<1, &mb.w[RLOC_RESP+3], 2 );
    while (((mb.w[RLOC_RESP + 3] & 0xffff) == 0xfffe) && no_break
		&& timeout < 6000 ) {
/*		&& alarm == 0) {	*/
	asp_read( (RLOC_EDFM+3)<<1, &mb.w[RLOC_EDFM+3], 2 );
	save = mb.w[RLOC_EDFM + 3];
	if ((save & 1) == 1) {
	    printf("trigger lost sync\n");
	    break;
	}
	usleep( 10000 );
	timeout++;
	asp_read( (RLOC_RESP+3)<<1, &mb.w[RLOC_RESP+3], 2 );
    }
/*    if (alarm != 0) printf("timeout in tddone ...\n");
    set_alarm(0);	*/
    if ( timeout>=6000 ) printf("timeout in tddone ...\n");
    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    mb.w[RLOC_RESP] &= 0xfff8;  /* Resp in mb access mode */
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );

    /* Get response buffer address */
    asp_read( (RLOC_RESP+1)<<1, &mb.w[RLOC_RESP+1], 4 );
    freezeaddr = mb.w[RLOC_RESP + 2] << 16;
    freezeaddr |= mb.w[RLOC_RESP + 1] & 0xffff;

/*
#ifdef DEBUG
    printf("trigger done, freezeaddr=%x\n",freezeaddr);
#endif
*/
    if (no_break) return(freezeaddr);
    else {
	printf ("tdone fail: no end of write\n");
	if (abob()) exit(1);
	return(-1);
    }
}

/* tjdone() ---------------------------------------------------
	This routine is JERS version of tddone.

	This routine is a partial derivative of the t() trigger routine
	above.  It waits for the exec to signal that the response has
	been written.  The routine returns the address of 
	the Response buffer when trigger occurred.
	This routine differs from tdone in that it tests the RPI
	board sync dropout flag, and quits if the flag is set.
*/
int tjdone()
{	
    int    freezeaddr;
    int    save;
    int    timeout = 0;

    mb.u[RESP_PAGE_PTR] = -1;
    mb.u[SYNC_PAGE_PTR] = -1;
    /* Set break routine */
    set_break();

    /* Wait for the end of write */
    asp_read( (RLOC_RESP+3)<<1, &mb.w[RLOC_RESP+3], 2 );
    while (((mb.w[RLOC_RESP + 3] & 0xffff) == 0xfffe) && no_break
	&& (timeout < 6000)) {
/*      asp_read( (RLOC_JDFM+7)<<1, &mb.w[RLOC_JDFM+7], 2 );
	save = mb.w[RLOC_JDFM + 7];
	if ((save & 4000) == 0) {
	    printf("trigger lost sync\n");
	    break;
	}
comment out CV 12/12/97*/

	usleep( 5000 );
	timeout++;
	asp_read( (RLOC_RESP+3)<<1, &mb.w[RLOC_RESP+3], 2 );
    }
    if (timeout >= 6000) 
	printf("timeout in tjdone ........\n");
    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
    mb.w[RLOC_RESP] &= 0xfff8;  /* Resp in mb access mode */
    asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );

    /* Get response buffer address */
    asp_read( (RLOC_RESP+1)<<1, &mb.w[RLOC_RESP+1], 4 );
    freezeaddr = mb.w[RLOC_RESP + 2] << 16;
    freezeaddr |= mb.w[RLOC_RESP + 1] & 0xffff;

/*
#ifdef DEBUG
    printf("trigger done, freezeaddr=%x\n",freezeaddr);
#endif
*/
    if (no_break) return(freezeaddr);
    else {
	printf ("tjdone fail: no end of write\n");
	return(-1);
    }
}
