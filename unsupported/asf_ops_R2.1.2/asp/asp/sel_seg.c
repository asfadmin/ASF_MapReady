/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* sel_seg.c -- select a segment/sub-segment for processing

	This module contains routines which are only used during
	Quick-Look request processing.  The routines ask the user
	to pick a tape segment for processing, and optionally a
	start time and duration within the segment.  This allows the
	user to control what part of a data take is Quick-Look
	processed.
*/


#include <stdio.h>
#include <aspdecl.h>
#include <procdec.h>
#include <procfil.h>
#include <string.h>


extern RQST_PTR Cur_Rqst;		/* pointer to current job request */
extern TAPE_SEG_PTR seg_list;		/* tape segment list */
extern TAPE_SEG_PTR save_seg_list;	/* copy of tape segment list */


static char *system_hdr = "ALASKA SAR PROCESSOR OPERATING SYSTEM";
int skip_segment = 0;


/* display_seg_list(seg_count) -----------------------------------------
	This routine displays a list of the tape segments in the 
	current job request.
*/

display_seg_list(seg_count)
	int seg_count;
{
	TAPE_SEG_PTR sp;
	int lines, cols, hline, last_item, nlines, sec, nfmts;
	static int first_item = 1;
	int i, j, k;
	static char hd[] = "##  START TIME    END TIME     # OF SECONDS";
	static char tab[] = "            ";
	double dsec,get_gmt_diff();

	termsize (&lines, &cols);
	nlines = lines - 6;
	first_item = 1;
	last_item = first_item + nlines - 2;
	if (last_item > seg_count)
	    last_item = seg_count;
    /* display system header */
	for (k = 0; k < ((cols - strlen(system_hdr)) / 2); k++)
	    printf (" ");
	printf("%s\n",system_hdr);
	hline = (lines - seg_count - 3) / 2 + 1;
    /* display the list */
	sp = seg_list;
	for (i = 2, j = first_item; i < lines; i++) {
	    if (i == hline) {
	    /* print header */
		printf("%s%s",tab,hd);
	    }
	    if (i == hline + 2)
	    /* print "all segments" line */
	    /* 3/6/95 */
		if ( skip_segment ) printf("%s 0. Process all segments",tab);
		/* printf("%s 0. All segments",tab); */
	    if ((j <= last_item) && (i > hline + 2) && (sp != NULL)) {
		printf("%s%2d. ",tab,j);
		sec = sp->start.second;
		printf("%.3d:%.2d:%.2d:%.2d  ",sp->start.day,
		    sp->start.hr,sp->start.min,sec);
		sec = sp->end.second;
		printf("%.3d:%.2d:%.2d:%.2d  ",sp->end.day,
		    sp->end.hr,sp->end.min,sec);
		dsec = get_gmt_diff(&sp->end,&sp->start);
		printf("%6.3f",dsec);
		if (dsec < 20.0)
		    printf(" <-- too small");
		j++;
		sp = sp->nxt_seg;
	    }
	    printf("\n");
	}
}


/* select_segment() ----------------------------------------------------
	This routine displays the segment list menu, and lets the user
	select a segment by number.  The routine then modifies the
	segment list data structure to contain only the selected
	segment.  The user may select "all segments" as well.
	This routine is only used for Quick-Look requests.
*/

select_segment()
{
	double sec,dsec,startoff,get_gmt_diff(),atof();
	double dur = 0.0;
	int isec,blk_start,bit_off,format;
	int seg_count = 0;
	char q[100];
	char t[80];
	PP_BLOCK_PTR pp,pp2;
	TAPE_SEG_PTR sp,sp2;
	TAPE_SEG_PTR sp_last;
	TAPE_SEG_PTR sp_str = NULL;
	TAPE_SEG_PTR sp_end = NULL;

	int ans;

    /* display the segments to the operator */
	for (sp = seg_list; sp != NULL; sp = sp->nxt_seg)
	    seg_count++;
	    display_seg_list(seg_count);

	for (sp = seg_list; sp != NULL; sp = sp->nxt_seg)
	    sp_last = sp;
	sp = seg_list;
	sec = get_gmt_diff(&sp_last->end,&sp->start);
	isec = sp->start.second;

	sprintf(q, "\nDatatake starts at %.3d:%.2d:%.2d:%.2d\n",
		sp->start.day,sp->start.hr,sp->start.min,isec);
	printf( q );
	printf("...and is %g seconds long.\n",sec);

	ans = op_answer("Do you want to process the entire Datatake");
	if (ans == PASS)
	    return(PASS);

	while (dur == 0.0) {
	    printf("enter the offset (in seconds) from the start of the datatake\n");
	    printf("  to the start of the region to be processed: ");
	    gets(t);
	    startoff = atof(t);
	    if (startoff < 0.0 || startoff > sec) {
		printf("\07\n** offset out of bounds\n");
	    }
	    else {
		while (dur == 0.0) {
		    printf("enter the # of seconds to process: ");
		    gets(t);
		    dur = atof(t);
		    if (dur < 20.0) {
			printf("...must be at least 20 seconds\n");
			dur = 0.0;
		    }
		}
		if (startoff + dur > sec) {
		    printf("...requested area goes past end of datatake\n");
		    dur = 0.0;
		}
	    }  /* else */
	    if (startoff < 0.0)
	        startoff = 0.0;
	    if ((startoff + dur) > sec)
	        dur = sec - startoff;
    /* find the segments for start and end times */
	    for (sp = seg_list; sp != NULL; sp = sp->nxt_seg) {
	       dsec = get_gmt_diff(&sp->end,&seg_list->start);
	       if (dsec > startoff && sp_str == NULL) 
		    sp_str = sp;
	       if (dsec > (startoff + dur) && sp_end == NULL) 
		    sp_end = sp;
	    }
	    if (sp_end == NULL) sp_end = sp_str;
	    if (sp_str == NULL) {
	       printf("\07\n** trouble finding begin segment\n");
	       dur = 0.0;
	       sp_str = NULL; sp_end = NULL;
	    }
	    startoff -= get_gmt_diff(&sp_str->start,&seg_list->start);
	    if (startoff < 0.0) {
	       printf("\07\n** offset is in the middle of a gap.\n");
	       dur = 0.0;
	       sp_str = NULL; sp_end = NULL;
	    }
	}  /* while dur */

     /* free up the memory for segments that are not used */
	for (sp=seg_list; sp != NULL; sp=sp2) {
	     if (sp == sp_str) break;
	     sp2 = sp->nxt_seg;
	     for (pp=sp->ppb_list; pp != NULL; pp=pp2) {
		pp2 = pp->nxt_ppb;
		free(pp);
	     }
	     free(sp);
	}

	if (sp_end->nxt_seg != NULL) { 
	   for (sp=sp_end->nxt_seg; sp != NULL; sp=sp2) {
	     sp2 = sp->nxt_seg;
	     for (pp=sp->ppb_list; pp != NULL; pp=pp2) {
		pp2 = pp->nxt_ppb;
		free(pp);
	     }
	     free(sp);
	   }
	}
	seg_list = sp_str;
	sp_end->nxt_seg = NULL;

    /* adjust the start time and format number/location */
	add_seconds(&sp_str->start,startoff);
	get_time_fmt(sp_str,&sp_str->start,&format);
	if (Cur_Rqst->take_id[0] == 'E') {
	    e1_get_fmt_loc(-1,format,sp_str,&blk_start,
		&bit_off);
	    sp_str->fmt_start = -1;
	    sp_str->fmt_id = format;
	    sp_str->blk_start = blk_start;
	    sp_str->bit_off = bit_off;
	}

    /* adjust the end time and format number */
	sp_end->end = sp_str->start;
	add_seconds(&sp_end->end,dur);
	get_time_fmt(sp_end,&sp_end->end,&sp_end->end_id);
	sp_end->fmt_end = -1;

    /* Ming's cheap fix for JERS, DO NOT across segments. 11/19/92 */
	if (Cur_Rqst->take_id[0] == 'J') {
		sp_str->ppfmt[0] = format;
		sp_end->fmt_end = sp_end->end_id;
	}

	return(PASS);
	
}
