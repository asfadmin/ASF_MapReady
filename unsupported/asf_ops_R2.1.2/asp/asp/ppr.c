/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <procfil.h>
#include <procdec.h>
#include <stdio.h>
#include <math.h>

extern RQST_PTR Cur_Rqst;       /* pointer to current job request */
extern TAPE_SEG_PTR seg_list;   /* tape segment list */
extern int frdelay_sw;      /* 1 = clamp frame delay to 8 */


/* build_proc_files() -----------------------------------------------
	This routine creates all the directories and files required to
	process the images of the current job.  The job must have been
	already preprocessed, and the preprocessing results should be
	loaded into the preprocessing data structures.
	    For each image to be processed, an image directory is
	created with the same name as the image ID.  All files required
	to process the image are recorded in this directory.
*/

build_proc_files()
{
	FILE *op;
	TAPE_SEG_PTR sp,sp2;
	PP_BLOCK_PTR pp;
	int seg = 0;
	int image = 0;
	int i,region,end_block;
	int regions = 0;
	int images = 0;
	int first = 1;
	float ber = 0.0;
	GMT start,end,ncstart,ncend;
	double ncdiff = 0.0;
	double get_gmt_diff(), diff;
	int ncflag = 0;
	int result,offset;

	printf("\nBuild processing files\n");

	frdelay_sw = 0;
    /* loop thru segments, creating processing files for each image */
	for (sp = seg_list; sp != NULL; sp = sp->nxt_seg) {
	    printf("-- Segment %d --\n",++seg);
	    if (first) {
		start = seg_list->start;
		end = seg_list->end;
		first = 0;
	    }
	/* check for transmission gap */
	    if ((sp2 = sp->nxt_seg) != NULL) {
		end = sp2->end;
		ncflag = 1;
		if (ncdiff == 0.0) {
		    ncstart = sp->end;
		    ncend = sp2->start;
		}
		if ((diff = get_gmt_diff(&sp2->start,&sp->end)) 
			> ncdiff) {
		    ncdiff = diff;
		    ncstart = sp->end;
		    ncend = sp2->start;
		}
	    }
	/* determine end block */
	    if ( Cur_Rqst->take_id[0] == 'E' ) {
	       if (sp->post != NULL)
		   end_block = sp->post->blk_start + 191;
	       else
		   e1_get_fmt_loc(sp->fmt_end,sp->end_id,sp,&end_block,&i);
	    } else if ( Cur_Rqst->take_id[0] == 'J' ) {
	       if (sp->post != NULL)
		   end_block = sp->post->blk_start + 1000;
	       else
		   end_block = sp->blk_end;
	    } else if ( Cur_Rqst->take_id[0] == 'R' ) {
		   r1_get_fmt_loc(0,sp,sp->fmt_end,&end_block,&i);
	    }
	    if (sp->ppb_list == NULL) {
		printf("...not enough data to make an image\n");
		printf("end block %d\n",end_block);
		continue;
	    }
	    region = 0;
	    image = 0;
	    for (pp = sp->ppb_list; pp != NULL; pp = pp->nxt_ppb) {
		region++;
		ber += pp->ber;
		image++;
	    }  /* for pp */
	    regions += region;
	    images += image;
	}  /* for sp */

    /* if no images to process, quit */
	if (images == 0) {
	    printf("...No images to process\n");
	    return (FAIL);
	}

    /* create and write the general status record */
	chdir(JOB_PATH);
	if ((op = fopen("general_status","w")) == NULL) {
	    perror("build_proc_files cannot create general_status file\n");
	    return (FAIL);
	}
	fprintf(op,"%-30s",Cur_Rqst->id);
	fprintf(op,"%-14s",Cur_Rqst->take_id);	/* take id */
	fprintf(op,"%-13s",Cur_Rqst->tape_id);	/* media id */
	fprintf(op,"%16d ",seg_list->blk_start);
	fprintf(op,"%16d ",end_block);
	put_gmt(op,&start);
	put_gmt(op,&end);
	if (regions)
	    ber /= (float) regions;
	fprintf(op,"%16.7g ",ber);
	if (ncflag) {
	    fprintf(op,"YES ");
	    put_gmt(op,&ncstart);
	    put_gmt(op,&ncend);
	}
	else {
	    fprintf(op,"NOT ");
	    fprintf(op,"                      ");
	    fprintf(op,"                      ");
	}
	fprintf(op,"%7d ",images);
	fclose(op);

	return (PASS);
}
/* put_gmt(op,gmt) --------------------------------------------------
	This routine formats the given gmt block as a 22-byte ascii
	string, and writes it to the stream op.
*/

put_gmt(op,gmt)
	FILE *op;
	GMT *gmt;
{
	int sec,mills;

	sec = (int) gmt->second;
	mills = (gmt->second - (float) sec) * 1000.0;
	fprintf(op,"%4.4d %3.3d:%2.2d:%2.2d:%2.2d.%3.3d ",
	    gmt->yr,gmt->day,gmt->hr,gmt->min,sec,mills);
}
