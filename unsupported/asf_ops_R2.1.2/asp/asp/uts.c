/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <procfil.h>
#include <procdec.h>

#define SEPCHAR "-T:"
#define FAIL -1
#define PASS 0
#define MBITS_PER_BYTE 1000*1000/8
#define DCRSi_BLKLEN  4356       /* bytes per DCRSi block */

extern char vbose;
extern char JOBS_PATH[];
extern float gaintbl[900];

write_job_file( rp, sv, tcf )
	RQST_PTR rp;
	SV_PTR sv;
	TC_FILE_PTR tcf;
{
	FILE *fp;
	char filename[132];
	int i,is,fs;
	static char *sv_type[] = { "UNKNOWN", "PREDICTED", 
				"RESTITUTED", "PRELIMINARY", "PRECISION" };

	sprintf(filename,"%s%s",JOBS_PATH,rp->jobname);
	printf("write_job_file: writing %s\n",filename);
    	if((fp = fopen(filename,"w+"))==NULL){
		printf("Cannot open %s\n",filename);
		return (FAIL);
    	}

/* print rp data */

	fprintf(fp,"%-29.29s\n",rp->id);
	fprintf(fp,"%-3.3s\n",rp->type);
	fprintf(fp,"%-32.32s\n",rp->site);
	fprintf(fp,"%-13.13s\n",rp->take_id);
/*
	fprintf(fp,"%-6.6s\n",rp->tape_id);
*/
	fprintf(fp,"%-12.12s\n",rp->tape_id);
	fprintf(fp,"%08.8d\n",rp->start_blk);
	fprintf(fp,"%08.8d\n",rp->end_blk);
	fprintf(fp,"%04.4d\n",rp->start.yr);
	/* 5/23/96 processor can not handle second = 00.xxx */
	is = (int)(rp->start.second);
	fs = (int)((rp->start.second - (float)(is))*1000.0+0.5);
	fprintf(fp,"%03.3d:%02.2d:%02.2d:%02.2d.%03.3d\n",rp->start.day,
			rp->start.hr, rp->start.min, is, fs);
	fprintf(fp,"%04.4d\n",rp->end.yr);
	is = (int)(rp->end.second);
	fs = (int)((rp->end.second - (float)(is))*1000.0+0.5);
	fprintf(fp,"%03.3d:%02.2d:%02.2d:%02.2d.%03.3d\n",rp->end.day,
			rp->end.hr, rp->end.min, is, fs);
	if (rp->lat == -1000.) {
		fprintf(fp,"        \n");
		fprintf(fp,"        \n");
	} else {
		fprintf(fp,"%-8.3f\n",rp->lat);
		fprintf(fp,"%-8.3f\n",rp->lon);
	}
		/*  12345678901234567890 */
	fprintf(fp,"    \n");
	fprintf(fp,"                \n");
	fprintf(fp,"    \n");
/*
	fprintf(fp,"%04.4d\n",rp->targ.yr);
	fprintf(fp,"%03.3d:%02.2d:%02.2d:%06.3f\n",rp->targ.day,
			rp->targ.hr, rp->targ.min, rp->targ.second);
	fprintf(fp,"%-4d\n",rp->targ_rg);
*/
	fprintf(fp,"%-6.2f\n",rp->ave_hght);
	fprintf(fp,"%+2.2d\n",rp->proc_gain);
	fprintf(fp,"%-3.3s\n",rp->deskew);
	fprintf(fp,"%-6.6s\n",rp->gnd_slnt_rg);

/* print sv data twice */

	for ( i = 0; i < 2; i++ ){

	fprintf(fp,"%-29.29s\n",rp->id);
	fprintf(fp,"STV\n");
	fprintf(fp,"%2.2s\n",rp->take_id);
	fprintf(fp,"%-11.11s\n",sv_type[sv->precision]);
	fprintf(fp,"TRUE EQUATORIAL    \n");
	fprintf(fp,"%5.5d\n",sv->rev);
	fprintf(fp,"%04.4d\n",sv->gmt.yr);
	/* 3/05/97 DEC-ALPHA can not handle second = 0000.xxx */
	is = (int)(sv->gmt.second);
	fs = (int)((sv->gmt.second - (float)(is))*1000.0+0.5);
	fprintf(fp,"%03.3d:%02.2d:%02.2d:%02.2d.%03.3d\n",sv->gmt.day,
			sv->gmt.hr, sv->gmt.min, is,fs);
	fprintf(fp,"%-11.5f\n",sv->pos.x);
	fprintf(fp,"%-11.5f\n",sv->pos.y);
	fprintf(fp,"%-11.5f\n",sv->pos.z);
	fprintf(fp,"%-11.5f\n",sv->vel.x);
	fprintf(fp,"%-11.5f\n",sv->vel.y);
	fprintf(fp,"%-11.5f\n",sv->vel.z);

	} /* end of for */

/* print tcf data */

	fprintf(fp,"%-29.29s\n",rp->id);
	fprintf(fp,"TCE\n");
	fprintf(fp,"%5.5d\n",tcf->rev);
	fprintf(fp,"%04.4d\n",tcf->gmt.yr);
/*	if (tcf->gmt.second < 1.0) tcf->gmt.second = 1.0;
	fprintf(fp,"%03.3d:%02.2d:%02.2d:%06.3f\n",tcf->gmt.day,
			tcf->gmt.hr, tcf->gmt.min, tcf->gmt.second);
*/
	/* Modify on 9/4/96 due to c-compiler can not handle 00.xxx */
	is = (int)(tcf->gmt.second);  
	fs = (int)((tcf->gmt.second - (float)(is))*1000.0+0.5);
	fprintf(fp,"%03.3d:%02.2d:%02.2d:%02.2d.%03.3d\n",tcf->gmt.day,
			tcf->gmt.hr, tcf->gmt.min, is, fs);
	fprintf(fp,"%-11.11u\n",tcf->bt);
	if (tcf->delta < 0) 
	    fprintf(fp,"%-11.10d\n",tcf->delta);
	else 
	    fprintf(fp,"%-11.11d\n",tcf->delta);
    	fclose(fp);
} /* end of write_job_file */

get_gmt(conv_date, gmt) 
	char *conv_date;
	GMT *gmt;
{
	char w[10][10], y[10];

	strcpy(y, strtok(conv_date,"-"));
	strcpy(w[0], strtok(NULL,SEPCHAR));
	strcpy(w[1], strtok(NULL,SEPCHAR));
	strcpy(w[2], strtok(NULL,SEPCHAR));
	strcpy(w[3], strtok(NULL,"\0"));

	gmt->yr = atoi(y);
	gmt->day = atoi(w[0]);
	gmt->hr= atoi(w[1]);
	gmt->min= atoi(w[2]);
	gmt->second= atof(w[3]);
	
} /* end of get_gmt */

int GetCalFile( filename )
	char *filename;
{
	int n, cnt;
	char t[132];

	if (vbose) printf("GetCalFile: reading %s\n", filename);
	if ( open_file(filename,"") == 0 ){
		printf("Cannot open %s\n",filename);
		return (FAIL);
    	}
	/*set_separators(", ");*/
	set_separators(" ");
	for (cnt = 0, n = next_token(t); n > 0; cnt++, n = next_token(t)){
		gaintbl[cnt] = atof(t);
/*		printf("data[%d]=%g\n",cnt,gaintbl[cnt]);  */
	}
	close_file();
	return(cnt);
}
SaveFrameResults( side, rq, sp_input )
	char side;
	RQST_PTR rq;
	TAPE_SEG_PTR sp_input;
{
	double deltat = 0.1;
	double dist2, dsec, sec, mills, get_gmt_diff();
	double xx, yy, zz, dd1, dd2;
	double x_begin[6], x_mid[6], x_end[6];
	GMT gmt;
	TAPE_SEG_PTR sp;
	PP_BLOCK_PTR pp;
	double a[3],b[3],c[3],d[3],e[3];
	double dist, time; 
	int center_fmt;
	int fst_len;
	int istat, nfmts;
	double r3p[3], fd3p[3], fr3p[3];
		
	dist = ((strcmp(rq->type,"STD") == 0) ||
		(strcmp(rq->type,"QLK") == 0)) ? 44800.0 : 51200.0;
	dist2 = strcmp(rq->type,"CPX") ? 102400. : 51200.;

	for ( sp = sp_input; sp != NULL; sp = sp->nxt_seg ){

	   for ( pp = sp->ppb_list; pp!=NULL; pp = pp->nxt_ppb ){

		time = dist / pp->swth_speed;

		/* Get the actually center fmt */
		if ( rq->take_id[0] == 'R' )
			center_fmt = pp->fmt_start; 
		else if ( rq->take_id[0] == 'E' )
			center_fmt = pp->fmt_id;
		else if ( rq->take_id[0] == 'J' )
			center_fmt = pp->fmt_start;
/*			center_fmt = pp->fmt_start +
					time * sp->pre->prf + 0.5; */

		/* calculate the actual starting format */

		pp->fmt_start = center_fmt - time * sp->pre->prf + 0.5;
		pp->center_gmt = pp->sv.gmt;

		/* calculate the actual starting gmt */

		pp->start_gmt = pp->sv.gmt;
		dsec = (pp->fmt_start - center_fmt) / sp->pre->prf;
	        newtprop_(&pp->sv.pos,&dsec,&deltat,x_begin);
		add_seconds( &pp->start_gmt, dsec );

		pp->end_gmt = pp->start_gmt;
		dsec = dist2 / pp->swth_speed;
	        newtprop_(&pp->sv.pos,&dsec,&deltat,x_end);
		add_seconds( &pp->end_gmt, dsec );
	
		xx = x_begin[0] * x_begin[0];
		yy = x_begin[1] * x_begin[1];
		zz = x_begin[2] * x_begin[2];
		dd1 = (xx + yy) / (xx + yy + zz);
		dd1 = acos(sqrt(dd1));
		xx = x_end[0] * x_end[0];
		yy = x_end[1] * x_end[1];
		zz = x_end[2] * x_end[2];
		dd2 = (xx + yy) / (xx + yy + zz);
		dd2 = acos(sqrt(dd2));
		if (pp->sv.pos.z > 0.0) {	/* N-pole */
		   if (dd2 > dd1)
	    		pp->asc_dsc = 'A';
		   else 
	    		pp->asc_dsc = 'D';
		}
		else {				/* S-pole */
		   if (dd2 < dd1)
	    		pp->asc_dsc = 'A';
		   else 
	    		pp->asc_dsc = 'D';
		}
		if ( rq->take_id[0] == 'E' ) {
		   e1_get_fmt_loc(-1,pp->fmt_start,sp,
					&pp->blk_start,&pp->bit_off);
            	} else if ( rq->take_id[0] == 'J' ) {
		   j1_get_fmt_loc(0,sp,pp->fmt_start,
				&fst_len,&pp->blk_start,&pp->bit_off);
            	} else if ( rq->take_id[0] == 'R' ) {
                   r1_get_fmt_loc(0,sp,pp->fmt_start,
					&pp->blk_start,&pp->bit_off);
            	}
/*SONY*/
		if ( sp->polarity ) 
			pp->blk_end = pp->blk_start + 
				dsec*105*MBITS_PER_BYTE/TAPE_BLOCK_LEN; 
		else
			pp->blk_end = pp->blk_start + 
				dsec*85*MBITS_PER_BYTE/TAPE_BLOCK_LEN; 
		istat = 0;
		nfmts = dsec * sp->pre->prf + 0.5;	
		r3p[0] = pp->r_close;
		r3p[1] = pp->r_mid;
		r3p[2] = pp->r_far;
		fd3p[0] = 0;
		fd3p[1] = 0;
		fd3p[2] = 0;
		fr3p[0] = -2200.0;
		fr3p[1] = -2200.0;
		fr3p[2] = -2200.0;
		get_corn_loc(sp,pp,pp->start_gmt,
				nfmts,r3p,fd3p,fr3p,0,0.0,
				a,b,c,d,e,&istat);

	        if (side == 'R') {  /* right looking radar:   */
	            pp->lat_a = a[0];  
	            pp->lon_a = a[1]; 
	            pp->lat_b = b[0];
	            pp->lon_b = b[1];
	            pp->lat_d = d[0];
	            pp->lon_d = d[1];
	            pp->lat_e = e[0];
	            pp->lon_e = e[1];
	        }
	        else {                  /* left looking radar:        */
	            pp->lat_a = d[0];    
	            pp->lon_a = d[1];   
	            pp->lat_b = e[0];
	            pp->lon_b = e[1];
	            pp->lat_d = a[0];
	            pp->lon_d = a[1];
	            pp->lat_e = b[0];
	            pp->lon_e = b[1];
	        }
	   } /* end of pp list */
	} /* end of sp list */
} /* end of SaveFrameResults */
