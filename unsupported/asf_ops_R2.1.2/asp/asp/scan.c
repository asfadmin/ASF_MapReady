/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* 11-29-95 CLW Add reader of df_proc_params & sensor_params respect to
		RADARSAT mode (ST1..ST7) */

#include <stdio.h>
#include <syslog.h>
#include <aspdecl.h>
#include <procfil.h>
#include <asp_msg.h>
#define FMT_RATIO    1767.9     /* ratio of format to prf */

extern char JOB_PATH[80];
extern char DEF_PATH[80];
extern TAPE_SEG_PTR seg_list;   /* tape segment list */
extern PREAM_FILE_PTR pream_list;
extern RQST_PTR Cur_Rqst;       /* pointer to current job request */
extern int asphw;
extern int cp_flag;
extern int do_scan;
extern int use_prev_scan;
extern int vbose;
extern DPP_FILE dpf;            /* default processing params file */
extern SP_FILE spf;             /* sensor parameters file */
extern int no_repeat_proc;
extern char frame_mode[];       /* amm */

extern int Write_CP_Error_Status;

float fmt_ratio;         	/* ratio of IDHT fmts to AMI fmts */
int fmt_len;                    /* avg fmt length(bit) for its prf */
int fst_len;                    /* 1st fmt length after srch-sync */
int fst_len24;                  /* 1st fmt length after srch-sync minus 24 */

/* scan_take() ------------------------------------------------------
	This routine activates scan programs 
*/

scan_take()
{
	TAPE_SEG_PTR sp;
	int segment, ans;
	int i;
	char string[132];

/* Determine to perform SCAN */

        if (vbose) printf("frame_mode=%s\n",frame_mode);
	do_scan = 0;
	if ( cp_flag ){
		p_clear_seg_list();
		ans = AspReadScanResults();
	        if ( ans == FAIL ) {
                     if (Write_CP_Error_Status != -2)        
			asp_msg(LOG_ERR,asp_messages[READ_SCAN_FILE_ERR]);
                     return( ans );
		} else if ( ans > 0 ) do_scan = 1;
	} else {
		if (p_rw_pp_file(0,"pp_regions0",0) == PASS) {
	    		p_rw_pream_file(0,"preambles");
	    		p_rw_postam_file(0,"postambles");
	    		p_link_pambles();
			if ( !no_repeat_proc ) { 
	       		   segment = 0;
	       		   for (sp = seg_list; sp != NULL; sp = sp->nxt_seg)
	   			segment++;
	       		   display_seg_list(segment);
	       		   asp_msg(LOG_DEBUG,"\nSCAN has been done for this request.\n");
	       		   if ( !use_prev_scan ){ 
	       		     ans = op_answer("...want to repeat this process");
	    		     if (ans == PASS) {
				p_clear_seg_list();
				do_scan = 1;
			     }
			   }
			}
		} else do_scan = 1;
	}

	/* Signal server to perform scan */

	if ( asphw && do_scan ){

		report_asp_status(ASP_SCAN);
                chdir( JOB_PATH );

		AspDisableKill();
                switch ( Cur_Rqst->take_id[0] ){
                case 'E':
                        ans = e1_scan_take();
                        break;
                case 'J':
                        ans = j1_scan_take();
                        break;
                case 'R':
                        ans = r1_scan_take();
                        break;
                default:
                        asp_msg(LOG_DEBUG,"SCAN: Unsupported satellite.\n");
                        ans = FAIL;
                        break;
                }
		AspEnableKill();
                /* save segments on disk */

                if (ans == FAIL) return ( ans );

                p_rw_pream_file(1,"preambles");
                p_rw_postam_file(1,"postambles");
                p_rw_pp_file(1,"pp_regions0",0);

		if ( !cp_flag ){
	           segment = 0;
	           for (sp = seg_list; sp != NULL; sp = sp->nxt_seg)
	   		segment++;
	           display_seg_list(segment);
	           ans = op_answer("\nSCAN done ... want to proceed?");
	    	   if (ans == FAIL) return ( ans );
		}

	} /* asphw if */
	else if ( cp_flag && do_scan ){  /* testing only */

		if (vbose) printf("Searching for pp_regions0 & preambles...\n");
                chdir( JOB_PATH );
		if (p_rw_pp_file(0,"pp_regions0",0) == PASS) {
	    		p_rw_pream_file(0,"preambles");
	    		p_rw_postam_file(0,"postambles");
	    		p_link_pambles();
		} else if (p_rw_pp_file(0,"/ASP/cptest/pp_regions0",0)==PASS) {
	    		p_rw_pream_file(0,"/ASP/cptest/preambles");
	    		p_rw_postam_file(0,"/ASP/cptest/postambles");
	    		p_link_pambles();
		}
		else return(FAIL);
	}
	
    /* Calculate fmt_ratio for ERS */

	for (sp = seg_list; sp != NULL && sp->pre == NULL; sp = sp->nxt_seg);
        if (sp != NULL)
	  if ( Cur_Rqst->take_id[0] == 'J' ){
                if ((int)sp->pre->prf == 1505) fst_len = 39844;
                if ((int)sp->pre->prf == 1530) fst_len = 39212;
                if ((int)sp->pre->prf == 1555) fst_len = 38580;
                if ((int)sp->pre->prf == 1581) fst_len = 37948;
                if ((int)sp->pre->prf == 1606) fst_len = 37358;
                if ((int)sp->pre->prf == 1646) fst_len = 37648; /* for seasat */
                fmt_len = fst_len;
                fst_len += 1; 
		fst_len24 = fst_len - 24;
	   } else if ( Cur_Rqst->take_id[0] == 'E' ){
		if (sp != NULL) fmt_ratio = FMT_RATIO / sp->pre->prf;
	     }

/* Modify by CV 3/8/96 to read in the sensor file for stand alone exe */

	if ( no_repeat_proc ){
	    if ( Cur_Rqst->take_id[0] == 'R' ){
	      for (sp = seg_list; sp != NULL; sp = sp->nxt_seg) { 
		sprintf(string,"%sdf_proc_params.%d",DEF_PATH,
				sp->aux.beam_seq);
		if (vbose) printf("Reading %s\n",string);
		if (p_get_dpp_file(string,&dpf) != PASS) return(FAIL);

                if (!strcmp(frame_mode,"ANTARCTIC")){ /* amm */
                    sprintf(string,"%ssensor_params.%d.L",DEF_PATH,
                         sp->aux.beam_seq);
                } else {
		    sprintf(string,"%ssensor_params.%d",DEF_PATH,
				sp->aux.beam_seq);
		}
		if (vbose) printf("Reading %s\n",string);
		if (p_get_sp_file(string,&spf) != PASS) return(FAIL);
	      }
	    }
	    return (PASS);
	}

    /* create preprocessing region blocks for each segment */
	segment = 0;
	ans = FAIL;
	if (!cp_flag && !strcmp(Cur_Rqst->type,"QLK")) {
		if (select_segment() == FAIL) return (FAIL);
	}
	report_asp_status(ASP_PPR);

	for (sp = seg_list, ans = PASS; 
			(sp != NULL) && (ans == PASS); sp = sp->nxt_seg) {

	    if ( Cur_Rqst->take_id[0] == 'R' ){
		sprintf(string,"%sdf_proc_params.%d",DEF_PATH,
				sp->aux.beam_seq);
		if (vbose) printf("Reading %s\n",string);
		if (p_get_dpp_file(string,&dpf) != PASS) return(FAIL);

                if (!strcmp(frame_mode,"ANTARCTIC")){ /* amm */
                    sprintf(string,"%ssensor_params.%d.L",DEF_PATH,
                         sp->aux.beam_seq);
                } else {
		    sprintf(string,"%ssensor_params.%d",DEF_PATH,
				sp->aux.beam_seq);
		}
		if (vbose) printf("Reading %s\n",string);
		if (p_get_sp_file(string,&spf) != PASS) return(FAIL);
	    }
	    asp_msg(LOG_DEBUG,"Making preprocessing regions for segment %d\n",
			++segment);
	    switch ( Cur_Rqst->take_id[0] ){
		case 'E':
/*
	    		ans = e1_make_pprs(sp);
*/
	    		ans = make_fix_ctr(sp);
			break;
		case 'J':
/*
	    		ans = j1_make_pprs(sp,segment);
*/
	    		ans = make_fix_ctr(sp);
			break;
		case 'R' :
/*
			ans = r1_make_pprs(sp);
*/
	    		ans = make_fix_ctr(sp);
			break;
		default:
			ans = FAIL;
	    		asp_msg(LOG_DEBUG,"Satellite data not supported\n");
			break;
	    }	
	}
	return(ans);
}
