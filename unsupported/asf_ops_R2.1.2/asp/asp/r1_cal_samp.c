/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <procdec.h>
#include <procfil.h>
#include <math.h>
#include <ctype.h>

/* r1_cal_samp.c -- calculate number of replica sample, number of
		bytes per format, convert time_unit of Rx window start
		time, Rx window duration, and PRF period   
*/

extern int del_time, first_time, vbose;

r1_cal_samp(fmt_no, aux, sp, prf_period, fmt_time)
	int fmt_no;
	RSAT_AUX_PTR aux;
        TAPE_SEG_PTR sp;
        double *prf_period;
	int *fmt_time;
{
#define NPFMTS 300;

	RSAT_AUX_DATA ap;
	int samp_code, b_seq;
	int win_start;
        int win_dur, diff_time, start_time;
	int second_time;
	double prf;
	int rep_valid;   	/* 1 = replica present
				   0 = no replica 	*/

  	if (r1_get_aux_data(fmt_no,&ap) == FAIL) {
		printf("fail in r1_get_aux\n");
		return (FAIL);
	}
	*aux = ap;
	b_seq = ap.beam_seq;			
	samp_code = ap.ADC_samp;
	rep_valid = ap.rep_pre;	/* if replica flag present */
	start_time = ap.time_all;
	win_dur = ap.win_duration; 	
	win_start = ap.window_start;
	prf = ap.PRF;

  	r1_samp(b_seq,samp_code,&prf,rep_valid,win_dur,win_start);
   	if (r1_get_aux_data(fmt_no+1, &ap) == FAIL){ 
		printf("fail in r1_get_aux\n");
		return (FAIL);
	}
	*prf_period = prf;
        *fmt_time = ap.time_all;
        if (sp->fmt_start == 0) {
                first_time = start_time;
                second_time = *fmt_time;
                del_time = second_time - first_time;
        } else *fmt_time = start_time;
        if (vbose) {
           printf("*prf_period= %g, fmt_time=%x, ",*prf_period,*fmt_time);
           printf("second=%x, first_time=%x, del_time=%x\n",
                second_time,first_time,del_time);
        }
}	
