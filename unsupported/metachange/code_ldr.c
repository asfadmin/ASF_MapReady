/********************************************************************
FUNCTION NAME:   Code_###.c


SYNTAX: varies


DESCRIPTION:
 
 These procedures convert ASF metadata structures to and from their 
 ASCII form.  See ceos.h for info on the structures.
 To convert an ASCII buffer read in from a .ldr,.trl, or .L file, call
 
Code_###(buf,ceosStructure [,era] ,fromASCII);

 To convert that structure back into ASCII, call
 
Code_###(buf,ceosStructure [,era] ,toASCII);



PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           2/97   O. Lawlor (ASF)  converted from various (one-way) utility procedures

*********************************************************************/
#include <stdio.h>
#include <string.h>
#include "ceos.h"
#include "ddr.h"

typedef enum {toASCII,fromASCII} codingDirection;

/************** Enocder/Decoder Utilities *********************
	Since there's plenty of symmetry between reading an ASCII
buffer into a structure and writing the structure back out to a buffer,
and to ease modifications and creation of such routines, I've written
these routines to take the enumerated parameter above, (called dir), which takes the
values of toASCII or fromASCII.  The routines which pay attention to it
are below. 
*/
void flt2asc(double *in,unsigned char *outBuf,int len,codingDirection dir)
{
	int i;
	char tmp[255]= /*250 spaces follow.*/
"                                                  \
                                                  \
                                                  \
                                                  \
                                                  ";	
	if (dir==toASCII)
	{
		if (len>22)
			sprintf(tmp,"%23.14lg",*in);
		else if (len>16)
			sprintf(tmp,"%17.12g",*in);
		else if (len>15)
			sprintf(tmp,"%16.11lg",*in);
		else
			sprintf(tmp,"%lg",*in);
		for (i=0;tmp[i];) i++;
		tmp[i]=' ';/*eliminate terminating null.*/
		strncpy((char *)outBuf,tmp,len);
	} else /*dir==fromASCII*/
	{
		strncpy(tmp,(char *)outBuf,len);
		tmp[len]=0;/* add terminating null.*/
		sscanf(tmp,"%lg",in);
	}
}
void sing2asc(float *in,unsigned char *outBuf,int len,codingDirection dir)
{
	double d=*in;
	flt2asc(&d,outBuf,len,dir);
	*in=d;
}
void long2asc(long *in,unsigned char *outBuf,int len,codingDirection dir)
{	
	int i;
	char tmp[255]= /*250 spaces follow.*/
"                                                  \
                                                  \
                                                  \
                                                  \
                                                  ";
	if (dir==toASCII)
	{
		sprintf(tmp,"%ld",*in);
		for (i=0;tmp[i];) i++;
		tmp[i]=' ';/*eliminate terminating null.*/
		strncpy((char *)outBuf,tmp,len);
	} else /*dir==fromASCII*/
	{
		strncpy(tmp,(char *)outBuf,len);
		tmp[len]=0;/* add terminating null.*/
		sscanf(tmp,"%ld",in);
	}
}
void shrt2asc(short *in,unsigned char *outBuf,int len,codingDirection dir)
{
	long l=*in;
	long2asc(&l,outBuf,len,dir);
	*in=l;
}
void int2asc(int *in,unsigned char *outBuf,int len,codingDirection dir)
{
	long l=*in;
	long2asc(&l,outBuf,len,dir);
	*in=l;
}

void str2asc(char *inString,unsigned char *outBuf,int len,codingDirection dir)
{
	int i;
	if (dir==toASCII)
	{
		for (i=0;inString[i]&&i<len;i++)
			outBuf[i]=inString[i];
		for (i;i<len;i++)
			outBuf[i]=' ';
	} else /*dir==fromASCII*/
	{
		for (i=0;i<len;i++)
			inString[i]=outBuf[i];
		inString[len]=0;
	}
}   
/*These #defines use the above routines to convert 
    floats, ints, and strings to/from ascii.  We can
    do this because the offset into the ASCII buffer
    doesn't depend on the direction you're translating.
    Hence, the below routines just provide the mapping
    between the two formats.  */

#define fltV(qField,bufOff,bufSize) flt2asc(&q->qField,&buf[bufOff],bufSize,dir);off+=bufSize;
#define singV(qField,bufOff,bufSize) sing2asc(&q->qField,&buf[bufOff],bufSize,dir);off+=bufSize;
#define shrtV(qField,bufOff,bufSize) shrt2asc(&q->qField,&buf[bufOff],bufSize,dir);off+=bufSize;
#define intV(qField,bufOff,bufSize)	int2asc(&q->qField,&buf[bufOff],bufSize,dir);off+=bufSize;
#define longV(qField,bufOff,bufSize) long2asc(&q->qField,&buf[bufOff],bufSize,dir);off+=bufSize;
#define strV(qField,bufOff,bufSize)	str2asc(q->qField,&buf[bufOff],bufSize,dir);off+=bufSize;

#define fltA(thing,bufOff,bufSize) flt2asc(&thing,&buf[bufOff],bufSize,dir);off+=bufSize;
#define shrtA(thing,bufOff,bufSize) shrt2asc(&thing,&buf[bufOff],bufSize,dir);off+=bufSize;
#define intA(thing,bufOff,bufSize) int2asc(&thing,&buf[bufOff],bufSize,dir);off+=bufSize;
#define longA(thing,bufOff,bufSize) long2asc(&thing,&buf[bufOff],bufSize,dir);off+=bufSize;
#define strA(thing,bufOff,bufSize) str2asc(thing,&buf[bufOff],bufSize,dir);off+=bufSize;

void Code_DQS(buf, q, era,dir)
    unsigned char* buf;
    struct qual_sum_rec* q;
    int era;
    codingDirection dir;
{
    int off=0, i;
   	shrtV(seq_num,12,4);
    strV(chan_ind,16,4);
    strV(cali_date,20,6);
    shrtV(nchn,26,4);
    fltV(islr,30, 16);
    fltV(pslr,46, 16);
    fltV(azi_ambig,62, 16);
    fltV(rng_ambig,78, 16);
    fltV(snr,94, 16);
    fltV(ber,110, 16);
    fltV(rng_res,126, 16);
    fltV(azi_res,142, 16);
    fltV(rad_res,158, 16);
    fltV(dyn_rng,174, 16);
    fltV(abs_rad_unc_db,190, 16);
    fltV(abs_rad_unc_deg,206, 16);
    off=222;
    for (i=0; (i<q->nchn && i<16); i++) {
        fltV(rel_rad_unc[0][i],off,16);
       	fltV(rel_rad_unc[1][i],off,16);
    }
    fltV(alt_locerr,734,16);
    fltV(crt_locerr,750,16);
    fltV(alt_scale,766,16);
    fltV(crt_scale,782,16);
    fltV(dis_skew,798,16);
    fltV(ori_err,814,16);
    off=830;
    for (i=0; i<16; i++) {
        fltV(misreg[0][i],off, 16);
        fltV(misreg[1][i],off, 16);
    }
    if (era==1) {
      fltV(nesz,off,16);
      fltV(enl,off,16);
      strV(tb_update,off,8);
      strV(cal_status,off,16);
      strV(spare3,off,22);
      strV(cal_comment,off,200);
    }
}
void Code_RADDR(unsigned char *buf, struct VRADDR* q,codingDirection dir)
{
	struct RADDR    *dr=(struct RADDR *)buf;
	struct VRADDR   *vdr=q;
	int i;
#define uc (unsigned char *)
	int2asc(&vdr->seqnum,uc dr->seqnum,4,dir);
	int2asc(&vdr->datfield,uc dr->datfield,4,dir);
	int2asc(&vdr->setsize,uc dr->setsize,8,dir);
	
    str2asc(vdr->sarchan,uc dr->sarchan,4,dir);
    str2asc(vdr->luttype,uc dr->luttype,24,dir);
	int2asc(&vdr->nosample,uc dr->nosample,9,dir);
    str2asc(vdr->samptype,uc dr->samptype,16,dir);
	for (i = 0; i < 3; i++)   flt2asc(&vdr->a[i],uc dr->a[i],16,dir);
	for (i = 0; i < 256; i++) flt2asc(&vdr->noise[i],uc dr->noise[i],16,dir); 
}
void Code_IOF(unsigned char *buf, struct IOF_VFDR* i,codingDirection dir)
{
	struct IOF_FDR    *ifdr=(struct IOF_FDR *)(buf+12);
#define uc (unsigned char *)
	int2asc(&i->numofrec, uc ifdr->numofrec,6,dir);
    int2asc(&i->reclen, uc ifdr->reclen,6,dir);
    int2asc(&i->bitssamp, uc ifdr->bitssamp,4,dir);
    int2asc(&i->sampdata, uc ifdr->sampdata,4,dir);
    int2asc(&i->bytgroup, uc ifdr->bytgroup,4,dir);
    str2asc(i->justific,uc ifdr->justific,4,dir);
    int2asc(&i->sarchan, uc ifdr->sarchan,4,dir);
    int2asc(&i->linedata, uc ifdr->linedata,8,dir);
    int2asc(& i->lbrdrpxl, uc ifdr->lbrdrpxl,4,dir);
    int2asc(& i->datgroup, uc ifdr->datgroup,8,dir);
    int2asc(&i->rbrdrpxl, uc ifdr->rbrdrpxl,4,dir);
    int2asc(&i->topbrdr, uc ifdr->topbrdr,4,dir);
    int2asc(&i->botbrdr, uc ifdr->botbrdr,4,dir);
    str2asc(i->interlv,uc ifdr->interlv,4,dir);
    int2asc(&i->recline, uc ifdr->recline,2,dir);
    int2asc(&i->mrecline, uc ifdr->mrecline,2,dir);
    int2asc(&i->predata, uc ifdr->predata,4,dir);
    int2asc(&i->sardata, uc ifdr->sardata,8,dir);
    int2asc(&i->sufdata, uc ifdr->sufdata,4,dir);
    str2asc(i->repflag,uc ifdr->repflag,4,dir);
    str2asc(i->formatid,uc ifdr->formatid,28,dir);
    str2asc(i->formcode,uc ifdr->formcode,4,dir);
    int2asc(&i->leftfill, uc ifdr->leftfill,4,dir);
    int2asc(&i->rigtfill, uc ifdr->rigtfill,4,dir);
    int2asc(&i->maxidata, uc ifdr->maxidata,8,dir);
}
void  Code_DHR(buf, q,dir)
    unsigned char* buf;
    struct data_hist_rec* q;
    codingDirection dir;
{
	int i, j, off=0;
	struct hist_dset *d,*oldD, *e;
	long *dv;
	shrtV(seq_num,12, 4);
	shrtV(sar_chan,16, 4);
	longV(ntab,20, 8);
	longV(ltab,28, 8);
	off=36;
	if (dir==fromASCII)
		oldD=NULL;
	else
		d=q->data;
	for (j=0; j< (q->ntab); j++)
	{ 
		if (dir==fromASCII)
			d = (struct hist_dset *) malloc (sizeof(struct hist_dset));
		strA(d->hist_desc,off,32);
		shrtA(d->nrec,off, 4);
		shrtA(d->tab_seq,off, 4);
		longA(d->nbin,off, 8);
		longA(d->ns_lin,off, 8);
		longA(d->ns_pix,off, 8);
		longA(d->ngrp_lin,off, 8);
		longA(d->ngrp_pix,off, 8);
		longA(d->nsamp_lin,off, 8);
		longA(d->nsamp_pix,off, 8);
		fltA(d->min_smp,off, 16);
		fltA(d->max_smp,off, 16);
		fltA(d->mean_smp,off, 16);
		fltA(d->std_smp,off, 16);
		fltA(d->smp_inc,off, 16);
		fltA(d->min_hist,off, 16);
		fltA(d->max_hist,off, 16);
		fltA(d->mean_hist,off, 16);
		fltA(d->std_hist,off, 16);
		longA(d->nhist,off,8);
		if (dir==fromASCII)
			d->data_values_hist = (long *) malloc (sizeof(long)*d->nhist);
		dv = d->data_values_hist;
		for (i=0; i<d->nhist; i++) 
			{ longA(*(dv+i),off, 8); }
		if (dir==fromASCII)
		{
			d->next = NULL;
			if (!oldD) 
				oldD = q->data = d;
			else
				oldD = ( oldD->next = d);
		}
		else /*(dir==toASCII)*/
		{
			oldD=d;
			d=d->next;
			free(oldD->data_values_hist);
			free(oldD);
		}
	}
}

void Code_FACDR(buf, q,era,dir)
unsigned char *buf;
struct VFDRECV *q;
int era;
codingDirection dir;
{
	int off=20;
    strV(dataid,off,14);
    strV(imageid,off,11);
    strV(coryear,off,5);
    strV(cortime,off,17);
    strV(sitename,off,33);
    strV(imgyear,off,5);
    strV(imgtime,off,17);
    
	fltV(imgclat,off,17);
	fltV(imgclon,off,17);
	fltV(nearslat,off,17);
	fltV(nearslon,off,17);
	fltV(nearelat,off,17);
	fltV(nearelon,off,17);
	fltV(farslat,off,17);
	fltV(farslon,off,17);
	fltV(farelat,off,17);
	fltV(farelon,off,17);
	fltV(swazim,off,17);
	fltV(swrange,off,17);
	
	longV(npixels,off,9);
	longV(nlines,off,9);
	longV(apixels,off,9);
	longV(alines,off,9);
	
	strV(mediaid,off,7);
	strV(sblock,off,17);
	strV(eblock,off,17);
	strV(platform,off,17);
	strV(sensmode,off,33);
	
	fltV(prfreq,off,17);
	fltV(antlook,off,17);
	fltV(datarate,off,17);
	fltV(datawin,off,17);
	fltV(rangegd,off,17);
	fltV(trackang,off,17);
	
	strV(ascdesc,off,2);
	
	fltV(scalt,off,17);
	fltV(scxpos,off,23);
	fltV(scypos,off,23);
	fltV(sczpos,off,23);
	fltV(scxvel,off,23);
	fltV(scyvel,off,23);
	fltV(sczvel,off,23);
	fltV(scroll,off,15);
	fltV(scyaw,off,15);
	fltV(scpitch,off,15);
	
	intV(qroll,off,5);
	intV(qyaw,off,5);
	intV(qpitch,off,5);
	
	fltV(rroll,off,15);
	fltV(ryaw,off,15);
	fltV(rpitch,off,15);
	
	intV(rqroll,off,5);
	intV(rqyaw,off,5);
	intV(rqpitch,off,5);
	
	fltV(eradnadr,off,17);
	fltV(eradcntr,off,17);
	fltV(incedang,off,17);
	
	strV(procvers,off,8);
	strV(imgproct,off,3);
	strV(ephemert,off,2);
	
	fltV(nlooksaz,off,17);
	fltV(nlooksra,off,17);
	fltV(weightaz,off,17);
	fltV(weightra,off,17);
	
	strV(normener,off,4);
	
	fltV(indistaz,off,17);
	fltV(indistra,off,17);
	fltV(recgain,off,17);
	fltV(swathvel,off,17);
	fltV(squintan,off,17);
	fltV(avgterht,off,17);
	
	strV(procgain,off,4);
	strV(deskewf,off,4);
	strV(grndslnt,off,7);
	
	fltV(sltrngfp,off,17);
	fltV(sltrnglp,off,17);
	
	intV(strtsamp,off,9);
	
	strV(clttrlkf,off,4);
	
	fltV(dpplrfrq,off,17);
	fltV(dpplrslp,off,17);
	fltV(dpplrqdr,off,17);
	
	strV(autfocsf,off,4);
	
	fltV(dpplrrat,off,17);
	fltV(dpratslp,off,17);
	fltV(dpratqdr,off,17);
	fltV(imresaz,off,17);
	fltV(imresra,off,17);
	fltV(azpixspc,off,17);
	fltV(rapixspc,off,17);
	
	strV(rngcompf,off,4);
	
	intV(bitssamp,off,5);
	
	fltV(calestim,off,17);
	fltV(biterrrt,off,17);
	fltV(sigtonoi,off,17);
	fltV(estnoifl,off,17);
	fltV(radiores,off,17);
	
	intV(nsatpnts,off,9);
	
	strV(inspecf,off,4);
	if (era)
	{
		fltV(repl_agc,off,17);
		fltV(temp_rx_lna,off,17);
		fltV(temp_rx_sub,off,17);
		fltV(temp_rx_prot,off,17);
		fltV(temp_cal_sys,off,17);
		fltV(rx_agc,off,17);
		fltV(pre_cal1_pow,off,17);
		fltV(pre_cal2_pow,off,17);
		fltV(post_cal1_pow,off,17);
		fltV(post_cal2_pow,off,17);
		fltV(repl_pow,off,17);
		fltV(ssar_roll_ang,off,17);
		strV(comment,off,100);
	} 
	else strV(comment,off,100);
}

void  Code_DSSR(buf,q,era,dir)
unsigned char* buf;
struct dataset_sum_rec * q; 
int era;
codingDirection dir;
{
	int off=0;
	shrtV(seq_num,12,4);
	shrtV(sar_chan,16,4);
	strV(product_id,20,16);
	strV(scene_des,36,32);
	strV(inp_sctim,68,32);
	strV(asc_des,100,16);
	fltV(pro_lat,116,16);
	fltV(pro_long,132,16);
	fltV(pro_head,148,16);
	strV(ellip_des,164,16);
	fltV(ellip_maj,180,16);
	fltV(ellip_min,196,16);
	fltV(earth_mass,212,16);
	fltV(grav_const,228,16);
	fltV(ellip_j[0],244,16);
	fltV(ellip_j[1],260,16);
	fltV(ellip_j[2],276,16);
	fltV(terrain_h ,292,16); 
	fltV(sc_lin    ,308,16);
	fltV(sc_pix    ,324,16);
	if (era!=0)
	{
		fltV(terrain_h,308,16); 
		fltV(sc_lin,324,8);
		fltV(sc_pix,332,8);
	}
	fltV(scene_len ,340,16);
	fltV(scene_wid ,356,16);
	shrtV(nchn,388,4);
	strV(mission_id,396,16);
	strV(sensor_id,412,32);
	strV(revolution,444,8);
	fltV(plat_lat,452,8);
	fltV(plat_long,460,8);
	fltV(plat_head_scene,468,8);
	fltV(clock_ang,476,8);
	fltV(incident_ang,484,8);
	fltV(frequency,492,8);
	fltV(wave_length,500,16);
	strV(motion_comp,516,2);
	strV(pulse_code,518,16);
	fltV(ampl_coef[0],534,16);
	fltV(ampl_coef[1],550,16);
	fltV(ampl_coef[2],566,16);
	fltV(ampl_coef[3],582,16);
	fltV(ampl_coef[4],598,16);
	fltV(phas_coef[0],614,16);
	fltV(phas_coef[1],630,16);
	fltV(phas_coef[2],646,16);
	fltV(phas_coef[3],662,16);
	fltV(phas_coef[4],678,16);
	longV(chirp_ext_ind,694,8);
	fltV(rng_samp_rate,710,16);
	fltV(rng_gate,726,16);
	fltV(rng_length,742,16);
	strV(baseband_f,758,4);
	strV(rngcmp_f,762,4);
	fltV(gn_polar,766,16);
	fltV(gn_cross,782,16);
	longV(chn_bits,798,8);
	strV(quant_desc,806,12);
	fltV(i_bias,818,16);
	fltV(q_bias,834,16);
	fltV(iq_ratio,850,16);
	fltV(spare_dss_7,866,16);
	fltV(spare_dss_8,882,16);
	fltV(ele_sight,898,16);
	fltV(mech_sight,914,16);
	strV(echo_track,930,4);
	fltV(prf,934,16);
	fltV(elev_beam,950,16);
	fltV(azi_beam,966,16);
	strV(sat_bintim,982,16);
	strV(sat_clktim,998,32);
	longV(sat_clkinc,1030,8);
	strV(fac_id,1046,16);
	strV(sys_id,1062,8);
	strV(ver_id,1070,8);
	strV(fac_code,1078,16);
	strV(lev_code,1094,16);
	strV(product_type,1110,32);
	strV(algor_id,1142,32);
	fltV(n_azilok,1174,16);
	fltV(n_rnglok,1190,16);
	fltV(bnd_azilok,1206,16);
	fltV(bnd_rnglok,1222,16);
	fltV(bnd_azi,1238,16);
	fltV(bnd_rng,1254,16);
	strV(azi_weight,1270,32);
	strV(rng_weight,1302,32);
	strV(data_inpsrc,1334,16);
	fltV(rng_res,1350,16);
	fltV(azi_res,1366,16);
	fltV(radi_stretch[0],1382,16);
	fltV(radi_stretch[1],1398,16);
	fltV(alt_dopcen[0],1414,16);
	fltV(alt_dopcen[1],1430,16);
	fltV(alt_dopcen[2],1446,16);
	fltV(crt_dopcen[0],1478,16);
	fltV(crt_dopcen[1],1494,16);
	fltV(crt_dopcen[2],1510,16);
	strV(time_dir_pix,1526,8);
	strV(time_dir_lin,1534,8);
	fltV(alt_rate[0],1542,16);
	fltV(alt_rate[1],1558,16);
	fltV(alt_rate[2],1574,16);
	fltV(crt_rate[0],1606,16);
	fltV(crt_rate[1],1622,16);
	fltV(crt_rate[2],1638,16);
	strV(line_cont,1670,8);
	strV(clutterlock_flg,1678,4);
	strV(auto_focus,1682,4); 
	fltV(line_spacing,1686,16);
	fltV(pixel_spacing,1702,16);
	strV(rngcmp_desg,1718,16);
	if (era==0)
	{
		int i;
		intV(annot_pts,2006,8);
		for (i=0,off=2022; i< q->annot_pts; i++) {
			intV(annot_line[i],off,8);
			intV(annot_pixel[i],off,8);
			strV(annot_text[i],off,16);
		}
	}
	else
	{
		intV(no_beams,1766,2);
		strV(beam1,1768,4);
		strV(beam2,1772,4);
		strV(beam3,1776,4);
		strV(beam4,1780,4);
		singV(prf1,1784,8);
		singV(prf2,1792,8);
		singV(prf3,1800,8);
		singV(prf4,1808,8);
		singV(rng_gate1,1816,8);
		singV(rng_gate2,1824,8);
		singV(rng_gate3,1832,8);
		singV(rng_gate4,1840,8);
		intV(tot_pls_burst,1848,4);
		intV(val_pls_burst,1852,4);
		intV(az_ovlp_nxt_img,1856,8);
		intV(rg_off_nxt_img,1864,8);
	}
}
