#include "asf.h"
#include <ctype.h>
#include "ceos.h"

/*************************************************************************
  These #defines convert floats, ints, and strings to/from ascii.
  We can do this because the offset into the ASCII buffer doesn't depend
  on the direction you're translating.  Hence, the below routines just
  provide the mapping between the two formats.
**************************************************************************/
#define uc (unsigned char *)
#define fltV(qFld,bfOf,bfSz) flt2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define sngV(qFld,bfOf,bfSz) sng2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define shrtV(qFld,bfOf,bfSz) shrt2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define intV(qFld,bfOf,bfSz) int2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define longV(qFld,bfOf,bfSz) long2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define strV(qFld,bfOf,bfSz) str2asc(q->qFld,&bf[bfOf],bfSz,dir);off+=bfSz;

#define fltA(tng,bfOf,bfSz)  flt2asc(&tng,&bf[bfOf],bfSz,dir);off+=bfSz;
#define shrtA(tng,bfOf,bfSz) shrt2asc(&tng,&bf[bfOf],bfSz,dir);off+=bfSz;
#define intA(tng,bfOf,bfSz)  int2asc(&tng,&bf[bfOf],bfSz,dir);off+=bfSz;
#define longA(tng,bfOf,bfSz) long2asc(&tng,&bf[bfOf],bfSz,dir);off+=bfSz;
#define strA(tng,bfOf,bfSz)  str2asc(tng,&bf[bfOf],bfSz,dir);off+=bfSz;


/***************** Enocder/Decoder Utilities **************************
FUNCTION NAME: varies
SYNTAX: varies
DESCRIPTION:
  Since there's plenty of symmetry between reading an ASCII buffer into
  a structure and writing the structure back out to a bffer, and to ease
  modifications and creation of such routines, these routines were created
  to take the enumerated parameter, (called dir), which takes the values
  of toASCII or fromASCII.

PROGRAM HISTORY:
VERSION  DATE    AUTHOR               PURPOSE
-------  ----    ------               -------------------------------
  1.0    2/97    O. Lawlor (ASF)      Original creation
  1.1    8/97    T. Logan (ASF)       ????
  1.4    6/01    P. Denny (ASF)       Fixed flt2asc & long2asc to deal with
                                        blank input
  1.5    9/01    S. Watts (ASF)       Fixed code_dqs to find all valid values
  2.0    9/03    R. Gens / P. Denny   Added new code functions for more CEOS
                                        records
  2.1    2/04    P. Denny             Fill in a more complete set of fields in
                                        Code_IOF
*********************************************************************/

void right_justify(char buf[], int len)
 {
   int to, from;

   to = len -1;
   from = len -1;
   while (isspace(buf[from])) from--;
   while (from >= 0)
    {
      buf[to]=buf[from];
      to--; from--;
    }
   while (to >= 0)
    {
      buf[to]=' ';
      to--;
    }
 }

void flt2asc(double *in,unsigned char *outBuf,int len,codingDir dir)
{
  int i;
  char	cmp[255],
	tmp[255]= /*250 spaces follow.*/
"                                                  \
                                                  \
                                                  \
                                                  \
                                                  ";
  strcpy(cmp,tmp);
  if (dir==toASCII) {
      if (len>22) sprintf(tmp,"%-23.20f",*in);
      else if (len==22) sprintf(tmp,"%-22.19f",*in);
      else if (len==17) sprintf(tmp,"%-17.14f",*in);
      else if (len==16) sprintf(tmp,"%-16.13f",*in);
      else if (len==14) sprintf(tmp,"%-14.11f",*in);
      else sprintf(tmp,"%-f",*in);
      for (i=0;tmp[i];) i++;
      tmp[i]=' ';                     /*eliminate terminating null.*/
      strncpy((char *)outBuf,tmp,len);

  } else { /*dir==fromASCII*/
      strncpy(tmp,(char *)outBuf,len);
      tmp[len]=0;/* add terminating null.*/
      if (0==strncmp(tmp,cmp,len))
	*in=0.0;
      else
	sscanf(tmp,"%lG",in);
  }
}

void long2asc(int *in,unsigned char *outBuf,int len,codingDir dir)
{
  int i;
  char	cmp[255],
	tmp[255]= /*250 spaces follow.*/
"                                                  \
                                                  \
                                                  \
                                                  \
                                                  ";
  strcpy(cmp,tmp);
  if (dir==toASCII) {
      sprintf(tmp,"%d",*in);
      for (i=0;tmp[i];) i++;
      tmp[i]=' ';/*eliminate terminating null.*/
      right_justify(tmp,len);
      strncpy((char *)outBuf,tmp,len);
  } else { /*dir==fromASCII*/
      strncpy(tmp,(char *)outBuf,len);
      tmp[len]=0;/* add terminating null.*/
      if (0==strncmp(tmp,cmp,len))
	*in=0;
      else
	sscanf(tmp,"%d",in);
  }
}

void sng2asc(float *in,unsigned char *outBuf,int len,codingDir dir)
  { double d=*in; flt2asc(&d,outBuf,len,dir); *in=d; }
void shrt2asc(short *in,unsigned char *outBuf,int len,codingDir dir)
  { int l=*in; long2asc(&l,outBuf,len,dir); *in=l; }
void int2asc(int *in,unsigned char *outBuf,int len,codingDir dir)
  { int l=*in; long2asc(&l,outBuf,len,dir); *in=l; }

void str2asc(char *inString,unsigned char *outBuf,int len,codingDir dir)
{
  int i;
  if (dir==toASCII)
  {
	for (i=0;inString[i]&&i<len;i++)
		outBuf[i]=inString[i];
	for (;i<len;i++)
		outBuf[i]=' ';
     /* right_justify((char *)outBuf,len); */
  } else /*dir==fromASCII*/
   {
   	for (i=0;i<len;i++)
   		inString[i]=outBuf[i];
   	inString[len]=0;
   }
}



/********************************************************************
FUNCTION NAME:   code_ceos - converts strings <-> records

SYNTAX:
  Code_FDR_common(unsigned char *bf, struct FDR* q,codingDir dir)
  Code_FDR(unsigned char *bf, struct FDR* q,codingDir dir)
  Code_IOF(unsigned char *bf, struct IOF_VFDR* q,codingDir dir)
  Code_DSSR(unsigned char *bf,struct dataset_sum_rec *q,int era, codingDir dir)
  Code_PPDR (unsigned char *bf, struct pos_data_rec* q, codingDir dir)
  Code_ATDR(unsigned char *bf, struct att_data_rec *q, codingDir dir)
  Code_ATVR(unsigned char *bf, struct att_vect_rec *q, codingDir dir)
  Code_RADDR(unsigned char *bf, struct VRADDR* q,codingDir dir)
  Code_DQS(unsigned char* bf,struct qual_sum_rec* q,int era,codingDir dir)
  Code_DHR(unsigned char* bf, struct data_hist_rec* q, codingDir dir)
  Code_DH(unsigned char *bf, struct hist_dset* q, codingDir dir)
  Code_RSR(unsigned char *bf, struct rng_spec_rec *q, codingDir dir)
  Code_ASF_FACDR(unsigned char *bf, struct VFDRECV *q, int era, codingDir dir)
  Code_ESA_FACDR(unsigned char *bf, struct ESA_FACDR *q, codingDir dir)
  Code_PPR(unsigned char *bf, struct ESA_FACDR *q, codingDir dir)

  Code_VDR(unsigned char *bf, struct VDR *q, codingDir dir)
  Code_LFPR(unsigned char *bf, struct FPR *q, codingDir dir)
  Code_DFPR(unsigned char *bf, struct FPR *q, codingDir dir)
  Code_TR(unsigned char *bf, struct TR *q, codingDir dir)

  Code_NVDR(unsigned char *bf, struct VDR *q, codingDir dir)

DESCRIPTION:
  These procedures convert ASF metadata structures to and from their
  ASCII form.  See ceos.h for info on the structures.
  To convert an ASCII buffer read in from a .ldr,.trl, or .L file, call
 	Code_###(bf,ceosStructure [,era] ,fromASCII);
  To convert that structure back into ASCII, call
	Code_###(bf,ceosStructure [,era] ,toASCII);

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           2/97   O. Lawlor (ASF)  converted from various
					(one-way) utility procedures
  2.0		2/98   T. Logan (ASF)   added all record types, debugged each
  2.1		3/98   O. Lawlor (ASF)  Debugged Tom's debugging.
*********************************************************************/

void Code_FDR_common(unsigned char *bf, struct FDR* q,codingDir dir)
{
    int off = 0;
    strV(ascii_flag,off,2);
    strV(spare1,off,2);
    strV(format_doc,off,12); strV(format_rev,off,2);
    strV(design_rev,off,2); strV(software_id,off,12);
    intV(file_num,off,4); strV(product_id,off,16);
    strV(rec_seq_flag,off,4); intV(seq_loc,off,8); intV(seq_len,off,4);
    strV(rec_code,off,4); intV(code_loc,off,8); intV(code_len,off,4);
    strV(rec_len,off,4); intV(rlen_loc,off,8); intV(rlen_len,off,4);
    strV(spare2,off,4); strV(spare3,off,64);
}

void Code_FDR(unsigned char *bf, struct FDR* q,codingDir dir)
{
    int off=168,i;
    bf+=12; /* Skip CEOS Header */
    Code_FDR_common(bf,q,dir);
    intV(n_dssr,off,6);  intV(l_dssr,off,6);
    intV(n_mpdr,off,6);  intV(l_mpdr,off,6);
    intV(n_ppdr,off,6);  intV(l_ppdr,off,6);
    intV(n_atdr,off,6);  intV(l_atdr,off,6);
    intV(n_raddr,off,6); intV(l_raddr,off,6);
    intV(n_rcr,off,6);   intV(l_rcr,off,6);
    intV(n_qsr,off,6);   intV(l_qsr,off,6);
    intV(n_dhr,off,6);   intV(l_dhr,off,6);
    intV(n_rsr,off,6);   intV(l_rsr,off,6);
    intV(n_demdr,off,6); intV(l_demdr,off,6);
    intV(n_rpr,off,6);   intV(l_rpr,off,6);
    intV(n_adr,off,6);   intV(l_adr,off,6);
    intV(n_dpr,off,6);   intV(l_dpr,off,6);
    intV(n_calr,off,6);  intV(l_calr,off,6);
    intV(n_gcp,off,6);   intV(l_gcp,off,6);
    for (i=0; i< 10; i++) { intV(spare4[i],off,6); }
    intV(n_facdr,off,6); intV(l_facdr,off,6);
}

void Code_IOF(unsigned char *bf, struct IOF_VFDR* q,codingDir dir)
{
    int off;
    bf+=12;/*Skip CEOS Header. (OSL 10/3/98)*/
    Code_FDR_common(bf, (struct FDR*)q, dir);
    off = 168;
    intV(numofrec,off,6);
    intV(reclen,off,6);
    strV(spare4,off,24);
    intV(bitssamp,off,4);
    intV(sampdata,off,4);
    intV(bytgroup,off,4);
    strV(justific,off,4);
    intV(sarchan,off,4);
    intV(linedata,off,8);
    intV(lbrdrpxl,off,4);
    intV(datgroup,off,8);
    intV(rbrdrpxl,off,4);
    intV(topbrdr,off,4);
    intV(botbrdr,off,4);
    strV(interlv,off,4);
    intV(recline,off,2);
    intV(mrecline,off,2);
    intV(predata,off,4);
    intV(sardata,off,8);
    intV(sufdata,off,4);
    strV(repflag,off,4);
    strV(lin_loc,off,8);
    strV(chn_loc,off,8);
    strV(time_loc,off,8);
    strV(left_loc,off,8);
    strV(right_loc,off,8);
    strV(pad_ind,off,4);
    strV(spare6,off,28);
    strV(qual_loc,off,8);
    strV(cali_loc,off,8);
    strV(gain_loc,off,8);
    strV(bais_loc,off,8);
    strV(formatid,off,28);
    strV(formcode,off,4);
    intV(leftfill,off,4);
    intV(rigtfill,off,4);
    intV(maxidata,off,8);
    /* ignore variable length spare bytes: byte 448 to ? */
}
void Code_MPDR(unsigned char *bf, struct VMPDREC *q,codingDir dir)
{
	int off=12+16;
	strV(mpdesc,off,32);
	longV(npixels,off,16);
	longV(nlines,off,16);
	fltV(nomipd,off,16);
	fltV(nomild,off,16);
	fltV(orient,off,16);
	fltV(orbitincl,off,16);
	fltV(ascnode,off,16);
	fltV(distplat,off,16);
	fltV(altplat,off,16);
	fltV(velnadir,off,16);
	fltV(plathead,off,16);

	strV(refelip,off,32);
	fltV(remajor,off,16);
	fltV(reminor,off,16);
	fltV(datshiftx,off,16);
	fltV(datshifty,off,16);
	fltV(datshiftz,off,16);
	fltV(datshift1,off,16);
	fltV(datshift2,off,16);
	fltV(datshift3,off,16);
	fltV(rescale,off,16);

	strV(mpdesig,off,32);
	strV(utmdesc,off,32);
	strV(utmzone,off,4);
	fltV(utmeast,off,16);
	fltV(utmnorth,off,16);
	fltV(utmlong,off,16);
	fltV(utmlat,off,16);
	fltV(utmpara1,off,16);
	fltV(utmpara2,off,16);
	fltV(utmscale,off,16);

	strV(upsdesc,off,32);
	fltV(upslong,off,16);
	fltV(upslat,off,16);
	fltV(upsscale,off,16);

	strV(nspdesc,off,32);

	fltV(nspeast,off,16);
	fltV(nspnorth,off,16);
	fltV(nsplong,off,16);
	fltV(nsplat,off,16);
	fltV(nsppara1,off,16);
	fltV(nsppara2,off,16);
	fltV(nsppara3,off,16);
	fltV(nsppara4,off,16);
	fltV(nspcm1,off,16);
	fltV(nspcm2,off,16);
	fltV(nspcm3,off,16);

	off+=64;/*Spares.*/
	fltV(tlcnorth,off,16);
	fltV(tlceast,off,16);
	fltV(trcnorth,off,16);
	fltV(trceast,off,16);
	fltV(brcnorth,off,16);
	fltV(brceast,off,16);
	fltV(blcnorth,off,16);
	fltV(blceast,off,16);
	fltV(tlclat,off,16);
	fltV(tlclong,off,16);
	fltV(trclat,off,16);
	fltV(trclong,off,16);
	fltV(brclat,off,16);
	fltV(brclong,off,16);
	fltV(blclat,off,16);
	fltV(blclong,off,16);
	fltV(tlcheight,off,16);
	fltV(trcheight,off,16);
	fltV(brcheight,off,16);
	fltV(blcheight,off,16);

	fltV(a11,off,20);
	fltV(a12,off,20);
	fltV(a13,off,20);
	fltV(a14,off,20);
	fltV(a21,off,20);
	fltV(a22,off,20);
	fltV(a23,off,20);
	fltV(a24,off,20);

	fltV(b11,off,20);
	fltV(b12,off,20);
	fltV(b13,off,20);
	fltV(b14,off,20);
	fltV(b21,off,20);
	fltV(b22,off,20);
	fltV(b23,off,20);
	fltV(b24,off,20);

}

void Code_DSSR(unsigned char *bf,struct dataset_sum_rec *q,int era, codingDir dir)
{
	int off=12;
	shrtV(seq_num,off,4);
	shrtV(sar_chan,16,4); strV(product_id,20,16); strV(scene_des,36,32);
	strV(inp_sctim,68,32); strV(asc_des,100,16); fltV(pro_lat,116,16);
	fltV(pro_long,132,16); fltV(pro_head,148,16); strV(ellip_des,164,16);
	fltV(ellip_maj,180,16); fltV(ellip_min,196,16); fltV(earth_mass,212,16);
	fltV(grav_const,228,16); fltV(ellip_j[0],244,16);
	fltV(ellip_j[1],260,16); fltV(ellip_j[2],276,16);
        if (!era)
          {
	    fltV(terrain_h ,292,16);
	    fltV(sc_lin    ,308,16);
	    fltV(sc_pix    ,324,16);
	  }
	else
  	  {
            strV(spare1,292,16);
	    fltV(terrain_h,308,16);
            fltV(sc_lin,324,8);
            fltV(sc_pix,332,8);
          }
	fltV(scene_len ,340,16); fltV(scene_wid ,356,16);
	shrtV(nchn,388,4); strV(mission_id,396,16); strV(sensor_id,412,32);
	strV(revolution,444,8); fltV(plat_lat,452,8); fltV(plat_long,460,8);
	fltV(plat_head_scene,468,8); fltV(clock_ang,476,8);
	fltV(incident_ang,484,8); fltV(frequency,492,8);
	fltV(wave_length,500,16); strV(motion_comp,516,2);
	strV(pulse_code,518,16);
 	fltV(ampl_coef[0],534,16); fltV(ampl_coef[1],550,16);
	fltV(ampl_coef[2],566,16); fltV(ampl_coef[3],582,16);
	fltV(ampl_coef[4],598,16); fltV(phas_coef[0],614,16);
	fltV(phas_coef[1],630,16); fltV(phas_coef[2],646,16);
	fltV(phas_coef[3],662,16); fltV(phas_coef[4],678,16);
	longV(chirp_ext_ind,694,8); fltV(rng_samp_rate,710,16);
	fltV(rng_gate,726,16); fltV(rng_length,742,16);
	strV(baseband_f,758,4); strV(rngcmp_f,762,4);
	fltV(gn_polar,766,16); fltV(gn_cross,782,16);
	longV(chn_bits,798,8); strV(quant_desc,806,12);
	fltV(i_bias,818,16); fltV(q_bias,834,16);
	fltV(iq_ratio,850,16); fltV(spare_dss_7,866,16);
	fltV(spare_dss_8,882,16); fltV(ele_sight,898,16);
	fltV(mech_sight,914,16); strV(echo_track,930,4);
	fltV(prf,934,16); fltV(elev_beam,950,16);
	fltV(azi_beam,966,16); strV(sat_bintim,982,16);
	strV(sat_clktim,998,32); longV(sat_clkinc,1030,8);
	strV(fac_id,1046,16); strV(sys_id,1062,8);
	strV(ver_id,1070,8); strV(fac_code,1078,16);
	strV(lev_code,1094,16); strV(product_type,1110,32);
	strV(algor_id,1142,32); fltV(n_azilok,1174,16);
	fltV(n_rnglok,1190,16); fltV(bnd_azilok,1206,16);
	fltV(bnd_rnglok,1222,16); fltV(bnd_azi,1238,16);
	fltV(bnd_rng,1254,16); strV(azi_weight,1270,32);
	strV(rng_weight,1302,32); strV(data_inpsrc,1334,16);
	fltV(rng_res,1350,16); fltV(azi_res,1366,16);
	fltV(radi_stretch[0],1382,16); fltV(radi_stretch[1],1398,16);
	fltV(alt_dopcen[0],1414,16); fltV(alt_dopcen[1],1430,16);
	fltV(alt_dopcen[2],1446,16); fltV(crt_dopcen[0],1478,16);
	fltV(crt_dopcen[1],1494,16); fltV(crt_dopcen[2],1510,16);
	strV(time_dir_pix,1526,8); strV(time_dir_lin,1534,8);
	fltV(alt_rate[0],1542,16); fltV(alt_rate[1],1558,16);
	fltV(alt_rate[2],1574,16); fltV(crt_rate[0],1606,16);
	fltV(crt_rate[1],1622,16); fltV(crt_rate[2],1638,16);
	strV(line_cont,1670,8); strV(clutterlock_flg,1678,4);
	strV(auto_focus,1682,4); fltV(line_spacing,1686,16);
	fltV(pixel_spacing,1702,16); strV(rngcmp_desg,1718,16);
	if (strncmp(q->fac_id, "ASF", 3)==0) {
	  if (era==0) {
	    int i;
 	    intV(annot_pts,2006,8);
 	    if (q->annot_pts>=64)
 	  	  q->annot_pts=63;
	    for (i=0,off=2022; i< q->annot_pts; i++) {
   	      intV(annot_line[i],off,8); intV(annot_pixel[i],off,8);
	      strV(annot_text[i],off,16);
	    }
	  }
	  else {
  	    intV(no_beams,1766,2);
	    strV(beam1,1768,4); strV(beam2,1772,4);
	    strV(beam3,1776,4); strV(beam4,1780,4);
	    sngV(prf1,1784,8); sngV(prf2,1792,8);
	    sngV(prf3,1800,8); sngV(prf4,1808,8);
	    sngV(rng_gate1,1816,8); sngV(rng_gate2,1824,8);
	    sngV(rng_gate3,1832,8); sngV(rng_gate4,1840,8);
	    intV(tot_pls_burst,1848,4); intV(val_pls_burst,1852,4);
	    intV(az_ovlp_nxt_img,1856,8); intV(rg_off_nxt_img,1864,8);
	    strV(cal_params_file,1872,32);
	    strV(scan_results_file,1904,32);
	    strV(scanner_version,1936,16);
	    strV(decode_version,1952,16);
	  }
	}
	if (strncmp(q->fac_id, "ES", 2)==0 || strncmp(q->fac_id, "D-PAF", 5)==0) {
	  int i;
	  for (i=0; i<3; i++) fltV(rng_time[i],1766+i*16,16);
	  strV(az_time_first,1814,24);
	  strV(az_time_center,1838,24);
	  strV(az_time_last,1862,24);
	}
}

void Code_PPDR (unsigned char *bf, struct pos_data_rec* q, codingDir dir)
{
    int off=12;
    int i,j;

    strV(orbit_ele_desg,off,32);
    fltV(orbit_ele[0],off,16);
    fltV(orbit_ele[1],off,16);
    fltV(orbit_ele[2],off,16);
    fltV(orbit_ele[3],off,16);
    fltV(orbit_ele[4],off,16);
    fltV(orbit_ele[5],off,16);
    shrtV(ndata,off,4);
    shrtV(year,off,4);
    shrtV(month,off,4);
    shrtV(day,off,4);
    shrtV(gmt_day,off,4);
    fltV(gmt_sec,off,22);
    fltV(data_int,off,22);
    strV(ref_coord,off,64);
    fltV(hr_angle,off,22);
    fltV(alt_poserr,off,16);
    fltV(crt_poserr,off,16);
    fltV(rad_poserr,off,16);
    fltV(alt_velerr,off,16);
    fltV(crt_velerr,off,16);
    fltV(rad_velerr,off,16);
    for (i = 0; i< q->ndata; i++)
     {
      for (j = 0; j<6; j++)
       {
        fltV(pos_vec[i][j],off,22);
       }
     }
/*  strV(spare_ppr_1,off,242); - caused CDPF data some grief & is not required */
}

void Code_ATDR(unsigned char *bf, struct att_data_rec *q, codingDir dir)
{
    struct att_vect_rec *v,*oldv=NULL;
    int off = 12;
    int pointNo=0;
    int i;
    shrtV(npoint,off,4);
    v = q->data;
    for (i=0; i< q->npoint; i++)
      {
	if (dir == fromASCII)
         {
	   v = (struct att_vect_rec *) MALLOC (sizeof(struct att_vect_rec));
	   if (oldv==NULL)
		q->data=v;
	   else
		oldv->next=v;
	 }
	off += Code_ATVR(&bf[off],v,dir);
        oldv=v;
   	v = v->next;
   	pointNo++;
      }
    strV(spare_adr_1,off,640);
}

int Code_ATVR(unsigned char *bf, struct att_vect_rec *q, codingDir dir)
{
    int off=0;
    shrtV(gmt_day,off,4);
    longV(gmt_msec,off,8);
    shrtV(pitch_flag,off,4);
    shrtV(roll_flag,off,4);
    shrtV(yaw_flag,off,4);
    fltV(pitch,off,14);
    fltV(roll,off,14);
    fltV(yaw,off,14);
    shrtV(pitch_rate_flag,off,4);
    shrtV(roll_rate_flag,off,4);
    shrtV(yaw_rate_flag,off,4);
    fltV(pitch_rate,off,14);
    fltV(roll_rate,off,14);
    fltV(yaw_rate,off,14);
    return(off);
}

void Code_RADDR(unsigned char *bf, struct VRADDR* q,codingDir dir)
{
    int off = 12;
    int i;

    shrtV(seqnum,off,4);
    shrtV(datfield,off,4);
    longV(setsize,off,8);
    strV(sarchan,off,4);
    strV(spare,off,4);
    strV(luttype,off,24);
    longV(nosample,off,8);
    strV(samptype,off,16);
    fltV(a[0],off,16);
    fltV(a[1],off,16);
    fltV(a[2],off,16);
    off+=4;
    for (i=0; i<256; i++)
    {/* <--- Since fltV is a #define, these brackets are necessary.*/
    	fltV(noise[i],off,16);
    }
}

void Code_RSI_RADDR(unsigned char *bf, struct RSI_VRADDR* q,codingDir dir)
{
    int off = 12;
    int ii;

    shrtV(seq_num,off,4);
    shrtV(n_data,off,4);
    longV(field_size,off,8);
    strV(chan_ind,off,4);
    strV(spare1,off,4);
    strV(table_desig,off,24);
    longV(n_samp,off,8);
    strV(samp_type,off,16);
    shrtV(samp_inc,off,4);
    for (ii=0; ii<512; ii++) {
    	fltV(lookup_tab[ii],off,16);
    }
    strV(spare2,off,4);
    fltV(noise_scale,off,16);
    fltV(spare3,off,16);
    fltV(offset,off,16);
    fltV(calib_const,off,16);
    strV(spare4,off,1512);
}

void Code_DQS(unsigned char* bf,struct qual_sum_rec* q,int era,codingDir dir)
{
    int off=12, i;
    shrtV(seq_num,off,4);
    strV(chan_ind,off,4);
    strV(cali_date,off,6);
    shrtV(nchn,off,4);
    fltV(islr,off, 16);
    fltV(pslr,off, 16);
    fltV(azi_ambig,off, 16);
    fltV(rng_ambig,off, 16);
    fltV(snr,off, 16);
    fltV(ber,off, 16);
    fltV(rng_res,off, 16);
    fltV(azi_res,off, 16);
    fltV(rad_res,off, 16);
    fltV(dyn_rng,off, 16);
    fltV(abs_rad_unc_db,off, 16);
    fltV(abs_rad_unc_deg,off, 16);
    off=222;
    for (i=0; (i<q->nchn && i<16); i++)
	 {
	   fltV(rel_rad_unc[0][i],off,16);
	   fltV(rel_rad_unc[1][i],off,16);
         }
    off=734;
    fltV(alt_locerr,off,16);
    fltV(crt_locerr,off,16);
    fltV(alt_scale,off,16);
    fltV(crt_scale,off,16);
    fltV(dis_skew,off,16);
    fltV(ori_err,off,16);
    off=830;
    for (i=0; i<16; i++)
    	 {
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
   else { strV(spare2,off,279); }
}


void Code_DHR(unsigned char* bf, struct data_hist_rec* q, codingDir dir)
{
  int j, off=12;
  struct hist_dset *d=NULL,*oldD=NULL;
  shrtV(seq_num,off,4);
  shrtV(sar_chan,off, 4);
  longV(ntab,off, 8);
  longV(ltab,off, 8);

  if (dir==fromASCII)
	oldD=NULL;
  else
	d=q->data;
  for (j=0; j< (q->ntab); j++)
  {
	if (dir==fromASCII)
		d=(struct hist_dset *)MALLOC(sizeof(struct hist_dset));
	Code_DH(&bf[off],d,dir);
	off += 248+8*d->nhist;
	if (dir==fromASCII) {
		d->next = NULL;
		if (!oldD)
			oldD = q->data = d;
		else
			oldD = ( oldD->next = d);
	}
	else /*(dir==toASCII)*/
		{ d=d->next; }
  }
}

void Code_DH(unsigned char *bf, struct hist_dset* q, codingDir dir)
{
	int off = 0, i;
	strV(hist_desc,off,32); shrtV(nrec,off, 4);
	shrtV(tab_seq,off, 4); longV(nbin,off, 8);
	longV(ns_lin,off, 8); longV(ns_pix,off, 8);
	longV(ngrp_lin,off, 8); longV(ngrp_pix,off, 8);
	longV(nsamp_lin,off, 8); longV(nsamp_pix,off, 8);
	fltV(min_smp,off, 16); fltV(max_smp,off, 16);
	fltV(mean_smp,off, 16); fltV(std_smp,off, 16);
	fltV(smp_inc,off, 16); fltV(min_hist,off, 16);
	fltV(max_hist,off, 16); fltV(mean_hist,off, 16);
	fltV(std_hist,off, 16); longV(nhist,off,8);
	if (dir==fromASCII)
	  q->data_values_hist = (int *) MALLOC (sizeof(int)*q->nhist);

	for (i=0; i<q->nhist; i++)
           { longV(data_values_hist[i],off, 8); }
}

void Code_RSR(unsigned char *bf, struct rng_spec_rec *q, codingDir dir)
{
    int off = 12;
    int i;
    shrtV(seq_num,off,4);
    shrtV(sar_chan,off,4);
    longV(n_dset,off,8);
    longV(dset_size,off,8);
    shrtV(req_recs,off,4);
    shrtV(table_no,off,4);
    longV(n_pixels,off,8);
    longV(pixel_offset,off,8);
    longV(n_lines,off,8);
    fltV(first_freq,off,16);
    fltV(last_freq,off,16);
    fltV(min_power,off,16);
    fltV(max_power,off,16);
    strV(spare_rsr_1,off,16);
    strV(spare_rsr_2,off,16);
    longV(n_bins,off,8);
    for (i=0;i<(q->n_bins); i++)
    {/* <-- since fltV is a #define, these {}'s are needed.*/
    	fltV(data_values_spec[i],off,16);
    }
    strV(spare_rsr_3,off,1052);
}

void Code_ASF_FACDR(unsigned char *bf,struct VFDRECV *q,int era,codingDir dir)
{
    int off=12;

    shrtV(seq_num,off,4);
    strV(spare_frd_1,off,4);
    strV(dataid,off,14); strV(imageid,off,11); strV(coryear,off,5);
    strV(cortime,off,17); strV(sitename,off,33); strV(imgyear,off,5);
    strV(imgtime,off,17);
    fltV(imgclat,off,17); fltV(imgclon,off,17); fltV(nearslat,off,17);
    fltV(nearslon,off,17); fltV(nearelat,off,17); fltV(nearelon,off,17);
    fltV(farslat,off,17); fltV(farslon,off,17); fltV(farelat,off,17);
    fltV(farelon,off,17); fltV(swazim,off,17); fltV(swrange,off,17);
    longV(npixels,off,9); longV(nlines,off,9); longV(apixels,off,9);
    longV(alines,off,9); strV(mediaid,off,7); strV(sblock,off,17);
    strV(eblock,off,17); strV(platform,off,17); strV(sensmode,off,33);
    fltV(prfreq,off,17); fltV(antlook,off,17); fltV(datarate,off,17);
    fltV(datawin,off,17); fltV(rangegd,off,17); fltV(trackang,off,17);
    strV(ascdesc,off,2); fltV(scalt,off,17); fltV(scxpos,off,23);
    fltV(scypos,off,23); fltV(sczpos,off,23); fltV(scxvel,off,23);
    fltV(scyvel,off,23); fltV(sczvel,off,23); fltV(scroll,off,15);
    fltV(scyaw,off,15); fltV(scpitch,off,15);
    intV(qroll,off,5); intV(qyaw,off,5); intV(qpitch,off,5);
    fltV(rroll,off,15); fltV(ryaw,off,15); fltV(rpitch,off,15);
    intV(rqroll,off,5); intV(rqyaw,off,5); intV(rqpitch,off,5);
    fltV(eradnadr,off,17); fltV(eradcntr,off,17); fltV(incedang,off,17);
    strV(procvers,off,8); strV(imgproct,off,3); strV(ephemert,off,2);
    fltV(nlooksaz,off,17); fltV(nlooksra,off,17); fltV(weightaz,off,17);
    fltV(weightra,off,17); strV(normener,off,4); fltV(indistaz,off,17);
    fltV(indistra,off,17); fltV(recgain,off,17); fltV(swathvel,off,17);
    fltV(squintan,off,17); fltV(avgterht,off,17);
    strV(procgain,off,4); strV(deskewf,off,4); strV(grndslnt,off,7);
    fltV(sltrngfp,off,17); fltV(sltrnglp,off,17); intV(strtsamp,off,9);
    strV(clttrlkf,off,4);
    fltV(dpplrfrq,off,17); fltV(dpplrslp,off,17); fltV(dpplrqdr,off,17);
    strV(autfocsf,off,4);
    fltV(dpplrrat,off,17); fltV(dpratslp,off,17); fltV(dpratqdr,off,17);
    fltV(imresaz,off,17); fltV(imresra,off,17); fltV(azpixspc,off,17);
    fltV(rapixspc,off,17); strV(rngcompf,off,4); intV(bitssamp,off,5);
    fltV(calestim,off,17); fltV(biterrrt,off,17); fltV(sigtonoi,off,17);
    fltV(estnoifl,off,17); fltV(radiores,off,17); intV(nsatpnts,off,9);
    strV(inspecf,off,4);
    if (era) {
	fltV(repl_agc,off,17); fltV(temp_rx_lna,off,17);
	fltV(temp_rx_sub,off,17); fltV(temp_rx_prot,off,17);
	fltV(temp_cal_sys,off,17); fltV(rx_agc,off,17);
	fltV(pre_cal1_pow,off,17); fltV(pre_cal2_pow,off,17);
	fltV(post_cal1_pow,off,17); fltV(post_cal2_pow,off,17);
	fltV(repl_pow,off,17); fltV(ssar_roll_ang,off,17);
	strV(comment,off,100);
    }
    else strV(comment,off,100);
}

void Code_ESA_FACDR(unsigned char *bf, struct ESA_FACDR *q, codingDir dir)
{
    int off=12, i;

    strV(seq_num,off,64);
    strV(qc_release,off,6); strV(spare1,off,2); strV(cal_update,off,6);
    shrtV(qa_flag,off,4); shrtV(prf_flag,off,4); shrtV(samp_flag,off,4);
    shrtV(cal_flag,off,4); shrtV(chirp_flag,off,4); shrtV(dop_conf_flag,off,4);
    shrtV(dop_val,off,4); shrtV(dop_amb_conf_flag,off,4); shrtV(out_flag,off,4);
    shrtV(range_flag,off,4);
    shrtV(n_prf_changes,off,4); shrtV(n_samp_changes,off,4); shrtV(n_gain_changes,off,4);
    shrtV(n_miss_lines,off,4); shrtV(n_rec_gain_changes,off,4);
    fltV(width_ccf,off,17); fltV(lobe_ccf,off,17); fltV(islr,off,17);
    fltV(dop_cent_conf,off,17); fltV(dop_amb_conf,off,17);
    fltV(main_i,off,17); fltV(main_q,off,17);
    fltV(stddev_i,off,17); fltV(stddev_q,off,17);
    fltV(cal_gain,off,17); fltV(rec_gain,off,17);
    fltV(dop_amb,off,17); strV(spare2,off,16);
    fltV(bias_i,off,17); fltV(bias_q,off,17);
    fltV(gain_i,off,17); fltV(gain_q,off,17);
    fltV(non_ortho,off,17); strV(spare3,off,16);
    fltV(noise_pow,off,17); strV(pulse_delay,off,17);
    shrtV(n_cal_pulses,off,4); shrtV(n_noise_pulses,off,4); shrtV(n_rep_pulses,off,4);
    fltV(f_samp_rep,off,17); fltV(cal_pulse_pow,off,4); fltV(noise_pulse_pow,off,17);
    fltV(range_comp,off,17); fltV(rep_pulse_pow,off,4);
    fltV(inc_first_pix,off,17); fltV(inc_center_pix,off,4); fltV(inc_last_pix,off,14);
    fltV(sl_range_ref,off,17); strV(spare4,off,12);
    shrtV(ant_pattern_flag,off,4);
    fltV(abs_cal_const,off,17); fltV(up_cal_const,off,17); fltV(low_cal_const,off,17);
    fltV(sigma_0,off,17);
    strV(k_gen,off,6); strV(k_ver,off,4);
    shrtV(dup_lines,off,4);
    fltV(ber,off,17);
    strV(spare5,off,12);
    fltV(img_mean,off,17); fltV(img_std_dev,off,17); fltV(img_max,off,17);
    strV(t_first_line,off,24); strV(t_asc_node,off,24);
    fltV(x_asc_pos,off,23); fltV(y_asc_pos,off,23); fltV(z_asc_pos,off,23);
    fltV(x_asc_vel,off,23); fltV(y_asc_vel,off,23); fltV(z_asc_vel,off,23);
    shrtV(out_bits,off,4);
    fltV(gain1,off,17); fltV(gain2,off,17); fltV(gain3,off,17);
    shrtV(peak_loc_first,off,4);
    fltV(width_ccf2,off,17); fltV(f_lobe,off,17); fltV(islr_ccf,off,17);
    shrtV(peak_loc_last,off,4);
    shrtV(roll_flag,off,4); shrtV(raw_data_flag,off,4); shrtV(look_flag,off,4);
    shrtV(dop_amb_flag,off,4); shrtV(az_base_flag,off,4);
    shrtV(samp_per_line,off,4); shrtV(lines_skip,off,4);
    strV(t_in_stvec,off,24);
    fltV(x_in_pos,off,23); fltV(y_in_pos,off,23); fltV(z_in_pos,off,23);
    fltV(x_in_vel,off,23); fltV(y_in_vel,off,23); fltV(z_in_vel,off,23);
    shrtV(in_stvec_flag,off,4);
    fltV(range_filt,off,17); fltV(az_filt,off,17); shrtV(update_filt,off,4);
    for (i=0; i<8; i++) fltV(look_gains[i],off,17);
    shrtV(samp_win_bias,off,4);
    fltV(dop_cube_coef,off,23);
    shrtV(prf_first_line,off,4); shrtV(prf_last_line,off,4);
    shrtV(win_first_line,off,4); shrtV(win_last_line,off,4);
    shrtV(cal_gain_last_line,off,4); shrtV(rec_gain_last_line,off,4);
    shrtV(first_range_samp,off,4);
    shrtV(fft_ratio,off,4);
    shrtV(n_az_blocks,off,4); intV(n_in_lines,off,8);
    shrtV(ini_dop_amb,off,4);
    for (i=0; i<3; i++) fltV(chirp_quality[i],off,17);
    for (i=0; i<5; i++) fltV(in_data_stats[i],off,17);
    for (i=0; i<2; i++) fltV(dop_amb_thres[i],off,17);
    for (i=0; i<2; i++) fltV(out_data_stats[i],off,17);
    strV(t_sat_first_line,off,17);
    shrtV(n_val_pix,off,4); shrtV(n_range_samp,off,4);
    fltV(gain_low,off,17); fltV(gain_up,off,17);
    fltV(quad_low,off,17); fltV(quad_up,off,17);
    fltV(look_bw,off,17); fltV(dop_bw,off,17);
    shrtV(range_spread,off,4); strV(more_flags,off,17);
    shrtV(max_look,off,14); shrtV(rep_norm_flag,off,4);
    for (i=0; i<4; i++) fltV(gr2sr_poly[i],off,21);
    for (i=0; i<5; i++) fltV(ant_elev_poly[i],off,21);
    fltV(range_time_poly,off,17);
    strV(spare6,off,10238);
}

void Code_PPR(unsigned char *bf, struct PPREC *q, codingDir dir)
{
    int off=12;

    strV(beam_type,931,3);
}

void Code_NVDR(unsigned char *bf, struct VDREC *q, codingDir dir)
{
    int off=12;

    strV(flag1,off,3);
    strV(blank,off,3);
    strV(format_doc,off,13);
    strV(super_doc,off,3);
    strV(super_doc_rev,off,3);
    strV(sw_release,off,13);
    strV(id,off,17);
    strV(vol_id,off,17);
    strV(vol_set_id,off,17);
    intV(vol_nrs,off,8);
    shrtV(vol_nr,off,4);
    shrtV(vol_volset,off,4);
    shrtV(vol_phys_vol,off,4);
    strV(create_date,off,9);
    strV(create_time,off,9);
    strV(country,off,13);
    strV(agency,off,9);
    strV(paf,off,13);
    shrtV(fprs,off,4);
    shrtV(rec_nr,off,4);
    strV(spare,off,93);
    strV(local,off,101);
}

void Code_VDR(unsigned char *bf, struct VDREC *q, codingDir dir)
{
    int off=12;

    strV(flag1,off,3);
    strV(blank,off,3);
    strV(format_doc,off,13);
    strV(super_doc,off,3);
    strV(super_doc_rev,off,3);
    strV(sw_release,off,13);
    strV(id,off,17);
    strV(vol_id,off,17);
    strV(vol_set_id,off,17);
    intV(vol_nrs,off,8);
    shrtV(vol_nr,off,4);
    shrtV(vol_volset,off,4);
    shrtV(vol_phys_vol,off,4);
    strV(create_date,off,9);
    strV(create_time,off,9);
    strV(country,off,13);
    strV(agency,off,9);
    strV(paf,off,13);
    shrtV(fprs,off,4);
    shrtV(rec_nr,off,4);
    strV(spare,off,93);
    strV(local,off,101);

}

void Code_LFPR(unsigned char *bf, struct FPREC *q, codingDir dir)
{
    int off=12;

    strV(flag1,off,3);
    strV(blank,off,3);
    shrtV(nr,off,4);
    strV(name,off,17);
    strV(class,off,29);
    strV(class_code,off,5);
    strV(data_type,off,29);
    strV(type_code,off,5);
    intV(records,off,8);
    intV(first_rec,off,8);
    intV(max_rec,off,8);
    strV(rec_type,off,13);
    strV(rec_type_code,off,5);
    shrtV(vol_nrs,off,4);
    intV(start,off,8);
    intV(end,off,8);
    strV(spare,off,101);
    strV(local,off,101);
}

void Code_DFPR(unsigned char *bf, struct FPREC *q, codingDir dir)
{
    int off=12;

    strV(flag1,off,3);
    strV(blank,off,3);
    shrtV(nr,off,4);
    strV(name,off,17);
    strV(class,off,29);
    strV(class_code,off,5);
    strV(data_type,off,29);
    strV(type_code,off,5);
    intV(records,off,8);
    intV(first_rec,off,8);
    intV(max_rec,off,8);
    strV(rec_type,off,13);
    strV(rec_type_code,off,5);
    shrtV(vol_nrs,off,4);
    intV(start,off,8);
    intV(end,off,8);
    strV(spare,off,101);
    strV(local,off,101);
}

void Code_TR(unsigned char *bf, struct TREC *q, codingDir dir)
{
    int off=12;

    strV(flag1,off,3);
    strV(flag2,off,3);
    strV(prod_type,off,41);
    strV(location,off,61);
    strV(vol_phys_id,off,41);
    strV(scene_id,off,41);
    strV(scene_loc,off,41);
    strV(spare1,off,21);
    strV(spare2,off,105);
}
