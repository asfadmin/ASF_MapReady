#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	mpsf77calls.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)mpsf77calls.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.mpsf77calls.h"

#ifndef _MPSF77CALLS_H_
#define _MPSF77CALLS_H_

/* asfet.for */
	extern asfet_ (char *asftime, double *et, int *ierr); 

/* cal2day.for */
        extern cal2day_(int *nday, int *iyear, int *imon, int *iday);

/* chasft.for */
	extern chasft_(char *asftime, int *ierr);

/* chfnam.for */
        extern chfnam_(char *fspec, int *isot);

/* chkphase.for */
        extern chkphase_(char *phase_name, char *phase_start,       
			double *phase_lon, int *phase_days,
			int *phase_orbits, int *last_rev,
			int *cycle_days, int *cycle_revs, double *orb_a,
			double *orb_e, double *orb_i, double *orb_arg_peri, int *ier) ;

/* day2cal.for */
        extern day2cal_ (int *nday, int *iyear, int *mon, int *iday) ;

/* ersswath.for */
	extern swath_compute_ (char *sat, int *imode, 
		double *sm, double *xi, double *om, double *ar, 
		double *tr,  double *ss, double *orbday, double *alfa3,
		double *qnear, double *qfar, double *qcenter, double *krite) ;


	extern ers_ysm_rot_(double *req, double *flat,
		double *sm, double *xi, double *ar, double *tr,
		double *orbday, double *pp, double *rpp, double *krite);

	extern ers_ysm_angl_(double *req, double *flat, double *sm,
		double *xi, double *ar, double *tr, double *orbday,
		double *zta, double *xki, double *eta, int *krite);

	extern rotate_zxy_(double *zta, double *xki, double *eta,
		double *ci, double *ss, double *pp, double *xers,
		double *yers, double *zers, double *qq, double *qq2,
		int *krite, char *qlabel) ;
        
	extern rotatx_(double *angle, double *rr, double *rrot);

	extern rotaty_(double *angle, double *rr, double *rrot);

	extern rotatz_(double *angle, double *rr, double *rrot);

	extern local_frame_(double *ss, double *xi, double *om,
		double *xers, double *yers, double *zers);

	extern ellp_(double *ss, double*tt, double *ci, double *xlamda,
		double *qq1, double *qq2);
 
/* et2asf.for */
        extern et2asf_(double *et, char *asftime);

/* et2utc.for */
        extern double et2utc_(double *et);

/* exfname.for*/
        extern exfname_(char *fname, int *iost);

/* gcatrv.for */
        extern gcatrv_(double *q, double *p1, double *p2, double *adeg,
                 double *d);
/* gcbomb.for */
        extern gcbomb_();

/* gcbtwn.for */
        extern gcbtwn_(int *iflag, double *p1, double *p2);

/* gcdist.for */
        extern gcdist_(double *d, double *p1, double *p2);

/* gcentc.for */
        extern gcentc_(int *iflag, double *xin, double *p1, double *p3);

/* gcfrac.for */
        extern gcfrac_(double *f, double *p1, double *p2, double *p3);

/* gclinq for */
        extern gclinq_(double *f, double *sl1, double *sl2,
               double *q1, double *q2, double *q3, double *q4);

/* gclose.for */
        extern gclose_(int *iflag, double *p1, double *p2);

/* gclovr.for */ 
        extern gclovr_(int *iflag, double *x, double *sl1,
               double *sl2, double *ql, double *qr);

/* gcperp.for */
        extern gcperp_(double *x, double *p1, double *p2, double *q);

/* gcpinq.for */
        extern gcpinq_(int *iflag, double *q1, double *q2,
               double *q3, double q4, double *p);

/* gcrinq.for */
        extern gcring_(double *f, double *sr1, double *sr2, 
               double *q1, double *q2, double *q3, double *q4);

/* gcrovr.for */
        extern gcrovr_(int *iflag, double *x, double *sr1,
               double *sr2, double *ql, double *qr);

/* gcsegp.for */
        extern gcsegp_(double *d, double *pc, double *pl,
               double *pr, double *q); 
 
/* gctrav.for */
        extern gctrav_(double *q, double *p1, double *p2,
               double *d);

/* gctrvf.for */
        extern gctrvf_(double *q, double *p1, double *p2,
               double *f);

/* geodet.for */
        extern geodet_(double *re, double *latc, double *longc,
               double *altc, double *latd, double *altd);
     
        extern togeod_(double *re, double *x, double *hite,
               double *trgf);

/* getargi.for */
        extern double getargi_(int *n, int *j, int *ncode);

/* getargxx.for */
        extern getargxx_(int *n, double *x, int *ncode);

/* jday2asf.for */
        extern char jday2asf_(double *time);
        
        extern asf2jday_(char *asftime, double *tj);

/* lastc.for */
        extern lastc_() ;

/* ll2xyz.for */
        extern ll2xyz_(double *pll, double *pxyz);
 
/* mps_cgen.for */
        extern mps_cgen_(float *lat, float *lon, float*rkm,
                int *npts, float *lats, float *lons);
 
/* mpsclose.for */
        extern mpsclose_(float *p1lat, float *p1lon, float p2lat,
                float *p2lon, int *iflag);

/* mpsintrp.for */
        extern mpsintrp_(float *p1lat, float *p1lon, float *p2lat,
                float *p2lon, int *n, float *plats, float *plons);

/* qnpnt.for */
        extern qnpnt_(double *nlat, double *q1, double *q2,
                double *q3, double *q4);

/* qspnt.for */
        extern qspnt_(double *slat, double *q1, double *q2,
                double *q3, double *q4);

/* rev2time.for */
        extern rev2time_(float *dbproc, char *sat, int *rev,
                double *et1, char *asf1, double *et2,
                char *asf2, int *ier);

/* revs2t.for */
        extern rev2t_(float *dbproc, char *sat, int *rev,
                double *et1, char *asf1, double *et2,
                char *asf2, int *ier);
 
/* sangl.for */
        extern sangl_(double *arad, double *p1, double *p2,
                double *p3);

/* sangld.for */
        extern sangld_(double *adeg, double *p1, double *p2,
               double *p3);

/* sscovc.for */
        extern sscovc_(double *center,double *radius, double *point,
               int *npoint, double *start, double *stop, char *ascdsc);

/* sscovq.for */
        extern sscovq_(double *q1, double *q2, double *q3, double *q4,
                double *point, int *npoint, double *start, double *stop,
                char *ascdc);     

/* sscvad.for */
        extern sscvad_(double *s, double *p1, double *p2,
                char *ascdsc);

/* sscvc.for */
        extern sscvc_(float *dbproc, int *ibatch, int *darid,
                char *sitename, double *radiuskm);

/* sscvcb.for */
        extern sscvcb_(double *cll, double *rdeg, double *nlat,
                double *slat, double *elon, double *wlon, int *inorml);

/* sscvq,for */
        extern sscvq_(float *dbproc, int *ibatch, int *darid,
                char *sitename, double *nwlat, double *nwlon,
                double *nelat, double *nelon, double *selat,
                double *selon, double swlat, double *swlon,
                char *sat, char *sensor, char *ascdsc, 
                double *time1, double *time2, int *trev1,
                int *trev2, int *nrecs, int *istat);

/* sscvqb.for */
        extern sscvqb_(double *nwlat, double *nwlon, double *nelat,
                double *nelon, double *selat, double *selon,
                double *swlat, double *swlon, double *rdeg,
                double *nlat, double *slat, double *elon,
                double *wlon, int *inorml);

/* sscvrevs2t.for */
        extern sscvrevs2t_(float *dbprpoc, int *ibatch, char *sat,
                int *irev1, int *irev2, double *et1, char *asft1,
                double *et2, char *asft2, int *ier);

/* swangl.for */
        extern swangl_(double *sl, double *x, double *sr,
                double *ql, double *qr, int *iflag);

/* swentc.for */
        extern swentc_(double *c, double *r, double *sl1, double *sr1,
                double *sl2, double *sr2, double *f);

/* swentq.for */
        extern swentq_(double *q1, double *q2, double *q3,
                double *q4, double *sl1, double *sr1, 
                double *sl2, double *sr2, double *f);

/* swinq.for */
        extern swinq_(double *sl1, double *sr1, double *sl2,
                double *sr2, double *q1, double *q2, double *q3,
                double *q4, int *iflag);

/* swside.for */
        extern swside_(double *sl, double *sr, double *ql,
                double *qr, int *iflag);

/* tfname.for */
        extern tfname_(char *fname, char *tname);

        extern findvar_(char *name, char *var, int *jcode);

        extern subvar_(char *name, int *last_vchar, char *dvar,
                char *new_name);
        extern tfclean_(char *name, char *nameout);

/* time2rev.for */
        extern time2rev_(int *dbproc, char *sat, double *eto,
                char *asftime, int *irev1, int *irev2, int *ier);

/* times2r.for */
        extern times2r_(int *dbproc, int *ibatch, char *sat, 
                double *et1, char *asftime1, double *et2,
                char *asftime2, int *irev1, int *irev2, int *ier);
               
/* uniq18.for */
         extern uniq18_(char *ch18);      

/* utc2et.for */
        extern double utc2et_(float *utc);

/* vintrd.for */
       extern vintrd_(float *w1, float *w2, float *v1,
               float *v2, float *va);

       extern dintrp(float *w1, float *w2, double *d1,
                 double *d2, double *c);

/* writecheck.for */
        extern writecheck_(char *filename, int *ier);

/* xyz2ll.for */
        extern xyz2ll_(double *xyz2ll, double *pll);

#endif	/* _MPSF77CALLS_H_ */
