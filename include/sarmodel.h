/*
   SARMODEL.H

   Header file for SARMODEL library. Mainly contains the functions I and
   others have used to hack it.

   Mike Shindle
*/

#ifndef __SARMODEL_H
#define __SARMODEL_H

double calcfd2(double[],double[],double,double);
double compang(double,double,double[],double,double,double,double,int);
double dot(double [],double []);
double fd2dot(double[],double[],double,double);
double los(double[],double[]);
double mag(double []);
double packtar(double*,double*,double*,double[],double[]);
void init_phase_model(double [],double [],double,char *,int,double);
void sar_phase(double,double,double,double,double,double,double,double,double,
               double,double,double,double,double,double,int,double*,double*,
               double*,double*,double*);
double local_incid_angle(double[],double[],double[]);

 void sar_sim (double latdt, double longt, double elevt, 
 	double latdt1, double longt1, double elevt1, 
 	double latdt2, double longt2, double elevt2, 
 	double latdt3, double longt3, double elevt3, 
 	double latdt4, double longt4, double elevt4, 
 	int viewflg, double *intensity, double *sini, double *mask, 
 	double *line, double *sample); 

 void init_sar_model (double *azcoefs, double *grcoefs, 
 	double user_val_pix, char *hdname, int frm, ...);

 void init_sar_model_ssv (double *azcoefs, double *grcoefs, 
 	double user_val_pix, char *hdname, int frm, ...);

#endif
