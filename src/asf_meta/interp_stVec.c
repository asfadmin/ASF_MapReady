/*Subroutine interp_stVec:
	Interpolates a state vector, given a starting 
state vector and time, ending state vector and time, and
some other time.

	Assumes an inertial coordinate frame, and that
state vector's units are self-consistent (e.g. if state vector
position (elements 0-2) is in kilometers, then velocity
(elements 3-5) must be in kilometers/second).
	
*/

#include "geolocate.h"
void stVec2array(stateVector *in,double *out);
void stVec2array(stateVector *in,double *out)
{
	out[0]=in->pos.x;out[1]=in->pos.y;out[2]=in->pos.z;
	out[3]=in->vel.x;out[4]=in->vel.y;out[5]=in->vel.z;
}
void array2stVec(double *in,stateVector *out);
void array2stVec(double *in,stateVector *out)
{
	out->pos.x=in[0];out->pos.y=in[1];out->pos.z=in[2];
	out->vel.x=in[3];out->vel.y=in[4];out->vel.z=in[5];
}
void interp_stVec(stateVector *st1_struct,double time1, 
				stateVector *st2_struct, double time2,
				stateVector *stOut_struct,double timeOut)
{
	int i;
	double st1[6],st2[6],stOut[6];
	double coefs[3][4],t,t2,t3,deltaT=time2-time1;
	stVec2array(st1_struct,st1);
	stVec2array(st2_struct,st2);
	for (i=0;i<3;i++)
	{/*For each coordinate axis, set up an interpolation function r(t)
	such that r(0.0)=st1.pos, r(1.0)=st2.pos, r'(0.0)=st1.vel, and r'(1.0)=st2.vel.*/
		double A,B,Av,Bv;
		A=st1[i];
		B=st2[i];
		Av=st1[i+3]*deltaT;
		Bv=st2[i+3]*deltaT;
		coefs[i][0]=A;
		coefs[i][1]=Av;
		coefs[i][2]=3*B-3*A-2*Av-Bv;
		coefs[i][3]=2*A-2*B+Av+Bv;
	}
	/*Now we evaluate that function at timeOut.*/
	t=(timeOut-time1)/deltaT;
	t2=t*t;t3=t2*t;
	for (i=0;i<3;i++)
	{/*For each coordinate, evaluate the function at t.*/
		stOut[i]=coefs[i][0]+coefs[i][1]*t+coefs[i][2]*t2+coefs[i][3]*t3;
		stOut[i+3]=(coefs[i][1]+2.0*coefs[i][2]*t+3.0*coefs[i][3]*t2)/deltaT;
	}
	array2stVec(stOut,stOut_struct);
}
