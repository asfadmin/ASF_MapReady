/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  sv_attd(x,y,z,x_vel,y_vel,z_vel,attitude)  -----------

  'sv_attd.c' calculates the coorections about the satellite roll,
  pitch and yaw axes in the local orbital reference frame in order to
  effect a local normal pointing to the Earth reference ellipsoid. 
   
INPUTS: 
variables:	vx,vy,vz
type:		double
description:	Velocity in the x,y,z direction in km/sec.

OUTPUTS:
variable:	attitude[3]	
type:		double
description:	3 element vector containing the correction for yaw
		pitch and roll respectively. The return angles are
		in degrees.



Algorithm:	attitude[0]   = arctan( (sin i.cos u)/(14+(1/3)-cos i))
		attitude[1]   = -e**2.(ae/a).sin**2 i.sin u.cos u
		attitude[2]   = -e**2.(ae/a).sin    i cos i.sin u

		where
		ee = sqrt(fe.(2-fe)),eccentricity of earth ref.ellipsoid
		fe = 1/298.257,oblateness of earth ref. ellipsoid
		ae = 6378.144 km, semimajor axis of earth ref. ellipsoid
		a  = mean orbit semimajor axis in (km)
		i  = mean orbit inclination(rad)
		u  = w + v, mean true anomaly of orbit position(rad)

*/

#include <math.h>
#include <stdio.h>

sv_attd(x,y,z,x_vel,y_vel,z_vel,attitude)
double  x,y,z,x_vel,y_vel,z_vel,attitude[3];

{
double kepler_elemnt[7];
double a,e,i,omega,w,M,u,ae;
double pi,i_r,u_r;


pi = 3.141592653589793;
ae = 6378.144;

crtsn2keplr(x,y,z,x_vel,y_vel,z_vel,kepler_elemnt);

a     = kepler_elemnt[0];
e     = kepler_elemnt[1];
i     = kepler_elemnt[2];
omega = kepler_elemnt[3];
w     = kepler_elemnt[4];
M     = kepler_elemnt[5];
u     = kepler_elemnt[6];

i_r = pi*i/180;
u_r = pi*u/180;

attitude[0]=atan((sin(i_r)*cos(u_r))/((14.+(1./3.) - cos(i_r))));
attitude[1]=(-1)*(e*e)*(ae/a)*sin(i_r)*sin(i_r)*sin(u_r)*cos(u_r);
attitude[2]=(-1)*(e*e)*(ae/a)*sin(i_r)*cos(i_r)*sin(u_r);

/* change to degree units */
attitude[0]=attitude[0]*180.0/pi;
attitude[1]=attitude[1]*180.0/pi;
attitude[2]=attitude[2]*180.0/pi;

return;

}



/*  crtsn2keplr(rx,ry,rz,vx,vy,vz,kepler)  ------------------

  crtsn2keplr.c' converts the Cartesian State Vector to Kepler coord.


INPUTS:
variables:	rx,ry,rz
type:		double

variables:	vx,vy,vz
type:		double
description:	Velocity in the x,y,z direction in Km/sec.

OUTPUTS:
variable:	kepler[7]
type:		double
description:	the resultant Kepler transformation consisting of the
		six Kepler elements,namely, 
		kepler[0]= 'a' the semi-major axis in Km
		kepler[1]= 'e' the eccentricity
		kepler[2]= 'i' angle of inclination (deg)
		kepler[3]= 'Omega' longitude of ascending node (deg.)
		kepler[4]= 'w' argument of periapsis (deg.)
		kepler[5]= 'M' mean anomaly (deg.)
		and additionally
		kepler[6]= 'u_deg' argument of true latitude(deg.)
*/

crtsn2keplr(rx,ry,rz,vx,vy,vz,kepler)
double rx,ry,rz,vx,vy,vz,kepler[7];

{
double a,u,u_deg,r,v,es,ec,e,E,M_deg,V2,H,i,i_deg;
double w_deg,cu,su,somega,comega,omega;
double pi, mu;

pi = 3.141592653589793;
mu = 3.9860045e+5;


 /* determine semi major axis 'a' */ 
r = sqrt((rx*rx)+(ry*ry)+(rz*rz));
V2 = (vx*vx)+(vy*vy)+(vz*vz);
a = (mu*r)/((2*mu)-(r*V2));

 /* determine eccentricity 'e' */
es = ((rx*vx)+(ry*vy)+(rz*vz))/sqrt(mu*a);
ec = 1 - (r/a);
e = sqrt((es*es)+(ec*ec));

/* determine mean anomaly'M' */
E = 2*atan((e-ec)/es);
M_deg = (180/pi)*(E-es);
if(M_deg<0)
   M_deg = M_deg+360;

/* determine angle of inclination 'i' */
H = sqrt(mu*a*(1-(e*e)));
i = acos(((rx*vy)-(ry*vx))/H);
i_deg = i*(180/pi);

/* determine omega */
somega = ((ry*vz)-(rz*vy))/(sin(i)*H);
comega = ((rx*vz)-(rz*vx))/(sin(i)*H);
omega = (180/pi)*2*atan((1-comega)/somega);
if (omega<0)
   omega = omega+360;

/* determine w_deg */
su = rz/(r*sin(i));
cu = ((ry/r)*somega)+((rx/r)*comega);
u = 2*atan((1-cu)/su);
u_deg = (180/pi)*u;
if((u_deg)<0)
   u_deg= u_deg+360;

v = 2*atan(sqrt((1+e)/(1-e))*tan(E/2));
w_deg = (180/pi)*(u-v);
if (w_deg<0)
   w_deg = w_deg+360;


kepler[0] = a;
kepler[1] = e;
kepler[2] = i_deg;
kepler[3] = omega;
kepler[4] = w_deg;
kepler[5] = M_deg;
kepler[6] = u_deg;
return;

}

