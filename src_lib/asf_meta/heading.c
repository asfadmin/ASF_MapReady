#include "asf.h"
#include "geolocate.h"
double getHeading(const stateVector *stVec)
{
	double r,phi,heading;
	vector pos,vel,north,out;
	pos=stVec->pos;
	vel=stVec->vel;
	r=sqrt(pos.x*pos.x+pos.y*pos.y);
	phi=atan2_check(pos.z,r);
	north.x=-pos.x/r*sin(phi);
	north.y=-pos.y/r*sin(phi);
	north.z=cos(phi);
	vecNormalize(&vel);
	heading=acos(vecDot(vel,north));
	vecCross(vel,north,&out);
	if (vecDot(pos,out)<0)
		heading*=-1.0;
	return heading;
}
