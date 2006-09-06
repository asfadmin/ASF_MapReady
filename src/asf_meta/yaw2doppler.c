#include "asf.h"
#include "geolocate.h"

double yaw2doppler(GEOLOCATE_REC *g,double slantRange,double yawAngleDeg)
{
	vector target,vRel;
        int err;
	double doppler,dopDot;
	double yawAngle=yawAngleDeg*D2R;
	double look;
	err=getLook(g,slantRange,yawAngle,&look);
	getDoppler(g,look,yawAngle,&doppler,&dopDot,&target,&vRel);
	return doppler;
}
