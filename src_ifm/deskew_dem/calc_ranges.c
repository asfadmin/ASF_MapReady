#include "deskew_dem.h"
#include "ddr.h"

double calc_ranges(const struct DDR *ddr)
{
	int x;
	double slantFirst,slantPer;
	double er=meta->ifm->er;
	double satHt=meta->ifm->ht;
	double saved_ER=er;
	double er2her2,phi,phiAtSeaLevel,slantRng;
	meta_get_slants(meta,&slantFirst,&slantPer);
	slantFirst+=slantPer*ddr->master_sample;
	slantPer*=ddr->sample_inc;
	er2her2=er*er-satHt*satHt;
	minPhi=acos((satHt*satHt+er*er-slantFirst*slantFirst)/(2.0*satHt*er));
/*Compute arrays indexed by slant range pixel:*/
	for (x=0;x<ns;x++)
	{
	/*Precompute slant range for SR pixel x.*/
		slantRange[x]=slantFirst+x*slantPer;
		slantRangeSqr[x]=slantRange[x]*slantRange[x];
	/*Compute incidence angle for SR pixel x.*/
		incidAng[x]=M_PI-acos((slantRangeSqr[x]+er2her2)/(2.0*er*slantRange[x]));
		sinIncidAng[x]=sin(incidAng[x]);
		cosIncidAng[x]=cos(incidAng[x]);
	}
	
	maxPhi=acos((satHt*satHt+er*er-slantRangeSqr[ns-1])/(2.0*satHt*er));
	phiMul=(ns-1)/(maxPhi-minPhi);

/*Compute arrays indexed by ground range pixel: slantGR and heightShiftGR*/
	for (x=0;x<ns;x++)
	{
		er=saved_ER;
		phiAtSeaLevel=grX2phi(x);
		slantRng=sqrt(satHt*satHt+er*er-2.0*satHt*er*cos(phiAtSeaLevel));
		slantGR[x]=(slantRng-slantFirst)/slantPer;
		er+=1000.0;
		phi=acos((satHt*satHt+er*er-slantRng*slantRng)/(2*satHt*er));
		
		heightShiftGR[x]=(phi2grX(phi)-x)/1000.0;
	}
/*Compute arrays indexed by slant range pixel: groundSR and heightShiftSR*/
	for (x=0;x<ns;x++)
	{
		er=saved_ER;
		phiAtSeaLevel=acos((satHt*satHt+er*er-slantRangeSqr[x])/(2*satHt*er));
		groundSR[x]=phi2grX(phiAtSeaLevel);
		er+=1000.0;
		slantRng=sqrt(satHt*satHt+er*er-2.0*satHt*er*cos(phiAtSeaLevel));
		
		heightShiftSR[x]=((slantRng-slantFirst)/slantPer-x)/1000.0;
	}
	er=saved_ER;
	return er/phiMul;
}
