#include "worgen.h"
#include "cproj.h"
#include "proj.h"
#include "deskew_dem.h"
#include "asf_meta.h"


void set_latlon_corners(struct DDR *outddr,
	double tlLat,double tlLon,double trLat,double trLon,
	double blLat,double blLon,double brLat,double brLon)
{
	int iflag=0;
	forward_transform for_trans[100],forXform;
	
	outddr->proj_code=UTM;  
	outddr->zone_code = calc_utm_zone(tlLon);
	if (tlLat < 0)
		outddr->zone_code = -outddr->zone_code;
	outddr->datum_code=0;
	
	for_init(outddr->proj_code,outddr->zone_code,outddr->proj_coef,outddr->datum_code,
		NULL,NULL,&iflag,for_trans);
	
	forXform=for_trans[outddr->proj_code];
	
	forXform(tlLon*D2R,tlLat*D2R,&outddr->upleft[1],&outddr->upleft[0]);
	forXform(blLon*D2R,blLat*D2R,&outddr->loleft[1],&outddr->loleft[0]);
	forXform(trLon*D2R,trLat*D2R,&outddr->upright[1],&outddr->upright[0]);
	forXform(brLon*D2R,brLat*D2R,&outddr->loright[1],&outddr->loright[0]);
	
	outddr->valid[0]=outddr->valid[1]=
	outddr->valid[2]=outddr->valid[3]=outddr->valid[6]=1;
}

void set_ddr_corners(char *ceos,struct DDR *outddr)
{
	int x,y;
	int nl=outddr->nl,ns=outddr->ns;
	double nearelat,nearelon,nearslat,nearslon;
	double farelat,farelon,farslat,farslon;
	meta_parameters *meta=meta_init(ceos);
	
	meta_get_orig((void *)outddr,1,1,&y,&x);
	meta_get_latLon(meta,(float)y,(float)x,0.0,&farelat,&farelon);
	meta_get_orig((void *)outddr,1,ns,&y,&x);
	meta_get_latLon(meta,(float)y,(float)x,0.0,&nearelat,&nearelon);
	meta_get_orig((void *)outddr,nl,1,&y,&x);
	meta_get_latLon(meta,(float)y,(float)x,0.0,&farslat,&farslon);
	meta_get_orig((void *)outddr,nl,ns,&y,&x);
	meta_get_latLon(meta,(float)y,(float)x,0.0,&nearslat,&nearslon);
	set_latlon_corners(outddr,
		farelat,farelon,nearelat,nearelon,
		farslat,farslon,nearslat,nearslon);
}
