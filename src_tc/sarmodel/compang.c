#include "asf.h"
#include "asf.h"


#include "constants.h"

double compang(tdtime,trka,normal,lg,lt,gi,li,form)

double tdtime;
double trka;
double normal[4];
double lg;
double lt;
double gi;
double li;
int    form;

{

double  angw, sinaw, cosaw,
	sinlg, coslg,
	sincl, coscl,
	sinct, cosct,
	xn, yn, zn, tx,
	ryz, rxz,
	phiv, phir, sinphir, 
	cosphia, tanphie, cosphi, rtcf;

	/***  Rotate by tdtime ***/
	angw = WE*tdtime;
	sinaw = sin(angw);
	cosaw = cos(angw);
	xn =      normal[1]*cosaw + normal[2]*sinaw;
	yn = -1.0*normal[1]*sinaw + normal[2]*cosaw;

	/***  Rotate by the longitude ***/
	sinlg = sin(lg);
	coslg = cos(lg);
	tx =  coslg*xn + sinlg*yn;
        yn = -sinlg*xn + coslg*yn;
        xn = tx;

	/***  Rotate by the latitude ***/
	sincl = cos(lt);
	coscl = sin(lt);
        tx =  coscl*xn - sincl*normal[3];
        zn =  sincl*xn + coscl*normal[3];
        xn = tx;

	/***  Rotate by the track angle ***/
        sinct = -1.0*sin(trka);
        cosct = -1.0*cos(trka);
        tx =  cosct*xn - sinct*yn;
        yn =  sinct*xn + cosct*yn;
        xn = tx;

	yn = -1.0*yn;
	
	ryz = sqrt( yn*yn + zn*zn );
        rxz = sqrt( xn*xn + zn*zn );

	phiv = asin(yn/ryz);
	phir = gi+phiv;
	sinphir = fabs(sin(phir));

 	if (zn>1.0) zn = 1.0;
	else if (zn<-1.0) zn = -1.0;
	cosphia = zn/rxz;

	tanphie = tan(gi);
	cosphi  = cos(li);

	switch (form)
          {
	    case 1: /* ftcli */
  		    rtcf = tan(li) / tanphie; 
		    break;
 	    case 2: /* ftcgo */
     		    rtcf = (sinphir * cosphia) / (tanphie * cosphi);
		    break;
	    case 3: /* ftcsq */
 		    rtcf = (sinphir * cosphia) / (tanphie * sqrt(cosphi));
		    break;
	    case 4: /* ftcvx */
 		    rtcf = (sinphir * cosphia) / sin(gi);
		    break;
 	    default:
		    printf("Error in compang : Unknown formaula\n");
		    exit(1);
		    break;
	  }

	return(rtcf);
}
