/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from various
metadata files and meta_parameters structure.
   Internal-only routine.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include "asf_meta.h"


/*Raw_init_state:
	Creates a state_vectors structure big
enough to hold the given number of state vectors.
*/
meta_state_vectors *raw_init_state(int nState)
{
	meta_state_vectors *s;
	s=(meta_state_vectors *)MALLOC(sizeof(meta_state_vectors)
				       + nState*sizeof(state_loc));
	s->num=nState;
	return s;
}

/*raw_init:
	Creates and initializes a meta_parameters
structure, guessing at conceivable values.
These bogus values always end in "989", so
you can tell them from real values.
*/
meta_parameters *raw_init(void)
{
	
/*Create new structure.*/
#define newStruct(type) (type *)MALLOC(sizeof(type))
	meta_parameters *sar=newStruct(meta_parameters);
	sar->geo=newStruct(geo_parameters);
	sar->ifm=newStruct(ifm_parameters);
	sar->stVec=NULL;
	sar->info=NULL;

/*Guess at conceivable values.*/
	sar->geo->type='G';
	sar->geo->proj=NULL;
	sar->geo->lookDir='R';
	sar->geo->deskew=0;
	sar->geo->xPix=sar->geo->yPix=10.989;
	sar->geo->rngPixTime=5.989e-8;
	sar->geo->azPixTime=7.989e-4;
	sar->geo->timeShift=sar->geo->slantShift=0;
	sar->geo->slantFirst=800000.989;
	sar->geo->wavelen=0.056989;
	sar->geo->dopRange[0]=
	sar->geo->dopRange[1]=
	sar->geo->dopRange[2]=0.0;
	sar->geo->dopAz[0]=
	sar->geo->dopAz[1]=
	sar->geo->dopAz[2]=0.0;
	
	sar->ifm->er=6360000.989;
	sar->ifm->ht=sar->ifm->er+700000.0;
	sar->ifm->nLooks=5;
	sar->ifm->orig_nLines=25989;
	sar->ifm->orig_nSamples=5989;
	sar->ifm->lookCenter=19.989;

	return sar;
}


/*meta_init: 
	Reads in a new meta_parameters record from
disk with the given filename.  If no .meta
exists, it calls create_meta to construct 
one.
*/
#ifndef WIN32
#include "unistd.h"/*For getpid()*/
#endif
meta_parameters *meta_init(const char *fName)
{
	if (extExists(fName,".meta")) /*Read .meta file if possible*/
		return meta_read(fName);
	else
		return meta_create(fName);
}

/*meta_free:
	Disposes of a given metadata parameters record.
*/
void meta_free(meta_parameters *meta)
{
	if (meta->geo->proj!=NULL)
		free((void *)meta->geo->proj);
	free((void *)meta->geo);
	free((void *)meta->ifm);
	if (meta->stVec!=NULL)
		free((void *)meta->stVec);
	meta->geo=NULL;
	meta->ifm=NULL;
	meta->stVec=NULL;
	free((void *)meta);
}

/*final_init:
	This routine is called after all other parameters
have been read in and filled out.  It computes cached
values, etc.
*/
void final_init(meta_parameters *sar)
{
	if (sar->geo->type!='P') /*Image not map projected-- compute look angle to beam center*/
		sar->ifm->lookCenter=meta_look(sar,
			0,sar->ifm->orig_nSamples/2);
	else 
	{/*Image *is* map projected-- compute earth's eccentricity*/
		double RE=sar->geo->proj->re_major;
		double RP=sar->geo->proj->re_minor;
		sar->geo->proj->ecc=sqrt(1.0-RP*RP/(RE*RE));
	}
}
