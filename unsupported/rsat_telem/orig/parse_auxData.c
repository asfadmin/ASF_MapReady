#include <stdio.h>
#include <math.h>
#include "sizes.h"
#include "auxData.h"

#define BAD(mess) {fprintf(stderr,"****ERROR!****\n%s\n\n",mess);*NULL=0;return 0;} /*This crashes the program, to invoke the debugger.*/

int parse_auxData(packed_auxData *in, auxData *out)
{
	int r; /*Temporary variable for bit-reversals;*/
/* First, check the sync pattern and payload status.*/
	if (in->aux_sync!=0x352EF853)
		BAD("Sync pattern not correct in parse_auxData!\n");
	if (in->payload_mode!=image)
		BAD("Payload not in image mode!\n");
	if (in->image!=exec_image)
		BAD("Payload not im image mode!\n");
	if (in->calibration!=0)
		BAD("Payload is calibrating!\n");
	if (in->fault||in->warning||!in->auto_manual)
		BAD("Payload error!\n");
	if (in->image_type==0)
		out->image_type=realTime;
	else if (in->image_type==1)
		out->image_type=recorded;
	else BAD("Payload image_type is not valid.\n");
	
/* Next, find and parse the number of beams. */
	out->num_beams=in->number_of_beams+1;
	if (out->num_beams==1)
	{
		out->beam[1]=out->beam[2]=out->beam[3]=no_beam;
		if ((in->beam_sequence>(int)no_beam)&&(in->beam_sequence<(int)too_big_beam))
			out->beam[0]=(beam_type)in->beam_sequence;
		else BAD("Unknown beam sequence detected.\n");
		if (in->scan_strip!=1)
			BAD("Inconsistency between num_beams and payload status\n");
	} else {
		u32 beam_seq=in->beam_sequence;
		int i;
		for (i=0;i<out->num_beams;i++)
		{
			out->beam[i]=(beam_type)(beam_seq&0x000f);
			beam_seq>>=4;
		}
		for (i;i<4;i++)
			out->beam[i]=no_beam;
		if (in->scan_strip!=1)
			BAD("Inconsistency between num_beams and payload status\n");
	}
/* Check the gain control and attenuation settings. */
	if (in->replica_acg&1==0)
	{ /*Low bit of replica_acg not set-- reverse the bits of replica_acg.*/
		int r=in->replica_acg;
		out->nom_attenuation=(r>>5)+((r>>3)&2)+((r>>1)&4)+((r<<1)&8)+((r<<3)&16);
	} else {
		switch(in->replica_acg)
		{
		    case 0x07:
			out->nom_attenuation=32; break;
		    case 0x23:
			out->nom_attenuation=33; break;
		    case 0x17:
			out->nom_attenuation=34; break;
		    case 0x27:
			out->nom_attenuation=35; break;
		    case 0x0F:
			out->nom_attenuation=36; break;
		    case 0x2F:
			out->nom_attenuation=37; break;
		    case 0x1F:
			out->nom_attenuation=38; break;
		    case 0x3F:
			out->nom_attenuation=39; break;
		    default:
		    	BAD("replica_acg setting not valid.\n");
		}
	}
	/*Reverse the bits of the calibration attenuator*/
	r=in->caln_atten;
	out->caln_attenuation=6.10*(((r>>3)&1)+((r>>1)&2)+((r<<1)&4)+((r<<3)&8));
	
	
	
	return 1;
}

