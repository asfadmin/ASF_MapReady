/*SARview image_desc.c:
The image pixel-oriented routines, used to extract
various textual descriptions of the current window.
Currently, LAS and CEOS images are supported.
ddr.h
*/
#include "main.h"
#include "image.h"

/*Write a description of the current image to the given string*/
void image_describeImage(char *dest)
{
	char *dtypeDesc=NULL;

	dtype2dsize(ddr->dtype,&dtypeDesc);
	sprintf(dest,
		"%s is a %s image.\n"
		"It is %d lines by %d samples, and the pixels are:\n"
		"    %s\n"
		"\n"
		"Your window is zoomed out by a factor of %.4f, so\n"
		"you see the image as being %d lines by %d samples.\n"
		"\n"
		"The pixels are converted to bytes via the equation\n"
		"      out_byte=%.7f*in_pix+%.7f\n"
		"This maps [%f,%f] to [0,255]\n\n"
		,
		image.fName,(type==image_ddr)?"LAS":"CEOS",
		image.height,image.width,
		     dtypeDesc,
		
		link_zoom,(int)(image.height/link_zoom),(int)(image.width/link_zoom),

		image.slope,image.offset,
		scale_fromByte(0),scale_fromByte(255)
	);
	if (meta!=NULL)
	{
#define prt sprintf(&dest[strlen(dest)],
		switch(meta->sar->image_type)
		{
		  case 'S':prt "The image is in slant range.\n");break;
		  case 'G':prt "The image is in ground range.\n");break;
		  case 'P':
			prt "The image is map-projected ");
			if (meta->projection) {
			  switch(meta->projection->type) {
			    case 'U':
				prt "to the Universal Transverse Mercator projection--\n"
					"zone %d.\n",
					meta->projection->param.utm.zone);
				break;
			    case 'P':
				prt "to the Polar Stereographic projection--\n"
					"reference latitude %f, longitude %f.\n",
					 meta->projection->param.ps.slat,
					 meta->projection->param.ps.slon);
				break;
			    case 'A':
			  	prt "to the Along Track/Cross Track projection.\n");
				break;
			    default:
			  	prt "to the unknown projection '%c'.\n",
			  		meta->projection->type);
				break;
			  }
			}
			break;
		  default:
			prt "The image is of the unknown type '%c'.\n",
				meta->sar->image_type);
			break;
		}
		prt "Pixels are %f meters wide, and %f meters high\n",meta->general->x_pixel_size,meta->general->y_pixel_size);
		if (meta->state_vectors) {
			prt "The image was acquired on julian day %d of %d, at %f seconds past midnight GMT\n",
				meta->state_vectors->julDay,
				meta->state_vectors->year,
				meta->state_vectors->second);
		}
		if (meta->sar->look_direction=='L')
			prt "The data is left-looking\n");
	}
	if (image_projOK())
	{
		prt "The DDR indicates pixels are %f meters wide and %f meters high\n",ddr->pdist_x,ddr->pdist_y);
	}
}


/*return 1 if the image projection is valid; 0 if it is not*/
int image_projOK(void)
{
	return ((ddr!=NULL)&&(ddr->valid[DDCCV]!=0)&&
			(ddr->pdist_x!=0.0)&&(ddr->pdist_y!=0.0));
}

/*Return the projection coordinates of the given point*/
void image_point2proj(double x,double y,double *projX,double *projY)
{
	*projX=ddr->upleft[1]+x*ddr->pdist_x;
	*projY=ddr->upleft[0]-y*ddr->pdist_y;
}
