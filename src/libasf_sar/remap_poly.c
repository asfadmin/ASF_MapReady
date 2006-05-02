#include "asf.h"
#include "asf_meta.h"
#include "matrix.h"
#include "remap.h"
#include <assert.h>

void poly_doMap(polyMapRec * map, fPoint  in, fPoint  *out)
{
  out->x=poly_eval(map->outToInX,in.x,in.y);
  out->y=poly_eval(map->outToInY,in.x,in.y);
}

polyMapRec * createPolyMap(poly_2d *fwX, poly_2d *fwY,
			   poly_2d *bwX, poly_2d *bwY)
{
  polyMapRec *q=(polyMapRec *)MALLOC(sizeof(polyMapRec));
  q->doMap=(mappingFunc)poly_doMap;
  q->inToOutX=fwX;
  q->inToOutY=fwY;
  q->outToInX=bwX;
  q->outToInY=bwY;
  return q;
}

/*grow: Increases the size of an axis-aligned box defined by two points min and max.*/
void grow(fPoint *min,fPoint *max,fPoint addThis)
{
	if (addThis.x<min->x)
		min->x=addThis.x;
	if (addThis.x>max->x)
		max->x=addThis.x;
	if (addThis.y<min->y)
		min->y=addThis.y;
	if (addThis.y>max->y)
		max->y=addThis.y;
}

fPoint makePoint(float x,float y)
{
	fPoint ret;
	ret.x=x;
	ret.y=y;
	return ret;
}

void forwardPolyMap(polyMapRec *map, fPoint in, fPoint *out)
{
  out->x=poly_eval(map->inToOutX,in.x,in.y);
  out->y=poly_eval(map->inToOutY,in.x,in.y);
}

void calc_extents(polyMapRec *map, meta_parameters *in_meta,
		  int *lines, int *samples)
{
/*Depending on the mapping function, we'll calculate the output
  extents differently.  In general, we're going to make the output
  image as big as it needs to be to hold the positive X and Y part
  of the image, BUT we'll truncate the image if it crosses the X
  or Y axis into negative coordinates.*/

  fPoint max={-1000000000,-1000000000},min={1000000000,1000000000},cur;
//  updateDDR(map,inDDR,outDDR);
  
  forwardPolyMap(map,makePoint(0,0),&cur);
  grow(&min,&max,cur);
  
  forwardPolyMap(map,makePoint(0,in_meta->general->line_count),&cur);
  grow(&min,&max,cur);
  
  forwardPolyMap(map,makePoint(in_meta->general->sample_count,0),&cur);
  grow(&min,&max,cur);
  
  forwardPolyMap(map,makePoint(in_meta->general->sample_count,
			   in_meta->general->line_count),&cur);
  grow(&min,&max,cur);
  
  *lines = (int)ceil(max.y);
  *samples = (int)ceil(max.x);
}

float bilinear_doSamp(pixelFetcher *getRec,fPoint inPt)
{
  /*ix and iy used to be computed as floor(inPt); but this is 
    much faster and gives the same results for inPt>-9*/
  int ix=(int)(inPt.x+10)-10,iy=(int)(inPt.y+10)-10;
  register float dx=inPt.x-ix,dy=inPt.y-iy;
  float tl=fetchPixelValue(getRec,ix,iy),
    tr=fetchPixelValue(getRec,ix+1,iy);
  float bl=fetchPixelValue(getRec,ix,iy+1),
    br=fetchPixelValue(getRec,ix+1,iy+1);
  float tc=tl+(tr-tl)*dx;
  float bc=bl+(br-bl)*dx;
  return tc+(bc-tc)*dy;
}

/***************Perform_mapping-- the most important call********************
  This is the function which does the file I/O and image manipulation.
  There are a few caveats:
    If complex-valued input is specified, the output DDR must also have a dtype of
      either DTYPE_COMPLEXREAL or DTYPE_COMPLEXIMAG.
      If either of these options are specified, the output file will be
      created in a bizarre way-- only the correct fields of the input image
      will be read in, and only those same fields will be written out.
    Hence, to generate a correct Complex image, you have to call perform_mapping twice--
      once to remap the real fields, and again to remap the imaginary fields.
*/
void perform_mapping(FILE *in, meta_parameters *meta_in,
		     FILE *out, meta_parameters *meta_out,
		     polyMapRec *map)
{
  int x,y;
  int maxOutX,maxOutY;

  maxOutX=meta_out->general->sample_count;
  maxOutY=meta_out->general->line_count;

  float *thisLine=(float *)MALLOC(maxOutX*sizeof(float));
  
  pixelFetcher *getRec=createFetchRec(in, meta_in);
  	
/*Iterate over each pixel of the output image.*/
  for (y=0;y<maxOutY;y++)
  {
    fPoint outPt,inPt;
    outPt.y=y;
    for (x=0;x<maxOutX;x++)
    {
      outPt.x=x;
      /*Compute the pixel's position in input space...*/
      map->doMap((void *)map,outPt,&inPt);
			
      /*Now read that pixel's value and put it in the "thisLine" array 
	as a float...*/
      thisLine[x]=bilinear_doSamp(getRec,inPt);
    }
	
    put_float_line(out,meta_out,y,thisLine);
    //writePixelLine(out,outDDR,y,bandNo,thisLine,outBuf);
  }

  killFetchRec(getRec);
  FREE(thisLine);
}

/*GetProjection:
	Interpolates the projection coordinates of the given DDR 
to find the projection coordinate of the given (pixel) point (x,y).
If val==0, Northing coordinates are returned.  
If val==1, Easting coordinates are returned.*/
float getProjection(float x,float y,cornerCoords *cc,int val,int ns, int nl)
{
	float dx=x/ns,dy=y/nl;
	float upint=cc->upleft[val]+(cc->upright[val]-cc->upleft[val])*dx;
	float loint=cc->loleft[val]+(cc->loright[val]-cc->loleft[val])*dx;
	return upint+(loint-upint)*dy;
}

/*UpdateProjection:
	Updates the projection corner coordinates for the new 
DDR.  It does so by reverse-projecting the corners of the new, output DDR
into the old, trusted, input DDR space.  The coordinates of the corners of the
output DDR are computed in input space using getProjection.	
*/
void update_projection(meta_parameters *in_meta, polyMapRec *map,
		       meta_parameters *out_meta)
{
  cornerCoords in, out;
  int ns, nl;

  ns = in_meta->general->sample_count;
  nl = in_meta->general->line_count;

  assert(in_meta->projection);
  meta_projection *proj = in_meta->projection;

  in.upleft[0] = proj->startY;
  in.upleft[1] = proj->startX;
  in.loleft[0] = proj->startY + in_meta->general->line_count * proj->perY;
  in.loleft[1] = proj->startX;
  in.upright[0] = proj->startY;
  in.upright[1] = proj->startX + in_meta->general->sample_count * proj->perX;
  in.loright[0] = proj->startY + in_meta->general->line_count * proj->perY;
  in.loright[1] = proj->startX + in_meta->general->sample_count * proj->perX;

  fPoint inPt,outPt;
  inPt.x=inPt.y=0;
  map->doMap((void *)map,inPt,&outPt);
  
  out.upleft[0]=getProjection(outPt.x,outPt.y,&in,0,ns,nl);
  out.upleft[1]=getProjection(outPt.x,outPt.y,&in,1,ns,nl);
	
  inPt.x=out_meta->general->sample_count;
  inPt.y=0;
  map->doMap((void *)map,inPt,&outPt);

  out.upright[0]=getProjection(outPt.x,outPt.y,&in,0,ns,nl);
  out.upright[1]=getProjection(outPt.x,outPt.y,&in,1,ns,nl);
	
  inPt.x=0;inPt.y=out_meta->general->line_count;
  map->doMap((void *)map,inPt,&outPt);

  out.loleft[0]=getProjection(outPt.x,outPt.y,&in,0,ns,nl);
  out.loleft[1]=getProjection(outPt.x,outPt.y,&in,1,ns,nl);
  
  inPt.x=out_meta->general->sample_count;
  inPt.y=out_meta->general->line_count;
  map->doMap((void *)map,inPt,&outPt);

  out.loright[0]=getProjection(outPt.x,outPt.y,&in,0,ns,nl);
  out.loright[1]=getProjection(outPt.x,outPt.y,&in,1,ns,nl);

  proj = out_meta->projection;
  assert(proj);

  proj->startY = out.upleft[0];
  proj->startX = out.upleft[1];
  proj->perY = (out.loright[0] - proj->startY) / 
                  out_meta->general->line_count;
  proj->perX = (out.loright[1] - proj->startX) / 
                  out_meta->general->sample_count;
  proj->hem = (out_meta->general->center_latitude>0) ? 'N' : 'S';
}

int remap_poly(poly_2d *fwX, poly_2d *fwY, poly_2d *bwX, poly_2d *bwY,
	       int outWidth, int outHeight, char *infile, char *outfile)
{
  FILE *in,*out=NULL;
  meta_parameters *meta_in, *meta_out;
  polyMapRec *map;
  int out_lines, out_samps;

  map = createPolyMap(fwX, fwY, bwX, bwY);

  in = fopenImage(infile,"rb");
  out = fopenImage(outfile,"wb"); FCLOSE(out); /*Create the image*/

  /*Re-Open for append (this lets us read & write).*/
  out = fopenImage(outfile,"r+b");

  meta_in = meta_read(infile);
  meta_out = meta_read(infile);
  calc_extents(map, meta_in, &out_lines, &out_samps);
  meta_out->general->sample_count = outWidth > 0 ? outWidth : out_samps;
  meta_out->general->line_count = outHeight > 0 ? outHeight : out_lines;
  meta_out->general->data_type = REAL32;
  fPoint origin={0,0}, outOrigin;
  map->doMap(map, origin, &outOrigin);
  update_projection(meta_in, map, meta_out);
  meta_write(meta_out, outfile);

  perform_mapping(in, meta_in, out, meta_out, map);
  FCLOSE(out);
  return 1;
}
