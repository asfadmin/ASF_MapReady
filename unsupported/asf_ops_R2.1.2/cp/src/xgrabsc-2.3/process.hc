/*========================================================================
 *
 * Name - process.hc
 *
 * ccs version:	1.2
 *
 * ccsid:	@(#)process.hc	1.2 - 07/06/92 10:52:51
 * from: 	ccs/s.process.hc
 * date: 	06/28/93 09:14:49
 *
 * Description:  color processing functions for xgrabsc
 *
 *               see cpyright.h for copyright information
 *
 *
 *========================================================================
 */

/*
 * Alter colors by setting or clearing bits in rgb values.
 * This effectively reduces the depth of the image, causing the
 * number of colors used to be reduced.  Equivalent colors are
 * merged in the image, and the used flags of remapped colors are
 * cleared.
 *
 * The number of eliminated colormap entries is returned.  The colormap
 * is not compressed.
 */
alterPlanes(image, modeIsAnd, bits)
  imageInfo *image;
  int modeIsAnd;      /* if TRUE, combine mask with AND; if FALSE, use OR */
  unsigned int bits;
{
  int nc, cidx, ridx, h, w;
  long p;
  XImage *ximage = image->ximage;
  long map[MAX_CELLS];
  int remapCount;
  word mask;

  if (ximage->depth <= 1)
    return 0;

  mask = 0xFFFF ^ ((1 << (bits+8)) - 1);
  if (!modeIsAnd)
    mask = ~mask & 0xFFFF;

  if (verbose) {
    fprintf(stderr, "%s: %s color with mask %x...", programName,
            modeIsAnd? "ANDing" : "ORing", mask);
    fflush(stderr);
  }

  nc = image->numcells;
  if (modeIsAnd)
    for (cidx=0; cidx<nc; cidx++) {
      nr[cidx] = image->red[cidx]   & mask;
      ng[cidx] = image->green[cidx] & mask;
      nb[cidx] = image->blue[cidx]  & mask;
    }
  else
    for (cidx=0; cidx<nc; cidx++) {
      nr[cidx] = image->red[cidx]   | mask;
      ng[cidx] = image->green[cidx] | mask;
      nb[cidx] = image->blue[cidx]  | mask;
    }

  /* now eliminate redundant colors */
  for (cidx=0; cidx<nc; cidx++)
    map[cidx] = cidx;
  remapCount = 0;
  for (cidx=0; cidx<nc; cidx++)
    if (image->used[cidx])
      for (ridx=cidx+1; ridx<nc; ridx++)
        if (image->used[ridx]  &&
            nr[cidx]==nr[ridx] &&
            ng[cidx]==ng[ridx] &&
            nb[cidx]==nb[ridx]) {
          /* the colors match - remap this pixel to the one we're scanning with */
          map[ridx] = cidx;
          image->used[ridx] = FALSE;
          remapCount++;
        }

  memcpy((char *)image->red,   (char *)nr, nc*sizeof(word));
  memcpy((char *)image->green, (char *)ng, nc*sizeof(word));
  memcpy((char *)image->blue,  (char *)nb, nc*sizeof(word));

  /* remap redundant pixels in the image */
  if (remapCount)
    for (h=0; h<ximage->height; h++)
      for (w=0; w<ximage->width; w++) {
        p = XGetPixel(ximage, w, h);
        if (p != map[p])
          XPutPixel(ximage, w, h, map[p]);
      }

  if (verbose)
    fprintf(stderr, "  %d colors remapped\n", remapCount, nc);
  return remapCount;
}





/* Brighten or darken colors in the image by the given amount ('percent').
 * The amount is an integer that, if less than 100 will darken the image
 * and if greater than 100 will brighten the image.  After modifying
 * colors equivalent colors are merged (as in alterPlanes).  The number
 * of eliminated colors is returned.
 */
brightenColors(image, percent)
  imageInfo *image;
  int percent;
{
  int nc, cidx, ridx, h, w;
  long p;
  XImage *ximage = image->ximage;
  float  adjustment;
  long map[MAX_CELLS];
  int remapCount;
  dw new;

  if (ximage->depth <= 1)
    return 0;

  if (verbose) {
    fprintf(stderr, "%s: adjusting intensity by %d...", programName, percent);
    fflush(stderr);
  }

  adjustment = (float)percent / 100.0;
  nc = image->numcells;
  for (cidx=0; cidx<nc; cidx++) {
    new = image->red[cidx] * adjustment;
    if (new > (dw)0xFFFF) new = (dw)0xFFFF;
    nr[cidx] = new;
    new = image->green[cidx] * adjustment;
    if (new > (dw)0xFFFF) new = (dw)0xFFFF;
    ng[cidx] = new;
    new = image->blue[cidx] * adjustment;
    if (new > (dw)0xFFFF) new = (dw)0xFFFF;
    nb[cidx] = new;
  }

  /* now eliminate redundant colors */
  for (cidx=0; cidx<nc; cidx++)
    map[cidx] = cidx;
  remapCount = 0;
  for (cidx=0; cidx<nc; cidx++)
    if (image->used[cidx])
      for (ridx=cidx+1; ridx<nc; ridx++)
        if (image->used[ridx]  &&
            nr[cidx]==nr[ridx] &&
            ng[cidx]==ng[ridx] &&
            nb[cidx]==nb[ridx]) {
          map[ridx] = cidx;
          image->used[ridx] = FALSE;
          remapCount++;
        }

  memcpy((char *)image->red,   (char *)nr, nc*sizeof(word));
  memcpy((char *)image->green, (char *)ng, nc*sizeof(word));
  memcpy((char *)image->blue,  (char *)nb, nc*sizeof(word));

  /* remap redundant pixels in the image */
  if (remapCount)
    for (h=0; h<ximage->height; h++)
      for (w=0; w<ximage->width; w++) {
        p = XGetPixel(ximage, w, h);
        if (p != map[p])
          XPutPixel(ximage, w, h, map[p]);
      }


  if (verbose)
    fprintf(stderr, "  %d colors remapped\n", remapCount, nc);

  return remapCount;
}





/* Reverse the colors in the image */
reverseColors(image)
  imageInfo *image;
{
  int nc, cidx;
  long size, idx;
  unsigned char *data;
  XImage *ximage;
  int map[2];

  if (verbose) {
    fprintf(stderr, "%s: reversing colors...", programName);
    fflush(stderr);
  }


  if (image->ximage->depth <= 1) {
    /* for black and white images, just reverse the bits */
    ximage = image->ximage;
    data = (unsigned char *)ximage->data;
    size = ximage->bytes_per_line * ximage->height;
    for (idx=0; idx<size; idx++, data++)
      *data = ~(*data);
  }
  else {
    /* for other images, reverse the color values in the color table */
    nc = image->numcells;
    for (cidx=0; cidx<nc; cidx++) {
      image->red[cidx]   = (unsigned short)~((unsigned short)(image->red[cidx]));
      image->blue[cidx]  = (unsigned short)~((unsigned short)(image->blue[cidx]));
      image->green[cidx] = (unsigned short)~((unsigned short)(image->green[cidx]));
    }
  }
}





/*
 * Compress the colors used in an XImage so that all pixel values are
 * adjacent.  Alters the rgb color tables and the XImage data values.
 */
compressColormap(image)
  imageInfo *image;
{
  XImage *ximage = image->ximage;
  long map[MAX_CELLS];
  int  ncolors, w, h, m;
  long p;

  if (ximage->depth <= 1  ||  image->numcells > MAX_CELLS)
    return;

  if (verbose) {
    fprintf(stderr, "%s: compressing colormap...", programName);
    fflush(stderr);
  }
  ncolors = 0;
  /* map[] is indexed by old pixel values.  It delivers new, compressed,
   * pixel values. */
  for (m=0; m<MAX_CELLS; m++) map[m] = MAX_CELLS+1;
  /* bludgeon through the whole image and remap each pixel value */
  for (h=0; h<ximage->height; h++) {
    for (w=0; w<ximage->width; w++) {
      /* Get the pixel index and see if it has been used or not.
       * Then remap the pixel */
      p = XGetPixel(ximage, w, h);
      if (map[p] == MAX_CELLS+1) {
        map[p] = ncolors;
        ncolors++;
      }
      if (p != map[p])
        XPutPixel(ximage, w, h, map[p]);
    }
  }
  /* now compress the color table */
  memset((char *)image->used, 0, MAX_CELLS);
  for (m=0; m<MAX_CELLS; m++) {
    if (map[m] != MAX_CELLS+1) {
      p = map[m];
      nr[p] = image->red[m];
      ng[p] = image->green[m];
      nb[p] = image->blue[m];
      image->used[p] = TRUE;
    }
  }
  memcpy((char *)image->red,   (char *)nr, ncolors*sizeof(word));
  memcpy((char *)image->green, (char *)ng, ncolors*sizeof(word));
  memcpy((char *)image->blue,  (char *)nb, ncolors*sizeof(word));
  image->numcells = ncolors;
  if (verbose)
    fprintf(stderr, "  %d colors used\n", ncolors);
}

