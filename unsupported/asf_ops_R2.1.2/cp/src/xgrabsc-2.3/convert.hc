/*========================================================================
 *
 * Name - convert.hc
 *
 * ccs version:	1.6
 *
 * ccsid:	@(#)convert.hc	1.6 - 04/22/93 16:27:04
 * from: 	ccs/s.convert.hc
 * date: 	06/28/93 09:14:48
 *
 * Description:  color->black&white conversions for xgrabsc
 *
 *               see cpyright.h for copyright information
 *
 *
 *========================================================================
 */


/*
 * convert a pixmap image into a bitmap image
 */
pixmap2bitmap(image)
  imageInfo *image;
{
  XImage *ximage = image->ximage;
  int x, y;
  word v, black, mid;
  dw total, blackrgb, midrgb, lowDelta, l;
  XImage *newImage;
  byte *newBytes;
  int usedCount;
  int blackp, whitep;

  if (ximage->bits_per_pixel == 1  ||  image->numcells < 1)
    return;


  blackp = BlackPixel(hDisplay,hScreen);
  whitep = WhitePixel(hDisplay,hScreen);

  /* get the darkest color */
  blackrgb = 0x2FFFD;  /* 3 * 0xFFFF == white */
  usedCount = total = 0;
  for (x=0; x<image->numcells; x++) {
    if (image->used[x]) {
      l = (unsigned)image->red[x]
          +(unsigned)image->green[x]
          +(unsigned)image->blue[x];
      if (l <= blackrgb) {
        black = x;
        blackrgb = l;
      }
      total += l;
      usedCount++;
    }
  }
  /* now find the mid color and use it as the cut-off for black */
  midrgb = total / usedCount;
  lowDelta = 0x2FFFD;
  for (x=0; x<image->numcells; x++) {
    if (image->used[x]) {
      l = (unsigned)image->red[x]
          +(unsigned)image->green[x]
          +(unsigned)image->blue[x];
      l -= midrgb;
      if (l < lowDelta) {
        mid = x;
        lowDelta = l;
      }
    }
  }
  midrgb = (unsigned)image->red[mid]
           +(unsigned)image->green[mid]
           +(unsigned)image->blue[mid];

  /* create a bitmap image */
  x = (ximage->width + 7) / 8;
  newBytes = (byte *)malloc(x * ximage->height);
  memset(newBytes, 0, x * ximage->height);
  newImage = XCreateImage(hDisplay, DefaultVisual(hDisplay, hScreen),
                1, XYBitmap, 0, newBytes, ximage->width, ximage->height,
                8, x);
  if (!newImage) {
    fprintf(stderr, "%s: unable to create bitmap for conversion\n",
      programName);
    XCloseDisplay(hDisplay);
    exit(3);
  }
  /* pound the pixels into it */
  for (y = 0; y < ximage->height; y++) {
    for (x = 0; x < ximage->width; x++) {
      v = XGetPixel(ximage, x, y);
      l = (dw)image->red[v]+(dw)image->green[v]+(dw)image->blue[v];
      XPutPixel(newImage, x, y, l<midrgb? blackp : whitep);
    }
  }
  free(ximage->data);
  memcpy((char *)ximage, (char *)newImage, sizeof(XImage));
  free(newImage);

  memset((char *)image->used, 0, MAX_CELLS);
  image->used[whitep] = 1;
  image->used[blackp] = 1;
  image->numcells = 2;
}







#define GRAYS    17 /* ((4 * 4) + 1) patterns for a good dither */
#define GRAYSTEP ((dw)(65536 / GRAYS))

static byte DitherBits[GRAYS][4] = {
  0xf, 0xf, 0xf, 0xf,
  0xe, 0xf, 0xf, 0xf,
  0xe, 0xf, 0xb, 0xf,
  0xa, 0xf, 0xb, 0xf,
  0xa, 0xf, 0xa, 0xf,
  0xa, 0xd, 0xa, 0xf,
  0xa, 0xd, 0xa, 0x7,
  0xa, 0x5, 0xa, 0x7,
  0xa, 0x5, 0xa, 0x5,
  0x8, 0x5, 0xa, 0x5,
  0x8, 0x5, 0x2, 0x5,
  0x0, 0x5, 0x2, 0x5,
  0x0, 0x5, 0x0, 0x5,
  0x0, 0x4, 0x0, 0x5,
  0x0, 0x4, 0x0, 0x1,
  0x0, 0x0, 0x0, 0x1,
  0x0, 0x0, 0x0, 0x0
  };

/* halftone or dither a color image, changing it into a monochrome
 * image
 */
pixmap2halftone(image, dither)
  imageInfo *image;
  ditherType dither;    /* type of dithering to perform */
{
  XImage *ximage = image->ximage;
  XImage *newImage;
  byte   *newBytes, *ditherBits;
  word   dindex;  /* index into dither array */
  dw     color;   /* pixel color */
  word  *index;   /* index into dither array for a given pixel */
  word   x, y;    /* random counters */
  word   x4, y4;
  register word   w, h;
  register byte  bits;
  char  *str;
  dw    intensity;
  int   maxIntensity, threshold;
  word  *fsIndex;
  int   err, i, ximageW, ximageH, rowL;
  int   *row1, *row2;
  int blackp = BlackPixel(hDisplay,hScreen);
  int whitep = WhitePixel(hDisplay,hScreen);

  if (ximage->depth <= 1  ||  dither == NO_DITHER)
    return;

  ximageW = ximage->width;
  rowL    = ximageW - 1;
  ximageH = ximage->height;

  if (verbose) {
    switch (dither) {
      case MATRIX_HALFTONE:
        str = "Matrix halfton";
        break;
      case MATRIX_DITHER:
        str = "Matrix dither";
        break;
      case FS_DITHER:
        str = "Floyd-Steinberg dither";
        break;
      default:
        fprintf(stderr, "%s: unknown type of dithering requested.  Exiting...\n",
            programName);
        exit(3);
    }
    fprintf(stderr, "%s: %sing image...", programName, str);
    fflush(stderr);
  }

  /* create a bitmap image */
  x = (dither == MATRIX_HALFTONE)? 4 : 1;
  w = ((ximageW + 7) / 8) * x;
  h = ximageH * x;
  newBytes = (byte *)malloc(w * h);
  memset((char *)newBytes, whitep=1?255:0, w * h);
  newImage = XCreateImage(hDisplay, DefaultVisual(hDisplay, hScreen),
                1, XYBitmap, 0, newBytes,
                ximageW * x,
                h,
                8, w);
  if (!newImage) {
    fprintf(stderr, "%s: unable to create bitmap for conversion\n",
      programName);
    XCloseDisplay(hDisplay);
    exit(3);
  }

  /* if the number of possible pixels isn't very large, build an array
   * which we index by the pixel value to find the dither array index
   * by color brightness.  we do this in advance so we don't have to do
   * it for each pixel.  things will break if a pixel value is greater
   * than (1 << depth), which is bogus anyway.  this calculation is done
   * on a per-pixel basis if the colormap is too big.
   */

  if (ximage->depth <= 16) {
    index= (word *)malloc(sizeof(word) * MAX_CELLS);
    fsIndex= (word *)malloc(sizeof(word) * MAX_CELLS);
    if (index)
      for (x= 0; x < image->numcells; x++) {
        fsIndex[x] = (word)(0.30 * image->red[x] +
                          0.59 * image->green[x] +
                          0.11 * image->blue[x]);
        index[x] = fsIndex[x]/GRAYSTEP;
        if (index[x] >= GRAYS)
          index[x] = GRAYS - 1;
      }
  }
  else
    index = fsIndex = NULL;

  if (dither == FS_DITHER) {
    maxIntensity = 65535;
    threshold = maxIntensity/2;
    row1 = (int *)malloc(ximageW*sizeof(int));
    row2 = (int *)malloc(ximageW*sizeof(int));
    /* initialize row2 */
    for (x= 0; x < ximageW; x++) {
      color = XGetPixel(ximage, x, 0);
      row2[x] = fsIndex? fsIndex[color] :
                  (dw)(0.30*image->red[color] +
                   0.59*image->green[color] +
                   0.11*image->blue[color]);
    }
    for (y= 0; y < ximageH; y++) {
      /* row1 := row2 */
      memcpy((char *)row1, (char *)row2, ximageW*sizeof(int));
      /* Fill in next row */
      if (y != ximageH-1)
        for (x= 0; x < ximageW; x++) {
          color = XGetPixel(ximage, x, y+1);
          row2[x] = fsIndex? fsIndex[color] :
                      (dw)(0.30*image->red[color] +
                       0.59*image->green[color] +
                       0.11*image->blue[color]);
        }
      for (x= 0; x < ximageW; x++) {
        color = XGetPixel(ximage, x, y);
        if ((i = row1[x]) > threshold)
          err = i - maxIntensity;
        else {
          XPutPixel(newImage, x, y, blackp);
          err = i;
        }
        /* Diagonal gets 1/4 of error. */
        if (x < rowL)
	  row2[x+1] += err/4;

        /* Right and below get 3/8 of error */
        err = err*3/8;
        row2[x] += err;
        if (x < rowL)
	  row1[x+1] += err;
      }
    }
    if (row1)  free(row1);
    if (row2)  free(row2);
  }


  else {  /* matrix dither or halftone */

    for (y= 0; y < ximageH; y++) {
      for (x= 0; x < ximageW; x++) {
        color = XGetPixel(ximage, x, y);
        dindex = index? index[color] :
                    (dw)(0.30*image->red[color] +
                     0.59*image->green[color] +
                     0.11*image->blue[color])/GRAYSTEP;
        if (dindex >= GRAYS)  /* catch rounding errors */
          dindex= GRAYS - 1;
        if (dither == MATRIX_DITHER) {
          if (DitherBits[dindex][y & 3] & (1 << (x & 3)))
             XPutPixel(newImage, x, y, blackp);
        }
        else { /* halftone */
          /* loop for the four Y bits in the dither pattern, putting all
           * four X bits in at once.  if you think this would be hard to
           * change to be an NxN dithering array, you're right, since we're
           * banking on the fact that we need only shift the mask based on
           * whether x is odd or not.  an 8x8 array wouldn't even need that,
           * but blowing an image up by 64x is probably not a feature.
           */
          ditherBits = &(DitherBits[dindex][0]);
          x4 = x * 4;
          y4 = y * 4;
          for (h= 0; h < 4; h++) {
            bits = ditherBits[h];
            for (w=0; w < 4; w++) {
              XPutPixel(newImage, x4+w, y4+h, bits & 1 ? blackp : whitep);
              bits /= 2;
            }
          }
        }
      }
    }
  }

  if (verbose)
    fputc('\n', stderr);

  free(ximage->data);
  memcpy((char *)ximage, (char *)newImage, sizeof(XImage));
  free(newImage);
  if (index) free(index);
  if (fsIndex) free(fsIndex);

  memset((char *)image->used, 0, MAX_CELLS);
  image->used[whitep] = 1;
  image->used[blackp] = 1;
  image->numcells = 2;
}

