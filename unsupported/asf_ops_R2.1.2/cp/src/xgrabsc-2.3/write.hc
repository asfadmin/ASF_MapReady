/*========================================================================
 *
 * Name - write.hc
 *
 * ccs version:	1.16
 *
 * ccsid:	@(#)write.hc	1.16 - 06/28/93 09:13:46
 * from: 	ccs/s.write.hc
 * date: 	06/28/93 09:14:49
 *
 * Description:  output conversions for xgrabsc
 *
 *               see cpyright.h for copyright information
 *
 *
 *========================================================================
 */


/* swap the bits in a byte */
swapbits(b)
  byte b;
{
  byte b2;

  b2 = 0;
  b2 |= (b & 0x01) << 7;
  b2 |= (b & 0x02) << 5;
  b2 |= (b & 0x04) << 3;
  b2 |= (b & 0x08) << 1;
  b2 |= (b & 0x10) >> 1;
  b2 |= (b & 0x20) >> 3;
  b2 |= (b & 0x40) >> 5;
  b2 |= (b & 0x80) >> 7;
  return b2;
}




/* swap the bytes in a long int */
swapbytes(pDblw)
  dw *pDblw;
  {
  union {
    dw  dbl;
    byte bytes[4];
    } cnv;
  byte aByte;

  cnv.dbl = *pDblw;
  aByte = cnv.bytes[0];
  cnv.bytes[0] = cnv.bytes[3];
  cnv.bytes[3] = aByte;
  aByte = cnv.bytes[1];
  cnv.bytes[1] = cnv.bytes[2];
  cnv.bytes[2] = aByte;
  *pDblw = cnv.dbl;
  }



/* swap some long ints.  (n is number of BYTES, not number of longs) */
swapdws (bp, n)
  register char *bp;
  register unsigned n;
{
  register char c;
  register char *ep = bp + n;
  register char *sp;

  while (bp < ep) {
    sp = bp + 3;
    c = *sp;
    *sp = *bp;
    *bp++ = c;
    sp = bp + 1;
    c = *sp;
    *sp = *bp;
    *bp++ = c;
    bp += 2;
  }
}



/* swap some short ints */
swapwords (bp, n)
  register char *bp;
  register unsigned n;
{
  register char c;
  register char *ep = bp + n;

  while (bp < ep) {
    c = *bp;
    *bp = *(bp + 1);
    bp++;
    *bp++ = c;
  }
}





writeSimple(image, outfile)
  imageInfo *image;
  FILE *outfile;
{
  dw width, height, hasColormap, colormapSize;
  dw swaptest = 1;
  int i, w, h;

  if (verbose)
    fprintf(stderr, "%s: writing in simple output format\n", programName);
  if (image->ximage->depth != 8) {
    fprintf(stderr, "%s: can't write simple image format if depth is not 8\n",
            programName);
    return;
  }
  width        = image->ximage->width;
  height       = image->ximage->height;
  hasColormap  = 1;
  colormapSize = image->numcells;
  if (*(char *)&swaptest==0) {
    swapdws(&width,        1);
    swapdws(&height,       1);
    swapdws(&hasColormap,  1);
    swapdws(&colormapSize, 1);
  }
  fwrite(&width, 4, 1, outfile);
  fwrite(&height, 4, 1, outfile);
  fwrite(&hasColormap, 4, 1, outfile);
  fwrite(&colormapSize, 4, 1, outfile);
  for (i=0; i<image->numcells; i++)
    fputc((byte)(image->red[i]>>8), outfile);
  for (i=0; i<image->numcells; i++)
    fputc((byte)(image->green[i]>>8), outfile);
  for (i=0; i<image->numcells; i++)
    fputc((byte)(image->blue[i]>>8), outfile);
  for (i=0; i<image->numcells; i++)
    fputc((byte)(image->used[i]), outfile);
  for (h=0; h<image->ximage->height; h++)
    for (w=0; w<image->ximage->width; w++)
      fputc(XGetPixel(image->ximage, w, h), outfile);
}







/*
 * makePSImage returns an XImage structure that contains the samples
 * to be written.  If the input image is monochrome, its XImage structure
 * will be returned.  Otherwise a new structure is allocated and returned.
 */

XImage* makePSImage(image, desiredDepth, depth, bpl, spb)
    imageInfo* image;
    int desiredDepth;  /* 0 = don't care */
    int* depth;
    int* bpl;
    int* spb;
{
    register byte* ptr;
    int lshift, lmask;
    long p;
    int x, y, i;
    XImage* ximage = image->ximage;
    XImage* psimage;

  /* use depth as the number of bits in output samples */
  *depth = ximage->depth;
  /* postscript only supports 1, 2, 4, or 8 */
  if (*depth > 8) *depth = 8;     /* max postscript bits/sample */
  if (*depth < 8 && *depth > 4) *depth = 8;
  if (*depth == 3) *depth = 4;
    
    if (desiredDepth == 0) {
	desiredDepth = *depth;
    }


  *bpl = ((ximage->width * desiredDepth) + 7) / 8;

  if (*depth == 1)
	/* Same image */
    psimage = ximage;
  else {
    /* colors have to be changed to luminescence */
    ptr = (byte *)malloc(ximage->height * *bpl);
    psimage = XCreateImage(hDisplay, DefaultVisual(hDisplay, hScreen),
                  desiredDepth, ZPixmap,
                  0, ptr,
                  ximage->width, ximage->height,
                  8, *bpl);
    if (!psimage) {
      fprintf(stderr, "%s: could not create image for Postscript conversion\n",
        programName);
      exit(3);
    }
    /* force the bits_per_pixel to be what is needed */
    psimage->bits_per_pixel = desiredDepth;
  }

  *spb = 8 / psimage->bits_per_pixel;    /* samples per byte */

  if (*depth > 1) {
    /* translate colors into grays */
    lshift = 16 - psimage->bits_per_pixel;
    lmask  = (1 << psimage->bits_per_pixel) - 1;
    for (y = 0; y < ximage->height; y++) {
      for (x = 0; x < ximage->width; x++) {
        p = XGetPixel(ximage, x, y);
        i = (0.30*(double)image->red[p]) +
            (0.59*(double)image->green[p])+
            (0.11*(double)image->blue[p]);
        i = (i >> lshift) & lmask;
        XPutPixel(psimage, x, y, i);
      }
    }
  }
  *depth = desiredDepth;  /* The final resolution */
  return psimage;
}







writeOnlyPreview(image, outfile)
  imageInfo *image;
  FILE* outfile;
{
  XImage* psimage = (XImage *) NULL;

  if (verbose) fprintf(stderr, "%s: generating only EPSI preview comment\n", programName);

  if (image->ximage->depth == 1) {
    /* First process the image to make it good */
    int depth, bpl, spb;
    psimage = makePSImage(image, 1, &depth, &bpl, &spb);
  }

  writePreview(image, outfile, psimage);

  if (psimage && psimage != image->ximage) {
    /* Free the allocated PostScript image */
    free(psimage->data);
    free(psimage);
  }
}



writePreview(image, outfile, defaultImage)
  imageInfo *image;
  XImage *defaultImage;
  FILE *outfile;
{
  register byte b, *ptr;
  int depth, bpl, spb;
  int reverse, x, y;
  int widthcount, lines;
  XImage* ximage = image->ximage;
  XImage* psimage = (XImage *)NULL;
  imageInfo newImage;
    
  newImage.ximage = (XImage*) NULL;
      
  if (ximage->depth > 1) {
    /* Copy the image before doing any changes! */
    int i;
    for (i = 0; i < image->numcells; ++i) {
	newImage.red[i] = image->red[i];
        newImage.green[i] = image->green[i];
        newImage.blue[i] = image->blue[i];
        newImage.used[i] = image->used[i];
    }
    newImage.numcells = image->numcells;
    newImage.ximage = XSubImage(ximage, 0, 0, ximage->width, ximage->height);
    if (!newImage.ximage) {
      fprintf(stderr, "%s: unable to create copy of color image\n",
        programName);
      XCloseDisplay(hDisplay);
      exit(3);
    }

    if (ForceBitmap)
      pixmap2bitmap(&newImage);
    else if (Halftone)
      pixmap2halftone(&newImage, DitherKind);
    else
      pixmap2halftone(&newImage, FS_DITHER);

    defaultImage = newImage.ximage;
    image = &newImage;
    psimage = makePSImage(&newImage, 1, &depth, &bpl, &spb);
  }
  else {
    /* the image is already monochrome, so use the psimage that's already
     * been processed */
    psimage = defaultImage;
    bpl = (psimage->width + 7) / 8;
    spb = 8;
  }
    

  /* compute the number of lines in the preview output so
   * apps reading the file can easily skip it */
  lines = (bpl * psimage->height) / (PREVIEW_CODEWIDTH / 2);
  if ((bpl * psimage->height) % (PREVIEW_CODEWIDTH / 2) > 0) lines++;

  fprintf(outfile, "%%%%BeginPreview: %d %d %d %d\n%%",
	    psimage->width, psimage->height, psimage->depth, lines);

    
  /* if the bits haven't been swizzled yet, we have to check for color
   * reversal */
  if (psimage == image->ximage)
    reverse = BlackPixel(hDisplay,hScreen)!=EPSF_BLACK;
  else
    reverse = FALSE;

  widthcount = 0;
  for (y=0; y<psimage->height; y++) {
    /* note that depth 1 images are already padded to even byte boundaries
     * under X, so there is no need to shift bits around to do padding of
     * the preview image */
    for (x=0, ptr=(byte *)(psimage->data+(y * psimage->bytes_per_line));
         x<psimage->width;
         x+=spb, ptr++) {
      b = *ptr;
      if (reverse) b = ~b;
      if (depth == 1  &&  psimage->bitmap_bit_order == LSBFirst)
        b = swapbits(b);
      fprintf(outfile, "%02.2x", b);
      widthcount += 2;
      if (widthcount >= PREVIEW_CODEWIDTH) {
	fputs("\n%", outfile);
        widthcount = 0;
      }
    }
  }

  if (widthcount == 0)
    fputs("%EndPreview\n", outfile);
  else
    fputs("\n%%EndPreview\n", outfile);
    
  if (psimage && psimage != defaultImage) {
    free(psimage->data);
    free(psimage);
  }
  if (newImage.ximage) {
    XDestroyImage(newImage.ximage);
  }
}



/*
 * Write an image in Postscript format
 */
writePostscript(image, outfile, encode, encapsulate, preview,
  landscape, binary, checkLimits)
  imageInfo *image;
  FILE *outfile;
  int encode;       /* TRUE if we're to encode the Postscript output */
  int encapsulate;  /* TRUE if encapsulated Postscript output is wanted */
  int preview;      /* TRUE if EPSI preview image is to be written with EPS output */
  int landscape;    /* TRUE if landscape format is wanted */
  int binary;       /* TRUE if binary output is wanted */
  int checkLimits;  /* TRUE if PS interpreter memory checks should be made */
{
  register byte b, *ptr;
  register int x, y;
  register int i;
  XImage *ximage = image->ximage;
  XImage *psimage;
  double xdpi, ydpi, xscale, yscale, f;
  int lshift, lmask;
  int depth, bpl, spb;
  int reverse;
  long p;
  /* rle variables */
  int rlecount;
  byte rlesample;
  dw  rletotal;
  int widthcount;
  int firstSample;

  if (verbose)
    fprintf(stderr, "%s: formatting Postscript output\n", programName);

  if (preview)
    encapsulate = TRUE;

  if (encapsulate)
    landscape = FALSE;  /* landscape uses a transformation matrix */

  psimage = makePSImage(image, 0, &depth, &bpl, &spb);

#ifndef NO_RLE_CHECKS
  if (encode) {
    rletotal = 0;
    rlecount = 0;
    firstSample = TRUE;
    for (y=0; y<psimage->height; y++)
      for (x=0, ptr=(byte *)(psimage->data + (y * psimage->bytes_per_line));
           x<psimage->width; x+=spb, ptr++) {
        b = *ptr;
	if (firstSample || b != rlesample || rlecount==254) {
	  if (!firstSample)
	    rletotal += 2;
	  else
	    firstSample = FALSE;
	  rlecount = 0;
	  rlesample = b;
	}
	else
	  rlecount++;
      }
    if (!firstSample)
      rletotal += 2;
    f = (float)(rletotal) / (float)(psimage->height*bpl);
    if (verbose)
      fprintf(stderr, "%s: encoding would change to %5.1f%% of orig size\n",
            programName, f * 100.0);
    encode = f <= 0.95;
  }
#endif



  if (verbose)
    fprintf(stderr, "%s: image will %sbe encoded\n", programName,
            encode? "" : "not ");

  if (encapsulate) {
    fprintf(outfile, "%%!PS-Adobe-2.0 EPSF-2.0\n");
    fprintf(outfile, "%%%%BoundingBox: %d %d %d %d\n",
                0, 0, psimage->width, psimage->height);
  }
  else
    fprintf(outfile, "%%!PS-Adobe-2.0\n");

  fprintf(outfile, "%%%%Creator: xgrabsc\n");
  fprintf(outfile, "%%%%Title: %s\n", imageName);
  if (outfileName)
    fprintf(outfile, "%%%%File: %s\n", outfileName);
  time(&p);
  fprintf(outfile, "%%%%CreationDate: %s", ctime(&p));
  fprintf(outfile, "%%%%EndComments\n");
  fprintf(outfile, "%%\n");
  fprintf(outfile, "%%\n");

  /* if the user wants a preview image, EPS 2.0 says it must go here */
  if (preview)
    writePreview(image, outfile, psimage);


  fprintf(outfile, "%%%%EndProlog\n");
  if (encapsulate) {
    fprintf(outfile, "%%%%Page: 1 1\n");
  }

  /* standard inch procedure */
  fputs("/inch {72 mul} def\n", outfile);


  /* define a string to hold image bytes */
  if (encode) {
    fputs("/rlebuffer 2 string def\n", outfile);
    fprintf(outfile, "/samples %d string def\n", 256);  /* max bytes per burst */
  }
  else
    fprintf(outfile, "/picstr %d string def\n", bpl);

  if (binary) {
    fputs("/endstr 1 string def\n", outfile);
    if (encode) fputs("/ccount 0 def\n", outfile);
  }

  /* define the image plotting procedure */
  fputs("/plotimage\n", outfile);

  fprintf(outfile, "{%d %d %d ",
        psimage->width, psimage->height, psimage->bits_per_pixel);



  /* transformation matrix */
  if (landscape)
    fprintf(outfile, "[0 %d %d 0 0 0]\n", psimage->width, psimage->height);
  else
    fprintf(outfile, "[%d 0 0 -%d 0 %d]\n",
        psimage->width, psimage->height, psimage->height);

  /* line reading function  */

  if (encode) {
    fputs("% run-length decoding block\n",                                       outfile);
    if (binary) {
      fputs("  { currentfile rlebuffer readstring pop pop\n", outfile);
      fputs("    /ccount ccount 2 add def                 %% count binary chars\n",outfile);
      fprintf(outfile,
            "    ccount %d ge                             %% test for full line\n", IMAGE_CODEWIDTH);
      fputs("    { /ccount 0 def                          %% reset character counter\n",outfile);
      fputs("      currentfile endstr readline pop pop    %% skip newline\n",outfile);
      fputs("    } if                                     %% skip newlines after full line\n",outfile);
    }
    else
      fputs("  { currentfile rlebuffer readhexstring pop pop\n",                 outfile);
    fputs("    rlebuffer 0 get 1 add       %% number of copies of the sample\n", outfile);
    fputs("    /nsamples exch store        %% save it away\n",                   outfile);
    fputs("    /lum rlebuffer 1 get store  %% the sample itself\n",              outfile);
    fputs("    0 1 nsamples 1 sub { samples exch lum put } for\n",               outfile);
    fputs("    samples 0 nsamples getinterval %% leave the pixels on the stack\n",outfile);
    fputs("  }\n", outfile);
  }
  else {
    if (binary) {
      /* Do a "readline" after each "readstring" so we can seperate each
         scanline of binary data with a "newline" */
      fputs("   {currentfile picstr readstring pop\n", outfile);
      fputs("    currentfile endstr readline pop pop}\n", outfile);
    }
    else
      fputs("  {currentfile picstr readhexstring pop}\n", outfile);
  }

  fputs("  image\n} def\n", outfile);


  /* emit some code to check for resource availability */
  if (!encapsulate  &&  checkLimits) {
    for (x=0; CheckVM[x] != NULL; x++) {
      fputs(CheckVM[x], outfile);
      fputc('\n', outfile);
    }
    fprintf(outfile, "\n\n");
    fprintf(outfile, "%d checkvm\n", psimage->height * bpl);
  }

  /* save context and move to a nice origin */
  fputs("gsave\n", outfile);

  if (encapsulate) {
    /* for encapsulated postscript, we need a scale factor that is equal
     * to the image width/height in samples */
    fprintf(outfile, "%d %d scale\n", psimage->width, psimage->height);
  }
  else {
    /* For physical output we need a scale factor that will create
      * the same size image, and we need to center it on the page.
      *   -Figure out the physical dimensions on the screen
      *    and make it come out the same on the printer. */
    xdpi = (((double)DisplayWidth(hDisplay,hScreen)) * 25.4) /
            ((double)DisplayWidthMM(hDisplay,hScreen));
    ydpi = (((double)DisplayHeight(hDisplay,hScreen)) * 25.4) /
            ((double)DisplayHeightMM(hDisplay,hScreen));
    xscale = ((double)psimage->width) / xdpi;
    yscale = ((double)psimage->height) / ydpi;
    if (landscape) {
      f = xscale; xscale = yscale; yscale = f;
    }
    if (xscale > horizInset) {
      yscale *= horizInset / xscale;
      xscale = horizInset;
    }
    if (yscale > vertInset) {
      xscale *= vertInset / yscale;
      yscale = vertInset;
    }
    fprintf(outfile, "%1.2g inch %1.2g inch translate\n",
                  (pageWidth - xscale) / 2.0, (pageHeight - yscale) / 2.0);
    if (landscape)
      fprintf(outfile, "%1.2g inch %1.2g inch scale\n", yscale, xscale);
    else
      fprintf(outfile, "%1.2g inch %1.2g inch scale\n", xscale, yscale);
  }



  if (binary)
    fprintf(outfile,"%%%%BeginBinary: %d\n",
	    encode ? rletotal+11+rletotal/IMAGE_CODEWIDTH
	    : ximage->height*(ximage->width+1)+10);

  fputs("plotimage\n", outfile);


  reverse = depth == 1? BlackPixel(hDisplay,hScreen)==1 : FALSE;
  if (encode) {
    rletotal = 0;
    rlecount = 0;
    firstSample = TRUE;
  }
  widthcount = 0;
  for (y=0; y<psimage->height; y++) {
    for (x=0, ptr=(byte *)(psimage->data+(y * psimage->bytes_per_line));
         x<psimage->width;
         x+=spb, ptr++) {
      b = *ptr;
      if (reverse) b = ~b;
      if (depth == 1  &&  psimage->bitmap_bit_order == LSBFirst)
        b = swapbits(b);
      if (encode) {
        if (firstSample || b != rlesample || rlecount==254) {
	  if (!firstSample) {
	    if (binary) {
	      putc((byte)rlecount,outfile);
	      putc((byte)rlesample,outfile);
	      widthcount += 2;
	    }
	    else {
	      fprintf(outfile, "%02.2x%02.2x", rlecount, rlesample);
	      widthcount += 4;
	    }
	    rletotal += 2;
	    if (widthcount >= IMAGE_CODEWIDTH) {
	      fputc('\n', outfile);
	      widthcount = 0;
	    }
	  }
	  else
	    firstSample = FALSE;
	  rlecount = 0;
	  rlesample = b;
	}
	else
	  rlecount++;
      }
      else {
	if (binary)
	  putc((byte)b,outfile);
	else {
          fprintf(outfile, "%02.2x", b);
	  widthcount += 2;
	  if (widthcount >= IMAGE_CODEWIDTH) {
	    fputc('\n', outfile);
            widthcount = 0;
          }
        }
      }
    }
    if (binary && !encode) putc('\n',outfile);
  }

  if (encode) {
    if (!firstSample) {
      if (binary) {
	putc((byte)rlecount,outfile);
	putc((byte)rlesample,outfile);
      }
      else
        fprintf(outfile, "%02.2x%02.2x\n", rlecount, rlesample);
      rletotal += 2;
    }
    putc('\n',outfile);
    if (binary) fputs("%%EndBinary\n",outfile);
    fputs("%\n", outfile);
    fprintf(outfile, "%% Run-length encoding savings = %5.1f%%\n",
          100.0 - ((float)(rletotal) * 100.0 / (float)(psimage->height * bpl)));
    fputs("%\n", outfile);
  }
  else if (binary) fputs("%%EndBinary\n",outfile);

  fputs("\n\n\ngrestore\nshowpage\n", outfile);
  fputs("\n%%Trailer\n", outfile);


  if (psimage != ximage) {
    free(psimage->data);
    free(psimage);
  }
}





/*
 * Write an image in Color Postscript format
 */
writeColorPS(image, outfile, encode, encapsulate, preview,
  landscape, binary, checkLimits)
  imageInfo *image;
  FILE *outfile;
  int encode;       /* TRUE if we're to encode the Postscript output    */
  int encapsulate;  /* TRUE if encapsulated Postscript output is wanted */
  int preview;      /* TRUE if EPSI preview image is to be written with EPS output */
  int landscape;    /* TRUE if landscape output is wanted               */
  int binary;       /* TRUE if binary Postscript output is wanted */
  int checkLimits;  /* TRUE if PS interpreter memory checks should be made */
{
  register byte *ptr, b;
  register int x, y;
  XImage *ximage = image->ximage;
  double xdpi, ydpi, xscale, yscale, f;
  double left, top;
  int depth, bpl, spb;
  long p;
  /* rle variables */
  int rlecount;
  dw  rletotal;
  byte rlesample;
  int firstSample;
  int widthcount;


  if (verbose)
    fprintf(stderr, "%s: formatting Color Postscript output\n", programName);

  if (preview)
    encapsulate = TRUE;  /* should have been enforced before this point */

  if (encapsulate)
    landscape = FALSE;  /* landscape uses a transformation matrix */

  depth = 8;                                  /* bits per sample  */
  spb   = 1;                                  /* samples per byte */
  bpl = ((ximage->width * depth) + 7) / 8;    /* bytes per line   */


#ifndef NO_RLE_CHECKS
  if (encode) {
    rletotal = 0;
    rlecount = 0;
    firstSample = TRUE;
    for (y=0; y<ximage->height; y++)
      for (x=0, ptr=(byte *)(ximage->data + (y * ximage->bytes_per_line));
           x<ximage->width; x+=spb, ptr++) {
        b = *ptr;
	if (firstSample || b != rlesample || rlecount==254) {
	  if (!firstSample)
	    rletotal += 2;
	  else
	    firstSample = FALSE;
	  rlecount = 0;
	  rlesample = b;
	}
	else
	  rlecount++;
      }
    rletotal += 2;
    f = (float)(rletotal) / (float)(ximage->height*bpl);
    if (verbose)
      fprintf(stderr, "%s: encoding would change to %5.1f%% of orig size\n",
            programName, f * 100.0);
    encode = f <= 0.95;
  }
#endif

  if (encapsulate) {
    fprintf(outfile, "%%!PS-Adobe-2.0 EPSF-2.0\n");
  }
  else
    fprintf(outfile, "%%!PS-Adobe-2.0\n");

  fprintf(outfile, "%%%%Creator: xgrabsc\n");
  fprintf(outfile, "%%%%Title: %s\n", imageName);
  if (outfileName)
    fprintf(outfile, "%%%%File: %s\n", outfileName);
  if (encapsulate) {
    fprintf(outfile, "%%%%Pages: 1\n");
    fprintf(outfile, "%%%%BoundingBox: %d %d %d %d\n",
                0, 0, ximage->width, ximage->height);
  }
  time(&p);
  fprintf(outfile, "%%%%CreationDate: %s", ctime(&p));
  fprintf(outfile, "%%%%EndComments\n");

  /* if the user wants a preview image, EPS 2.0 says it must go here */
  if (preview)
    writePreview(image, outfile, image->ximage);

  fprintf(outfile, "%%%%EndProlog\n");
  if (encapsulate) {
    fprintf(outfile, "%%%%Page: 1 1\n");
  }
  fprintf(outfile, "\n\ngsave\n\n");


  fputs("/inch {72 mul} def\n", outfile);

  /* emit some code to check for resource availability */
  if (!encapsulate  &&  checkLimits) {
    for (x=0; CheckVM[x] != NULL; x++) {
      fputs(CheckVM[x], outfile);
      fputc('\n', outfile);
    }
    fprintf(outfile, "\n\n");
    fprintf(outfile, "%d checkvm\n\n", ximage->height * bpl);
  }

  if (encapsulate) {
    /* don't translate the image for encapsulated postscript.  The
     * scale should match the data dimensions of the image in samples.  */
    fprintf(outfile, "%d %d scale\n", ximage->width, ximage->height);
  }
  else {
    /* For physical output we need a scale factor that will create
     * the same size image, and we need to center it on the page.
     *   -Figure out the physical dimensions on the screen
     *    and make it come out the same on the printer.
     */
    xdpi = (((double)DisplayWidth(hDisplay,hScreen)) * 25.4) /
            ((double)DisplayWidthMM(hDisplay,hScreen));
    ydpi = (((double)DisplayHeight(hDisplay,hScreen)) * 25.4) /
            ((double)DisplayHeightMM(hDisplay,hScreen));
    xscale = ((double)ximage->width) / xdpi;
    yscale = ((double)ximage->height) / ydpi;
    if (landscape) {
      f = xscale; xscale = yscale; yscale = f;
    }
    if (xscale > horizInset) {
      yscale *= horizInset / xscale;
      xscale = horizInset;
    }
    if (yscale > vertInset) {
      xscale *= vertInset / yscale;
      yscale = vertInset;
    }
     

    left = ((pageWidth - xscale) / 2.0);
    top  = ((pageHeight - yscale) / 2.0);
    fprintf(outfile, "%1.2g inch %1.2g inch translate\n", left, top);
    if (landscape)
      fprintf(outfile, "%1.2g inch %1.2g inch scale\n", yscale, xscale);
    else
      fprintf(outfile, "%1.2g inch %1.2g inch scale\n", xscale, yscale);
    fprintf(outfile, "\n\n\n");
  }

  if (binary) {
    fputs("/endstr 1 string def\n", outfile);
    fputs("/ccount 0 def\n", outfile);
  }

  if (encode) {
    /* encoded output:
     * define a drawcolorimage procedure geared to this image
     */
    fprintf(outfile, "/rgbstr %d string def\n", 256 * 3); /* max pixels per burst */
    fprintf(outfile, "/buffer %d string def\n", 2);
    fputs("/rgb (000) def\n", outfile);
    fprintf(outfile, "/rgbmap %d string def\n", image->numcells * 3);
    fputs("/samples 256 string def\n", outfile);
    fputs("\n\n", outfile);

    fputs("/drawcolormappedimage {\n", outfile);
    fputs("  %% for greyscale printers, convert rgb values into grey samples\n", outfile);
    fputs("  %% and use these in the 'image' operator\n", outfile);
    fputs("  systemdict /colorimage known userdict /colorimage known or not {\n", outfile);
    fputs("    %% convert colors to greyscale\n", outfile);
    fprintf(outfile,"    /ncolors %d store\n", image->numcells);
    fputs("    /ridx 0 store\n", outfile);
    fputs("    /greys ncolors string def\n", outfile);
    fputs("    0 1 ncolors 1 sub {\n", outfile);
    fputs("      /gidx exch store\n", outfile);
    fputs("      rgbmap ridx get .3 mul\n", outfile);
    fputs("      rgbmap ridx 1 add get .59 mul add\n", outfile);
    fputs("      rgbmap ridx 2 add get .11 mul add\n", outfile);
    fputs("      cvi\n", outfile);
    fputs("      /agrey exch store\n", outfile);
    fputs("      greys gidx agrey put\n", outfile);
    fputs("      /ridx ridx 3 add store\n", outfile);
    fputs("    } for\n", outfile);
    fprintf(outfile, "    %d %d %d", ximage->width, ximage->height,depth);
    if (landscape)
      fprintf(outfile, "  [0 %d %d 0 0 0]\n", ximage->width, ximage->height);
    else
      fprintf(outfile, "  [%d 0 0 -%d 0 %d]\n", ximage->width, ximage->height,
                                                ximage->height);
    if (binary) {
      fputs("    { currentfile buffer readstring pop pop    %% run length and index\n", outfile);
      fputs("      /ccount ccount 2 add def                 %% count binary chars\n",outfile);
      fprintf(outfile,
            "      ccount %d ge                             %% test for full line\n", IMAGE_CODEWIDTH);
      fputs("      { /ccount 0 def                          %% reset character counter\n",outfile);
      fputs("        currentfile endstr readline pop pop    %% skip newline\n",outfile);
      fputs("      } if                                     %% skip newlines after full line\n",outfile);
    }
    else
      fputs("    { currentfile buffer readhexstring pop pop\n", outfile);
    fputs("      buffer 0 get 1 add\n", outfile);
    fputs("      /nsamples exch store\n", outfile);
    fputs("      /lum greys buffer 1 get get store\n", outfile);
    fputs("      0 1 nsamples 1 sub { samples exch lum put } for\n", outfile);
    fputs("      samples 0 nsamples getinterval\n", outfile);
    fputs("    } image\n\n", outfile);
    fputs("  } {  %% ifelse\n", outfile);
    fputs("\n", outfile);
    fprintf(outfile, "    %d %d %d", ximage->width, ximage->height,depth);
    if (landscape)
      fprintf(outfile, "  [0 %d %d 0 0 0]\n", ximage->width, ximage->height);
    else
      fprintf(outfile, "  [%d 0 0 -%d 0 %d]\n", ximage->width, ximage->height,
                                                ximage->height);
    fputs("    %% define a block of code to read and decode the rle input stream\n", outfile);
    if (binary) {
      fputs("    { currentfile buffer readstring pop pop    %% run length and index\n", outfile);
      fputs("      /ccount ccount 2 add def                 %% count binary chars\n",outfile);
      fprintf(outfile,
            "      ccount %d ge                             %% test for full line\n", IMAGE_CODEWIDTH);
      fputs("      { /ccount 0 def                          %% reset character counter\n",outfile);
      fputs("        currentfile endstr readline pop pop    %% skip newline\n",outfile);
      fputs("      } if                                     %% skip newlines after full line\n",outfile);
    }
    else
      fputs("    { currentfile buffer readhexstring pop pop %% run length and index\n", outfile);
    fputs("      /npixels buffer 0 get 1 add 3 mul store  %% number of bytes\n", outfile);
    fputs("      /color buffer 1 get 3 mul store          %% fix index into rgb map\n", outfile);
    fputs("      /rgb rgbmap color 3 getinterval store    %% and get the colors\n", outfile);
    fputs("\n", outfile);
    fputs("      0 3 npixels 1 sub {                      %% loop to store the rgb bytes\n", outfile);
    fputs("        rgbstr exch rgb putinterval\n", outfile);
    fputs("      } for\n", outfile);
    fputs("      rgbstr 0 npixels getinterval\n", outfile);
    fputs("    }\n", outfile);
    fputs("    false 3 colorimage\n\n", outfile);
    fputs("  } ifelse\n\n", outfile);
    fputs("} bind def\n", outfile);
  }


  else {
    /* non-encoded output:
     * define a drawcolorimage procedure geared to this image
     */
    fprintf(outfile, "/buffer %d string def\n", 1);
    fprintf(outfile, "/line   %d string def\n", ximage->width);
    fprintf(outfile, "/rgbmap %d string def\n", image->numcells * 3);
    fputs("\n\n", outfile);

    fputs("/onepixel 3 string store\n", outfile);

    fputs("/drawcolormappedimage {\n", outfile);

    fputs("  systemdict /colorimage known userdict /colorimage known or not {\n", outfile);
    fputs("    %% convert colors to greyscale\n", outfile);
    fprintf(outfile,"    /ncolors %d store\n", image->numcells);
    fputs("    /ridx 0 store\n", outfile);
    fputs("    /greys ncolors string def\n", outfile);
    fputs("    /linecount 0 store\n", outfile);
    fputs("    0 1 ncolors 1 sub {\n", outfile);
    fputs("      /gidx exch store\n", outfile);
    fputs("      rgbmap ridx get .3 mul\n", outfile);
    fputs("      rgbmap ridx 1 add get .59 mul add\n", outfile);
    fputs("      rgbmap ridx 2 add get .11 mul add\n", outfile);
    fputs("      cvi\n", outfile);
    fputs("      /agrey exch store\n", outfile);
    fputs("      greys gidx agrey put\n", outfile);
    fputs("      /ridx ridx 3 add store\n", outfile);
    fputs("    } for\n", outfile);


    fprintf(outfile, "    %d %d %d", ximage->width, ximage->height,depth);
    if (landscape)
      fprintf(outfile, "  [0 %d %d 0 0 0]\n", ximage->width, ximage->height);
    else
      fprintf(outfile, "  [%d 0 0 -%d 0 %d]\n", ximage->width, ximage->height,
                                                ximage->height);

    fputs("    %% define a block of code to read and decode the input stream\n", outfile);
    if (binary) {
      fputs("    { currentfile read not { 0 } if\n", outfile);
      fputs("      greys exch 1 getinterval ", outfile);
    }
    else {
      fputs("    { currentfile line readhexstring pop pop\n", outfile);
      fprintf(outfile, "      0 1 %d { dup\n", ximage->width-1);
      fputs("        line exch get greys exch get\n", outfile);
      fputs("        line 3 1 roll put\n", outfile);
      fputs("      } for\n", outfile);
      fputs("      line\n", outfile);
    }
    fputs("    } image\n\n", outfile);

    fputs("  } {  %% ifelse\n", outfile);
    fputs("\n", outfile);

    fprintf(outfile, "    %d %d %d", ximage->width, ximage->height,depth);
    if (landscape)
      fprintf(outfile, "  [0 %d %d 0 0 0]\n", ximage->width, ximage->height);
    else
      fprintf(outfile, "  [%d 0 0 -%d 0 %d]\n", ximage->width, ximage->height,
                                                ximage->height);

    fputs("    %% define a block of code to read and decode the input stream\n", outfile);
    if (binary) {
      fputs("    { rgbmap   currentfile read not { 0 } if   3 mul 3 getinterval \n", outfile);
    }
    else {
      fputs("    { currentfile buffer readhexstring pop pop\n", outfile);
      fputs("      rgbmap buffer 0 get 3 mul 3 getinterval   %% color bytes\n", outfile);
    }
    fputs("    }\n", outfile);
    fputs("    false 3 colorimage\n", outfile);
    fputs("  } ifelse\n", outfile);
    fputs("} bind def\n", outfile);
    fprintf(outfile, "\n\n\n");

  }


  /* write the rgb map */
  fputs("%% get the rgb map\n", outfile);
  fputs("currentfile rgbmap readhexstring\n", outfile);
  for (x=0; x<image->numcells; x++)
    fprintf(outfile, "%02.2x%02.2x%02.2x\n",
                      (byte)((image->red[x]   >> 8) & 0xff),
		      (byte)((image->green[x] >> 8) & 0xff),
		      (byte)((image->blue[x]  >> 8) & 0xff) );
  fputs("pop pop\n\n", outfile);


  if (binary)
    fprintf(outfile,"%%%%BeginBinary: %d\n",
	    encode ? rletotal+22+rletotal/IMAGE_CODEWIDTH
	    : ximage->height*ximage->width+22+
	      (ximage->height*ximage->width)/IMAGE_CODEWIDTH);

  fputs("drawcolormappedimage\n", outfile);

  /* write the map indexes */
  rletotal = 0;
  rlecount = 0;
  firstSample = TRUE;
  widthcount = 0;
  for (y=0; y<ximage->height; y++) {
    for (x=0, ptr=(byte *)(ximage->data+(y * ximage->bytes_per_line));
        x<ximage->width;
        x+=spb, ptr++) {
      b = *ptr;
      if (encode) {
        if (firstSample || b != rlesample || rlecount==254) {
	  if (!firstSample) {
	    if (binary) {
	      putc((byte)rlecount,outfile);
	      putc((byte)rlesample,outfile);
	      widthcount += 2;
	    }
	    else {
	      fprintf(outfile, "%02.2x%02.2x", rlecount, rlesample);
	      widthcount += 4;
	    }
	    rletotal += 2;
	    if (widthcount >= IMAGE_CODEWIDTH) {
	      fputc('\n', outfile);
	      widthcount = 0;
	    }
	  }
	  else
	    firstSample = FALSE;
	  rlecount = 0;
	  rlesample = b;
        }
        else
	  rlecount++;
      }
      else {
	if (binary) {
	  /* no width wrapping for non-encoded binary */
	  fputc((byte)b,outfile);
	}
	else {
	  fprintf(outfile, "%02.2x", b & 0xFF);
	  widthcount += 2;
	  if (widthcount >= IMAGE_CODEWIDTH) {
	    fputc('\n', outfile);
	    widthcount = 0;
	  }
	}
      }
    }
  }

  if (encode) {
    if (!firstSample) {
      if (binary) {
	fputc((byte)rlecount,outfile);
	fputc((byte)rlesample,outfile);
      }
      else
        fprintf(outfile, "%02.2x%02.2x\n", rlecount, rlesample);
      rletotal += 2;
    }
  }
  else
    fputc('\n', outfile);
  if (binary)
    fprintf(outfile,"\n%%%%EndBinary\n");

  /* print some statistics */
  if (encode) {
    fputs("\n%\n", outfile);
    fprintf(outfile, "%% Run-length encoding savings = %5.1f%%\n",
          100.0 - ((float)(rletotal) * 100.0 / (float)(ximage->height * bpl)));
  }

  fputs("\n%\n", outfile);
  fputs("\ngrestore\nshowpage\n%%Trailer\n", outfile);
}











/*
 * Write an image in 'puzzle' format, suitable for loading with
 * "puzzle -picture".
 */
writePuzzle(image, outfile)
  imageInfo *image;
  FILE *outfile;
{
  XImage *ximage = image->ximage;
  int nc, width, height, w, h, cidx;
  dw swaptest = 1;

  if (verbose)
    fprintf(stderr, "%s: formatting Puzzle output\n", programName);

  if (ximage->depth > 8) {
    fprintf(stderr, "%s: Puzzle converter can't handle depth > 8 yet\n",
            programName);
    return;
  }

  nc     = image->numcells;
  width  = ximage->width;
  height = ximage->height;
  if (*(char *)&swaptest) {
    swapbytes(&width);
    swapbytes(&height);
  }
  fwrite(&width, 4, 1, outfile);
  fwrite(&height, 4, 1, outfile);
  fputc(nc, outfile);
  for (cidx=0; cidx<nc; cidx++) {
    fputc(image->red[cidx]>>8,   outfile);
    fputc(image->green[cidx]>>8, outfile);
    fputc(image->blue[cidx]>>8,  outfile);
  }
  for (h=0; h<ximage->height; h++)
    if (ximage->bits_per_pixel == 8)
      fwrite(ximage->data+(h*ximage->bytes_per_line),ximage->width,1,outfile);
    else
      /* this won't work if depth > 8 */
      for (w=0; w<ximage->width; w++)
        fputc(XGetPixel(ximage, w, h), outfile);
}







writeXWD(image, outfile, xyformat)
  imageInfo *image;
  FILE *outfile;
  int xyformat;
{
  XImage   *ximage = image->ximage;
  XWDFileHeader header;
  Visual   *visual = DefaultVisual(hDisplay, hScreen);
  XColor    color;
  dw        visMask = (visual->red_mask
                      | visual->green_mask
                      | visual->blue_mask);
  dw        swaptest = 1;
  int       i, x, y, size;
  byte      *ptr;
  int	    XYtoZ;
  byte	    b;
  int	    bc;

  if (verbose)
    fprintf(stderr, "%s: formatting xwd output\n", programName);

  header.header_size    = (CARD32)(sizeof(header)+strlen(imageName)+1);
  header.file_version   = (CARD32) XWD_FILE_VERSION;
  XYtoZ = (ximage->depth==1 && !xyformat);
  header.pixmap_format  = (CARD32)(ximage->depth>1? ZPixmap : (xyformat? XYPixmap : ZPixmap));
  header.pixmap_depth   = (CARD32) (XYtoZ? 8 : ximage->depth);
  header.pixmap_width   = (CARD32) ximage->width;
  header.pixmap_height  = (CARD32) ximage->height;
  header.xoffset        = (CARD32) ximage->xoffset;
  header.byte_order     = (CARD32) ximage->byte_order;
  header.bitmap_unit    = (CARD32) (XYtoZ ? 8 : ximage->bitmap_unit);
  header.bitmap_bit_order = (CARD32) (XYtoZ ? MSBFirst : ximage->bitmap_bit_order);
  size = XYtoZ ? ximage->bitmap_pad * 8 : ximage->bitmap_pad;
  header.bitmap_pad     = (CARD32) size;
  header.bits_per_pixel = (CARD32) XYtoZ? 8 : ximage->bits_per_pixel;
  size = XYtoZ? ximage->width : ximage->bytes_per_line;
  header.bytes_per_line = (CARD32)size;
  header.visual_class   = (CARD32)visual->class;
  header.red_mask       = (CARD32)visual->red_mask;
  header.green_mask     = (CARD32)visual->green_mask;
  header.blue_mask      = (CARD32)visual->blue_mask;
  header.bits_per_rgb   = (CARD32)visual->bits_per_rgb;
  header.colormap_entries = (CARD32)visual->map_entries;
  /* ncolors should be image->numcells, but some programs seem to expect
   * that it is == colormap_entries.  sigh */
  header.ncolors        = header.colormap_entries;
  header.window_width   = (CARD32)ximage->width;
  header.window_height  = (CARD32)ximage->height;
  header.window_x       = 0;
  header.window_y       = 0;
  header.window_bdrwidth = 0;

  if (*(char *) &swaptest)
    swapdws(&header, sizeof(header));

  fwrite(&header, sizeof(header), 1, outfile);
  fwrite(imageName, 1, strlen(imageName)+1, outfile);

  if (*(char *) &swaptest)
    swapdws(&header, sizeof(header));

  for (i=0; i<image->numcells; i++) {
    color.pixel = i;
    color.red   = image->red[i];
    color.green = image->green[i];
    color.blue  = image->blue[i];
    color.flags = visMask;
    color.pad   = 0;
    if (*(char *) &swaptest) {
      swapdws(&color.pixel, sizeof(color.pixel));
      swapwords(&color.red, 3 * sizeof(color.red)); /* assume g and b follow r */
    }
    fwrite(&color, sizeof(XColor), 1, outfile);
  }

  /* pad out with black entries */
  color.red = color.green = color.blue = 0;
  color.flags = visMask;
  color.pad = 0;
  for (i=image->numcells; i<visual->map_entries; i++) {
    color.pixel = i;
    if (*(char *) &swaptest)
      swapdws(&color.pixel, sizeof(color.pixel));
    fwrite(&color, sizeof(XColor), 1, outfile);
  }

  if (XYtoZ) {
    for (y = 0; y < ximage->height; y++) {
      for (x = 0; x < ximage->width; x++) {
        b = XGetPixel(ximage, x, y);
	fputc(b, outfile);
      }
    }
  }
  else {
    if (ximage->depth == 1  &&  BlackPixel(hDisplay,hScreen) == 0) {
      size = ximage->height * ximage->bytes_per_line;
      for (i=0, ptr=&ximage->data[0]; i<size; i++, ptr++)
        fputc(~(*ptr), outfile);
    }
    else
      fwrite(ximage->data, ximage->height * ximage->bytes_per_line, 1, outfile);
  }
}





/*
 * Write a monochrome image out in Bitmap format.  XWriteBitmapToFile
 * requires a Pixmap as input & we'd have to invent one before we could
 * use it.
 */

writeXYPixmap(image, outfile)
  imageInfo *image;
  FILE *outfile;
{
  XImage *ximage = image->ximage;
  int w, h;
  byte b, *line;
  int lcount;
  int reverse = BlackPixel(hDisplay, hScreen) == 0;
  int swap    = ximage->bitmap_bit_order != LSBFirst;

  if (verbose)
    fprintf(stderr, "%s: formatting Bitmap output\n", programName);

  if (ximage->depth != 1) {
    fprintf(stderr, "%s: can't write polychrome images in XY bitmap format\n",
      programName);
    return;
  }

  fprintf(outfile, "#define %s_width %d\n",  imageName, ximage->width);
  fprintf(outfile, "#define %s_height %d\n", imageName, ximage->height);
  fprintf(outfile, "#define %s_x_hot 0\n",   imageName);
  fprintf(outfile, "#define %s_y_hot 0\n",   imageName);
  fprintf(outfile, "static char %s_bits[] = {\n", imageName);
  lcount = 0;
  fputs("  ", outfile);
  for (h=0; h<ximage->height; h++) {
    line = (byte *)(ximage->data + (h * ximage->bytes_per_line));
    for (w=0; w<ximage->width; w+=8) {
      b = line[w/8];
      if (reverse) b = ~b;
      if (swap)    b = swapbits(b);
      fprintf(outfile, " 0x%02x", b);
      if (h<ximage->height || w+8<ximage->width)
        fputc(',', outfile);
      lcount++;
      if (lcount >= 12) {
        fputs("\n  ", outfile);
        lcount = 0;
      }
    }
  }
  fputs("  };\n", outfile);
}








/*
 * Write a color image out in Pixmap format.
 * Supported output formats are xpm1 (original xpm), xpm2  and xpm3
 */
writeZPixmap(xpmFormat, image, outfile)
  imageInfo *image;
  FILE *outfile;
{
  XImage *ximage = image->ximage;
  int nc, width, height, w, h, cidx, cpp;
  char mne[MAX_CELLS][3];

  if (verbose) {
    switch (xpmFormat) {
    case 3:
      fprintf(stderr, "%s: formatting XPM3 Pixmap output\n", programName);
      break;
    case 2:
      fprintf(stderr, "%s: formatting XPM2 Pixmap output\n", programName);
      break;
    default:
    case 1:
      fprintf(stderr, "%s: formatting XPM output\n", programName);
      break;
    }
  }

  nc  = image->numcells;
  cpp = image->numcells <= 26? 1 : 2;
  switch (xpmFormat) {
  case 3:
    fprintf(outfile, "/* XPM */\nstatic char * %s_name [] = {\n\"%d %d %d %d\",\n",
      imageName, ximage->width, ximage->height, image->numcells, cpp);
    fputs("/* pixels*/\n", outfile);
    break;
  case 2:
    fprintf(outfile, "! XPM2  \n%d %d %d %d\n", ximage->width,
      ximage->height, image->numcells, cpp);
    fputs("! pixels\n", outfile);
    break;
  case 1:
  default:
    fprintf(outfile, "#define %s_format 1\n",   imageName);
    fprintf(outfile, "#define %s_width %d\n",   imageName, ximage->width);
    fprintf(outfile, "#define %s_height %d\n",  imageName, ximage->height);
    fprintf(outfile, "#define %s_ncolors %d\n", imageName, image->numcells);
    fprintf(outfile, "#define %s_chars_per_pixel %d\n",     imageName, cpp);
    fprintf(outfile, "static char * %s_colors[] = {\n", imageName);
    break;
  }

  for (cidx=0; cidx<image->numcells; cidx++) {
    if (cpp > 1) {
      mne[cidx][0] = (char)(cidx / 10) + 'a';
      mne[cidx][1] = (char)(cidx % 10) + '0';
      mne[cidx][2] = '\0';
    }
    else {
      mne[cidx][0] = (char)cidx + (cidx? 'A' : ' ');
      mne[cidx][1] = '\0';
    }
    switch (xpmFormat) {
    case 3:
      fprintf(outfile, "\"%s\tc #%4.4x%4.4x%4.4x\",\n",mne[cidx],
                image->red[cidx], image->green[cidx], image->blue[cidx]);
      break;
    case 2:
      fprintf(outfile, "%s c #%4.4x%4.4x%4.4x\n", mne[cidx],
                image->red[cidx], image->green[cidx], image->blue[cidx]);
      break;
    default:
    case 1:
      fprintf(outfile, "\"%s\", \"#%4.4x%4.4x%4.4x\"\n", mne[cidx],
                image->red[cidx], image->green[cidx], image->blue[cidx]);
      break;
    }
  }
  if (xpmFormat == 1) {
    fputs("} ;\n", outfile);
    fprintf(outfile, "static char * %s_pixels[] = {\n", imageName);
  }
  for (h=0; h<ximage->height; h++) {
    if (xpmFormat != 2)
      fputs("\"", outfile);
    for (w=0; w<ximage->width; w++)
      fputs(mne[XGetPixel(ximage, w, h)], outfile);
    if (xpmFormat == 2)
      fputs("\n", outfile);
    else
      fputs("\",\n", outfile);
  }
  if (xpmFormat == 3)
    fputs("};\n", outfile);
  else if (xpmFormat != 2)
    fputs("} ;\n", outfile);
}




