/*========================================================================
 *
 * Name - get.hc
 *
 * ccs version:	1.4
 *
 * ccsid:	@(#)get.hc	1.4 - 11/03/92 14:55:59
 * from: 	ccs/s.get.hc
 * date: 	06/28/93 09:14:48
 *
 * Description:  image grabbing functions for xgrabsc
 *
 *               see cpyright.h for copyright information
 *
 *
 *========================================================================
 */

/*
 * Get the image bounded by the given rectangle.
 * The associated colormap information is also extracted and returned.
 * TRUE is returned if an image was successfully grabbed, and FALSE
 * otherwise.
 */
getImage(xrect, image, window, cmapwindow)
  XRectangle *xrect;
  imageInfo *image;
  Window window, cmapwindow;
{
  XImage *ximage;
  int depth, ncolors, cmapSize, numCmaps;
  int h, w;
  long i;
  XColor colors[MAX_CELLS];
  Colormap *cmaps, cmap;

  if (xrect->width == 0  || xrect->height == 0)
    return FALSE;

  /* get the image */
  depth  = DefaultDepth(hDisplay, hScreen);
  ximage = XGetImage(hDisplay, window,
            xrect->x, xrect->y, xrect->width, xrect->height, AllPlanes,
            depth==1 ? XYPixmap : ZPixmap);
  image->ximage = ximage;

  /* get the colormap info */
  if (cmapwindow != (Window)NULL)
    cmaps = XListInstalledColormaps(hDisplay, cmapwindow, &numCmaps);
  else
    cmaps = XListInstalledColormaps(hDisplay, window, &numCmaps);

  if (numCmaps == 0)
    cmap = DefaultColormap(hDisplay, hScreen);
  else {
    cmap = *cmaps;
    if (numCmaps > 1)
      fprintf(stderr,
        "%s: more than one colormap found - using first encountered",
        programName);
  }
  XFree(cmaps);

  ncolors = DisplayCells(hDisplay, hScreen);
  /* this won't cut the mustard for DirectColor */
  for (i=0; i<ncolors; i++)
    colors[i].pixel = i;

  XQueryColors(hDisplay, cmap, colors, ncolors);
  for (i=0; i<ncolors; i++) {
    image->red[i]   = colors[i].red;
    image->green[i] = colors[i].green;
    image->blue[i]  = colors[i].blue;
  }

  /* figure out which colormap entries are actually used by the image */
  ncolors = cmapSize = 0;
  memset((char *)image->used, 0, MAX_CELLS);
  for (h=0; h<ximage->height; h++)
    for (w=0; w<ximage->width; w++) {
      i = XGetPixel(ximage, w, h);
      if (!image->used[i]) {
        image->used[i] = TRUE;
        if (i+1 > cmapSize)      /* keep track of colormap size */
          cmapSize = i+1;
        ncolors++;
      }
    }
  image->numcells = cmapSize;
  if (verbose)
    fprintf(stderr, "%s: image has %d colors\n", programName, ncolors);

  return TRUE;
}



/*
 * Let the user stretch a rectangle on the screen and return its values.
 * It may be wise to grab the server before calling this routine.  If the
 * screen is allowed to change during XOR drawing video droppings may result.
 */
getRectangle(xrect)
  XRectangle *xrect;
{
  XEvent event;
  unsigned int x, y, rootx, rooty;
  GC gc;
  Cursor pointer1, pointer2;
  int boxDrawn = False;
  int rx, ry, rw, rh;

  /* get some cursors for rectangle formation */
  pointer1 = XCreateFontCursor(hDisplay, XC_ul_angle);
  pointer2 = XCreateFontCursor(hDisplay, XC_lr_angle);

  /* grab the pointer */
  if (GrabSuccess != XGrabPointer(hDisplay, hRoot, False, ButtonPressMask,
        GrabModeAsync, GrabModeAsync, hRoot, pointer1, CurrentTime)) {
    fprintf(stderr,"%s - could not grab pointer!\n", programName);
    exit(3);
  }

  /* create a graphics context to draw with */
  gc = XCreateGC(hDisplay, hRoot, 0, NULL);
  if (!gc) {
    fprintf(stderr,"%s - could not get drawing resources\n", programName);
    exit(3);
  }
  XSetSubwindowMode(hDisplay, gc, IncludeInferiors);
  XSetForeground(hDisplay, gc, 255);
  XSetFunction(hDisplay, gc, GXxor);

  /* get a button-press and pull out the root location */
  XMaskEvent(hDisplay, ButtonPressMask, &event);
  rootx = rx = event.xbutton.x_root;
  rooty = ry = event.xbutton.y_root;

  /* get pointer motion events */
  XChangeActivePointerGrab(hDisplay, ButtonMotionMask | ButtonReleaseMask,
        pointer2, CurrentTime);


  /* MAKE_RECT converts the original root coordinates and the event root
   * coordinates into a rectangle in xrect */
#define MAKE_RECT(etype) \
  x = event.etype.x_root;       \
  y = event.etype.y_root;       \
  rw  = x - rootx;              \
  if (rw  < 0) rw  = -rw;       \
  rh  = y - rooty;              \
  if (rh  < 0) rh  = -rh;       \
  rx = x < rootx ? x : rootx;   \
  ry = y < rooty ? y : rooty

  /* loop to let the user drag a rectangle */
  while (TRUE) {
    XNextEvent(hDisplay, &event);
    switch(event.type) {
      case ButtonRelease:
        if (boxDrawn) {
          XDrawRectangle(hDisplay, hRoot, gc, rx, ry, rw, rh);
          boxDrawn = False;
        }
        XFlush(hDisplay);
        /* record the final location */
        MAKE_RECT(xbutton);
        /* release resources */
        XFreeGC(hDisplay, gc);
        XFreeCursor(hDisplay, pointer1);
        XFreeCursor(hDisplay, pointer2);
        xrect->x      = rx;
        xrect->y      = ry;
        xrect->width  = rw;
        xrect->height = rh;
        XUngrabPointer(hDisplay, CurrentTime);
	XSync(hDisplay, FALSE);
        return True;
      case MotionNotify:
        if (boxDrawn) {
          XDrawRectangle(hDisplay, hRoot, gc, rx, ry, rw, rh);
          boxDrawn = False;
        }
        while (XCheckTypedEvent(hDisplay, MotionNotify, &event))
          {}
        MAKE_RECT(xmotion);
        XDrawRectangle(hDisplay, hRoot, gc, rx, ry, rw, rh);
        boxDrawn = True;
        break;
    }
  }
}






/*
 * find the window under the mouse pointer when one of the bits in keyMask
 * is satisfied.
 */
Window getWindowWhenKeyIsPressed(keyMask)
  unsigned int keyMask;
{
  Bool status;
  Window result;
  Window root, child;
  int root_x, root_y, win_x, win_y;
  unsigned int keys_buttons;

  if (verbose)
    fprintf(stderr, "%s: waiting for keypress\n", programName);

  do {
    status = XQueryPointer(hDisplay, vRoot, &root, &child,
              &root_x, &root_y, &win_x, &win_y, &keys_buttons);
    if (!status)
      return (Window)0;
  } while ((keyMask != 0) && ((keyMask & keys_buttons) == 0));

  if (child == (Window)0) {
    if (verbose) {
      fprintf(stderr, "%s: unable to find a non-root window under pointer.\n",
              programName);
      fprintf(stderr, "%s: using root window.\n", programName);
    }
    result = (Window)vRoot;
  }
  else
    result = child;

  if (verbose)
    fprintf(stderr, "%s: returning window id %d\n", programName, result);

  return result;
}




/*
 * choose a window as in xwd
 */

Window getWindow() {
  int status;
  Cursor cursor;
  XEvent event;
  Window result;

  result = None;

  cursor = XCreateFontCursor(hDisplay, XC_target);

  status = XGrabPointer(hDisplay, vRoot, FALSE,
         ButtonPressMask|ButtonReleaseMask, GrabModeSync,
         GrabModeAsync, hRoot, cursor, CurrentTime);
  if (status != GrabSuccess) {
    fprintf(stderr, "%s: can't grab mouse\n", programName);
    exit(3);
  }

  while (TRUE) {
    XAllowEvents(hDisplay, SyncPointer, CurrentTime);
    XWindowEvent(hDisplay, vRoot, ButtonPressMask|ButtonReleaseMask, &event);

    switch (event.type) {
      case ButtonRelease:
        result = event.xbutton.subwindow;
        if (result == None)
          result = hRoot;
        XUngrabPointer(hDisplay, CurrentTime);      /* Done with pointer */
        if (verbose)
          fprintf(stderr, "%s: found window with id 0x%x\n", programName, result);
        return result;
        break;
    }
  }

}




