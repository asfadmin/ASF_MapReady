/*Escher:
Perform phase integration, avoiding the (already laid-out) cuts.*/
#include "escher.h"
#include "asf_endian.h"

void finishUwp(void)
{
  int i, j;
  printf("\n   Zeroing out un-unwrapped phase\n");
  for (j = 0; j < len; j++) {
    register float *lineStart=&phase[wid*j];
    for (i = 0; i < wid; i++) 
      if (!(mask[j*wid+i]&INTEGRATED))
       *(lineStart+i) = 0.0;/*Set non-integrated phases to zero*/
  }
  printf("\n   Zero'd out un-unwrapped phase\n");
  return;
}

void saveUwp(char *f)
{
  meta_parameters *meta;

  FILE *fd = FOPEN(f, "wb");
  meta = meta_read(f);
  put_float_lines(fd, meta, 0, len, phase);
  FCLOSE(fd);

  printf("   saved unwrapped phase...\n");
  return;
}


void integratePhase(int i, int j)
{
  int    u, v;      /* starting point coordinates                         */
  Uchar  s;         /* temp status value                                  */
  int    t = 0;     /* total number of pixels integrated for this region  */
  int    madeJump;  /* indicates an integration jump has been made        */

  /* diagnostic */ 
  printf ("\n   starting integratePhase() at (%d, %d)\n\n", i, j);

  /* initialize things */
  /* set 'source' bits for seed point in 'im' to 0; no source! */
  im[j*wid+i]   &= 0x0f; 
  
  /* set unwrapped phase to seed value p0                      */
/*  *(uwp+j*wid+i)   = p0; <--IMPLICIT*/
  
  /* label seed point as integrated                            */
  mask[j*wid+i] |= INTEGRATED; 
  
  /* increment integration counter                             */
  t++;

  /* start out (u, v) = (i, j), a fall-through the while loop condition */
  u = i; v = j;

  /* 
   * this section tries to initialize (u, v) to a 4-nbr starting point.
   */

  /* try 1:  go up one pixel to (i, j-1) */
  if (!(mask[(j-1)*wid+i] & IICG)){
    v--; t++; 
    im[ j*wid + i] |= TRIED_U;
    im[ v*wid + u] |= SOURCE_B;
    mask[ v*wid + u] |= INTEGRATED;
    phase[v*wid + u]  = phase[j*wid+i] + 
			   phaseRemap((phase[v*wid+u]) - 
			   (phase[j*wid+i]));
    printf("   from seed point, started out by going up...");
  }

  /* try 2:  go right one pixel to (i + 1, j) */
  else if (!(mask[j*wid+(i+1)] & IICG)){ 
    u++; t++;
    im[ j*wid + i] |= TRIED_UR;
    im[ v*wid + u] |= SOURCE_L;
    mask[ v*wid + u] |= INTEGRATED;
    phase[ v*wid + u]  = phase[j*wid+i] + 
			   phaseRemap((phase[v*wid+u]) - 
			   (phase[j*wid+i]));
    printf("   from seed point, started out by going right...\n");
  }

  /* try 3:  go down one pixel to (i, j + 1) */
  else if (!(mask[(j+1)*wid+i] & IICG)){
    v++; t++;
    im[ j*wid + i] |= TRIED_URD;
    im[ v*wid + u] |= SOURCE_A;
    mask[ v*wid + u] |= INTEGRATED;
    phase[ v*wid + u]  = phase[j*wid+i] + 
			   phaseRemap((phase[v*wid+u]) - 
			   (phase[j*wid+i]));
    printf("   from seed point, started out by going down...\n");
  }

  /* try 4:  go left one pixel to (i - 1, j) */
  else if (!(mask[j*wid+(i-1)] & IICG)){
    u--; t++;
    im[ j*wid + i] |= TRIED_URDL;
    im[ v*wid + u] |= SOURCE_R;
    mask[ v*wid + u] |= INTEGRATED;
    phase[ v*wid + u]  = phase[j*wid+i] + 
			   phaseRemap((phase[v*wid+u]) -
			   (phase[j*wid+i]));
    printf("\n   from seed point, started out by going left...\n");
  }

  /* fall through:  No good 4-nbrs found */
  else { 
    im[ j*wid + i] |= TRIED_URDL; 
  }

  /*
   * this section is a big 'while not done'
   *   Keep on going inside the while() as long as:
   *     u is not back to i, and
   *     v is not back to j, and
   *     the TRIED_L bit is not set *at the seed point (i, j)*.
   *  This way we only fall out when we get back to (i, j) and all search
   *    directions are exhausted.
   */
  while (u != i || v != j || !(im[j*wid+i] & TRIED_L)){

    if (!(t%100000)) printf ("\r   total integrated = %d", t);

    /* s = temp value of 'im' at pixel (u, v) */
    s        = im[v*wid + u];
    madeJump = FALSE;

    /*
     * The following series of ifs assume we want to test successively
     *   for jump directions until a good one is found.  When this happens,
     *   the jump is made and the remaining ifs are skipped by virtue of
     *   a boolean 'madeJump' set to TRUE.
     */

    /*  
     * Start at (u, v); no jumps tried from here yet?
     */
    if (!(s & TRIED_URDL)) {  

      /* if this is the case, then set TRIED_U since we're about to try 'up' */
      im[ v*wid + u] |= TRIED_U;  

      if (!(mask[(v-1)*wid+u] & IICG)) {
        v--; t++;
        madeJump = TRUE;
        im[ v*wid + u] |= SOURCE_B;
        mask[ v*wid + u] |= INTEGRATED; 
        phase[ v*wid + u]  = phase[(v+1)*wid + u] + 
                               phaseRemap(phase[v*wid+u] -
			       phase[(v+1)*wid+u]);
      }

    }

    /* Have we tried 'up' (and no others)? */
    if (!madeJump && ((s & TRIED_URDL) == TRIED_U)) { 

      /* if this is the case, 
	 then set TRIED_R since we're about to try 'right' */
      im[ v*wid + u] |= TRIED_R;

      /* check if potential destination is not integrated, 
	 not cut, and not grounded */
      if (!(mask[v*wid+(u+1)] & IICG)) {
        u++; t++;
        madeJump = TRUE;
        im[ v*wid + u] |= SOURCE_L;
        mask[ v*wid + u] |= INTEGRATED; 
        phase[ v*wid + u]  = phase[ v*wid + (u-1)] + 
                               phaseRemap(phase[v*wid+u] -
			         phase[v*wid+(u-1)]);
      }

    }

    /* Have we tried 'up' and 'right'? */
    if (!madeJump && ((s & TRIED_URDL) == TRIED_UR)) { 

      /* if this is the case, then set TRIED_D since 
	 we're about to try 'down' */
      im[ v*wid + u] |= TRIED_D;

      /* check if potential destination is 
	 not integrated, not cut, and not grounded */
      if (!(mask[(v+1)*wid+u] & IICG)) {
        v++; t++;
        madeJump = TRUE;
        im[ v*wid + u] |= SOURCE_A;
        mask[ v*wid + u] |= INTEGRATED; 
        phase[ v*wid + u]  = phase[ (v-1)*wid + u] + 
                               phaseRemap(phase[v*wid+u] -
			         phase[(v-1)*wid+u]);
      }

    }

    /* Have we tried 'up', 'right', and 'down'? */
    if (!madeJump && ((s & TRIED_URDL) == TRIED_URD)) { 

      /* if this is the case, 
	 then set TRIED_L since we're about to try 'left' */
      im[ v*wid + u] |= TRIED_L;

      /* check if potential destination is 
	 not integrated, not cut, and not grounded */
      if (!(mask[v*wid+(u-1)] & IICG)) {
        u--; t++;
        madeJump = TRUE;
        im[ v*wid + u] |= SOURCE_R;
        mask[ v*wid + u] |= INTEGRATED; 
        phase[ v*wid + u]  = phase[v*wid + (u+1)] + 
                               phaseRemap(phase[v*wid+u] -
			         phase[v*wid+(u+1)]);
      }

    }

    /* Have we tried 'up', 'right', 'down', and 'left'? */
    if (!madeJump && ((s & TRIED_URDL) == TRIED_URDL)) { 

      /* if so, then all we can do is back up */                            
      if      (s & SOURCE_B) { v++; }
      else if (s & SOURCE_L) { u--; }
      else if (s & SOURCE_A) { v--; }
      else if (s & SOURCE_R) { u++; }
      else {      
         /* Don't change (u, v) if all source bits are
          *   0, as this means that we are back at the 
          *   seed point (i, j).
          */
      }
    }

  }  /* end of the big 'while-not-done' loop */

  printf("\n   integratePhase() unwrapped %d pixels...\n", t);
  return;
}

