/*******************************************************************
FUNCTION NAME:   filter - performs an equal weight filtering
 
DESCRIPTION:     Places filter of size nsk*nsk over point
                 x,y in inbuf.  Sums valid values (>0), and
                 divides by the number of valid samples found.
 
RETURN VALUE:    char value = average kernel value 
 
SPECIAL CONSIDERATIONS:
                 Assumption is made that the kernel is square
                 Does not perform mirroring at edges
 
PROGRAM HISTORY: Written 3/17/94 by Tom Logan (ASF)
		 Ported to Suns and modified for small buffers 4/95
*******************************************************************/
unsigned char filter(
                          /****************************************/
    unsigned char *inbuf, /* input image buffer                   */
    int    nl,            /* number of lines for inbuf            */
    int    ns,            /* number of samples per line for inbuf */
    int    x,             /* sample in desired line               */
    int    nsk)           /* number of samples in kernel          */
{                         /****************************************/
    float  kersum =0.0;                  /* sum of kernel       */
    int    half   =(nsk-1)/2,            /* half size kernel    */
           base   =(x-half),             /* index into inbuf    */
           total  =0,                    /* valid kernel values */ 
           i, j;                         /* loop counters       */
                                         /***********************/
    for (i = 0; i < nl; i++)
      {
       for (j = x-half; j <= x+half; j++)
         {
          if (inbuf[base] != 0 && j < ns)
           {
             kersum += (float) inbuf[base];
             total  += 1;
           }
          base++;
         }
       base += ns;
       base -= nsk;
      }
    if (total != 0) kersum /= (float) total;
    return ((unsigned char) (kersum + .5));
}
